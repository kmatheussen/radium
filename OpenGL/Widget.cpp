
/* Copyright 2014-2016 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <bitset>
#include <vector>

#include <QFile>
#include <QCommandLineParser>

#include <QWidget>

#if USE_QT5
#include <QWindow>
#include <QScreen>
#endif

#include <math.h>
#include <stdio.h>

#include <QTextEdit>
#include <QMessageBox>
#include <QApplication>
#include <QAbstractButton>
//#include <QGLFormat>
#include <QDebug>
#include <QElapsedTimer>
#include <QOperatingSystemVersion>

#define GE_DRAW_VL

#include "RhiWindow.hpp"

#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"
#include "../common/playerclass.h"
#include "../common/list_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/time_proc.h"
#include "../common/disk.h"
#include "../common/sequencer_proc.h"
#include "../common/settings_proc.h"
#include "../common/OS_Semaphores.h"
#include "../common/OS_Player_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/Mutex.hpp"
#include "../common/Vector.hpp"
#include "../common/MovingAverage.hpp"
#include "../common/player_proc.h"
#include "../common/visual_proc.h"

#include "../embedded_scheme/scheme_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../Qt/Qt_Bs_edit_proc.h"
#include "../Qt/Timer.hpp"
#include "../Qt/Qt_Fonts_proc.h"
#include "../Qt/Qt_mix_colors.h"

#include "../audio/Juce_plugins_proc.h"

#include "../api/api_gui_proc.h"

#include "GfxElements.h"
#include "Vertices.hpp"
#include "TextureAtlas.hpp"
//#include "T2.hpp"
#include "Timing.hpp"
#include "Render_proc.h"
#include "Widget_proc.h"
//#include "CheckOpenGL_proc.h"

DEFINE_ATOMIC(char *, GE_vendor_string) = strdup("TODO/FIX: vendor-string not set by Radium yet");

static DEFINE_ATOMIC(int, g_curr_realline);

// TS (called from both main thread and opengl thread)
void GE_set_curr_realline(int curr_realline){
  //printf("  ############      Setting g_curr_realline to %d\n", curr_realline);
  ATOMIC_SET(g_curr_realline, curr_realline);
}

#if 1
// OpenGL thread
static float GE_scroll_pos(const SharedVariables *sv, double realline){
  double extra = sv->top_realline - sv->curr_realline;
  return
    (   (realline+extra) * sv->fontheight  );
}
#endif

extern int scrolls_per_second;
extern int default_scrolls_per_second;


// The time_estimator is used to estimate the screen refresh rate according to the soundcard clock.
// I.e. 60.0 hz when using the gfx card clock is unlikely to be 60.0 hz when using the soundcard clock.
//
// However, since we put a high pass filter on the scroll position, it doesn't really matter, so
// we only estimate using QT4. In Qt5, we just use the widget->windowHandle()->screen()->refreshRate() value instead.
//
static TimeEstimator time_estimator;

#if 1
// OpenGL thread
static double get_realline_stime(const SharedVariables *sv, int realline)
{
	double blocktime;
	if(realline==sv->num_reallines)
		blocktime = sv->block_duration;
	else
		blocktime = Place2STime_from_times2(sv->times, p_getDouble(sv->reallines[realline]->l.p));
  
	return blocktime_to_seqtime_double(sv->seqblock_stretch, blocktime);
}
#endif

#if 1
// OpenGL thread
static bool need_to_reset_timing(const SharedVariables *sv, double stime, int last_used_i_realline, const struct Blocks *last_used_block, double last_used_stime, double blocktime)
{
	if (stime < 0){
		fprintf(stderr,"Error: stime: %f, pc->blocktime: %f\n",stime,blocktime);
#if 0
#if !defined(RELEASE)
        abort();
#endif
#endif
		return true;
	}

	if (last_used_block != sv->block)    
		return true;
  
	if(last_used_i_realline>=sv->num_reallines) // First check that i_realline is within the range of the block. (block might have changed number of lines)
		return true;
    
	// TODO: Make the "last_stime < stime"-check configurable.
	if (stime < last_used_stime)
		return true;
  
	if(stime < get_realline_stime(sv, last_used_i_realline)) // Time is now before the line we were at when we left last time. Start searching from 0 again. (Not sure if is correct. It might be last_used_i_realline+1 instead)
		return true;

	return false;
}
#endif

#if 1
// OpenGL thread
static double find_current_realline_while_playing(const SharedVariables *sv, double blocktime)
{
	double time_in_ms = blocktime * 1000.0 / (double)pc->pfreq; // I'm not entirely sure reading pc->start_time_f instead of pc->start_time is unproblematic.
	
	double stime = time_estimator.get(time_in_ms,
									  sv->reltempo * ATOMIC_DOUBLE_GET(g_curr_song_tempo_automation_tempo))
		* (double)pc->pfreq / 1000.0; // Could this value be slightly off because we just changed block, and because of that we skipped a few calles to time_estimator.get ? (it shouldn't matter though, timing is resetted when that happens. 'time_in_ms' should always be valid)

	//stime      = time_in_ms* (double)pc->pfreq / 1000.0;

	static double last_stime = stime;
    
    
	//Strictly speaking, we need to atomically get current block + pc->blocktime. But it is uncertain how important this is is.
	// Maybe store blocktime in the block itself?
  
	static int i_realline = 0; // Note that this one is static. The reason is that we are usually placed on the same realline as last time, so we remember last position and start searching from there.
	static const struct Blocks *block = NULL; // Remember block used last time. If we are not on the same block now, we can't use the i_realline value used last time.

	R_ASSERT(i_realline>=0);
  
	//                                    Common situation. We are usually on the same line as the last visit,
	if (i_realline > 0) i_realline--; //  but we need to go one step back to reload prev_line_stime.
	//                                    (storing last used 'stime1' and/or 'stime2' would be an optimization which would make my head hurt and make no difference in cpu usage)

	if (need_to_reset_timing(sv, stime, i_realline, block, last_stime, blocktime)) {
		i_realline = 0;
		block = sv->block;
		time_estimator.set_time(time_in_ms);
		stime = time_in_ms * (double)pc->pfreq / 1000.0; // Convert the current block time into number of frames.
	}

	//  stime -= 24000;
      
	last_stime = stime;
  
	double stime2 = get_realline_stime(sv, i_realline);
  
	while(true){

		double stime1 = stime2;
		for(;;){ // This for loop is here to handle a very special situation where we play so fast that stime1==stime2. In normal songs, this should not happen.
			stime2 = get_realline_stime(sv, i_realline+1);

#if 0
			if (stime1==stime2){ // Could probably happen if playing really fast... Not sure. (yes, it happens if playing really fast)
#if !defined(RELEASE)
				/abort();
#endif
				return i_realline;
			}
#endif
      
			if (i_realline==sv->num_reallines)
				return sv->num_reallines;
			if (equal_doubles(stime1, stime2))
				i_realline++;
			else
				break;
		}
      
		if (stime >= stime1 && stime <= stime2){
			return scale_double(stime,
								stime1, stime2,
								i_realline, i_realline+1
				);
		}

		i_realline++;

		if (i_realline==sv->num_reallines)
			break;
	}

	return sv->num_reallines;
}

static bool find_scrollpos(const SharedVariables *sv, double &scroll_pos)
{
	const int player_id = ATOMIC_GET(pc->play_id);
	bool is_playing = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;

	if (is_playing)
		if (sv->block_is_visible==false || sv->block!=sv->curr_playing_block)
			is_playing = false; // I.e. we are not rendering the block that is currently playing (if any).

    double blocktime = 0.0;
	
    int playing_blocknum = -1;
	
    if (is_playing){
		
#if 0
		if ((sv->curr_playing_block==NULL || sv->block!=sv->curr_playing_block)) { // Check that our blocktime belongs to the block that is rendered.
        
			//if (new_t2_data!=NULL && use_t2_thread)
			//  T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();
        
			if (t2_data_can_be_used){
				//printf("Waiting...\n");
				//_rendering->render();
				return true;
			}else{

				//printf("Retfalse2. old_t2_datas.size: %d. sv->curr_playing_block==NULL (%d) || sv->block!=sv->curr_playing_block (%d)\n",old_t2_datas.size(), sv->curr_playing_block==NULL, sv->block!=sv->curr_playing_block);
				//printf("  Wait.gakk\n");
				return false; // Returning false uses 100% CPU on Intel gfx / Linux, and could possibly cause jumpy graphics, but here we are just waiting for the block to be rendered.
			}
		}
#endif
		
		playing_blocknum = sv->curr_playing_block->l.num;
        
		blocktime = ATOMIC_DOUBLE_GET(sv->curr_playing_block->player_time);
		//if (blocktime < -50)
		//  printf("blocktime: %f\n",blocktime);
#if 0
		
		if (blocktime < 0.0) {  // Either the block hasn't started playing yet (sequencer cursor is inside a pause), or we just switched block and waiting for a proper blocktime to be calculated.
			
			//if (new_t2_data!=NULL && use_t2_thread)
			//  T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();

			if (t2_data_can_be_used  || !equal_doubles(blocktime, -100.0)){
				_rendering->render();
				//printf("   rettrue1\n");
				return true;
			} else {
				//printf("Retfalse3\n");
				return false;
			}
		}
#endif
	  
    }

    double current_realline_while_playing =
		is_playing
		? find_current_realline_while_playing(sv, blocktime)
		: 0.0;
    
    R_ASSERT_NON_RELEASE(current_realline_while_playing >= 0);

    int current_realline_while_not_playing = ATOMIC_GET(g_curr_realline);
    
    double till_realline =
		ATOMIC_GET_RELAXED(sv->root->play_cursor_onoff)
		? current_realline_while_not_playing
		: is_playing
		? current_realline_while_playing
		: current_realline_while_not_playing;
	
    Play_set_curr_playing_realline(
		is_playing ? (int)current_realline_while_playing : current_realline_while_not_playing,
		playing_blocknum
		);
    
    scroll_pos = GE_scroll_pos(sv, till_realline);

    
    if (player_id != ATOMIC_GET(pc->play_id)) {// In the very weird and unlikely case that the player has stopped and started since the top of this function (the computer is really struggling), we return false
      
		//  if (new_t2_data!=NULL && use_t2_thread)
		//   T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();

		//printf("Retfalse4\n");
		return false;
    }

#if 0
	static float last_scroll_pos = -1;
	
    if (!is_playing && equal_floats(scroll_pos, last_scroll_pos) && new_t2_data==NULL) {
		if (t2_data_can_be_used){
			//_rendering->render();
			//printf("   rettrue2\n");
			return true;
		}else{
			//printf("Retfalse5\n");
			return false; // TODO: Check if this still uses 100% cpu on Intel/linux. It's a little bit wasteful to render the same frame again and again while not playing just because of one driver on one platform.
		}
    }
#endif
	
	return true;
}

#endif


#if 0
// Main thread
static Tracker_Windows *get_window(void){
  return root->song->tracker_windows;
}

// Main thread
static EditorWidget *get_editorwidget(void){
  return (EditorWidget *)get_window()->os_visual.widget;
}
#endif


volatile float g_scroll_pos = 0.0f;

static DEFINE_ATOMIC(double, g_vblank) = 1000 / 60.0;

void GL_update(void)
{	
	//if (SCHEME_is_currently_getting_scheme_history()) // Avoid deadlock when assertion reporter is showing.
	//	return;
}

static r::PaintingData *g_painting_data = NULL; // Accessed from render thread only.
static QColor g_background_color = Qt::black; // Accessed from render thread only.

static QShader getShader(const QString &name)
{
	assert(QThread::currentThread() == g_thread);

    QFile f(name);
    if (f.open(QIODevice::ReadOnly))
        return QShader::fromSerialized(f.readAll());

    return QShader();
}

#if 0
static void init_test_triangles(r::TriangleContext *my_vertices, float dy = 0)
{
	my_vertices->addTriangle({
			-1.0f,   0.0f+dy,       1.0f,   dy,   dy, 0.6f,
			-0.5f,  -1.0f+dy,       1.0f,   0.0f,   dy, 0.6f,
			-0.0f,  -0.0f+dy,       dy,   0.0f,   dy, 0.6f
		});
	
	my_vertices->addTriangle({
			0.5f,  1.0f+dy,       dy,   1.0f,   dy, 0.6f,
			0.0f,  0.0f+dy,       0.0f,   dy,   dy, 0.6f,
			1.0f,  0.0f+dy,       0.0f,   1.0f,   dy, 0.6f
		});
	
	my_vertices->addTriangle(20, 400+dy,
							20, 1800+dy,
							800, 1800+dy,
							0,0,1);
};
#endif

// Main thread
static Tracker_Windows *get_window(void){
  return root->song->tracker_windows;
}

// Main thread
static EditorWidget *get_editorwidget(void){
  return (EditorWidget *)get_window()->os_visual.widget;
}

QRhi *g_rhi = NULL;

//extern r::TextureVertices *g_texture_vertices;

namespace
{

struct TextureRenderer : public r::TextRenderer
{
	r::TextureVertices *_vertices = nullptr;
	
	r::TextureAtlasBackend *_texture_atlas_backend = nullptr;
	r::TextureAtlas *_texture_atlas = nullptr;

	QRhiBuffer *_viewCorrectionBuffer = nullptr;
    QRhiBuffer *_scrollPosBuffer = nullptr;
    QRhiGraphicsPipeline *_pipeline;

	int _num_vertices_in_buffer = 0;
	bool _is_scrolling = true;
	bool _use_scissors = true;

	void init(QRhi *rhi,
			  r::TextureAtlasBackend *texture_atlas_backend,
			  QRhiRenderPassDescriptor *render_pass_descriptor,
			  bool is_scrolling,
			  bool use_scissors)
	{
		_texture_atlas_backend = texture_atlas_backend;
		
		_is_scrolling = is_scrolling;

		_use_scissors = use_scissors;
		
		init_verticess(rhi);
		
		_viewCorrectionBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
										 QRhiBuffer::UniformBuffer,
										 sizeof(QMatrix4x4) + sizeof(float));
		
		if (!_viewCorrectionBuffer || !_viewCorrectionBuffer->create())
		{
			qDebug() << "Failed to create clip correction buffer";
			getchar();
			//return false;
		}
		
		_scrollPosBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
									   QRhiBuffer::UniformBuffer,
									   sizeof(float));
		
		if (!_scrollPosBuffer || !_scrollPosBuffer->create())
		{
			qDebug() << "Failed to create scroll correction buffer";
			getchar();
			//return false;
		}
		
		//QFont font("Cousine", 14, QFont::Normal);
		//font.setStyleStrategy(QFont::PreferAntialias);
		//QString supportedChars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVW #-,.(){}<>=*:0123456789";
		
		_texture_atlas = new r::TextureAtlas(rhi, texture_atlas_backend, _viewCorrectionBuffer, _scrollPosBuffer);
		
		QShader vertexShader = getShader("texture_vertex.qsb");
		QShader fragmentShader = getShader("texture_fragment.qsb");
		
		if (!vertexShader.isValid() || !fragmentShader.isValid())
		{
			qDebug() << "Failed to load compiled shaders";
			getchar();
		}
		
		_pipeline = rhi->newGraphicsPipeline();
		
		if (!_pipeline)
		{
			qDebug() << "Failed to create graphics pipeline";
			getchar();
		}
		
		_pipeline->setSampleCount(4);

		if (_use_scissors)
			_pipeline->setFlags(QRhiGraphicsPipeline::UsesScissor);
		
		QRhiVertexInputLayout inputLayout;
		
		inputLayout.setBindings({
				QRhiVertexInputBinding(sizeof(r::TextureVertex))
			});
		
		inputLayout.setAttributes({
				QRhiVertexInputAttribute(0, 0, QRhiVertexInputAttribute::Float2, offsetof(r::TextureVertex, x)),
				QRhiVertexInputAttribute(0, 1, QRhiVertexInputAttribute::Float2, offsetof(r::TextureVertex, u)),
				QRhiVertexInputAttribute(0, 2, QRhiVertexInputAttribute::Float4, offsetof(r::TextureVertex, r))
			});
			
		{
			QRhiShaderStage vsStage(QRhiShaderStage::Vertex, vertexShader);
			QRhiShaderStage fsStage(QRhiShaderStage::Fragment, fragmentShader);
			_pipeline->setShaderStages({vsStage, fsStage});
		}
		
		_pipeline->setVertexInputLayout(inputLayout);
		_pipeline->setShaderResourceBindings(_texture_atlas->getShaderBindings());
		_pipeline->setRenderPassDescriptor(render_pass_descriptor);
		
		// Enable alpha blending for smooth font edges
		{
			QRhiGraphicsPipeline::TargetBlend blend;
			blend.enable = true;
			blend.srcColor = QRhiGraphicsPipeline::SrcAlpha;
			blend.dstColor = QRhiGraphicsPipeline::OneMinusSrcAlpha;
			blend.srcAlpha = QRhiGraphicsPipeline::One;
			blend.dstAlpha = QRhiGraphicsPipeline::OneMinusSrcAlpha;
			_pipeline->setTargetBlends({blend});
		}
		
		if (!_pipeline->create())
		{
			qDebug() << "Failed to create pipeline";
			getchar();
			//return false;
		}
	}
	
    void release(void)
    {
        if (_texture_atlas)
		{
        	delete _texture_atlas;
        	_texture_atlas = nullptr;
        }

        if (_viewCorrectionBuffer)
		{
        	_viewCorrectionBuffer->destroy();
        	delete _viewCorrectionBuffer;
        	_viewCorrectionBuffer = nullptr;
        }

        if (_scrollPosBuffer)
		{
        	_scrollPosBuffer->destroy();
        	delete _scrollPosBuffer;
        	_scrollPosBuffer = nullptr;
        }

        if (_vertices)
		{
        	delete _vertices;
        	_vertices = nullptr;
        }

        if (_pipeline)
		{
        	_pipeline->destroy();
        	delete _pipeline;
        	_pipeline = nullptr;
        }

        _num_vertices_in_buffer = 0;
    }
	
	void add_text(const QString &text, int x, int y, float r, float g, float b, float a) override
	{
		if (_texture_atlas)
			_texture_atlas->appendStringToVertices(_vertices,
												   text,
												   x, y,
												   r, g, b, a);
		//g_window->_texture_renderer->add_text(gc, text, x, y);
	}

	bool _vertexDataDirty = true;
	
    void prepare_frame(QRhi *rhi,
					   QRhiResourceUpdateBatch *batch,
					   const QMatrix4x4 view_projection,
					   float scroll_pos,
					   float width, float height)
    {
		_vertices->maybe_merge_in(batch);
			
        if (_viewCorrectionBuffer)
		{
#if 0
            QMatrix4x4 clipCorr = rhi->clipSpaceCorrMatrix();
            batch->updateDynamicBuffer(_viewCorrectionBuffer, 0, sizeof(QMatrix4x4), clipCorr.constData());
#else
			batch->updateDynamicBuffer(_viewCorrectionBuffer, 0, sizeof(QMatrix4x4), view_projection.constData());
#endif
            if (_is_scrolling) {
                batch->updateDynamicBuffer(_viewCorrectionBuffer,
									   sizeof(QMatrix4x4),
									   sizeof(float),
									   &scroll_pos);
            }
        }
			
        if (_scrollPosBuffer && _is_scrolling)
		{
            batch->updateDynamicBuffer(_scrollPosBuffer,
									   0,
									   sizeof(float),
									   &scroll_pos);
        }
	}

	void render_frame(QRhi *rhi, QRhiCommandBuffer *command_buffer, const QSize &outputSizeInPixels)
    {
        if (_pipeline)
		{
            command_buffer->setGraphicsPipeline(_pipeline);
            command_buffer->setShaderResources(_texture_atlas->getShaderBindings());

            if (_vertices)
				_vertices->render(command_buffer);
        }
    }

    void init_verticess(QRhi *rhi)
    {
        _vertices = new r::TextureVertices;
		//g_texture_vertices = _vertices;
		
        //_vertices->call_me_when_finished_painting(rhi);

    }
};


#if 0
r::TriangleVertices *g_vertices_under_text = new r::TriangleVertices(); // Scrolling: Everything painted under text (only the track backgruond color.)
r::TriangleVertices *g_vertices_text = new r::TriangleVertices(); // Scrolling: Text.
r::TriangleVertices *g_vertices = new r::TriangleVertices(); // Scrolling: Everything painted above text)
r::TriangleVertices *g_vertices_left_slider = new r::TriangleVertices(); // Left slider (Scrolls it's own way) (not the border around the slider, only the moving box)
r::TriangleVertices *g_vertices_static = new r::TriangleVertices(); // Non-Scrolling: Cursor + Indicators + left slider border.

r::TextureVertices *g_texture_vertices = NULL;

//std::initializer_list<r::TriangleVertices> g_all_contexts = {g_vertices_under_text, g_vertices_text, g_vertices, g_vertices_left_slider, g_vertices_static};
#endif

struct TriangleRenderer : public r::TriangleRenderer
{
	int _slice_num;
	TriangleRenderer *_next_renderer;

	float _top_y;
	float _bottom_y;

	r::TriangleVertices _vertices;
	
    QRhiShaderResourceBindings *_shader_resource_bindings = nullptr;
    QRhiGraphicsPipeline *_pipeline = nullptr;

	QRhiBuffer *_ubuf = nullptr;
	bool _is_scrolling = true;
	bool _use_scissors = true;

    void init(QRhi *rhi,
			  int slice_num,
			  TriangleRenderer *next_renderer,
              QRhiRenderPassDescriptor *render_pass_descriptor,
              bool is_scrolling,
			  bool use_scissors)
	{
		_slice_num = slice_num;
		_next_renderer = next_renderer;
				  
        _is_scrolling = is_scrolling;

		_use_scissors = use_scissors;
		
        _shader_resource_bindings = rhi->newShaderResourceBindings();
		
        _ubuf = rhi->newBuffer(QRhiBuffer::Dynamic,
                               QRhiBuffer::UniformBuffer,
                               sizeof(QMatrix4x4) + sizeof(float));
		
        _ubuf->create();
		
        QVector<QRhiShaderResourceBinding> bindings;
		
        bindings.push_back(QRhiShaderResourceBinding::uniformBuffer(
							   0,
							   QRhiShaderResourceBinding::VertexStage,
							   _ubuf));
		
        _shader_resource_bindings->setBindings(bindings.cbegin(),
											   bindings.cend());
		
        _shader_resource_bindings->create();

        _pipeline = rhi->newGraphicsPipeline();

#if DO_ANTIALIASING
        _pipeline->setSampleCount(4);
#endif
		if (use_scissors)
			_pipeline->setFlags(QRhiGraphicsPipeline::UsesScissor);
		
        {
            QRhiGraphicsPipeline::TargetBlend blend;
            blend.enable = true;
            _pipeline->setTargetBlends({blend});
        }

        _pipeline->setShaderStages({
				{
					QRhiShaderStage::Vertex,
					getShader("color.vert.qsb")
				},
				{
					QRhiShaderStage::Fragment,
					getShader("color.frag.qsb")
				}
			});

		QRhiVertexInputLayout inputLayout;
		
		inputLayout.setBindings({
				{ 6 * sizeof(float) }
			});
		
		inputLayout.setAttributes({
				{ 0, 0, QRhiVertexInputAttribute::Float2, 0 },
				{ 0, 1, QRhiVertexInputAttribute::Float4, 2 * sizeof(float) }
			});

        _pipeline->setVertexInputLayout(inputLayout);
		
        _pipeline->setShaderResourceBindings(_shader_resource_bindings);
		
        _pipeline->setRenderPassDescriptor(render_pass_descriptor);

        _pipeline->create();
    }

    void release(void)
	{
		//delete _vertices1;
        //delete _vertices2;

        delete _shader_resource_bindings;
        delete _pipeline;
        delete _ubuf;
		
        //_vertices1 = nullptr;
        //_vertices2 = nullptr;
		
        _shader_resource_bindings = nullptr;
        _pipeline = nullptr;
        _ubuf = nullptr;
    }
	
	void call_me_before_adding_triangles(void)
	{
		_vertices.call_me_when_starting_to_generate_vertices();

		if (_next_renderer != NULL)
		{
			const float slice_size = GE_get_slice_size(g_painting_data);
			_top_y = _slice_num * slice_size;
			_bottom_y = _next_renderer->_slice_num * slice_size;
		}
	}
	
    void prepare_frame(QRhi *rhi,
					   QRhiResourceUpdateBatch *batch,
					   const QMatrix4x4 &viewProjection,
					   float scrollPos)
    {
		_vertices.maybe_merge_in(batch);
		/*
        for (r::TriangleVertices *context : ALL_VERTICESS)
        {
            if (context)
            {
                context->maybe_merge_in(batch);
            }
        }
		*/
			
        //if (_vertices1)
         //   _vertices1->maybe_merge_in(batch);
			
        //if (_vertices2)
		//_vertices2->maybe_merge_in(batch);
			
        batch->updateDynamicBuffer(_ubuf,
								   0,
								   64,
								   viewProjection.constData());

        if (_is_scrolling) {
            batch->updateDynamicBuffer(_ubuf,
								   64,
								   sizeof(float),
								   &scrollPos);
        }
    }
	
    void render_frame(QRhiCommandBuffer *command_buffer,
					  const QSize &outputSizeInPixels)
	{
		command_buffer->setGraphicsPipeline(_pipeline);

        command_buffer->setViewport({
				0,
				0,
				float(outputSizeInPixels.width()),
				float(outputSizeInPixels.height())
			});
		
        command_buffer->setShaderResources();

		_vertices.render(command_buffer);
		/*
        for (r::TriangleVertices *context : ALL_VERTICESS)
        {
            if (context)
            {
                context->render(command_buffer);
            }
        }
		*/
        //if (_vertices1)
		//  _vertices1->render(command_buffer);
		
        //if (_vertices2)
		//  _vertices2->render(command_buffer);
    }

	
private:
	
	void add_triangle_no_split(const GE_Context &c, const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3, r2::GradientType::Type gradient_type)
	{
		switch(gradient_type)
		{
			case r2::GradientType::Type::NOTYPE:
				_vertices.add_triangle(p1, p2, p3, c.color.c);
				break;
			case r2::GradientType::Type::HORIZONTAL:
				_vertices.add_triangle_horizontal_gradient(p1, p2, p3, c.color.c, c.color.c_gradient);
				break;
			case r2::GradientType::Type::VELOCITY:
				_vertices.add_triangle_vertical_gradient(p1, p2, p3, c.color.c, c.color.c_gradient);
				break;
		}
	}
	
public:
	
	void add_triangle(const GE_Context &c, const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3, r2::GradientType::Type gradient_type) override
	{	
		if (_next_renderer == NULL)
		{
			add_triangle_no_split(c, p1, p2, p3, gradient_type);
			return;
		}

		const float maxy = _next_renderer == NULL ? -1 : R_MAX(p1.b, R_MAX(p2.b, p3.b));
			
		if (maxy <= _bottom_y)
		{
			add_triangle_no_split(c, p1, p2, p3, gradient_type);
			return;
		}

		// Clip triangle against horizontal line y = _bottom_y. Produce top and bottom polygons.
		std::vector<r::fvec2> inVerts = {p1, p2, p3};

		std::vector<r::fvec2> topPoly, bottomPoly;
		
		for (size_t i = 0; i < inVerts.size(); ++i)
		{
			r::fvec2 a = inVerts[i];
			r::fvec2 b = inVerts[(i+1) % inVerts.size()];

			for(bool keepTop : {false, true})
			{
				auto &out = keepTop ? topPoly : bottomPoly;

				// Top should be strictly above the boundary; bottom includes the boundary
				bool a_in = keepTop ? (a.b < _bottom_y) : (a.b >= _bottom_y);
				bool b_in = keepTop ? (b.b < _bottom_y) : (b.b >= _bottom_y);
			
				if (a_in && b_in)
				{
					// both in: keep b
					out.push_back(b);
				}
				else if (a_in && !b_in)
				{
					// exiting: add intersection
					float dy = b.b - a.b;
					if (std::fabs(dy) > 1e-9f) {
						float t = (_bottom_y - a.b) / dy;
						float x = a.a + t * (b.a - a.a);
						out.push_back({x, _bottom_y});
					}
				}
				else if (!a_in && b_in)
				{
					// entering: add intersection then b
					float dy = b.b - a.b;
					if (std::fabs(dy) > 1e-9f) {
						float t = (_bottom_y - a.b) / dy;
						float x = a.a + t * (b.a - a.a);
						out.push_back({x, _bottom_y});
					}
					out.push_back(b);
				}
				else
				{
					// both out: nothing
				}
			}
		}
		
		GE_Context c1 = c;
		GE_Context c2 = c;
			
		if (gradient_type == r2::GradientType::Type::VELOCITY)
		{
			const float miny = _next_renderer == NULL ? -1 : R_MIN(p1.b, R_MIN(p2.b, p3.b));

			if ((maxy - miny) > 0.01)
			{
				c1.color.c_gradient = GE_mix(c1.color.c,
											 c1.color.c_gradient,
											 scale(_bottom_y,
												   miny, maxy,
												   1000, 0));
				
				c2.color.c = c1.color.c_gradient;
			}
		}
		
		// Triangulate top polygon and add locally
		if (topPoly.size() >= 3)
		{
			for (size_t i = 1; i + 1 < topPoly.size(); ++i)
			{
				add_triangle_no_split(c1, topPoly[0], topPoly[i], topPoly[i+1], gradient_type);
			}
		}

		// Triangulate bottom polygon and forward to next renderer
		if (bottomPoly.size() >= 3)
		{
			for (size_t i = 1; i + 1 < bottomPoly.size(); ++i)
			{
				_next_renderer->add_triangle(c2, bottomPoly[0], bottomPoly[i], bottomPoly[i+1], gradient_type);
			}
		}
	}
};

class RenderWindow : public radium::RhiWindow, public radium::MouseCycleFix
{
public:

	r::TextureAtlasBackend *_texture_atlas_backend = nullptr;
	r::TextureAtlasBackend *_texture_atlas_backend_halfsize = nullptr;

	TextureRenderer _texture_renderers[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_scissors[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_scissors_halfsize[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_static;
	
	TriangleRenderer _triangle_renderers[MAX_NUM_SLICES];
	TriangleRenderer _triangle_renderer_scissors[MAX_NUM_SLICES];
	TriangleRenderer _triangle_renderer_static;
	TriangleRenderer _triangle_renderer_static_scissor;
	
	DEFINE_ATOMIC(bool, _main_window_is_exposed) = false;

	
public:

	RenderWindow(QRhi::Implementation graphicsApi)
		: RhiWindow(graphicsApi)
	{
	}

	~RenderWindow()
	{
		fprintf(stderr, "H1\n");

		QSemaphore sem;
	
		put_event([this, &sem]()
			{
				for (int i = 0; i < MAX_NUM_SLICES; ++i) {
					_triangle_renderers[i].release();
					_triangle_renderer_scissors[i].release();
					_texture_renderers[i].release();
					_texture_renderer_scissors[i].release();
					_texture_renderer_scissors_halfsize[i].release();
				}
				_texture_renderer_static.release();
				_triangle_renderer_static.release();
				_triangle_renderer_static_scissor.release();
				
				fprintf(stderr, "H5\n");
				sem.release();
			});

		fprintf(stderr, "H6\n");
		sem.acquire();
		fprintf(stderr, "H7\n");
	}

	const QFont get_halfsize_font(const QFont &font)
	{
		int full_size = font.pointSize();
		int half_size = full_size / 2;
		
		for(int size = half_size ; size > 1 && size < full_size ; size++)
		{
			QFont font2(font);
			font2.setPointSize(size);
			
			if (font2.pointSize() != full_size)
				return font2;
		}
  
		return font; // give up.
	}

	void setFont(const QFont &font)
	{
		if (_texture_atlas_backend && _texture_atlas_backend_halfsize)
		{
			_texture_atlas_backend->setFont(font);
			_texture_atlas_backend_halfsize->setFont(get_halfsize_font(font));
		}
	}
	
	void customInit(const QFont &font) override
	{
		_texture_atlas_backend = new r::TextureAtlasBackend(_rhi, font);
		_texture_atlas_backend_halfsize = new r::TextureAtlasBackend(_rhi, get_halfsize_font(font));
		
		for (int i = 0; i < MAX_NUM_SLICES; ++i)
		{
			// Texture renderers (scrolling, tempo tracks, per-slice)
			_texture_renderers[i].init(_rhi, _texture_atlas_backend, _render_pass_descriptor, true, false);

			// Triangle renderers (per-slice, scrolling)
			_triangle_renderers[i].init(_rhi, i, i >= (MAX_NUM_SLICES-1) ? NULL : &_triangle_renderers[i+1], _render_pass_descriptor, true, false);

			// Texture renderers (scrolling, normal tracks, per-slice scissored)
			_texture_renderer_scissors[i].init(_rhi, _texture_atlas_backend, _render_pass_descriptor, true, true);
			_texture_renderer_scissors_halfsize[i].init(_rhi, _texture_atlas_backend_halfsize, _render_pass_descriptor, true, true);

			// Triangle renderers (per-slice, scissored)
			_triangle_renderer_scissors[i].init(_rhi, i, i == (MAX_NUM_SLICES-1) ? NULL : &_triangle_renderer_scissors[i+1], _render_pass_descriptor, true, true);
		}

		// Non-scrolling texture renderer
		_texture_renderer_static.init(_rhi, _texture_atlas_backend, _render_pass_descriptor, false, false);

		// Non-scrolling triangle renderer
		_triangle_renderer_static.init(_rhi, -1, NULL, _render_pass_descriptor, false, false);
		// Non-scrolling scissored triangle renderer
		_triangle_renderer_static_scissor.init(_rhi, -1, NULL, _render_pass_descriptor, false, true);
		
		const double ratio = devicePixelRatio();

		safe_double_write(&g_opengl_scale_ratio, ratio);

#if 0
		// Make sure editor font is scaled.
		THREADING_run_on_main_thread_async([](void)
			{
				if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL){
					setFontValues(root->song->tracker_windows);
				}
			});
#endif
		
		QScreen *qscreen = screen();
		R_ASSERT(qscreen != NULL);

		double refresh_rate;
		
		if (qscreen != NULL)			
			refresh_rate = qscreen->refreshRate();
		else
			refresh_rate = 60;

		R_ASSERT(refresh_rate >= 0.5);

		if (refresh_rate >= 0.5)
		{
			ATOMIC_DOUBLE_SET(g_vblank, 1000.0 / refresh_rate);		
		}

		time_estimator.set_vblank(ATOMIC_DOUBLE_GET(g_vblank));

		connect(this, &QWindow::screenChanged, [](QScreen *screen)
			{
				R_ASSERT(screen != NULL);
		
				if (screen==NULL)
					return;
		
				double refresh_rate = screen->refreshRate();
		
				printf("  NEW REFRESH RATE: %f\n", refresh_rate);

				R_ASSERT(refresh_rate >= 0.5);
				
				if (refresh_rate >= 0.5)
				{
					//widget->set_vblank(1000.0 / refresh_rate);
					ATOMIC_DOUBLE_SET(g_vblank, 1000.0 / refresh_rate);
				}

				time_estimator.set_vblank(ATOMIC_DOUBLE_GET(g_vblank));
#if !defined(RELEASE)
				getchar();
#endif
			});

		printf("Ratio: %f. refresh: %f\n", ratio, refresh_rate);

		printf("gotit\n");
#if !defined(RELEASE)
		//getchar();
#endif
	}

#if !defined(RELEASE)
	double _num_renderings=0;
	double _last_rendering_time = 0;
#endif
	
	void customRender() override
	{
		assert(QThread::currentThread() == g_thread);

#if !defined(RELEASE)
		static double s_start_time = TIME_get_ms();

		double new_time = TIME_get_ms();
		double dur = new_time - _last_rendering_time;
		if (dur > 30 || dur < 5)
			printf("\n\n\n****************** Dur: %f. Average: %f ************\n\n\n", dur, (new_time - s_start_time) / R_MAX(1, _num_renderings));
		_num_renderings++;
		_last_rendering_time = new_time;

		//static int g_n=0; printf("Rendering %d\n", g_n++);
#endif

		auto *sv = GE_get_shared_variables(g_painting_data);

		// TODO: Check this one.
		//GE_set_curr_realline(sv->curr_realline);

		const QSize outputSizeInPixels = _swap_chain->currentPixelSize();
		
		double scroll_pos = 0.0;

		find_scrollpos(sv, scroll_pos);

		if (ATOMIC_GET(pc->player_state) != PLAYER_STATE_PLAYING)
		{
			R_ASSERT(fabsf(scroll_pos - round(scroll_pos)) < 0.001);
			scroll_pos = round(scroll_pos);
		}

		const float scroll_y1 = scroll_pos;
		const float scroll_y2 = scroll_pos + outputSizeInPixels.height();

		const float slice_size = GE_get_slice_size(g_painting_data);
		
		const int slice_start = R_BOUNDARIES(0, scroll_y1 / slice_size, MAX_NUM_SLICES-1);
		const int slice_end = R_BOUNDARIES(slice_start+1, ceilf(scroll_y2 / slice_size), MAX_NUM_SLICES);

		printf("slices: %d -> %d. scroll: %f -> %f. scroll_pos: %f\n", slice_start, slice_end, scroll_y1, scroll_y2, scroll_pos);

		QRhiResourceUpdateBatch *batch = _rhi->nextResourceUpdateBatch();


		_texture_atlas_backend->uploadTexture(batch);
		_texture_atlas_backend_halfsize->uploadTexture(batch);


		for (int i = slice_start ; i < slice_end ; ++i)
		{
			// Triangles (per-slice) - scrolling
			_triangle_renderers[i].prepare_frame(_rhi,
												 batch,
												 _viewProjection,
												 scroll_pos);
		
			// Triangles (per-slice) - scrolling+scissored
			_triangle_renderer_scissors[i].prepare_frame(_rhi,
														 batch,
														 _viewProjection,
														 scroll_pos);
		}
		
		// Triangles (non-scrolling)
		_triangle_renderer_static.prepare_frame(_rhi,
												batch,
												_viewProjection,
												0.0f);
		
		// Triangles (non-scrolling, scissors)
		_triangle_renderer_static_scissor.prepare_frame(_rhi,
														batch,
														_viewProjection,
														0.0f);
		
		for (int i = slice_start ; i < slice_end ; ++i)
		{
			// Textures (i.e. text) - scrolling, per-slice
			_texture_renderers[i].prepare_frame(_rhi,
												batch,
												_viewProjection,
												scroll_pos,
												outputSizeInPixels.width(),
												outputSizeInPixels.height());

			// Texture, scrolling+scissored, per-slice
			_texture_renderer_scissors[i].prepare_frame(_rhi,
														batch,
														_viewProjection,
														scroll_pos,
														outputSizeInPixels.width(),
														outputSizeInPixels.height());
			_texture_renderer_scissors_halfsize[i].prepare_frame(_rhi,
																 batch,
																 _viewProjection,
																 scroll_pos,
																 outputSizeInPixels.width(),
																 outputSizeInPixels.height());
		}
		
		// Textures (non-scrolling)
		_texture_renderer_static.prepare_frame(_rhi,
											   batch,
											   _viewProjection,
											   0.0f,
											   outputSizeInPixels.width(),
											   outputSizeInPixels.height());
		
		QRhiCommandBuffer *command_buffer = _swap_chain->currentFrameCommandBuffer();
		
		command_buffer->beginPass(_swap_chain->currentFrameRenderTarget(), g_background_color, { 1.0f, 0 }, batch);
		{
			// triangles (scrolling) - per-slice
			for (int i = slice_start ; i < slice_end ; ++i)
				_triangle_renderers[i].render_frame(command_buffer, outputSizeInPixels);

			// scissor rectangle based on shared variables
			int sc_x = 0;
			int sc_w = 0;
			if (g_painting_data)
			{
				const SharedVariables *sv = GE_get_shared_variables(g_painting_data);
				sc_x = int(sv->wtracks_scissor_x1 * g_opengl_scale_ratio);
				sc_w = int((sv->wtracks_scissor_x2 - sv->wtracks_scissor_x1) * g_opengl_scale_ratio);
				if (sc_x < 0)
					sc_x = 0;

				/*
				printf("sc_x: %d. scissor_x1: %d. sc_w: %d. scissor_x2: %d\n",
					   sc_x, sv->wtracks_scissor_x1,
					   sc_w, sv->wtracks_scissor_x2);
				*/
				
				if (sc_w > 0)
					command_buffer->setScissor({sc_x, 0, sc_w, outputSizeInPixels.height()});
			}

			for (int i = slice_start ; i < slice_end ; ++i)
				_triangle_renderer_scissors[i].render_frame(command_buffer, outputSizeInPixels);
			
			// triangles (non-scrolling, scissors) - draw on top
			_triangle_renderer_static_scissor.render_frame(command_buffer,
														   outputSizeInPixels);
			
			if (sc_w > 0)
				command_buffer->setScissor({0, 0, outputSizeInPixels.width(), outputSizeInPixels.height()});

			// triangles (non-scrolling) - draw on top
			_triangle_renderer_static.render_frame(command_buffer,
												   outputSizeInPixels);

			// text (scrolling) - per-slice
			for (int i = slice_start ; i < slice_end ; ++i)
				_texture_renderers[i].render_frame(_rhi,
												   command_buffer,
												   outputSizeInPixels);

			if (sc_w > 0)
				command_buffer->setScissor({sc_x, 0, sc_w, outputSizeInPixels.height()});

			// text (scissored)
			for (int i = slice_start ; i < slice_end ; ++i)
			{
				_texture_renderer_scissors[i].render_frame(_rhi,
														   command_buffer,
														   outputSizeInPixels);
				_texture_renderer_scissors_halfsize[i].render_frame(_rhi,
																	command_buffer,
																	outputSizeInPixels);
			}
			
			if (sc_w > 0)
				command_buffer->setScissor({0, 0, outputSizeInPixels.width(), outputSizeInPixels.height()});

			// text (non-scrolling) - draw on top
			_texture_renderer_static.render_frame(_rhi,
												  command_buffer,
												  outputSizeInPixels);
		}		
		command_buffer->endPass();

		safe_volatile_float_write(&g_scroll_pos, scroll_pos);
	}

	// Main thread
	bool fix_mousePressEvent(radium::MouseCycleEvent &qmouseevent) override
	{
		return get_editorwidget()->handle_mouse_press(qmouseevent, qmouseevent.x(), qmouseevent.y() + root->song->tracker_windows->wblock->t.y1);
	}

	// Main thread
	void fix_mouseMoveEvent(radium::MouseCycleEvent &qmouseevent) override
	{
		//printf("fix_mouseMoveEvent %d %d\n", (int)qmouseevent.x(), (int)qmouseevent.y());
		get_editorwidget()->handle_mouse_move(qmouseevent.button(), qmouseevent.x(), qmouseevent.y() + root->song->tracker_windows->wblock->t.y1);
	}

	// Main thread
	bool fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override
	{
		return get_editorwidget()->handle_mouse_release(event.button(), event.x(), event.y() + root->song->tracker_windows->wblock->t.y1);
	}
  
	MOUSE_CYCLE_CALLBACKS_FOR_QT;

	void wheelEvent(QWheelEvent *qwheelevent) override
	{
		get_editorwidget()->wheelEvent(qwheelevent);
	}

	void resizeEvent(QResizeEvent *qresizeevent) override
	{
		if (g_editor->window != NULL)
			calculateNewWindowWidthAndHeight(g_editor->window);

		GE_set_height(qresizeevent->size().height());
	}
};

#if QT_CONFIG(vulkan)
static QVulkanInstance *g_vulkan_inst = nullptr;
#endif

static QRhi::Implementation init_qrhi(void)
{
    QRhi::Implementation graphicsApi;

    // Use platform-specific defaults when no command-line arguments given.
#if defined(Q_OS_WIN)
    graphicsApi = QRhi::D3D11;
#elif QT_CONFIG(metal)
    graphicsApi = QRhi::Metal;
#elif QT_CONFIG(vulkan)
    graphicsApi = QRhi::Vulkan;
#else
    graphicsApi = QRhi::OpenGLES2;
#endif

	//graphicsApi = QRhi::OpenGLES2;
	
    QCommandLineParser cmdLineParser;
    cmdLineParser.addHelpOption();
    QCommandLineOption nullOption({ "n", "null" }, QLatin1String("Null"));
    cmdLineParser.addOption(nullOption);
    QCommandLineOption glOption({ "g", "opengl" }, QLatin1String("OpenGL"));
    cmdLineParser.addOption(glOption);
    QCommandLineOption vkOption({ "v", "vulkan" }, QLatin1String("Vulkan"));
    cmdLineParser.addOption(vkOption);
    QCommandLineOption d3d11Option({ "d", "d3d11" }, QLatin1String("Direct3D 11"));
    cmdLineParser.addOption(d3d11Option);
    QCommandLineOption d3d12Option({ "D", "d3d12" }, QLatin1String("Direct3D 12"));
    cmdLineParser.addOption(d3d12Option);
    QCommandLineOption mtlOption({ "m", "metal" }, QLatin1String("Metal"));
    cmdLineParser.addOption(mtlOption);

    cmdLineParser.process(*qApp);
    if (cmdLineParser.isSet(nullOption))
        graphicsApi = QRhi::Null;
    if (cmdLineParser.isSet(glOption))
        graphicsApi = QRhi::OpenGLES2;
    if (cmdLineParser.isSet(vkOption))
        graphicsApi = QRhi::Vulkan;
    if (cmdLineParser.isSet(d3d11Option))
        graphicsApi = QRhi::D3D11;
    if (cmdLineParser.isSet(d3d12Option))
        graphicsApi = QRhi::D3D12;
    if (cmdLineParser.isSet(mtlOption))
        graphicsApi = QRhi::Metal;

 //! [api-setup]
    // For OpenGL, to ensure there is a depth/stencil buffer for the window.
    // With other APIs this is under the application's control (QRhiRenderBuffer etc.)
    // and so no special setup is needed for those.
    QSurfaceFormat fmt;
    fmt.setDepthBufferSize(24);
    fmt.setStencilBufferSize(8);
    // Special case macOS to allow using OpenGL there.
    // (the default Metal is the recommended approach, though)
    // gl_VertexID is a GLSL 130 feature, and so the default OpenGL 2.1 context
    // we get on macOS is not sufficient.
#ifdef Q_OS_MACOS
    fmt.setVersion(4, 1);
    fmt.setProfile(QSurfaceFormat::CoreProfile);
#endif

#if 1 //DO_ANTIALIASING
	fmt.setSamples(4); // Don't see any difference setting this one.
#endif
	
#if 0 // investigate when/if there's a difference between these three. (if no difference, than SingleBuffer is probably better due to lower input latency)
	fmt.setSwapBehavior(QSurfaceFormat::SingleBuffer);
	fmt.setSwapBehavior(QSurfaceFormat::TripleBuffer);
	fmt.setSwapBehavior(QSurfaceFormat::DoubleBuffer);
#endif

	//fmt.setSwapBehavior(QSurfaceFormat::TripleBuffer);
	
	printf("--- Swap interval: %d\n"
		   "--- Renderable types: %x\n",
		   fmt.swapInterval(),
		   (unsigned int)fmt.renderableType()
		);
#if 0
	fmt.setAlphaBufferSize(8); // Tipper dette bare er for å få gjennomsiktige os-vinduer... (om man har compositør kjørende)
	
	if (!fmt.hasAlpha())
	{
		abort();
	}
#endif
	
    QSurfaceFormat::setDefaultFormat(fmt);

    // For Vulkan.
#if QT_CONFIG(vulkan)
    if (graphicsApi == QRhi::Vulkan) {
        // Allocate the global Vulkan instance on the heap so its lifetime
        // can be controlled precisely and torn down before static shutdown.
        if (g_vulkan_inst == nullptr)
            g_vulkan_inst = new QVulkanInstance();

        // Request validation, if available. This is completely optional
        // and has a performance impact, and should be avoided in production use.
        g_vulkan_inst->setLayers({ "VK_LAYER_KHRONOS_validation" });
        // Play nice with QRhi.
        g_vulkan_inst->setExtensions(QRhiVulkanInitParams::preferredInstanceExtensions());
        if (!g_vulkan_inst->create()) {
            qWarning("Failed to create Vulkan instance, switching to OpenGL");
            delete g_vulkan_inst;
            g_vulkan_inst = nullptr;
            graphicsApi = QRhi::OpenGLES2;
        }
    }
#endif
//! [api-setup]

	return graphicsApi;
}



} // anon. namespace


bool g_gl_widget_started = false;
static RenderWindow *g_window;
static QWidget *g_widget;


static bool is_scrolling(const GE_Context &c)
{
    if (Z_IS_STATIC_X(c._conf.z))
		return true; // scroll, non-scissor.

	if (c._conf.z == Z_PLAYCURSOR)
		return false; // play cursor 

    if (c._conf.z <= Z_MAX_SCROLLTRANSFORM)
		return true; // scroll

	if (c._conf.z < Z_MIN_STATIC)
		return false; // scroll bar

	return false; // everything else.
}

r::TriangleRenderer *GE_get_triangle_renderer(const GE_Context &context)
{
	R_ASSERT(g_gl_widget_started);

	if (!is_scrolling(context))
	{
		if (context._conf.use_scissors == USE_SCISSORS)
			return &g_window->_triangle_renderer_static_scissor;
		else
			return &g_window->_triangle_renderer_static;
	}

	int slice = context._slice;
	
	R_ASSERT(slice >= 0 && slice < MAX_NUM_SLICES);
	
	if (slice < 0)
		slice = 0;
	
	if (slice >= MAX_NUM_SLICES)
		slice = MAX_NUM_SLICES-1;

	if (context._conf.use_scissors == USE_SCISSORS)
		return &g_window->_triangle_renderer_scissors[slice];
	else
		return &g_window->_triangle_renderers[slice];
}

r::TextRenderer *GE_get_text_renderer(const GE_Context &context, bool is_half_size)
{
	R_ASSERT(g_gl_widget_started);

	if (!is_scrolling(context))
	{
		R_ASSERT_NON_RELEASE(!is_half_size);
		return dynamic_cast<r::TextRenderer*>(&g_window->_texture_renderer_static);
	}
			
	int slice = context._slice;

	R_ASSERT(slice >= 0 && slice < MAX_NUM_SLICES);
	
	if (slice < 0)
		slice = 0;
	
	if (slice >= MAX_NUM_SLICES)
		slice = MAX_NUM_SLICES-1;

	if (context._conf.use_scissors == USE_SCISSORS)
	{
		if (is_half_size)
			return dynamic_cast<r::TextRenderer*>(&g_window->_texture_renderer_scissors_halfsize[slice]);
		else
			return dynamic_cast<r::TextRenderer*>(&g_window->_texture_renderer_scissors[slice]);
	}
	else
	{
		R_ASSERT_NON_RELEASE(!is_half_size);
		return dynamic_cast<r::TextRenderer*>(&g_window->_texture_renderers[slice]);
	}
}

	
#if 0
extern void gakk_GE_text(const char *text, int x, int y, float r, float g, float b, float a);
void gakk_GE_text(const char *text, int x, int y, float r, float g, float b, float a)
{
	g_window->_texture_renderers[0].add_text(text, x, y, r, g, b, a);
}
#endif

void GE_set_font(const QFont &font)
{
	if (g_window)
	{
		g_window->setFont(font);
		#if 0
		for (int i = 0; i < MAX_NUM_SLICES; ++i) {
			if (g_window->_texture_renderers[i]._texture_atlas)
				g_window->_texture_renderers[i]._texture_atlas->setFont(font);
			if (g_window->_texture_renderer_scissors[i]._texture_atlas)
				g_window->_texture_renderer_scissors[i]._texture_atlas->setFont(font);
		}
		if (g_window->_texture_renderer_static._texture_atlas)
			g_window->_texture_renderer_static._texture_atlas->setFont(font);
		#endif
		GFX_ForceScheduleEditorRedraw(); // New font will be set before starting new paint.
	}
}

void aiai(void);
void aiai(void)  // Called before starting to render, before sharedData has been filled in.
{
	if (g_window && g_window->_texture_atlas_backend && g_window->_texture_atlas_backend_halfsize)
	{
		g_window->_texture_atlas_backend->maybe_switch_to_next_d();
		g_window->_texture_atlas_backend_halfsize->maybe_switch_to_next_d();
		#if 0
		for (int i = 0; i < MAX_NUM_SLICES; ++i) {
			if (g_window->_texture_renderers[i]._texture_atlas)
				g_window->_texture_renderers[i]._texture_atlas->maybe_switch_to_next_d();
			if (g_window->_texture_renderer_scissors[i]._texture_atlas)
				g_window->_texture_renderer_scissors[i]._texture_atlas->maybe_switch_to_next_d();
		}
		if (g_window->_texture_renderer_static._texture_atlas)
			g_window->_texture_renderer_static._texture_atlas->maybe_switch_to_next_d();
		#endif
		// Note: previously this function obtained render buffers for new
		// vertex generation. That could race with GE_start_writing calling
		// the same routines, causing duplicate starts. Start/finish must be
		// driven by the main-thread writing lifecycle (GE_start_writing/
		// GE_end_writing). Leave aiai purely for texture atlas switching.

		//g_window->_triangle_renderer._vertices.call_me_when_starting_to_generate_vertices();
		//if (g_window->_texture_renderer._vertices)
		//	g_window->_texture_renderer._vertices->call_me_when_starting_to_generate_vertices();
	}
}

void aiai2(void);
void aiai2(void) // Called before starting to render, after sharedData has been filled in.
{
	// Obtain render buffers for vertex generators here, driven by the main-thread
	// painting-data lifecycle to avoid duplicate starts from other paths.
	if (g_rhi != NULL && g_window)
	{
		for (int i = 0; i < MAX_NUM_SLICES; ++i)
		{
			// Triangle vertices (scrolling) - per-slice
			g_window->_triangle_renderers[i].call_me_before_adding_triangles();

			// Triangle vertices (scrolling, scissored) - per-slice
			g_window->_triangle_renderer_scissors[i].call_me_before_adding_triangles();
		}
		
		// Triangle vertices (non-scrolling/static)
		g_window->_triangle_renderer_static.call_me_before_adding_triangles();
		
		// Triangle vertices (non-scrolling/static, scissors)
		g_window->_triangle_renderer_static_scissor.call_me_before_adding_triangles();
		
		for (int i = 0; i < MAX_NUM_SLICES; ++i)
		{
			// Texture vertices (may not exist yet) - scrolling (per-slice)
			if (g_window->_texture_renderers[i]._vertices)
				g_window->_texture_renderers[i]._vertices->call_me_when_starting_to_generate_vertices();
			
			// Texture vertices (scissored) (per-slice)
			if (g_window->_texture_renderer_scissors[i]._vertices)
				g_window->_texture_renderer_scissors[i]._vertices->call_me_when_starting_to_generate_vertices();
			if (g_window->_texture_renderer_scissors_halfsize[i]._vertices)
				g_window->_texture_renderer_scissors_halfsize[i]._vertices->call_me_when_starting_to_generate_vertices();
		}
		
		// Texture vertices (non-scrolling/static)
		if (g_window->_texture_renderer_static._vertices)
			g_window->_texture_renderer_static._vertices->call_me_when_starting_to_generate_vertices();
	}
}


// Called from main thread -> schedule painting data on the RHI thread.
void GL_set_new_painting_data(r::PaintingData *painting_data, GE_Rgb new_background_color)
{
	// The render buffers should already have been obtained by aiai()/GE_start_writing.
	// Don't re-obtain them here — that caused double-start issues.

	g_window->put_event([painting_data, new_background_color](void)
		{

			// TODO: Free/etc. All global Context variables here as well, maybe.

			GE_delete_painting_data(g_painting_data);
			
			g_painting_data = painting_data;
			g_background_color = QColor((int)new_background_color.r,
										(int)new_background_color.g,
										(int)new_background_color.b,
										(int)new_background_color.a);
		});
}

// Called from main thread to indicate generation finished and buffers may be released.
void GL_finish_generating_vertices(void)
{
	if (!g_window)
		return;

	// These methods are thread-safe wrt the internal buffer locking.
	for (int i = 0; i < MAX_NUM_SLICES; ++i)
	{
		g_window->_triangle_renderers[i]._vertices.call_me_after_finished_generating_vertices();
		g_window->_triangle_renderer_scissors[i]._vertices.call_me_after_finished_generating_vertices();
	}
	
	g_window->_triangle_renderer_static._vertices.call_me_after_finished_generating_vertices();
	g_window->_triangle_renderer_static_scissor._vertices.call_me_after_finished_generating_vertices();
	
	for (int i = 0; i < MAX_NUM_SLICES; ++i)
	{
		if (g_window->_texture_renderers[i]._vertices)
			g_window->_texture_renderers[i]._vertices->call_me_after_finished_generating_vertices();

		if (g_window->_texture_renderer_scissors[i]._vertices)
			g_window->_texture_renderer_scissors[i]._vertices->call_me_after_finished_generating_vertices();
		if (g_window->_texture_renderer_scissors_halfsize[i]._vertices)
			g_window->_texture_renderer_scissors_halfsize[i]._vertices->call_me_after_finished_generating_vertices();
	}
	
	if (g_window->_texture_renderer_static._vertices)
		g_window->_texture_renderer_static._vertices->call_me_after_finished_generating_vertices();
}


double GL_get_vblank(void){
    return ATOMIC_DOUBLE_GET(g_vblank);
}

bool GL_check_compatibility(void)
{
	return true;
}

void GL_set_vsync(bool onoff){
  SETTINGS_write_bool("vsync", onoff);
}

bool GL_get_vsync(void){
  return SETTINGS_read_bool("vsync", true);
}

void GL_set_multisample(int size){
  SETTINGS_write_int("multisample", size);
}

int GL_get_multisample(void){
  return R_BOUNDARIES(1, SETTINGS_read_int32("multisample", 4), 32);
}

void GL_set_safe_mode(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("safe_mode", onoff);
}


static bool g_high_render_thread_priority = true;
bool GL_get_high_render_thread_priority(void){
  static bool s_has_inited = false;
  if (s_has_inited==false){
    g_high_render_thread_priority = SETTINGS_read_bool("high_render_thread_priority", g_high_render_thread_priority);
    s_has_inited = true;
  }
  
  return g_high_render_thread_priority;
}

void GL_set_high_render_thread_priority(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("high_render_thread_priority", onoff);
  g_high_render_thread_priority = onoff;
	/*
  if (g_render_thread != NULL)
    g_render_thread->setPriority(onoff ? QThread::HighestPriority : QThread::NormalPriority);
	*/
}


bool GL_get_safe_mode(void){
  return SETTINGS_read_bool("safe_mode", false);
}

void GL_set_backend(const char *backend)
{
	if (strcmp(backend, "null")
		&& strcmp(backend, "opengl")
		&& strcmp(backend, "vulkan")
		&& strcmp(backend, "d3d11")
		&& strcmp(backend, "d3d12")
		&& strcmp(backend, "metal"))
	{
		GFX_Message(NULL, "Unknown backend \"%s\"", backend);
	}
	else
	{
		SETTINGS_write_string("rhi_backend", backend);
	}
}

const char *GL_get_backend(void)
{
	const char *rhi_backend = SETTINGS_read_string("rhi_backend", "");

	if (!strcmp(rhi_backend, "null"))
		return "null";
	else if (!strcmp(rhi_backend, "opengl"))
		return "opengl";
	else if (!strcmp(rhi_backend, "vulkan"))
		return "vulkan";
	else if (!strcmp(rhi_backend, "d3d11"))
		return "d3d11";
	else if (!strcmp(rhi_backend, "d3d12"))
		return "d3d12";
	else if (!strcmp(rhi_backend, "metal"))
		return "metal";
	else {
		// Platform default when no backend has been configured.

#if FOR_WINDOWS
		if (strcmp(rhi_backend, "null")
			&& strcmp(rhi_backend, "opengl")
			&& strcmp(rhi_backend, "vulkan")
			&& strcmp(rhi_backend, "d3d11")
			&& strcmp(rhi_backend, "d3d12"))
		{
			return "d3d11";
		}
#elif FOR_MACOSX
		if (strcmp(rhi_backend, "null")
			&& strcmp(rhi_backend, "metal")
			//&& strcmp(rhi_backend, "vulkan") // maybe later.
			)
		{
			return "metal";
		}
#elif FOR_LINUX && QT_CONFIG(vulkan)
		if (strcmp(rhi_backend, "null")
			&& strcmp(rhi_backend, "opengl")
			&& strcmp(rhi_backend, "vulkan")
			)
		{
			return "vulkan";
		}
#elif FOR_LINUX && !QT_CONFIG(vulkan)
		if (strcmp(rhi_backend, "null")
			&& strcmp(rhi_backend, "opengl"))
		{
			return "opengl";
		}
#else
#  error "unknown architecture"
#endif
	}

	return rhi_backend;
}

static bool g_pause_rendering_on_off = false;

static void init_g_pause_rendering_on_off(void){
  g_pause_rendering_on_off = SETTINGS_read_bool("pause_rendering", false);
}

void GL_set_pause_rendering_on_off(bool onoff){
  SETTINGS_write_bool("pause_rendering", onoff);
  g_pause_rendering_on_off = onoff;
}

bool GL_get_pause_rendering_on_off(void){
  return g_pause_rendering_on_off;
}


QWidget *GL_create_widget(QWidget *parent)
{
	init_g_pause_rendering_on_off();

	QRhi::Implementation graphicsApi = init_qrhi();
	
	g_window = new RenderWindow(graphicsApi);

#if QT_CONFIG(vulkan)
	if (graphicsApi == QRhi::Vulkan && g_vulkan_inst != nullptr)
		g_window->setVulkanInstance(g_vulkan_inst);
#endif

	g_widget = QWidget::createWindowContainer(g_window);

	g_gl_widget_started = true;
	
	return g_widget;
}


void GL_stop_widget(QWidget *widget)
{
	R_ASSERT(widget == g_widget);

	// First delete the widget (which may use the Vulkan instance internally),
	// then tear down the global Vulkan instance to ensure Qt has finished
	// cleaning up any Vulkan-related state.
	delete widget;
	g_widget = NULL;

#if QT_CONFIG(vulkan)
	if (g_vulkan_inst)
	{
		g_vulkan_inst->destroy();
		delete g_vulkan_inst;
		g_vulkan_inst = nullptr;
	}
#endif
}
