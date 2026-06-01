
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
#include "Text.hpp"
//#include "T2.hpp"
#include "Timing.hpp"
#include "Render_proc.h"
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

extern r::TextureVertices *g_texture_vertices;

namespace
{

struct TextureRenderer
{
	r::TextureVertices *_vertices = nullptr;
	r::TextureAtlas *_texture_atlas = nullptr;

	QRhiBuffer *_clipCorrBuffer = nullptr;
    QRhiBuffer *_scrollBuffer = nullptr;
    QRhiGraphicsPipeline *_pipeline;

	int _num_vertices_in_buffer = 0;

	void init(QRhi *rhi, QRhiRenderPassDescriptor *render_pass_descriptor, const QFont &font)
	{
		init_verticess(rhi);
		
		_clipCorrBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
										 QRhiBuffer::UniformBuffer,
										 sizeof(QMatrix4x4) + sizeof(float));
		
		if (!_clipCorrBuffer || !_clipCorrBuffer->create())
		{
			qDebug() << "Failed to create clip correction buffer";
			getchar();
			//return false;
		}
		
		_scrollBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
									   QRhiBuffer::UniformBuffer,
									   sizeof(float));
		
		if (!_scrollBuffer || !_scrollBuffer->create())
		{
			qDebug() << "Failed to create scroll correction buffer";
			getchar();
			//return false;
		}
		
		//QFont font("Cousine", 14, QFont::Normal);
		//font.setStyleStrategy(QFont::PreferAntialias);
		QString supportedChars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVW #-,.(){}<>=*:0123456789";
		
		_texture_atlas = new r::TextureAtlas(rhi, font, supportedChars, _clipCorrBuffer, _scrollBuffer);
		
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
		
		if (!_pipeline->create()) {
			qDebug() << "Failed to create pipeline";
			getchar();
			//return false;
		}
	}
	
    void release(void)
    {
        delete _vertices;
		
        delete _pipeline;
        //delete _ubuf;

        _vertices = nullptr;

        _pipeline = nullptr;
        //_ubuf = nullptr;
    }
	
	void add_text(const char *text, int x, int y, float r, float g, float b, float a)
	{
		if (_texture_atlas)
			_texture_atlas->appendString(_vertices, text, x, y, 20, 20, r, g, b, a);
		//g_window->_texture_renderer->add_text(gc, text, x, y);
	}

	bool _vertexDataDirty = true;
	
    void prepare_frame(QRhi *rhi,
					   QRhiResourceUpdateBatch *batch,
					   const QMatrix4x4 view_projection,
					   float scroll_pos,
					   float width, float height)
    {
		static float _y_inc = -height;
		
        _y_inc += 0.5f;
        if (_y_inc > 0) {
            _y_inc = -height;
        }

        _texture_atlas->uploadTexture(batch);

		_vertices->maybe_merge_in(batch);
		
        if (_clipCorrBuffer)
		{
#if 0
            QMatrix4x4 clipCorr = rhi->clipSpaceCorrMatrix();
            batch->updateDynamicBuffer(_clipCorrBuffer, 0, sizeof(QMatrix4x4), clipCorr.constData());
#else
			batch->updateDynamicBuffer(_clipCorrBuffer, 0, sizeof(QMatrix4x4), view_projection.constData());
#endif
            batch->updateDynamicBuffer(_clipCorrBuffer,
									   sizeof(QMatrix4x4),
									   sizeof(float),
									   &scroll_pos);
        }
		
        if (_scrollBuffer) {
			//float yscroll = _y_inc / height;
            batch->updateDynamicBuffer(_scrollBuffer,
									   0,
									   sizeof(float),
									   &scroll_pos);
        }
	}

	void render_frame(QRhi *rhi, QRhiCommandBuffer *command_buffer)
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
		g_texture_vertices = _vertices;
		
        //_vertices->call_me_when_finished_painting(rhi);

    }
};

struct TriangleRenderer
{
    r::TriangleVertices *_vertices1 = nullptr;
    r::TriangleVertices *_vertices2 = nullptr;

    QRhiShaderResourceBindings *_shader_resource_bindings = nullptr;
    QRhiGraphicsPipeline *_pipeline = nullptr;

    QRhiBuffer *_ubuf = nullptr;

    void init(QRhi *rhi,
              QRhiRenderPassDescriptor *render_pass_descriptor)
	{
        init_verticess(rhi);
		
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
		
        _shader_resource_bindings->setBindings(
            bindings.cbegin(),
            bindings.cend());
		
        _shader_resource_bindings->create();

        _pipeline = rhi->newGraphicsPipeline();

#if DO_ANTIALIASING
        _pipeline->setSampleCount(4);
#endif
		
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
		delete _vertices1;
        delete _vertices2;

        delete _shader_resource_bindings;
        delete _pipeline;
        delete _ubuf;
		
        _vertices1 = nullptr;
        _vertices2 = nullptr;
		
        _shader_resource_bindings = nullptr;
        _pipeline = nullptr;
        _ubuf = nullptr;
    }

    void prepare_frame(QRhi *rhi,
					   QRhiResourceUpdateBatch *batch,
					   const QMatrix4x4 &viewProjection,
					   float scrollPos)
    {
        for (r::TriangleVertices *context : ALL_VERTICESS)
        {
            if (context)
            {
                context->maybe_merge_in(batch);
            }
        }

        if (_vertices1)
            _vertices1->maybe_merge_in(batch);
		
        if (_vertices2)
            _vertices2->maybe_merge_in(batch);
		
        batch->updateDynamicBuffer(_ubuf,
								   0,
								   64,
								   viewProjection.constData());

        batch->updateDynamicBuffer(_ubuf,
								   64,
								   sizeof(float),
								   &scrollPos);
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

        for (r::TriangleVertices *context : ALL_VERTICESS)
        {
            if (context)
            {
                context->render(command_buffer);
            }
        }

        if (_vertices1)
            _vertices1->render(command_buffer);
		
        if (_vertices2)
            _vertices2->render(command_buffer);
    }
	
private:
	
    void init_verticess(QRhi *rhi)
	{
		_vertices1 = new r::TriangleVertices;
		
		//init_test_triangles(_vertices1, 0);
		
        //_vertices1->call_me_when_finished_painting(rhi);
		
        _vertices2 = new r::TriangleVertices;
		
#if 0
        int range = 150;
		
        for (int i = 0; i < range; ++i)
            init_test_triangles(_vertices2, 0.3f + i);
		
        for (int i = 0; i < range; ++i)
            init_test_triangles(_vertices2, 5.3f - i);
#endif
        //_vertices2->call_me_when_finished_painting(rhi);
    }
};


class RenderWindow : public radium::RhiWindow, public radium::MouseCycleFix
{
	
public:

	TextureRenderer _texture_renderer;
	
	TriangleRenderer _triangle_renderer;
	
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
				_triangle_renderer.release();
				_texture_renderer.release();
				
				fprintf(stderr, "H5\n");
				sem.release();
			});

		fprintf(stderr, "H6\n");
		sem.acquire();
		fprintf(stderr, "H7\n");
	}

	void customInit(const QFont &font) override
	{

		// Texture renderer
		_texture_renderer.init(_rhi, _render_pass_descriptor, font);

		// Triangle renderer
		_triangle_renderer.init(_rhi, _render_pass_descriptor);

		
		double ratio = devicePixelRatio();

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
		
		QRhiResourceUpdateBatch *batch = _rhi->nextResourceUpdateBatch();

		// Triangles
		_triangle_renderer.prepare_frame(_rhi,
										 batch,
										 _viewProjection,
										 scroll_pos);
		
		// Textures (i.e. text)
		_texture_renderer.prepare_frame(_rhi,
										batch,
										_viewProjection,
										scroll_pos,
										outputSizeInPixels.width(),
										outputSizeInPixels.height());
		
		QRhiCommandBuffer *command_buffer = _swap_chain->currentFrameCommandBuffer();

		command_buffer->beginPass(_swap_chain->currentFrameRenderTarget(), g_background_color, { 1.0f, 0 }, batch);
		{
			// triangles
			_triangle_renderer.render_frame(command_buffer,
											outputSizeInPixels);

			// text
			_texture_renderer.render_frame(_rhi,
										   command_buffer);
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
static QVulkanInstance g_vulkan_inst;
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
        // Request validation, if available. This is completely optional
        // and has a performance impact, and should be avoided in production use.
        g_vulkan_inst.setLayers({ "VK_LAYER_KHRONOS_validation" });
        // Play nice with QRhi.
        g_vulkan_inst.setExtensions(QRhiVulkanInitParams::preferredInstanceExtensions());
        if (!g_vulkan_inst.create()) {
            qWarning("Failed to create Vulkan instance, switching to OpenGL");
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

extern void gakk_GE_text(const char *text, int x, int y, float r, float g, float b, float a);
void gakk_GE_text(const char *text, int x, int y, float r, float g, float b, float a)
{
	g_window->_texture_renderer.add_text(text, x, y, r, g, b, a);
}

void GE_set_font(const QFont &font)
{
	if (g_window)
	{
		g_window->_texture_renderer._texture_atlas->setFont(font);
		GFX_ForceScheduleEditorRedraw(); // New font will be set before starting new paint.
	}
}

void aiai(void);
void aiai(void) // Called after finished rendering.
{
	if (g_window)
		if (g_window->_texture_renderer._texture_atlas)
			g_window->_texture_renderer._texture_atlas->maybe_switch_to_next_d();
}


void GL_set_new_painting_data(r::PaintingData *painting_data, GE_Rgb new_background_color)
{	
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
	if (graphicsApi == QRhi::Vulkan)
		g_window->setVulkanInstance(&g_vulkan_inst);
#endif

	g_widget = QWidget::createWindowContainer(g_window);

	g_gl_widget_started = true;
	
	return g_widget;
}


void GL_stop_widget(QWidget *widget)
{
	R_ASSERT(widget == g_widget);
	delete widget;
	g_widget = NULL;
}
