
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



#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <bitset>
#include <vector>

#include <QFile>
//#include <QCommandLineParser>

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
#include "../Qt/EditorWidget.h"

#include "../audio/Juce_plugins_proc.h"

#include "../api/api_gui_proc.h"

#include "RhiWindow.hpp"
#include "GfxElements.h"
#include "Vertices.hpp"
#include "TextureAtlas.hpp"
#include "Timing.hpp"
#include "Render_proc.h"

#include "Widget_proc.h"


DEFINE_ATOMIC(char *, GE_vendor_string) = strdup("TODO/FIX: vendor-string not set by Radium yet");

static DEFINE_ATOMIC(int, g_curr_realline);

// TS (called from both main thread and opengl thread)
void GE_set_curr_realline(int curr_realline){
  //printf("  ############      Setting g_curr_realline to %d\n", curr_realline);
  ATOMIC_SET(g_curr_realline, curr_realline);
}

#if 1
// OpenGL thread
static float QRHI_GE_scroll_pos(const SharedVariables &sv, double realline){
  R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

  double extra = sv.top_realline - sv.curr_realline;
  return
    (   (realline+extra) * sv.fontheight  );
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
static double QRHI_get_realline_stime(const SharedVariables &sv, int realline)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	double blocktime;
	if(realline==sv.num_reallines)
		blocktime = sv.block_duration;
	else
		blocktime = Place2STime_from_times2(sv.times, p_getDouble(sv.reallines[realline]->l.p));
  
	return blocktime_to_seqtime_double(sv.seqblock_stretch, blocktime);
}
#endif

#if 1
// OpenGL thread
static bool QRHI_need_to_reset_timing(const SharedVariables &sv, double stime, int last_used_i_realline, const struct Blocks *last_used_block, double last_used_stime, double blocktime)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	if (stime < 0){
		fprintf(stderr,"Error: stime: %f, pc->blocktime: %f\n",stime,blocktime);
#if 0
#if !defined(RELEASE)
        abort();
#endif
#endif
		return true;
	}

	if (last_used_block != sv.block)    
		return true;
  
	if(last_used_i_realline>=sv.num_reallines) // First check that i_realline is within the range of the block. (block might have changed number of lines)
		return true;
    
	// TODO: Make the "last_stime < stime"-check configurable.
	if (stime < last_used_stime)
		return true;
  
	if(stime < QRHI_get_realline_stime(sv, last_used_i_realline)) // Time is now before the line we were at when we left last time. Start searching from 0 again. (Not sure if is correct. It might be last_used_i_realline+1 instead)
		return true;

	return false;
}
#endif

#if 1
// OpenGL thread
static double QRHI_find_current_realline_while_playing(const SharedVariables &sv, double blocktime)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	double time_in_ms = blocktime * 1000.0 / (double)pc->pfreq; // I'm not entirely sure reading pc->start_time_f instead of pc->start_time is unproblematic.
	
	double stime = time_estimator.get(time_in_ms,
									  sv.reltempo * ATOMIC_DOUBLE_GET(g_curr_song_tempo_automation_tempo))
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

	if (QRHI_need_to_reset_timing(sv, stime, i_realline, block, last_stime, blocktime)) {
		i_realline = 0;
		block = sv.block;
		time_estimator.set_time(time_in_ms);
		stime = time_in_ms * (double)pc->pfreq / 1000.0; // Convert the current block time into number of frames.
	}

	//  stime -= 24000;
      
	last_stime = stime;
  
	double stime2 = QRHI_get_realline_stime(sv, i_realline);
  
	while(true){

		double stime1 = stime2;
		for(;;){ // This for loop is here to handle a very special situation where we play so fast that stime1==stime2. In normal songs, this should not happen.
			stime2 = QRHI_get_realline_stime(sv, i_realline+1);

#if 0
			if (stime1==stime2){ // Could probably happen if playing really fast... Not sure. (yes, it happens if playing really fast)
#if !defined(RELEASE)
				/abort();
#endif
				return i_realline;
			}
#endif
      
			if (i_realline==sv.num_reallines)
				return sv.num_reallines;
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

		if (i_realline==sv.num_reallines)
			break;
	}

	return sv.num_reallines;
}

static bool QRHI_find_scrollpos(const SharedVariables &sv, double &scroll_pos, double &current_realline_while_playing_out)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	const int player_id = ATOMIC_GET(pc->play_id);
	bool is_playing = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;

	if (is_playing)
		if (sv.block_is_visible==false || sv.block!=sv.curr_playing_block)
			is_playing = false; // I.e. we are not rendering the block that is currently playing (if any).

    double blocktime = 0.0;
	
    int playing_blocknum = -1;
	
    if (is_playing){
		
#if 0
		if ((sv.curr_playing_block==NULL || sv.block!=sv.curr_playing_block)) { // Check that our blocktime belongs to the block that is rendered.
        
			//if (new_t2_data!=NULL && use_t2_thread)
			//  T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();
        
			if (t2_data_can_be_used){
				//printf("Waiting...\n");
				//_rendering->QRHI_render();
				return true;
			}else{

				//printf("Retfalse2. old_t2_datas.size: %d. sv.curr_playing_block==NULL (%d) || sv.block!=sv.curr_playing_block (%d)\n",old_t2_datas.size(), sv.curr_playing_block==NULL, sv.block!=sv.curr_playing_block);
				//printf("  Wait.gakk\n");
				return false; // Returning false uses 100% CPU on Intel gfx / Linux, and could possibly cause jumpy graphics, but here we are just waiting for the block to be rendered.
			}
		}
#endif
		
		playing_blocknum = sv.curr_playing_block->l.num;
        
		blocktime = ATOMIC_DOUBLE_GET(sv.curr_playing_block->player_time);

		if (equal_doubles(blocktime, -100.0))
			return false; // we just switched block and waiting for a proper blocktime to be calculated, I think.
																	  
		
		//if (blocktime < -50)
		//  printf("blocktime: %f\n",blocktime);
#if 0
		if (blocktime < 0.0) {  // Either the block hasn't started playing yet (sequencer cursor is inside a pause), or we just switched block and waiting for a proper blocktime to be calculated.
			
			//if (new_t2_data!=NULL && use_t2_thread)
			//  T3_t2_data_picked_up_but_old_data_will_be_sent_back_later();

			if (t2_data_can_be_used  || !equal_doubles(blocktime, -100.0)){
				_rendering->QRHI_render();
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
		? QRHI_find_current_realline_while_playing(sv, blocktime)
		: 0.0;
    
    R_ASSERT_NON_RELEASE(current_realline_while_playing >= 0);

    int current_realline_while_not_playing = ATOMIC_GET(g_curr_realline);
    
    double till_realline =
		ATOMIC_GET_RELAXED(sv.root->play_cursor_onoff)
		? current_realline_while_not_playing
		: is_playing
		? current_realline_while_playing
		: current_realline_while_not_playing;
	
    Play_set_curr_playing_realline(
		is_playing ? (int)current_realline_while_playing : current_realline_while_not_playing,
		playing_blocknum
		);
    
    scroll_pos = QRHI_GE_scroll_pos(sv, till_realline);

    current_realline_while_playing_out = current_realline_while_playing;
    
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
			//_rendering->QRHI_render();
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


// Main thread
static Tracker_Windows *get_window(void){
  return root->song->tracker_windows;
}

// Main thread
static EditorWidget *get_editorwidget(void){
  return (EditorWidget *)get_window()->os_visual.widget;
}


volatile float g_scroll_pos = 0.0f;
int g_msaa_samples = 8; // set on main thread, read by QRHI thread

static DEFINE_ATOMIC(double, g_vblank) = 1000 / 60.0;

void GL_update(void)
{	
	//if (SCHEME_is_currently_getting_scheme_history()) // Avoid deadlock when assertion reporter is showing.
	//	return;
}

static QColor g_background_color = Qt::black; // Accessed from render thread only.

static QShader QRHI_getShader(const QString &name)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

    QFile f(name);
    if (f.open(QIODevice::ReadOnly))
        return QShader::fromSerialized(f.readAll());

    return QShader();
}



QRhi *g_rhi = NULL;



enum class RendererFlags : uint16_t
{
	None             = 0,
	IsScrolling      = 1 << 0,
	UseScissors      = 1 << 1,
	UseBlending      = 1 << 2,
	CreateBuffer     = 1 << 3,
	CreatePipeline   = 1 << 4,
};

constexpr RendererFlags operator|(RendererFlags a, RendererFlags b)
{
	return static_cast<RendererFlags>(static_cast<uint16_t>(a) | static_cast<uint16_t>(b));
}

constexpr RendererFlags operator&(RendererFlags a, RendererFlags b)
{
	return static_cast<RendererFlags>(static_cast<uint16_t>(a) & static_cast<uint16_t>(b));
}

constexpr bool has_flag(RendererFlags flags, RendererFlags flag)
{
	return (static_cast<uint16_t>(flags) & static_cast<uint16_t>(flag)) != 0;
}

namespace
{

struct TextureRenderer
{
	r::TextureVertices *_vertices = nullptr;
	
	r::TextureAtlasBackend *_texture_atlas_backend = nullptr;
	r::TextureAtlas *_texture_atlas = nullptr;

	QRhiBuffer *_viewCorrectionBuffer = nullptr;
    QRhiBuffer *_scrollPosBuffer = nullptr;
    QRhiGraphicsPipeline *_pipeline;

	bool _is_scrolling = true;
	bool _use_scissors = true;

	void init(QRhi *rhi,
			  r::TextureAtlasBackend *texture_atlas_backend,
			  QRhiRenderPassDescriptor *render_pass_descriptor,
			  RendererFlags flags)
	{
		_is_scrolling = has_flag(flags, RendererFlags::IsScrolling);
		_use_scissors = has_flag(flags, RendererFlags::UseScissors);
		
		_texture_atlas_backend = texture_atlas_backend;
		
		init_verticess();
		
		_viewCorrectionBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
										 QRhiBuffer::UniformBuffer,
										 sizeof(QMatrix4x4) + sizeof(float));
		
		if (!_viewCorrectionBuffer || !_viewCorrectionBuffer->create())
		{
			GFX_Message(NULL, "Failed to create clip correction buffer"); // Should never happen, so we don't care about avoiding crash or misbheaviors that may happen later in code if this happens.
			
			//getchar();
			//return false;
		}
		
		_scrollPosBuffer = rhi->newBuffer(QRhiBuffer::Dynamic,
									   QRhiBuffer::UniformBuffer,
									   sizeof(float));
		
		if (!_scrollPosBuffer || !_scrollPosBuffer->create())
		{
			GFX_Message(NULL, "Failed to create scroll correction buffer"); // Should never happen, so we don't care about avoiding crash or misbheaviors that may happen later in code if this happens.
			
			//getchar();
			//return false;
		}
		
		//QFont font("Cousine", 14, QFont::Normal);
		//font.setStyleStrategy(QFont::PreferAntialias);
		//QString supportedChars = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVW #-,.(){}<>=*:0123456789";
		
		_texture_atlas = new r::TextureAtlas(rhi, texture_atlas_backend, _viewCorrectionBuffer, _scrollPosBuffer);
		
		QShader vertexShader = QRHI_getShader("texture_vertex.qsb");
		QShader fragmentShader = QRHI_getShader("texture_fragment.qsb");
		
		if (!vertexShader.isValid() || !fragmentShader.isValid())
		{
			GFX_Message(NULL, "Failed to load compiled shaders");
			//getchar();
		}
		
		_pipeline = rhi->newGraphicsPipeline();
		
		if (!_pipeline)
		{
			GFX_Message(NULL, "Failed to create graphics pipeline");
			//getchar();
		}
		
		if (g_msaa_samples > 1)
			_pipeline->setSampleCount(g_msaa_samples);

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
		_pipeline->setShaderResourceBindings(_texture_atlas->QRHI_getShaderBindings());
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
			GFX_Message(NULL, "Failed to create pipeline");
			//getchar();
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
    }
	
	void MAIN_add_text(const QString &text, int x, int y, float r, float g, float b, float a)
	{
		if (_texture_atlas)
			_texture_atlas->MAIN_appendStringToVertices(_vertices,
														text,
														x, y,
														r, g, b, a);
		//g_window->_texture_renderer->add_text(gc, text, x, y);
	}

    void QRHI_prepare_frame(QRhi *rhi,
							QRhiResourceUpdateBatch *batch,
							const QMatrix4x4 view_projection,
							float scroll_pos)
	//float width, float height)
    {
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		_vertices->QRHI_maybe_merge_in(batch);
			
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

	void QRHI_render_frame(QRhiCommandBuffer *command_buffer)
    {
        R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

        if (_pipeline && _vertices && _vertices->QRHI_has_vertices())
		{
            command_buffer->setGraphicsPipeline(_pipeline);
            command_buffer->setShaderResources(_texture_atlas->QRHI_getShaderBindings());

			_vertices->QRHI_render(command_buffer);
        }
    }

    void init_verticess(void)
    {
        _vertices = new r::TextureVertices;
		//g_texture_vertices = _vertices;
		
        //_vertices->call_me_when_finished_painting(rhi);

    }
};

struct TriangleRenderer
{
	r::TriangleVertices _vertices;
	
    QRhiShaderResourceBindings *_shader_resource_bindings = nullptr;
    QRhiGraphicsPipeline *_pipeline = nullptr;

	QRhiBuffer *_ubuf = nullptr;
	bool _is_scrolling = true;
	bool _use_scissors = true;

    void init(QRhi *rhi,
              QRhiRenderPassDescriptor *render_pass_descriptor,
              RendererFlags flags)
	{
		_is_scrolling = has_flag(flags, RendererFlags::IsScrolling);
		_use_scissors = has_flag(flags, RendererFlags::UseScissors);
		
		bool use_blending = has_flag(flags, RendererFlags::UseBlending);
		bool create_buffer = has_flag(flags, RendererFlags::CreateBuffer);
		bool create_pipeline = has_flag(flags, RendererFlags::CreatePipeline);

        if (create_buffer)
        {
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
        }

        if (create_pipeline)
        {
            _pipeline = rhi->newGraphicsPipeline();

            if (g_msaa_samples > 1)
                _pipeline->setSampleCount(g_msaa_samples);
            
            if (_use_scissors)
                _pipeline->setFlags(QRhiGraphicsPipeline::UsesScissor);
            
            if (use_blending)
			{
                QRhiGraphicsPipeline::TargetBlend blend;
                blend.enable = true;
                _pipeline->setTargetBlends({blend});
            }

            _pipeline->setShaderStages({
                    {
                        QRhiShaderStage::Vertex,
                        QRHI_getShader("color.vert.qsb")
                    },
                    {
                        QRhiShaderStage::Fragment,
                        QRHI_getShader("color.frag.qsb")
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
    }

    void release(void)
	{
		//delete _vertices1;
        //delete _vertices2;

        if (_shader_resource_bindings)
        {
            _shader_resource_bindings->destroy();
            delete _shader_resource_bindings;
        }
        if (_pipeline)
        {
            _pipeline->destroy();
            delete _pipeline;
        }
        if (_ubuf)
        {
            _ubuf->destroy();
            delete _ubuf;
        }
		
        //_vertices1 = nullptr;
        //_vertices2 = nullptr;
		
        _shader_resource_bindings = nullptr;
        _pipeline = nullptr;
        _ubuf = nullptr;
    }

	void MAIN_call_me_before_adding_triangles(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		_vertices.MAIN_call_me_when_starting_to_generate_vertices();
	}
	
    void QRHI_prepare_frame(QRhi *rhi,
							QRhiResourceUpdateBatch *batch,
							const QMatrix4x4 &viewProjection,
							float scrollPos)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		_vertices.QRHI_maybe_merge_in(batch);
			
        if (!_is_scrolling) {
            batch->updateDynamicBuffer(_ubuf,
            						   0,
            						   64,
            						   viewProjection.constData());
        } else {
            // Scrolling renderers share a single ubuf; skip per-renderer upload.
        }
    }
	
    void QRHI_render_frame(QRhiCommandBuffer *command_buffer,
                           QRhiGraphicsPipeline *pipeline = nullptr,
                           QRhiShaderResourceBindings *sbr = nullptr)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		if (!_vertices.QRHI_has_vertices())
			return;

		QRhiGraphicsPipeline *p = pipeline ? pipeline : _pipeline;

		if (!p)
		{
			R_ASSERT_NON_RELEASE(false);
			return;
		}

		if (sbr == NULL)
		{
			if (_shader_resource_bindings == NULL)
			{
				R_ASSERT_NON_RELEASE(false);
				return;
			}
		}
		else
		{
			R_ASSERT_NON_RELEASE(_shader_resource_bindings == NULL || _shader_resource_bindings == sbr);
		}
		
		command_buffer->setGraphicsPipeline(p);
		command_buffer->setShaderResources(sbr);

		_vertices.QRHI_render(command_buffer);
    }

	
	// Add triangle without splitting across renderers
	void MAIN_add_triangle(const GE_Context &c, const r::Triangle &triangle, r2::GradientType::Type gradient_type)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		switch(gradient_type)
		{
			case r2::GradientType::Type::NOTYPE:
				_vertices.MAIN_add_triangle(triangle, c.color.c);
				break;
			case r2::GradientType::Type::HORIZONTAL:
				_vertices.MAIN_add_triangle_horizontal_gradient(triangle, c.color.c, c.color.c_gradient);
				break;
			case r2::GradientType::Type::VELOCITY:
				_vertices.MAIN_add_triangle_vertical_gradient(triangle, c.color.c, c.color.c_gradient);
				break;
		}
	}
};

#if !defined(RELEASE)
// Profiling: count opaque vs transparent triangle draws per frame
static int g_profile_opaque = 0;
static int g_profile_transparent = 0;
static int g_profile_solid = 0;
static int g_profile_gradient = 0;
static int g_profile_slice_dups = 0;
static int g_profile_frame_count = 0;
#endif

static bool g_render_window_has_been_deleted = false;

class RenderWindow : public radium::RhiWindow, public radium::MouseCycleFix
{
public:

	r::TextureAtlasBackend *_texture_atlas_backend = nullptr;
	r::TextureAtlasBackend *_texture_atlas_backend_halfsize = nullptr;

	TextureRenderer _texture_renderers[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_scissors[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_scissors_halfsize[MAX_NUM_SLICES];
	TextureRenderer _texture_renderer_static;
	
	//TriangleRenderer _triangle_renderers_background[MAX_NUM_SLICES];
	TriangleRenderer _triangle_renderers[MAX_NUM_SLICES];
	TriangleRenderer _triangle_renderer_scissors[MAX_NUM_SLICES];

	TriangleRenderer _triangle_renderer_scrollbar;  // single non-sliced renderer for scrollbar
	TriangleRenderer _triangle_renderer_node_indicator;  // single non-sliced foreground renderer for node indicator
	TriangleRenderer _triangle_renderer_playcursor;  // single non-sliced renderer for playcursor (custom scroll_pos)
	
	TriangleRenderer _triangle_renderer_static;

	r::PaintingData *_painting_data = nullptr;

	
public:

	RenderWindow(QRhi::Implementation graphicsApi)
		: RhiWindow(graphicsApi)
	{
	}

	template <typename F>
	void for_each_renderer(int tri_start, int tri_end, int text_start, int text_end, F func)
	{
		// Triangles.
		//
		for (int i = tri_start; i < tri_end; ++i)
		{
			func(_triangle_renderers[i]);
			func(_triangle_renderer_scissors[i]);
		}
		
		func(_triangle_renderer_static);
		func(_triangle_renderer_scrollbar);
		func(_triangle_renderer_node_indicator);
		func(_triangle_renderer_playcursor);

		// Textures.
		//
		for (int i = text_start; i < text_end; ++i)
		{
			func(_texture_renderers[i]);
			func(_texture_renderer_scissors[i]);
			func(_texture_renderer_scissors_halfsize[i]);
		}
		
		func(_texture_renderer_static);
	}

	template <typename F>
	void for_each_renderer(F func)
	{
		for_each_renderer(0, MAX_NUM_SLICES, 0, MAX_NUM_SLICES, func);
	}

	template <typename F>
	void for_each_vertices(F func)
	{
		for_each_renderer([func](auto &r)
			{
				if constexpr (std::is_pointer_v<decltype(r._vertices)>)
				{
					if (r._vertices)
						func(*r._vertices);
				}
				else
				{
					func(r._vertices);
				}
			});
	}

	~RenderWindow()
	{
		fprintf(stderr, "H1\n");

		// All QRhi resources must be freed on the RHI thread while _rhi is still alive.
		// Non-QRhi cleanup (_painting_data) is done outside on the main thread.
		MAIN_put_event_sync([this]()
			{
				for_each_renderer([](auto &r){ r.release(); });
				
				delete _texture_atlas_backend;
				delete _texture_atlas_backend_halfsize;
				
				fprintf(stderr, "H5\n");
			});

		fprintf(stderr, "H6\n");
		fprintf(stderr, "H7\n");
		
		delete _painting_data;

		g_render_window_has_been_deleted = true;
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

	void MAIN_setFont(const QFont &font)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		if (_texture_atlas_backend && _texture_atlas_backend_halfsize)
		{
			_texture_atlas_backend->MAIN_setFont(font);
			_texture_atlas_backend_halfsize->MAIN_setFont(get_halfsize_font(font));
		}
	}
	
	void QRHI_customInit(const QFont &font) override
	{
		_texture_atlas_backend = new r::TextureAtlasBackend(_rhi, font);
		_texture_atlas_backend_halfsize = new r::TextureAtlasBackend(_rhi, get_halfsize_font(font));
		
		for (int i = 0; i < MAX_NUM_SLICES; ++i)
		{
			// Texture renderers (scrolling, tempo tracks, per-slice)
			_texture_renderers[i].init(_rhi, _texture_atlas_backend, _render_pass_descriptor,
									   RendererFlags::IsScrolling);

			// Triangle renderers (per-slice, scrolling). Only [0] creates its own pipeline.
			_triangle_renderers[i].init(_rhi, _render_pass_descriptor,
			                            RendererFlags::IsScrolling |
										RendererFlags::UseScissors |
										RendererFlags::UseBlending |
										(i == 0 ? RendererFlags::CreateBuffer | RendererFlags::CreatePipeline : RendererFlags::None));

			// Texture renderers (scrolling, normal tracks, per-slice scissored)
			_texture_renderer_scissors[i].init(_rhi, _texture_atlas_backend, _render_pass_descriptor,
											   RendererFlags::IsScrolling |
											   RendererFlags::UseScissors);
			
			_texture_renderer_scissors_halfsize[i].init(_rhi, _texture_atlas_backend_halfsize, _render_pass_descriptor,
														RendererFlags::IsScrolling |
														RendererFlags::UseScissors);

			// Triangle renderers (per-slice, scissored). Use shared scrolling pipeline
			_triangle_renderer_scissors[i].init(_rhi, _render_pass_descriptor,
			                                    RendererFlags::IsScrolling |
												RendererFlags::UseScissors |
												RendererFlags::UseBlending);
		}

		// Non-scrolling texture renderer
		_texture_renderer_static.init(_rhi, _texture_atlas_backend, _render_pass_descriptor,
									  RendererFlags::None);

		// Non-scrolling triangle renderer
		_triangle_renderer_static.init(_rhi, _render_pass_descriptor,
		                               RendererFlags::UseBlending |
									   RendererFlags::CreateBuffer |
									   RendererFlags::CreatePipeline);
		
		// Scrollbar renderer (non-sliced, non-scrolling, non-scissored, no blending)
		_triangle_renderer_scrollbar.init(_rhi, _render_pass_descriptor,
		                                  RendererFlags::CreateBuffer |
		                                  RendererFlags::CreatePipeline);
		
		// Node indicator renderer (non-sliced, scrolling, non-scissored, no blending, uses shared pipeline)
		_triangle_renderer_node_indicator.init(_rhi, _render_pass_descriptor,
		                                       RendererFlags::IsScrolling);

		// Playcursor renderer (non-sliced, scrolling, non-scissored, blending, owns ubuf+sbr, uses shared pipeline)
		_triangle_renderer_playcursor.init(_rhi, _render_pass_descriptor,
		                                   RendererFlags::IsScrolling |
										   RendererFlags::UseBlending |
										   RendererFlags::CreateBuffer);
		
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
				//getchar();
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
	
	void QRHI_customRender(void) override
	{

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

		if (_painting_data == nullptr) // Note: Only happens during startup, if at all.
			return;
		
		auto &sv = _painting_data->shared_variables;

#if !defined(RELEASE)
		// Profile: print opaque/transparent triangle counts
		{
			g_profile_frame_count++;
			if (g_profile_frame_count % 60 == 0) {
				int total = g_profile_opaque + g_profile_transparent;
				if(0)
				printf("--- TRI PROFILE frame #%d: %d triangles (%d opaque %.0f%%, %d transparent %.0f%%), "
					   "%d solid %d gradient, %d slice duplications (avg %.1f/tri)\n",
					   g_profile_frame_count, total,
					   g_profile_opaque, total > 0 ? 100.0 * g_profile_opaque / total : 0.0,
					   g_profile_transparent, total > 0 ? 100.0 * g_profile_transparent / total : 0.0,
					   g_profile_solid, g_profile_gradient,
					   g_profile_slice_dups, total > 0 ? (float)g_profile_slice_dups / total : 0.0f);
				
				int opaque_verts = 0, transp_verts = 0;
				for (int i = 0; i < MAX_NUM_SLICES; ++i) {
					transp_verts += _triangle_renderers[i]._vertices.QRHI_has_vertices() ? 1 : 0;
					transp_verts += _triangle_renderer_scissors[i]._vertices.QRHI_has_vertices() ? 1 : 0;
				}
				if(0)
					printf("  Renderer slices: %d opaque, %d transparent\n",
						   opaque_verts, transp_verts);
				
				g_profile_opaque = 0;
				g_profile_transparent = 0;
				g_profile_solid = 0;
				g_profile_gradient = 0;
				g_profile_slice_dups = 0;
			}
		}
#endif
		
		const QSize outputSizeInPixels = _swap_chain->currentPixelSize();
		
		double scroll_pos = 0.0;
		double current_realline_while_playing = 0.0;

		if (!QRHI_find_scrollpos(sv, scroll_pos, current_realline_while_playing))
			return;

		if (ATOMIC_GET(pc->player_state) != PLAYER_STATE_PLAYING)
		{
			R_ASSERT(fabs(scroll_pos - round(scroll_pos)) < 0.001);
			scroll_pos = round(scroll_pos);
		}

		const float scroll_y1 = scroll_pos;
		const float scroll_y2 = scroll_pos + outputSizeInPixels.height();

		const float slice_size = _painting_data->slice_size;

		const int tri_slice_start = R_BOUNDARIES(0, scroll_y1 / slice_size, MAX_NUM_SLICES-1);
		const int tri_slice_end   = R_BOUNDARIES(tri_slice_start+1, ceilf(scroll_y2 / slice_size), MAX_NUM_SLICES);

		const int font_overspill = R_MAX(0, _texture_atlas_backend->QRHI_getFontHeight() - slice_size);
		
		const int text_slice_start = R_BOUNDARIES(0, (scroll_y1 - font_overspill) / slice_size, MAX_NUM_SLICES-1);
		const int text_slice_end   = tri_slice_end; //R_BOUNDARIES(text_slice_start+1, ceilf(scroll_y2 / slice_size), MAX_NUM_SLICES);

		//printf("slices: %d -> %d. scroll: %f -> %f. scroll_pos: %f\n", tri_slice_start, tri_slice_end, scroll_y1, scroll_y2, scroll_pos);

		QRhiResourceUpdateBatch *batch = _rhi->nextResourceUpdateBatch();


		_texture_atlas_backend->QRHI_uploadTexture(batch);
		_texture_atlas_backend_halfsize->QRHI_uploadTexture(batch);

		for_each_renderer(tri_slice_start, tri_slice_end,
						  text_slice_start, text_slice_end,
						  [this, batch, scroll_pos]
				(auto &r)
			{
				r.QRHI_prepare_frame(_rhi,
									 batch,
									 _viewProjection,
									 scroll_pos);
			});

		
		QRhiCommandBuffer *command_buffer = _swap_chain->currentFrameCommandBuffer();

		
#if !defined(RELEASE)
		// Diagnostic: check for blank frames
		{
			static int blank_count = 0;
			bool any_tri = false;
			for (int i = tri_slice_start; i < tri_slice_end && !any_tri; ++i)
				any_tri |= _triangle_renderers[i]._vertices.QRHI_has_vertices()
					|| _triangle_renderer_scissors[i]._vertices.QRHI_has_vertices();
			any_tri |= _triangle_renderer_static._vertices.QRHI_has_vertices();
			if (!any_tri) {
				blank_count++;
				printf("BLANK FRAME #%d: no tri verts. _painting_data=%p, slice_size=%f, tri_range=[%d,%d)\n",
					   blank_count, (void*)_painting_data, slice_size, tri_slice_start, tri_slice_end);
				//getchar();
			}
		}
#endif
		
		// Upload shared scrolling uniforms once (mvp + yscroll same for all slices)
		{
			batch->updateDynamicBuffer(_triangle_renderers[0]._ubuf, 0, 64, _viewProjection.constData());
			float scrollPos_f = (float)scroll_pos;
			batch->updateDynamicBuffer(_triangle_renderers[0]._ubuf, 64, sizeof(float), &scrollPos_f);
		}

		// Upload scrollbar uniforms (different scroll_pos)
		{
			batch->updateDynamicBuffer(_triangle_renderer_scrollbar._ubuf, 0, 64, _viewProjection.constData());
			float scrollbar_pos = 0.0f;
			{
				double till_realline = 0.0;
				bool is_playing = ATOMIC_GET(pc->player_state) == PLAYER_STATE_PLAYING;
				// Recompute till_realline from scroll_pos
				till_realline = scroll_pos / sv.fontheight - (sv.top_realline - sv.curr_realline);
				float y1 = get_scrollbar_scroller_y1(till_realline, sv.num_reallines - (is_playing ? 0 : 1),
				                                     sv.scrollbar_height, sv.scrollbar_scroller_height);
				scrollbar_pos = scale(y1, 0, sv.scrollbar_height, 0, -(float)sv.scrollbar_height);
			}
			batch->updateDynamicBuffer(_triangle_renderer_scrollbar._ubuf, 64, sizeof(float), &scrollbar_pos);
		}

		// Upload playcursor uniforms (different scroll_pos)
		{
			batch->updateDynamicBuffer(_triangle_renderer_playcursor._ubuf, 0, 64, _viewProjection.constData());
			
			float playcursor_pos = (float)(scroll_pos - QRHI_GE_scroll_pos(sv, current_realline_while_playing));
			
			batch->updateDynamicBuffer(_triangle_renderer_playcursor._ubuf, 64, sizeof(float), &playcursor_pos);
		}

		command_buffer->beginPass(_swap_chain->currentFrameRenderTarget(), g_background_color, { 1.0f, 0 }, batch);
		{
			command_buffer->setViewport({
					0,
					0,
					float(outputSizeInPixels.width()),
					float(outputSizeInPixels.height())
				});

			
			// scissor rectangle based on shared variables
			int scissor_x = 0;
			int scissor_w = 0;
			{
				scissor_x = int(sv.wtracks_scissor_x1 * g_opengl_scale_ratio);
				scissor_w = int((sv.wtracks_scissor_x2 - sv.wtracks_scissor_x1) * g_opengl_scale_ratio);
				if (scissor_x < 0)
					scissor_x = 0;

				/*
				printf("scissor_x: %d. scissor_x1: %d. scissor_w: %d. scissor_x2: %d\n",
					   scissor_x, sv.wtracks_scissor_x1,
					   scissor_w, sv.wtracks_scissor_x2);
				*/
			}


			auto *shared_scrolling_triangles_pipeline = _triangle_renderers[0]._pipeline;


			// triangles (scrolling). Non-scissored then scissored
			//
			{
				auto setScissor = [&](int i, int scissor_x, int scissor_w)
					{
						float y_px_high = (i==0)
							? 0
							: (i * slice_size - scroll_pos) * g_opengl_scale_ratio;
						
						float y_px_low = (i==MAX_NUM_SLICES-1)
							? outputSizeInPixels.height()
							: ((i + 1) * slice_size - scroll_pos) * g_opengl_scale_ratio;
						
						int scissor_y = R_MAX(0, outputSizeInPixels.height() - (int)floorf(y_px_low));
						
						int scissor_bottom = R_MIN(outputSizeInPixels.height(), outputSizeInPixels.height() - (int)floorf(y_px_high));
						
						int scissor_h = scissor_bottom - scissor_y;

						if (scissor_h <= 0)
							return false;

						command_buffer->setScissor({scissor_x, scissor_y, scissor_w, scissor_h});

						return true;
					};
					
				// Non-vertical-scissored
				for (int i = tri_slice_start ; i < tri_slice_end ; i++)
					if (setScissor(i, 0, outputSizeInPixels.width()))
						_triangle_renderers[i].QRHI_render_frame(command_buffer,
																 shared_scrolling_triangles_pipeline,
																 _triangle_renderers[0]._shader_resource_bindings);
				
				// Vertical-scissored
				for (int i = tri_slice_start ; i < tri_slice_end ; i++)
					if (setScissor(i, scissor_x, scissor_w))
						_triangle_renderer_scissors[i].QRHI_render_frame(command_buffer,
																		 shared_scrolling_triangles_pipeline,
																		 _triangle_renderers[0]._shader_resource_bindings);
				
				command_buffer->setScissor({0, 0, outputSizeInPixels.width(), outputSizeInPixels.height()});
			}

			// Node indicator (scrolling, non-scissored, foreground)
			_triangle_renderer_node_indicator.QRHI_render_frame(command_buffer,
																shared_scrolling_triangles_pipeline,
																_triangle_renderers[0]._shader_resource_bindings);

			// Playcursor (scrolling, non-scissored, foreground, separate scroll_pos)
			_triangle_renderer_playcursor.QRHI_render_frame(command_buffer,
															shared_scrolling_triangles_pipeline,
															_triangle_renderer_playcursor._shader_resource_bindings);

			// triangles (non-scrolling) - draw on top
			//
			_triangle_renderer_static.QRHI_render_frame(command_buffer);

			// Scrollbar (opaque, non-scissored)
			_triangle_renderer_scrollbar.QRHI_render_frame(command_buffer);
			
			// text (non-scissored) - per slice
			for (int i = text_slice_start ; i < text_slice_end ; i++)
				_texture_renderers[i].QRHI_render_frame(command_buffer);

			// text (scissored) - per slice
			//
			{
				if (scissor_w > 0)
					command_buffer->setScissor({scissor_x, 0, scissor_w, outputSizeInPixels.height()});
				
				for (int i = text_slice_start ; i < text_slice_end ; i++)
				{
					_texture_renderer_scissors[i].QRHI_render_frame(command_buffer);
					_texture_renderer_scissors_halfsize[i].QRHI_render_frame(command_buffer);
				}
				
				if (scissor_w > 0)
					command_buffer->setScissor({0, 0, outputSizeInPixels.width(), outputSizeInPixels.height()});
			}
			
			// text (non-scrolling) - draw on top. Not often used, but we need it for messages.
			//
			_texture_renderer_static.QRHI_render_frame(command_buffer);
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

		if (Undo_num_undos()==0 && !CanRedo() && isIllegalFilepath(dc.filename))
		{
			// Schedule it to run a little bit later just to be safe. minimizeBlockTracks is doing a lot so it's hard to keep track at all times of whether it does Qt operations or not.
			QTimer::singleShot(1, []
				{
					radium::ScopedIgnoreUndo ignore;
					minimizeBlockTracks(-1, -1, false); // maximize track widths.
				});
		}
	}
};

#if QT_CONFIG(vulkan)
static QVulkanInstance *g_vulkan_inst = nullptr;
#endif

static QRhi::Implementation MAIN_init_qrhi(void)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

    QRhi::Implementation graphicsApi = QRhi::Null;

    {
      const char *rhi_backend = GL_get_backend();
      if (!strcmp(rhi_backend, "null"))
        graphicsApi = QRhi::Null;
      else if (!strcmp(rhi_backend, "opengl"))
        graphicsApi = QRhi::OpenGLES2;
      else if (!strcmp(rhi_backend, "vulkan"))
        graphicsApi = QRhi::Vulkan;
      else if (!strcmp(rhi_backend, "d3d11"))
        graphicsApi = QRhi::D3D11;
      else if (!strcmp(rhi_backend, "d3d12"))
        graphicsApi = QRhi::D3D12;
      else if (!strcmp(rhi_backend, "metal"))
        graphicsApi = QRhi::Metal;
    }
	
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

	fmt.setSamples(g_msaa_samples);
	
#if 0 // investigate when/if there's a difference between these three. (if no difference, than SingleBuffer is probably better due to lower input latency)
	fmt.setSwapBehavior(QSurfaceFormat::SingleBuffer);
	fmt.setSwapBehavior(QSurfaceFormat::TripleBuffer);
	fmt.setSwapBehavior(QSurfaceFormat::DoubleBuffer);
#endif

	//fmt.setSwapBehavior(QSurfaceFormat::SingleBuffer);
	fmt.setSwapBehavior(QSurfaceFormat::DefaultSwapBehavior);
	
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
            GFX_Message(NULL, "Failed to create Vulkan instance, switching to OpenGL");
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


static RenderWindow *g_window;
static QWidget *g_widget;


static bool MAIN_is_scrolling(const GE_Context &c)
{
    R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
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

static int MAIN_get_slice_from_y(const int y)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

	int slice;
	
	if (y < 0)
		slice = 0;
	else
		slice = R_MIN(MAX_NUM_SLICES-1, y / g_main_thread_slice_size);

	if (slice < 0)
	{
		R_ASSERT(false);
		slice = 0;
	}

	if (slice >= MAX_NUM_SLICES)
	{
		R_ASSERT(false);
		slice = MAX_NUM_SLICES-1;
	}
	
	return slice;
}

static TextureRenderer *MAIN_get_texture_renderer(const GE_Context &context, bool is_half_size, float y)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

	if (!MAIN_is_scrolling(context))
	{
		R_ASSERT_NON_RELEASE(!is_half_size);
		return &g_window->_texture_renderer_static;
	}
			
	int slice = MAIN_get_slice_from_y(y);

	if (context._conf.use_scissors == USE_SCISSORS)
	{
		if (is_half_size)
			return &g_window->_texture_renderer_scissors_halfsize[slice];
		else
			return &g_window->_texture_renderer_scissors[slice];
	}
	else
	{
		R_ASSERT_NON_RELEASE(!is_half_size);
		return &g_window->_texture_renderers[slice];
	}
}

void GE_Context::add_triangle(const r::Triangle &triangle, r2::GradientType::Type gradient_type) const
{
	// MAIN THREAD

	if (g_rhi == NULL)
		return;

#if !defined(RELEASE)
	// Opaque pass: only Z_BACKGROUND objects (lowest z-layer, nothing behind them)
	bool is_opaque = (_conf.z == Z_BACKGROUND);

	if (is_opaque)
		g_profile_opaque++;
	else
		g_profile_transparent++;
	
	if (gradient_type == r2::GradientType::Type::NOTYPE)
		g_profile_solid++;
	else
		g_profile_gradient++;
#endif
	
	if (_conf.z == Z_PLAYCURSOR)
	{
		g_window->_triangle_renderer_playcursor.MAIN_add_triangle(*this, triangle, gradient_type);
		return;
	}

	if (_conf.z >= Z_SCROLLBAR && _conf.z < Z_MIN_STATIC)
	{
		g_window->_triangle_renderer_scrollbar.MAIN_add_triangle(*this, triangle, gradient_type);
		return;
	}

	// Node indicator: foreground non-scissored, needs to scroll but render after scissored pass
	if (_conf.z == Z_MAX_SCROLLTRANSFORM && _conf.use_scissors == NO_SCISSORS)
	{
		g_window->_triangle_renderer_node_indicator.MAIN_add_triangle(*this, triangle, gradient_type);
		return;
	}
	
	if (!MAIN_is_scrolling(*this))
	{
		R_ASSERT_NON_RELEASE(_conf.use_scissors==NO_SCISSORS);

		g_window->_triangle_renderer_static.MAIN_add_triangle(*this, triangle, gradient_type);
		
		return;
	}

	const int slice_size = g_main_thread_slice_size;

#if !defined(RELEASE)
	int num_slices = 0;
#endif
	
	for(int slice = R_BOUNDARIES(0, triangle.get_y1() / slice_size, MAX_NUM_SLICES-1) ; slice < MAX_NUM_SLICES ; slice++)
	{
		TriangleRenderer *triangle_renderer;
		
		if (_conf.use_scissors == USE_SCISSORS)
			triangle_renderer = &g_window->_triangle_renderer_scissors[slice];
		else
			triangle_renderer = &g_window->_triangle_renderers[slice];

		triangle_renderer->MAIN_add_triangle(*this, triangle, gradient_type);

#if !defined(RELEASE)
		num_slices++;
#endif
		
		if (slice * slice_size >= triangle.get_y3())
			break;
	}

#if !defined(RELEASE)
	g_profile_slice_dups += (num_slices - 1); // duplications beyond the first
#endif
}

void GE_Context::add_text(const QString &text, int x, int y) const
{
	if (g_rhi != NULL)
	{
		const GE_Rgb &rgb = color.c;
		
		MAIN_get_texture_renderer(*this, false, y)->MAIN_add_text(text, x, y,
																  rgb.r / 256.0,
																  rgb.g / 256.0,
																  rgb.b / 256.0,
																  rgb.a / 256.0);
	}
}

void GE_Context::add_text_halfsize(const QString &text, int x, int y) const
{
	if (g_rhi != NULL)
	{
		const GE_Rgb &rgb = color.c;
		
		MAIN_get_texture_renderer(*this, true, y)->MAIN_add_text(text, x, y,
																 rgb.r / 256.0,
																 rgb.g / 256.0,
																 rgb.b / 256.0,
																 rgb.a / 256.0);
	}
}

	
void GE_set_font(const QFont &font)
{
	if (g_window)
	{
		g_window->MAIN_setFont(font);
		GFX_ForceScheduleEditorRedraw(); // New font will be set before starting new paint.
	}
}

bool GL_call_me_before_starting_to_generate_vertices1(void)
{
	if (g_window == NULL || g_rhi == NULL)
		return false;
	
	// Switch texture atlas double-buffers
		
	g_window->_texture_atlas_backend->MAIN_maybe_switch_to_next_d();
	
	g_window->_texture_atlas_backend_halfsize->MAIN_maybe_switch_to_next_d();

	return true;
}

void GL_call_me_before_starting_to_generate_vertices2(void)
{
	// Obtain render buffers for vertex generators here, driven by the main-thread
	// painting-data lifecycle to avoid duplicate starts from other paths.
	g_window->for_each_vertices([](auto &vertices)
		{
			vertices.MAIN_call_me_when_starting_to_generate_vertices();
		});
}


// Called from main thread.
//
// Two-phase commit to ensure all vertex buffer releases and the painting-data swap
// happen atomically from the RHI thread's perspective, while also preventing the
// main thread from overwriting vertex buffers before the RHI thread consumes them.
//
// Phase 1 (main thread): publish all render buffers by moving them to _next_ready.
//   This clears _render_buffer so the next GE_start_writing picks a different buffer.
// Phase 2 (RHI thread, via MAIN_put_event): atomically commit all published buffers
//   to _committed, swap _painting_data, update background color, and delete old data.
void GL_set_new_painting_data(r::PaintingData *painting_data, GE_Rgb new_background_color)
{
	// ---- Phase 1: Main thread. Publish all vertex buffers ----

	// Triangle renderers (scrolling, per-slice)
	g_window->for_each_vertices([](auto &v)
		{
			v.MAIN_call_me_after_finished_generating_vertices();
		});

	// ---- Phase 2: RHI thread. Atomically commit, swap, and delete ----
	g_window->MAIN_put_event([painting_data, new_background_color](void)
		{
			// Commit all published buffers. Must happen before _painting_data swap
			// so that QRHI_customRender() always sees consistent vertex data + painting data.

			g_window->for_each_vertices([](auto &v)
				{
					v.QRHI_commit_buffers();
				});

			// Now swap painting data and background color atomically with the buffer commits.
			auto *old = g_window->_painting_data;
			g_window->_painting_data = painting_data;

			GE_set_curr_realline(painting_data->shared_variables.curr_realline);

			g_background_color = QColor((int)new_background_color.r,
										(int)new_background_color.g,
										(int)new_background_color.b,
										(int)new_background_color.a);

			// Safe to delete here since we're on the RHI thread.
			if (old)
				delete old;
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
  SETTINGS_write_int("qrhi_multisample", size);
}

int GL_get_multisample(void){
  return R_BOUNDARIES(1, SETTINGS_read_int32("qrhi_multisample", 8), 32);
}

void GL_set_safe_mode(bool onoff){
  printf("setting safe mode to %d\n",onoff);
  SETTINGS_write_bool("safe_mode", onoff);
}

static std::atomic<bool> g_clamp_text_rendering{false};

void GL_set_clamp_text_rendering(bool onoff)
{
	g_clamp_text_rendering = onoff;
	
	SETTINGS_write_bool("clamp_text_rendering", onoff);
}

// Can be called from any thread.
bool GL_get_clamp_text_rendering(void){
	static bool s_has_inited = false;

	if (!s_has_inited)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		
		g_clamp_text_rendering = SETTINGS_read_bool("clamp_text_rendering", false);

		s_has_inited = true;
	}

	return g_clamp_text_rendering;
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
  printf("setting high render thread priority to %d\n",onoff);
  SETTINGS_write_bool("high_render_thread_priority", onoff);
  g_high_render_thread_priority = onoff;
  radium::RhiWindow::QRHI_set_thread_priority(onoff);
}


bool GL_get_safe_mode(void){
  return SETTINGS_read_bool("safe_mode", false);
}

void GL_request_setting_backend(const char *backend)
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
		SETTINGS_write_string("requested_rhi_backend", backend);
	}
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
	// MAIN THREAD
	
	// To force the gpu-selection-dialog to pop up the next time the program starts up,
	// in case the program crashes between here and after 4 calls to GL_create.
	// (Commented out since it causes the gpu-selection-dialog to pop up the
	// next time if not making any edits in the current session.)
	//SETTINGS_write_string("last_successfully_started_rhi_backend", "");
	
	g_msaa_samples = GL_get_multisample();

	GL_get_clamp_text_rendering(); // Ensure SETTINGS_read_bool is not called on the qrhi thread.
		
	init_g_pause_rendering_on_off();

	QRhi::Implementation graphicsApi = MAIN_init_qrhi();
	
	g_window = new RenderWindow(graphicsApi);

#if QT_CONFIG(vulkan)
	if (graphicsApi == QRhi::Vulkan && g_vulkan_inst != nullptr)
		g_window->setVulkanInstance(g_vulkan_inst);
#endif

	g_widget = QWidget::createWindowContainer(g_window);
	g_widget->setFocusPolicy(Qt::WheelFocus);

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
	g_window = nullptr;

	R_ASSERT(g_render_window_has_been_deleted); // g_window should have been deleted automatically when calling "delete widget"...
	
#if QT_CONFIG(vulkan)
	if (g_vulkan_inst)
	{
		g_vulkan_inst->destroy();
		delete g_vulkan_inst;
		g_vulkan_inst = nullptr;
	}
#endif
}
