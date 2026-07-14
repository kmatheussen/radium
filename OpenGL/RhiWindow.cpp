#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#include <functional>

#include <QQueue>
#include <QMutex>
#include <QSemaphore>
#include <QThread>
#include <QOffscreenSurface>
#include <QVarLengthArray>
#include <QFont>

#include "../common/nsmtracker.h"

#include "RhiWindow.hpp"
#include "Widget_proc.h"
//#include "Vertices.hpp"
extern QRhi *g_rhi;
extern int g_msaa_samples;
extern bool g_vsync_enabled;


static QThread *g_thread = NULL;


//! [rhiwindow-ctor]
radium::RhiWindow::RhiWindow(QRhi::Implementation graphicsApi)
    : _graphicsApi(graphicsApi)
{
    switch (graphicsApi) {
		case QRhi::OpenGLES2:
			setSurfaceType(OpenGLSurface);
			break;
		case QRhi::Vulkan:
			setSurfaceType(VulkanSurface);
			break;
		case QRhi::D3D11:
		case QRhi::D3D12:
			setSurfaceType(Direct3DSurface);
			break;
		case QRhi::Metal:
			setSurfaceType(MetalSurface);
			break;
		case QRhi::Null:
			break; // RasterSurface
    }
}
//! [rhiwindow-ctor]

QString radium::RhiWindow::graphicsApiName() const
{
    switch (_graphicsApi) {
		case QRhi::Null:
			return QLatin1String("Null (no output)");
		case QRhi::OpenGLES2:
			return QLatin1String("OpenGL");
		case QRhi::Vulkan:
        return QLatin1String("Vulkan");
		case QRhi::D3D11:
			return QLatin1String("Direct3D 11");
		case QRhi::D3D12:
			return QLatin1String("Direct3D 12");
		case QRhi::Metal:
			return QLatin1String("Metal");
    }
    return QString();
}

//! [expose]
void radium::RhiWindow::exposeEvent(QExposeEvent *)
{
	const bool is_exposed = isExposed();
	bool must_call_resize_swap_chain = false;
	
	if (!_initialized)
	{
		//printf("2. ExposeEvent!\n");
		if (is_exposed)
		{
			MAIN_init(g_initial_editor_font);			
			must_call_resize_swap_chain = true;
		}
		else
		{
			return; // Avoid possible deadlock.
		}
	}
	
	//printf("1. ExposeEvent!\n");

	QSemaphore sem;
	
	MAIN_put_event([this, &sem, is_exposed, must_call_resize_swap_chain](void)
	{
		if (must_call_resize_swap_chain)
		{
			QRHI_resizeSwapChain();
			_initialized = true;
		}

		//printf("3. ExposeEvent!\n");
	
		const QSize surfaceSize = _hasSwapChain ? _swap_chain->surfacePixelSize() : QSize();

		// stop pushing frames when not exposed (or size is 0)
		if ((!is_exposed || (_hasSwapChain && surfaceSize.isEmpty())) && _initialized && !_notExposed)
			_notExposed = true;

		//printf("4. ExposeEvent!\n");
		
		// Continue when exposed again and the surface has a valid size. Note that
		// surfaceSize can be (0, 0) even though size() reports a valid one, hence
		// trusting surfacePixelSize() and not QWindow.
		if (is_exposed && _initialized && _notExposed && !surfaceSize.isEmpty()) {
			_notExposed = false;
			_newlyExposed = true;
		}

		sem.release();

		// always render a frame on exposeEvent() (when exposed) in order to update
		// immediately on window resize.
		if (is_exposed)
		{
			//printf("     1.5. Expose event: About to call render\n");
			QRHI_render();
			//printf("    2. Expose event: Render() finished, puttning a requestUpdate event on queue\n");
			const QSize surfaceSize = _hasSwapChain ? _swap_chain->surfacePixelSize() : QSize();
			if (!surfaceSize.isEmpty())
				QRHI_request_update_from_thread();
		}
	});
	//printf("8. ExposeEvent Finished!\n");

	sem.acquire();
}
//! [expose]

//! [event]
bool radium::RhiWindow::event(QEvent *e)
{
    switch (e->type())
	{

		case QEvent::Enter:
			// The mouse has entered the QWindow
			//qDebug() << "\n\n===========Mouse entered window!\n\n";
			if (root && root->song && root->song->tracker_windows)
				root->song->tracker_windows->must_redraw_editor = true;
			break;//return true;
			
#if 0
		case QEvent::UpdateRequest:
		{
			if (isExposed())
			{
				//printf("Gakk\n");
				MAIN_put_event([this](void)
					{
						const QSize surfaceSize = _hasSwapChain ? _swap_chain->surfacePixelSize() : QSize();
						if (!surfaceSize.isEmpty())
							QRHI_render();
					});
			}
			break;
		}
#endif
	
		case QEvent::PlatformSurface:
		{
			auto *surfaceEvent = static_cast<QPlatformSurfaceEvent *>(e);
			if (surfaceEvent->surfaceEventType() == QPlatformSurfaceEvent::SurfaceAboutToBeDestroyed)
			{
			    MAIN_put_event([this](void)
					{
					    QRHI_releaseSwapChain();
					});
			}
			else if (surfaceEvent->surfaceEventType() == QPlatformSurfaceEvent::SurfaceCreated)
			{
			    MAIN_put_event([this](void)
					{
					    if (!_hasSwapChain)
					        QRHI_resizeSwapChain();
					});
			}
		}
        break;

		default:
			break;
    }

    return QWindow::event(e);
}
//! [event]

static QQueue<std::function<void(void)>>  g_queue;
static QSemaphore g_sem;
static QMutex g_mutex;

void radium::RhiWindow::QRHI_handle_thread_events(void)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	QRHI_request_update_from_thread();

#if THREADED_GFX
		
	while(true)
	{
		//printf("0.  About to checheck queue, calling QSemaphore::acquire()\n");

		//if (g_sem.tryAcquire(1, QDeadlineTimer(16)))
		//g_sem.acquire();
		while (g_sem.tryAcquire())
		{
			//printf("1.  Got message that threre is a new event on queue\n");

			if (_please_shut_down_qrhi_thread)
			{
				_qrhi_thread_has_shut_down = true;
				return;
			}

			std::function<void(void)> func;
		
			{
				QMutexLocker lock(&g_mutex);
				func = g_queue.dequeue();
			}
		
			//printf("2.  Got event from queue: Running now.\n");
			
			func();
		}		

		if (_stop_rendering)
			QThread::msleep(5);
		else
			QRHI_render();
	}

#endif // THREADED_GFX
}

void radium::RhiWindow::MAIN_put_event(std::function<void(void)> event)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

#if THREADED_GFX
	
	//printf("  About to put event\n");
	{
		QMutexLocker lock(&g_mutex);
		g_queue.enqueue(event);
	}
	//printf("  Finished putting event. Calling release on semaphore\n");
	g_sem.release();
	//printf("  Finished releasing semaphore\n");
	
#else
	
	event();
	
#endif
}

void radium::RhiWindow::MAIN_put_event_sync(std::function<void(void)> event)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

#if THREADED_GFX
	
	QSemaphore sem;

	MAIN_put_event([event = std::move(event), &sem]()
		{
			event();
			sem.release();
		});

	sem.acquire();
	
#else
	
	event();
	
#endif
}

void radium::RhiWindow::QRHI_request_update_from_thread(void)
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

#if THREADED_GFX
	QMetaObject::invokeMethod(qApp->thread(), [this] ()
		{
			requestUpdate();
		});
#else
	requestUpdate();
#endif
}

void radium::RhiWindow::QRHI_stop_rendering(void)
{
	_stop_rendering = true;
}

void radium::RhiWindow::QRHI_set_thread_priority(bool high)
{
	if (g_thread != NULL)
		g_thread->setPriority(high ? QThread::HighestPriority : QThread::NormalPriority);
}

//! [rhi-init]
void radium::RhiWindow::MAIN_init(const QFont &font)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

	//printf("INIT CALLED\n");
	if (g_thread==NULL)
	{
		QSemaphore finished_initing;

		auto initfunc = [this, &finished_initing, font]
			{
				THREADING_init_qrhi_thread_type();

				if (_graphicsApi == QRhi::Null)
				{
					QRhiNullInitParams params;
					_rhi = QRhi::create(QRhi::Null, &params);
				}
				
#if QT_CONFIG(opengl)
				if (_graphicsApi == QRhi::OpenGLES2)
				{
					_fallbackSurface = QRhiGles2InitParams::newFallbackSurface();
					
					QRhiGles2InitParams params;
					params.fallbackSurface = _fallbackSurface;
					params.window = this;
					
					_rhi = QRhi::create(QRhi::OpenGLES2, &params);
				}
#endif
				
#if QT_CONFIG(vulkan)
				if (_graphicsApi == QRhi::Vulkan)
				{
					QRhiVulkanInitParams params;
					params.inst = vulkanInstance();
					params.window = this;
					
					_rhi = QRhi::create(QRhi::Vulkan, &params);
				}
#endif
				
#ifdef Q_OS_WIN
				if (_graphicsApi == QRhi::D3D11)
				{
					QRhiD3D11InitParams params;
					// Enable the debug layer, if available. This is optional
					// and should be avoided in production builds.
					params.enableDebugLayer = true;
					_rhi = QRhi::create(QRhi::D3D11, &params);
				}
				else if (_graphicsApi == QRhi::D3D12)
				{
					QRhiD3D12InitParams params;
					// Enable the debug layer, if available. This is optional
					// and should be avoided in production builds.
					params.enableDebugLayer = true;
					_rhi = QRhi::create(QRhi::D3D12, &params);
				}
#endif
				
#if QT_CONFIG(metal)
				if (_graphicsApi == QRhi::Metal)
				{
					QRhiMetalInitParams params;
					_rhi = QRhi::create(QRhi::Metal, &params);
				}
#endif
				
				if (!_rhi)
					qFatal("Failed to create RHI backend");
//! [rhi-init]

//! [swapchain-init]
				_swap_chain = _rhi->newSwapChain();
				if (!g_vsync_enabled)
					_swap_chain->setFlags(_swap_chain->flags() | QRhiSwapChain::NoVSync);

#if USE_RENDER_BUFFER
				if (g_msaa_samples > 1)
				{
					_ds = _rhi->newRenderBuffer(QRhiRenderBuffer::DepthStencil,
					                            QSize(),
					                            g_msaa_samples,
					                            QRhiRenderBuffer::UsedWithSwapChainOnly);
				}
#endif
				_swap_chain->setWindow(this);
#if USE_RENDER_BUFFER
				if (_ds)
					_swap_chain->setDepthStencil(_ds);
#endif
				for(int s : _rhi->supportedSampleCounts())
					printf("Supported sample count on qrhi: %d\n", s);

#if USE_RENDER_BUFFER
				if (g_msaa_samples > 1)
					_swap_chain->setSampleCount(g_msaa_samples);
#endif
				
				_render_pass_descriptor = _swap_chain->newCompatibleRenderPassDescriptor();
				_swap_chain->setRenderPassDescriptor(_render_pass_descriptor);
//! [swapchain-init]
				
QRHI_customInit(font);

				finished_initing.release();
				
				QRHI_handle_thread_events();
			};

#if THREADED_GFX
		
		g_thread = qthread2("RHI-thread", initfunc);

		g_thread->setPriority(GL_get_high_render_thread_priority() ? QThread::HighestPriority : QThread::NormalPriority);

		finished_initing.acquire();
		
#else // THREADED_GFX -> !THREADED_GFX
	
		g_thread = QThread::currentThread();
	
		initfunc();
		
#endif // !THREADED_GFX

		g_rhi = _rhi;
	}
}


//extern double g_opengl_scale_ratio;

//QMatrix4x4 g_viewProjection;

//! [swapchain-resize]
void radium::RhiWindow::QRHI_resizeSwapChain()
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	R_ASSERT_NON_RELEASE(QThread::currentThread() == g_thread);
	
    _hasSwapChain = _swap_chain->createOrResize(); // also handles _ds

    const QSize outputSizeInPixels = _swap_chain->currentPixelSize();

	QMatrix4x4 s_y_flipper_matrix;
	s_y_flipper_matrix.scale(1.0f, -1.0f, 1.0f);

	QMatrix4x4 orthoProjection;
	orthoProjection.ortho(-0.5f,
						  float(outputSizeInPixels.width()/g_opengl_scale_ratio)-0.5f,
						  -0.5f,
						  float(outputSizeInPixels.height()/g_opengl_scale_ratio)-0.5f,
						  -1.0, +1.0);

	_viewProjection = (_rhi->clipSpaceCorrMatrix() * s_y_flipper_matrix) * orthoProjection;
		
	/*
    _viewProjection = _rhi->clipSpaceCorrMatrix();
    _viewProjection.perspective(45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 1000.0f);
    _viewProjection.translate(0, 0, -4);

	g_viewProjection = _viewProjection;
	*/
}
//! [swapchain-resize]

void radium::RhiWindow::QRHI_releaseSwapChain()
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	R_ASSERT_NON_RELEASE(QThread::currentThread() == g_thread);
	
	if (_hasSwapChain)
	{
        _hasSwapChain = false;
        _swap_chain->destroy();
    }
}

//! [render-precheck]
void radium::RhiWindow::QRHI_render()
{
	R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

	R_ASSERT_NON_RELEASE(QThread::currentThread() == g_thread);

	const QSize surfaceSize = _hasSwapChain ? _swap_chain->surfacePixelSize() : QSize();
	if (surfaceSize.isEmpty())
		return;

    if (!_hasSwapChain || _notExposed)
        return;
	
//! [render-precheck]

//! [render-resize]
    // If the window got resized or newly exposed, resize the swapchain. (the
    // newly-exposed case is not actually required by some platforms, but is
    // here for robustness and portability)
    //
    // This (exposeEvent + the logic here) is the only safe way to perform
    // resize handling. Note the usage of the RHI's surfacePixelSize(), and
    // never QWindow::size(). (the two may or may not be the same under the hood,
    // depending on the backend and platform)
    //
    if (_swap_chain->currentPixelSize() != _swap_chain->surfacePixelSize() || _newlyExposed)
	{
        QRHI_resizeSwapChain();
		
        if (!_hasSwapChain)
            return;
		
        _newlyExposed = false;
    }
//! [render-resize]

//! [beginframe]
    QRhi::FrameOpResult result = _rhi->beginFrame(_swap_chain);
    if (result == QRhi::FrameOpSwapChainOutOfDate)
	{
        QRHI_resizeSwapChain();
        if (!_hasSwapChain)
            return;
        result = _rhi->beginFrame(_swap_chain);
    }
    if (result != QRhi::FrameOpSuccess)
	{
        fprintf(stderr, "beginFrame failed with %d, will retry", result);
        QRHI_request_update_from_thread();
        return;
    }

QRHI_customRender();
//! [beginframe]

//! [request-update]
    _rhi->endFrame(_swap_chain);

    // Always request the next frame via requestUpdate(). On some platforms this is backed
    // by a platform-specific solution, e.g. CVDisplayLink on macOS, which is potentially
    // more efficient than a timer, queued metacalls, etc.
	QRHI_request_update_from_thread();
    //requestUpdate();
}
