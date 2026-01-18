#include <functional>

#include <QQueue>
#include <QMutex>
#include <QSemaphore>
#include <QThread>
#include <QOffscreenSurface>

#include "RhiWindow.hpp"

#include "../common/nsmtracker.h"


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
	
	if (!_initialized) {
		//printf("2. ExposeEvent!\n");
		if (is_exposed)
		{
			init();
			must_call_resize_swap_chain = true;
		}
		else
		{
			return; // Avoid possible deadlock.
		}
	}
	
	//printf("1. ExposeEvent!\n");

	QSemaphore sem;
	
	put_event([this, &sem, is_exposed, must_call_resize_swap_chain](void)
	{
		if (must_call_resize_swap_chain)
		{
			resizeSwapChain();
			_initialized = true;
		}

		//printf("3. ExposeEvent!\n");
	
		const QSize surfaceSize = _hasSwapChain ? _sc->surfacePixelSize() : QSize();

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
			render();
			//printf("    2. Expose event: Render() finished, puttning a requestUpdate event on queue\n");
			const QSize surfaceSize = _hasSwapChain ? _sc->surfacePixelSize() : QSize();
			if (!surfaceSize.isEmpty())
				request_update_from_thread();
		}
	});
	//printf("8. ExposeEvent Finished!\n");

	sem.acquire();
}
//! [expose]

//! [event]
bool radium::RhiWindow::event(QEvent *e)
{
    switch (e->type()) {
    case QEvent::UpdateRequest:
	{
		if (isExposed())
		{
			//printf("Gakk\n");
			put_event([this](void)
				{
					const QSize surfaceSize = _hasSwapChain ? _sc->surfacePixelSize() : QSize();
					if (!surfaceSize.isEmpty())
						render();
				});
		}
        break;
	}
	
    case QEvent::PlatformSurface:
        // this is the proper time to tear down the swapchain (while the native window and surface are still around)
		if (static_cast<QPlatformSurfaceEvent *>(e)->surfaceEventType() == QPlatformSurfaceEvent::SurfaceAboutToBeDestroyed)
			put_event([this](void)
				{
					releaseSwapChain();
				});
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

void radium::RhiWindow::handle_thread_events(void)
{
	request_update_from_thread();

#if THREADED_GFX
		
	while(true)
	{
		//printf("0.  About to checheck queue, calling QSemaphore::acquire()\n");

		//if (g_sem.tryAcquire(1, QDeadlineTimer(16)))
		g_sem.acquire();
		{
			//printf("1.  Got message that threre is a new event on queue\n");
			
			std::function<void(void)> func;
		
			{
				QMutexLocker lock(&g_mutex);
				func = g_queue.dequeue();
			}
		
			//printf("2.  Got event from queue: Running now.\n");
			
			func();
		}

		//render();
	}

#endif // THREADED_GFX
}

void radium::RhiWindow::put_event(std::function<void(void)> event)
{
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

void radium::RhiWindow::request_update_from_thread(void)
{
#if THREADED_GFX
	QMetaObject::invokeMethod(qApp->thread(), [this] ()
		{
			requestUpdate();
		});
#else
	requestUpdate();
#endif
}

//! [rhi-init]
void radium::RhiWindow::init()
{
	//printf("INIT CALLED\n");
	if (g_thread==NULL)
	{
		QSemaphore finished_initing;

		auto initfunc = [this, &finished_initing]
			{
				if (_graphicsApi == QRhi::Null) {
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
				_sc = _rhi->newSwapChain();

#if USE_RENDER_BUFFER
				_ds = _rhi->newRenderBuffer(QRhiRenderBuffer::DepthStencil,
											QSize(), // no need to set the size here, due to UsedWithSwapChainOnly
											4, // antialias
											QRhiRenderBuffer::UsedWithSwapChainOnly);
#endif
				_sc->setWindow(this);
#if USE_RENDER_BUFFER
				_sc->setDepthStencil(_ds);
#endif
				for(int s : _rhi->supportedSampleCounts())
					printf("Supported sample count on qrhi: %d\n", s);

#if USE_RENDER_BUFFER
#if 1 // DO_ANTIALIASING
				// Crash... (on 6.8.2, without render buffer. Too old version of Qt perhaps?)
				_sc->setSampleCount(4); // Weird, must set this one as well, to the same value as above. (guess it's because it's an unfinished API, this should have been done automatically.)
#endif
#endif
				
				_rp = _sc->newCompatibleRenderPassDescriptor();
				_sc->setRenderPassDescriptor(_rp);
//! [swapchain-init]
				
				customInit();

				finished_initing.release();
				
				handle_thread_events();
			};

#if THREADED_GFX
		
		g_thread = qthread2("RHI-thread", initfunc);

		finished_initing.acquire();
		
#else // THREADED_GFX -> !THREADED_GFX
	
		g_thread = QThread::currentThread();
	
		initfunc();
		
#endif // !THREADED_GFX

	}
}
						  
//! [swapchain-resize]
void radium::RhiWindow::resizeSwapChain()
{
	assert(QThread::currentThread() == g_thread);
	
    _hasSwapChain = _sc->createOrResize(); // also handles _ds

	/*
    const QSize outputSize = _sc->currentPixelSize();
    _viewProjection = _rhi->clipSpaceCorrMatrix();
    _viewProjection.perspective(45.0f, outputSize.width() / (float) outputSize.height(), 0.01f, 1000.0f);
    _viewProjection.translate(0, 0, -4);
	*/
}
//! [swapchain-resize]

void radium::RhiWindow::releaseSwapChain()
{
	assert(QThread::currentThread() == g_thread);
	
    if (_hasSwapChain) {
        _hasSwapChain = false;
        _sc->destroy();
    }
}

//! [render-precheck]
void radium::RhiWindow::render()
{
	assert(QThread::currentThread() == g_thread);

	const QSize surfaceSize = _hasSwapChain ? _sc->surfacePixelSize() : QSize();
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
    if (_sc->currentPixelSize() != _sc->surfacePixelSize() || _newlyExposed) {
        resizeSwapChain();
        if (!_hasSwapChain)
            return;
        _newlyExposed = false;
    }
//! [render-resize]

//! [beginframe]
    QRhi::FrameOpResult result = _rhi->beginFrame(_sc);
    if (result == QRhi::FrameOpSwapChainOutOfDate) {
        resizeSwapChain();
        if (!_hasSwapChain)
            return;
        result = _rhi->beginFrame(_sc);
    }
    if (result != QRhi::FrameOpSuccess) {
        qWarning("beginFrame failed with %d, will retry", result);
        request_update_from_thread();
        return;
    }

    customRender();
//! [beginframe]

//! [request-update]
    _rhi->endFrame(_sc);

    // Always request the next frame via requestUpdate(). On some platforms this is backed
    // by a platform-specific solution, e.g. CVDisplayLink on macOS, which is potentially
    // more efficient than a timer, queued metacalls, etc.
	request_update_from_thread();
    //requestUpdate();
}
