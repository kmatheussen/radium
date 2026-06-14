#include <QWindow>
#include <QSemaphore>
#include <QOffscreenSurface>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop

#include "../Qt/Qt_MainWindow_proc.h"

#define THREADED_GFX 1
#define USE_RENDER_BUFFER 1 // At least one of opengl/directx/metal/vulkan systems required this one, don't remember which.


static inline QThread *qthread2(const QString &name, std::function<void(void)> callback)
{
	QThread *thread = QThread::create(callback);
	
	if (QThread::currentThread() != qApp->thread())
	{
		R_ASSERT(thread->thread() == QThread::currentThread()); // If not, 'QObject::moveToThread' doesn't work.

		R_ASSERT(qApp->thread() != NULL);
		
		thread->moveToThread(qApp->thread());
	}

	thread->setObjectName(name);
	thread->start();

	return thread;
}



namespace radium
{
class RhiWindow : public QWindow
{
	std::atomic<bool> _please_shut_down_qrhi_thread{false};
	std::atomic<bool> _qrhi_thread_has_shut_down{false};
	
public:
    RhiWindow(QRhi::Implementation graphicsApi);
    QString graphicsApiName() const;
    void QRHI_releaseSwapChain();

	virtual ~RhiWindow()
	{
		fprintf(stderr, "A1\n");

		MAIN_put_event_sync([this]()
			{
#if QT_CONFIG(opengl)
				delete _fallbackSurface;
#endif
				fprintf(stderr, "A2\n");
				delete _swap_chain;
				//fprintf(stderr, "A3\n");
#if USE_RENDER_BUFFER
				delete _ds;
#endif
				fprintf(stderr, "A4\n");
				delete _render_pass_descriptor;
				fprintf(stderr, "A5\n");

				if (_graphicsApi != QRhi::OpenGLES2) // Crash if deleting _rhi when using OpenGL.
					delete _rhi;
				
				fprintf(stderr, "A6\n");
				fprintf(stderr, "A7\n");
			});

		fprintf(stderr, "A8\n");
		fprintf(stderr, "A9\n");

		_please_shut_down_qrhi_thread = true;

		MAIN_put_event([]()
			{
				return;
			});
		
		while(_qrhi_thread_has_shut_down == false)
			QThread::msleep(10);
	}
	
protected:
	
    virtual void QRHI_customInit(const QFont &font) = 0;
    virtual void QRHI_customRender() = 0;

    // destruction order matters to a certain degree: the fallbackSurface must
    // outlive the rhi, the rhi must outlive all other resources.  The resources
    // need no special order when destroying.
#if QT_CONFIG(opengl)
    QOffscreenSurface *_fallbackSurface = NULL;
#endif
	
    QRhi *_rhi;
//! [swapchain-data]
    QRhiSwapChain *_swap_chain;
#if USE_RENDER_BUFFER
    QRhiRenderBuffer *_ds = nullptr;
#endif
    QRhiRenderPassDescriptor *_render_pass_descriptor;
//! [swapchain-data]
	bool _hasSwapChain = false;
    QMatrix4x4 _viewProjection;

private:
    void MAIN_init(const QFont &font);
    void QRHI_resizeSwapChain();
    void QRHI_render();

    void exposeEvent(QExposeEvent *) override;
    bool event(QEvent *) override;

    QRhi::Implementation _graphicsApi;
    bool _initialized = false;
    bool _notExposed = false;
    bool _newlyExposed = false;
	
	void QRHI_handle_thread_events(void);

public:
	
	void MAIN_put_event(std::function<void(void)> event);
	void MAIN_put_event_sync(std::function<void(void)> event);
	void QRHI_request_update_from_thread(void);

	static void QRHI_set_thread_priority(bool high);
};
}


