#include <QWindow>
#include <QSemaphore>
#include <QOffscreenSurface>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop


#define THREADED_GFX 0
#define DO_ANTIALIASING 1
#define USE_RENDER_BUFFER 1


static inline QThread *qthread2(QString name, std::function<void(void)> callback)
{
	//assert_is_qthread();

	QThread *thread = QThread::create([name, callback]()
		{
			//QTHREAD_SCOPED_INIT;

			//LOG_EXCEPTIONS(name.toUtf8().constData(), callback);
			callback();
		});
	
	if (QThread::currentThread() != qApp->thread())
	{
		assert(thread->thread() == QThread::currentThread()); // If not, 'QObject::moveToThread' doesn't work.

		// If we don't do this, 'thread' won't be deleted until after the current thread is deleted/
		// E.g. if this function is called from a thread pool-thread, the object may never be deleted.

		assert(qApp->thread() != NULL);
		
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
public:
    RhiWindow(QRhi::Implementation graphicsApi);
    QString graphicsApiName() const;
    void releaseSwapChain();

	virtual ~RhiWindow()
	{
		fprintf(stderr, "A1\n");

		QSemaphore sem;
	
		put_event([this, &sem]()
			{
#if QT_CONFIG(opengl)
				delete _fallbackSurface;
#endif
				fprintf(stderr, "A2\n");
				delete _sc;
				//fprintf(stderr, "A3\n");
#if USE_RENDER_BUFFER
				delete _ds;
#endif
				fprintf(stderr, "A4\n");
				delete _rp;
				fprintf(stderr, "A5\n");

				if (_graphicsApi != QRhi::OpenGLES2) // Crash if deleting _rhi when using OpenGL.
					delete _rhi;
				
				fprintf(stderr, "A6\n");
				
				sem.release();
				fprintf(stderr, "A7\n");
			});

		fprintf(stderr, "A8\n");
		sem.acquire();
		fprintf(stderr, "A9\n");
	}
	
protected:
	
    virtual void customInit() = 0;
    virtual void customRender() = 0;

    // destruction order matters to a certain degree: the fallbackSurface must
    // outlive the rhi, the rhi must outlive all other resources.  The resources
    // need no special order when destroying.
#if QT_CONFIG(opengl)
    QOffscreenSurface *_fallbackSurface = NULL;
#endif
	
    QRhi *_rhi;
//! [swapchain-data]
    QRhiSwapChain *_sc;
#if USE_RENDER_BUFFER
    QRhiRenderBuffer *_ds = nullptr;
#endif
    QRhiRenderPassDescriptor *_rp;
//! [swapchain-data]
	bool _hasSwapChain = false;
    //QMatrix4x4 _viewProjection;

private:
    void init();
    void resizeSwapChain();
    void render();

    void exposeEvent(QExposeEvent *) override;
    bool event(QEvent *) override;

    QRhi::Implementation _graphicsApi;
    bool _initialized = false;
    bool _notExposed = false;
    bool _newlyExposed = false;
	
	void handle_thread_events(void);

public:
	
	void put_event(std::function<void(void)> event);
	void request_update_from_thread(void);
};
}


