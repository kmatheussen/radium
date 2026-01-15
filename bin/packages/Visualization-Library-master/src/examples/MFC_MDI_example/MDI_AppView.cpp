#include "stdafx.h"
#include "MDI_App.h"

#include "MDI_AppDoc.h"
#include "MDI_AppView.h"
#include "App_RotatingTeapot.hpp"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

IMPLEMENT_DYNCREATE(CMDI_AppView, vlMFC::MDIWindow)
//-----------------------------------------------------------------------------
// VL: it is important to enable these messages
BEGIN_MESSAGE_MAP(CMDI_AppView, vlMFC::MDIWindow)
  
END_MESSAGE_MAP()
//-----------------------------------------------------------------------------
CMDI_AppView::CMDI_AppView()
{
  // VL: add this view to the view list so that it can be updated
  theApp.AddView(this);
}
//-----------------------------------------------------------------------------
CMDI_AppView::~CMDI_AppView()
{
  // VL: add this view to the view list so that it can be updated
  theApp.RemoveView(this);
}
//-----------------------------------------------------------------------------
void CMDI_AppView::OnInitialUpdate()
{
	vlMFC::MDIWindow::OnInitialUpdate();
  
	/* setup the OpenGL context format */
	vl::OpenGLContextFormat format;
	format.setDoubleBuffer(true);
	format.setRGBABits( 8,8,8,0 );
	format.setDepthBufferBits(24);
	format.setStencilBufferBits(8);
	format.setFullscreen(false);
	format.setMultisampleSamples(16);
	format.setMultisample(true);

	/* create a new vl::Rendering for this window */
	vl::ref<vl::Rendering> rend = new vl::Rendering;
	rend->renderer()->setFramebuffer( this->OpenGLContext::framebuffer() );

	/* black background */
	rend->camera()->viewport()->setClearColor( vl::black );

	/* define the camera position and orientation */
	vl::vec3 eye    = vl::vec3(0,10,35); // camera position
	vl::vec3 center = vl::vec3(0,0,0);   // point the camera is looking at
	vl::vec3 up     = vl::vec3(0,1,0);   // up direction
	vl::mat4 view_mat = vl::mat4::getLookAt(eye, center, up);
	rend->camera()->setViewMatrix( view_mat );

	/* create the applet to be run */
	vl::ref<App_RotatingCube> applet = new App_RotatingCube;
  applet->setRendering(rend.get());
	applet->initialize();

	/* bind the applet so it receives all the GUI events related to the OpenGLContext */
	this->OpenGLContext::addEventListener(applet.get());
	
 	/* Initialize the OpenGL context and window properties */	
 	CRect r; 	
	GetWindowRect(&r);
  Win32Context::initWin32GLContext(NULL, "Visualization Library MFC MDI- Rotating Cube", format, /*these last for are ignored*/0, 0, r.Width(), r.Height());
}

//-----------------------------------------------------------------------------

// the rest is MFC stuff...

//-----------------------------------------------------------------------------
void CMDI_AppView::OnDraw(CDC* /*pDC*/)
{
	CMDI_AppDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	if (!pDoc)
		return;
}
//-----------------------------------------------------------------------------
CMDI_AppDoc* CMDI_AppView::GetDocument() const // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CMDI_AppDoc)));
	return (CMDI_AppDoc*)m_pDocument;
}
//-----------------------------------------------------------------------------
