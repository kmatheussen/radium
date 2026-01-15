/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include <vlWX/WXGLCanvas.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include "Applets/App_RotatingCube.hpp"

using namespace vlWX;
using namespace vl;

//-----------------------------------------------------------------------------
// main window
//-----------------------------------------------------------------------------
class MyFrame: public wxFrame
{
public:
  MyFrame(wxWindow *parent, const wxString& title, const wxPoint& pos, const wxSize& size, long style = wxDEFAULT_FRAME_STYLE):
    wxFrame(parent, wxID_ANY, title, pos, size, style) {}
  ~MyFrame(){}
};
//-----------------------------------------------------------------------------
// implement the application
//-----------------------------------------------------------------------------
class MyApp: public wxApp
{
public:
  bool OnInit();
  int OnExit();
};
//-----------------------------------------------------------------------------
IMPLEMENT_APP(MyApp)
//-----------------------------------------------------------------------------
bool MyApp::OnInit()
{
  /* open a console so we can see the applet's output on stdout */
  showWin32Console();

  /* init Visualization Library */
  VisualizationLibrary::init();

  MyFrame *frame = new MyFrame(NULL, L"WXGLCanvas", wxDefaultPosition, wxSize(400, 300));

  /* Initialize the OpenGL context and window properties */
  // WX_GL_RGBA:            Use true colour
  // WX_GL_BUFFER_SIZE:     Bits for buffer if not WX_GL_RGBA
  // WX_GL_LEVEL:           0 for main buffer, >0 for overlay, <0 for underlay
  // WX_GL_DOUBLEBUFFER:    Use doublebuffer
  // WX_GL_STEREO:          Use stereoscopic display
  // WX_GL_AUX_BUFFERS:     Number of auxiliary buffers (not all implementation support this option)
  // WX_GL_MIN_RED:         Use red buffer with most bits (> MIN_RED bits)
  // WX_GL_MIN_GREEN:       Use green buffer with most bits (> MIN_GREEN bits)
  // WX_GL_MIN_BLUE:        Use blue buffer with most bits (> MIN_BLUE bits)
  // WX_GL_MIN_ALPHA:       Use alpha buffer with most bits (> MIN_ALPHA bits)
  // WX_GL_DEPTH_SIZE:      Bits for Z-buffer (0,16,32)
  // WX_GL_STENCIL_SIZE:    Bits for stencil buffer
  // WX_GL_MIN_ACCUM_RED:   Use red accum buffer with most bits (> MIN_ACCUM_RED bits)
  // WX_GL_MIN_ACCUM_GREEN: Use green buffer with most bits (> MIN_ACCUM_GREEN bits)
  // WX_GL_MIN_ACCUM_BLUE:  Use blue buffer with most bits (> MIN_ACCUM_BLUE bits)
  // WX_GL_MIN_ACCUM_ALPHA: Use blue buffer with most bits (> MIN_ACCUM_ALPHA bits)
  int context_format[] =
  {
    WX_GL_RGBA,
    WX_GL_DOUBLEBUFFER,
    WX_GL_MIN_RED, 8,
    WX_GL_MIN_GREEN, 8,
    WX_GL_MIN_BLUE, 8,
    WX_GL_MIN_ALPHA, 8,
    WX_GL_DEPTH_SIZE, 24,
    WX_GL_STENCIL_SIZE, 8,
    /*WX_GL_LEVEL, 0,
    WX_GL_AUX_BUFFERS, 0*/
    0
  };
  ref<WXGLCanvas> vl_gl_canvas = new WXGLCanvas( frame, NULL, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxFULL_REPAINT_ON_RESIZE, context_format );

  /* create the applet to be run */
  ref<Applet> applet = new App_RotatingCube;

  applet->initialize();

  /* target the window so we can render on it */
  applet->rendering()->as<Rendering>()->renderer()->setFramebuffer( vl_gl_canvas->framebuffer() );
  
  /* black background */
  applet->rendering()->as<Rendering>()->camera()->viewport()->setClearColor( black );
  
  /* define the camera position and orientation */
  vec3 eye    = vec3(0,10,35); // camera position
  vec3 center = vec3(0,0,0);   // point the camera is looking at
  vec3 up     = vec3(0,1,0);   // up direction
  mat4 view_mat = mat4::getLookAt(eye, center, up);
  applet->rendering()->as<Rendering>()->camera()->setViewMatrix( view_mat );
  
  /* show the window */
  frame->Show();
  
  /* THE ORDER IS IMPORTANT IMPORTANT */
  vl_gl_canvas->initGLContext();
  
  /* bind the applet so it receives all the GUI events related to the OpenGLContext */
  vl_gl_canvas->addEventListener(applet.get());

  /* these must be done after the window is visible */
  int x = 0;
  int y = 0;
  int width = 512;
  int height= 512;
  frame->SetPosition( wxPoint(x,y) );
  frame->SetClientSize( wxSize(width,height) );
  frame->SetLabel(wxT("Visualization Library on wxWindows - Rotating Cube"));

  return true;
}
//-----------------------------------------------------------------------------
int MyApp::OnExit()
{
  VisualizationLibrary::shutdown();
  return 0;
}
//-----------------------------------------------------------------------------
// Have fun!
