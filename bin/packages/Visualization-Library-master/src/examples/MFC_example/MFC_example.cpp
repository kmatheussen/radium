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

#include "StdAfx.h"
#include <vlCore/VisualizationLibrary.hpp>
#include <vlMFC/MFCWindow.hpp>
#include "Applets/App_RotatingCube.hpp"

using namespace vl;
using namespace vlMFC;

/* MFC_Example: implements the MFC application */
class MFC_Example: public CWinApp
{
public:
  MFC_Example() {}

  virtual BOOL InitInstance();
  virtual int ExitInstance();
  /*virtual int Run();*/
  virtual BOOL OnIdle(LONG lCount);

protected:
  ref<MFCWindow> mVLCWin;

  DECLARE_MESSAGE_MAP ()
};
BEGIN_MESSAGE_MAP(MFC_Example, CWinApp)
END_MESSAGE_MAP()

/* instance of the MFC application */
MFC_Example mfc_app;

/* updates the GL window */
BOOL MFC_Example::OnIdle(LONG lCount)
{
  if( mVLCWin->continuousUpdate() )
    mVLCWin->Win32Context::update();
  else
    Sleep(1);
  return TRUE;
}

/* called when the application exits */
int MFC_Example::ExitInstance()
{
  CWinApp::ExitInstance();

  /* destroy the window and the OpenGL rendering context */
  mVLCWin = NULL;

  /* shutdown Visualization Library */
  VisualizationLibrary::shutdown();

  return 0;
}

/* called when the application starts */
BOOL MFC_Example::InitInstance()
{
  CWinApp::InitInstance();

  /* open a console so we can see the program's output on stdout */
  showWin32Console();

  /* init Visualization Library */
  VisualizationLibrary::init();

  /* setup the OpenGL context format */
  OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8,8,8,0 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  format.setMultisampleSamples(16);
  format.setMultisample(true);

  /* create the applet to be run */
  ref<Applet> applet = new App_RotatingCube;
  applet->initialize();
  /* instance the MFC window/OpenGLContext */
  mVLCWin = new MFCWindow;
  /* bind the applet so it receives all the GUI events related to the OpenGLContext */
  mVLCWin->addEventListener(applet.get());
  /* target the window so we can render on it */
  applet->rendering()->as<Rendering>()->renderer()->setFramebuffer( mVLCWin->framebuffer() );
  /* black background */
  applet->rendering()->as<Rendering>()->camera()->viewport()->setClearColor( black );
  /* define the camera position and orientation */
  vec3 eye    = vec3(0,10,35); // camera position
  vec3 center = vec3(0,0,0);   // point the camera is looking at
  vec3 up     = vec3(0,1,0);   // up direction
  mat4 view_mat = mat4::getLookAt(eye, center, up);
  applet->rendering()->as<Rendering>()->camera()->setViewMatrix( view_mat );
  /* Initialize the OpenGL context and window properties */
  int x = 100;
  int y = 100;
  int width = 512;
  int height= 512;
  mVLCWin->initMFCWindow(NULL, NULL, "Visualization Library on MFC - Rotating Cube", format, x, y, width, height);

  /* MFC specific stuff */
  m_pMainWnd = mVLCWin.get();
  m_pMainWnd->ShowWindow(m_nCmdShow);
  m_pMainWnd->UpdateWindow();

  return TRUE;
}
// Have fun!
