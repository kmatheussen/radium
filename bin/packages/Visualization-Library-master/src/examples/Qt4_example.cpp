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

#include <vlCore/VisualizationLibrary.hpp>
#include <vlQt4/Qt4Widget.hpp>
#include "Applets/App_RotatingCube.hpp"

//#include "tests.hpp"
//#include "Applets/BaseDemo.hpp"
//#include "Applets/BaseDemo.hpp"
//#include "Applets/App_VectorGraphics.cpp"

//extern BaseDemo* Create_App_VectorGraphics();

using namespace vl;
using namespace vlQt4;



int main(int argc, char *argv[])
{
  QApplication app(argc, argv);

  /* init Visualization Library */
  VisualizationLibrary::init();

  /* setup the OpenGL context format */
  OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8,8,8,8 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  format.setMultisampleSamples(16);
  format.setMultisample(true);

  /* create the applet to be run */
ref<Applet> applet = new App_RotatingCube;
//  ref<Applet> applet = Create_App_VectorGraphics(); //new App_RotatingCube;
  applet->initialize();
  /* create a native Qt4 window */
  ref<vlQt4::Qt4Widget> qt4_window = new vlQt4::Qt4Widget;
  /* bind the applet so it receives all the GUI events related to the OpenGLContext */
  qt4_window->addEventListener(applet.get());
  /* target the window so we can render on it */
  applet->rendering()->as<Rendering>()->renderer()->setFramebuffer( qt4_window->framebuffer() );
  /* black background */
  applet->rendering()->as<Rendering>()->camera()->viewport()->setClearColor( black );
  /* define the camera position and orientation */
  vec3 eye    = vec3(0,10,35); // camera position
  vec3 center = vec3(0,0,0);   // point the camera is looking at
  vec3 up     = vec3(0,1,0);   // up direction
  mat4 view_mat = mat4::getLookAt(eye, center, up);
  applet->rendering()->as<Rendering>()->camera()->setViewMatrix( view_mat );
  /* Initialize the OpenGL context and window properties */
  int x = 10;
  int y = 10;
  int width = 512;
  int height= 512;
  qt4_window->initQt4Widget( "Visualization Library on Qt4 - Rotating Cube", format, NULL, x, y, width, height );
  /* show the window */
  qt4_window->show();

  /* run the Win32 message loop */
  int val = app.exec();

  /* deallocate the window with all the OpenGL resources before shutting down Visualization Library */
  qt4_window = NULL;

  /* shutdown Visualization Library */
  VisualizationLibrary::shutdown();

  return val;
}
// Have fun!
