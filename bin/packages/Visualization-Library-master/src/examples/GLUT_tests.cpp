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

#include <vlGLUT/GLUTWindow.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include "tests.hpp"

using namespace vl;

class TestBatteryGLUT: public TestBattery
{
public:
  void runGUI(const vl::String& title, BaseDemo* applet, vl::OpenGLContextFormat format, int x, int y, int width, int height, vl::fvec4 bk_color, vl::vec3 eye, vl::vec3 center)
  {
    /* used to display the application title next to FPS counter */
    applet->setAppletName(title);

    /* install Visualization Library shutdown function */
    atexit( vlGLUT::atexit_visualization_library_shutdown );

    /* create a GLUT window */
    vl::ref<vlGLUT::GLUTWindow> glut_window = new vlGLUT::GLUTWindow;

    setupApplet(applet, glut_window.get(), bk_color, eye, center);

    /* Initialize the OpenGL context and window properties */
    glut_window->initGLUTWindow( title, format, x, y, width, height );

    /* show the window */
    glut_window->show();

    /* run the GLUT message loop */
    glutMainLoop();

    /* this point is never reached since glutMainLoop() never returns! */

    ///* deallocate the window with all the OpenGL resources before shutting down Visualization Library */
    //glut_window = NULL;
  }
};
int main ( int argc, char *argv[] )
{
  /* init GLUT */
  int pargc = argc;
  glutInit( &pargc, argv );

  /* parse command line arguments */
  int   test = 0;
  if (argc>=2)
    test = atoi(argv[1]);

  /* setup the OpenGL context format */
  vl::OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8,8,8,8 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  //format.setMultisampleSamples(16);
  //format.setMultisample(true);

  TestBatteryGLUT test_battery;
  test_battery.run(test, argv[1], format);

  return 0;
}
