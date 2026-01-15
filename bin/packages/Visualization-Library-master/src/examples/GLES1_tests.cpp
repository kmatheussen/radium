/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlCore\VisualizationLibrary.hpp>
#include <vlCore\Log.hpp>
#include <vlCore\Time.hpp>
#include <vlEGL\EGLWindow.hpp>
#include "Applets/App_RotatingCube.hpp"

using namespace vl;

#include "tests.hpp"

using namespace vl;
using namespace vlEGL;

class TestBatteryEGL: public TestBattery
{
public:
  void runGUI(const String& title, BaseDemo* applet, OpenGLContextFormat format, int x, int y, int width, int height, fvec4 bk_color, vec3 eye, vec3 center)
  {
    /* used to display the application title next to FPS counter */
    applet->setAppletName(title);

    /* create a native EGL window */
    ref<vlEGL::EGLWindow> egl_window = new vlEGL::EGLWindow;

    setupApplet(applet, egl_window.get(), bk_color, eye, center);

    /* Initialize the OpenGL context and window properties */
    egl_window->initEGLWindow(NULL, "OpenGL ES 1.x Tests", format, x, y, width, height );

    /* show the window */
    egl_window->show();

    /* run the EGL message loop */
    vlEGL::messageLoop();

    /* deallocate the window with all the OpenGL resources before shutting down Visualization Library */
    egl_window = NULL;
  }
};

int main ( int argc, char *argv[] )
{ 

  /* parse command line arguments */
  int test = 0;
  if (argc>=2)
    test = atoi(argv[1]);

  vl::OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 5,6,5,0 );
  format.setDepthBufferBits(16);
  format.setStencilBufferBits(8);
  format.setMultisampleSamples(0);
  format.setMultisample(false);
  format.setVSync(false);
  /* NOTE THIS */
  format.setContextClientVersion(1);

  TestBatteryEGL test_battery;
  test_battery.run(test, argv[1], format);

  return 0;
}
