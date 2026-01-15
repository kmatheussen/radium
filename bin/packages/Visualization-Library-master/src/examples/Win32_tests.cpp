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

#include <vlCore/DiskDirectory.hpp>
#include <vlWin32/Win32Window.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include "tests.hpp"

using namespace vl;
using namespace vlWin32;

class TestBatteryWin32: public TestBattery
{
public:
  void runGUI(const String& title, BaseDemo* applet, OpenGLContextFormat format, int x, int y, int width, int height, fvec4 bk_color, vec3 eye, vec3 center)
  {
    /* used to display the application title next to FPS counter */
    applet->setAppletName(title);

    /* create a native Win32 window */
    ref<vlWin32::Win32Window> win32_window = new vlWin32::Win32Window;

    setupApplet(applet, win32_window.get(), bk_color, eye, center);

    /* Used to test OpenGL Core Profile */
#if 0
    int attribs[] =
    {
        WGL_CONTEXT_MAJOR_VERSION_ARB, 4,
        WGL_CONTEXT_MINOR_VERSION_ARB, 1,

        // Includes removed & deprecated features.
        // WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB, 

        // Does not include previously removed features, but might include currently deprecated (not yet removed) ones.
        // WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,

        // Does not include any (previously or currently) deprecated feature.
        WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,

        0
    };
    win32_window->setContextAttribs(attribs, sizeof(attribs)/sizeof(attribs[0]));
#endif

    /* Initialize the OpenGL context and window properties */
    win32_window->initWin32GLWindow(NULL, NULL, title, format, x, y, width, height );

    /* show the window */
    win32_window->show();

    /* run the Win32 message loop */
    vlWin32::messageLoop();

    /* deallocate the window with all the OpenGL resources before shutting down Visualization Library */
    win32_window = NULL;
  }
};
//-----------------------------------------------------------------------------
int APIENTRY WinMain(HINSTANCE /*hCurrentInst*/, HINSTANCE /*hPreviousInst*/, LPSTR lpszCmdLine, int /*nCmdShow*/)
{
  /* parse command line arguments */
  int test = 0;
  String cmd = lpszCmdLine;
  std::vector<String> parms;
  cmd.split(' ', parms);
  std::string test_str;
  if (parms.size()>=1)
  {
    test = parms[0].toInt();
    test_str = parms[0].toStdString();
  }

  /* setup the OpenGL context format */
  OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8, 8, 8, 0 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  /*format.setMultisampleSamples(8);
  format.setMultisample(true);*/

  TestBatteryWin32 test_battery;
  test_battery.run(test, test_str, format);

  return 0;
}
