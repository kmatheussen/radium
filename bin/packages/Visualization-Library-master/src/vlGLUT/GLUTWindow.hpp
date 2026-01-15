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

#ifndef GLUT_Window_INCLUDE_ONCE
#define GLUT_Window_INCLUDE_ONCE

#include <vlGLUT/link_config.hpp>
#include <vlCore/Vector4.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <map>
#include <cstdlib> // exit()
#if defined(__APPLE__)
  #include <GLUT/glut.h>
#else
  #include <GL/freeglut.h>
#endif

namespace vlut
{
  class Applet;
}

namespace vlGLUT
{
//-----------------------------------------------------------------------------
// GLUTWindow
//-----------------------------------------------------------------------------
  /**
   * The GLUTWindow class implements an OpenGLContext using the GLUT API.
  */
  class VLGLUT_EXPORT GLUTWindow: public vl::OpenGLContext
  {
  public:
    GLUTWindow();
    
    GLUTWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x, int y, int width, int height);
    
    //! Initializes and shows a GLUT window.
    bool initGLUTWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x, int y, int width, int height);

    ~GLUTWindow()
    {
      destroyWindow();
    }

    virtual void setMouseVisible(bool visible);

    virtual void setMousePosition(int x, int y);

    //! Quits the progra calling the C function exit(0).
    void quitApplication() { eraseAllEventListeners(); exit(0); }

    void update();

    bool setFullscreen(bool fs);

    void makeCurrent();

    void updateOverlay();

    void swapBuffers();

    void setWindowTitle(const vl::String& title) ;

    int handle() const { return mHandle; }

    void setPosition(int x, int y) ;

    void setSize(int w, int h) ;

    vl::ivec2 position() const;

    vl::ivec2 size() const;

    void show();

    void hide();

    void getFocus();

  protected:
    void destroyWindow();
    void initKeymap();

    vl::EKey mapAsciiKey(unsigned char ascii);

    static vl::EKey mapSpecialKey(int special_key);

    void updateModifiers();

    static void glut_keyboard_func(unsigned char ch, int x, int y);

    static void glut_keyboard_up_func(unsigned char ch, int x, int y);

    static void glut_special_func(int key, int x, int y);

    static void glut_special_up_func(int key, int x, int y);

    static void glut_mouse_func(int button, int state, int x, int y);

    static void glut_motion_func(int x, int y);

    static void glut_passive_motion_func(int x, int y);

    static void glut_mouse_wheel_func(int a, int rotation, int c, int d);

    static void glut_visibility_func(int visibility);

    static void glut_reshape_func(int w, int h);

    static void glut_display_func();

    static void glut_close_func();

    static void glut_wmclose_func();

    // used for continuous update
    static void glut_idle_func();

    // static void glut_overlay_display_func();

    // does not seem to work properly or usefully
    // static void glut_entry_func(int leave_enter);

    // not used
    // static void glut_timer_func(int value);

  protected:
    std::map<unsigned char, vl::EKey> mKeymap;
    int mHandle;
    bool mInited;

    static std::map< int, GLUTWindow* > mWinMap;
  };

  VLGLUT_EXPORT void atexit_visualization_library_shutdown();
}

#endif
