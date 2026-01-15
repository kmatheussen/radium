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

#ifndef EGL_Window_INCLUDE_ONCE
#define EGL_Window_INCLUDE_ONCE

#include <vlGraphics/OpenGLContext.hpp>
#include <vlEGL/link_config.hpp>
#include <EGL/egl.h>
#include <map>

namespace vlEGL
{
//-----------------------------------------------------------------------------
  VLEGL_EXPORT int messageLoop();
  VLEGL_EXPORT void peekMessage(MSG& msg);
  VLEGL_EXPORT void dispatchUpdate();
//-----------------------------------------------------------------------------
// EGLWindow
//-----------------------------------------------------------------------------
  /**
   * The EGLWindow class is a EGLContext that can be used as a top or child window.
  */
  class VLEGL_EXPORT EGLWindow: public vl::OpenGLContext
  {
  public:
    static const wchar_t* EGLWindowClassName;
    static LONG WINAPI WindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

  public:
    EGLWindow();
    ~EGLWindow();

    // *** OpenGLContext implementation ***

    void swapBuffers();
    void makeCurrent();
    void update();
    void quitApplication();
    void setMouseVisible(bool visible);
    void setPosition(int x, int y);
    void setSize(int w, int h);
    void setWindowSize(int w, int h);
    vl::ivec2 position() const;
    vl::ivec2 windowSize() const;
    vl::ivec2 size() const;
    void setWindowTitle(const vl::String& title);
    void show();
    void hide();
    void getFocus();
    void setMousePosition(int x, int y);

    //! Initializes a new EGL window with a new OpenGL rendering context.
    //! After the initialization to show the window use the show() method.
    bool initEGLWindow(HWND parent, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x=0, int y=0, int width=640, int height=480);

    //! Destroys the window and the OpenGL rendering context
    void destroyEGLGLWindow();

    HWND  hwnd()  const { return mHWND;  }

    //! The dwExStyle parameter passed to the EGL function CreateWindowEx
    DWORD style() const { return mStyle; }

    //! The dwExStyle parameter passed to the EGL function CreateWindowEx
    void setStyle(DWORD style) { mStyle = style; }

    //! The dwStyle parameter passed to the EGL function CreateWindowEx
    DWORD exStyle() const { return mStyle; }

    //! The dwStyle parameter passed to the EGL function CreateWindowEx
    void setExStyle(DWORD ex_style) { mStyle = ex_style; }

    //! Specifies the value given to the lpClassName parameter passed to CreateWindowEx.
    const wchar_t* windowClassName() const { return mWindowClassName; }

    //! Specifies the value given to the lpClassName parameter passed to CreateWindowEx.
    void setWindowClassName(const wchar_t* name) { mWindowClassName = name; }

    const EGLDisplay& eglDisplay() const { return mEGL_Display; }
    const EGLContext& eglContext() const { return mEGL_Context; }
    const EGLSurface& eglSurface() const { return mEGL_Surface; }

  protected:
    int   mMouseDownCount;
    HWND  mHWND;
    DWORD mStyle;
    DWORD mExStyle;
    const wchar_t* mWindowClassName;
    
    EGLDisplay mEGL_Display;
    EGLContext mEGL_Context;
    EGLSurface mEGL_Surface;

    // *** static members start here ***

  public:
    //! Returns when a WM_QUIT message is sent or when no more windows are alive.
    static EGLWindow* getWindow(HWND hWnd);
    static const std::map< HWND, EGLWindow* >& winMap() { return mWinMap; }

  protected:
    static std::map< HWND, EGLWindow* > mWinMap;
  };
//-----------------------------------------------------------------------------
  VLEGL_EXPORT void translateKeyEvent(WPARAM wParam, LPARAM lParam, unsigned short& unicode_out, vl::EKey& key_out);
//-----------------------------------------------------------------------------
}

#endif
