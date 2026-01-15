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

#ifndef Win32_Window_INCLUDE_ONCE
#define Win32_Window_INCLUDE_ONCE

#include <vlGraphics/OpenGLContext.hpp>
#include <vlWin32/Win32Context.hpp>
#include <map>

namespace vlWin32
{
//-----------------------------------------------------------------------------
  VLWIN32_EXPORT int messageLoop();
  VLWIN32_EXPORT void peekMessage(MSG& msg);
  VLWIN32_EXPORT void dispatchUpdate();
//-----------------------------------------------------------------------------
// Win32Window
//-----------------------------------------------------------------------------
  /**
   * The Win32Window class is a Win32Context that can be used as a top or child window.
  */
  class VLWIN32_EXPORT Win32Window: public Win32Context
  {
  public:
    static LONG WINAPI WindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

  public:
    Win32Window();
    ~Win32Window();

    //! Initializes a new Win32 window with a new OpenGL rendering context.
    //! After the initialization to show the window use the show() method.
    bool initWin32GLWindow(HWND parent, HGLRC share_context, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x=0, int y=0, int width=640, int height=480);

    //! Destroys the window and the OpenGL rendering context
    void destroyWin32GLWindow();

    HWND  hwnd()  const { return mHWND;  }

    //! The dwExStyle parameter passed to the Win32 function CreateWindowEx
    DWORD style() const { return mStyle; }

    //! The dwExStyle parameter passed to the Win32 function CreateWindowEx
    void setStyle(DWORD style) { mStyle = style; }

    //! The dwStyle parameter passed to the Win32 function CreateWindowEx
    DWORD exStyle() const { return mStyle; }

    //! The dwStyle parameter passed to the Win32 function CreateWindowEx
    void setExStyle(DWORD ex_style) { mStyle = ex_style; }

    //! Specifies the value given to the lpClassName parameter passed to CreateWindowEx.
    const wchar_t* windowClassName() const { return mWindowClassName; }

    //! Specifies the value given to the lpClassName parameter passed to CreateWindowEx.
    void setWindowClassName(const wchar_t* name) { mWindowClassName = name; }

  protected:
    int   mMouseDownCount;
    HWND  mHWND;
    DWORD mStyle;
    DWORD mExStyle;
    const wchar_t* mWindowClassName;
    
    // *** static members start here ***

  public:
    //! Returns when a WM_QUIT message is sent or when no more windows are alive.
    static Win32Window* getWindow(HWND hWnd);
    static const std::map< HWND, Win32Window* >& winMap() { return mWinMap; }

  protected:
    static std::map< HWND, Win32Window* > mWinMap;
  };
//-----------------------------------------------------------------------------
  VLWIN32_EXPORT void translateKeyEvent(WPARAM wParam, LPARAM lParam, unsigned short& unicode_out, vl::EKey& key_out);
//-----------------------------------------------------------------------------
}

#endif
