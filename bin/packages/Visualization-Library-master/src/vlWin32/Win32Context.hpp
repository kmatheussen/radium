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

#ifndef Win32Context_INCLUDE_ONCE
#define Win32Context_INCLUDE_ONCE

#include <vlWin32/link_config.hpp>
#include <vlGraphics/OpenGLContext.hpp>

namespace vlWin32
{
//-----------------------------------------------------------------------------
  VLWIN32_EXPORT int choosePixelFormat(const vl::OpenGLContextFormat& fmt, bool verbose = true);
//-----------------------------------------------------------------------------
  /**
   * The Win32Context class implements an OpenGLContext using the Win32 API.
   */
  class VLWIN32_EXPORT Win32Context: public vl::OpenGLContext
  {
  public:
    Win32Context(): mHDC(NULL), mHGLRC(NULL) {}

    Win32Context(int w, int h): OpenGLContext(w,h), mHDC(NULL), mHGLRC(NULL) {}

    ~Win32Context();
  
    virtual HWND hwnd() const = 0;

    HDC   hdc()   const { return mHDC;   }
    
    HGLRC hglrc() const { return mHGLRC; }

    //! Use this function when you want two OpenGL contexts to share their resources (display lists, textures, shader objects, buffer objects etc.)
    //! Equivalent to wglShareLists(this->hglrc(), hGLRC)
    //! \remarks 
    //! If you want to share resources among two or more OpenGL contexts, you must call this function 
    //! before you start creating any resources.
    void shareOpenGLResources(HGLRC hGLRC);

    void makeCurrent();

    void update();

    void swapBuffers();

    void setWindowTitle(const vl::String& title);

    void show();

    void hide();

    void getFocus();

    void setMouseVisible(bool visible);

    void setMousePosition(int x, int y);

    void setPosition(int x, int y);

    vl::ivec2 position() const;

    //! The actual size of the OpenGL context, i.e. the client area if this is a window.
    //! Note that if this Win32Window has window decorations the actual window size will be bigger than the given w and h parameters.
    void setSize(int w, int h);

    //! The actual size of the OpenGL context, i.e. the client area if this is a window.
    vl::ivec2 size() const;

    //! Sets the size of the window. Note that if this Win32Window has window decorations the actual OpenGL context will be smaller than the given w and h parameters.
    void setWindowSize(int w, int h);

    //! Returns the size of the window and not the client area. Use the size() method if you need the size of the actual OpenGL rendering context.
    //! \note windowSize() can be different from size() because of the space taken by the window caption and decorations.
    vl::ivec2 windowSize() const;

    bool setFullscreen(bool fullscreen_on);

    //! Calls the PostQuitMessage(0) function (Win32 API).
    void quitApplication();

    //! Context attributes used when creating an OpenGL 3.x / 4.x context. 
    //! The flags must be the ones specified by http://www.opengl.org/registry/specs/ARB/wgl_create_context.txt
    const std::vector<int>& contextAttribs() const { return mContextAttribs; }

    //! Context attributes used when creating an OpenGL 3.x / 4.x context. 
    //! The flags must be the ones specified by http://www.opengl.org/registry/specs/ARB/wgl_create_context.txt
    std::vector<int>& contextAttribs() { return mContextAttribs; }
    
    //! Context attributes used when creating an OpenGL 3.x / 4.x context. 
    //! The flags must be the ones specified by http://www.opengl.org/registry/specs/ARB/wgl_create_context.txt
    void setContextAttribs(const int* attribs, int size);

  protected:
    bool initWin32GLContext(HGLRC share_context, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x, int y, int width, int height);

  protected:
    std::vector<int> mContextAttribs;

    HDC   mHDC;
    HGLRC mHGLRC;

    vl::ivec2 mNormPosit;
    vl::ivec2 mNormSize;
    unsigned int mNormFlags;
  };
}
//-----------------------------------------------------------------------------

#endif
