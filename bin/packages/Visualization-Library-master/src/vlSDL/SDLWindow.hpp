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

#ifndef SDLAdapter_INCLUDE_ONCE
#define SDLAdapter_INCLUDE_ONCE

#include <vlSDL/link_config.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlCore/String.hpp>
#include <vlCore/Vector4.hpp>
#include <SDL.h>

namespace vlut
{
  class Applet;
}

namespace vlSDL
{
  void messageLoop();

//-----------------------------------------------------------------------------
// SDLWindow
//-----------------------------------------------------------------------------
  /**
   * The SDLWindow class implements an OpenGLContext using the SDL API.
   * @note
   * SDL notifies Unicode codes only on key-press events not on release events.
  */
  class VLSDL_EXPORT SDLWindow: public vl::OpenGLContext
  {
  public:
    SDLWindow();
    SDLWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x=0, int y=0, int width=640, int height=480);
    bool initSDLWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x=0, int y=0, int width=640, int height=480);

    ~SDLWindow();

    void setPosition(int x, int y);

    virtual void swapBuffers();

    void translateEvent(SDL_Event * ev);

    //! Quits the event loop
    void quitApplication();

    void setWindowTitle(const vl::String&);

    void setMouseVisible(bool visible)
    {
      mMouseVisible = visible;
      SDL_ShowCursor(visible ? SDL_ENABLE : SDL_DISABLE);
    }

    void setMousePosition(int x, int y)
    {
      SDL_WarpMouse((Uint16)x, (Uint16)y);
    }

    void update();

    void makeCurrent() { /*SDL can have only one window*/ }

    static SDL_Surface* sdlSurface() { return mScreen; }

  protected:
    static SDL_Surface* mScreen;
  };
}

#endif
