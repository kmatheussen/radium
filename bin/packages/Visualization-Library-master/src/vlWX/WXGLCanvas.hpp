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

#ifndef vlWXGLCanvas_INCLUDE_ONCE
#define vlWXGLCanvas_INCLUDE_ONCE

#include <vlWX/link_config.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlCore/Time.hpp>
#include <wx/frame.h>
#include <wx/glcanvas.h>
#include <wx/timer.h>
#include <wx/dcclient.h>
#include <wx/image.h>
#include <wx/app.h>

#if !wxUSE_GLCANVAS
  #error "OpenGL required: set wxUSE_GLCANVAS to 1 and rebuild the library"
#endif

namespace vlWX
{
  /** The WXGLCanvas class implements a vl::OpenGLContext using the wxWidgets library. */
  class VLWX_EXPORT WXGLCanvas: public wxGLCanvas, public vl::OpenGLContext
  {
  public:
    WXGLCanvas( wxWindow *parent,
      const wxGLContext *shared,
      wxWindowID id = wxID_ANY,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = 0,
      int *attribList = 0,
      const wxString& name = wxT("WXGLCanvas"),
      const wxPalette& palette = wxNullPalette );

    ~WXGLCanvas();

    void OnPaint(wxPaintEvent& ev);
    void OnSize(wxSizeEvent& ev);
    void OnEraseBackground(wxEraseEvent& ev);
    void OnKeyDown(wxKeyEvent& ev);
    void OnKeyUp(wxKeyEvent& ev);
    /*void OnChar(wxKeyEvent& ev);*/
    void OnMouseMotion(wxMouseEvent &ev);
    void OnMouseDown(wxMouseEvent &ev);
    void OnMouseUp(wxMouseEvent &ev);
    void OnMouseWheel( wxMouseEvent& ev );
    void OnMouseEnter(wxMouseEvent& ev);
    void OnIdle(wxIdleEvent& ev);
    void OnDropFiles(wxDropFilesEvent& ev);

    bool setFullscreen(bool fullscreen);
    void quitApplication();
    void makeCurrent();
    void swapBuffers();
    void getFocus();
    void setMousePosition(int x, int y);
    void update();
    void setWindowTitle(const vl::String& text);
    void show();
    void hide();
    void setPosition(int x, int y);
    void setSize(int w, int h);
    vl::ivec2 position() const;
    void setMouseVisible(bool visible);

  private:
    wxCursor mCursor;
    int mMouseCount;
    DECLARE_EVENT_TABLE()
  };
}

#endif
