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

#ifndef MFCWindow_INCLUDE_ONCE
#define MFCWindow_INCLUDE_ONCE

#include <vlMFC/link_config.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlWin32/Win32Context.hpp>

namespace vlMFC
{
//-----------------------------------------------------------------------------
// MFCWindow
//-----------------------------------------------------------------------------
  /**
   * The MFCWindow class is an MFC CWnd with the functionalities of a Win32Context.
   */
  class VLMFC_EXPORT MFCWindow: public CWnd, public vlWin32::Win32Context /* the order is important! */
  {
  public:
    MFCWindow() { vl::OpenGLContext::setAutomaticDelete(false); }

    virtual ~MFCWindow();

    //! Creates the window and initializes the OpenGL rendering context
    bool initMFCWindow(CWnd* parent, MFCWindow* share_context, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x=0, int y=0, int width=640, int height=480);

    //! Returns the Win32 window handle
    HWND hwnd() const { return m_hWnd; }

  public:
	  //{{AFX_MSG(MFCContext
    /*afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);*/
	  /*afx_msg void OnDraw(CDC *pDC);*/
    afx_msg void OnPaint();
    // afx_msg void OnClose();
    afx_msg void OnDestroy();
    afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
    afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
    afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
    afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
    afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
    afx_msg void OnMButtonDblClk(UINT nFlags, CPoint point);
    afx_msg void OnMButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
    afx_msg void OnMouseMove(UINT nFlags, CPoint point);
    afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
    afx_msg void OnRButtonDblClk(UINT nFlags, CPoint point);
    afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
    afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
    afx_msg void OnSize(UINT nType, int cx, int cy);
    /*afx_msg void OnTimer(UINT_PTR nIDEvent);*/
    afx_msg void OnDropFiles(HDROP hDropInfo);
	  //}}AFX_MSG

  protected:
    void destroyGLContext();
    void countAndCapture();
    void countAndRelease();

  protected:
    int mMouseDownCount;
    static CString mClassName;

    DECLARE_MESSAGE_MAP()
  };
}

#endif
