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

#include "StdAfx.h"

#include <vlMFC/MFCWindow.hpp>
#include <vlWin32/Win32Window.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Time.hpp>
#include <shellapi.h>

using namespace vl;
using namespace vlMFC;

//-----------------------------------------------------------------------------
// MFCWindow
//-----------------------------------------------------------------------------
CString MFCWindow::mClassName;
//-----------------------------------------------------------------------------
BEGIN_MESSAGE_MAP(MFCWindow, CWnd)
  ON_WM_CHAR()
  ON_WM_CLOSE()
  ON_WM_CREATE()
  ON_WM_KEYDOWN()
  ON_WM_KEYUP()
  ON_WM_LBUTTONDBLCLK()
  ON_WM_LBUTTONDOWN()
  ON_WM_LBUTTONUP()
  ON_WM_MBUTTONDBLCLK()
  ON_WM_MBUTTONDOWN()
  ON_WM_MBUTTONUP()
  ON_WM_MOUSEMOVE()
  ON_WM_MOUSEWHEEL()
  ON_WM_PAINT()
  ON_WM_RBUTTONDBLCLK()
  ON_WM_RBUTTONDOWN()
  ON_WM_RBUTTONUP()
  ON_WM_SIZE()
  ON_WM_TIMER()
  ON_WM_DROPFILES()
  ON_WM_DESTROY()
END_MESSAGE_MAP()
/*
  WM_SYSKEYDOWN
  WM_SYSKEYUP
  WM_GETICON
  WM_SETCURSOR
  WM_SETICON
  WM_CAPTURECHANGED
  WM_MOUSEFIRST 
*/
//-----------------------------------------------------------------------------
MFCWindow::~MFCWindow()
{
  dispatchDestroyEvent();
}
//-----------------------------------------------------------------------------
int MFCWindow::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
  if (CWnd::OnCreate(lpCreateStruct) == -1)
    return -1;
  return 0;
}
//-----------------------------------------------------------------------------
bool MFCWindow::initMFCWindow(CWnd* parent, MFCWindow* share_context, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x, int y, int width, int height)
{
  destroyGLContext();

  // register the class only once
  if (mClassName.IsEmpty())
 	  mClassName = AfxRegisterWndClass(CS_HREDRAW | CS_VREDRAW | CS_OWNDC | CS_DBLCLKS, LoadCursor(NULL, IDC_ARROW), NULL, LoadIcon(NULL, IDI_WINLOGO));

  CreateEx(WS_EX_APPWINDOW|WS_EX_ACCEPTFILES, 
           mClassName, L"MFCWindow", 
           (parent?WS_CHILD:WS_OVERLAPPEDWINDOW) | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, 
           x, y, width, height, 
           parent?parent->m_hWnd:NULL, NULL, NULL);

  return initWin32GLContext(share_context?share_context->hglrc():NULL, title, fmt, x, y, width, height);
}
//-----------------------------------------------------------------------------
void MFCWindow::destroyGLContext()
{
  // wglMakeCurrent(NULL, NULL) not needed 
  if (hwnd())
  {
    if (mHGLRC)
    {
      if ( wglDeleteContext(mHGLRC) == FALSE )
      {
        MessageBox( L"OpenGL context creation failed.\n"
         L"The handle either doesn't specify a valid context or the context is being used by another thread.", L"MFCWindow::destroyGLContext() error!", MB_OK);
      }
      mHGLRC = NULL;
    }

    if (mHDC)
    {
      DeleteDC(mHDC);
      mHDC = NULL;
    }
  }
}
//-----------------------------------------------------------------------------
void MFCWindow::OnDestroy()
{
  dispatchDestroyEvent();
  destroyGLContext();
}
//-----------------------------------------------------------------------------
void MFCWindow::OnPaint()
{
  if (hwnd() && hdc() && hglrc())
    dispatchRunEvent();
  ValidateRect(NULL);
}
//-----------------------------------------------------------------------------
/*void MFCWindow::OnDraw(CDC *pDC)
{
}*/
//-----------------------------------------------------------------------------
/*void MFCWindow::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
  unsigned short unicode_out = 0;
  vl::EKey       key_out     = Key_None;
  vlWin32::translateKeyEvent(nChar, nFlags, unicode_out, key_out);
  dispatchKeyPressEvent(unicode_out, key_out);
}*/
//-----------------------------------------------------------------------------
void MFCWindow::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
  unsigned short unicode_out = 0;
  vl::EKey       key_out     = Key_None;
  vlWin32::translateKeyEvent(nChar, nFlags, unicode_out, key_out);
  dispatchKeyPressEvent(unicode_out, key_out);
}
//-----------------------------------------------------------------------------
void MFCWindow::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags)
{
  unsigned short unicode_out = 0;
  vl::EKey       key_out     = Key_None;
  vlWin32::translateKeyEvent(nChar, nFlags, unicode_out, key_out);
  dispatchKeyReleaseEvent(unicode_out, key_out);
}
//-----------------------------------------------------------------------------
void MFCWindow::countAndCapture()
{
  mMouseDownCount++;
  if (mMouseDownCount == 1)
    ::SetCapture(hwnd());
}
//-----------------------------------------------------------------------------
void MFCWindow::countAndRelease()
{
  mMouseDownCount--;
  if (mMouseDownCount <= 0)
  {
    ReleaseCapture();
    mMouseDownCount = 0;
  }
}
//-----------------------------------------------------------------------------
void MFCWindow::OnLButtonDblClk(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( LeftButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnLButtonDown(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( LeftButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnLButtonUp(UINT nFlags, CPoint point)
{
  countAndRelease();
  dispatchMouseUpEvent( LeftButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnMButtonDblClk(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( MiddleButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnMButtonDown(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( MiddleButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnMButtonUp(UINT nFlags, CPoint point)
{
  countAndRelease();
  dispatchMouseUpEvent( MiddleButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnMouseMove(UINT nFlags, CPoint point)
{
  dispatchMouseMoveEvent( point.x, point.y );
}
//-----------------------------------------------------------------------------
BOOL MFCWindow::OnMouseWheel(UINT nFlags, short zDelta, CPoint pt)
{
  dispatchMouseWheelEvent (zDelta/120);
  return FALSE;
}
//-----------------------------------------------------------------------------
void MFCWindow::OnRButtonDblClk(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( RightButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnRButtonDown(UINT nFlags, CPoint point)
{
  countAndCapture();
  dispatchMouseDownEvent( RightButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnRButtonUp(UINT nFlags, CPoint point)
{
  countAndRelease();
  dispatchMouseUpEvent( RightButton, point.x, point.y );
}
//-----------------------------------------------------------------------------
void MFCWindow::OnDropFiles(HDROP hDrop)
{
  int count = DragQueryFile(hDrop, 0xFFFFFFFF, 0, 0);
  const int char_count = 1024;
  std::vector<String> files;
  for(int i=0; i<count; ++i)
  {
    wchar_t file_path[char_count];
    memset(file_path, 0, char_count);
    DragQueryFile(hDrop,i,file_path,char_count);
    files.push_back(file_path);
  }
  dispatchFileDroppedEvent(files);
}
//-----------------------------------------------------------------------------
void MFCWindow::OnSize (UINT nType, int cx, int cy)
{
  CWnd::OnSize(nType, cx, cy);

  if (0 >= cx || 0 >= cy || nType == SIZE_MINIMIZED)
    return;

  framebuffer()->setWidth(cx);
  framebuffer()->setHeight(cy);
  dispatchResizeEvent(cx, cy);
}
//-----------------------------------------------------------------------------
