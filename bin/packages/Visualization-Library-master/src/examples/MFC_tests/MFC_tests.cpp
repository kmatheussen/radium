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
#include <vlCore/VisualizationLibrary.hpp>
#include <vlMFC/MFCWindow.hpp>
#include <vlCore/DiskDirectory.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include "tests.hpp"

using namespace vl;
using namespace vlWin32;
using namespace vlMFC;

//-----------------------------------------------------------------------------
/* TestBattery implementation to work with MFC */
class TestBatteryMFC: public TestBattery
{
public:
  TestBatteryMFC(MFCWindow* mfc_win): mVLCWin(mfc_win) {}

  void runGUI(const vl::String& title, BaseDemo* program, vl::OpenGLContextFormat format, int x, int y, int width, int height, vl::fvec4 bk_color, vl::vec3 eye, vl::vec3 center)
  {
    program->setAppletName(title);

    setupApplet(program, mVLCWin, bk_color, eye, center);

    /* Initialize the OpenGL context and window properties */
    mVLCWin->initMFCWindow(NULL, NULL, title, format, x, y, width, height );
  }

protected:
  MFCWindow* mVLCWin;
};
//-----------------------------------------------------------------------------
/* MFC_Test: implements the MFC application */
class MFC_Test: public CWinApp
{
public:
  MFC_Test()
  {
    mVLCWin = new MFCWindow;
  }

  virtual BOOL InitInstance();
  virtual int ExitInstance();
  /*virtual int Run();*/
  virtual BOOL OnIdle(LONG lCount);

protected:
  ref<MFCWindow> mVLCWin;

  DECLARE_MESSAGE_MAP ()
};
//-----------------------------------------------------------------------------
BEGIN_MESSAGE_MAP(MFC_Test, CWinApp)
END_MESSAGE_MAP()
//-----------------------------------------------------------------------------
/* instance the MFC application*/
MFC_Test mfc_app;
//-----------------------------------------------------------------------------
/* called when the application exits */
int MFC_Test::ExitInstance()
{
  CWinApp::ExitInstance();

  /* destroy the window and OpenGL rendering context */
  mVLCWin = NULL;

  return 0;
}
//-----------------------------------------------------------------------------
/* called when the application starts */
BOOL MFC_Test::InitInstance()
{
  CWinApp::InitInstance();

  /* parse the command line */
  vl::String cmd = m_lpCmdLine;
  int test  = 0;
  std::vector<String> parms;
  cmd.split(' ', parms);
  std::string test_str;
  if (parms.size()>=1)
  {
    test = parms[0].toInt();
    test_str = parms[0].toStdString();
  }

  /* setup the OpenGL context format */
  vl::OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8,8,8,0 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  /*format.setMultisampleSamples(16);
  format.setMultisample(true);*/

  TestBatteryMFC test_battery(mVLCWin.get());
  test_battery.run(test, test_str, format, false);

  /* MFC specific stuff */
  if (mVLCWin->m_hWnd)
  {
    m_pMainWnd = mVLCWin.get();
    m_pMainWnd->ShowWindow(m_nCmdShow);
    m_pMainWnd->UpdateWindow();
  }

  return TRUE;
}
//-----------------------------------------------------------------------------
BOOL MFC_Test::OnIdle(LONG lCount)
{
  if( mVLCWin->continuousUpdate() )
    mVLCWin->Win32Context::update();
  else
    Sleep(1);
  return TRUE;
}
//-----------------------------------------------------------------------------
