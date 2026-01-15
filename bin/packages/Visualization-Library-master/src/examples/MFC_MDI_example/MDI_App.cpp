#include "stdafx.h"
#include "MDI_App.h"
#include "MainFrm.h"

#include "ChildFrm.h"
#include "MDI_AppDoc.h"
#include "MDI_AppView.h"

#include <vlCore/VisualizationLibrary.hpp>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//-----------------------------------------------------------------------------
BEGIN_MESSAGE_MAP(CMDI_App, CWinApp)
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, &CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, &CWinApp::OnFileOpen)
END_MESSAGE_MAP()
//-----------------------------------------------------------------------------
// The one and only CMDI_App object
CMDI_App theApp;
//-----------------------------------------------------------------------------
CMDI_App::CMDI_App()
{
	// Place all significant initialization in InitInstance
}
//-----------------------------------------------------------------------------
BOOL CMDI_App::OnIdle(LONG /*lCount*/)
{
  // VL: update the windows that require continuous update
  for(size_t i=0; i<m_Views.size(); ++i)
  {
    if (m_Views[i]->continuousUpdate())
      m_Views[i]->MDIWindow::update();
    else
      Sleep(1);
  }
  return TRUE;
}
//-----------------------------------------------------------------------------
void CMDI_App::InitVisualizationLibrary()
{
  /* open a console so we can see the program's output on stdout */
  vl::showWin32Console();

  /* init Visualization Library */
  vl::VisualizationLibrary::init();
}
//-----------------------------------------------------------------------------
int CMDI_App::ExitInstance()
{
  __super::ExitInstance();

  /* shutdown Visualization Library */
  vl::VisualizationLibrary::shutdown();

  return 0;
}
//-----------------------------------------------------------------------------
BOOL CMDI_App::InitInstance()
{
  // VL stuff...

  InitVisualizationLibrary();

  // MFC stuff...

  __super::InitInstance();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	// of your final executable, you should remove from the following
	// the specific initialization routines you do not need
	// Change the registry key under which our settings are stored
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization
	SetRegistryKey(_T("Local AppWizard-Generated Applications"));
	LoadStdProfileSettings(4);  // Load standard INI file options (including MRU)
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views
	CMultiDocTemplate* pDocTemplate;
	pDocTemplate = new CMultiDocTemplate(IDR_MDI_AppTYPE,
		RUNTIME_CLASS(CMDI_AppDoc),
		RUNTIME_CLASS(CChildFrame), // custom MDI child frame
		RUNTIME_CLASS(CMDI_AppView));
	if (!pDocTemplate)
		return FALSE;
	AddDocTemplate(pDocTemplate);

	// create main MDI Frame window
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame || !pMainFrame->LoadFrame(IDR_MAINFRAME))
	{
		delete pMainFrame;
		return FALSE;
	}
	m_pMainWnd = pMainFrame;
	// call DragAcceptFiles only if there's a suffix
	//  In an MDI app, this should occur immediately after setting m_pMainWnd


	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);


	// Dispatch commands specified on the command line.  Will return FALSE if
	// app was launched with /RegServer, /Register, /Unregserver or /Unregister.
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
	// The main window has been initialized, so show and update it
	pMainFrame->ShowWindow(m_nCmdShow);
	pMainFrame->UpdateWindow();

	return TRUE;
}
//-----------------------------------------------------------------------------
