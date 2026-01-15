#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"
#include <vector>
#include <algorithm>

class CMDI_AppView;

class CMDI_App : public CWinApp
{
public:
	CMDI_App();

  // VL: initializes the application to use Visualization Library
  void InitVisualizationLibrary();

  // VL: keeps track of the opened GL windows to update them on-idle
  void AddView(CMDI_AppView* view) 
  { 
    m_Views.push_back(view); 
  }

  // VL: keeps track of the opened GL windows to update them on-idle
  void RemoveView(CMDI_AppView* view)
  {
    std::vector<CMDI_AppView*>::iterator it = std::find(m_Views.begin(), m_Views.end(), view);
    if (it != m_Views.end())
      m_Views.erase(it);
  }

public:
  // VL: initialize VL here 
	virtual BOOL InitInstance();

  // VL: shutdown VL here 
  virtual int ExitInstance();

  // VL: update the GL windows here
  virtual BOOL OnIdle(LONG lCount);

	DECLARE_MESSAGE_MAP()

private:
  std::vector<CMDI_AppView*> m_Views;
};

extern CMDI_App theApp;