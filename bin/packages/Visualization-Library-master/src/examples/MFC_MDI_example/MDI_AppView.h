#pragma once

#include <vlMFC/MDIWindow.hpp>

/* VL: subclass MDIWindow */
class CMDI_AppView : public vlMFC::MDIWindow
{
protected:
	CMDI_AppView();
	virtual ~CMDI_AppView();
	DECLARE_DYNCREATE(CMDI_AppView)

  // VL: initialize the OpenGL context here!
  virtual void OnInitialUpdate();

  // the rest is MFC stuff...

	CMDI_AppDoc* GetDocument() const;

	virtual void OnDraw(CDC* pDC);  // overridden to draw this view

protected:
	DECLARE_MESSAGE_MAP()
};
