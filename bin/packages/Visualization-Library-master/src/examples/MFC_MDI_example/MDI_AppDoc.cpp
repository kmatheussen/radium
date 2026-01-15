// MDI_AppDoc.cpp : implementation of the CMDI_AppDoc class
//

#include "stdafx.h"
#include "MDI_App.h"

#include "MDI_AppDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CMDI_AppDoc

IMPLEMENT_DYNCREATE(CMDI_AppDoc, CDocument)

BEGIN_MESSAGE_MAP(CMDI_AppDoc, CDocument)
END_MESSAGE_MAP()


// CMDI_AppDoc construction/destruction

CMDI_AppDoc::CMDI_AppDoc()
{
	// TODO: add one-time construction code here

}

CMDI_AppDoc::~CMDI_AppDoc()
{
}

BOOL CMDI_AppDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}




// CMDI_AppDoc serialization

void CMDI_AppDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}


// CMDI_AppDoc diagnostics

#ifdef _DEBUG
void CMDI_AppDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CMDI_AppDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG


// CMDI_AppDoc commands
