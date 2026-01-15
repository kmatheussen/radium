// MDI_AppDoc.h : interface of the CMDI_AppDoc class
//


#pragma once


class CMDI_AppDoc : public CDocument
{
protected: // create from serialization only
	CMDI_AppDoc();
	DECLARE_DYNCREATE(CMDI_AppDoc)

// Attributes
public:

// Operations
public:

// Overrides
public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);

// Implementation
public:
	virtual ~CMDI_AppDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
};


