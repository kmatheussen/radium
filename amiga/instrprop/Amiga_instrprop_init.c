/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */









#include <exec/types.h>
#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <graphics/displayinfo.h>
#include <graphics/gfxbase.h>
#include <clib/exec_protos.h>
#include <clib/intuition_protos.h>
#include <clib/gadtools_protos.h>
#include <clib/graphics_protos.h>
#include <clib/utility_protos.h>
#include <string.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/gadtools_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/utility_pragmas.h>

#include "Amiga_instrprop.h"

int myHandleCPPWindowIDCMP( void )
{
	struct IntuiMessage	*m;
	struct MenuItem		*n;
	int			(*func)();
	BOOL			running = TRUE;
	int ret=0;

	while( m = GT_GetIMsg( CPPWindowWnd->UserPort )) {

		CopyMem(( char * )m, ( char * )&CPPWindowMsg, (long)sizeof( struct IntuiMessage ));

		GT_ReplyIMsg( m );

		switch ( CPPWindowMsg.Class ) {

			case	IDCMP_REFRESHWINDOW:
				GT_BeginRefresh( CPPWindowWnd );
				GT_EndRefresh( CPPWindowWnd, TRUE );
				break;

			case	IDCMP_RAWKEY:
				ret = CPPWindowRawKey();
				break;

			case IDCMP_MOUSEMOVE:
			case	IDCMP_GADGETUP:
			case	IDCMP_GADGETDOWN:
				func = ( void * )(( struct Gadget * )CPPWindowMsg.IAddress )->UserData;
				running = func();
				break;

			case	IDCMP_MENUPICK:
				while( CPPWindowMsg.Code != MENUNULL ) {
					n = ItemAddress( CPPWindowMenus, CPPWindowMsg.Code );
					func = (void *)(GTMENUITEM_USERDATA( n ));
					running = func();
					CPPWindowMsg.Code = n->NextSelect;
				}
				break;
		}
	}
	return(ret);
}

int myOpenCPPWindowWindow( void )
{
	struct NewGadget	ng;
	struct Gadget	*g;
	UWORD		lc, tc;
	UWORD		offx = Scr->WBorLeft, offy = Scr->WBorTop + Scr->RastPort.TxHeight + 1;

	if ( ! ( g = CreateContext( &CPPWindowGList )))
		return( 1L );

	for( lc = 0, tc = 0; lc < CPPWindow_CNT; lc++ ) {

		CopyMem((char * )&CPPWindowNGad[ lc ], (char * )&ng, (long)sizeof( struct NewGadget ));

		ng.ng_VisualInfo = VisualInfo;
		ng.ng_TextAttr   = &topaz8;
		ng.ng_LeftEdge  += offx;
		ng.ng_TopEdge   += offy;

		CPPWindowGadgets[ lc ] = g = CreateGadgetA((ULONG)CPPWindowGTypes[ lc ], g, &ng, ( struct TagItem * )&CPPWindowGTags[ tc ] );

		while( CPPWindowGTags[ tc ] ) tc += 2;
		tc++;

		if ( NOT g )
			return( 2L );
	}

	if ( ! ( CPPWindowMenus = CreateMenus( CPPWindowNewMenu, GTMN_FrontPen, 0L, TAG_DONE )))
		return( 3L );

	LayoutMenus( CPPWindowMenus, VisualInfo, GTMN_TextAttr, &topaz8, TAG_DONE );

	if ( ! ( CPPWindowWnd = OpenWindowTags( NULL,
				WA_Left,	CPPWindowLeft,
				WA_Top,		CPPWindowTop,
				WA_Width,	CPPWindowWidth,
				WA_Height,	CPPWindowHeight + offy,
				WA_IDCMP,	IDCMP_MOUSEMOVE|STRINGIDCMP|BUTTONIDCMP|INTEGERIDCMP|SLIDERIDCMP|CHECKBOXIDCMP|IDCMP_MENUPICK|IDCMP_REFRESHWINDOW | IDCMP_RAWKEY,
				WA_Flags,	WFLG_DRAGBAR|WFLG_DEPTHGADGET|WFLG_SMART_REFRESH,
				WA_Gadgets,	CPPWindowGList,
				WA_Title,	CPPWindowWdt,
				WA_ScreenTitle,	"ScreenTitle",
				WA_PubScreen,	Scr,
				TAG_DONE )))
	return( 4L );

	SetMenuStrip( CPPWindowWnd, CPPWindowMenus );
	GT_RefreshWindow( CPPWindowWnd, NULL );

	return( 0L );
}
