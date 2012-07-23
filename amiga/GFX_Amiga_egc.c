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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>

#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <graphics/displayinfo.h>
#include <graphics/gfxbase.h>
#include <graphics/gfxmacros.h>
#include <graphics/text.h>
#include <proto/exec.h>
#include <proto/intuition.h>
#include <proto/gadtools.h>
#include <proto/graphics.h>
#include <proto/utility.h>
#include <proto/dos.h>
#include <proto/diskfont.h>
#include <proto/asl.h>
#include <proto/reqtools.h>
#include <exec/exec.h>

#include "nsmtracker.h"

#include "../common/resizewindow_proc.h"
#include "Amiga_readstr_proc.h"

#define GFX_DONTSHRINK
#include "visual_proc.h"

extern struct Root *root;
extern struct Screen *Scr;
extern struct Screen *mainscreen;

extern APTR                   VisualInfo;

extern struct TextFont *sysfont;


int GFX_SetupScreen(struct Screen **screen)
{
//	*screen=LockPubScreen(NULL);

	*screen=mainscreen;

	if(*screen==NULL) return 1;

	return 0;
}

extern bool useworkbench;

void GFX_CloseDownScreen(struct Screen **screen)
{
	if ( VisualInfo ) {
		FreeVisualInfo( VisualInfo );
		VisualInfo = NULL;
	}

	if(*screen!=NULL){
		if(useworkbench){
			UnlockPubScreen( NULL, *screen );
		}else{
			CloseScreen( *screen );
			*screen=NULL;
		}
	}
}


void SetColor(struct RastPort *rp,int color){
/*
	if(color==1 || color==0){
		SetWrMsk(rp,1)
	}else{
		SetWrMsk(rp,color);
	}
*/
	SetAPen(rp,(ULONG)color);
}

UWORD __chip ResizePointer[50]={
	0x5123,
	0x4854,
	0x3536,
	0x2834,
	0x1164,
	0x0246,
	0x6867
};

int maxcol;

//struct TextAttr fontstruct={"Condensed60.font",9,0,0};
//struct TextAttr fontstruct={"topaz.font",9,0,0};
struct TextAttr fontstruct={"courier.font",13,0,0};
struct TextAttr newfontstruct;
struct TextFont *edittextfont;
struct TextFont *h_textfont;

int GFX_CreateVisual(struct Tracker_Windows *tvisual){
	struct Window *window;
	ULONG height,width;
	struct BitMap *bitmap;
	static int exclude=0;
	int lokke;ULONG depth;

	tvisual->os_visual=talloc(sizeof(struct OS_visual));

	if(tvisual->os_visual==NULL){
		RError("Out of memory at function GFX_CreateVisual in file GFX_Amiga_egc.c\n");
		return 5;
	}

	if(Scr==NULL){
		GFX_SetupScreen(&tvisual->os_visual->screen);
		if(tvisual->os_visual->screen==NULL){
			RError("Can't open screen.\n");
			return 6;
		}
	}else{
		tvisual->os_visual->screen=Scr;
	}

	debug("screen: %x\n",tvisual->os_visual->screen);

	maxcol=1;
	depth=GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_DEPTH);
	for(lokke=0;lokke<depth;lokke++){
		maxcol*=2;
	}

	tvisual->os_visual->xpluss=6;
	tvisual->os_visual->ypluss=mainscreen->Font->ta_YSize+4;

	if(tvisual->height==0 || tvisual->width==0){
		rtGetVScreenSize(mainscreen,&width,&height);
		tvisual->x=0;
		tvisual->y=0;
		tvisual->width=width-DWIDTH -5;
		tvisual->height=height -DHEIGHT -15;
	}

	if ( ! ( window = OpenWindowTags( NULL,
				WA_Left,		tvisual->x,
				WA_Top,		tvisual->y,
				WA_Width,	tvisual->width+tvisual->os_visual->xpluss+DWIDTH,
				WA_Height,	tvisual->height+tvisual->os_visual->ypluss+DHEIGHT,
				WA_IDCMP,	IDCMP_NEWSIZE | IDCMP_MOUSEMOVE | IDCMP_MOUSEBUTTONS | IDCMP_REFRESHWINDOW
								| IDCMP_RAWKEY | IDCMP_CHANGEWINDOW,
				WA_Flags,	WFLG_REPORTMOUSE | WFLG_ACTIVATE | WFLG_DRAGBAR | WFLG_DEPTHGADGET
								| WFLG_SMART_REFRESH | WFLG_RMBTRAP,
				WA_Title,	"",
				WA_PubScreen,	tvisual->os_visual->screen,
				TAG_DONE ))
	){
		RError("Can't open tracker_window\n");
		return(4);
	}

	GT_RefreshWindow(window, NULL );

	tvisual->os_visual->window=window;

	if(tvisual->fontname==NULL){
fontfailed:
		h_textfont=edittextfont=OpenDiskFont(&fontstruct);
		if(edittextfont==NULL){
			RError("Error. Can't open edit-font.\n",&fontstruct);
			CloseWindow(tvisual->os_visual->window);
			GFX_CloseDownScreen(&tvisual->os_visual->screen);
			return 10;
		}else{
			SetFont(window->RPort,edittextfont);
		}
		tvisual->fontheight=window->RPort->Font->tf_YSize+2;
	}else{
		newfontstruct.ta_Name=tvisual->fontname;
		newfontstruct.ta_YSize=tvisual->org_fontheight;
		newfontstruct.ta_Style=0;
		newfontstruct.ta_Flags=0;
		edittextfont=OpenDiskFont(&newfontstruct);
		if(edittextfont==NULL){
			RError("Could not open font %s, with size %d\n",tvisual->fontname,tvisual->org_fontheight);
			goto fontfailed;
		}
		SetFont(window->RPort,edittextfont);
		if(tvisual->fontheight<tvisual->org_fontheight){
			tvisual->fontheight=window->RPort->Font->tf_YSize+2;
		}
	}

	tvisual->fontname=window->RPort->Font->tf_Message.mn_Node.ln_Name;
	tvisual->fontID=window->RPort->Font->tf_YSize;
	tvisual->fontwidth=window->RPort->Font->tf_XSize;
	tvisual->org_fontheight=window->RPort->Font->tf_YSize;

	debug("name: %s, X: %d, Y:%d\n",tvisual->fontname,tvisual->fontwidth,tvisual->fontheight);

	/*
	   Don't want to access chip-mem unnecesarry. (The command stops
      (at least thats what I think) the Garbage collector from spanning
      this areas.)
   */

/*
	debug(
		"exl, S: %d, 1: %x, 2: %x\n",
		sizeof(struct OS_visual *),
		&tvisual->os_visual,
		&tvisual->os_visual+(sizeof(struct OS_visual *))
	);
*/

	if(exclude==0){
//		debug("exl, 1: %x, 2: %x\n",ResizePointer,ResizePointer+50);
		GC_exclude_static_roots(ResizePointer,ResizePointer+50);
		exclude=1;
	}
	debug("screen: %x\n",tvisual->os_visual->screen);


	rtGetVScreenSize(mainscreen,&width,&height);

	/* The main pixmap. */
	bitmap=AllocBitMap(
//		(ULONG)width+100,(ULONG)height+100,
		GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_WIDTH),
		GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_HEIGHT),
//		(ULONG)tvisual->width+tvisual->os_visual->xpluss+DWIDTH+100,
//		(ULONG)tvisual->height+tvisual->os_visual->ypluss+DHEIGHT+100,
		depth,
		BMF_CLEAR,
//		NULL
		window->RPort->BitMap
	);

	if(bitmap==NULL){
		fprintf(stderr,"Not enough memory for bitmap\n");
		if(edittextfont!=NULL)CloseFont(edittextfont);
		CloseWindow(tvisual->os_visual->window);
		return 20;
	}

	if((void *)bitmap->Planes[0]<(void *)(SysBase->MaxLocMem)){
		fprintf(stderr,"\n\n************************************************\n");
		fprintf(stderr," WARNING! It seems like you don`t use a GFX card, and that\n");
		fprintf(stderr," you have not installed the FBlit patch.\n");
		fprintf(stderr," It is strongly reccomended that you do so, you will then gain\n");
		fprintf(stderr," a 50-100 percent speed increase on the graphics operations, and\n");
		fprintf(stderr," will have much less graphical flickering. A link to the FBlint\n");
		fprintf(stderr," homepage can be foundn here: http://www.notam.uio.no/~kjetism/radium/\n");
		fprintf(stderr,"************************************************\n\n\n");
	}

	tvisual->os_visual->RPort = talloc(sizeof(struct RastPort));

	InitRastPort(tvisual->os_visual->RPort);

	tvisual->os_visual->RPort->BitMap=bitmap;

	SetFont(tvisual->os_visual->RPort,edittextfont);


	/* The cursor pixmap. */
	bitmap=AllocBitMap(
//		(ULONG)width+100,(ULONG)height+100,
		GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_WIDTH),
		GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_HEIGHT),
//		(ULONG)tvisual->width+tvisual->os_visual->xpluss+DWIDTH+100,
//		(ULONG)tvisual->height+tvisual->os_visual->ypluss+DHEIGHT+100,
		depth,
		BMF_CLEAR,
		window->RPort->BitMap
//		NULL
	);

	if(bitmap==NULL){
		fprintf(stderr,"Not enough memory for cursor bitmap.\n");
		if(edittextfont!=NULL)CloseFont(edittextfont);
		WaitBlit();
		FreeBitMap(tvisual->os_visual->RPort->BitMap);
		CloseWindow(tvisual->os_visual->window);
		return 20;
	}

	tvisual->os_visual->CRPort = talloc(sizeof(struct RastPort));

	InitRastPort(tvisual->os_visual->CRPort);

	tvisual->os_visual->CRPort->BitMap=bitmap;

	tvisual->width=window->Width-tvisual->os_visual->xpluss-DWIDTH;
	tvisual->height=window->Height-tvisual->os_visual->ypluss-DHEIGHT;

	return 0;
}



int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){
	int ret=0;
	if(tvisual->os_visual==NULL) return 2;

	CloseFont(edittextfont);

	WaitBlit();
	FreeBitMap(tvisual->os_visual->RPort->BitMap);

	WaitBlit();
	FreeBitMap(tvisual->os_visual->CRPort->BitMap);

	if (tvisual->os_visual->window) {
		CloseWindow(tvisual->os_visual->window);
	}else{
		ret=1;
	}

	tvisual->os_visual=NULL;

	return ret;
}

extern struct Window *BlockSelectWnd;
extern struct Window         *CPPWindowWnd;
extern struct Window         *HelpWnd;

void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
	WindowToFront(tvisual->os_visual->window);
	ActivateWindow(tvisual->os_visual->window);
}
void GFX_PlayListWindowToFront(void){
	WindowToFront(BlockSelectWnd);
	ActivateWindow(BlockSelectWnd);
}
void GFX_InstrumentWindowToFront(void){
	WindowToFront(CPPWindowWnd);
	ActivateWindow(CPPWindowWnd);
}
void GFX_HelpWindowToFront(void){
	WindowToFront(HelpWnd);
	ActivateWindow(HelpWnd);
}

void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual){
	ULONG width,height;
	rtGetVScreenSize(mainscreen,&width,&height);
	WindowResize(tvisual,(int)width,(int)height);
}
void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual){
	WindowResize(tvisual,1,1);
}

bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){
	struct Window *window=tvisual->os_visual->window;
	struct FontRequester *requester;
	struct TextFont *textfont;

	requester=AllocAslRequestTags(
		ASL_FontRequest,
		ASLFO_TitleText,"Editor-font",
		ASLFO_Screen,tvisual->os_visual->screen,
		ASLFO_FixedWidthOnly, TRUE,
		ASLFO_MinHeight, 4,
		TAG_END
	);

	if(requester==NULL){
		RError("Could not open ASL-requester\n");
		return false;
	}

	if(AslRequestTags(requester,0L)==NULL){
		FreeAslRequest(requester);
		return false;
	}

	textfont=OpenDiskFont(&requester->fo_Attr);
	if(textfont==NULL){
		RError("Error. Could not open font.\n");
		return false;
	}

	CloseFont(edittextfont);
	edittextfont=textfont;

	FreeAslRequest(requester);

	SetFont(window->RPort,edittextfont);
	SetFont(tvisual->os_visual->RPort,edittextfont);
	tvisual->h_fontname=tvisual->fontname=window->RPort->Font->tf_Message.mn_Node.ln_Name;
	tvisual->h_fontID=tvisual->fontID=window->RPort->Font->tf_YSize;
	tvisual->h_fontwidth=tvisual->fontwidth=window->RPort->Font->tf_XSize;
	tvisual->org_fontheight=window->RPort->Font->tf_YSize;
	tvisual->fontheight=tvisual->org_fontheight+2;
	return true;

}



bool GFX_SelectHeaderFont(struct Tracker_Windows *tvisual){
	struct Window *window=tvisual->os_visual->window;
	struct FontRequester *requester;
	struct TextFont *textfont;

	requester=AllocAslRequestTags(
		ASL_FontRequest,
		ASLFO_TitleText,"Editor-font",
		ASLFO_Screen,tvisual->os_visual->screen,
		ASLFO_FixedWidthOnly, TRUE,
		ASLFO_MinHeight, 4,
		TAG_END
	);

	if(AslRequestTags(requester,0L)==NULL){
		RError("Could not open asl-requester\n");
		FreeAslRequest(requester);
		return false;
	}

	textfont=OpenDiskFont(&requester->fo_Attr);
	if(textfont==NULL){
		RError("Error. Could not open font.\n");
		return false;
	}

	CloseFont(edittextfont);
	h_textfont=textfont;

	FreeAslRequest(requester);

	tvisual->h_fontname=tvisual->fontname=window->RPort->Font->tf_Message.mn_Node.ln_Name;
	tvisual->h_fontID=tvisual->fontID=window->RPort->Font->tf_YSize;
	tvisual->h_fontwidth=tvisual->fontwidth=window->RPort->Font->tf_XSize;
	tvisual->org_fontheight=tvisual->fontheight=window->RPort->Font->tf_YSize;

	return true;

}


void GFX_C2V_bitBlt(
				    struct Tracker_Windows *window,
				    int from_x1,int from_x2,
				    int to_y
				    )
{

	from_x1+=window->os_visual->xpluss;
	from_x2+=window->os_visual->xpluss;
	to_y+=window->os_visual->ypluss;

	ClipBlit(
		window->os_visual->CRPort,
		(LONG)from_x1,0L,
		window->os_visual->window->RPort,
		(LONG)from_x1,(LONG)to_y,
		(LONG)from_x2-from_x1+1,(LONG)window->fontheight,
		0xc0
	);

}
void GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      )
{
//	static int g=0;

	x1+=window->os_visual->xpluss;
	x2+=window->os_visual->xpluss;
	x3+=window->os_visual->xpluss;
	x4+=window->os_visual->xpluss;
	y_pixmap+=window->os_visual->ypluss;

	SetWrMsk(window->os_visual->CRPort,maxcol-1);

//	SetDrMd(window->os_visual->CRPort,JAM2);


//	SetColor(window->os_visual->CRPort,0);
//	RectFill(window->os_visual->CRPort,(LONG)x1,0L,(LONG)x4,(LONG)height+1);


	BltBitMap(
		window->os_visual->RPort->BitMap,
		(LONG)x1,(LONG)y_pixmap,
		window->os_visual->CRPort->BitMap,
		(LONG)x1,0L,
		(LONG)x4-x1+1,(LONG)height,
//		(LONG)0xe7,0xff,NULL
		(LONG)0xc0,0xff,NULL
	);


	SetDrMd(window->os_visual->CRPort,COMPLEMENT);
	SetWrMsk(window->os_visual->CRPort,3);

	SetColor(window->os_visual->CRPort,2);
	RectFill(window->os_visual->CRPort,(LONG)x1,0L,(LONG)x2,(LONG)height-1);
	SetColor(window->os_visual->CRPort,2);
	RectFill(window->os_visual->CRPort,(LONG)x3,0L,(LONG)x4,(LONG)height-1);


	SetDrMd(window->os_visual->CRPort,JAM2);

	SetColor(window->os_visual->CRPort,1);

	Move(window->os_visual->CRPort,(long)x1,0L);
	Draw(window->os_visual->CRPort,(long)x2,0L);

	Move(window->os_visual->CRPort,(long)x3,0L);
	Draw(window->os_visual->CRPort,(long)x4,0L);

	Move(window->os_visual->CRPort,(long)x1,(LONG)height-1);
	Draw(window->os_visual->CRPort,(long)x2,(LONG)height-1);

	Move(window->os_visual->CRPort,(long)x3,(LONG)height-1);
	Draw(window->os_visual->CRPort,(long)x4,(LONG)height-1);



	SetColor(window->os_visual->CRPort,2);

	Move(window->os_visual->CRPort,(long)x1,1L);
	Draw(window->os_visual->CRPort,(long)x2,1L);

	Move(window->os_visual->CRPort,(long)x3,1L);
	Draw(window->os_visual->CRPort,(long)x4,1L);

	Move(window->os_visual->CRPort,(long)x1,(LONG)height-2);
	Draw(window->os_visual->CRPort,(long)x2,(LONG)height-2);

	Move(window->os_visual->CRPort,(long)x3,(LONG)height-2);
	Draw(window->os_visual->CRPort,(long)x4,(LONG)height-2);


}


void GFX_P2V_bitBlt(
				struct Tracker_Windows *window,
				int from_x,int from_y,
				int to_x,int to_y,
				int width,int height
				)
{
	from_x+=window->os_visual->xpluss;
	to_x+=window->os_visual->xpluss;
	from_y+=window->os_visual->ypluss;
	to_y+=window->os_visual->ypluss;

//	SetWrMsk(window->os_visual->RPort,1)
//	SetWrMsk(window->os_visual->window->RPort,1)

	ClipBlit(
		window->os_visual->RPort,
		(LONG)from_x,(LONG)from_y,
		window->os_visual->window->RPort,
		(LONG)to_x,(LONG)to_y,
		(LONG)width,(LONG)height,
		0xc0
	);
}

void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetColor(tvisual->os_visual->RPort,color>=maxcol?maxcol==4?color%(maxcol-1)+1:color%(maxcol-5)+4:color);
	RectFill(tvisual->os_visual->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
}

void GFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	WORD box[8];
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	box[0]=x2;
	box[1]=y;
	box[2]=x2;
	box[3]=y2;
	box[4]=x;
	box[5]=y2;
	box[6]=x;
	box[7]=y;
	Move(tvisual->os_visual->RPort,(long)x,(long)y);
	SetColor(tvisual->os_visual->RPort,color>=maxcol?maxcol==4?color%(maxcol-1)+1:color%(maxcol-5)+4:color);
	PolyDraw(tvisual->os_visual->RPort,4,box);
}

void GFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetColor(tvisual->os_visual->RPort,color>=maxcol?maxcol==4?color%(maxcol-1)+1:color%(maxcol-5)+4:color);
	Move(tvisual->os_visual->RPort,(long)x,(long)y);
	Draw(tvisual->os_visual->RPort,(long)x2,(long)y2);
}

void GFX_P_Point(struct Tracker_Windows *tvisual,int color,int x,int y){
	x+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	SetColor(tvisual->os_visual->RPort,color);
	WritePixel(tvisual->os_visual->RPort,(long)x,(long)y);
}

void GFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){

	x+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;

	SetColor(tvisual->os_visual->RPort,color);

	Move(
		tvisual->os_visual->RPort,
		(LONG)x,
		(LONG)y+edittextfont->tf_Baseline +1
	);

	Text(tvisual->os_visual->RPort,text,(LONG)strlen(text));

}


void GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetColor(tvisual->os_visual->window->RPort,color);
	Move(tvisual->os_visual->window->RPort,(long)x,(long)y);
	Draw(tvisual->os_visual->window->RPort,(long)x2,(long)y2);
}

void GFX_All_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetAPen(tvisual->os_visual->window->RPort,(ULONG)color);
	Move(tvisual->os_visual->window->RPort,(long)x,(long)y);
	Draw(tvisual->os_visual->window->RPort,(long)x2,(long)y2);
}

void GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	WORD box[8];
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	box[0]=x2;
	box[1]=y;
	box[2]=x2;
	box[3]=y2;
	box[4]=x;
	box[5]=y2;
	box[6]=x;
	box[7]=y;
	Move(tvisual->os_visual->window->RPort,(long)x,(long)y);
	SetColor(tvisual->os_visual->window->RPort,color);
	PolyDraw(tvisual->os_visual->window->RPort,4,box);
}

void GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetColor(tvisual->os_visual->window->RPort,color);
	RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
}

void GFX_Slider_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetAPen(tvisual->os_visual->window->RPort,(ULONG)color);
	RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
}

void GFX_All_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	SetAPen(tvisual->os_visual->window->RPort,(ULONG)color);
	RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
}

void GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
	x+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;

	SetColor(tvisual->os_visual->window->RPort,color);
//	SetBPen(tvisual->os_visual->window->RPort,5);

	Move(
		tvisual->os_visual->window->RPort,
		(LONG)x,
		(LONG)y+edittextfont->tf_Baseline +1
	);

	Text(tvisual->os_visual->window->RPort,text,(LONG)strlen(text));
/*
	if(tvisual->textborder){
		x-=tvisual->os_visual->xpluss;
		y-=tvisual->os_visual->ypluss;

		GFX_Box(
			tvisual,1,
			x,
			y,
			x+(strlen(text)*tvisual->fontwidth),
			y+tvisual->fontheight
		);
	}
*/
}


void GFX_P_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
	GFX_P_Line(tvisual,2,x,y,x,y2);
	GFX_P_Line(tvisual,1,x+1,y,x+1,y2);
}

void GFX_P_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
	GFX_P_Line(tvisual,2,x,y,x,y2);
}

void GFX_V_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}

void GFX_V_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
	SetWindowTitles(tvisual->os_visual->window,title,(UBYTE *)-1);
}



void GFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	ClipBlit(
		tvisual->os_visual->window->RPort,
		(long)x,(long)y,
		tvisual->os_visual->window->RPort,
		(long)(x+dx),(long)(y+dy),
		(long)(x2-x+1),(long)(y2-y+1),
		0xc0
	);
}
void GFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
	x+=tvisual->os_visual->xpluss;
	x2+=tvisual->os_visual->xpluss;
	y+=tvisual->os_visual->ypluss;
	y2+=tvisual->os_visual->ypluss;
	ClipBlit(
		tvisual->os_visual->RPort,
		(long)x,(long)y,
		tvisual->os_visual->RPort,
		(long)(x+dx),(long)(y+dy),
		(long)(x2-x+1),(long)(y2-y+1),
		0xc0
	);
}

int GFX_IDCMP_NEWSIZE(struct Tracker_Windows *window){
	if(window->os_visual->dowaitfor_IDCMP_NEWSIZE==1){
		window->os_visual->dowaitfor_IDCMP_NEWSIZE=0;
		return FinishedResizing(window);
	}
	return 0;
}

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){

	debug("gfx_resize\n");
	SizeWindow(
		tvisual->os_visual->window,
		(long)( x - tvisual->width),
		(long)( y - tvisual->height)
	);

	tvisual->width=x-2;
	tvisual->height=y;

	tvisual->os_visual->dowaitfor_IDCMP_NEWSIZE=1;

	return 0;
}

void SetNormalPointer(struct Tracker_Windows *tvisual){
	ClearPointer(tvisual->os_visual->window);
}

UWORD testing[5]={34,43};


void SetResizePointer(struct Tracker_Windows *tvisual){

	SetPointer(
		tvisual->os_visual->window,
		ResizePointer,
		10,10,
		0,0
	);
}

extern char *screenname;

ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){

	ReqType file;

	file=OpenCon(
		tvisual->os_visual->window->IFont->tf_XSize*min(60,(width+3)),
		tvisual->os_visual->window->IFont->tf_YSize*min(30,(height+3)),
		title,
		screenname
	);

	return file;
}

void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype){
	if(reqtype==NULL) return;
	Close(reqtype);
}

int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,int min,int max){
	ReqType file;
	char rettext[50];
	int ret=min-1;

	if(reqtype==NULL){
		file=GFX_OpenReq(tvisual,strlen(text)+10,4,"title");
	}else{
		file=reqtype;
	}

	if(file==0){
		return ret;
	}

	while(ret<min || ret>max){
		writestring(file,text);
		readstring(file,rettext,40);
		if(rettext[0]=='\0'){
			ret=min-1;
			break;
		}
		ret=atoi(rettext);
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return ret;
}

float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,float min,float max){
	ReqType file;
	char rettext[50];
	float ret=min-1.0f;

	if(reqtype==NULL){
		file=GFX_OpenReq(tvisual,strlen(text)+10,4,"title");
	}else{
		file=reqtype;
	}

	if(file==0){
		return ret;
	}

	while(ret<min || ret>max){
		writestring(file,text);
		readstring(file,rettext,40);
		if(rettext[0]=='\0'){
			ret=min-1.0f;
			break;
		}
		if(rettext[0]=='/'){
			ret=(float)atof(rettext+1);
			if(ret!=0.0f){
				ret=(float)1.0f/ret;
			}else{
				ret=min-1.0f;
			}
		}else{
			ret=(float)atof(rettext);
		}
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return ret;
}


char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,char *text){
	ReqType file;
	char temp[70];
	char *rettext=NULL;

	if(reqtype==NULL){
		file=GFX_OpenReq(tvisual,strlen(text)+10,4,"title");
	}else{
		file=reqtype;
	}

	if(file==0){
		return NULL;
	}

	writestring(file,text);
	readstring(file,temp,49);
	if(temp[0]=='\0'){
	}else{
		rettext=talloc_atomic(strlen(temp)+2);
		sprintf(rettext,"%s",temp);
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return rettext;
}

int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
){
#ifdef USEPROGRESSBAR
	BPTR file;
#endif
	BPTR tradfile;
	BPTR lock;
	char temp[500];
	int lokke,ret=0;
	struct FileRequester *requester;

	if(reqtype==NULL || num_sel>20){
#ifdef USEPROGRESSBAR
		file=OpenCon(tvisual->fontwidth*(30),tvisual->fontheight,"Please Wait...",screenname);

		if(file==0){
			return ret;
		}

		writestring(file,".");

		writestring(file,".");
#endif
		debug("hallo1\n");

		lock=Lock("rad:trackertemp",ACCESS_WRITE);

		for(lokke=0;lokke<num_sel;lokke++){
//			sprintf(temp,"echo >\"rad:trackertemp/%s\"",menutext[lokke]);
//			system(temp);
			sprintf(temp,"rad:trackertemp/%.2d. %s",lokke+1,menutext[lokke]);
			tradfile=Open(temp,0x3ee);
			if(tradfile==0){
				RError("Error. Could not open Con. ReturnCode from IoErr(): %d\n",IoErr());
				RError("(Check out include:dos/dos.h for explanation\n");
			}else{
				Close(tradfile);
			}
#ifdef USEPROGRESSBAR
			writestring(file,".");
#endif
		}

		UnLock(lock);

		debug("hallo3\n");

#ifdef USEPROGRESSBAR
		Close(file);
#endif

//		Delay(1);		// Sometimes necesarry. Don't know why.

		requester=AllocAslRequestTags(
			ASL_FileRequest,
			ASLFR_TitleText,seltext,
			ASLFR_InitialDrawer,"rad:trackertemp",
			ASLFR_Screen,Scr,
			TAG_END
		);

		if(requester==NULL){
			RError("Could not open asl-requester, 1\n");
			Execute("delete rad:trackertemp/#? >NIL:",NULL,NULL);
			return -1;
		}

		debug("hallo4\n");

		if(AslRequestTags(requester,0L)==NULL){
			FreeAslRequest(requester);
			Execute("delete rad:trackertemp/#? >NIL:",NULL,NULL);
			return -1;
		}

		debug("hallo5\n");

		if(requester->fr_File==NULL){
			FreeAslRequest(requester);
			Execute("delete rad:trackertemp/#? >NIL:",NULL,NULL);
			return -1;
		}

		for(lokke=0;;){
			if(!strcmp(requester->fr_File+4,menutext[lokke])) break;
			lokke++;
			if(lokke==num_sel){
				FreeAslRequest(requester);
				Execute("delete rad:trackertemp/#? >NIL:",NULL,NULL);
				return -1;
			}
		}

		FreeAslRequest(requester);

		Execute("delete rad:trackertemp/#? >NIL:",NULL,NULL);

		return lokke;

	}else{

		writestring(reqtype,seltext);
		writestring(reqtype,"\n");

		for(lokke=0;lokke<num_sel;lokke++){
			sprintf(temp,"%d. %s\n",lokke+1,menutext[lokke]);
			writestring(reqtype,temp);
		}
		writestring(reqtype,"\n");

		while(ret<=0 || ret>num_sel){
			writestring(reqtype,"> ");
			readstring(reqtype,temp,3);
			if(temp[0]=='\0'){
				ret=0;
				break;
			}
			ret=atoi(temp);
		}

		return ret-1;

	}

}


char *filedrawer;

char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
	char *drawer;
	char *name;
	struct FileRequester *requester;

	requester=AllocAslRequestTags(
		ASL_FileRequest,
		ASLFR_TitleText,seltext,
		ASLFR_InitialDrawer,filedrawer,
		ASLFR_Screen,Scr,
		ASLFR_InitialPattern,"#?.(rad|mmd|mmd2|mmd3)",
		ASLFR_DoPatterns,TRUE,
		TAG_END
	);

	if(requester==NULL){
			RError("Could not open asl-requester, 1\n");
		return NULL;
	}

	if(AslRequestTags(requester,0L)==NULL){
		FreeAslRequest(requester);
		return NULL;
	}

	if(requester->fr_File==NULL || strlen(requester->fr_File)==0){
		FreeAslRequest(requester);
		return NULL;
	}

	drawer=requester->fr_Drawer;

	if(strlen(requester->fr_Drawer)>0){
		name=talloc_atomic(strlen(requester->fr_File)+1+strlen(drawer)+1);
		sprintf(
			name,
			"%s%s%s",
			requester->fr_Drawer,
			requester->fr_Drawer[strlen(drawer)-1]==':'?"":"/",
			requester->fr_File
		);
	}else{
		name=talloc_atomic(strlen(requester->fr_File)+1);
		sprintf(name,"%s",requester->fr_File);
	}
	filedrawer=talloc_atomic(strlen(drawer)+1);
	sprintf(filedrawer,"%s",drawer);

	FreeAslRequest(requester);

	return name;
}

char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
	char *drawer;
	char *name;
	struct FileRequester *requester;

	requester=AllocAslRequestTags(
		ASL_FileRequest,
		ASLFR_TitleText,seltext,
		ASLFR_InitialDrawer,filedrawer,
		ASLFR_InitialPattern,"#?.(rad)",
		ASLFR_Screen,Scr,
		TAG_END
	);

	if(requester==NULL){
			RError("Could not open asl-requester, 1\n");
		return NULL;
	}

	if(AslRequestTags(requester,0L)==NULL){
		FreeAslRequest(requester);
		return NULL;
	}

	if(requester->fr_File==NULL || strlen(requester->fr_File)==0){
		FreeAslRequest(requester);
		return NULL;
	}

	drawer=requester->fr_Drawer;

	if(strlen(requester->fr_Drawer)>0){
		name=talloc_atomic(strlen(requester->fr_File)+1+strlen(drawer)+1);
		sprintf(
			name,
			"%s%s%s",
			requester->fr_Drawer,
			requester->fr_Drawer[strlen(drawer)-1]==':'?"":"/",
			requester->fr_File
		);
	}else{
		name=talloc_atomic(strlen(requester->fr_File)+1);
		sprintf(name,"%s",requester->fr_File);
	}
	filedrawer=talloc_atomic(strlen(drawer)+1);
	sprintf(filedrawer,"%s",drawer);

	FreeAslRequest(requester);

	return name;
}


















