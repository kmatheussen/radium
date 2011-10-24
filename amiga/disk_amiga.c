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










#include "nsmtracker.h"
#include <proto/intuition.h>
#include <proto/gadtools.h>
#include <intuition/intuition.h>
#include "../common/disk.h"
#include "plug-ins/disk_camd_mymidilinks_proc.h"
#include "instrprop/Amiga_instrprop.h"
#include "Amiga_bs.h"
#include "Amiga_bs_edit_proc.h"

#include "../common/OS_disk_proc.h"
#include "disk_amiga_proc.h"

long BS_Width;
long BS_Height;
long BS_Left;
long BS_Top;

static bool loaded=false;

void SaveOsStuff(void){
	DC_SaveCleanString("Amiga\n");

	SaveMyMidiLinks();

	DC_SSI("PatchWindowX",(int)CPPWindowWnd->LeftEdge);
	DC_SSI("PatchWindowY",(int)CPPWindowWnd->TopEdge);

	DC_SSI("HelpWindowX",(int)HelpWnd->LeftEdge);
	DC_SSI("HelpWindowY",(int)HelpWnd->TopEdge);

	if(BlockSelectWnd!=NULL){
		DC_SSL("BSWindowX",(long)BlockSelectWnd->LeftEdge);
		DC_SSL("BSWindowY",(long)BlockSelectWnd->TopEdge);
		DC_SSL("BSWindowWidth",(long)BlockSelectWnd->Width);
		DC_SSL("BSWindowHeight",(long)BlockSelectWnd->Height);
	}else{
		DC_SSL("BSWindowX",(long)10);
		DC_SSL("BSWindowY",(long)10);
		DC_SSL("BSWindowWidth",(long)180);
		DC_SSL("BSWindowHeight",(long)180);
	}
}

void LoadOsStuff(void){
	char *osname;

	char *objs[1]={
		"MYMIDILINKS"
	};
	char *vars[8]={
		"PatchWindowX",
		"PatchWindowY",
		"HelpWindowX",
		"HelpWindowY",
		"BSWindowX",
		"BSWindowY",
		"BSWindowWidth",
		"BSWindowHeight"
	};

	osname=DC_LoadS();
	if(strcmp(osname,"Amiga")){
		return;							//Must be done something somewhere, when porting. This will just fail the loading. (probably crash)
	}

	loaded=true;

	GENERAL_LOAD(1,8)


var0:
	CPPWindowLeft=(short)DC_LoadI();
	goto start;
var1:
	CPPWindowTop=(short)DC_LoadI();
	goto start;
var2:
	HelpLeft=(short)DC_LoadI();
	goto start;
var3:
	HelpTop=(short)DC_LoadI();
	goto start;
var4:
	BlockSelectLeft=0;
	BS_Left=DC_LoadL();
	goto start;
var5:
	BlockSelectTop=0;
	BS_Top=DC_LoadL();
	goto start;
var6:
	BS_Width=DC_LoadL();
	goto start;
var7:
	BS_Height=DC_LoadL();
	goto start;

obj0:
	LoadMyMidiLinks();
	goto start;



var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:

obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	DC_Next();
	return;
}

void Disk_Amiga_ControlInit(void){
	BS_Width=BlockSelectWidth;
	BS_Height=BlockSelectHeight;
	BS_Left=BlockSelectLeft;
	BS_Top=BlockSelectTop;
}

void Disk_Amiga_ResizeBSWindow(void){
//	struct IntuiMessage *msg;
//	long lastx=BS_Width,lasty=BS_Height;

 
	if(loaded && BlockSelectWnd!=NULL){
		ChangeWindowBox(BlockSelectWnd,BS_Left,BS_Top,BS_Width,BS_Height);

/*
		ChangeWindowBox(BlockSelectWnd,0,0,lastx,lasty);
		BS_resizewindow();
		while(msg=GT_GetIMsg(BlockSelectWnd->UserPort)){
			BS_handleevents(msg);
		}
		while(
			BlockSelectWnd->Width>BS_Width
		){
			lastx--;
			ChangeWindowBox(BlockSelectWnd,0,0,lastx,lasty);
			BS_resizewindow();
			while(msg=GT_GetIMsg(BlockSelectWnd->UserPort)){
				BS_handleevents(msg);
			}
		}
		while(
			BlockSelectWnd->Height>BS_Height
		){
			lasty--;
			ChangeWindowBox(BlockSelectWnd,0,0,lastx,lasty);
			BS_resizewindow();
			while(msg=GT_GetIMsg(BlockSelectWnd->UserPort)){
				BS_handleevents(msg);
			}
		}
		while(BlockSelectWnd->TopEdge==0 && BlockSelectWnd->LeftEdge==0){
			MoveWindow(BlockSelectWnd,BS_Left,BS_Top);
//			BS_resizewindow();
			while(msg=GT_GetIMsg(BlockSelectWnd->UserPort)){
				BS_handleevents(msg);
			}
		}
*/
	}
}

