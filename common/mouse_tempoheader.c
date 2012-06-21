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
#include "visual_proc.h"
#include "gfx_tempotrackheader_proc.h"
#include "temponodes_proc.h"
#include "gfx_wblocks_proc.h"
#include "time_proc.h"
#include "undo_reltempomax_proc.h"
#include "undo_maintempos_proc.h"
#include "gfx_window_title_proc.h"
#include "player_proc.h"
#include "mouse_tempoheader_proc.h"
#include "gfx_tempocolor_proc.h"

extern struct Root *root;

void SetMouseActionMainLPB(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

	int newlpb=GFX_GetInteger(window,NULL,"New LPB: >",0,99);
	if(newlpb==-1) return;

	Undo_MainTempo(window,wblock);

	root->lpb=newlpb;

	UpdateTempoTrackHeader_LPB(
		window,
		wblock,
		2
	);

	UpdateAllSTimes();

	WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}

void SetMouseActionMainBPM(
                           struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;

	int newtempo=GFX_GetInteger(window,NULL,"New tempo: >",0,999);
	if(newtempo==-1) return;

	Undo_MainTempo(window,wblock);

	root->tempo=newtempo;

	UpdateTempoTrackHeader_BPM(
		window,
		wblock,
		2
	);

	UpdateAllSTimes();

	WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);

	GFX_DrawWindowTitle(window,wblock);
}

void SetMouseActionRelTempo(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	float max=FindHighestTempoNodeVal(wblock->block)+1.0f;
	char temp[500];
	float ret;

	max=R_MAX(1.1f,max);

	sprintf(temp,"New RelTempoMax (%.1f - 99.9) :",max);

	ret=GFX_GetFloat(window,NULL,temp,max,99.9f);

	if(ret<max){
		return;
	}

	Undo_RelTempoMax(window,wblock);

	wblock->reltempomax=ret;

	UpdateWTempoNodes(window,wblock);

	UpdateTempoTrackHeader_reltempo(
		window,
		wblock,
		2
	);

	DrawUpWTempoNodes(window,wblock);

}

void SetMouseActionTempoHeader(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	int x,int y,
	int click
){
	struct WBlocks *wblock=window->wblock;

	if(y<window->org_fontheight){
//		action->action=NOACTION;
		return;
	}

	if(x>wblock->temponodearea.x){
//		action->action=RELTEMPO;
		if(click==0) return;
		PlayStop();
		SetMouseActionRelTempo(window);
		return;
	}

	if(x>wblock->tempoTypearea.x){
//		action->action=MAINBPM;
		if(click==0) return;
		PlayStop();
		SetMouseActionMainBPM(window);
		return;
	}

//	action->action=MAINLPB;
	if(click==0) return;
	PlayStop();
	SetMouseActionMainLPB(window);
}


