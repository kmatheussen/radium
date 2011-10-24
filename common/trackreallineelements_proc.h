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






extern void FreeAllTREelementHolders(void);

extern void ReturnRTEelement(struct TrackReallineElements *tre);
extern void FreeAllRTEelements_fromroot(
	struct TrackReallineElements **to
);
extern bool FreeASpesifiedWBlockTREelement(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);
extern bool FreeANotShowedWBlockTREelement(void);

extern void FreeAllNotShowedWBlockTREelement(void);

extern bool collectTREgarbage;
extern bool TRE_collectGarbage(void);

extern struct TrackReallineElements *TRE_GetTREelementHard(void);

extern struct TrackReallineElements *nextfreeelement;

#define GetTREelement(a) if(nextfreeelement!=NULL){					\
			     (a)=nextfreeelement;nextfreeelement=nextfreeelement->next;(a)->next=NULL;	\
			  }else (a)=TRE_GetTREelementHard()					
			  								
