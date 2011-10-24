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

#include <string.h>
#include "nsmtracker.h"

#include <proto/gadtools.h>
#include "Amiga_bs.h"
#include "../common/blocklist_proc.h"
#include "../common/list_proc.h"
#include "../common/player_proc.h"
#include "../common/gfx_window_title_proc.h"

#include "Amiga_bs_edit_proc.h"


extern struct Root *root;

struct Node *fromroot;
struct Node *toroot;

char *BS_makelistname(struct Blocks *block){
	char *listname=talloc_atomic(strlen("   - ")+strlen(block->name)+10);
	sprintf(listname,"%.2d - %s",block->l.num,block->name);
	return listname;
}

char *BS_maketolistname(int num,struct Blocks *block){
	char *listname=talloc_atomic(strlen("%.2d - %.2d - %s")+strlen(block->name)+10);
	sprintf(listname,"%.2d - %.2d - %s",num,block->l.num,block->name);
	return listname;
}

void BS_UpdateBlockList(void){
	int lokke;
	int num_blocks=root->song->num_blocks;
	struct Node *LabelFromNodes;
	struct Node *temp;
	struct Blocks *block=root->song->blocks;

	if(BlockSelectWnd==NULL) return;

	fromroot=LabelFromNodes=talloc(sizeof(struct Node));
	LabelFromNodes->ln_Pred=(struct Node *)&BS_to0List.mlh_Head;
	LabelFromNodes->ln_Name=BS_makelistname(block);;
	block=NextBlock(block);

	debug("bs1, num_blocks: %d\n",num_blocks);

	for(lokke=1;lokke<num_blocks;lokke++){
		debug("bsfor%d\n",lokke);
		temp=talloc(sizeof(struct Node));
		temp->ln_Pred=LabelFromNodes;
		LabelFromNodes->ln_Succ=temp;
		temp->ln_Name=BS_makelistname(block);

		LabelFromNodes=temp;

		block=NextBlock(block);
	}
	LabelFromNodes->ln_Succ=(struct Node *)&BS_to0List.mlh_Tail;

	BS_from0List.mlh_Head=(struct MinNode *)fromroot;
	GT_SetGadgetAttrs(BlockSelectGadgets[0],BlockSelectWnd,NULL,
		(GTLV_Labels), (ULONG)&BS_from0List,
		(GTLV_ShowSelected), NULL,
		(GTLV_Selected), root->song->tracker_windows->wblock->l.num,
		(TAG_DONE)
	);
}

void BS_UpdatePlayList(void){
	int lokke;
	struct Node *LabeltoNodes;
	struct Node *temp;
	struct Blocks *block=BL_GetBlockFromPos(0);

	if(BlockSelectWnd==NULL) return;

	toroot=LabeltoNodes=talloc(sizeof(struct Node));
	LabeltoNodes->ln_Pred=(struct Node *)&BS_to0List.mlh_Head;
	LabeltoNodes->ln_Name=BS_maketolistname(0,block);;

	lokke=1;
	block=BL_GetBlockFromPos(lokke);

//	debug("bs1, num_blocks: %d, block->l.num: %d\n",num_blocks,block->l.num);

	while(block!=NULL){
		debug("bsfor%d\n",lokke);
		temp=talloc(sizeof(struct Node));
		temp->ln_Pred=LabeltoNodes;
		LabeltoNodes->ln_Succ=temp;
		temp->ln_Name=BS_maketolistname(lokke,block);

		LabeltoNodes=temp;

		lokke++;
		block=BL_GetBlockFromPos(lokke);
	}

	temp=talloc(sizeof(struct Node));
	temp->ln_Pred=LabeltoNodes;
	LabeltoNodes->ln_Succ=temp;
	temp->ln_Name=" ";

	LabeltoNodes=temp;

	debug("bs2\n");
	LabeltoNodes->ln_Succ=(struct Node *)&BS_to0List.mlh_Tail;

	BS_to0List.mlh_Head=(struct MinNode *)toroot;
	GT_SetGadgetAttrs(BlockSelectGadgets[1],BlockSelectWnd,NULL,
		(GTLV_Labels), (ULONG)&BS_to0List,
		(GTLV_ShowSelected), NULL,
		(GTLV_Selected), root->curr_playlist,
		(TAG_DONE)
	);
	GFX_DrawWindowTitle(root->song->tracker_windows,root->song->tracker_windows->wblock);
}

struct IntuiMessage mymsg;

bool BS_resizewindow(void){
	int width,height;

	if(BlockSelectWnd==NULL) return true;

	width=BlockSelectWnd->Width-BlockSelectWnd->BorderRight-BlockSelectWnd->BorderLeft;
	height=BlockSelectWnd->Height-BlockSelectWnd->BorderBottom;

	BlockSelectLeft=BlockSelectWnd->LeftEdge;
	BlockSelectTop=BlockSelectWnd->TopEdge;
	BlockSelectWidth=BlockSelectWnd->Width;
	BlockSelectHeight=BlockSelectWnd->Height;

	BlockSelectNGad[0].ng_Width=(width-BlockSelectNGad[2].ng_Width)/2;
	BlockSelectNGad[0].ng_Height=height;
	BlockSelectNGad[1].ng_Width=BlockSelectNGad[0].ng_Width;
	BlockSelectNGad[1].ng_Height=BlockSelectNGad[0].ng_Height;

	BlockSelectNGad[2].ng_LeftEdge=BlockSelectNGad[0].ng_LeftEdge+BlockSelectNGad[0].ng_Width;
	BlockSelectNGad[3].ng_LeftEdge=BlockSelectNGad[0].ng_LeftEdge+BlockSelectNGad[0].ng_Width;

	BlockSelectNGad[2].ng_TopEdge=0;
	BlockSelectNGad[2].ng_Height=height/2-2;
	BlockSelectNGad[3].ng_TopEdge=height/2;
	BlockSelectNGad[3].ng_Height=BlockSelectNGad[2].ng_Height;

	BlockSelectNGad[1].ng_LeftEdge=BlockSelectNGad[2].ng_LeftEdge+BlockSelectNGad[2].ng_Width;

	CloseBlockSelectWindow();
	if(OpenBlockSelectWindow()!=0){
		RError("Could not resize Block Selector window.\n");
		RError("Probably out of memory. You should really quit Now!\n");
		return false;
	}

	BS_UpdateBlockList();
	BS_UpdatePlayList();

	return true;
}


NInt selfrom=0;
int selto=0;

#include "../common/playerclass.h"
extern PlayerClass *pc;

bool BS_handleevents(struct IntuiMessage *msg){
	UWORD gadID;
	struct Gadget *gad;

	if(BlockSelectWnd==NULL) return true;

	memcpy(&mymsg,msg,sizeof( struct IntuiMessage ));
	GT_ReplyIMsg(msg);

	switch(mymsg.Class){
		case IDCMP_REFRESHWINDOW:
			GT_BeginRefresh(BlockSelectWnd);
			GT_EndRefresh(BlockSelectWnd, TRUE );
			break;
		case IDCMP_GADGETUP:
			gad=(struct Gadget *)(mymsg.IAddress);
			gadID=gad->GadgetID;
			debug("%d,%d,%d\n",mymsg.Code,mymsg.Class,gadID);
			switch(gadID){
				case 0:
					selfrom=mymsg.Code;
					break;
				case 1:
					if(! pc->isplaying){
						PlayStop();
						root->curr_playlist=selto=mymsg.Code;
						GFX_DrawWindowTitle(root->song->tracker_windows,root->song->tracker_windows->wblock);
					}else{
						selto=mymsg.Code;
					}
					break;
				case 2:
					PlayStop();
					BL_insert(selto,(struct Blocks *)ListFindElement1(&root->song->blocks->l,selfrom));
					selto++;
					root->curr_playlist=selto;
					BS_UpdatePlayList();
					GT_SetGadgetAttrs(
						BlockSelectGadgets[1],BlockSelectWnd,NULL,
						(GTLV_Selected), selto,
						(TAG_DONE)
					);
					break;
				case 3:
					if(selto<root->song->length && root->song->length>1){
						PlayStop();
						BL_delete(selto);
						BS_UpdatePlayList();
					}
					break;
			}
			break;
		case IDCMP_NEWSIZE:
			if(BS_resizewindow()==false) return false;
			break;
	}
	return true;
}


void BS_SelectBlock(struct Blocks *block){

	if(BlockSelectWnd==NULL) return;

	GT_SetGadgetAttrs(
		BlockSelectGadgets[0],BlockSelectWnd,NULL,
		(GTLV_Selected), block->l.num,
		(TAG_DONE)
	);
	selfrom=block->l.num;
}

void BS_SelectPlaylistPos(int pos){

	if(BlockSelectWnd==NULL) return;

	GT_SetGadgetAttrs(
		BlockSelectGadgets[1],BlockSelectWnd,NULL,
		(GTLV_Selected), pos,
		(TAG_DONE)
	);
	selto=pos;
}

