#include "nsmtracker.h"
#include "mouse.h"
#include "undo_reltemposlider_proc.h"
#include "gfx_wblocks_reltempo_proc.h"
#include "gfx_window_title_proc.h"
#include "gfx_tempocolor_proc.h"
#include "playerclass.h"

#include "mouse_reltemposlider_proc.h"

extern PlayerClass *pc;


int MoveRelTempoSlider_Mouse(
	struct Tracker_Windows *window,
	int x,int y
){
	struct WBlocks *wblock=window->wblock;

	if(wblock!=(struct WBlocks *)window->prevaction.pointer1) return 0;

	wblock->block->reltempo=R_BOUNDARIES(
		MINBLOCKRELTIME,
		MINBLOCKRELTIME+(((MAXBLOCKRELTIME-MINBLOCKRELTIME)*((float)(x-wblock->reltempo.x1)))/((float)(wblock->reltempo.x2-wblock->reltempo.x1))),
		MAXBLOCKRELTIME
	);

//	if(pc->isplaying==false){
		GFX_SetChangeInt(window,wblock,"Block RelTempo 0.001*",(int)(wblock->block->reltempo*1000));
		GFX_DrawWindowTitle(window,wblock);
//	}

	DrawBlockRelTempo(window,wblock);
#if 1
	if(pc->isplaying==false){
		WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);
	}
#endif
	return 0;
}

void SetMouseActionRelTempoSlider(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	int x,int y,
	int click
){
	void *temp;
	struct WBlocks *wblock=window->wblock;

	action->action=RELTEMPOSLIDER;
	if(click==0) return;
	Undo_RelTempoSlider(window,wblock);

	action->pointer1=wblock;
	action->MouseUpFunction=&MoveRelTempoSlider_Mouse;

	temp=window->prevaction.pointer1;
	window->prevaction.pointer1=wblock;
	MoveRelTempoSlider_Mouse(window,x,0);
	window->prevaction.pointer1=temp;
	
}



