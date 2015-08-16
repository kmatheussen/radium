#include "nsmtracker.h"
#include "mouse.h"
#include "undo_reltemposlider_proc.h"
#include "gfx_wblocks_reltempo_proc.h"
#include "gfx_statusbar_proc.h"
#include "gfx_tempocolor_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"

#include "playerclass.h"

#include "mouse_reltemposlider_proc.h"

extern PlayerClass *pc;

static void update_statusbar(struct Tracker_Windows *window){
  struct WBlocks *wblock = window->wblock;
  GFX_SetChangeInt(window,wblock,"Block RelTempo 0.001*",(int)(wblock->block->reltempo*1000));
  GFX_DrawStatusBar(window,wblock);
}

static int MoveRelTempoSlider_Mouse(
	struct Tracker_Windows *window,
	float x,float y
){
	struct WBlocks *wblock=window->wblock;

	if(wblock!=(struct WBlocks *)window->prevaction.pointer1) return 0;

	wblock->block->reltempo=R_BOUNDARIES(
		MINBLOCKRELTIME,
		MINBLOCKRELTIME+(((MAXBLOCKRELTIME-MINBLOCKRELTIME)*((float)(x-wblock->reltempo.x1)))/((float)(wblock->reltempo.x2-wblock->reltempo.x1))),
		MAXBLOCKRELTIME
	);

        update_statusbar(window);

	DrawBlockRelTempo(window,wblock);

        TRACKREALLINES_update_peak_tracks(window,NULL);

#if !USE_OPENGL
        WBLOCK_DrawTempoColor(window,wblock,0,wblock->num_reallines);

        DrawUpAllWTracks(window,wblock,NULL);
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
	if(click==0){
          update_statusbar(window);
          return;
        }
	Undo_RelTempoSlider(window,wblock);

	action->pointer1=wblock;
	action->MouseUpFunction=&MoveRelTempoSlider_Mouse;

	temp=window->prevaction.pointer1;
	window->prevaction.pointer1=wblock;
	MoveRelTempoSlider_Mouse(window,x,0);
	window->prevaction.pointer1=temp;
	
}



