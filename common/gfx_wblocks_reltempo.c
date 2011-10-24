
#include "nsmtracker.h"
#include "gfx_slider_proc.h"



void DrawBlockRelTempo(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){

/*
	TBox tbox;

	tbox.x1=0;
	tbox.y1=wblock->t.y2+1;
	tbox.x2=wblock->wtracks->x-3;
	tbox.y2=window->height;
*/

	DrawSlider(
		window,
		&wblock->reltempo,
		wblock->block->reltempo,
		MINBLOCKRELTIME,
		MAXBLOCKRELTIME,
		true,
		false
	);
}



