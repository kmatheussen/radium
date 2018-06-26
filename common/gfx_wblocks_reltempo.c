
#include "nsmtracker.h"
#include "gfx_slider_proc.h"


#include "gfx_wblocks_reltempo_proc.h"


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
		ATOMIC_DOUBLE_GET(wblock->block->reltempo),
		MINBLOCKRELTIME,
		MAXBLOCKRELTIME,
		true,
		PAINT_BUFFER
	);
}



