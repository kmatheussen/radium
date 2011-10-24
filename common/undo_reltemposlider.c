

#include "nsmtracker.h"
#include "undo.h"

#include "undo_reltemposlider_proc.h"


struct Undo_RelTempoSlider{
	float reltempo;
};

void *Undo_Do_RelTempoSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void Undo_RelTempoSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct Undo_RelTempoSlider *u_rts=talloc_atomic(sizeof(struct Undo_RelTempoSlider));
	u_rts->reltempo=wblock->block->reltempo;

	Undo_New(
		window->l.num,
		wblock->l.num,
		wblock->wtrack->l.num,
		window->wblock->curr_realline,
		u_rts,
		Undo_Do_RelTempoSlider
	);

}

void *Undo_Do_RelTempoSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_RelTempoSlider *u_rts=(struct Undo_RelTempoSlider *)pointer;

	float reltempo=wblock->block->reltempo;

	wblock->block->reltempo=u_rts->reltempo;

	u_rts->reltempo=reltempo;

	return u_rts;
}

