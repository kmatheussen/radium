

#include "nsmtracker.h"
#include "sequencer_proc.h"

#include "undo.h"

#include "undo_reltemposlider_proc.h"


struct Undo_RelTempoSlider{
	float reltempo;
};

static void *Undo_Do_RelTempoSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(RelTempoSlider(
                                  struct Tracker_Windows *window,
                                  struct WBlocks *wblock
                                  )
                   )
{
	struct Undo_RelTempoSlider *u_rts=talloc_atomic(sizeof(struct Undo_RelTempoSlider));
	u_rts->reltempo=ATOMIC_DOUBLE_GET(wblock->block->reltempo);

        Undo_Add_dont_stop_playing(
                                   window->l.num,
                                   wblock->l.num,
                                   wblock->wtrack->l.num,
                                   window->wblock->curr_realline,
                                   u_rts,
                                   Undo_Do_RelTempoSlider,
                                   "Block multiplier"
                                   );
}

static void *Undo_Do_RelTempoSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_RelTempoSlider *u_rts=(struct Undo_RelTempoSlider *)pointer;

	float reltempo=ATOMIC_DOUBLE_GET(wblock->block->reltempo);

	ATOMIC_DOUBLE_SET(wblock->block->reltempo, u_rts->reltempo);
        SEQUENCER_block_changes_tempo_multiplier(wblock->block, u_rts->reltempo);

	u_rts->reltempo=reltempo;

	return u_rts;
}

