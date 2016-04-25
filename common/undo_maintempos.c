
#include "nsmtracker.h"
#include "undo.h"
#include "time_proc.h"
#include "Beats_proc.h"
#include "OS_visual_input.h"

#include "undo_maintempos_proc.h"

extern struct Root *root;

struct Undo_MainTempo{
	int tempo;
	int lpb;
        Ratio signature;
        quantitize_options_t quantitize_options;
};

void *Undo_Do_MainTempo(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void Undo_MainTempo(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	struct Undo_MainTempo *u_rt=talloc_atomic(sizeof(struct Undo_MainTempo));
	u_rt->tempo=root->tempo;
	u_rt->lpb=root->lpb;
        u_rt->signature=root->signature;
        u_rt->quantitize_options = root->quantitize_options;

	Undo_Add(
                 window->l.num,
                 wblock->l.num,
                 wblock->wtrack->l.num,
                 wblock->curr_realline,
                 u_rt,
                 Undo_Do_MainTempo,
                 "Block tempo/lpb/signature/quantiatize settings",
                 LOC()
	);

}

void *Undo_Do_MainTempo(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_MainTempo *u_rt=(struct Undo_MainTempo *)pointer;
	Ratio signature = root->signature;
        int lpb=root->lpb;
	int tempo=root->tempo;
        quantitize_options_t quantitize_options = root->quantitize_options;

	root->signature=u_rt->signature;
        root->lpb=u_rt->lpb;
	root->tempo=u_rt->tempo;
        root->quantitize_options = u_rt->quantitize_options;

        GFX_OS_update_bottombar();
        
	UpdateAllSTimes();
        UpdateAllBeats();

	u_rt->signature=signature;
        u_rt->lpb=lpb;
	u_rt->tempo=tempo;
        u_rt->quantitize_options = quantitize_options;

	return u_rt;
}


