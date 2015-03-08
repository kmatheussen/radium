
#include "nsmtracker.h"
#include "undo.h"
#include "time_proc.h"

#include "undo_maintempos_proc.h"

extern struct Root *root;

struct Undo_MainTempo{
	int tempo;
	int lpb;
        int quantitize_numerator;
        int quantitize_denominator;
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
        u_rt->quantitize_numerator = root->quantitize_numerator;
        u_rt->quantitize_denominator = root->quantitize_denominator;

	Undo_Add(
                 window->l.num,
                 wblock->l.num,
                 wblock->wtrack->l.num,
                 wblock->curr_realline,
                 u_rt,
                 Undo_Do_MainTempo
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
	int lpb=root->lpb;
	int tempo=root->tempo;
	int quantitize_numerator=root->quantitize_numerator;
        int quantitize_denominator=root->quantitize_denominator;

	root->lpb=u_rt->lpb;
	root->tempo=u_rt->tempo;
        root->quantitize_numerator = u_rt->quantitize_numerator;
        root->quantitize_denominator = u_rt->quantitize_denominator;

	UpdateAllSTimes();

	u_rt->lpb=lpb;
	u_rt->tempo=tempo;
        u_rt->quantitize_numerator = quantitize_numerator;
        u_rt->quantitize_denominator = quantitize_denominator;

	return u_rt;
}


