
#include "nsmtracker.h"
#include "undo.h"
#include "time_proc.h"

#include "undo_maintempos_proc.h"

extern struct Root *root;

struct Undo_MainTempo{
	int tempo;
	int lpb;
	float quantitize;
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
	u_rt->quantitize=root->quantitize;

	Undo_New(
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
	float quantitize=root->quantitize;

	root->lpb=u_rt->lpb;
	root->tempo=u_rt->tempo;
	root->quantitize=u_rt->quantitize;

	UpdateAllSTimes();

	u_rt->lpb=lpb;
	u_rt->tempo=tempo;
	u_rt->quantitize=quantitize;

	return u_rt;
}


