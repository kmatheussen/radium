

#include "nsmtracker.h"
#include "undo.h"
#include "OS_Player_proc.h"

#include "undo_trackheader_proc.h"


struct Undo_TrackHeader{
	int volume;
	int pan;
	bool volumeonoff;
	bool panonoff;
};

static void *Undo_Do_TrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(TrackHeader(
	struct Tracker_Windows *window,
	struct Blocks *block,
	struct Tracks *track,
	int realline
                               )
                   )
{
	struct Undo_TrackHeader *u_th=talloc_atomic(sizeof(struct Undo_TrackHeader));
	u_th->volume=track->volume;
	u_th->pan=track->pan;
	u_th->volumeonoff=track->volumeonoff;
	u_th->panonoff=track->panonoff;

        Undo_Add_dont_stop_playing(
                                   window->l.num,
                                   block->l.num,
                                   track->l.num,
                                   realline,
                                   u_th,
                                   Undo_Do_TrackHeader,
                                   "Block track header"
                                   );

}


static void *Undo_Do_TrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
	struct Undo_TrackHeader *u_th=(struct Undo_TrackHeader *)pointer;
	int volume=wtrack->track->volume;
	int pan=wtrack->track->pan;
	bool volumeonoff=wtrack->track->volumeonoff;
	bool panonoff=wtrack->track->panonoff;

	wtrack->track->volume=u_th->volume;
	wtrack->track->pan=u_th->pan;
	wtrack->track->volumeonoff=u_th->volumeonoff;
	wtrack->track->panonoff=u_th->panonoff;

	if(wtrack->track->panonoff && wtrack->track->patch!=NULL){
          PLAYER_lock();
          (*wtrack->track->patch->changeTrackPan)(wtrack->track->pan,wtrack->track);
          PLAYER_unlock();
	}

	u_th->volume=volume;
	u_th->pan=pan;
	u_th->panonoff=panonoff;
	u_th->volumeonoff=volumeonoff;

	return u_th;
}

