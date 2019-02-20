

#include "nsmtracker.h"
#include "undo.h"
#include "list_proc.h"
#include "OS_Player_proc.h"

#include "undo_trackheader_proc.h"


struct Undo_TrackHeader{
        NInt blocknum;
        NInt tracknum;
	int volume;
	int pan;
	bool volumeonoff;
	bool panonoff;
        int midi_channel;
        int onoff;
};

static void fill_in(struct Undo_TrackHeader *u_th, struct WBlocks *wblock, struct WTracks *wtrack){
  u_th->blocknum=wblock->l.num;
  u_th->tracknum=wtrack->l.num;

  struct Tracks *track = wtrack->track;
    
  u_th->volume=track->volume;
  u_th->pan=track->pan;
  u_th->volumeonoff=track->volumeonoff;
  u_th->panonoff=track->panonoff;
  u_th->midi_channel=ATOMIC_GET(track->midi_channel);
  u_th->onoff=track->onoff;
}

static void fill_out(struct Undo_TrackHeader *u_th, struct WBlocks *wblock, struct WTracks *wtrack){
  R_ASSERT(wblock->l.num==u_th->blocknum);
  R_ASSERT(wtrack->l.num==u_th->tracknum);

  struct Tracks *track = wtrack->track;

  track->volume=u_th->volume;
  track->pan=u_th->pan;
  track->volumeonoff=u_th->volumeonoff;
  track->panonoff = u_th->panonoff;
  ATOMIC_SET(track->midi_channel, u_th->midi_channel);
  track->onoff = u_th->onoff;
}

static void *Undo_Do_TrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(TrackHeader(
                               NInt blocknum,
                               NInt tracknum
                               )
                   )
{
  R_ASSERT_RETURN_IF_FALSE(blocknum>=0);
  R_ASSERT_RETURN_IF_FALSE(tracknum>=0);

        struct Tracker_Windows *window = root->song->tracker_windows;
    
	struct Undo_TrackHeader *u_th=talloc_atomic(sizeof(struct Undo_TrackHeader));

        struct WBlocks *wblock = ListFindElement1(&window->wblocks->l, blocknum);
        struct WTracks *wtrack = ListFindElement1(&wblock->wtracks->l, tracknum);

        fill_in(u_th, wblock, wtrack);
        
        Undo_Add_dont_stop_playing(
                                   window->l.num,
                                   window->wblock->l.num,
                                   window->wblock->wtrack->l.num,
                                   window->wblock->curr_realline,
                                   u_th,
                                   Undo_Do_TrackHeader,
                                   "Block track header"
                                   );

}


static void *Undo_Do_TrackHeader(
	struct Tracker_Windows *window,
	struct WBlocks *currwblock,
	struct WTracks *currwtrack,
	int realline,
	void *pointer
){
	struct Undo_TrackHeader *u_th=(struct Undo_TrackHeader *)pointer;
        
        struct WBlocks *wblock = ListFindElement1(&window->wblocks->l, u_th->blocknum);
        struct WTracks *wtrack = ListFindElement1(&wblock->wtracks->l, u_th->tracknum);

        struct Undo_TrackHeader u_th2 = {0};
        fill_in(&u_th2, wblock, wtrack);

        fill_out(u_th, wblock, wtrack);

	if(wtrack->track->panonoff && wtrack->track->patch!=NULL){
          PLAYER_lock();
          (*wtrack->track->patch->changeTrackPan)(wtrack->track->pan,wtrack->track);
          PLAYER_unlock();
	}

        memcpy(u_th, &u_th2, sizeof(struct Undo_TrackHeader));
        
	return u_th;
}

