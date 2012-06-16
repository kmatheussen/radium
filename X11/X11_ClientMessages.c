
#include "X11.h"
#include "../common/nsmtracker.h"
#include "../common/blocklist_proc.h"
#include "../api/api_common_proc.h"
#include "../common/eventreciever_proc.h"
#include "../midi/midi_getEvents_proc.h"

#include "X11_ClientMessages_proc.h"

extern struct Root *root;



bool X11Event_ClientMessage(XClientMessageEvent *event,struct Tracker_Windows *window){
  struct TEvent tevent={0};
  //fprintf(stderr,"******************** Got client message *************************** \n");

  switch(event->data.l[0]){
  case X11EVENT_PLAYLISTINSERT:
    fprintf(stderr,"******************** Got plauylist insert client message *************************** \n");
    BL_insert(
	      event->data.l[1],
	      getBlockFromNum(event->data.l[2])
	      );
    break;
  case X11EVENT_PLAYLISTDELETE:
    fprintf(stderr,"******************** Got plauylistdetlete client message *************************** \n");
    BL_delete(event->data.l[1]);
    break;
  case X11EVENT_KEYBOARDDOWN:
    //printf("Got Down %d %d\n",(int)event->data.l[1],(int)event->data.l[2]);
    //X11Event_KeyPress(event->data.s[1],window);
    tevent.ID=TR_KEYBOARD;
    tevent.SubID=event->data.l[1];
    tevent.keyswitch=event->data.l[2];
    if(EventReciever(&tevent,window)==1){
      return false;
    }
    break;
  case X11EVENT_KEYBOARDUP:
    //printf("Got Up %d %d\n",(int)event->data.l[1],(int)event->data.l[2]);
    //X11Event_KeyRelease(event->data.s[1],window);
    break;
  case X11EVENT_MIDIEVENT:
    MIDIGetEvents(
		  root->def_instrument,
		  (int)event->data.l[1],
		  (int)event->data.l[2],
		  (int)event->data.l[3],
		  (int)event->data.l[4]
		  );
    break;
    
  case X11EVENT_UPDATESONGPOS:
    fprintf(stderr,"Obsolete client message: X11EVENT_UPDATESONGPOS\n");
    //  P2MUpdateSongPosCallBack();
    break;
  
  default:
    printf("Unknown ClientMessage event: %d\n",(int)event->data.l[0]);
    break;
  }
  //	printf("Received \"%s\"\n",event->data.b);	

  return true;
}

