/* Copyright 2000 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include "includepython.h"

#include "nsmtracker.h"

#include "windows_proc.h"
#include "wblocks_proc.h"
#include "gfx_op_queue_proc.h"
#include "undo.h"

#include "fxlines_proc.h"

#include "visual_proc.h"
#include "../common/gfx_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"

#include "../api/api_proc.h"


#include <string.h>

#include "../api/api_support_proc.h"
#include "../api/api_requesters_proc.h"

#include "OS_Player_proc.h"

#include "eventreciever_proc.h"


extern struct Root *root;

bool doquit=false;
bool isloaded=false;

bool Quit(struct Tracker_Windows *window){
        PlayStop();

	printf("Going to quit\n");

        return Undo_are_you_sure_questionmark();
}



extern PyObject *gotkeyFunc;

static PyObject *Integers_py[EVENT_DASMAX];
static PyObject *Lists_py[12];

void init_pyobjects(void){
	int lokke;

	for(lokke=0;lokke<EVENT_DASMAX;lokke++){
		Integers_py[lokke]=PyInt_FromLong((long)lokke);
		Py_INCREF(Integers_py[lokke]);
		Py_INCREF(Integers_py[lokke]);
	}

	for(lokke=0;lokke<12;lokke++){
		Lists_py[lokke]=PyList_New(lokke);
		Py_INCREF(Lists_py[lokke]);
	}

}

static bool EventTreater(struct TEvent *in_tevent,struct Tracker_Windows *window){
  bool ret = false;
  
	uint32_t a=in_tevent->keyswitch;
	static bool isPyObjects=false;
	int lokke;

	int places[22];

//	PyObject *Tuple_py;
	PyObject *result,*arglist;
	int len;
	PyObject *list;

	if(isPyObjects==false){
		init_pyobjects();
		isPyObjects=true;
	}

	switch(in_tevent->ID){
		case TR_KEYBOARDUP:
		case TR_KEYBOARD:
		  if(in_tevent->SubID<0 || in_tevent->SubID>=EVENT_DASMAX) {
		    RError("Warning, Unknown TR_KEYBOARD; TR_SUB event: %d. \n",in_tevent->SubID);
		    break;
		  }
                  //printf("subid: %d\n",in_tevent->SubID);

			len=0;

			if(a&EVENT_LEFTCTRL){
				places[len]=EVENT_CTRL_L;
				len++;
			}
			if(a&EVENT_RIGHTCTRL){
				places[len]=EVENT_CTRL_L;
				len++;
			}
			if(a&EVENT_CAPSLOCK){
				places[len]=EVENT_CAPS;
				len++;
			}
			if(a&EVENT_LEFTSHIFT){
				places[len]=EVENT_SHIFT_L;
				len++;
			}
			if(a&EVENT_RIGHTSHIFT){
				places[len]=EVENT_SHIFT_R;
				len++;
			}
			if(a&EVENT_LEFTALT){
				places[len]=EVENT_ALT_L;
				len++;
			}
			if(a&EVENT_RIGHTALT){
				places[len]=EVENT_ALT_R;
				len++;
			}
			if(a&EVENT_LEFTEXTRA1){
				places[len]=EVENT_EXTRA_L;
				len++;
			}
			if(a&EVENT_RIGHTEXTRA1){
				places[len]=EVENT_EXTRA_R;
				len++;
			}
			if(a&EVENT_MOUSE_EDITOR2){
                          places[len]=EVENT_MOUSE_EDITOR;
                          len++;
			}
			if(a&EVENT_MOUSE_MIXER2){
                          places[len]=EVENT_MOUSE_MIXER;
                          len++;
			}
			if(a&EVENT_MOUSE_MIXERSTRIPS2){
                          places[len]=EVENT_MOUSE_MIXERSTRIPS;
                          len++;
			}
			if(a&EVENT_MOUSE_SEQUENCER2){
                          places[len]=EVENT_MOUSE_SEQUENCER;
                          len++;
			}
			//if(a&EVENT_AUTOREPEAT2){
			//	places[len]=EVENT_AUTOREPEAT;
			//	len++;
			//}
			if(in_tevent->ID==TR_KEYBOARDUP){
				places[len]=EVENT_UP;
				len++;
			}

			list=Lists_py[len];

			for(lokke=0;lokke<len;lokke++){
			        /* Incrementing the one to insert, so it doesn't dissapear when/if its replaced later. */
				Py_INCREF(Integers_py[places[lokke]]);
				PyList_SetItem(list,lokke,Integers_py[places[lokke]]);
			}

			if(list!=NULL){
				Py_INCREF(Integers_py[window->l.num]);
				Py_INCREF(Integers_py[in_tevent->SubID]);
				Py_INCREF(list);

				Py_INCREF(Integers_py[window->l.num]);
				Py_INCREF(Integers_py[in_tevent->SubID]);
				Py_INCREF(list);

				arglist=Py_BuildValue("llO",window->l.num,in_tevent->SubID,list);

				/* Executing python-code */
				//printf("EVALING gotkeyfunc\n");
				result=PyEval_CallObject(gotkeyFunc,arglist);
          
				Py_DECREF(arglist);
				if(result!=NULL){

                                  bool resultbool;

                                  if (result==Py_False)
                                    resultbool=false;
                                  else if (result==Py_True)
                                    resultbool=true;
                                  else {
                                    RError("Something is wrong. keyboard exe function didnt return boolean");                                    
                                    resultbool=true;
                                  }
                                  //printf("********** result: %d\n",resultbool);
                                  ret = resultbool;
                                  
				  Py_DECREF(result);
				}
			}

			break;
	}

	if(isloaded==true)
	  isloaded=false;

        //printf("*********** ret: %d\n",ret);
        return ret;
}

/*
uint32_t CanITreatThisEvent_questionmark(int ID,struct Tracker_Windows *window){
	return (window->event_treat&(1<<ID));
}
*/

bool EventReciever(struct TEvent *in_tevent, struct Tracker_Windows *window){
         R_ASSERT(g_pausing_level==0);
  
        int ret;
        DO_GFX({
            ret=EventTreater(in_tevent,window);
          });

        if(g_pausing_level != 0){
          RError("EventReceiver: g_pausing_level: %d", g_pausing_level);
          g_pausing_level = 0;
        }
        
        return ret;
}

/*
static int DoTreatAllEvents(struct Tracker_Windows *window){
	struct TEventFIFO *element,*prev=NULL;
	int ret=0,nowret;
	element=window->TELroot;
	while(element!=NULL){
		if(CanITreatThisEvent_questionmark(element->t.ID,window)==0){
			nowret=EventTreater(&element->t,window);
			ret=R_MAX(ret,nowret);
			if(prev!=NULL){
				prev->next=element->next;
			}else{
				window->TELroot=element->next;
			}
		}
		prev=element;
		element=element->next;
	}

	element=window->TELroot;
	if(element==NULL){
		window->TELlast=NULL;
	}else{
		while(element!=NULL){
			if(element->next==NULL) window->TELlast=element;
			element=element->next;
		}
	}

	return ret;
}

void DontTreatAnyEvents(struct Tracker_Windows *window){
	window->event_treat=(uint32_t)(~0);
}

void DontTreatAnyEvents_AndDontBuffer(struct Tracker_Windows *window){
	window->event_treat=(uint32_t)(~0);
	window->dontbuffer=1;
}

int TreatAllEvents(struct Tracker_Windows *window){
	window->event_treat=0;
	window->dontbuffer=0;
	return DoTreatAllEvents(window);
}

int TreatEvents(int ID,struct Tracker_Windows *window){
	window->event_treat&=~(1<<ID);
	return DoTreatAllEvents(window);
}

void DontTreatEvents(int ID,struct Tracker_Windows *window){
	window->event_treat|=(1<<ID);
}

*/
