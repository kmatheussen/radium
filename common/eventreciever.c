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



#include <Python.h>

#include "nsmtracker.h"

#include "windows_proc.h"
#include "wblocks_proc.h"
#include "mouse_proc.h"
#include "gfx_op_queue_proc.h"

#include "fxlines_proc.h"

#include "visual_proc.h"
#include "../common/gfx_proc.h"
#include "../common/player_proc.h"

#include "../api/radium_proc.h"


#include <string.h>

#include "../api/api_support_proc.h"
#include "../api/api_requesters_proc.h"

#include "OS_Player_proc.h"

#include "eventreciever_proc.h"


extern struct Root *root;

extern int num_undos;

bool doquit=false;
bool isloaded=false;

bool Quit(struct Tracker_Windows *window){

	char temp[200];
	char *ret=NULL;

        PlayHardStop();

	printf("Going to quit\n");
	if(num_undos>0){
		sprintf(
			temp,
			"%d change%s been made to file.\nAre you shure? (yes/no) >",
			num_undos,
			num_undos>1 ? "s have" : " has"
                        );
		while(
			ret==NULL || (
				strcmp("yes",ret) &&
				strcmp("no",ret)
			)
		){
			ret=GFX_GetString(
				window,
				NULL,
				temp
			);
			
		}
		if(!strcmp("no",ret)) return false;
	}

	return true;
}



extern PyObject *gotkeyFunc;

static PyObject *Integers_py[130];
static PyObject *Lists_py[12];

void init_pyobjects(void){
	int lokke;

	for(lokke=0;lokke<130;lokke++){
		Integers_py[lokke]=PyInt_FromLong((long)lokke);
		Py_INCREF(Integers_py[lokke]);
		Py_INCREF(Integers_py[lokke]);
	}

	for(lokke=0;lokke<12;lokke++){
		Lists_py[lokke]=PyList_New(lokke);
		Py_INCREF(Lists_py[lokke]);
	}

}

extern struct WrapFuncList wrapfunclist[];

struct KeyConfigs{
	struct KeyConfigs *next;
	void *func;
	uint32_t a;
	int num_args;
	int *args;
};

struct KeyConfigs *keyconfigs[EVENT_DASMAX+1]={0};


/* This is a function that is quite equialent to the addKey function
   in start.py. It is used to speed up keybindings that does not need
   any python parsing. The functions must just have integer arguments.
*/
const char *ER_keyAdd(int key,char *funcname,PyObject *pykeys,PyObject *pyargs){
//	char *thechar;
	struct KeyConfigs *kc;
	int keyslen;
	int argslen;
	int lokke;
	struct WrapFuncList *wfl;
	void *func;

	int *keys=PYR_getIntArray(&keyslen,pykeys);
	int *args=PYR_getIntArray(&argslen,pyargs);

	if(keyslen==-1) return "Illegal keybinding";
	if(argslen==-1) return "Illegal arguments";

	for(lokke=0;;lokke++){
		wfl=&wrapfunclist[lokke];
		if(wfl->funcname==NULL){
		  return "function does not exist.";
		}
		if(!strcmp(funcname,wfl->funcname)){
			func=wfl->func;
			break;
		}
	}

	kc=calloc(1,sizeof(struct KeyConfigs));
	if(kc==NULL) return "Out of memory";

	kc->func=func;
	kc->num_args=argslen;
	kc->args=malloc(sizeof(int)*argslen);
	if(kc==NULL) return "Out of memory";

	for(lokke=0;lokke<argslen;lokke++){
		kc->args[lokke]=args[lokke];
	}

	for(lokke=0;lokke<keyslen;lokke++){
		kc->a |= 1<<keys[lokke];
	}

	kc->next=keyconfigs[key];
	keyconfigs[key]=kc;

	return "OK";
}

bool ER_gotKey(int key,uint32_t a,bool down){
	struct KeyConfigs *kc=keyconfigs[key];

	if(down==false){
		a |= EVENT_UP2;
	}

	while(kc!=NULL){
		if(kc->a==a){
			switch(kc->num_args){
				case 0:
					(*((void (*)(void))(kc->func)))();
					break;
				case 1:
					(*((void (*)(int))(kc->func)))(kc->args[0]);
					break;
				case 2:
					(*((void (*)(int,int))(kc->func)))(kc->args[0],kc->args[1]);
					break;
				case 3:
					(*((void (*)(int,int,int))(kc->func)))(kc->args[0],kc->args[1],kc->args[2]);
					break;
				case 4:
					(*((void (*)(int,int,int,int))(kc->func)))(kc->args[0],kc->args[1],kc->args[2],kc->args[3]);
					break;
				case 5:
					(*((void (*)(int,int,int,int,int))(kc->func)))(kc->args[0],kc->args[1],kc->args[2],kc->args[3],kc->args[4]);
					break;
				default:
					RError("Program is not able to to handle that many arguments in file eventreceiver.c. Very strange that this bug showed up, by the way. Please report.\n");
					break;
			}
			return true;
		}

		kc=kc->next;
	}
	return false;
}

static int EventTreater(struct TEvent *in_tevent,struct Tracker_Windows *window){
	uint32_t a=in_tevent->keyswitch;
	static bool isPyObjects=false;
	int lokke;

	int places[12];

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
			if(ER_gotKey(in_tevent->SubID,a,in_tevent->ID==TR_KEYBOARD?true:false)==true){
				break;
			}

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
//				printf("EVALING!!!\n");
				result=PyEval_CallObject(gotkeyFunc,arglist);

				Py_DECREF(arglist);
				if(result!=NULL){
				  Py_DECREF(result);
				}
			}

			break;
		case TR_MOUSEMOVE:
			MouseMove(window,in_tevent->x,in_tevent->y);
			break;
		case TR_LEFTMOUSEDOWN:
			LeftMouseDown(window,in_tevent->x,in_tevent->y);
			break;
		case TR_LEFTMOUSEUP:
			return LeftMouseUp(window,in_tevent->x,in_tevent->y);
		case TR_RIGHTMOUSEUP:
		 // return RightMouseDown(window,in_tevent->x,in_tevent->y);
		 			AddFXNodeLineCurrPos(window);
			break;
	}

	if(doquit==true){return 1;}
	if(isloaded==true){
	  isloaded=false;
	  return 2;
	}
	return 0;
}

uint32_t CanITreatThisEvent_questionmark(int ID,struct Tracker_Windows *window){
	return (window->event_treat&(1<<ID));
}

int EventReciever(struct TEvent *in_tevent, struct Tracker_Windows *window){
	struct TEventFIFO *element;

	if(window->dontbuffer==1) return 0;

	if(CanITreatThisEvent_questionmark(in_tevent->ID,window)==0){

          int ret;
          DO_GFX({
              ret=EventTreater(in_tevent,window);
            });

	  return ret;
	}

	if(window->TELlast==NULL){
		element=window->TELroot=window->TELlast=talloc(sizeof(struct TEventFIFO));
	}else{
		element=window->TELlast->next=talloc(sizeof(struct TEventFIFO));
	}

	element->t.ID=in_tevent->ID;
	element->t.SubID=in_tevent->SubID;
	element->t.keyswitch=in_tevent->keyswitch;
	element->t.x=in_tevent->x;
	element->t.y=in_tevent->y;

	window->TELlast=element;

        closeRequester();

	return 0;
}

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








