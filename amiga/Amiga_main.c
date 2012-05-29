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



/************************************************************
   Main. Lots of good examples on really ugly code.
************************************************************/


#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <dos/dos.h>
#include <proto/gadtools.h>
#include <proto/dos.h>
#include <proto/input.h>
#include <proto/reqtools.h>
#include <devices/inputevent.h>
#include <exec/types.h>
#include <proto/realtime.h>
#include <string.h>
#include <sys/stat.h>


#include "../common/nsmtracker.h"

#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <exec/memory.h>
#include <proto/exec.h>
#include <proto/intuition.h>

#include "../common/eventreciever_proc.h"
#include "../common/windows_proc.h"
#include "../common/song_proc.h"
#include "../common/list_proc.h"
#include "GFX_Amiga_egc_proc.h"
#include "../common/instruments_proc.h"
#include "../common/visual_proc.h"
#include "Amiga_bs.h"
#include "Amiga_bs_edit_proc.h"
#include "Amiga_player_proc.h"
#include "../common/instruments_proc.h"
#include "../common/playerclass.h"
#include "../common/control_proc.h"
#include "../common/trackreallineelements_proc.h"
#include "../common/PEQ_clock_proc.h"
#include "../common/notes_proc.h"
#include "plug-ins/camd_i_input_proc.h"
#include "instrprop/Amiga_instrprop.h"
#include "instrprop/Amiga_instrprop_init_proc.h"
#include "instrprop/Amiga_instrprop_edit_proc.h"
#include "disk_amiga_proc.h"
#include "Amiga_config_proc.h"
#include "Amiga_colors_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/disk_save_proc.h"
#include "../mmd2loader/mmd2load_proc.h"
#include "../common/blts_proc.h"
#include "../common/resizewindow_proc.h"

#include "../config/config.h"


struct ReqToolsBase *ReqToolsBase=NULL;

#ifndef USEMINICAMD
#  include <midi/camdbase.h>
	struct Library *CamdBase=NULL;
#endif

void GC_amiga_free_all_mem(void);


struct Library *InputBase;
struct IOStdReq *InputIO;
struct MsgPort *keyPort;

extern struct Root *root;

extern PlayerClass *pc;

#ifndef TRACKER_INCLUDE
extern LONG ptask2mtasksig;
extern LONG debugsig;
extern LONG clocksig;
#endif

extern void (*Ptask2MtaskCallBack)(void);
struct Task *mytask;

extern void *tmemory;

struct TEvent tevent={0};


#define Q2E(a,b) 	if(qualifier&a){tevent.keyswitch|=b;}else{tevent.keyswitch&=~b;}

void TreatKey(struct IntuiMessage *msg){
	UWORD qualifier=msg->Qualifier;
	Q2E( IEQUALIFIER_LSHIFT   , EVENT_LEFTSHIFT   );
	Q2E( IEQUALIFIER_RSHIFT   , EVENT_RIGHTSHIFT  );
	Q2E( IEQUALIFIER_CAPSLOCK , EVENT_CAPSLOCK    );
	Q2E( IEQUALIFIER_CONTROL  , EVENT_LEFTCTRL    );
	Q2E( IEQUALIFIER_LALT     , EVENT_LEFTALT     );
	Q2E( IEQUALIFIER_RALT     , EVENT_RIGHTALT    );
	Q2E( IEQUALIFIER_LCOMMAND , EVENT_LEFTEXTRA1  );
	Q2E( IEQUALIFIER_RCOMMAND , EVENT_RIGHTEXTRA1 );
}

extern int dodebug;

LONG Bptask2mtasksig=-1;
LONG Bdebugsig=-1;
LONG inputsig=-1;

extern struct Conductor *conductor;

char *mmp2filename=NULL;

static char *num_down=NULL;

int HandleKey(struct IntuiMessage *msg,struct Tracker_Windows *win){
	int ret=0;
	static const unsigned char AmigaToTrackerSubID[0x60]={EVENT_1L1,EVENT_1,EVENT_2,EVENT_3,EVENT_4,EVENT_5,EVENT_6,EVENT_7,EVENT_8,EVENT_9,EVENT_0,EVENT_0R1,EVENT_0R2,EVENT_0R3,EVENT_NO,EVENT_KP_0,EVENT_Q,EVENT_W,EVENT_E,EVENT_R,EVENT_T,EVENT_Y,EVENT_U,EVENT_I,EVENT_O,EVENT_P,EVENT_PR1,EVENT_PR2,EVENT_NO,EVENT_KP_1,EVENT_KP_2,EVENT_KP_3,EVENT_A,EVENT_S,EVENT_D,EVENT_F,EVENT_G,EVENT_H,EVENT_J,EVENT_K,EVENT_L,EVENT_LR1,EVENT_LR2,EVENT_LR3,EVENT_NO,EVENT_KP_4,EVENT_KP_5,EVENT_KP_6,EVENT_ZL1,EVENT_Z,EVENT_X,EVENT_C,EVENT_V,EVENT_B,EVENT_N,EVENT_M,EVENT_MR1,EVENT_MR2,EVENT_MR3,EVENT_NO,EVENT_KP_DOT,EVENT_KP_7,EVENT_KP_8,EVENT_KP_9,EVENT_SPACE,EVENT_BACKSPACE,EVENT_TAB,EVENT_KP_ENTER,EVENT_RETURN,EVENT_ESC,EVENT_DEL,EVENT_NO,EVENT_NO,EVENT_NO,EVENT_KP_SUB,EVENT_NO,EVENT_UPARROW,EVENT_DOWNARROW,EVENT_RIGHTARROW,EVENT_LEFTARROW,EVENT_F1,EVENT_F2,EVENT_F3,EVENT_F4,EVENT_F5,EVENT_F6,EVENT_F7,EVENT_F8,EVENT_F9,EVENT_F10,EVENT_KP_E1,EVENT_KP_E2,EVENT_KP_DIV,EVENT_KP_MUL,EVENT_KP_ADD,EVENT_HELP};

	TreatKey(msg);
	tevent.x=msg->MouseX-win->os_visual->xpluss;
	tevent.y=msg->MouseY-win->os_visual->ypluss;
	if(msg->Code >= 0x80 && msg->Code<0x80+0x60){
		tevent.ID=TR_KEYBOARDUP;
		tevent.SubID=AmigaToTrackerSubID[msg->Code-0x80];
		num_down[msg->Code]=0;
		EventReciever(&tevent,win);
	}else{
		if(msg->Code<0x60){
			tevent.SubID=AmigaToTrackerSubID[msg->Code];
			if(num_down[msg->Code]==1){
				tevent.ID=TR_KEYBOARDUP;
				EventReciever(&tevent,win);
			}
			num_down[msg->Code]=1;
			tevent.ID=TR_KEYBOARD;
			switch(EventReciever(&tevent,win)){
				case 1: ret=1;
					break;
				case 2: ret=2;
				default:
					break;
			}
		}
	}
	return ret;
}

//extern struct IntuiMessage *CPPWindowMsg;
int CPPWindowRawKey(void){
	return HandleKey(&CPPWindowMsg,root->song->tracker_windows);
}

extern void Amiga_memory_config(void);

int __stdargs __saveds radium_main(char *arg){

  //	int argc=1;
  //	char **argv[2]={"radium",NULL};

	int bswsuccess;
	ULONG signal;
	ULONG waitsig;
	struct IntuiMessage *msg;
	BOOL done=FALSE;
	struct Tracker_Windows *temp;
	bool doconfig=false;
	FILE *file;
	bool firsttime=true;
	struct stat st;
	struct Window *window;


// Used to keap track of number of downs before ups.
//	char *num_down=NULL;

	int note,velocity;

#ifndef NOPLAYER
	STime lastStuffTime=0;
#endif

	//	if(argc==0){
	//		fprintf(stderr,"This program can not be started from workbench. \n");
	//		return 0;
		//	}

	if(stacksize()<32000){
		fprintf(stderr,"Stack-size: %d\n",stacksize());
		fprintf(stderr,"Sorry, you should have at least 32000 bytes of stackspace to run this program.\n");
		fprintf(stderr,"\nRun \"Stack 50000\" before you try again.\n");
		exit(1);
	}

	if(stat("devs:midi/mmp",&st)==0){
//		printf("size: %d\n",st.st_size);
		if(st.st_size==3722 || st.st_size==3924 || st.st_size==3728 || st.st_size==3724){
			fprintf(stderr,"You are running a to old version of devs:midi/mmp\n");
			fprintf(stderr,"Please update to latest version.\n");
			exit(2);
		}
	}else{
		if(stat("devs:midi/mmp68000",&st)==0){
			if(st.st_size==3722 || st.st_size==3924 || st.st_size==3728){
				fprintf(stderr,"You are running a to old version of devs:midi/mmp\n");
				fprintf(stderr,"Please update to latest version.\n");
				exit(2);
			}
		}
	}

#ifndef USEMINICAMD
   CamdBase=OpenLibrary("camd.library",40L);
	if(CamdBase==NULL){
		fprintf(stderr,"Couldn't open at least V40 of camd.library.\n");
		exit(3);
	}
	if(CamdBase->lib_Version==40L && CamdBase->lib_Revision<5){
		fprintf(stderr,"WARNING! You should update camd.library to at least V40.5.\n");
	}
#endif

	printf(
				"\n\n***********************************************************************"
				"\n...Radium V%f%s, Usage: radium start.py keybindings.conf [-config]\n"
				"***********************************************************************"
				"\n\n\n",
				VERSION,VERSION_LETTER
			);

//	SetTaskPri(FindTask(0),20);

	Amiga_memory_config();

	keyPort=CreatePort(NULL,NULL);
	if(keyPort==NULL){
		fprintf(stderr,"Could not open keyport\n");
		return 16;
	}
	InputIO=(struct IOStdReq *)CreateExtIO(keyPort,sizeof(struct IOStdReq));
	if(InputIO==NULL){
		fprintf(stderr,"Could not make ExtIO.\n");
		DeletePort(keyPort);
		return 17;
	}
	if(OpenDevice("input.device",NULL,(struct IORequest *)InputIO,NULL)!=0){
		fprintf(stderr,"Could not open input.device\n");
		DeleteExtIO((struct IORequest *)InputIO);
		DeletePort(keyPort);
		return 15;
	}
	InputBase=(struct Library *)InputIO->io_Device;

	if(!strcmp(arg,"-config")){
	  doconfig=1;
	  dodebug=0;
	}

//	debug("Main: stacksize: %d\n",stacksize());


	ReqToolsBase=(struct ReqToolsBase *)OpenLibrary("reqtools.library",0L);
	if(ReqToolsBase==NULL){
		fprintf(stderr,"Could not open reqtools.library\n");
		exit(90);
	}

	if( ! ReadAmigaConfig(doconfig) ){
		exit(3);
	}

	Amiga_SetScreenColors();

	Bptask2mtasksig=ptask2mtasksig=AllocSignal(-1);
	if(-1==ptask2mtasksig){
		fprintf(stderr,"Cant allocate signal. \n");
		exit(2);
	}
	ptask2mtasksig=1L<<ptask2mtasksig;

	mytask=FindTask(NULL);

	Bdebugsig=debugsig=AllocSignal(-1);
	if(-1==debugsig){
		FreeSignal(Bptask2mtasksig);
		fprintf(stderr,"Cant allocate debug signal. \n");
		exit(2);
	}
	debugsig=1L<<debugsig;

	if( ! Amiga_initplayer1()) goto exit;

	system("makedir rad:trackertemp >NIL:");


/*************************************************/

	if ( ! InitProgram() ) goto exit;

/*************************************************/

	Amiga_initplayer2();		// Actually, start player.

	Disk_Amiga_ControlInit();

newprogram:

//	fprintf(stderr,"freemem, start: %d\n",AvailMem(MEMF_ANY));

	free(num_down);
	num_down=calloc(1,1000);

	Scr=root->song->tracker_windows->os_visual->screen;

	if(OpenHelpWindow()!=0){
		fprintf(stderr,"Error. Could not open Help window. Probably out of memory.\n");
		goto exit;
	}
	bswsuccess=OpenBlockSelectWindow();
	if(bswsuccess!=0){
		fprintf(stderr,"Error. Could not open Blockselector window. The reason was:\n");
		switch(bswsuccess){
			case 1:
				fprintf(stderr,"You were out of memory.\n");
				break;
			case 2:
				fprintf(stderr,"Could not create gadget. Probably out of memory.\n");
				break;
			default:
				fprintf(stderr,"You are probably very low on chip or fast memory.\n");
				break;
		}
		goto exit;
	}
	if(myOpenCPPWindowWindow()!=0){
		fprintf(stderr,"Could not open Camd Patch Properties window. Probably out of memory.\n");
		goto exit;
	}

	BS_UpdateBlockList();

	BS_UpdatePlayList();

	(*root->song->instruments->PP_Update)(root->song->instruments,root->song->tracker_windows->wblock->wtrack->track->patch);

	Disk_Amiga_ResizeBSWindow();

	if(mmp2filename!=NULL) LoadMMP2(root->song->tracker_windows,mmp2filename);
	mmp2filename=NULL;

	if(firsttime){
		firsttime=false;
		file=fopen("Radium:Init.rad","r");
		if(file==NULL){
			Save_Clean("Radium:Init.rad",root);
		}else{
			fclose(file);
			Load("Radium:Init.rad");
			goto newprogram;
		}
	}

	Blt_blt(root->song->tracker_windows);

	while(!done){

		waitsig=SIGBREAKF_CTRL_C;
		waitsig|=ptask2mtasksig;
		waitsig|=debugsig;
		waitsig|=1L<<clocksig;
		waitsig|=1L<<inputsig;
		waitsig|=1L<<CPPWindowWnd->UserPort->mp_SigBit;

		temp=root->song->tracker_windows;
		window=temp->os_visual->window;
		while(temp!=NULL){
			waitsig|=1L<<window->UserPort->mp_SigBit;
			temp=NextWindow(temp);
		}
		if(BlockSelectWnd!=NULL){
			waitsig|=1L<<BlockSelectWnd->UserPort->mp_SigBit;
		}
		waitsig|=1L<<HelpWnd->UserPort->mp_SigBit;
		waitsig|=1L<<CPPWindowWnd->UserPort->mp_SigBit;


		// Nothing to do? Why not allocate some potensional useful memory and collect some garbage..

#ifndef NOPLAYER
		if( ! pc->isplaying){	// Don't want to do this while playing.
			if(lastStuffTime + 1200 < conductor->cdt_ClockTime){		// Make shure it is at least one second since last time.
//				debug("alloc and unalloc\n");
/*
				while( ((SetSignal(0L,0L)) & waitsig) == 0L){
					if( ! IncTREelementPool() ) break;
				}
*/
				while( ((SetSignal(0L,0L)) & waitsig) == 0L){
					if(GC_collect_a_little()==0) break;				//De-allocate some not used memory.
				}
				lastStuffTime=conductor->cdt_ClockTime;
			}
		}
#endif

		signal=Wait(waitsig);
		temp=root->song->tracker_windows;
		window=temp->os_visual->window;

		while(temp!=NULL){
			if(signal&1L<<window->UserPort->mp_SigBit){
				while(!done && (msg=GT_GetIMsg(window->UserPort))){
					TreatKey(msg);
					tevent.x=msg->MouseX-temp->os_visual->xpluss;
					tevent.y=msg->MouseY-temp->os_visual->ypluss;
					switch (msg->Class){
						case IDCMP_RAWKEY:
						  switch(msg->Code){
						  case 0x7a:
						    tevent.ID=TR_KEYBOARD;
						    tevent.SubID=EVENT_UPARROW;
						    EventReciever(&tevent,temp);
						    break;
						  case 0x7b:
						    tevent.ID=TR_KEYBOARD;
						    tevent.SubID=EVENT_DOWNARROW;
						    EventReciever(&tevent,temp);
						    break;
						  default:
							switch(HandleKey(msg,temp)){
								case 1: done=TRUE;
									break;
								case 2: goto newprogram;
								default:
									break;
							}
                                                  }
                                                  break;
						case IDCMP_MOUSEMOVE:
							tevent.ID=TR_MOUSEMOVE;
							if(EventReciever(&tevent,temp)==1) done=TRUE;
							break;
						case IDCMP_MOUSEBUTTONS:
							switch(msg->Code){
								case SELECTDOWN:
									tevent.ID=TR_LEFTMOUSEDOWN;
									if(EventReciever(&tevent,temp)==1) done=TRUE;
									break;
								case SELECTUP:
									tevent.ID=TR_LEFTMOUSEUP;
									if(EventReciever(&tevent,temp)==1) done=TRUE;
									break;
								case MENUUP:
									tevent.ID=TR_RIGHTMOUSEUP;
									if(EventReciever(&tevent,temp)==1) done=TRUE;
									break;
							}
						case IDCMP_NEWSIZE:
//							if(GFX_IDCMP_NEWSIZE(temp)==1) done=TRUE;
							break;
						case IDCMP_CHANGEWINDOW:
							Resize_resized(
								temp,
								window->Width - temp->os_visual->xpluss - DWIDTH,
								window->Height - temp->os_visual->ypluss - DHEIGHT,
								false
							);
							temp->os_visual->dowaitfor_IDCMP_NEWSIZE=0;
							TreatAllEvents(temp);
//							temp->x=window->LeftEdge;
//							temp->y=window->TopEdge;
							break;
					}
					GT_ReplyIMsg(msg);
				}
			}
			temp=NextWindow(temp);
		}

		if (signal & 1L<<CPPWindowWnd->UserPort->mp_SigBit){
			switch(myHandleCPPWindowIDCMP()){
				case 1: done=TRUE;
						break;
				case 2: goto newprogram;
				default:
					break;
			}
		}

		if (signal & 1L<<HelpWnd->UserPort->mp_SigBit){
			while(msg=GT_GetIMsg(HelpWnd->UserPort)){
				if(msg->Class==IDCMP_RAWKEY){
					switch(HandleKey(msg,root->song->tracker_windows)){
						case 1: done=TRUE;
								break;
						case 2: goto newprogram;
						default:
							break;
					}
				}
				GT_ReplyIMsg(msg);
			}
		}

		if (BlockSelectWnd!=NULL && (signal & 1L<<BlockSelectWnd->UserPort->mp_SigBit)){
			while(msg=GT_GetIMsg(BlockSelectWnd->UserPort)){
				if(msg->Class==IDCMP_RAWKEY){
					switch(HandleKey(msg,root->song->tracker_windows)){
						case 1: done=TRUE;
								break;
						case 2: goto newprogram;
						default:
							break;
					}
					GT_ReplyIMsg(msg);
				}else{
					if(BS_handleevents(msg)==false){
						break;
					}
				}
			}
		}

#ifdef TRACKER_DEBUG
		if(signal & debugsig) DoPdebug();
#endif

		if(signal & 1L<<inputsig){
		   Blt_markVisible(root->song->tracker_windows);
			GetMidiInput(&note,&velocity);
		   Blt_clearNotUsedVisible(root->song->tracker_windows);
		   Blt_blt(root->song->tracker_windows);
		}

		if(signal & ptask2mtasksig){
			(*Ptask2MtaskCallBack)();
		}

		if(signal & 1L<<clocksig){
			UpdateClock(root->song->tracker_windows);
		}

		if(signal & SIGBREAKF_CTRL_C) done=TRUE;

	}
/*
#endif
*/

exit:



//	fprintf(stderr,"freemem1: %d\n",AvailMem(MEMF_ANY));
//	GC_free(root);


//	GC_gcollect();


	EndProgram();

//	GC_amiga_free_all_mem();

//	fprintf(stderr,"freemem2: %d\n",AvailMem(MEMF_ANY));

	printf("\nRadium ended successfully. The following output\n");
	printf("is generated by the memorymanager. No need to\n");
	printf("worry about it, theres nothing wrong:\n\n\n");

	return 0;
}


