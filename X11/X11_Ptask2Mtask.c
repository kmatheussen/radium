/* Copyright 2003 Kjetil S. Matheussen

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


#include <pthread.h>

#include "../common/nsmtracker.h"
#include "../common/PEQ_clock_proc.h"

// Note, this file is no longer used from Qt!


#ifdef GUIISQT
extern void Qt_Ptask2Mtask(void);
#else
#  include "X11.h"
#endif

#include "X11_Ptask2Mtask_proc.h"

#include "../common/OS_Ptask2Mtask_proc.h"



#if 0

A:
Send Thread 1: User event GUI process. Kommer fra main() og er ikke en thread.

B:
Send Thread 2: er PlayerThreaden og sender pthread_kill

C:
Receive Thread: Player GUI thread.


A->X11
B->C
C->X11

A og C er beskyttet av mutex_lock/unlock()
A Sjekker X11 manuellt med select for å unngå å blocke på XNextEvent.



*********************


X11 select code:
----------------

int fd=ConnectionNumber(x11_display);
fd_set fdset;

FD_ZERO(&fdset);
FD_SET(fd,&fdset);

XFlush(x11_display);

while((nfd=select(fd+1,&fdset,NULL,NULL,NULL)==-1)){
  if(errno!=EINTR)
    break;
  pthread_kill(thread);
}

while(XPending(display)) {
  XNextEvent(x11_display, &event);
  YourHandleEvent(&event);
}

#endif


static pthread_t guiplayerthread={0};
static pthread_cond_t cond={{0}};
static pthread_mutex_t mutex={{0}};
static pthread_mutex_t guimutex={{0}};

static bool goingtoend=false;

extern struct Root *root;


extern void P2MUpdateSongPosCallBack(void);


static void *PlayerGuiThread(void *arg){
  //  static int ant=0;
  for(;;){
    pthread_cond_wait(&cond,&mutex);
    if(goingtoend==true) break;
#ifdef GUIISQT
    abort(); // Not supposed to be here.
    Qt_Ptask2Mtask(); // Qt_Ptask2Mtask triggers the Qt main loop, so we don't have to use the guilock.
#else
    pthread_mutex_lock(&guimutex);
    P2MUpdateSongPosCallBack();
    UpdateClock(root->song->tracker_windows);
    XFlush(x11_display);
    pthread_mutex_unlock(&guimutex);
#endif
  }
  return NULL;
}



// Called from main-thread.
bool StartGuiThread(void){
  pthread_mutex_init(&guimutex,NULL);  
  pthread_mutex_init(&mutex,NULL);
  pthread_cond_init(&cond,NULL);
  if(pthread_create(&guiplayerthread,NULL,PlayerGuiThread,NULL)!=0){
    fprintf(stderr,"Could not start a thread.\n");
    return false;
  }
  return true;
}


void EndGuiThread(void){
  goingtoend=true;
  pthread_cond_broadcast(&cond);
  pthread_join(guiplayerthread,NULL);
}


void lockGUI(void){
  pthread_mutex_lock(&guimutex);
}

void unlockGUI(void){
  pthread_mutex_unlock(&guimutex);
}


// Called from player-thread.

void Ptask2Mtask(void){
#ifdef GUIISQT
    Qt_Ptask2Mtask(); // Qt_Ptask2Mtask triggers the Qt main loop, so we don't have to use the guilock.
#else
  pthread_cond_broadcast(&cond);
#endif
}


