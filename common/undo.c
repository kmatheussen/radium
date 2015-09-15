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


#include <string.h>

#include "nsmtracker.h"
#include "disk.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "windows_proc.h"
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "undo.h"


struct UndoEntry{
  NInt windownum;
  NInt blocknum;
  NInt tracknum;
  int realline;
  bool stop_playing;

  struct Patch *current_patch;

  void *pointer;
  UndoFunction function;
};


struct Undo{
  struct Undo *prev;
  struct Undo *next;

  vector_t entries;

  // Used to find cursorpos.
  NInt windownum;
  NInt blocknum;
  NInt tracknum;
  int realline;
};


static struct Undo UndoRoot={0};
static struct Undo *CurrUndo=&UndoRoot;
static struct Undo *curr_open_undo=NULL;

static int undo_pos_at_last_saving=0;
static int num_undos=0;
static int max_num_undos=MAX_NUM_UNDOS;

static bool undo_is_open=false;

extern struct Patch *g_currpatch;

static bool currently_undoing = false;

static bool showing_star_in_filename = false;

static const wchar_t *get_filename(void){
  if(dc.filename==NULL)
    return STRING_create("Radium - New song.");
  else
    return dc.filename;
}

static void show_star(void){
  if(showing_star_in_filename==false){
    wchar_t *s = STRING_append(
                               STRING_append(STRING_create("** "),
                                             get_filename()),
                               STRING_create(" **")
                               );
    GFX_SetWindowTitle(root->song->tracker_windows,s);
    showing_star_in_filename=true;
  }
}
static void hide_star(void){
  if(showing_star_in_filename==true){
    GFX_SetWindowTitle(root->song->tracker_windows,get_filename());
    showing_star_in_filename=false;
  }
}

int Undo_num_undos_since_last_save(void){
  return num_undos - undo_pos_at_last_saving;
}

static void update_gfx(void){
  if(Undo_num_undos_since_last_save()==0)
    hide_star();
  else
    show_star();
  OS_GFX_NumUndosHaveChanged(num_undos,CurrUndo->next!=NULL,showing_star_in_filename);
}

int Undo_num_undos(void){
  return num_undos;
}

void Undo_saved_song(void){
  undo_pos_at_last_saving = num_undos;
  update_gfx();
}

void ResetUndo(void){
  if(currently_undoing){
    RError("Can not call ResetUndo from Undo()\n");
  }

  memset(&UndoRoot,0,sizeof(struct Undo));

  CurrUndo=&UndoRoot;
         
  num_undos=0;
  undo_pos_at_last_saving=0;
  update_gfx();
}

bool Undo_are_you_shure_questionmark(void){
  int num_undos = Undo_num_undos_since_last_save();

  if(num_undos>0){
    char temp[200];
    char *ret=NULL;

    sprintf(
            temp,
            "%d change%s been made to file since song was saved.\nAre you shure? (yes/no) >",
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
                        root->song->tracker_windows,
                        NULL,
                        temp
			);
      
    }
    if(!strcmp("no",ret))
      return false;
  }

  return true;
}

bool Undo_Is_Open(void){
  return undo_is_open;
}

void Undo_Open(void){
  if(currently_undoing){
    RError("Can not call Undo_Open from Undo()\n");
  }

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  struct WTracks *wtrack = wblock->wtrack;
  int realline = wblock->curr_realline;

  //printf("Undo_Open\n");

  if(undo_is_open==true)
    RError("Undo_Open: Undo_is_open==true");

  curr_open_undo = talloc(sizeof(struct Undo));
  curr_open_undo->windownum=window->l.num;
  curr_open_undo->blocknum=wblock->l.num;
  curr_open_undo->tracknum=wtrack->l.num;
  curr_open_undo->realline=realline;
    

#if 0 // Disabled. Code doesn't look right.
  if(num_undos!=0 && num_undos>max_num_undos){
    num_undos--;
    UndoRoot.next=UndoRoot.next->next;
    UndoRoot.next->prev=&UndoRoot;
  }
#endif

  undo_is_open = true;
}

bool Undo_Close(void){
  if(currently_undoing){
    RError("Can not call Undo_Close from Undo()\n");
  }

  undo_is_open = false;

  struct Undo *undo = curr_open_undo;

  if(undo->entries.num_elements > 0){
    undo->prev=CurrUndo;
    CurrUndo->next=undo;
    CurrUndo=undo;
    
    num_undos++;

    update_gfx();
    
    return true;

  } else
    return false;
}

void Undo_CancelLastUndo(void){
  R_ASSERT_RETURN_IF_FALSE(currently_undoing==false);
  R_ASSERT_RETURN_IF_FALSE(undo_is_open==false);

  CurrUndo=CurrUndo->prev;
  CurrUndo->next=NULL;

  num_undos--;

  update_gfx();
}

UndoFunction Undo_get_last_function(void){
  if(CurrUndo==&UndoRoot)
    return NULL;

  struct UndoEntry *entry=CurrUndo->entries.elements[0];
  return entry->function;
}


/***************************************************
  FUNCTION
    Insert a new undo-element.
***************************************************/
static void Undo_Add_internal(
              int windownum,
              int blocknum,
              int tracknum,
              int realline,
              void *pointer,
              UndoFunction undo_function,
              bool stop_playing_when_undoing
){
  if(currently_undoing){
    RError("Can not call Undo_Add from Undo()\n");
  }

  bool undo_was_open = undo_is_open;

  if(undo_is_open==false)
    Undo_Open();

  struct UndoEntry *entry=talloc(sizeof(struct UndoEntry));
  entry->windownum=windownum;
  entry->blocknum=blocknum;
  entry->tracknum=tracknum;
  entry->realline=realline;
  entry->current_patch = g_currpatch;
  entry->pointer=pointer;
  entry->function=undo_function;
  entry->stop_playing=stop_playing_when_undoing;

  VECTOR_push_back(&curr_open_undo->entries,entry);

  if(undo_was_open==false)
    Undo_Close();

  update_gfx();
}

void Undo_Add(
              int windownum,
              int blocknum,
              int tracknum,
              int realline,
              void *pointer,
              UndoFunction undo_function
){
  Undo_Add_internal(windownum,blocknum,tracknum,realline,pointer,undo_function,true);
}

void Undo_Add_dont_stop_playing(
              int windownum,
              int blocknum,
              int tracknum,
              int realline,
              void *pointer,
              UndoFunction undo_function
){
  Undo_Add_internal(windownum,blocknum,tracknum,realline,pointer,undo_function,false);
}

void Undo(void){
	struct Undo *undo=CurrUndo;

	if(undo==&UndoRoot) return;

currently_undoing = true;


        struct Patch *current_patch = NULL;

       {
          int i;
          for(i=undo->entries.num_elements-1 ; i>=0 ; i--){

            struct UndoEntry *entry=undo->entries.elements[i];

            if(entry->stop_playing)
              PlayStop();
 
            struct Tracker_Windows *window=ListFindElement1(&root->song->tracker_windows->l,entry->windownum);
            struct WBlocks *wblock=ListFindElement1_r0(&window->wblocks->l,entry->blocknum);
            struct WTracks *wtrack=NULL;
            current_patch = entry->current_patch;

            if(wblock!=NULL){
              window->wblock=wblock;
              if(entry->tracknum<0){
                wtrack=wblock->wtracks;
              }else{
                wtrack=ListFindElement1_r0(&wblock->wtracks->l,entry->tracknum);
              }
              if(wtrack!=NULL){
                wblock->wtrack=wtrack;
              }
              wblock->curr_realline = R_BOUNDARIES(0, entry->realline, wblock->num_reallines-1);
              window->curr_track=entry->tracknum;
            }

            entry->pointer = entry->function(window,wblock,wtrack,entry->realline,entry->pointer);

            wblock=ListFindElement1_r0(&window->wblocks->l,entry->blocknum);
            if(wblock==NULL)
              wblock=ListFindElement1_r0(&window->wblocks->l,entry->blocknum-1);
            if(wblock==NULL){
              RError("undo.c: block %d does not exist. Using block 0.",entry->blocknum-1);
              wblock=window->wblocks;
            }
          
            window->wblock = wblock;
            if(entry->tracknum<0){
              wtrack=wblock->wtracks;
            }else{
              wtrack=ListFindElement1_r0(&wblock->wtracks->l,entry->tracknum);
            }
            wblock->wtrack=wtrack;
            wblock->curr_realline = R_BOUNDARIES(0, entry->realline, wblock->num_reallines-1);
            window->curr_track=entry->tracknum;
          }
       }

       VECTOR_reverse(&undo->entries);

       CurrUndo=undo->prev;

       num_undos--;

       {
         struct Tracker_Windows *window=ListFindElement1(&root->song->tracker_windows->l,undo->windownum);
         struct WBlocks *wblock=ListFindElement1_r0(&window->wblocks->l,undo->blocknum);
         struct WTracks *wtrack;

         if(wblock==NULL)
           wblock=window->wblocks;

         if(undo->tracknum<0){
           wtrack=wblock->wtracks;
         }else{
           wtrack=ListFindElement1_r0(&wblock->wtracks->l,undo->tracknum);
         }

         if(wtrack==NULL)
           wtrack=wblock->wtracks;
           
         wblock->wtrack=wtrack;
         
         wblock->curr_realline = R_BOUNDARIES(0, undo->realline, wblock->num_reallines-1);         
         window->curr_track=undo->tracknum;

         SelectWBlock(
                      window,
                      wblock
                      );

         if(current_patch!=NULL){
           GFX_update_instrument_patch_gui(current_patch);
           GFX_update_instrument_widget(current_patch);
         }

         GE_set_curr_realline(wblock->curr_realline);
       }
currently_undoing = false;

 update_gfx();
}


void Redo(void){
  //printf("CurrUndo: %p, CurrUndo->next: %p, UndoRoot: %p, UndoRoot->next: %p\n",CurrUndo,CurrUndo->next,&UndoRoot,UndoRoot.next);

	if(CurrUndo->next==NULL) return;

	CurrUndo=CurrUndo->next;
	Undo();
	CurrUndo=CurrUndo->next;

	num_undos+=2;

        update_gfx();
}

void SetMaxUndos(struct Tracker_Windows *window){
	int newmax=0;
	char seltext[50];

	sprintf(seltext,"Max Undos (0=unlimited) (now: %d): ",max_num_undos);
	while(newmax==1 || newmax==2)
		newmax=GFX_GetInteger(window,NULL,seltext,0,2000000);
	if(newmax==-1) return;

	max_num_undos=newmax;

        GFX_Message(NULL, "The max number of undoes variables is ignored. Undo is unlimited.");
}





