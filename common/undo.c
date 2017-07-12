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
#include "seqtrack_proc.h"

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

  const char *info;
  source_pos_t source_pos;
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

int64_t g_curr_undo_generation = 0;  // used by autobackup

static int undo_is_open=0;

extern struct Patch *g_currpatch;

static bool currently_undoing = false;

static bool showing_star_in_filename = false;

static int ignore_undo_operations_level = 0;

static inline bool ignore(void){
  return ignore_undo_operations_level > 0;
}

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

bool Undo_Is_Currently_Undoing(void){
  return currently_undoing;
}

bool Undo_Is_Currently_Ignoring(void){
  return ignore();
}

void ResetUndo(void){
  if (ignore()) {
    if (ignore()==false)
      RError("ignore_undo_operations_level < 0: %d",ignore_undo_operations_level);
    ignore_undo_operations_level = 0;
  }

  if(currently_undoing){
    RError("Can not call ResetUndo from Undo()\n");
    return;
  }

  memset(&UndoRoot,0,sizeof(struct Undo));

  CurrUndo=&UndoRoot;
  
  //printf("        UNDO ResetUndo(). num_undos=0\n");
  num_undos=0;
  undo_pos_at_last_saving=0;
  update_gfx();
}

bool Undo_are_you_sure_questionmark(void){
  int num_undos = Undo_num_undos_since_last_save();

  if(num_undos>0){
    char temp[200];
    char *ret=NULL;

    sprintf(
            temp,
            "%d change%s been made to file since song was saved.\nAre you sure? (yes/no) > ",
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

static bool g_is_adding_undo = false;
static source_pos_t g_curr_source_pos;

bool Undo_Currently_Adding_Undo(void){
  return g_is_adding_undo;
}
void Undo_Start_Adding_Undo(source_pos_t source_pos){
  memcpy(&g_curr_source_pos, &source_pos, sizeof(source_pos_t));
  R_ASSERT(g_is_adding_undo==false);
  g_is_adding_undo = true;
}

void Undo_End_Adding_Undo(void){
  R_ASSERT(g_is_adding_undo==true);
  g_is_adding_undo = false;
}

bool Undo_Is_Open(void){
  return undo_is_open > 0;
}

void Das_Undo_Open_rec(void){
  //printf("        UNDO OPEN\n");

  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  if (ignore()) return;

  if(currently_undoing){
    //RError("Can not call Undo_Open from Undo()\n");
    return;
  }

  if (undo_is_open==0){
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    struct WTracks *wtrack = wblock->wtrack;
    int realline = wblock->curr_realline;
    
    //printf("Undo_Open\n");
    
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
    
  }

  undo_is_open++;
}

void Das_Undo_Open(void){
  if (ignore()) return;

  if(Undo_Is_Open())
    RError("Undo_Open: Undo_is_open==true: %d", undo_is_open);

  Das_Undo_Open_rec();
}

// Returns true if we created another undo entry.
bool Das_Undo_Close(void){

  R_ASSERT_RETURN_IF_FALSE2(THREADING_is_main_thread(), false);

  if (ignore()) return false;

  //EVENTLOG_add_event(talloc_format("Undo_Close. currently_undoing: %d. undo_is_open: %d", currently_undoing, Undo_Is_Open()));
  
  //printf("        UNDO CLOSE\n");


  if(currently_undoing){
    //RError("Can not call Undo_Close from Undo()\n");    
    return false;
  }

  //if (undo_is_open > 1)
  //  RError("undo_is_open > 1: %d", undo_is_open);

  if (undo_is_open==0){
    RError("undo_is_open==0");
    return false;
  }
  
  undo_is_open--;

  if (undo_is_open > 0)
    return false;

  if (undo_is_open < 0){
    RError("undo_is_open < 0: %d", undo_is_open);
    undo_is_open = 0;
  }

  struct Undo *undo = curr_open_undo;

  if(undo->entries.num_elements > 0){
    undo->prev=CurrUndo;
    CurrUndo->next=undo;
    CurrUndo=undo;
    
    num_undos++;
    //printf("        UNDO Undo_Close(). num_undos++: %d\n", num_undos);

    update_gfx();
    
    return true;

  } else
    return false;
}

void Das_Undo_ReopenLast(void){
  if (ignore())
    return;
  
  R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open()==false);
  R_ASSERT_RETURN_IF_FALSE(currently_undoing==false);
  R_ASSERT_RETURN_IF_FALSE(CurrUndo->entries.num_elements > 0);

  curr_open_undo = CurrUndo;
  
  CurrUndo=CurrUndo->prev;
  CurrUndo->next=NULL;

  undo_is_open++;
  num_undos--;
  //printf("        UNDO Reopenlast. num_undos--: %d\n", num_undos);
}

void Das_Undo_CancelLastUndo(void){
  if (ignore()) return;

  R_ASSERT_RETURN_IF_FALSE(currently_undoing==false);
  R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open()==false);
  R_ASSERT_RETURN_IF_FALSE(num_undos>0);
  R_ASSERT_RETURN_IF_FALSE(CurrUndo!=NULL);
  R_ASSERT_RETURN_IF_FALSE(CurrUndo->prev!=NULL);

  CurrUndo=CurrUndo->prev;
  CurrUndo->next=NULL;

  num_undos--;
  //printf("        UNDO CancelLast. num_undos++: %d\n", num_undos);

  update_gfx();
}

UndoFunction Undo_get_last_function(void){
  if(CurrUndo==&UndoRoot)
    return NULL;

  struct UndoEntry *entry=CurrUndo->entries.elements[0];
  return entry->function;
}

static char *get_entry_string(struct UndoEntry *entry){
  return talloc_format("  Add Undo. %s. %s:%d. Function: %s.%s",
                       entry->info,
                       entry->source_pos.filename,
                       entry->source_pos.linenum,
                       entry->source_pos.function_name,
                       entry->source_pos.extra_info==NULL?"":talloc_format(" (%s)",entry->source_pos.extra_info)
                       );
}

vector_t Undo_get_history(void){
  vector_t ret = {0};

  struct Undo *undo = CurrUndo;
  int pos = 0;

  if (Undo_Is_Open()){
    VECTOR_FOR_EACH(struct UndoEntry* entry, &curr_open_undo->entries){
      VECTOR_push_back(&ret, talloc_format("curr: %s", get_entry_string(entry)));
    }END_VECTOR_FOR_EACH;
  }
  
  while(undo != NULL){
    VECTOR_FOR_EACH(struct UndoEntry* entry, &undo->entries){
      VECTOR_push_back(&ret, talloc_format("%d: %s", pos, get_entry_string(entry)));
    }END_VECTOR_FOR_EACH;
    undo = undo->prev;
    pos++;
  }

  return ret;
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
              bool stop_playing_when_undoing,
              const char *info
){
  if (ignore()) return;

  if(currently_undoing){
    RError("Can not call Undo_Add from Undo()\n",info);
    return;
  }

  Das_Undo_Open_rec();{

    struct UndoEntry *entry=talloc(sizeof(struct UndoEntry));
    entry->windownum=windownum;
    entry->blocknum=blocknum;
    entry->tracknum=tracknum;
    entry->realline=realline;
    entry->current_patch = g_currpatch;
    entry->pointer=pointer;
    entry->function=undo_function;
    entry->stop_playing=stop_playing_when_undoing;
    entry->info = talloc_strdup(info);
    memcpy(&entry->source_pos, &g_curr_source_pos, sizeof(source_pos_t));
      
    VECTOR_push_back(&curr_open_undo->entries,entry);

    EVENTLOG_add_event(get_entry_string(entry));

  }Das_Undo_Close();

  g_curr_undo_generation++;
}

void Undo_Add(
              int windownum,
              int blocknum,
              int tracknum,
              int realline,
              void *pointer,
              UndoFunction undo_function,
              const char *info
){
  Undo_Add_internal(windownum,blocknum,tracknum,realline,pointer,undo_function,true,info);
}

void Undo_Add_dont_stop_playing(
              int windownum,
              int blocknum,
              int tracknum,
              int realline,
              void *pointer,
              UndoFunction undo_function,
              const char *info
){
  Undo_Add_internal(windownum,blocknum,tracknum,realline,pointer,undo_function,false,info);
}

void Undo_start_ignoring_undo_operations(void){
  R_ASSERT(currently_undoing==false);

  ignore_undo_operations_level++;
}

void Undo_stop_ignoring_undo_operations(void){
  R_ASSERT(currently_undoing==false);

  R_ASSERT_RETURN_IF_FALSE(ignore_undo_operations_level > 0);

  ignore_undo_operations_level--;
}

void Undo(void){
  R_ASSERT(ignore()==false);

	struct Undo *undo=CurrUndo;

	if(undo==&UndoRoot) return;

        
currently_undoing = true;


        struct Patch *current_patch = NULL;

        EVENTLOG_add_event("     Undoing Start");
                
       {
         int num_pausing = 0;
         
          int i;
          for(i=undo->entries.num_elements-1 ; i>=0 ; i--){
            //for(i=0 ; i < undo->entries.num_elements; i++){

            struct UndoEntry *entry=undo->entries.elements[i];

            EVENTLOG_add_event(talloc_format("        Undoing %s",get_entry_string(entry)));
                    
            if(entry->stop_playing) {
              PC_Pause();
              num_pausing++;
            }
 
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
              ATOMIC_WRITE(window->curr_track, entry->tracknum);
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
            ATOMIC_WRITE(window->curr_track, entry->tracknum);
          }

          for(int i=0;i<num_pausing;i++)
            PC_StopPause(NULL);
       }

       EVENTLOG_add_event("     Undoing Finished");
               
       VECTOR_reverse(&undo->entries);

       CurrUndo=undo->prev;

       num_undos--;
       //printf("        UNDO Undo(). num_undos--: %d\n", num_undos);

       //if (!is_playing())  // <- Not sure if this is safe.
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
         ATOMIC_WRITE(window->curr_track, undo->tracknum);

         SelectWBlock(
                      window,
                      wblock
                      );

         if(current_patch!=NULL){
           GFX_update_instrument_patch_gui(current_patch);
           GFX_update_instrument_widget(current_patch);
         }

         GE_set_curr_realline(wblock->curr_realline);

         // Unnecessary, and can cause a small flicker.
         //SEQUENCER_update();
         
       }
       
currently_undoing = false;

 update_gfx();
}


void Redo(void){
  //printf("CurrUndo: %p, CurrUndo->next: %p, UndoRoot: %p, UndoRoot->next: %p\n",CurrUndo,CurrUndo->next,&UndoRoot,UndoRoot.next);

  R_ASSERT(ignore()==false);

	if(CurrUndo->next==NULL) return;

        EVENTLOG_add_event("   Redoing Start");
                           
	CurrUndo=CurrUndo->next;
	Undo();
	CurrUndo=CurrUndo->next;

        EVENTLOG_add_event("   Redoing Finished");
                
	num_undos+=2;
        //printf("        UNDO Redo(). num_undos--: %d\n", num_undos);

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





