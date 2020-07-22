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
#include "sequencer_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../OpenGL/Widget_proc.h"

#include "../audio/SampleReader_proc.h"

#include "../api/api_undo_proc.h"
#include "../api/api_proc.h"


#include "undo.h"

namespace{
  
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

struct UnavailableCallback{
  UndoUnavailableCallback callback;
  void *data;
  int downcount;
};
}

static vector_t g_unavailable_callbacks_to_call = {};

namespace radium{
struct Undo{
  radium::Undo *prev;
  radium::Undo *next;
  radium::Undo *last_next; // Used when cancelling last undo.

  vector_t entries;
  int generation;
  vector_t unavailable_callbacks;
  
  // Used to find cursorpos.
  NInt windownum;
  NInt blocknum;
  NInt tracknum;
  int realline;

  bool isranged;
  NInt rangex1;
  NInt rangex2;
  Place rangey1;
  Place rangey2;

  int curr_track;
  int curr_track_sub;
  int curr_othertrack_sub;
};
}

static radium::Undo UndoRoot={};
static radium::Undo *CurrUndo=&UndoRoot;
static radium::Undo *curr_open_undo=NULL;

static int undo_pos_at_last_saving=0;
static int num_undos=0;
static int max_num_undos=MAX_NUM_UNDOS;

int64_t g_curr_undo_generation = 0;  // used by autobackup

static int undo_is_open=0;

static bool currently_undoing = false;

static bool showing_star_in_filename = false;

static int ignore_undo_operations_level = 0;

static inline bool ignore(void){
  return ignore_undo_operations_level > 0;
}

static const wchar_t *get_filename(void){
  if(isIllegalFilepath(dc.filename))
    return STRING_create("Radium - New song.");
  else
    return dc.filename.id;
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
    API_call_me_when_dirty_status_has_changed(true);
  }
}
static void hide_star(void){
  if(showing_star_in_filename==true){
    GFX_SetWindowTitle(root->song->tracker_windows,get_filename());
    showing_star_in_filename=false;
    API_call_me_when_dirty_status_has_changed(false);
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

static void run_undo_unavailable_callbacks(const radium::Undo *undo){
  while(undo != NULL){

    VECTOR_FOR_EACH(struct UnavailableCallback *, unavailable_callback, &undo->unavailable_callbacks){

      VECTOR_push_back(&g_unavailable_callbacks_to_call, unavailable_callback);
      
    }END_VECTOR_FOR_EACH;
    
    undo = undo->next;
  }

  vector_t next_callbacks = {};
  
  VECTOR_FOR_EACH(struct UnavailableCallback *, unavailable_callback, &g_unavailable_callbacks_to_call){
    
    if (unavailable_callback->downcount <= 0 ){
      
      R_ASSERT(unavailable_callback->downcount==0);      
      unavailable_callback->callback(unavailable_callback->data);
      
    } else {

      unavailable_callback->downcount--;
    
      VECTOR_push_back(&next_callbacks, unavailable_callback);
      
    }
    
  }END_VECTOR_FOR_EACH;

  g_unavailable_callbacks_to_call = next_callbacks;
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

  run_undo_unavailable_callbacks(UndoRoot.next);
  
  memset(&UndoRoot,0,sizeof(radium::Undo));

  CurrUndo=&UndoRoot;
  
  //printf("        UNDO ResetUndo(). num_undos=0\n");
  num_undos=0;
  undo_pos_at_last_saving=0;
  update_gfx();
}


bool Undo_NSM_are_you_sure_questionmark(void){
  int num_undos = Undo_num_undos_since_last_save();

  bool boolret = true;

  ReqType reqtype = NULL;

  if (num_undos>0) {
    
    reqtype = GFX_OpenReq(root->song->tracker_windows, 200, 100, "");
    
    if(num_undos>0){
      char temp[200];
      const char *ret=NULL;
      
      sprintf(
              temp,
              "%d change%s been made to file since song was saved.\n"
              "<p>\n"
              "<center><u>Your current project will be saved automatically by NSM if you answer yes now.</u></center>\n"
              "<p>\n"
              "Are you sure? (yes/no) >",
              num_undos,
              num_undos>1 ? "s have" : " has"
              );
      do{
        ret=GFX_GetString(
                          root->song->tracker_windows,
                          reqtype,
                          temp,
                          true
                          );
      }while(
             ret!=NULL &&
             strcmp("yes",ret) &&
             strcmp("no",ret)
             );
      
      if(ret==NULL || !strcmp("no",ret)){
        boolret = false;
        goto exit;
      }
    }
  }
  
 exit:
  
  if (reqtype != NULL)
    GFX_CloseReq(root->song->tracker_windows, reqtype);
  
  return boolret;
}


bool Undo_are_you_sure_questionmark(void){
  EVENTLOG_add_event("Undo_are_you_sure_questionmark 1");
  
  int num_undos = Undo_num_undos_since_last_save();

  EVENTLOG_add_event("Undo_are_you_sure_questionmark 2");
  
  const vector_t deletable_audiofiles = SAMPLEREADER_get_all_deletable_filenames();
  
  EVENTLOG_add_event("Undo_are_you_sure_questionmark 3");
  
  const bool num_deletable_audio_files = deletable_audiofiles.num_elements;

  EVENTLOG_add_event("Undo_are_you_sure_questionmark 4");
  
  const bool has_deletable_audio_files = num_deletable_audio_files > 0;

  bool boolret = true;

  ReqType reqtype = NULL;

  if (num_undos>0) {
    
    reqtype = GFX_OpenReq(root->song->tracker_windows, 200, 100, "");
    
    if(num_undos>0){
      char temp[200];
      const char *ret=NULL;
      
      sprintf(
              temp,
              "%d change%s been made to file since song was saved.\nAre you sure? (yes/no) > ",
              num_undos,
              num_undos>1 ? "s have" : " has"
              );
      do{
        ret=GFX_GetString(
                          root->song->tracker_windows,
                          reqtype,
                          temp,
                          true
                          );
      }while(
             ret!=NULL &&
             strcmp("yes",ret) &&
             strcmp("no",ret)
             );
      
      if(ret==NULL || !strcmp("no",ret)){
        boolret = false;
        goto exit;
      }
    }
  }
  
  if (has_deletable_audio_files) {

    enum WhatToDoWithDeletableFileWhenLoadingOrQuitting wtt;

    switch(unusedRecordingTakesTreatment()){
        
      case URTT_NEVER:
        wtt = WTT_KEEP;
        break;

      case URTT_ALWAYS:
        wtt = WTT_DELETE;
        break;

      case URTT_ASK:
        {
          if (reqtype==NULL)
            reqtype = GFX_OpenReq(root->song->tracker_windows, 200, 100, "");
          
          char temp[200];
          const char *ret=NULL;
          sprintf(
                  temp,
                  "There %s %d unused/unsaved recording take%s. Do you want to delete %s file%s? (yes/no) > ",
                  num_deletable_audio_files==1 ? "is" : "are",
                  num_deletable_audio_files,
                  num_deletable_audio_files==1 ? "" : "s",
                  num_deletable_audio_files==1 ? "that" : "those",
                  num_deletable_audio_files==1 ? "" : "s"
                  );
          do{
            ret=GFX_GetString(
                              root->song->tracker_windows,
                              reqtype,
                              temp,
                              true
                              );
          }while(
                 ret!=NULL &&
                 strcmp("yes",ret) &&
                 strcmp("no",ret)
                 );
          
          if (ret==NULL){            
            boolret = false;
            goto exit;
          }
          
          if (!strcmp("yes", ret))
            wtt = WTT_DELETE;
          else
            wtt = WTT_KEEP;
        }
        break;
    
      default:
        R_ASSERT(false);
        wtt = WTT_KEEP;
        break;
    }
      
    VECTOR_FOR_EACH(const wchar_t *, filename, &deletable_audiofiles){      
      SAMPLEREADER_mark_what_to_do_with_deletable_file_when_loading_or_quitting(make_filepath(filename), wtt);
    }END_VECTOR_FOR_EACH;
    
  }


 exit:
  
  if (reqtype != NULL)
    GFX_CloseReq(root->song->tracker_windows, reqtype);
  
  return boolret;
}



static bool g_is_adding_undo = false;
static source_pos_t g_curr_source_pos = {};

bool Undo_Currently_Adding_Undo(void){
  return g_is_adding_undo;
}
void Undo_Start_Adding_Undo(source_pos_t source_pos){
  memcpy(static_cast<void*>(&g_curr_source_pos), &source_pos, sizeof(source_pos_t));
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
    
    curr_open_undo             = (radium::Undo*)talloc(sizeof(radium::Undo));
    curr_open_undo->generation = g_curr_undo_generation;
    curr_open_undo->windownum  = window->l.num;
    curr_open_undo->blocknum   = wblock->l.num;
    curr_open_undo->tracknum   = wtrack->l.num;
    curr_open_undo->realline   = realline;
    
    curr_open_undo->isranged = wblock->isranged;
    curr_open_undo->rangex1 = wblock->rangex1;
    curr_open_undo->rangex2 = wblock->rangex2;
    curr_open_undo->rangey1 = wblock->rangey1;
    curr_open_undo->rangey2 = wblock->rangey2;
      
    curr_open_undo->curr_track = window->curr_track;
    curr_open_undo->curr_track_sub = window->curr_track_sub;
    curr_open_undo->curr_othertrack_sub = window->curr_othertrack_sub;
    
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

  radium::Undo *undo = curr_open_undo;

  if(undo->entries.num_elements > 0){

    run_undo_unavailable_callbacks(CurrUndo->next);

    if (CurrUndo->prev != NULL)
      CurrUndo->prev->last_next = NULL; // Free up memory. Not needed anymore since last_next is only used by CancelLastUndo which can not be called two times in a row.
    
    undo->prev=CurrUndo;
    CurrUndo->last_next = CurrUndo->next;
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
  CurrUndo->next=CurrUndo->last_next;

  num_undos--;
  //printf("        UNDO CancelLast. num_undos++: %d\n", num_undos);

  update_gfx();
}

UndoFunction Undo_get_last_function(void){
  if(CurrUndo==&UndoRoot)
    return NULL;

  struct UndoEntry *entry=(struct UndoEntry *)CurrUndo->entries.elements[0];
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
  vector_t ret = {};

  radium::Undo *undo = CurrUndo;
  int pos = 0;

  if (Undo_Is_Open()){
    VECTOR_FOR_EACH(struct UndoEntry*,  entry, &curr_open_undo->entries){
      VECTOR_push_back(&ret, talloc_format("curr: %s", get_entry_string(entry)));
    }END_VECTOR_FOR_EACH;
  }
  
  while(undo != NULL){
    VECTOR_FOR_EACH(struct UndoEntry*, entry, &undo->entries){
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
    RError("Can not call Undo_Add from Undo(). (%s)\n", info);
    return;
  }

  Das_Undo_Open_rec();{

    struct UndoEntry *entry = (struct UndoEntry *)talloc(sizeof(struct UndoEntry));
    
    entry->windownum        = windownum;
    entry->blocknum         = blocknum;
    entry->tracknum         = tracknum;
    entry->realline         = realline;
    entry->current_patch    = PATCH_get_current();
    entry->pointer          = pointer;
    entry->function         = undo_function;
    entry->stop_playing     = stop_playing_when_undoing;
    entry->info             = talloc_strdup(info);    
    memcpy(static_cast<void*>(&entry->source_pos), &g_curr_source_pos, sizeof(source_pos_t));
  
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

static void undo_internal(void){
  R_ASSERT(ignore()==false);

	radium::Undo *undo=CurrUndo;

	if(undo==&UndoRoot) return;

        
currently_undoing = true;


        struct Patch *current_patch = NULL;

        EVENTLOG_add_event("     Undoing Start");
                
       {
         int num_pausing = 0;
         
          int i;
          for(i=undo->entries.num_elements-1 ; i>=0 ; i--){
            //for(i=0 ; i < undo->entries.num_elements; i++){

            struct UndoEntry *entry=(struct UndoEntry *)undo->entries.elements[i];

            EVENTLOG_add_event(talloc_format("        Undoing %s",get_entry_string(entry)));
                    
            if(entry->stop_playing) {
              PC_Pause();
              num_pausing++;
            }
 
            struct Tracker_Windows *window=(struct Tracker_Windows *)ListFindElement1(&root->song->tracker_windows->l,entry->windownum);
            struct WBlocks *wblock=(struct WBlocks *)ListFindElement1_r0(&window->wblocks->l,entry->blocknum);
            struct WTracks *wtrack=NULL;
            current_patch = entry->current_patch;

            if(wblock!=NULL){
              window->wblock=wblock;
              if(entry->tracknum<0){
                wtrack=wblock->wtracks;
              }else{
                wtrack=(struct WTracks *)ListFindElement1_r0(&wblock->wtracks->l,entry->tracknum);
              }
              if(wtrack!=NULL){
                ATOMIC_WRITE(wblock->wtrack, wtrack);
              }
              wblock->curr_realline = R_BOUNDARIES(0, entry->realline, wblock->num_reallines-1);
              ATOMIC_WRITE(window->curr_track, entry->tracknum);
            }

            entry->pointer = entry->function(window,wblock,wtrack,entry->realline,entry->pointer);

            wblock=(struct WBlocks *)ListFindElement1_r0(&window->wblocks->l,entry->blocknum);
            if(wblock==NULL)
              wblock=(struct WBlocks *)ListFindElement1_r0(&window->wblocks->l,entry->blocknum-1);
            if(wblock==NULL){
              RError("undo.c: block %d does not exist. Using block 0.",entry->blocknum-1);
              wblock=window->wblocks;
            }
          
            window->wblock = wblock;
            if(entry->tracknum<0){
              wtrack=wblock->wtracks;
            }else{
              wtrack=(struct WTracks*)ListFindElement1_r0(&wblock->wtracks->l,entry->tracknum);
            }
            
            ATOMIC_WRITE(wblock->wtrack, wtrack);
            
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

         struct Tracker_Windows *window=(struct Tracker_Windows *)ListFindElement1(&root->song->tracker_windows->l,undo->windownum);
         struct WBlocks *wblock=(struct WBlocks *)ListFindElement1_r0(&window->wblocks->l,undo->blocknum);
         struct WTracks *wtrack;

         if(wblock==NULL)
           wblock=window->wblocks;

         if(undo->tracknum<0){
           wtrack=wblock->wtracks;
         }else{
           wtrack=(struct WTracks *)ListFindElement1_r0(&wblock->wtracks->l,undo->tracknum);
         }

         if(wtrack==NULL)
           wtrack=wblock->wtracks;
           
         ATOMIC_WRITE(wblock->wtrack, wtrack);
         
         wblock->curr_realline = R_BOUNDARIES(0, undo->realline, wblock->num_reallines-1);         
         ATOMIC_WRITE(window->curr_track, undo->tracknum);
         ATOMIC_WRITE(window->curr_track, undo->curr_track);
         window->curr_track_sub = undo->curr_track_sub;
         window->curr_othertrack_sub = undo->curr_othertrack_sub;

         bool update_range = wblock->isranged != undo->isranged;

         if (!update_range && undo->isranged) {
           update_range = false
             || wblock->rangex1 != undo->rangex1
             || wblock->rangex1 != undo->rangex2
             || p_NOT_Equal(wblock->rangey1, undo->rangey1)
             || p_NOT_Equal(wblock->rangey2, undo->rangey2);
         }
         
         SelectWBlock(
                      window,
                      wblock,
                      false // Set to false to prevent curr track / sub track to be overridden, for instance if current track is a timing track. Don't know why it was set to true originally.
                      );

         if (update_range){
           if (undo->isranged)
             setRange(undo->rangey1, undo->rangey2, undo->rangex1, undo->rangex2+1, wblock->l.num, window->l.num);
           else
             cancelRange(window->l.num);
         }
         
         if(current_patch!=NULL){
           GFX_update_instrument_patch_gui(current_patch);
           GFX_update_instrument_widget(current_patch);
         }

         window->must_redraw = true; // We do this instead of calling GE_set_curr_realline. GE_set_curr_realline can also cause flickering if editor is redrawn at the same time.
         //GE_set_curr_realline(wblock->curr_realline);

         // Unnecessary, and can cause a small flicker.
         //SEQUENCER_update();
         
       }
       
currently_undoing = false;

 update_gfx();
}

void Undo(void){
  undo_internal();
  API_call_me_right_after_undoing_or_redoing();
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

        API_call_me_right_after_undoing_or_redoing();
}

void SetMaxUndos(struct Tracker_Windows *window){
	int newmax=0;
	char seltext[50];

	sprintf(seltext,"Max Undos (0=unlimited) (now: %d): ",max_num_undos);
	while(newmax==1 || newmax==2)
          newmax=GFX_GetInteger(window,NULL,seltext,0,2000000,true);
	if(newmax==-1) return;

	max_num_undos=newmax;

        GFX_Message2(NULL, true, "The max number of undoes variables is ignored. Undo is unlimited.");
}

void UNDO_add_callback_when_curr_entry_becomes_unavailable(UndoUnavailableCallback callback, void *data, int delay){
  R_ASSERT_RETURN_IF_FALSE(currently_undoing==false);
  
  struct UnavailableCallback *unavailable_callback = (struct UnavailableCallback *)talloc(sizeof(struct UnavailableCallback));
  
  unavailable_callback->callback = callback;
  unavailable_callback->data = data;
  unavailable_callback->downcount = delay;

  radium::Undo *undo = curr_open_undo != NULL ? curr_open_undo : CurrUndo;
  
  VECTOR_push_back(&undo->unavailable_callbacks, unavailable_callback);  
  
  //VECTOR_push_back(&next_unavailable_callbacks, unavailable_callback);  
};

namespace{
  struct UndoFunctions{
    bool must_undo;
    std::function<void(void)> redo;
    std::function<void(void)> undo;

    UndoFunctions(std::function<void(void)> &redo, std::function<void(void)> &undo)
      : redo(redo)
      , undo(undo)
    {}
  };
}

static void *Undo_Do_Functions(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  UndoFunctions *functions = static_cast<UndoFunctions*>(pointer);

  if (functions->must_undo)
    functions->undo();
  else
    functions->redo();
  
  functions->must_undo = !functions->must_undo;
  
  return functions;
}

static void Undo_free_functions(void *data){
  //printf("          Freeing %p\n", data);
  delete static_cast<UndoFunctions*>(data);
}

void UNDO_functions(const char *name, std::function<void(void)> redo, std::function<void(void)> undo){
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    UndoFunctions *functions = new UndoFunctions(redo, undo);
    //printf("          Allocating %p\n", functions);
    
    Undo_Add_dont_stop_playing(
                               window->l.num,
                               wblock->l.num,
                               wblock->wtrack->l.num,
                               wblock->curr_realline,
                               functions,
                               Undo_Do_Functions,
                               talloc_strdup(name)
                               );

    UNDO_add_callback_when_curr_entry_becomes_unavailable(Undo_free_functions, functions, 0);
      
    redo();
    functions->must_undo = true;
}
