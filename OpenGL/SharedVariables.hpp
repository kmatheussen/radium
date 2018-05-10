
#include "Widget_proc.h"

#include "../common/time_proc.h"
#include "../common/vector_proc.h"
#include "../common/threading.h"
#include "../common/wblocks_proc.h"
#include "../audio/Mixer_proc.h"


extern struct Root *root;

// Can't read data from 'root' from the OpenGL thread, because we risk reading from NULL now and then.
// So we store everything we need into this structure, and send it over to the OpenGL thread.
struct SharedVariables{
  int top_realline;
  int num_reallines;
  int curr_realline;
  int fontheight;

  int num_lines;

  double reltempo;
  STime block_duration;

  float scrollbar_height;
  float scrollbar_scroller_height;

  int wtracks_scissor_x1, wtracks_scissor_x2;


  // These 5 pointers are also stored in g_shared_variables_gc_storage, so that they can't be garbage collected while only being used in the OpenGL thread.
  const struct Root *root;  
  const struct Blocks *block;
  const struct Blocks *curr_playing_block;
  const struct STimes *times;
  const struct LocalZooms **reallines;

  double seqblock_stretch;
  
  bool block_is_visible;
  
  SharedVariables(bool block_is_visible)
    : block_is_visible(block_is_visible)
  {}

  ~SharedVariables();
};


#ifdef OPENGL_GFXELEMENTS_CPP

static radium::Mutex vector_mutex;
static vector_t g_shared_variables_gc_storage = {}; // Here we store stuff used in SharedVariables that should not be garbage collected

// Called from T2 or main thread
SharedVariables::~SharedVariables(){
  {
    radium::ScopedMutex locker(vector_mutex);

    bool is_main_thread = THREADING_is_main_thread();
    
    if(!is_main_thread)
      Threadsafe_GC_disable();

    {
      VECTOR_remove(&g_shared_variables_gc_storage, this->root);
      VECTOR_remove(&g_shared_variables_gc_storage, this->times);
      //VECTOR_remove(&g_shared_variables_gc_storage, this->times_with_global_swings);
      VECTOR_remove(&g_shared_variables_gc_storage, this->block);
      VECTOR_remove(&g_shared_variables_gc_storage, this->curr_playing_block);

      VECTOR_remove(&g_shared_variables_gc_storage, this->reallines);      
    }

    if(!is_main_thread)
      Threadsafe_GC_enable();
  }
}

// Called from main thread
static void GE_fill_in_shared_variables(SharedVariables *sv){

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  struct Blocks *block = wblock->block;

  sv->root          = root;
  sv->top_realline  = wblock->top_realline;
  sv->curr_realline = wblock->curr_realline;
  sv->fontheight    = window->fontheight;

  sv->num_reallines = wblock->num_reallines;
  sv->reallines     = wblock->reallines;  

  sv->num_lines     = block->num_lines;

  sv->reltempo       = is_playing_song() ? 1.0 : ATOMIC_DOUBLE_GET(block->reltempo);
  sv->block_duration = getBlockSTimeLength(block);

  sv->scrollbar_height          = R_MAX(1, get_scrollbar_y2(window,wblock) - get_scrollbar_y1(window,wblock));
  sv->scrollbar_scroller_height = R_MAX(1, get_scrollbar_scroller_height(window,wblock));

  sv->wtracks_scissor_x1 = wblock->t.x1;
  sv->wtracks_scissor_x2 = wblock->t.x2;

  sv->block          = block;

  if (!root->song->editor_should_swing_along)
    sv->times = block->times_without_global_swings; 
  else if (window->curr_track >= 0)
    sv->times = wblock->wtrack->track->times;
  else
    sv->times = block->times_with_global_swings;

  //printf("    CHANGE. %d. sv->times: %p (%p %p)\n", block->l.num, sv->times, block->times_with_global_swings, block->times_without_global_swings);

  {
    bool is_playing = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;
    
    const struct SeqTrack *seqtrack = is_playing && pc->playtype==PLAYBLOCK ? sv->root->song->block_seqtrack : SEQUENCER_get_curr_seqtrack();
    R_ASSERT(seqtrack!=NULL);
    
    const struct SeqBlock *seqblock = seqtrack==NULL ? NULL : (struct SeqBlock*)atomic_pointer_read_relaxed((void**)&seqtrack->curr_seqblock);
    sv->curr_playing_block = seqblock==NULL ? NULL : seqblock->block;

    if(seqblock==NULL)
      sv->seqblock_stretch = 1.0;
    else
      sv->seqblock_stretch = seqblock->t.stretch;
  }

  
  {
    radium::ScopedMutex locker(vector_mutex);
    VECTOR_push_back(&g_shared_variables_gc_storage, sv->root);
    VECTOR_push_back(&g_shared_variables_gc_storage, sv->times);
    //VECTOR_push_back(&g_shared_variables_gc_storage, sv->times_with_global_swings);
    VECTOR_push_back(&g_shared_variables_gc_storage, sv->block);
    VECTOR_push_back(&g_shared_variables_gc_storage, sv->curr_playing_block);
    VECTOR_push_back(&g_shared_variables_gc_storage, sv->reallines);
  }

}

#endif
