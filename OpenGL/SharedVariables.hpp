
#include "Widget_proc.h"

#include "../common/time_proc.h"
#include "../common/vector_proc.h"


extern struct Root *root;

// Can't read data from 'root' from the OpenGL thread, because we risk reading from NULL now and then.
// So we store everything we need into this structure, and send it over to the OpenGL thread.
struct SharedVariables{
  int top_realline;
  int num_reallines;
  int curr_realline;
  int fontheight;

  float reltempo;
  STime block_duration;

  float scrollbar_height;
  float scrollbar_scroller_height;

  const struct Blocks *block; // Only used like this: if(sv->block==pc->block){...}. Reading anything inside 'block' would be dangerous.
  
  const struct STimes *times;

  Place *realline_places;

  SharedVariables()
    : realline_places(NULL)
  {}

  ~SharedVariables();
};


static inline float get_scrollbar_y1(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return 1.0;
}

static inline float get_scrollbar_y2(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return wblock->t.y2 - wblock->t.y1;
}

static inline float get_scrollbar_scroller_height(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return  (wblock->t.y2 - wblock->t.y1 - 4)
    * wblock->num_visiblelines
    / (wblock->num_reallines + wblock->num_visiblelines - 2);
}


#ifdef OPENGL_GFXELEMENTS_CPP

static QMutex vector_mutex;
static vector_t g_times_storage; // We just copy the 'times' pointer from 'root' (not the content), so we need to store the pointer somewhere where the GC can get hold of it.

// Called from OpenGL thread
SharedVariables::~SharedVariables(){
  free(realline_places);
  {
    QMutexLocker locker(&vector_mutex);
    VECTOR_remove(&g_times_storage, times);
  }
}

// Called from main thread
static void GE_fill_in_shared_variables(SharedVariables *sv){

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  struct Blocks *block = wblock->block;

  sv->top_realline  = wblock->top_realline;
  sv->num_reallines = wblock->num_reallines;
  sv->curr_realline = wblock->curr_realline;
  sv->fontheight    = window->fontheight;
  
  sv->reltempo       = block->reltempo;
  sv->block_duration = getBlockSTimeLength(block);

  sv->scrollbar_height          = get_scrollbar_y2(window,wblock) - get_scrollbar_y1(window,wblock);
  sv->scrollbar_scroller_height = get_scrollbar_scroller_height(window,wblock);

  sv->block          = block;

  sv->times          = block->times;

  {
    QMutexLocker locker(&vector_mutex);
    VECTOR_push_back(&g_times_storage, sv->times);
  }
  
  sv->realline_places = (Place*)malloc(sv->num_reallines * sizeof(Place));
  for(int i=0;i<sv->num_reallines;i++){
    sv->realline_places[i] = wblock->reallines[i]->l.p;
  }
}

#endif
