#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

#include "mQt_song_properties_callbacks.h"


extern struct Root *root;

static song_properties *widget=NULL;

static void ensure_widget_is_created(void){
  if(widget==NULL){
    widget = new song_properties(NULL);
    g_static_toplevel_widgets.push_back(widget);
  }
}


extern "C"{
  void SONGPROPERTIES_open(void){
    ensure_widget_is_created();

    safeShowOrExec(widget, true);
  }

  void SONGPROPERTIES_update(struct Song *song){
    if (widget==NULL)
      return;

    widget->_initing = true;
    
    widget->update_widgets(song);
      
    widget->_initing = false;
  }
}
