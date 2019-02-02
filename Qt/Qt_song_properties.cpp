#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>


#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

#include "mQt_song_properties_callbacks.h"


static bool g_has_been_made = false;

static QPointer<song_properties> widget;

static void ensure_widget_is_created(void){
  if(widget.data()==NULL){
    R_ASSERT(g_has_been_made==false);
    widget = new song_properties(g_main_window);
    g_static_toplevel_widgets.push_back(widget.data());
    g_has_been_made = true;
  }
}


extern "C"{
  void SONGPROPERTIES_open(void){
    ensure_widget_is_created();

    safeShowOrExec(widget.data(), true);
    
    // minimize
    //widget->adjustSize();
    //widget->updateGeometry();
  }

  void SONGPROPERTIES_update(struct Song *song){
    if (widget.data()==NULL)
      return;

    widget->_initing = true;
    
    widget->update_widgets(song);
      
    widget->_initing = false;
  }
}
