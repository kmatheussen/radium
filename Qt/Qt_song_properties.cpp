#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

#include "mQt_song_properties_callbacks.h"


extern struct Root *root;

static song_properties *widget=NULL;

static void ensure_widget_is_created(void){
  if(widget==NULL){
    widget = new song_properties(NULL);
    //widget->setWindowFlags(widget->windowFlags() | Qt::WindowStaysOnTopHint);
    //widget->setWindowModality(Qt::ApplicationModal);
  }
}


extern "C"{
  void SONGPROPERTIES_open(void){
    ensure_widget_is_created();

    safeShowOrExec(widget);
  }

  void SONGPROPERTIES_set_linear_accelerando_and_ritardando(bool linear_accelerando, bool linear_ritardando){
    if (widget==NULL)
      return;

    widget->_initing = true;
    
    widget->set_linear_accelerando_and_ritardando(linear_accelerando, linear_ritardando);
      
    widget->_initing = false;
  }
}
