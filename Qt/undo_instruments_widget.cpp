/* Copyright 2012 Kjetil S. Matheussen

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



// This file is #included from Qt_instruments.cpp


#include "../common/nsmtracker.h"
#include "../common/undo.h"
#include "../common/vector_proc.h"

#include "undo_instruments_widget_proc.h"


extern struct Root *root;

static void *Undo_Do_InstrumentsWidget(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

static vector_t *create_instruments_widget_state(void){
  vector_t *state = (vector_t *)talloc(sizeof(vector_t));

  //vector_t *names = (vector_t *)talloc(sizeof(vector_t));
  vector_t *pages = (vector_t *)talloc(sizeof(vector_t));

  //VECTOR_push_back(state, names);
  VECTOR_push_back(state, pages);

  QStackedWidget* tabs = instruments_widget->tabs;

  for(int i=0;i<tabs->count();i++){
    //VECTOR_push_back(names, talloc_strdup(tabs->tabText(i).toUtf8().constData()));
    VECTOR_push_back(pages, tabs->widget(i));
  }

  VECTOR_push_back(state, tabs->currentWidget());
  
  return state;
}


static void recreate_instruments_widget_from_state(vector_t *state){
  QStackedWidget* tabs = instruments_widget->tabs;

  while(tabs->count()>0)
    tabs->removeWidget(tabs->currentWidget());

  //vector_t *names = (vector_t*)state->elements[0];
  vector_t *pages = (vector_t*)state->elements[0];

  for(int i=pages->num_elements-1; i>=0; i--){
    QWidget *page = (QWidget*)pages->elements[i];
    tabs->insertWidget(0,page);//,(const char*)names->elements[i]);
  }

  tabs->setCurrentWidget((QWidget*)state->elements[1]);
}

static void Undo_InstrumentsWidget(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             create_instruments_widget_state(),
                             Undo_Do_InstrumentsWidget
                             );
}

void Undo_InstrumentsWidget_CurrPos(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  Undo_InstrumentsWidget(window,window->wblock);
}

static void *Undo_Do_InstrumentsWidget(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  vector_t *current_state = create_instruments_widget_state();

  recreate_instruments_widget_from_state((vector_t *)pointer);

  return current_state;
}


