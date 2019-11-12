/* Copyright 2013 Kjetil S. Matheussen

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

#include "../common/includepython.h"


#include "mQt_pianorollheader_callbacks.h"


extern EditorWidget *g_editor;

void *PIANOROLLHEADER_create(void){
  R_ASSERT(g_editor != NULL);
    
  Pianorollheader *widget = new Pianorollheader(g_editor);
  //widget->show();
  
  return widget;
}

void PIANOROLLHEADER_assignTrack(void *pianorollheader, int blocknum, int tracknum){
  Pianorollheader *widget=static_cast<Pianorollheader*>(pianorollheader);

  widget->blocknum = blocknum;
  widget->tracknum = tracknum;
}

void PIANOROLLHEADER_show(const struct WBlocks *wblock, void *pianorollheader, int x, int y, int x2, int y2){
  Pianorollheader *widget=static_cast<Pianorollheader*>(pianorollheader);

  //if (widget->parent()==NULL)
  //  widget->setParent(g_editor);

  GL_lock();{
    widget->move(x,y);
    widget->resize(x2-x, y2-y);

    if (x < wblock->t.x1){
      int x1 = wblock->t.x1 - x;
      int w = x2-x1;
      widget->setMask(QRegion(x1, 0, w, widget->height()));
    }else
      widget->clearMask();
    
    //widget->move(0,widget->tracknum*20);
    //widget->resize(100,100);
    
    //printf("A: x: %d, y: %d, width: %d, height: %d. Visible: %d\n", widget->pos().x(), widget->pos().y(), widget->size().width(), widget->size().height(),widget->isVisible());
    if (g_is_starting_up==false)
      widget->updateWidgets();
    
    widget->show();
    //widget->raise();
    //widget->update();

  } GL_unlock();
  
  //printf("B: x: %d, y: %d, width: %d, height: %d. Visible: %d\n", widget->pos().x(), widget->pos().y(), widget->size().width(), widget->size().height(),widget->isVisible());
}

void PIANOROLLHEADER_hide(void *pianorollheader){
  //  if (is_starting_up)
  // return;
  
  Pianorollheader *widget=static_cast<Pianorollheader*>(pianorollheader);
  widget->hide();
}
