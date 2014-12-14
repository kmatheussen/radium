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



#include "Qt_upperleft_widget.h"

extern EditorWidget *g_editor;


class Upperleft_widget : public QWidget, public Ui::Upperleft_widget {
  Q_OBJECT

 public:

 Upperleft_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    setupUi(this);
    setStyle("cleanlooks");
  }

  void paintEvent( QPaintEvent *e ){
  static int upcounter = 0;
    printf("upperleft paintEvent %d\n",upcounter++);
    QPainter paint(this);
    //paint.fillRect(0,0,width(),height(),g_editor->colors[0]);
    paint.eraseRect(0,0,width(),height());
  }

  void position(struct WBlocks *wblock){

    int x1 = 0;
    int x2 = wblock->lpbTypearea.x;
    int x3 = wblock->tempoTypearea.x;
    int x4 = wblock->temponodearea.x;
    int x5 = wblock->temponodearea.x2;

    int y2 = wblock->t.y1-2;

    printf("resizing to %d - %d - %d - %d\n",x1,x2,x3,x4);

    resize(x5,y2);

    lineZoomWidget->move(x1,0);
    lineZoomWidget->resize(x2-x1,y2);

    LPBWidget->move(x2,0);
    LPBWidget->resize(x3-x2,y2);

    BPMWidget->move(x3,0);
    BPMWidget->resize(x4-x3,y2);

    ReltempoWidget->move(x4,0);
    ReltempoWidget->resize(x5-x4,y2);

  }

public slots:

  void on_lz_editingFinished(){
    printf("lz\n");
    set_editor_focus();
  }
  void on_grid_editingFinished(){
    printf("grid\n");
    set_editor_focus();
  }
  void on_lpb_editingFinished(){
    printf("lpb\n");
    set_editor_focus();
  }
  void on_bpm_editingFinished(){
    printf("bpm\n");
    set_editor_focus();
  }
  void on_reltempomax_editingFinished(){
    printf("reltempomax\n");
    set_editor_focus();
  }
};
