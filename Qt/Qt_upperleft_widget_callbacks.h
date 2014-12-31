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
#include "../common/undo_reltempomax_proc.h"
#include "../common/reallines_proc.h"
#include "../common/temponodes_proc.h"

extern EditorWidget *g_editor;

extern struct Root *root;


class Upperleft_widget : public QWidget, public Ui::Upperleft_widget {
  Q_OBJECT

 public:

 Upperleft_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    setupUi(this);
    setStyle("cleanlooks");
  }

  virtual void paintEvent( QPaintEvent *e ){
    static int upcounter = 0;
    printf("upperleft paintEvent %d\n",upcounter++);
    QPainter paint(this);
    //paint.fillRect(0,0,width(),height(),g_editor->colors[0]);
    paint.eraseRect(0,0,width(),height());
  }

  // called from the outside as well
  void updateWidgets(struct WBlocks *wblock){    
    lz->setValue(wblock->num_expand_lines);
    grid->setText(QString::number(root->grid_numerator)+"/"+QString::number(root->grid_denominator));
    
    lpb->setValue(root->lpb);
    bpm->setValue(root->tempo);

    // The bottom bar mirrors the lpb and bpm widgets.
    g_bottom_bar->lpb->setValue(root->lpb);
    g_bottom_bar->bpm->setValue(root->tempo);

    reltempomax->setValue(wblock->reltempomax);
  }

  // called from the outside
  void position(struct Tracker_Windows *window, struct WBlocks *wblock){

    int x1 = 0;
    int x2 = wblock->lpbTypearea.x;
    int x3 = wblock->tempoTypearea.x;
    int x4 = wblock->temponodearea.x;
    int x5 = wblock->t.x1;

    int width = x5;
    int height = wblock->t.y1;

    // upperleft lpb/bpm/reltempo show/hide
    if (window->show_lpb_track)
      LPBWidget->show();
    else
      LPBWidget->hide();
    
    if (window->show_bpm_track)
      BPMWidget->show();
    else
      BPMWidget->hide();

    if (window->show_reltempo_track)
      ReltempoWidget->show();
    else
      ReltempoWidget->hide();

    // bottombar lpb/bpm show/hide
    if (window->show_lpb_track)
      g_bottom_bar->LPBWidget->hide();
    else
      g_bottom_bar->LPBWidget->show();
    
    if (window->show_bpm_track)
      g_bottom_bar->BPMWidget->hide();
    else
      g_bottom_bar->BPMWidget->show();


    printf("resizing to %d - %d - %d - %d\n",x1,x2,x3,x4);

    resize(width,height);

    lineZoomWidget->move(x1,0);
    lineZoomWidget->resize(x2-x1,height);

    LPBWidget->move(x2,0);
    LPBWidget->resize(x3-x2,height);

    BPMWidget->move(x3,0);
    BPMWidget->resize(x4-x3,height);

    ReltempoWidget->move(x4,0);
    ReltempoWidget->resize(x5-x4,height);

  }

public slots:

  void on_lz_editingFinished(){
    printf("lz\n");
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    LineZoomBlock(window,wblock,lz->value());

    set_editor_focus();
  }

  void on_grid_editingFinished(){
    printf("grid\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    
    QStringList splitted = grid->text().split("/", QString::SkipEmptyParts);

    if (splitted.size() >= 2) {
      
      QString a = splitted[0];
      QString b = splitted[1];

      int numerator = a.toInt();
      int denominator = b.toInt();
      
      if (numerator>0 && denominator>0) {

        root->grid_numerator = numerator;
        root->grid_denominator = denominator;
      }
    }

    updateWidgets(wblock);
    set_editor_focus();
  }

  void on_lpb_editingFinished(){
    printf("lpb upperleft\n");
    setLPB(lpb->value());
    set_editor_focus();
  }

  void on_bpm_editingFinished(){
    printf("bpm upperleft\n");
    setBPM(bpm->value());
    set_editor_focus();
  }

  void on_reltempomax_editingFinished(){

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    float new_value = reltempomax->value();
    if (new_value < 1.1){
      new_value = 1.1;
      reltempomax->setValue(new_value); // Setting the min value in the widget causes the widget to ignore trying to set a value less than the min value, not setting it to the min value.
    }
    
    if (fabs(new_value-wblock->reltempomax)<0.01) {
      set_editor_focus();
      return;
    }
    
    float highest = FindHighestTempoNodeVal(wblock->block) + 1.0f;

    if(highest > new_value)
      reltempomax->setValue(highest);
    
    Undo_RelTempoMax(window,wblock);

    wblock->reltempomax=new_value;

    window->must_redraw = true;
    
    set_editor_focus();
  }
};
