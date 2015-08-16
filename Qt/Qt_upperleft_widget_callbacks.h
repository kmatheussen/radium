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

#include "Rational.h"

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

    // Set up custom popup menues for the time widgets
    {
      bpm->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(bpm, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowBPMPopup(const QPoint&)));

      bpm_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(bpm_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowBPMPopup(const QPoint&)));

      lpb->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lpb, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLPBPopup(const QPoint&)));
      
      lpb_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lpb_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLPBPopup(const QPoint&)));
      
      signature->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(signature, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSignaturePopup(const QPoint&)));

      signature_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(signature_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSignaturePopup(const QPoint&)));

      reltempomax->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(reltempomax, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowReltempoPopup(const QPoint&)));

      reltempomax_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(reltempomax_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowReltempoPopup(const QPoint&)));
    }
  }


  virtual void paintEvent( QPaintEvent *e ){
    //static int upcounter = 0;
    //printf("upperleft paintEvent %d\n",upcounter++);
    QPainter paint(this);
    //paint.fillRect(0,0,width(),height(),g_editor->colors[0]);
    paint.eraseRect(0,0,width(),height());
  }
  
  // called from the outside as well
  void updateWidgets(struct WBlocks *wblock){
    lz->setText(lz->getRational(wblock).toString());
    
    //printf("%d/%d (%s)\n",root->grid_numerator, root->grid_denominator, Rational(root->grid_numerator, root->grid_denominator).toString().toUtf8().constData());
    //getchar();
    
    grid->setText(Rational(root->grid_numerator, root->grid_denominator).toString());
    signature->setText(Rational(root->signature).toString());
    lpb->setValue(root->lpb);
    bpm->setValue(root->tempo);

    // The bottom bar mirrors the lpb and bpm widgets.
    g_bottom_bar->signature->setText(signature->text());
    g_bottom_bar->lpb->setValue(root->lpb);
    g_bottom_bar->bpm->setValue(root->tempo);

    reltempomax->setValue(wblock->reltempomax);
  }

  void updateLayout(QWidget *w, int x1, int x2, int height){
    w->move(x1, 0);
    w->resize(x2-x1, height);

    QLayout *l = w->layout();

    l->setSpacing(2);
    l->setContentsMargins(0, 2, 2, 3);
  }
  
  // called from the outside
  void position(struct Tracker_Windows *window, struct WBlocks *wblock){


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

    // bottombar signature/lpb/bpm show/hide
    if (window->show_signature_track)
      g_bottom_bar->SignatureWidget->hide();
    else
      g_bottom_bar->SignatureWidget->show();
    
    if (window->show_lpb_track)
      g_bottom_bar->LPBWidget->hide();
    else
      g_bottom_bar->LPBWidget->show();
    
    if (window->show_bpm_track)
      g_bottom_bar->BPMWidget->hide();
    else
      g_bottom_bar->BPMWidget->show();

    if (window->show_lpb_track && window->show_bpm_track)
      g_bottom_bar->tempo_line->hide();

    
    int width = wblock->t.x1;
    int height = wblock->t.y1;
    
    //printf("resizing to %d - %d - %d - %d\n",x1,x2,x3,x4);

    resize(width,height);

    // Grid / tempocolor
    /////////////////////
    int x1 = 0; //wblock->tempocolorarea.x;
    int x2 = wblock->tempoTypearea.x;
    updateLayout(GridWidget, x1, x2, height);
    QMargins margins = GridWidget->layout()->contentsMargins();
    margins.setLeft(2);
    GridWidget->layout()->setContentsMargins(margins);
    
    // BPM
    //////////////////////////
    x1 = x2;
    x2 = wblock->lpbTypearea.x;
    updateLayout(BPMWidget, x1, x2, height);
    
    // LPB
    //////////////////////////
    x1 = x2;
    x2 = wblock->signaturearea.x;
    updateLayout(LPBWidget, x1, x2, height);
    
    // Signature
    ///////////////////////////
    x1 = x2;
    x2 = wblock->linenumarea.x;
    updateLayout(SignatureWidget, x1, x2, height);
    
    // LZ / linenumber(bars/beats)
    ///////////////////////////////
    x1 = x2;
    x2 = wblock->temponodearea.x;
    updateLayout(lineZoomWidget, x1, x2, height);

    // Reltempo (temponode)
    ///////////////////////////////
    x1 = x2;
    x2 = wblock->t.x1;
    updateLayout(ReltempoWidget, x1, x2, height);
    QMargins margins2 = ReltempoWidget->layout()->contentsMargins();
    margins2.setRight(1);
    ReltempoWidget->layout()->setContentsMargins(margins2);
    
  }

public slots:

  void ShowReltempoPopup(const QPoint& pos)
  {
    printf("GOTIT reltempo\n");
    if (popupMenu((char*)"hide tempo multiplier track")==0)
      showHideReltempoTrack(-1);
  }

  void ShowBPMPopup(const QPoint& pos)
  {
    printf("GOTIT bpm\n");
    if (popupMenu((char*)"hide BPM track")==0)
      showHideBPMTrack(-1);
  }

  void ShowLPBPopup(const QPoint& pos)
  {
    printf("GOTIT lpb\n");
    if (popupMenu((char*)"hide LPB track")==0)
      showHideLPBTrack(-1);
  }

  void ShowSignaturePopup(const QPoint& pos)
  {
    printf("GOTIT signature\n");
    if (popupMenu((char*)"hide time signature track")==0)
      showHideSignatureTrack(-1);
  }

  void on_lz_editingFinished(){

    // Also see lzqlineedit.h, where the 'wheelEvent' handler is implemented.

    printf("lz\n");
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational = create_rational_from_string(lz->text());

    int value = lz->getNumExpandLinesFromRational(rational);

    if (value==0) {
      
      updateWidgets(wblock);

    } else if (value != wblock->num_expand_lines) {
      
      LineZoomBlock(window,wblock,value);
      window->must_redraw = true;
      
    }
    
    set_editor_focus();      
  }

  void on_grid_editingFinished(){
    printf("grid\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational = create_rational_from_string(grid->text());
    grid->pushValuesToRoot(rational);    

    updateWidgets(wblock);
    set_editor_focus();
  }

  void on_signature_editingFinished(){
    printf("signature\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational = create_rational_from_string(signature->text());
    signature->pushValuesToRoot(rational);
    
    //setSignature(rational.numerator, rational.denominator);
    //printf("rational: %s. root: %d/%d",rational.toString().toUtf8().constData(),root->signature.numerator,root->signature.denominator);
    
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
