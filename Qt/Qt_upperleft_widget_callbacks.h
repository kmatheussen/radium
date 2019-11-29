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

#ifdef USE_QT5
#include <QStyleFactory>
#else
#include <QCleanlooksStyle>
#endif


extern EditorWidget *g_editor;

extern struct Root *root;


class Upperleft_widget : public QWidget, public Ui::Upperleft_widget {
  Q_OBJECT

 public:

  bool _is_initing;
  
 Upperleft_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    _is_initing = true;
    
    setupUi(this);
    
#ifdef USE_QT5
#else
      setStyle(new QCleanlooksStyle);
#endif

      swing_onoff->setText("");
      swing_onoff->y1_border=0;
      swing_onoff->y2_border=0;
      swing_onoff->x1_border=0;
      swing_onoff->x2_border=0;
      swing_onoff->_always_gradient_when_checked = true;
                             
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

      lz_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lz_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLZPopup(const QPoint&)));

      lz->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(lz, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowLZPopup(const QPoint&)));

      signature_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(signature_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSignaturePopup(const QPoint&)));

      
      swing_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(swing_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSwingPopup(const QPoint&)));

      swing_onoff->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(swing_onoff, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowSwingPopup(const QPoint&)));

      reltempomax->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(reltempomax, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowReltempoPopup(const QPoint&)));

      reltempomax_label->setContextMenuPolicy(Qt::CustomContextMenu);
      connect(reltempomax_label, SIGNAL(customContextMenuRequested(const QPoint&)),
              this, SLOT(ShowReltempoPopup(const QPoint&)));
    }

    _is_initing = false;
  }


  virtual void paintEvent( QPaintEvent *e ) override {
    TRACK_PAINT();
    
    //static int upcounter = 0;
    //printf("upperleft paintEvent %d\n",upcounter++);
    QPainter paint(this);
    //paint.fillRect(0,0,width(),height(),g_editor->colors[0]);
    paint.eraseRect(0,0,width(),height());
  }
  
  // called from the outside as well
  void updateWidgets(struct WBlocks *wblock){

    _is_initing = true;
        
    lz->setText(lz->getRational(wblock).toString());
    
    //printf("%d/%d (%s)\n",root->grid_numerator, root->grid_denominator, Rational(root->grid_numerator, root->grid_denominator).toString().toUtf8().constData());
    //getchar();
    
    grid->setText(Rational(root->grid_numerator, root->grid_denominator).toString());
    signature->setText(Rational(root->signature).toString());
    lpb->setValue(root->lpb);
    bpm->setValue(root->tempo);
    swing_onoff->setChecked(wblock->block->swing_enabled);

    // The bottom bar mirrors the lpb and bpm widgets.

    for(auto *bottom_bar : g_bottom_bars){
      bottom_bar->signature->setText(signature->text());
      bottom_bar->lpb->setValue(root->lpb);
      bottom_bar->bpm->setValue(root->tempo);
    }
    
    reltempomax->setValue(wblock->reltempomax);

    update();

    //printf("UpperLeft: Has updated\n");
    
    _is_initing = false;

    // why was this line added? (removed since it caused various widgets to lose keyboard focus if they set must_redraw=true)
    //    set_editor_focus();
  }

  void updateLayout(QWidget *w1, QWidget *w2, int x1, int x2, int height){
    QSizePolicy policy(QSizePolicy::MinimumExpanding ,QSizePolicy::Fixed);

    w1->setSizePolicy(policy);
    w2->setSizePolicy(policy);

    int width = (x2-x1) - 1;

    int ysplit = height/2;
    
    w1->move(x1, 0);
    w2->move(x1, ysplit);
    
    w1->resize(width, ysplit-1);
    w2->resize(width, height-ysplit-1);
    
  }
  
  // called from the outside
  void position(struct Tracker_Windows *window, struct WBlocks *wblock){

    GL_lock();{

      // upperleft lpb/bpm/reltempo show/hide
      if (window->show_lpb_track){
        lpb_label->show();
        lpb->show();
      }else{
        lpb_label->hide();
        lpb->hide();
      }
      
      if (window->show_bpm_track){
        bpm_label->show();
        bpm->show();
      }else{
        bpm_label->hide();
        bpm->hide();
      }
      
      if (window->show_swing_track){
        swing_label->show();
        swing_onoff->show();
      }else{
        swing_label->hide();
        swing_onoff->hide();
      }
      
      if (window->show_reltempo_track){
        reltempomax_label->show();
        reltempomax->show();
      }else{
        reltempomax_label->hide();
        reltempomax->hide();
      }
        
      // bottombar signature/lpb/bpm show/hide
      for(auto *bottom_bar : g_bottom_bars){
        if (window->show_signature_track)
          bottom_bar->SignatureWidget->hide();
        else
          bottom_bar->SignatureWidget->show();
        
        if (window->show_lpb_track)
          bottom_bar->LPBWidget->hide();
        else
          bottom_bar->LPBWidget->show();
        
        if (window->show_bpm_track)
          bottom_bar->BPMWidget->hide();
        else
          bottom_bar->BPMWidget->show();
        
        if (window->show_lpb_track && window->show_bpm_track)
          bottom_bar->tempo_line->hide();
      }
      
    } GL_unlock();
    
    int width = wblock->t.x1;
    int height = wblock->t.y1;
    
    //printf("Upper left: resizing to %d - %d - %d - %d\n",x(),y(),width,height);

    /*
    move(0,0); // Necessary on windows. I have no idea why.
    resize(width,height);
    */
    setGeometry(0, 0, width, height); // Not enought just to call resize. On windows, the widget moves to around 700, 300 after initialization, even though move(0,0) is called in the EditorWidget constructor. I have no idea why that happens.
                 
    // Grid / tempocolor
    /////////////////////
    int x1 = 0; //wblock->tempocolorarea.x;
    int x2 = wblock->tempoTypearea.x;
    updateLayout(grid_label, grid, x1, x2, height);
    /*
    QMargins margins = GridWidget->layout()->contentsMargins();
    margins.setLeft(2);
    GridWidget->layout()->setContentsMargins(margins);
    */
    
    // BPM
    //////////////////////////
    x1 = x2;
    x2 = wblock->lpbTypearea.x;
    updateLayout(bpm_label, bpm, x1, x2, height);
    
    // LPB
    //////////////////////////
    x1 = x2;
    x2 = wblock->signaturearea.x;
    updateLayout(lpb_label, lpb, x1, x2, height);
    
    // Signature
    ///////////////////////////
    x1 = x2;
    x2 = wblock->linenumarea.x;
    updateLayout(signature_label, signature, x1, x2, height);
    
    // LZ / linenumber(bars/beats)
    ///////////////////////////////
    x1 = x2;
    x2 = wblock->swingTypearea.x;
    updateLayout(lz_label, lz, x1, x2, height);

    // Swing
    ///////////////////////////////
    x1 = x2;
    x2 = wblock->temponodearea.x;
    updateLayout(swing_label, swing_onoff, x1, x2, height);
    /*
    SwingWidget->setContentsMargins(0, 0, 0, 0);
    SwingWidget->resize(x2-x1,2+height);
    //SwingWidget->layout()->itemAt(0)->widget()->resize(x2-x1,height/2 - 2); // qt is a nightmare. It's only useful for toy programs.
    SwingWidget->layout()->itemAt(1)->widget()->move(0,height/2 + 2);
    //SwingWidget->layout()->itemAt(1)->widget()->resize(x2-x1,height/2 + 2); // qt is a nightmare.
    */
    
    // Reltempo (temponode)
    ///////////////////////////////
    x1 = x2;
    x2 = wblock->t.x1;
    updateLayout(reltempomax_label, reltempomax, x1, x2, height);
    /*
    QMargins margins2 = ReltempoWidget->layout()->contentsMargins();
    margins2.setRight(1);
    ReltempoWidget->layout()->setContentsMargins(margins2);
    */
  }

public slots:

  void ShowReltempoPopup(const QPoint& pos)
  {
    printf("GOTIT reltempo\n");
    GFX_SimpleMenu("hide tempo multiplier track", [](int command, bool onoff){
      showHideReltempoTrack(-1);
      });
  }

  void ShowBPMPopup(const QPoint& pos)
  {
    printf("GOTIT bpm\n");
    GFX_SimpleMenu("hide BPM track", [](int command, bool onoff){
        showHideBPMTrack(-1);
      });
  }

  void ShowLPBPopup(const QPoint& pos)
  {
    printf("GOTIT lpb\n");
    GFX_SimpleMenu("hide LPB track", [](int command, bool onoff){
        showHideLPBTrack(-1);
      });
  }

  void ShowSignaturePopup(const QPoint& pos)
  {
    printf("GOTIT signature\n");
    GFX_SimpleMenu("hide time signature track", [](int command, bool onoff){
        showHideSignatureTrack(-1);
      });
  }

  void ShowLZPopup(const QPoint&){
    S7CALL2(void_void,"show-bars-and-beats-or-line-numbers-popup-menu");
  }
  
  void ShowSwingPopup(const QPoint& pos)
  {
    printf("GOTIT swing\n");
    S7CALL2(void_void,"show-global-swing-track-popup-menu");
  }

  void on_swing_onoff_toggled(bool val){
    if (_is_initing==true)
      return;
    
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    wblock->block->swing_enabled = val;
    TIME_block_swings_have_changed(wblock->block);
  }
    
  void on_lz_editingFinished(){
    if (_is_initing==true)
      return;

    // Also see lzqlineedit.h, where the 'wheelEvent' handler is implemented.

    //printf("lz\n");
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational(lz->text());

    int value = lz->getNumExpandLinesFromRational(rational);

    if (value==0) {
      
      updateWidgets(wblock);

    } else if (value != GetLineZoomBlock(wblock)) {
      
      LineZoomBlock(window,wblock,value);
      window->must_redraw = true;
      
    }
    
    set_editor_focus();      
  }

  void on_grid_editingFinished(){
    if (_is_initing==true)
      return;
    
    printf("grid\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational(grid->text());
    grid->pushValuesToRoot(rational);    

    updateWidgets(wblock);
    set_editor_focus();
  }

  void on_signature_editingFinished(){
    if (_is_initing==true)
      return;

    printf("signature\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational rational(signature->text());
    signature->pushValuesToRoot(rational);
    
    //setSignature(rational.numerator, rational.denominator);
    //printf("rational: %s. root: %d/%d",rational.toString().toUtf8().constData(),root->signature.numerator,root->signature.denominator);
    
    updateWidgets(wblock);
    set_editor_focus();
  }

  void on_lpb_editingFinished(){
    if (_is_initing==true)
      return;

    printf("lpb upperleft\n");
    setMainLPB(lpb->value());
    set_editor_focus();
  }

  void on_bpm_editingFinished(){
    if (_is_initing==true)
      return;

    printf("bpm upperleft\n");
    setMainBPM(bpm->value());
    set_editor_focus();
  }

  void on_reltempomax_editingFinished(){
    if (_is_initing==true)
      return;

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    double new_value = reltempomax->value();
    if (new_value < 1.1){
      new_value = 1.1;
      reltempomax->setValue(new_value); // Setting the min value in the widget causes the widget to ignore trying to set a value less than the min value, not setting it to the min value.
    }
    
    if (fabs(new_value-wblock->reltempomax)<0.01) {
      set_editor_focus();
      return;
    }
    
    double highest = FindHighestTempoNodeVal(wblock->block) + 1.0f;

    if(highest > new_value){
      new_value = highest;
      reltempomax->setValue(highest);
    }
    
    ADD_UNDO(RelTempoMax(window,wblock));

    wblock->reltempomax=new_value;

    window->must_redraw = true;
    
    set_editor_focus();
  }
};

