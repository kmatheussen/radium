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

// Based on: (although probably not much left of anymore)


/****************************************************************************
**
** Copyright (C) 2012 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** This file is part of the demonstration applications of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser General Public
** License version 2.1 as published by the Free Software Foundation and
** appearing in the file LICENSE.LGPL included in the packaging of this
** file. Please review the following information to ensure the GNU Lesser
** General Public License version 2.1 requirements will be met:
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights. These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU General
** Public License version 3.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of this
** file. Please review the following information to ensure the GNU General
** Public License version 3.0 requirements will be met:
** http://www.gnu.org/copyleft/gpl.html.
**
** Other Usage
** Alternatively, this file may be used in accordance with the terms and
** conditions contained in a signed written agreement between you and Nokia.
**
**
**
**
**
** $QT_END_LICENSE$
**
****************************************************************************/
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#include <unistd.h>

#include <QFont>
#include <QGraphicsSceneMouseEvent>
#include <QAction>
#include <QMenu>
#include <QPainter>
#include <QMouseEvent>


#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "QM_MixerWidget.h"
#include "QM_view.h"
#include "QM_chip.h"

#include "../Qt/helpers.h"

#include "../Qt/Qt_MyQButton.h"
#include "../Qt/Qt_MyQCheckBox.h"
#include "../Qt/Qt_MyQSlider.h"
#include "../Qt/mQt_mixer_widget_callbacks.h"
#include "../Qt/KeyboardFocusFrame.hpp"
#include "../Qt/Qt_bottom_bar_widget_proc.h"

#include "../common/vector_proc.h"
#include "../common/Vector.hpp"
#include "../common/hashmap_proc.h"
#include "../common/instruments_proc.h"
#include "../common/patch_proc.h"

#include "../Qt/EditorWidget.h"
#include "../Qt/Qt_instruments_proc.h"

#include "../common/undo.h"
#include "undo_mixer_proc.h"
#include "undo_mixer_connections_proc.h"
#include "undo_chip_position_proc.h"

#include "../audio/SoundProducer_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/Presets_proc.h"
#include "../audio/CpuUsage.hpp"
#include "../audio/Sampler_plugin_proc.h"
#include "../audio/SendReceive_plugins_proc.h"

#include "../api/api_common_proc.h"

#include "../embedded_scheme/s7extra_proc.h"


extern EditorWidget *g_editor;

static bool g_connections_are_visible = true;
static bool g_bus_connections_are_visible = false;



#define MOUSE_SCENE_CYCLE_CALLBACKS_FOR_QT                                    \
  void	mousePressEvent(QGraphicsSceneMouseEvent *event) override{cycle_mouse_press_event(this, event, true);} \
  void	mouseMoveEvent(QGraphicsSceneMouseEvent *event) override{cycle_mouse_move_event(this, event, true);} \
  void	mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override{cycle_mouse_release_event(this, event, true);}


namespace{
  
class MyScene : public QGraphicsScene ,public radium::MouseCycleFix {
  Q_OBJECT
 public:
  MyScene(QWidget *parent);

 protected:
  void 	mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event ) override;
  /*
  void 	mouseMoveEvent ( QGraphicsSceneMouseEvent * event ) override;
  void 	mousePressEvent ( QGraphicsSceneMouseEvent * event ) override;
  void 	mouseReleaseEvent ( QGraphicsSceneMouseEvent * event ) override;
  */
  
  bool fix_mousePressEvent(radium::MouseCycleEvent &event) override;
  void fix_mouseMoveEvent(radium::MouseCycleEvent &event) override;
  bool fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override;

  MOUSE_SCENE_CYCLE_CALLBACKS_FOR_QT;
  
  /*
  void dragEnterEvent(QGraphicsSceneDragDropEvent *e){
    printf("               GOT DRAG\n");
    //e->acceptProposedAction();
  }
  */
  
  void dragMoveEvent(QGraphicsSceneDragDropEvent *e) override {
    printf("               GOT MOVE\n");
    e->acceptProposedAction();
  }
  
  void dropEvent(QGraphicsSceneDragDropEvent *event) override {
    printf("               GOT DOP\n");
    if (event->mimeData()->hasUrls())
      {
        foreach (QUrl url, event->mimeData()->urls())
          {
            handleDropEvent(url.toLocalFile(), -100);
          }
      }
  }
  
 public:
  QWidget *_parent;

  AudioConnection *_current_connection;
  Chip *_current_from_chip;
  Chip *_current_to_chip;

  QPointer<Chip> _current_temp_connection_chip; // Make it a qpointer just in case (theoretically it shouldn't be necessary but it's just to be safe (I especially don't trust Qt to always call the mouse release event)).

  EventConnection *_current_econnection;
  Chip *_ecurrent_from_chip;
  Chip *_ecurrent_to_chip;

  std::vector<Chip*>_moving_chips;

  QPointF _start_mouse_pos;
  bool _mouse_has_moved;
  
  QRubberBand *_rubber_band = NULL;
  
#if 0
  public slots:
    void on_scene_changed ( const QList<QRectF> & region ){
    printf("Hepp! changed\n");
  }
#endif
};


class MixerWidget_OnlyMixer : public radium::KeyboardFocusFrame // ,public radium::MouseCycleFix
{
    Q_OBJECT
public:
    MixerWidget_OnlyMixer(QWidget *parent = 0);

    void setupMatrix();
    void populateScene();
    
    MyScene scene;
    MyQGraphicsView *view;
  
  /*
  bool isChildWidget(QWidget *w){
    if (w==NULL)
      return false;
    
    else if (w==this)
      return true;

    else
      return isChildWidget(w->parentWidget());
  }
    
  bool eventFilter(QObject *obj, QEvent *event) override {
    if (event->type()==QEvent::GraphicsSceneMousePress || event->type()==QEvent::MouseButtonPress){
      if (isChildWidget(dynamic_cast<QWidget*>(obj))){
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);
        //printf("    CLICKED\n");
      }
    }
    
    return false;
  }
  */

  /*
  void fix_mousePressEvent(radium::MouseCycleEvent &event) override {
    printf("................fix press\n");
  }

  void	fix_mouseMoveEvent(radium::MouseCycleEvent &event) override{
    printf("................fix move\n");
  }
  
  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override{
    printf("................fix release\n");
  }

  MOUSE_CYCLE_CALLBACKS_FOR_QT;
  */
};

}

static MixerWidget_OnlyMixer *g_mixer;

namespace radium{
  
class MixerWidget : public QWidget { 

public:
  
  radium::Splitter *_splitter;
  
  MixerWidget_OnlyMixer *_only_mixer;

  QWidget *_bottom_bar;


  RememberGeometry remember_geometry;

  void setVisible(bool visible) override {
    remember_geometry.setVisible_override<QWidget>(this, visible);
  }
  
  void hideEvent(QHideEvent *event_) override {
    remember_geometry.hideEvent_override(this);
    FOCUSFRAMES_set_focus_best_guess();
  }

  MixerWidget(QWidget *parent)
  {

    if(g_mixer_widget!=NULL){
      fprintf(stderr,"Error. More than one MixerWidget created.\n");
      abort();
    }

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setSpacing(0);
    layout->setContentsMargins(0,0,0,0);
    
    setLayout(layout);

    _splitter = new radium::Splitter(Qt::Vertical, parent);
    _splitter->setChildrenCollapsible(false);
 
    _only_mixer = new MixerWidget_OnlyMixer(_splitter);

    _bottom_bar = BottomBar_create(this, false, true);

    /*
    layout->insertWidget(-1, _splitt, 0);
    layout->insertWidget(-1, _bottom_bar, 0);
    */
    layout->addWidget(_splitter);
    layout->addWidget(_bottom_bar);
    
    _bottom_bar->hide();

    g_mixer_widget = this;
    g_static_toplevel_widgets.push_back(this);
  }
  
};

}

QGraphicsScene *get_scene(radium::MixerWidget *mixer_widget){
  return &mixer_widget->_only_mixer->scene;
}

QWidget *get_mixer_bottom_bar(radium::MixerWidget *mixer_widget){
  return mixer_widget->_bottom_bar;
}

QSplitter *get_mixer_ysplitter(radium::MixerWidget *mixer_widget){
  return mixer_widget->_splitter;
}

QWidget *get_qwidget(radium::MixerWidget *mixer_widget){
  return mixer_widget;
}

radium::MixerWidget *create_mixer_widget(QWidget *parent){
  return new radium::MixerWidget(parent);
}


static bool modular_mixer_visible(void){
  return g_mixer->view->isVisible();
}


/*
struct HelpText : public QTimer{
  QGraphicsTextItem *_text;

  HelpText(MyScene *scene){
    QFont font = g_editor->main_window->font();
    font.setPointSize(15);
    font.setPixelSize(15);

    // When is this text shown?
    _text = scene->addText(
                           // "* Add objects with right mouse button\n"
                           "* Move objects with right mouse button.\n"
                           "\n"
                           "* Double-click the name of an object to open GUI. (only if there is one)\n"
                           "\n"
                           "* Delete objects or connections by pressing SHIFT and click left (or right).\n"
                           "  - Alternatively, click with middle mouse button.\n"
                           "\n"
                           "* Select more than one object by holding CTRL when clicking.\n"
                           "  - Alternatively, mark an area of objects with left mouse button.\n"
                           "\n"
                           "* To autoconnect a new object to an existing object, right click at the input or output of an existing object.\n"
                           "\n"
                           "* Zoom in and out by pressing CTRL and using the scroll wheel.\n"
                           ,
                           font);

    _text->setDefaultTextColor(get_qcolor(HIGH_BACKGROUND_COLOR_NUM).light(70));
    _text->setPos(-150,-150);
    _text->setZValue(-1000);

    setSingleShot(true);
    setInterval(1000*60);
    start();
  }

  void 	timerEvent ( QTimerEvent * e ){
    delete _text;
    delete this;
  }
};
*/

class SlotIndicatorItem : public QGraphicsItem
 {
 public:
     QRectF boundingRect() const override
     {
         return QRectF(0,0,grid_width,grid_height);
     }

     void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
       override
     {
       QColor color = get_qcolor(MIXER_SLOT_INDICATOR_COLOR_NUM); //(59,68,155,40);
       painter->setPen(color);
       painter->setBrush(QBrush(color,Qt::SolidPattern));
       //painter->drawRoundedRect(port_width, port_height, grid_width-(port_width*2), grid_height-(port_height*2), grid_border, grid_border);
       painter->drawRoundedRect(0, 0, grid_width, grid_height, grid_border, grid_border);
       painter->setBrush(QBrush());
     }
 };

static SlotIndicatorItem *_slot_indicator = NULL;

MyScene::MyScene(QWidget *parent)
  : _parent(parent)
  , _current_connection(NULL)
  , _current_from_chip(NULL)
  , _current_to_chip(NULL)
  , _current_econnection(NULL)
  , _ecurrent_from_chip(NULL)
  , _ecurrent_to_chip(NULL)
{
#if 0
  connect(this,SIGNAL(changed( const QList<QRectF> &)),
          this,SLOT(on_scene_changed(const QList<QRectF> &)));
#endif

  //QColor color(40,40,40);
  _slot_indicator = new SlotIndicatorItem(); //addRect(0,0,chip_width,chip_width, QPen(color));//, Qt::SolidPattern);
  _slot_indicator->setZValue(-20);

  //setAcceptDrops(true);
  addItem(_slot_indicator);
  //new HelpText(this);

  setBackgroundBrush(get_qcolor(MIXER_BACKGROUND_COLOR_NUM));
}


static void get_slot_coordinates(int slot_x, int slot_y, float &x1, float &y1, float &x2, float &y2){
  //const int border_width = (grid_width-slot_width)/2;

  x1 = slot_x*grid_width;// + port_width;
  x2 = x1 + grid_width;// - (port_width*2);

  y1 = slot_y*grid_height;// + border_width;
  y2 = y1 + grid_height;// - border_width;
}

static int get_slot_x(int x){
  x++;
  if(x<0)
    return x/grid_width - 1;
  else
    return x/grid_width;
}

static int get_slot_y(int y){
  y++;
  if(y<0)
    return y/grid_height - 1;
  else
    return y/grid_height;
}

static bool x_is_placed_on_right_side(float x){
  int slot_x_pos = get_slot_x(x)*grid_width;
  if(x < slot_x_pos+grid_width/2)
    return false;
  else
    return true;
}

static bool x_is_placed_on_left_side(float x){
  return !x_is_placed_on_right_side(x);
}

static int get_chip_slot_x(Chip *chip){
  return get_slot_x(chip->pos().x()+chip_width/2);
}

static int get_chip_slot_y(Chip *chip){
  return get_slot_y(chip->pos().y()+chip_height/2);
}

/*
static bool chip_body_is_placed_at(Chip *chip, float mouse_x, float mouse_y){
  if(mouse_x < (get_chip_slot_x(chip)*grid_width + port_width))
    return false;

  else if(mouse_x > (get_chip_slot_x(chip)*grid_width + grid_width - port_width))
    return false;

  else
    return true;
}
*/

static Chip *get_chip_with_port_at(QGraphicsScene *scene,int x, int y);
static void draw_slot(MyScene *myscene, float x, float y){
  float x1,y1,x2,y2;
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  get_slot_coordinates(slot_x, slot_y, x1,y1,x2,y2);

#if 0
  if(_current_slot==NULL){
    QColor color(40,40,40);
    _current_slot = myscene->addRect(0,0,x2-x1,y2-y1, QPen(color), Qt::SolidPattern);
    //_current_slot->setZValue(-1);
  }
#endif

  QGraphicsView *view = g_mixer->view;
  const QRectF visibleRect = view->visibleRect();

  //printf("slot x1: %d (visible x1: %d). slot y1: %d (visible y1: %d)\n",(int)x1,(int)visibleRect.left(),(int)y1,(int)visibleRect.top());

  if(y2 >= visibleRect.bottom())
    return;
  if(x2 >= visibleRect.right()){
    //printf("x2: %d, visible x2: %d\n",(int)x2,(int)visibleRect.right());
    return;
  }
  if(y1 <= visibleRect.top())
    return;
  if(x1 < visibleRect.left())
    return;


  //myscene->setSceneRect(myscene->itemsBoundingRect ()); // Dangerous. Crashes now and then.

  _slot_indicator->setPos(x1,y1);

  if (myscene->_moving_chips.size() > 0)
    return;

#if 0  
  Chip *chip = MW_get_chip_at((x1+x2)/2, (y1+y2)/2, NULL);
  if (chip != NULL) {
    
    SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL,);

    struct Instruments *instrument = get_audio_instrument();
    printf("Calling pp_update\n");
    instrument->PP_Update(instrument,(struct Patch*)patch,false);
    MW_set_selected_chip(chip);
  }
#endif
}

static void move_moving_chips(MyScene *myscene, float mouse_x, float mouse_y){
  //QPointF pos=event->scenePos();
  //printf("x: %f. y: %f\n",pos.x(),pos.y());

  for(unsigned int i=0;i<myscene->_moving_chips.size(); i++){
    Chip *chip = myscene->_moving_chips.at(i);
    float x = mouse_x + chip->_moving_x_offset;
    float y = mouse_y + chip->_moving_y_offset;
    chip->setPos(x,y);
  }

  //printf("size: %d\n",(int)myscene->_moving_chips.size());
  
  draw_slot(myscene,mouse_x,mouse_y);

  // Find out whether the new position is on top of another chip. If so, set cursor.
  if(myscene->_moving_chips.size()>0){
    Chip *chip = MW_get_chip_at(mouse_x,mouse_y, myscene->_moving_chips.at(0));
    if(chip!=NULL){
      //printf("got chip at %f / %f %p / %p\n",mouse_x,mouse_y,chip,myscene->_moving_chips.at(0));      

      if(mouse_x < chip->pos().x()+(chip_width/2)){

        g_mixer->view->viewport()->setCursor(QCursor(Qt::SizeBDiagCursor));

      }else{

        g_mixer->view->viewport()->setCursor(QCursor(Qt::SizeFDiagCursor));

      }

    }else{

      //printf("got no chip\n");      
      g_mixer->view->viewport()->setCursor(QCursor(Qt::SizeHorCursor));

    }
  }
}

void MW_set_selected_chip(Chip *chip){
  printf("MW_set_selected_chip called\n");
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip2 = dynamic_cast<Chip*>(das_items.at(i));
    if(chip2!=NULL && chip2!=chip)
      chip2->mySetSelected(false);
  }
  chip->mySetSelected(true);
}

void MW_update_all_chips(void){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip2 = dynamic_cast<Chip*>(das_items.at(i));
    if(chip2!=NULL){
      chip2->update();
      for(auto *connection : chip2->_output_audio_connections)
        connection->update_shape(true, true);
    }
  }
}

static void handle_chip_selection(MyScene *myscene, radium::MouseCycleEvent &event, Chip *chip){
  bool was_selected = chip->isSelected();

  if(event.modifiers() & Qt::ControlModifier)
    chip->mySetSelected(!was_selected);

  else if(was_selected==false)
    MW_set_selected_chip(chip);

  myscene->_moving_chips.push_back(chip);
}

static void start_moving_chips(MyScene *myscene, radium::MouseCycleEvent &event, Chip *main_chip, float mouse_x, float mouse_y){
  //EVENTLOG_add_event("start_moving_chips (open undo)");
  UNDO_OPEN();

  handle_chip_selection(myscene, event, main_chip);

  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL && chip->isSelected()==true){

      SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
      volatile struct Patch *patch = plugin->patch;
      R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

      ADD_UNDO(ChipPos_CurrPos((struct Patch*)patch));
      
      chip->_moving_start_pos = chip->pos();
      chip->_moving_x_offset = chip->scenePos().x() - mouse_x;
      chip->_moving_y_offset = chip->scenePos().y() - mouse_y;

      if(chip!=main_chip)
        myscene->_moving_chips.push_back(chip);
    }
  }

  //myscene->addItem(_slot_indicator);
  draw_slot(myscene,mouse_x,mouse_y);
}

static void get_slotted_x_y(float from_x, float from_y, float &x, float &y){
  float x1,x2,y1,y2;

  get_slot_coordinates(get_slot_x(from_x), get_slot_y(from_y), x1,y1,x2,y2);

  x = x1;
  y = y1;
}

void MW_get_slotted_x_y(float from_x, float from_y, float *x, float *y){
  float to_x, to_y;
  get_slotted_x_y(from_x, from_y, to_x, to_y);
  *x = to_x;
  *y = to_y;
}

void MW_get_curr_mixer_slot(float &x, float &y){
    QPoint viewPoint = g_mixer->view->mapFromGlobal(QCursor::pos());
    QPointF scenePoint = g_mixer->view->mapToScene(viewPoint);

    get_slotted_x_y(scenePoint.x(), scenePoint.y(), x, y);
  
}

static bool move_chip_to_slot(Chip *chip, float from_x, float from_y){
  float x,y;
  get_slotted_x_y(from_x, from_y, x, y);
  //printf("   %f/%f,  %f/%f\n",chip->pos().x(),x, chip->pos().y(),y);

  chip->setPos(x,y);

  //printf("       Remake: move_chip_to_slot\n");
  remakeMixerStrips(make_instrument(-2));
  
  return chip->_moving_start_pos!=chip->pos();
}

bool MW_move_chip_to_slot(Chip *chip, float x, float y){
  return move_chip_to_slot(chip, x, y);
}
  
bool MW_move_chip_to_slot(struct Patch *patch, float x, float y){  
  return move_chip_to_slot(CHIP_get(&g_mixer->scene, patch),
                           x, y);
}

static AudioConnection *find_clean_connection_at(const MyScene *scene, float x, float y);

// Also kicks.
static bool autoconnect_chip(MyScene *myscene, Chip *chip, float x, float y){
  bool do_autoconnect = chip->_input_audio_connections.size()==0 && chip->_output_audio_connections.size()==0; // don't autoconnect if the chip already has connections.  
  
  Chip *chip_under = MW_get_chip_at(x,y,chip);
  //printf("   do_autocnn: %d. chip_under: %p\n",do_autoconnect,chip_under);
 
  if(chip_under != NULL){
    if(x_is_placed_on_left_side(x)){

      CHIP_kick_right(chip_under);
      if(do_autoconnect){
        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_connect_right(myscene, chip, chip_under);
      }

    }else{

      CHIP_kick_left(chip_under);
      if(do_autoconnect){
        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_connect_left(myscene,chip_under, chip);
      }
    }

    return true;
    
  }else if(do_autoconnect){

    AudioConnection *connection = find_clean_connection_at(myscene, x, y);
    if(connection!=NULL){
      ADD_UNDO(MixerConnections_CurrPos());

      Chip *from = connection->from;
      Chip *to = connection->to;

      CHIP_add_chip_to_connection_sequence(myscene, from, chip, to);
      
      return true;
    }

  }

  return false;
}

bool MW_autoconnect(struct Patch *patch, float x, float y){
  return autoconnect_chip(&g_mixer->scene, CHIP_get(&g_mixer->scene, patch), x, y);
}
  
static bool cleanup_a_chip_position(MyScene *myscene){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    const Chip *chip = dynamic_cast<const Chip*>(das_items.at(i));
    if(chip!=NULL){
      Chip *chip_under = MW_get_chip_at(chip->pos().x()+grid_width/2, chip->pos().y()+grid_height/2, chip);
      if(chip_under!=NULL){
        CHIP_kick_left(chip_under);
        return true;
      }
    }
  }

  return false;
}

// Make sure no chips are placed in the same slot
static void cleanup_chip_positions(MyScene *myscene){
  while(cleanup_a_chip_position(myscene)==true);
}

void MW_cleanup_chip_positions(void){
  cleanup_chip_positions(&g_mixer->scene);
}

static bool stop_moving_chips(MyScene *myscene, bool is_ctrl_clicking, const QPointF &mouse_pos){

  float mouse_x = mouse_pos.x();
  float mouse_y = mouse_pos.y();

  //myscene->removeItem(_slot_indicator);
  g_mixer->view->viewport()->setCursor(QCursor(Qt::ArrowCursor));

  Chip *main_chip = myscene->_moving_chips.at(0);
  float main_chip_x = main_chip->pos().x();
  float main_chip_y = main_chip->pos().y();

  int size = (int)myscene->_moving_chips.size();
  bool has_updated = false;

  if (!is_ctrl_clicking){
    
    for(int i=0;i<size;i++){
      Chip *chip = myscene->_moving_chips.at(i);
      //float x = chip->_moving_x_offset+mouse_x;
      //float y = chip->_moving_y_offset+mouse_y;
      float x = mouse_x + (chip->pos().x() - main_chip_x);
      float y = mouse_y + (chip->pos().y() - main_chip_y);
      //printf("x: %d, mouse_x: %d, chip_x: %d, main_chip_x: %d, diff: %d\n",(int)x,(int)mouse_x,(int)chip->pos().x(),(int)main_chip_x,(int)
      if (move_chip_to_slot(chip, x, y)==true)
        has_updated=true;
      if(size==1)
        if (autoconnect_chip(myscene, chip, mouse_x, mouse_y)==true)
          has_updated=true;
    }
  }
  
  if (has_updated==true)
    cleanup_chip_positions(myscene);

  //printf("                                  <<<<<       99999999999999999999 close undo\n");
  EVENTLOG_add_event("sstop_moving_chips (close undo)");
  bool created_undo = UNDO_CLOSE();

  // TODO: has_updated returns true if the chip was just moved around a bit. Need to store original position for has_updated to get correct value.
  if (has_updated==false && created_undo==true)
    UNDO_CANCEL_LAST_UNDO();

  myscene->_moving_chips.clear();

  if (has_updated)
    return true;
  else if (myscene->_start_mouse_pos != QCursor::pos())
    return true;
  else
    return false;
}

Chip *MW_get_chip_at(float x, float y, const Chip *except){
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL && chip!=except){
      const QPointF pos = chip->pos();
      //printf("%d / %d,    %d / %d\n",get_slot_x(pos.x()+grid_width/2), slot_x,    get_slot_y(pos.y()+grid_height/2), slot_y);
      if(get_slot_x(pos.x()+grid_width/2)==slot_x && get_slot_y(pos.y()+grid_height/2)==slot_y)
        return chip;
    }
  }

  return NULL;
}

static AudioConnection *find_clean_connection_at(const MyScene *scene, float x, float y){
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    AudioConnection *connection = dynamic_cast<AudioConnection*>(das_items.at(i));
    if(connection != NULL){
      if(get_chip_slot_y(connection->from)==slot_y && get_chip_slot_y(connection->to)==slot_y){
        if(get_chip_slot_x(connection->from)<slot_x && get_chip_slot_x(connection->to)>slot_x)
          return connection;
      }
    }
  }

  return NULL;
}

static Chip *get_chip_with_port_at(QGraphicsScene *scene,int x, int y){
  const QList<QGraphicsItem *> &das_items = scene->items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(CHIP_is_at_input_port(chip,x,y))
        return chip;
      if(CHIP_is_at_output_port(chip,x,y))
        return chip;
      if(CHIP_is_at_input_eport(chip,x,y))
        return chip;
      if(CHIP_is_at_output_eport(chip,x,y))
        return chip;
    }
  }

  return NULL;
}

static void delete_instrument(struct Patch *patch){
  struct Patch *parent = NULL;

  if (AUDIO_is_permanent_patch(patch)) {
    GFX_addMessage("Instrument \"%s\" can not be deleted", patch->name);
    return;
  }    
    

  if (getNumInAudioConnections(patch->id)==1){
    instrument_t parent_id = getAudioConnectionSourceInstrument(0, patch->id);
    parent = getAudioPatchFromNum(parent_id);
  }
  
  {
    radium::ScopedUndo undo;
    if (parent != NULL){
      ADD_UNDO(MixerConnections_CurrPos());
      S7CALL2(void_dyn_dyn, "FROM_C-remove-instrument-from-connection-path", DYN_create_instrument(parent->id), DYN_create_instrument(patch->id));
    }
    
    deleteInstrument(patch->id);
  }
}
                        
static bool mousepress_delete_chip(MyScene *scene, QGraphicsItem *item, float mouse_x, float mouse_y){
  printf("Going to delete %p\n", item);
  Chip *chip = dynamic_cast<Chip*>(item);
  if(chip!=NULL){
    printf("Got chip\n");

    struct Patch *patch = CHIP_get_patch(chip);

    if (patch != NULL){
      delete_instrument(patch);
      return true;
    }
      
  }

  return false;
}

static void delete_several_chips(const vector_t &patches){
  UNDO_OPEN_REC();{
    VECTOR_FOR_EACH(struct Patch *,patch,&patches){
      delete_instrument(patch);
    }END_VECTOR_FOR_EACH;
  }UNDO_CLOSE();
}

static bool mousepress_start_connection(MyScene *scene, radium::MouseCycleEvent &event, QGraphicsItem *item, float mouse_x, float mouse_y){

  Chip *chip = get_chip_with_port_at(scene,mouse_x,mouse_y);
  //printf("chip: %p\n", chip);

  if(chip!=NULL){

    struct Instruments *instrument = get_audio_instrument();
    SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL,false);
    instrument->PP_Update(instrument,(struct Patch*)patch,false);
    MW_set_selected_chip(chip);

    // connection
    {
      
      if(CHIP_is_at_output_port(chip,mouse_x,mouse_y)) {
        if (chip->_num_outputs==0)
          return false;
        
        scene->_current_from_chip = chip;
        R_ASSERT(scene->_current_to_chip==NULL);
                 
      } else if(CHIP_is_at_input_port(chip,mouse_x,mouse_y)) {
        
        if (!chip->input_audio_port_is_operational())
          return false;
        
        scene->_current_to_chip = chip;
        R_ASSERT(scene->_current_from_chip==NULL);
      }

      if(scene->_current_from_chip!=NULL || scene->_current_to_chip!=NULL){
        //printf("x: %d, y: %d. Item: %p. input/output: %d/%d\n",(int)mouse_x,(int)mouse_y,item,_current_input_port,_current_output_port);

        if (scene->_current_from_chip!=NULL && scene->_current_to_chip!=NULL){
          R_ASSERT(false);
          return false;
        }
        
        scene->_current_connection = new AudioConnection(scene, scene->_current_from_chip, scene->_current_to_chip, ConnectionType::IS_SEND, true/*, true*/);
        scene->_current_connection->set_on_top();
        scene->addItem(scene->_current_connection);
        
        scene->_current_connection->setLine(mouse_x,mouse_y,mouse_x,mouse_y);
        scene->_current_connection->update_shape(true, true);
        
        event.accept();
        return true;
      }

    }

    // econnection
    {
      if(CHIP_is_at_output_eport(chip,mouse_x,mouse_y))
        scene->_ecurrent_from_chip = chip;

      else if(CHIP_is_at_input_eport(chip,mouse_x,mouse_y))
        scene->_ecurrent_to_chip = chip;

      if(scene->_ecurrent_from_chip!=NULL || scene->_ecurrent_to_chip!=NULL){

        if (scene->_current_from_chip!=NULL && scene->_current_to_chip!=NULL){
          R_ASSERT(false);
          return false;
        }

        scene->_current_econnection = new EventConnection(scene, scene->_ecurrent_from_chip, scene->_ecurrent_to_chip);
        scene->addItem(scene->_current_econnection);
        scene->_current_econnection->set_on_top();
        
        scene->_current_econnection->setLine(mouse_x,mouse_y,mouse_x,mouse_y);
        scene->_current_econnection->update_shape(true, true);
        
        event.accept();
        return true;
      }
    }
    
  }

  return false;
}

static bool mousepress_delete_connection(MyScene *scene, radium::MouseCycleEvent &event, QGraphicsItem *item, float mouse_x, float mouse_y){
  SuperConnection *connection = dynamic_cast<SuperConnection*>(item);

  if(connection==NULL){
    const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
    for (int i = 0; i < das_items.size(); ++i) {
      connection = dynamic_cast<SuperConnection*>(das_items.at(i));
      if(connection!=NULL && connection->isVisible() && connection->isUnderMouse()==true)
        break;
      else
        connection = NULL;
    }
  }

  if(connection!=NULL){
    ADD_UNDO(MixerConnections_CurrPos());
    CONNECTION_delete_connection(connection);
    event.accept();
    return true;
  }
  return false;
}

static bool mousepress_select_chip(MyScene *scene, radium::MouseCycleEvent &event, QGraphicsItem *item, float mouse_x, float mouse_y, bool ctrl_pressed){
  Chip *chip = dynamic_cast<Chip*>(item);

  if(chip!=NULL && chip->positionedAtSlider(QPointF(mouse_x-chip->scenePos().x(),mouse_y-chip->scenePos().y()))==false){
    SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);
    
    struct Instruments *instrument = get_audio_instrument();
    //printf("Calling pp_update\n");
    instrument->PP_Update(instrument,(struct Patch*)patch,false);

    //printf("                                                         ^^^^^^^^   mousepress_select_chip\n");
    EVENTLOG_add_event("start_moving_chips called from mousepress_select_chip");
    start_moving_chips(scene,event,chip,mouse_x,mouse_y);
    event.accept();
    return true;
  }

  return false;
}

/*
static bool mouserelease_replace_patch(MyScene *scene, float mouse_x, float mouse_y){
  Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
  if(chip_under!=NULL){
    if(chip_body_is_placed_at(chip_under, mouse_x, mouse_y)==true) {

      SoundPlugin *plugin = SP_get_plugin(chip_under->_sound_producer);
      Patch *patch = (struct Patch*)plugin->patch;
      R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);

      requestReplaceInstrument(patch->id, "", API_get_gui_from_existing_widget(g_mixer->window()));
        
      return true;
    }
  }
  return false;
}
*/

static QVector<Chip*> get_selected_chips(void){
  QVector<Chip*> ret;
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL && chip->isSelected()==true){
      ret.push_back(chip);
    }
  }

  return ret;
}

vector_t MW_get_selected_chips(void){
  QVector<Chip*> chips = get_selected_chips();
  
  vector_t ret = {};

  for(auto *chip : chips){
    VECTOR_push_back(&ret, chip);
  }

  return ret;
}


static void mouserelease_show_connection_popup(SuperConnection *connection){

  if (connection->from==NULL)
    return;
  
  if (connection->to==NULL)
    return;  
  
  S7EXTRA_GET_FUNC(func, "FROM_C-show-mixer-connection-popup-menu");
    
  s7extra_applyFunc_void_varargs(func,
                                 DYN_create_instrument(CHIP_get_patch(connection->_from)->id),
                                 DYN_create_instrument(CHIP_get_patch(connection->_to)->id),
                                 DYN_create_bool(connection->_is_event_connection),
                                 g_uninitialized_dyn
                                 );
}

static bool mouserelease_create_chip(MyScene *scene, float mouse_x, float mouse_y){
  
  {
    printf("mouserelease_create_chip called\n");
    
    draw_slot(scene,mouse_x,mouse_y);
    
    float x, y;
    get_slotted_x_y(mouse_x, mouse_y, x, y);

    S7EXTRA_GET_FUNC(func, "FROM_C-show-mixer-popup-menu");
    
    s7extra_applyFunc_void_varargs(func,
                                   DYN_create_instrument(createIllegalInstrument()),
                                   DYN_create_float(x),
                                   DYN_create_float(y),
                                   g_uninitialized_dyn);
  }
  
  /*
  createInstrumentDescriptionPopupMenu(createNewInstrumentConf(x, y, false, true,
                                                               true,
                                                               false, false,
                                                               API_get_gui_from_existing_widget(g_mixer->window())
                                                               )
                                       );
  */
  /*                                       
  const char *instrument_description = instrumentDescriptionPopupMenu(false, false);
  printf("   instrument_description: %s\n",instrument_description);
  
  if (instrument_description != NULL){

    float x, y;
    get_slotted_x_y(mouse_x, mouse_y, x, y);
    
    UNDO_OPEN();{

      int num_patches_before = get_audio_instrument()->patches.num_elements;
      
      int64_t patch_id = createAudioInstrumentFromDescription(instrument_description, NULL, x, y);
      //SoundPlugin *plugin = add_new_audio_instrument_widget(NULL,mouse_x,mouse_y,false,NULL, MIXER_get_buses());

      //printf("           ID: %d\n",(int)patch_id);

      int num_new_patches = get_audio_instrument()->patches.num_elements - num_patches_before;
      
      if (patch_id >= 0 && num_new_patches==1){
        struct Patch *patch = PATCH_get_from_id(patch_id);
        Chip *chip = CHIP_get(scene, patch);
        autoconnect_chip(scene, chip, mouse_x, mouse_y); // If we place a new chip on top of another chip, or on top of a connection, we autoconnect.
      }
        
    }UNDO_CLOSE();

  }
  */
  
  return true;
}

static vector_t get_selected_patches(void){
  vector_t ret = {};
  const vector_t &patches = get_audio_instrument()->patches;//MW_get_selected_patches();

  VECTOR_FOR_EACH(struct Patch *, patch, &patches){
    struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (plugin != NULL && plugin->is_selected)
      VECTOR_push_back(&ret, patch);
  }END_VECTOR_FOR_EACH;

  return ret;
}

vector_t MW_get_selected_patches(void){
  return get_selected_patches();
}

static vector_t get_curr_mixer_patches(void){
  vector_t ret = {};

  const dynvec_t patchids = getCurrMixerInstruments();
  
  for(const dyn_t dyn : patchids){
    R_ASSERT_RETURN_IF_FALSE2(dyn.type==INSTRUMENT_TYPE, ret);

    struct Patch *patch = PATCH_get_from_id(dyn.instrument);

    R_ASSERT_NON_RELEASE(patch!=NULL);
      
    if (patch != NULL)
      VECTOR_push_back(&ret, patch);
  }

  return ret;  
}

void MW_copy(void){
  const vector_t patches = get_curr_mixer_patches(); //get_selected_patches();

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }

  PRESET_copy(&patches);
}

static void MW_delete2(float mouse_x, float mouse_y, bool has_mouse_coordinates){
  vector_t patches = get_curr_mixer_patches(); //get_selected_patches();
  delete_several_chips(patches);
}

void MW_delete(void){
  MW_delete2(0,0,false);
}

static void MW_cut2(float mouse_x, float mouse_y, bool has_mouse_coordinates){
  vector_t patches = get_curr_mixer_patches(); //get_selected_patches();

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }
  
  if (PRESET_copy(&patches)==true)
    MW_delete2(mouse_x, mouse_y, has_mouse_coordinates);
}

void MW_cut(void){
  MW_cut2(0,0,false);
}

instrument_t MW_paste(float x, float y){
  if (PRESET_has_copy()==false)
    return make_instrument(-1);

  if (x <= -10000 || y <= -10000)
    MW_get_curr_mixer_slot(x, y);

  return PRESET_paste(x, y);
}

bool MW_has_mouse_pointer(void){
  if (g_mixer_widget->isVisible()==false)
    return false;

  QPoint p = g_mixer_widget->mapFromGlobal(QCursor::pos());
  //printf("x: %d, y: %d. width: %d, height: %d\n", (int)p.x(), (int)p.y(), g_mixer->width(), g_mixer->height());
  if (true
      && p.x() >= 0
      && p.x() <  g_mixer_widget->width()
      && p.y() >= 0
      && p.y() <  g_mixer_widget->height()
      )
    return true;
  else
    return false;
}

static void update_connections_visibility(void){  
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
  for(auto *item : das_items){
    SuperConnection *connection = dynamic_cast<SuperConnection*>(item);
    if (connection != NULL){
      connection->update_visibility();
    }
  }
}

void MW_update_connections_visibility(void){
  update_connections_visibility();
}
  
bool MW_get_connections_visibility(void){
  return g_connections_are_visible;
}

void MW_set_connections_visibility(bool show){
  g_connections_are_visible=show;
  update_connections_visibility();
}

bool MW_get_bus_connections_visibility(void){
  return g_bus_connections_are_visible;
}

void MW_set_bus_connections_visibility(bool show){
  if (show != g_bus_connections_are_visible) {
    g_bus_connections_are_visible=show;
    update_connections_visibility();
    MW_update_all_chips(); // input ports for buses.
  }
}

void MW_zoom(int inc){
  g_mixer->view->zoom(inc);
}

void MW_reset_zoom(void){
  g_mixer->view->reset_zoom();
}

/*
static const char *get_displayable_keybinding(const char *prefix, const char *racommand, const dynvec_t &args){
  const char *result = S7CALL2(charpointer_charpointer_dyn, "FROM_C-get-displayable-keybinding", racommand, DYN_create_array(args));

  if (!strcmp("", result))
    return prefix;
  else
    return talloc_format("[shortcut]%s[/shortcut]%s", result, prefix);
}

static int create_menu_entry(vector_t *v, const char *prefix, const char *racommand, const dynvec_t &args = g_empty_dynvec){
  return VECTOR_push_back(v, get_displayable_keybinding(prefix, racommand, args));
}
*/
                             
static bool mousepress_save_presets_etc(MyScene *scene, radium::MouseCycleEvent &event, float mouse_x, float mouse_y){

  Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
  if(chip_under==NULL)
    return false;

  const vector_t patches = get_curr_mixer_patches(); //get_selected_patches();
  if (patches.num_elements==0)
    return false;

  const SoundPlugin *plugin = SP_get_plugin(chip_under->_sound_producer);
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(plugin->patch!=NULL, false);
  
  int effect_num = -1;

  switch(chip_under->get_hover_item()){
    case Chip::HoverItem::NOTHING:
    case Chip::HoverItem::NAME:
    case Chip::HoverItem::EVENT_PORT:
    case Chip::HoverItem::AUDIO_INPUT_PORT:
    case Chip::HoverItem::AUDIO_OUTPUT_PORT:
      {

        S7EXTRA_GET_FUNC(func, "FROM_C-show-mixer-popup-menu");
    
        s7extra_applyFunc_void_varargs(func,
                                       DYN_create_instrument(CHIP_get_patch(chip_under)->id),
                                       DYN_create_float(mouse_x),
                                       DYN_create_float(mouse_y),
                                       g_uninitialized_dyn);
        break;
      }
      
    case Chip::HoverItem::SOLO_BUTTON:
      effect_num = plugin->type->num_effects + EFFNUM_SOLO_ONOFF;
      break;
      
    case Chip::HoverItem::MUTE_BUTTON:
      effect_num = get_mute_effectnum(plugin->type);
      break;
      
    case Chip::HoverItem::BYPASS_BUTTON:
      effect_num = plugin->type->num_effects + EFFNUM_EFFECTS_ONOFF;
      break;
      
    case Chip::HoverItem::VOLUME_SLIDER:
      effect_num = chip_under->get_volume_effect_num();
      break;
  }

  if (effect_num >= 0) {

    S7CALL2(void_dyn_dyn,
            "FROM_C-show-mixer-popup-menu-effect",
            DYN_create_instrument(plugin->patch->id),
            DYN_create_string(PLUGIN_get_effect_name(plugin, effect_num))
            );

  }
  
  return true;

  /*
  vector_t v = {};

  int connect_to_main_pipe = -1;
  int insert = -1;
  int replace = -1;
  int copy = -1;
  int cut = -1;
  int delete_ = -1;
  int rename = -1;
  int load = -1;
  int save = -1;
  int show_mixer_strips = -1;
  int config_color = -1;
  int generate_new_color = -1;
  int instrument_info = -1;
  int random = -1;
  int solo = -1;
  int solo_several = -1;
  int unsolo_several = -1;
  int mute = -1;
  int mute_several = -1;
  int unmute_several = -1;
  int bypass = -1;
  int bypass_several = -1;
  int unbypass_several = -1;
  int unsolo_all = -1;
  int mute_all = -1;
  int unmute_all = -1;
  int show_gui = -1;
  int receive_external_midi = -1;
  int showhide_mixer = -1;
  
  int64_t parentguinum = API_get_gui_from_existing_widget(g_mixer->window());
  
  bool has_sampler_instrument = false;
  VECTOR_FOR_EACH(struct Patch *,patch,&patches){
    struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (QString("Sample Player") == plugin->type->type_name){
      has_sampler_instrument = true;
      break;
    }
  }END_VECTOR_FOR_EACH;
  
  if (patches.num_elements > 1) {

    VECTOR_push_back(&v, "--------Sample player");
    
    if (has_sampler_instrument)
      random = create_menu_entry(&v, "Load random samples from folders", "ra:set-random-sample-for-all-selected-instruments");
    else
      random = create_menu_entry(&v, "[disabled]Load random samples from folders", "ra:set-random-sample-for-all-selected-instruments");

    VECTOR_push_back(&v, "--------Selected objects");

    copy = create_menu_entry(&v, "Copy", "ra:copy-selected-mixer-objects");
    
    cut = create_menu_entry(&v, "Cut", "ra:cut-selected-mixer-objects");
    
    delete_ = create_menu_entry(&v, "Delete", "ra:delete-selected-mixer-objects"); // sound objects");

    VECTOR_push_back(&v, "--------");
    
    save = VECTOR_push_back(&v, "Save multi preset (.mrec)");
    VECTOR_push_back(&v, "--------");
    show_mixer_strips = VECTOR_push_back(&v, "Create new mixer strips window for the selected objects");
    VECTOR_push_back(&v, "--------");
    solo_several = create_menu_entry(&v, "Solo all selected", "ra:switch-solo-for-selected-instruments");
    unsolo_several = create_menu_entry(&v, "Un-solo all selected", "ra:switch-solo-for-selected-instruments");
    VECTOR_push_back(&v, "--------");
    mute_several = create_menu_entry(&v, "Mute all selected", "ra:switch-mute-for-selected-instruments");
    unmute_several = create_menu_entry(&v, "Un-mute all selected", "ra:switch-mute-for-selected-instruments");
    VECTOR_push_back(&v, "--------");
    bypass_several = create_menu_entry(&v, "Bypass all selected", "ra:switch-bypass-for-selected-instruments");
    unbypass_several = create_menu_entry(&v, "Un-bypass all selected", "ra:switch-bypass-for-selected-instruments");
    VECTOR_push_back(&v, "--------");
    config_color = VECTOR_push_back(&v, "Configure color");
    generate_new_color = create_menu_entry(&v, "Generate new color", "ra:generate-new-color-for-all-selected-instruments");

    VECTOR_push_back(&v, "--------Mixer");
    unsolo_all = VECTOR_push_back(&v, "Un-solo all");
    mute_all = VECTOR_push_back(&v, "Mute all");
    unmute_all = VECTOR_push_back(&v, "Un-mute all");

    showhide_mixer = create_menu_entry(&v, "[check on]Visible", "ra:show-hide-mixer-widget");
    
  } else { // i.e. if (patches.num_elements == 1){

    struct Patch *patch = CHIP_get_patch(chip_under);
    struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    
    if (QString("Sample Player") == SP_get_plugin(chip_under->_sound_producer)->type->type_name){
      VECTOR_push_back(&v, "--------Sample player");
      random = create_menu_entry(&v, "Load random samples from folders", "ra:set-random-sample-for-all-selected-instruments");
    }

    VECTOR_push_back(&v, "--------Object");

    if (plugin->type->num_outputs>0 && getNumOutAudioConnections(patch->id)==0)
      connect_to_main_pipe = VECTOR_push_back(&v, "Connect to main pipe");
      
    insert = VECTOR_push_back(&v, "Insert"); // sound object");
    replace = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Replace" : "Replace"); // sound object");
    
    VECTOR_push_back(&v, "--------");

    copy = create_menu_entry(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Copy" : "Copy", "ra:copy-selected-mixer-objects");
    
    cut = create_menu_entry(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Cut" : "Cut", "ra:cut-selected-mixer-objects");

    delete_ = create_menu_entry(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Delete" : "Delete", "ra:delete-selected-mixer-objects");

    VECTOR_push_back(&v, "--------");

    if (ATOMIC_GET(plugin->solo_is_on)){
      solo = create_menu_entry(&v, "[check on]Solo", "ra:switch-solo-for-selected-instruments");
      //unsolo = VECTOR_push_back(&v, "Un-solo");
    }else{
      solo = create_menu_entry(&v, "[check off]Solo", "ra:switch-solo-for-selected-instruments");
      //unsolo = VECTOR_push_back(&v, "[disabled]Un-solo");
    }

    VECTOR_push_back(&v, "--------");
        
    if (is_muted_relaxed(plugin)){
      mute = create_menu_entry(&v, "[check on]Mute", "ra:switch-mute-for-selected-instruments");
      //unmute = VECTOR_push_back(&v, "Un-mute");
    }else{
      mute = create_menu_entry(&v, "[check off]Mute", "ra:switch-mute-for-selected-instruments");
      //unmute = VECTOR_push_back(&v, "[disabled]Un-mute");
    }

    if (PLUGIN_get_effect_value(plugin, plugin->type->num_effects + EFFNUM_EFFECTS_ONOFF, VALUE_FROM_PLUGIN) < 0.5){
      bypass = create_menu_entry(&v, "[check on]Bypass", "ra:switch-bypass-for-selected-instruments");
    }else{
      bypass = create_menu_entry(&v, "[check off]Bypass", "ra:switch-bypass-for-selected-instruments");
    }

    VECTOR_push_back(&v, talloc_format("--------Instrument: \"%s\"", patch->name));

    rename = VECTOR_push_back(&v, "Rename");
    
    VECTOR_push_back(&v, "---------");
        
    load = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Load preset" : "Load preset (.rec)");

    save = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Save preset" : "Save preset (.rec)");

    VECTOR_push_back(&v, "---------");
        
    config_color = VECTOR_push_back(&v, "Configure color");
    generate_new_color = create_menu_entry(&v, "Generate new color", "ra:generate-new-color-for-all-selected-instruments");
    
    VECTOR_push_back(&v, "--------");

    {
      bool is_enabled = hasNativeInstrumentGui(patch->id);
      bool is_visible = is_enabled && instrumentGuiIsVisible(patch->id, parentguinum);
      
      if (!is_enabled)
        show_gui = VECTOR_push_back(&v, "[disabled][check off]Show GUI");
      else if (is_visible)
        show_gui = VECTOR_push_back(&v, "[check on]Show GUI");
      else
        show_gui = VECTOR_push_back(&v, "[check off]Show GUI");
    }

    {
      bool is_enabled = instrumentAlwaysReceiveMidiInput(patch->id);
      if (is_enabled)
        receive_external_midi = VECTOR_push_back(&v,"[check on]Recv. external MIDI");
      else
        receive_external_midi = VECTOR_push_back(&v,"[check off]Recv. external MIDI");
    }
      
    VECTOR_push_back(&v, "--------");
    
    instrument_info = VECTOR_push_back(&v, "Show info");
    
    VECTOR_push_back(&v, "--------Mixer");
        
    unsolo_all = VECTOR_push_back(&v, "Un-solo all");
    mute_all = VECTOR_push_back(&v, "Mute all");
    unmute_all = VECTOR_push_back(&v, "Un-mute all");

    showhide_mixer = create_menu_entry(&v, "[check on]Visible", "ra:show-hide-mixer-widget");
  }

  
  IsAlive is_alive(chip_under);

  // Not safe to store gc objects in a c++ closure. (At least I think so.)
  QVector<instrument_t> patch_ids;
  VECTOR_FOR_EACH(struct Patch *,patch,&patches){
    patch_ids.push_back(patch->id);
  }END_VECTOR_FOR_EACH;

#define sels connect_to_main_pipe,insert,replace,solo,solo_several,unsolo_several,mute,mute_several,unmute_several,unsolo_all,mute_all,unmute_all,showhide_mixer,bypass,bypass_several,unbypass_several,copy,cut,delete_,load,save,rename,show_mixer_strips,config_color,generate_new_color,show_gui,receive_external_midi,random,instrument_info
  
  GFX_Menu3(v,[is_alive, chip_under, scene, patch_ids, sels, mouse_x, mouse_y, parentguinum](int sel, bool onoff){
      
#undef sels
      
      if (!is_alive)
        return;

      vector_t patches = {};
      
      for(instrument_t patch_id : patch_ids){
        struct Patch *patch = PATCH_get_from_id(patch_id);
        
        if(patch==NULL || patch->patchdata==NULL){
          if(patch!=NULL && patch->patchdata==NULL)
            R_ASSERT(false);
          else
            R_ASSERT_NON_RELEASE(false);
          return;
        }
        
        VECTOR_push_back(&patches, patch);
      }

      R_ASSERT(parentguinum == API_get_gui_from_existing_widget(g_mixer->window()));

      if (sel==-1) {
    
      } else if (sel==connect_to_main_pipe) {
        connectAudioInstrumentToMainPipe(CHIP_get_patch(chip_under)->id);
          
      } else if (sel==insert) {
    
        mouserelease_create_chip(scene, mouse_x, mouse_y);
        
      } else if (sel==replace) {
        
        mouserelease_replace_patch(scene, mouse_x, mouse_y);
        
      } else if (sel==solo) {

        setSoloForInstruments(getSelectedInstruments(), onoff);
        
      } else if (sel==solo_several) {

        setSoloForInstruments(getSelectedInstruments(), true);
        
      } else if (sel==unsolo_several) {

        setSoloForInstruments(getSelectedInstruments(), false);
        
      } else if (sel==mute) {

        setMuteForInstruments(getSelectedInstruments(), onoff);
        
      } else if (sel==bypass) {

        setBypassForInstruments(getSelectedInstruments(), onoff);
        
      } else if (sel==mute_several) {

        setMuteForInstruments(getSelectedInstruments(), true);
        
      } else if (sel==unmute_several) {

        setMuteForInstruments(getSelectedInstruments(), false);
        
      } else if (sel==bypass_several) {

        setBypassForInstruments(getSelectedInstruments(), true);
        
      } else if (sel==unbypass_several) {

        setBypassForInstruments(getSelectedInstruments(), false);
        
      } else if (sel==unsolo_all) {

        setSoloForInstruments(getSelectedInstruments(), false);
        
      } else if (sel==mute_all) {

        setMuteForInstruments(getSelectedInstruments(), true);
        
      } else if (sel==unmute_all) {

        setMuteForInstruments(getSelectedInstruments(), false);

      } else if (sel==showhide_mixer) {

        showHideMixerWidget();
        
      } else if (sel==copy) {
        
        MW_copy();
        
      } else if (sel==cut) {
        
        MW_cut2(mouse_x, mouse_y, true);
        
      } else if (sel==delete_) {
        
        MW_delete2(mouse_x, mouse_y, true);
        
      } else if (sel==rename) {

        R_ASSERT_RETURN_IF_FALSE(patch_ids.size() == 1);
        S7CALL2(void_dyn, "FROM_C-request-rename-instrument", DYN_create_instrument(patch_ids.at(0)));
        
      } else if (sel==load) {

        R_ASSERT_RETURN_IF_FALSE(patch_ids.size() == 1);
        requestLoadInstrumentPreset(patch_ids.at(0), "", parentguinum);
        
      } else if (sel==save) {
        
        PRESET_save(&patches, false, parentguinum);
        
      } else if (sel==show_mixer_strips) {
        
        int num_rows = R_BOUNDARIES(1, 1 + (patches.num_elements / 6), 3);
        dynvec_t instruments = {};
        
        VECTOR_FOR_EACH(struct Patch *,patch,&patches){
          DYNVEC_push_back(&instruments, DYN_create_instrument(patch->id));
        }END_VECTOR_FOR_EACH;
        
        showMixerStrips2(num_rows, DYN_create_array(instruments));
        
      } else if (sel==config_color) {
        
        S7EXTRA_GET_FUNC(scheme_func, "FROM_C-show-instrument-color-dialog");

        dynvec_t args = {};

        DYNVEC_push_back(args, DYN_create_int(parentguinum));

        dynvec_t instruments = {};
          
        DYNVEC_push_back(instruments, DYN_create_instrument(CHIP_get_patch(chip_under)->id));
        
        VECTOR_FOR_EACH(struct Patch *,patch,&patches){
          if (patch!=CHIP_get_patch(chip_under))
            DYNVEC_push_back(instruments, DYN_create_instrument(patch->id));
        }END_VECTOR_FOR_EACH;
        
        
        DYNVEC_push_back(args, DYN_create_array(instruments));        

        s7extra_applyFunc_void(scheme_func, args);
                                       
      } else if (sel==generate_new_color) {

        generateNewColorForAllSelectedInstruments(0.9);
        
      } else if (sel==instrument_info) {
        
        showInstrumentInfo(DYN_create_instrument(CHIP_get_patch(chip_under)->id), parentguinum);
        
      } else if (sel==show_gui) {
        
        struct Patch *patch = CHIP_get_patch(chip_under);
        struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        
        if (instrumentGuiIsVisible(patch->id, parentguinum))
          PLUGIN_close_gui(plugin);
        else
          PLUGIN_open_gui(plugin, parentguinum);
        
      } else if (sel==receive_external_midi) {
        
        struct Patch *patch = CHIP_get_patch(chip_under);
        
        setInstrumentAlwaysReceiveMidiInput(patch->id, onoff);
        
      } else if (sel==random) {
        
        setRandomSampleForAllSelectedInstruments();
        
      } else {
        
        R_ASSERT(false);
        
      }
    });

  return true;
  */
}

static bool event_can_delete(radium::MouseCycleEvent &event){
  if(event.button()==Qt::MiddleButton)
    return true;

  else if(event.modifiers() & Qt::ShiftModifier)
    return true;

  else if (event.button()==Qt::BackButton)
    return true;
  
  else
    return false;
}

void MyScene::mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event ){
  QPointF pos=event->scenePos();
  printf("Scene is double-clicked\n");

  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();
  
    
  Chip *chip = MW_get_chip_at(pos.x(), pos.y(), NULL);
  if(chip!=NULL){
    
    if(event->modifiers() & Qt::ControlModifier){
      
      chip->mySetSelected(!chip->isSelected());
      event->accept();
      return;
      
    } else {
      
      if (chip->myMouseDoubleClickEvent(pos.x()-chip->x(), pos.y()-chip->y())){
        event->accept();
        return;
      }
      
    }
    
    /*
      if(SP_get_plugin(chip->_sound_producer)->type->show_gui != NULL) {
      
      int x1,y1,x2,y2;
      chip->get_name_coordinates(x1,y1,x2,y2);
      
      QPointF pos = event->pos();
      
      if(pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2){
      
      SP_get_plugin(chip->_sound_producer)->type->show_gui(SP_get_plugin(chip->_sound_producer));
      event->accept();
      }
      }
    */
  }
  
  QGraphicsScene::mouseDoubleClickEvent(event);
}
/*
static QMouseEvent *get_qmouseevent(const QGraphicsSceneMouseEvent *event){
  static QMouseEvent e(QEvent::MouseButtonRelease,
                       event->scenePos(),
                       event->button(),
                       event->buttons(),
                       event->modifiers());
  
  e = QMouseEvent(QEvent::MouseButtonRelease,
                  event->scenePos(),
                  event->button(),
                  event->buttons(),
                  event->modifiers());
  return &e;
}
*/
static bool g_is_pressed = false; // Workaround for nasty Qt bug.

static bool g_is_moving_rubberband = false;
static QPointF g_start_rubberband;

static bool g_is_dragging_everything = false;
static double g_startpos_scrollbar_x;
static double g_startpos_scrollbar_y;

static bool g_is_ctrl_clicking_this_cycle = false;

bool MyScene::fix_mousePressEvent(radium::MouseCycleEvent &event){
  //FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);
  
  //MOUSE_CYCLE_register(g_mixer_widget, get_qmouseevent(event));

  //printf("  1. MousePress. Ctrl-clicking: %d\n", g_is_ctrl_clicking_this_cycle);
  
  _mouse_has_moved = false;
  
  if (g_is_pressed==true)
    return true;

  g_is_pressed = true;

  //printf("mousepress: %p\n",_current_connection);
  
  EVENTLOG_add_event(talloc_format(">>>>  MyScene::mousePressEvent. has_undo: %d, runs_custom_exec: %d, _current_connection: %p, _current_econnection: %p, _moving_chips.size(): %d", (int)Undo_Is_Open(), (int)g_radium_runs_custom_exec, _current_connection, _current_econnection, (int)_moving_chips.size()));

  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2(false);
  
  const QPointF pos = event.localOrScenePos();
  const float mouse_x = pos.x();
  const float mouse_y = pos.y();

  printf("           SCENE press: %f %f\n", mouse_x, mouse_y);
  
  _start_mouse_pos = QCursor::pos(); //pos;

  if(event.button()==Qt::MidButton) {
    
    g_is_dragging_everything = true;
    g_startpos_scrollbar_x = g_mixer->view->horizontalScrollBar()==NULL ? 0 : g_mixer->view->horizontalScrollBar()->value();
    g_startpos_scrollbar_y = g_mixer->view->verticalScrollBar()==NULL ? 0 : g_mixer->view->verticalScrollBar()->value();

    event.accept();

    g_mixer->view->viewport()->setCursor(Qt::OpenHandCursor);
    return true;    
  }
  
  QGraphicsItem *item = itemAt(pos, QTransform());
  
  //printf("mouse button: %d %d\n",event->button(),Qt::MiddleButton);
  
  root->song->tracker_windows->must_redraw = true;

  g_is_ctrl_clicking_this_cycle = (event.modifiers() & Qt::ControlModifier);

  if (g_is_ctrl_clicking_this_cycle){
    Chip *chip = dynamic_cast<Chip*>(item);
      
    if(chip!=NULL)
      chip->mySetSelected(!chip->isSelected());

    event.accept();
    
    return true;
  }
  
  bool ctrl_pressed = (event.modifiers() & Qt::ControlModifier);

  if(event_can_delete(event)) {

    if(mousepress_delete_chip(this,item,mouse_x,mouse_y)==true) {
      event.accept();
      return true;
    }

    if(mousepress_delete_connection(this,event,item,mouse_x,mouse_y)==true)
      return true;
  }
  
  if (event.button()==Qt::RightButton || (ctrl_pressed==false && event.button()==Qt::LeftButton && dynamic_cast<Chip*>(item)!=NULL && dynamic_cast<Chip*>(item)->get_hover_item()==Chip::HoverItem::NAME)){

    /*
    if(ctrl_pressed==false && mousepress_create_chip(this,event,item,mouse_x,mouse_y)==true) { // create
      _chip_was_created = true;
      event->accept();
      return;
    }
    */
    
    // start moving chip    
    Chip *chip = dynamic_cast<Chip*>(item);
      
    if(chip!=NULL){

      {
        struct Instruments *instrument = get_audio_instrument();
        SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
        volatile struct Patch *patch = plugin->patch;
        if(patch==NULL)
          R_ASSERT(false);
        else
          instrument->PP_Update(instrument,(struct Patch*)patch,false);
      }

      //printf("                                                    ^^^^^^^^^^^^ 222 mousepress_select_chip\n");
      EVENTLOG_add_event("start_moving_chips called from MyScene::mousePressEvent");
      start_moving_chips(this,event,chip,mouse_x,mouse_y);
      event.accept();
      return true;
    }

    //if(mousepress_select_chip(this,event,item,mouse_x,mouse_y,ctrl_pressed)==true) // select
    //  return;
    
  } else if(event.button()==Qt::LeftButton) {
    
    if(ctrl_pressed==false && mousepress_start_connection(this,event,item,mouse_x,mouse_y)==true)
      return true;

    if(ctrl_pressed==true && mousepress_select_chip(this,event,item,mouse_x,mouse_y,ctrl_pressed)==true) // select
      return true;

    if (dynamic_cast<Chip*>(item) == NULL) {
      
      clearSelection();
      g_is_moving_rubberband = true;
      g_start_rubberband = pos;
      
      _rubber_band = new QRubberBand(QRubberBand::Rectangle, g_mixer->view->viewport());
      QPoint p(g_mixer->view->mapFromScene(pos));
      _rubber_band->setGeometry(QRect(p, p));
      _rubber_band->show();
      
    }
    
  } 
  if(event.button()!=Qt::RightButton && event.is_real_event())
    QGraphicsScene::mousePressEvent(event.get_qtscene_event());

  return true;
}

static Chip *handle_temp_connection_chip(MyScene *myscene, bool is_output, SuperConnection *current_connection, int x1, int y1, int x2, int y2){
  
  Chip *chip = MW_get_chip_at(x2, y2, NULL);

  if (chip != NULL) {
    
    if (current_connection->_from != NULL && chip==current_connection->_from)
      chip=NULL;
    
    if (current_connection->_to != NULL && chip==current_connection->_to)
      chip=NULL;
  }
  
  if (chip != myscene->_current_temp_connection_chip.data()) {
    
    if (chip != NULL) {

      if (current_connection->_is_event_connection) {
        
        chip->set_hover_item(Chip::HoverItem::EVENT_PORT);
        
      } else {
        
        if(is_output){
          chip->set_hover_item(Chip::HoverItem::AUDIO_INPUT_PORT);
        }else{
          chip->set_hover_item(Chip::HoverItem::AUDIO_OUTPUT_PORT);
        }
        
      }
    }
    
    if (myscene->_current_temp_connection_chip.data() != NULL)
      myscene->_current_temp_connection_chip->set_hover_item(Chip::HoverItem::NOTHING);
    
    myscene->_current_temp_connection_chip = chip;

  }
  
  if (chip != NULL) {
      
    if (is_output) {
      x2 = current_connection->_is_event_connection ? CHIP_get_eport_x(chip) : CHIP_get_input_port_x(chip);
      y2 = current_connection->_is_event_connection ? CHIP_get_input_eport_y(chip) : CHIP_get_port_y(chip);
    } else {
      x2 = current_connection->_is_event_connection ? CHIP_get_eport_x(chip) : CHIP_get_output_port_x(chip);
      y2 = current_connection->_is_event_connection ? CHIP_get_output_eport_y(chip) : CHIP_get_port_y(chip);
    }

  }
    
  current_connection->setLine(x1,y1,x2,y2);
  current_connection->update_shape(true, true);
    
  return chip;
}

void MyScene::fix_mouseMoveEvent (radium::MouseCycleEvent &event){
  FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);

  API_unregister_last_mouse_move_event();
  
  //printf("  2. MouseMove. Ctrl-clicking: %d\n", g_is_ctrl_clicking_this_cycle);
  
  if (g_is_ctrl_clicking_this_cycle){
    event.accept();
    return;
  }
  
  _mouse_has_moved = true;

  //MOUSE_CYCLE_move(g_mixer_widget, get_qmouseevent(event));
  
  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();

  QPointF pos=event.localOrScenePos();

  //printf("           SCENE move: %f %f\n", pos.x(), pos.y());
  
  if(_current_connection != NULL){

    int x1,y1, x2, y2;

    if(_current_from_chip != NULL){
      x1 = CHIP_get_output_port_x(_current_from_chip);
      y1 = CHIP_get_port_y(_current_from_chip);
      x2 = pos.x();
      y2 = pos.y();
    }else{
      x1 = pos.x();
      y1 = pos.y();
      x2 = CHIP_get_input_port_x(_current_to_chip);
      y2 = CHIP_get_port_y(_current_to_chip);
    }

    handle_temp_connection_chip(this, _current_from_chip != NULL, _current_connection, x1, y1, x2, y2);
  
    event.accept();

  }else if(_current_econnection != NULL){

    int x1,y1, x2, y2;
    
    if(_ecurrent_from_chip != NULL){
      x1 = CHIP_get_eport_x(_ecurrent_from_chip);
      y1 = CHIP_get_output_eport_y(_ecurrent_from_chip);
      x2 = pos.x();
      y2 = pos.y();
    }else{
      x1 = pos.x();
      y1 = pos.y();
      x2 = CHIP_get_eport_x(_ecurrent_to_chip);
      y2 = CHIP_get_input_eport_y(_ecurrent_to_chip);
    }

    handle_temp_connection_chip(this, _ecurrent_from_chip != NULL, _current_econnection, x1, y1, x2, y2);

    event.accept();

  } else if(_moving_chips.size()>0){

    move_moving_chips(this,pos.x(),pos.y());

  } else if (g_is_moving_rubberband) {

    QPainterPath path;
    path.addRect(QRectF(g_start_rubberband, pos));
    
    setSelectionArea(path, QTransform());

    QRectF rect(g_mixer->view->mapFromScene(g_start_rubberband), g_mixer->view->mapFromScene(pos));
    
    _rubber_band->setGeometry(rect.toRect().normalized());
    
    //printf(".......................Selection set to\n");

  } else if (g_is_dragging_everything) {

    g_mixer->view->viewport()->setCursor(Qt::ClosedHandCursor);
    
    QPointF end_mouse_pos = QCursor::pos();
    qreal dx = _start_mouse_pos.x() - end_mouse_pos.x();
    qreal dy = _start_mouse_pos.y() - end_mouse_pos.y();

    g_mixer->view->gakk(g_startpos_scrollbar_x, g_startpos_scrollbar_y, dx, dy);
    
  }else{

    draw_slot(this, pos.x(), pos.y());

    auto *sevent = event.get_qtscene_event();
    if (sevent != NULL)
      QGraphicsScene::mouseMoveEvent(sevent);
    
  }

}

bool MyScene::fix_mouseReleaseEvent(radium::MouseCycleEvent &event){
  //MOUSE_CYCLE_unregister(g_mixer_widget);

  //printf("  3. MouseRelease. Ctrl-clicking: %d\n", g_is_ctrl_clicking_this_cycle);
  
  g_is_pressed = false;

  //printf("mouse release: %p\n",_current_connection);

  EVENTLOG_add_event(talloc_format("<<<< MyScene::mouseReleaseEvent. has_undo: %d, runs_custom_exec: %d, _current_connection: %p, _current_econnection: %p, _moving_chips.size(): %d", (int)Undo_Is_Open(), (int)g_radium_runs_custom_exec, _current_connection, _current_econnection, (int)_moving_chips.size()));

  root->song->tracker_windows->must_redraw = true; // why?

  QPointF pos=event.localOrScenePos();
  float mouse_x = pos.x();
  float mouse_y = pos.y();

  //printf("           SCENE release: %f %f. Has moved: %d\n", mouse_x, mouse_y, _mouse_has_moved);

  if (g_is_ctrl_clicking_this_cycle){
    g_is_ctrl_clicking_this_cycle = false;
    event.accept();
    return true;
  }
  

  Chip *chip = MW_get_chip_at(mouse_x, mouse_y, NULL);

  bool must_accept = false;
  
  if(_current_connection!=NULL){

    R_ASSERT(_current_econnection==NULL);
    R_ASSERT(_moving_chips.size()==0);

    if (!_mouse_has_moved){
      if (_current_from_chip!=NULL){
        struct Patch *patch = CHIP_get_patch(_current_from_chip);
        GFX_PP_Update_even_if_locked(patch, false);
      } else if (_current_to_chip!=NULL){
        struct Patch *patch = CHIP_get_patch(_current_to_chip);
        GFX_PP_Update_even_if_locked(patch, false);
      }
    }
    
    if(chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_current_from_chip != NULL && chip != _current_from_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        if (!CHIP_connect_chips(this, _current_from_chip, chip, ConnectionType::IS_SEND))
          UNDO_CANCEL_LAST_UNDO();

      }else if(_current_to_chip != NULL && chip != _current_to_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        if (!CHIP_connect_chips(this, chip, _current_to_chip, ConnectionType::IS_SEND))
          UNDO_CANCEL_LAST_UNDO();
      }
    }

    removeItem(_current_connection);
    delete _current_connection;     
    _current_connection = NULL;
    _current_from_chip = NULL;
    _current_to_chip = NULL;

    if (_current_temp_connection_chip.data() != NULL) {
      _current_temp_connection_chip->set_hover_item(Chip::HoverItem::NOTHING);
      _current_temp_connection_chip = NULL;
    }
          
    must_accept = true;
    
  } else if (_current_econnection!=NULL){

    R_ASSERT(_moving_chips.size()==0);

    if (!_mouse_has_moved){
      if (_ecurrent_from_chip!=NULL){
        struct Patch *patch = CHIP_get_patch(_ecurrent_from_chip);
        GFX_PP_Update_even_if_locked(patch, false);
      } else if (_ecurrent_to_chip!=NULL){
        struct Patch *patch = CHIP_get_patch(_ecurrent_to_chip);
        GFX_PP_Update_even_if_locked(patch, false);
      }
    }

    if(can_internal_data_be_accessed_questionmark_safer()==true && chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_ecurrent_from_chip != NULL && chip != _ecurrent_from_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        if (!CHIP_econnect_chips(this, _ecurrent_from_chip, chip))
          UNDO_CANCEL_LAST_UNDO();

      }else if(_ecurrent_to_chip != NULL && chip != _ecurrent_to_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        if (!CHIP_econnect_chips(this, chip, _ecurrent_to_chip))
          UNDO_CANCEL_LAST_UNDO();

      }
    }

    removeItem(_current_econnection);
    delete _current_econnection;     
    _current_econnection = NULL;
    _ecurrent_from_chip = NULL;
    _ecurrent_to_chip = NULL;

    if (_current_temp_connection_chip.data() != NULL) {
      _current_temp_connection_chip->set_hover_item(Chip::HoverItem::NOTHING);
      _current_temp_connection_chip = NULL;
    }
    
    must_accept = true;
    
  } else if (_moving_chips.size()>0 && stop_moving_chips(this, g_is_ctrl_clicking_this_cycle, pos)) {

    //printf("       Remake: mousereleaseevent\n");
    remakeMixerStrips(make_instrument(-2));
    
    must_accept = true;
    
  } else if (g_is_moving_rubberband) {

    g_is_moving_rubberband = false;
    must_accept = true;
    delete _rubber_band;
    
  } else if (g_is_dragging_everything) {

    g_mixer->view->viewport()->setCursor(Qt::ArrowCursor);
    g_is_dragging_everything = false;
    must_accept = true;
    
  }

  if (must_accept){
    event.accept();
    return true;
  }


  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2(true);

  
  bool ctrl_pressed = (event.modifiers() & Qt::ControlModifier);
  bool shift_pressed = (event.modifiers() & Qt::ShiftModifier);
  
  if (event.button()==Qt::RightButton && shift_pressed==false && ctrl_pressed==false){
    
    bool chip_is_under = false;
    
    Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
    if(chip_under!=NULL){
      //        if(chip_body_is_placed_at(chip_under, mouse_x, mouse_y)==true)
      chip_is_under = true;
    }
    
    if(chip_is_under==false && !MOUSE_CYCLE_has_moved()){

      if (SuperConnection::get_current_connection() != NULL) {
        mouserelease_show_connection_popup(SuperConnection::get_current_connection());
      } else {
        mouserelease_create_chip(this,mouse_x,mouse_y);
      }
      
      event.accept();
      return true;
    }
    
    if(mousepress_save_presets_etc(this,event,mouse_x,mouse_y)==true){
      event.accept();
      return true;
    }
    /*
      && mouserelease_replace_patch(this,mouse_x,mouse_y)==true) {
      event->accept();
      return;
    */
  }

  //if(event->button()!=Qt::RightButton)
  auto *sevent = event.get_qtscene_event();
  if (sevent != NULL)
    QGraphicsScene::mouseReleaseEvent(sevent);

  return true;
}


DEFINE_ATOMIC(bool, g_show_cpu_usage_in_mixer) = false;

radium::MixerWidget *g_mixer_widget = NULL;

namespace{
  struct MixerWidgetTimer {

    int64_t counter = 0;

    void maybe_schedule_update_audio_connection(AudioConnection *connection, const float db) const {
      float connection_db = db;
      
      if (connection->to != NULL) {
        
        bool link_is_enabled;
        const char *error = NULL;
        float link_gain = SP_get_actual_link_gain_and_enabled(connection->to->_sound_producer, connection->from->_sound_producer, &error, link_is_enabled);
        if (error != NULL)
          printf("\n\n\n\n=======================ERROR: %s\n\n\n\n\n", error);
        /*
          if (!equal_floats(link_gain, 1.0) && !equal_floats(link_gain, 0.0)){
          printf("      LINK_GAIN %s ->%s: %f.  db: %f\n", CHIP_get_patch(connection->from)->name, CHIP_get_patch(connection->to)->name, link_gain, gain2db(link_gain));
          }
        */
        if (error==NULL){
          if (!link_is_enabled)
            connection_db = MIN_DB;
          else
            connection_db = R_BOUNDARIES(MIN_DB, connection_db + gain2db(link_gain), MAX_DB);
        }
      }
      
      constexpr double gakk = 0.05;
      
      static_assert(MIN_DB_A_LITTLE_BIT_ABOVE - MIN_DB > gakk, "wrong");
      
      if (fabs(connection_db - connection->_last_displayed_db_peak) > gakk){                      
#if 0
        static int num = 0;
        printf("    %d: Update \"%s\" -> \"%s\". Old: %f. New: %f\n",
               num++,
               CHIP_get_patch(connection->from)->name, connection->to==NULL ? "(NULL)" : CHIP_get_patch(connection->to)->name,
               connection->_last_displayed_db_peak,
               connection_db);
#endif                 
        connection->_last_displayed_db_peak = connection_db;
        connection->schedule_update_shape(false, true);
      }

    }
    
    void call_each_16ms(double ms) {
      counter++;

      RETURN_IF_DATA_IS_INACCESSIBLE();

      if (modular_mixer_visible()){
        
        bool less_frequently_updated = (counter % 3) == 0;
        
        bool show_cpu_update = less_frequently_updated && ATOMIC_GET_RELAXED(g_show_cpu_usage_in_mixer);
              
        for (QGraphicsItem *das_item : g_mixer->scene.items()){
          
          Chip *chip = dynamic_cast<Chip*>(das_item);
          
          if(chip!=NULL){
            
            SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
            
            if(plugin != NULL){

              if (show_cpu_update){
                CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET(plugin->cpu_usage);
                
                if (cpu_usage==NULL || cpu_usage->should_update() || chip->_name_text!=cpu_usage->_last_cpu_text)
                  chip->update();
              }

              // Unfortunately QGraphicsItem::setSelected is not virtual. Workaround.
              if (chip->isSelected() != plugin->is_selected){
                chip->mySetSelected(chip->isSelected());
              }
              
              struct Patch *patch = plugin->patch;
              
              if(patch!=NULL){

                if (less_frequently_updated) {
                  float volume = chip->get_slider_volume();
                  bool is_muted = is_muted_relaxed(plugin);
                  bool is_bypass = !ATOMIC_GET_RELAXED(plugin->effects_are_on);
                  bool is_recording = ATOMIC_GET_RELAXED(patch->is_recording);
                  bool is_autosuspending = ATOMIC_GET_RELAXED(plugin->_is_autosuspending);
                  
                  //printf("last: %f. vol: %f. Equal? %d\n", chip->_last_updated_volume, volume, chip->_last_updated_volume == volume);
                  if (!equal_floats(chip->_last_updated_volume, volume)){
                    chip->_last_updated_volume = volume;
                    CHIP_update(chip, plugin);
                  }
                  
                  if (chip->_last_updated_mute != is_muted){
                    chip->_last_updated_mute = is_muted;
                    chip->update();
                  }

                  if (chip->_last_updated_bypass != is_bypass){
                    chip->_last_updated_bypass = is_bypass;
                    chip->update();
                    for(auto connection : chip->_output_audio_connections)
                      connection->update_shape(true, true); // Update implitly disabled connection caused by bypass.
                  }
                  
                  if (chip->_last_updated_recording != is_recording) {
                    chip->_last_updated_recording = is_recording;
                    chip->update();
                  }
                  
                  // turn on is_autosuspending (only gfx)
                  if (chip->_last_updated_autosuspending==true &&  is_autosuspending==false){
                    chip->update();
                    chip->_last_updated_autosuspending = false;
                    //printf("Turned off autosuspending for %s\n", patch->name);
                  }

                  if (is_autosuspending==false)
                    chip->_autosuspend_on_time = counter;
                  
                  // turn off is_autosuspending (only gfx) (wait at least one second to turn if on, to avoid flickering)
                  if (chip->_last_updated_autosuspending==false && is_autosuspending==true){
                    //printf("Turned on autosuspending for %s (%f)\n", patch->name, (TIME_get_ms() - _autosuspend_on_time));
                    if ( (counter - chip->_autosuspend_on_time) > (1000 / ms)){
                      chip->_last_updated_autosuspending = true;
                      chip->update();
                    }
                  }
                }

                if (plugin->type->num_outputs>0 && chip->_output_audio_connections.size() > 0){

                  float db = MIN_DB;

                  for(int ch=0;ch<plugin->type->num_outputs;ch++)
                    db = R_MAX(db, plugin->output_volume_peaks.decaying_dbs_10x[ch]);
                  
                  for(auto *connection : chip->_output_audio_connections)
                    maybe_schedule_update_audio_connection(connection, db);

                  if (g_mixer->scene._current_connection != NULL && g_mixer->scene._current_connection->from==chip)
                    maybe_schedule_update_audio_connection(g_mixer->scene._current_connection, db);
                }
                
                {
                  int note_intencity = ATOMIC_GET_RELAXED(patch->visual_note_intencity);
                  if(note_intencity != chip->_last_note_intencity){
                    chip->_last_note_intencity = note_intencity;
                    //printf("intencity: %d\n",ATOMIC_GET(patch->visual_note_intencity));
                    int x1,y1,x2,y2;
                    CHIP_get_note_indicator_coordinates(x1,y1,x2,y2);
                    chip->update(x1,y1,x2-x1,y2-y1);

                    for(auto *connection : chip->_output_event_connections)
                      connection->schedule_update_shape(true, true);
                    
                    for(auto *connection : chip->_output_audio_connections)
                      connection->schedule_update_shape(true, false);
                    
                    if (g_mixer->scene._current_econnection != NULL && g_mixer->scene._current_econnection->from==chip)
                      g_mixer->scene._current_econnection->schedule_update_shape(true, true);
                    
                    if (g_mixer->scene._current_connection != NULL && g_mixer->scene._current_connection->from==chip)
                      g_mixer->scene._current_connection->schedule_update_shape(true, false);                  
                  }
                }

                if (g_mixer->scene._current_econnection != NULL)
                  g_mixer->scene._current_econnection->apply_update_shapes();
                
                if (g_mixer->scene._current_connection != NULL)
                  g_mixer->scene._current_connection->apply_update_shapes();

                for(auto *connection : chip->_output_event_connections)
                  connection->apply_update_shapes();
                
                for(auto *connection : chip->_output_audio_connections)
                  connection->apply_update_shapes();

                if (chip->_input_slider != NULL)
                  SLIDERPAINTER_call_regularly(chip->_input_slider, -1);
                
                if (chip->_output_slider != NULL)
                  SLIDERPAINTER_call_regularly(chip->_output_slider, plugin->num_visible_outputs);
              }
            }
          }
        }
      }
    }
  };

  static MixerWidgetTimer g_mixer_widget_timer;
}

void MW_call_each_16ms(double ms){
  g_mixer_widget_timer.call_each_16ms(ms);
}

MixerWidget_OnlyMixer::MixerWidget_OnlyMixer(QWidget *parent)
  : radium::KeyboardFocusFrame(parent, radium::KeyboardFocusFrameType::MIXER, true)
  , scene(this)
{

  if(g_mixer!=NULL){
    fprintf(stderr,"Error. More than one MixerWidget_OnlyMixer created.\n");
    abort();
  }

  g_mixer = this;
  //g_static_toplevel_widgets.push_back(this);

  Mixer_widget *mixer_widget = new Mixer_widget(this);
  mixer_widget->view->setScene(&scene);

  layout()->addWidget(mixer_widget);
  this->view = mixer_widget->view;

  //this->view->setDragMode(QGraphicsView::ScrollHandDrag);
  this->view->setDragMode(QGraphicsView::NoDrag);

  setWindowTitle(tr("Radium Mixer"));

  //this->view->installEventFilter(this->view);
  //qApp->installEventFilter(this);
}

bool GFX_MixerIsVisible(void){
  return !g_mixer_widget->isHidden();
}
void GFX_ShowMixer(void){
  if (sequencerInFullMode() && !sequencerInWindow()){
    setSequencerInFullMode(false);    
  }
  
  GL_lock();{
    setMixerKeyboardFocus(true);
    g_mixer_widget->show();
  }GL_unlock();
}
void GFX_HideMixer(void){
  g_mixer_widget->hide();
  set_editor_focus();
}

void GFX_showHideMixerWidget(void){
  bool must_show = false;

  if (sequencerInMainTabs()){
    if (sequencerInFullMode() && !sequencerInWindow()){
      setSequencerInFullMode(false);
      must_show = true;
    }
  }

  GL_lock();{
    if(g_mixer_widget->isHidden()){
      setMixerKeyboardFocus(true);
      g_mixer_widget->show();
    } else {
      if (!must_show){
        g_mixer_widget->hide();
        FOCUSFRAMES_set_focus_best_guess();
      } else {
        setMixerKeyboardFocus(true);
      }
    }
  }GL_unlock();
  set_editor_focus();
}

static const instrument_t g_main_pipe_patch_id = make_instrument(0);

struct Patch *get_main_pipe_patch(void){
  return PATCH_get_from_id(g_main_pipe_patch_id);
}
  
instrument_t get_main_pipe_patch_id(void){
  return g_main_pipe_patch_id;
}
  
SoundPlugin *get_main_pipe(void){
  struct Patch *patch = get_main_pipe_patch();
  if (patch==NULL)
    return NULL;
  
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin;
  /*
  QList<QGraphicsItem *> das_items = g_mixer->scene.items();
  for (int i = 0; i < das_items.size(); ++i){
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return plugin;
    }
  }

  RError("no main pipe");
  return NULL;
  */
}


void MW_connect_plugin_to_main_pipe(SoundPlugin *plugin){
  SoundPlugin *main_pipe = get_main_pipe();
  R_ASSERT_RETURN_IF_FALSE(main_pipe!=NULL);
  
  if(plugin->type->num_outputs>0)
    CHIP_connect_chips(&g_mixer->scene, plugin, main_pipe, ConnectionType::IS_SEND);
}


static float find_next_autopos_y(Chip *system_chip){
  int x = system_chip->x()-(grid_width*3/2);
  int y = system_chip->y()+2;
  while(MW_get_chip_at(x,y,NULL)!=NULL)
    y+=grid_height;
  return y-2;
}

void MW_set_autopos(double *x, double *y){
  SoundPlugin *main_pipe    = get_main_pipe();
  if (main_pipe != NULL){
    Chip        *system_chip  = find_chip_for_plugin(&g_mixer->scene, main_pipe);
    *x                         = system_chip->x()-(grid_width*3/2);
    *y                         = find_next_autopos_y(system_chip);
    printf("Adding at pos %f %f\n",*x,*y);
  } else {
    *x = 0.0;
    *y = 0.0;
  }
}


/*
namespace{
  struct MyQAction : public QAction{
    MyQAction(QString name, QMenu *menu, const PluginMenuEntry entry, bool must_have_inputs, bool must_have_outputs)
      : QAction(name,menu)
      , entry(entry)
    {
      SoundPluginType *type = entry.plugin_type;

      if (entry.plugin_type!=NULL){
        if (must_have_inputs==true && type->num_inputs==0)
          setEnabled(false);
        if (must_have_outputs==true && type->num_outputs==0)
          setEnabled(false);
      }
    }
    const PluginMenuEntry entry;
  };
}

static int menu_up(QMenu *menu, const QVector<PluginMenuEntry> &entries, int i, bool include_load_preset, bool must_have_inputs, bool must_have_outputs){
  while(i < entries.size()){
    //printf("%d\n",i);
    const PluginMenuEntry &entry = entries[i];
    i++;

    if(entry.type==PluginMenuEntry::IS_SEPARATOR){
      menu->addSeparator();

    }else if(entry.type==PluginMenuEntry::IS_LEVEL_UP){
      QString name = entry.level_up_name;
      QMenu *new_menu = new QMenu(name,menu);
      menu->addMenu(new_menu);
      i = menu_up(new_menu, entries, i, include_load_preset, must_have_inputs, must_have_outputs);

    }else if(entry.type==PluginMenuEntry::IS_LEVEL_DOWN){
      return i;

    }else if(entry.type==PluginMenuEntry::IS_CONTAINER && entry.plugin_type_container->is_populated){
      SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;

      for(int i=0 ; i < plugin_type_container->num_types ; i++){
        SoundPluginType *type = plugin_type_container->plugin_types[i];
        const char *name = type->name;
        menu->addAction(new MyQAction(name,menu,PluginMenuEntry::normal(type), must_have_inputs, must_have_outputs));
      }
    
    }else if(entry.type==PluginMenuEntry::IS_CONTAINER){
      SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;
      const char *name = plugin_type_container->name;
      menu->addAction(new MyQAction(name,menu,entry, must_have_inputs, must_have_outputs));

    }else if(entry.type==PluginMenuEntry::IS_LOAD_PRESET){

      MyQAction *action = new MyQAction("Load Preset(s)", menu, entry, must_have_inputs, must_have_outputs);
      menu->addAction(action);
      if (include_load_preset==false)
        action->setEnabled(false);

    }else if(entry.type==PluginMenuEntry::IS_PASTE_PRESET){

      MyQAction *action = new MyQAction("Paste sound object(s)", menu, entry, must_have_inputs, must_have_outputs);
      menu->addAction(action);
      menu->addSeparator();
      if (include_load_preset==false || PRESET_has_copy()==false)
        action->setEnabled(false);

    }else if(entry.type==PluginMenuEntry::IS_NUM_USED_PLUGIN){

      MyQAction *action = new MyQAction(entry.hepp.menu_text, menu, entry, must_have_inputs, must_have_outputs);
      menu->addAction(action);

    }else{
      const char *name = entry.plugin_type->name;
      menu->addAction(new MyQAction(name,menu,entry, must_have_inputs, must_have_outputs));
    }
  }

  return i;
}

static char *create_selector_text(SoundPluginType *type){
  return talloc_format(
                       "1%s:%s",
                       STRING_get_chars(STRING_toBase64(STRING_create(type->type_name))),
                       STRING_get_chars(STRING_toBase64(STRING_create(type->name)))
                       );
}
*/



/*
static const char *popup_plugin_selector(SoundPluginType **type, bool must_have_inputs, bool must_have_outputs){
  QMenu menu(0);

  if (type!=NULL)
    *type = NULL;

  menu_up(&menu, PR_get_menu_entries(), 0, type==NULL, must_have_inputs, must_have_outputs);
  printf("Menu created\n");
  
  MyQAction *action;

  action = dynamic_cast<MyQAction*>(safeExec(&menu, true));
  printf("action: %p\n",action);
  
  if (action==NULL)
    return NULL;

  const PluginMenuEntry entry = action->entry;

  if (entry.type==PluginMenuEntry::IS_CONTAINER) {
    vector_t names={};

    SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;
    plugin_type_container->populate(plugin_type_container);

    if (plugin_type_container->num_types==0) {
      //GFX_Message(NULL, talloc_format("%s does not contain any plugin",plugin_type_container->name));
      return NULL; // no error message here. populate() must do that.
    }
     
    if (plugin_type_container->num_types==1) {
      if (type!=NULL)
        *type = plugin_type_container->plugin_types[0];

      return create_selector_text(plugin_type_container->plugin_types[0]);
    }
    
    for(int i=0 ; i < plugin_type_container->num_types ;  i++)
      VECTOR_push_back(&names,plugin_type_container->plugin_types[i]->name);

    char temp[1024];
    sprintf(temp,"Select plugin contained in %s", plugin_type_container->name);
    int selection=GFX_Menu(NULL,NULL,temp,names,true);
    
    if (selection==-1)
      return NULL;
    else {
      if (type!=NULL)
        *type = plugin_type_container->plugin_types[selection];
      return create_selector_text(plugin_type_container->plugin_types[selection]);
    }
    
  }else if(entry.type==PluginMenuEntry::IS_LOAD_PRESET){
    //return PRESET_request_load_instrument_description();
    return "";
    
  }else if(entry.type==PluginMenuEntry::IS_PASTE_PRESET){
    return "3";
 
  }else if(entry.type==PluginMenuEntry::IS_NUM_USED_PLUGIN){
    SoundPluginType *type2 = PR_get_plugin_type_by_name(entry.hepp.container_name.toUtf8().constData(), entry.hepp.type_name.toUtf8().constData(), entry.hepp.name.toUtf8().constData());

    if (type2 != NULL){
      if (type!=NULL)
        *type = type2;
      return create_selector_text(type2);
    }else
      return NULL;

  } else {
    
    if (type!=NULL)
      *type = entry.plugin_type;

    return create_selector_text(entry.plugin_type);

  }
}
                                  
const char *MW_popup_plugin_selector2(bool must_have_inputs, bool must_have_outputs){
  return popup_plugin_selector(NULL, must_have_inputs, must_have_outputs);
}

SoundPluginType *MW_popup_plugin_type_selector(bool must_have_inputs, bool must_have_outputs){
  SoundPluginType *type;
  popup_plugin_selector(&type, must_have_inputs, must_have_outputs);

  if (type != NULL)
    PR_inc_plugin_usage_number(type);

  return type;
}
*/

void MW_connect(Patch *source, Patch *dest, ConnectionType connection_type){
  Chip *chip_source = CHIP_get(&g_mixer->scene, source);
  R_ASSERT_RETURN_IF_FALSE(chip_source!=NULL);
  
  Chip *chip_dest = CHIP_get(&g_mixer->scene, dest);
  R_ASSERT_RETURN_IF_FALSE(chip_dest!=NULL);

  CHIP_connect_chips(&g_mixer->scene, chip_source, chip_dest, connection_type);
}

bool MW_disconnect(Patch *source, Patch *dest){

  QGraphicsScene *scene = &g_mixer->scene;
    
  Chip *chip_from = CHIP_get(scene, source);
  Chip *chip_to = CHIP_get(scene, dest);
  
  return CHIP_disconnect_chips(scene, chip_from, chip_to);
}

void MW_econnect(Patch *source, Patch *dest){
  Chip *chip_source = CHIP_get(&g_mixer->scene, source);
  R_ASSERT_RETURN_IF_FALSE(chip_source!=NULL);
  
  Chip *chip_dest = CHIP_get(&g_mixer->scene, dest);
  R_ASSERT_RETURN_IF_FALSE(chip_dest!=NULL);

  CHIP_econnect_chips(&g_mixer->scene, chip_source, chip_dest);
}

bool MW_edisconnect(Patch *source, Patch *dest){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
  for (int i = 0; i < das_items.size(); ++i) {
    EventConnection *connection = dynamic_cast<EventConnection*>(das_items.at(i));
    if(connection!=NULL){
      Patch *a = CHIP_get_patch(connection->from);
      Patch *b = CHIP_get_patch(connection->to);
      if (a==source && b==dest) {
        CONNECTION_delete_event_connection(connection);
        return true;
      }
    }
  }

  return false;
}

bool MW_are_connected(Patch *source, Patch *dest){
  Chip *a = CHIP_get(&g_mixer->scene, source);
  Chip *b = CHIP_get(&g_mixer->scene, dest);

  return CHIPS_are_connected(a, b);
}

bool MW_are_econnected(Patch *source, Patch *dest){
  Chip *a = CHIP_get(&g_mixer->scene, source);
  Chip *b = CHIP_get(&g_mixer->scene, dest);

  return CHIPS_are_econnected(a, b);
}


#if 0
static bool delete_a_connection(){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Connection *connection = dynamic_cast<Connection*>(das_items.at(i));
    if(connection!=NULL){
      CONNECTION_delete_connection(connection);
      return true;
    }
  }
  return false;
}
#endif

static void MW_cleanup_connections(bool is_loading){
  if (is_loading)
    GFX_ShowProgressMessage("Deleting all connection between instruments", true);

  CONNECTIONS_remove_all(&g_mixer->scene);
}



static bool delete_a_chip(bool is_loading){
  bool ret = false;
  
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  UNDO_OPEN_REC();{
    
    for (int i = 0; i < das_items.size(); ++i) {
      Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
      if(chip!=NULL){
        printf("  MAKING %p inactive (%s), by force\n",chip,CHIP_get_patch(chip)->name);

        if (is_loading)
          GFX_ShowProgressMessage(talloc_format("Deleting instrument %s", CHIP_get_patch(chip)->name), true);

        PATCH_force_make_inactive(CHIP_get_patch(chip));
        //MW_delete_plugin(SP_get_plugin(chip->_sound_producer));
        ret = true;
        break;
      }
    }

  } UNDO_CLOSE();

  return ret;
}

void MW_cleanup(bool is_loading){
  Undo_start_ignoring_undo_operations();{

    MW_reset_ab(-1);
    
    MW_cleanup_connections(is_loading); // Calling this function is not necessary since all connecting connections are deleted when deleting a chip (which happens in the next line), but it's a lot faster to delete all connections in one go than deleting them one by one since we only have to wait for the audio thread one time.
    
    while(delete_a_chip(is_loading)); // remove all chips. All connections are removed as well when removing all chips.
    
    MW_update_mixer_widget(true);
    
  }Undo_stop_ignoring_undo_operations();
}

static void get_patches_min_x_y(const vector_t *patches, float &min_x, float &min_y){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();
  
  min_x = 0;
  min_y = 0;

  bool sat=false;
  
  for (int i = 0; i < das_items.size(); ++i) {
    
    const Chip *chip = dynamic_cast<const Chip*>(das_items.at(i));
    
    if(chip!=NULL) {
      
      const struct Patch *patch = CHIP_get_patch(chip);

      if (!patch->is_visible)
        continue;
      
      if (patches==NULL || VECTOR_is_in_vector(patches, patch)){
        
        if(sat==false){
          
          min_x = chip->x();
          min_y = chip->y();
          sat = true;
          
        } else {
          
          if (chip->x() < min_x)
            min_x = chip->x();
          if (chip->y() < min_y)
            min_y = chip->y();
          
        }

      }
      
    }
  }     
}

static char *get_patch_key(const struct Patch *patch){
  static char *temp = NULL;
  if(temp==NULL)
    temp = (char*)malloc(sizeof(temp) * 128);
  
  snprintf(temp, 110, "%" PRId64, patch->id.id);
  
  return temp;
}


static hash_t *MW_get_audio_patches_state(const vector_t *patches, bool put_in_array){

  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  hash_t *chips = HASH_create(das_items.size()/2);

  float min_x, min_y;
  get_patches_min_x_y(patches, min_x, min_y);

  HASH_put_float(chips, "min_x_pos", min_x);
  HASH_put_float(chips, "min_y_pos", min_y);

  int num_chips=0;
  for (int i = 0; i < das_items.size(); ++i) {
    
    const Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    
    if(chip!=NULL) {
      
      const struct Patch *patch = CHIP_get_patch(chip);

      if (!patch->is_visible)
        continue;

      if (patches==NULL || VECTOR_is_in_vector(patches, patch)){
        
        hash_t *state = AUDIO_get_audio_patch_state(patch);

        if (state!=NULL){ // This should always happen.
          
          // dont need it
          if (patches != NULL)
            HASH_remove(state, "plugin");

          if (put_in_array)
            HASH_put_hash_at(chips, "", num_chips, state);
          else
            HASH_put_hash(chips, get_patch_key(patch), state);
          
          num_chips++;
          
        }
        
      }
    }
  }

  HASH_put_int(chips, "num_chips", num_chips);

  return chips;
}

// returns true if the 'connection' connects two patches in 'patches'. (or 'patches' is NULL)
static bool connection_is_in_patches(SuperConnection *connection, const vector_t *patches){
  if (patches==NULL)
    return true;

  struct Patch *to = CHIP_get_patch(connection->to);
  struct Patch *from = CHIP_get_patch(connection->from);

  int num_hits = 0;
  
  VECTOR_FOR_EACH(struct Patch *, patch, patches){

    R_ASSERT_RETURN_IF_FALSE2(AUDIO_is_permanent_patch(patch)==false, true);

    if(patch==to)
      num_hits++;
    
    if(patch==from)
      num_hits++;

  }END_VECTOR_FOR_EACH;

  if(num_hits==2)
    return true;

  R_ASSERT(num_hits < 2); // really strange if this happens
      
  return false;
}

// Does not return connections to hidden (i.e. temporary) instruments.
static QVector<SuperConnection*> get_connections(bool include_audio, bool include_event){ 
  QVector<SuperConnection*> ret;
  
  const QList<QGraphicsItem *> das_items = g_mixer->scene.items();
  
  for (int i = 0; i < das_items.size(); ++i) {
    SuperConnection *connection = dynamic_cast<SuperConnection*>(das_items.at(i));
    if (connection!=NULL && !connection->_is_connected_to_hidden_instrument)
      if ((include_event && connection->_is_event_connection) || (include_audio && !connection->_is_event_connection))
        if (connection->from!=NULL && connection->to!=NULL) // ongoing connections are not real connections.
          ret.push_back(connection);
  }

  return ret;
}

hash_t *MW_get_connections_state(const vector_t *patches, bool include_audio, bool include_events, bool include_modulator_connections){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  hash_t *connections = HASH_create(das_items.size() + 4);

  HASH_put_bool(connections, "includes_audio", include_audio);
  HASH_put_bool(connections, "includes_events", include_events);
  
  auto super_connections = get_connections(include_audio, include_events);
  int num_connections = 0;
  
  for(auto *connection : super_connections){
    if (connection->to!=NULL && connection->from!=NULL && connection_is_in_patches(connection, patches)){
      hash_t *state = CONNECTION_get_state(connection, patches);
      if (state != NULL)
        HASH_put_hash_at(connections, "", num_connections++, state);
    }
  }

  HASH_put_int(connections, "num_connections", num_connections);

  if (include_modulator_connections)
    HASH_put_dyn(connections, "modulator_connections", MODULATORS_get_connections_state());

  return connections;
}

// Return NULL if the connection doesn't exist. Used by a/b testing to avoid recreating all connections when changing a/b number.
/*
static SuperConnection *get_connection(int64_t id_from, int64_t id_to, bool is_event_connection){
  auto super_connections = get_connections();
  for(auto *connection : super_connections){
    if (true
        && CHIP_get_patch(connection->from)->id == id_from
        && CHIP_get_patch(connection->to)->id == id_to
        && connection->is_event_connection == is_event_connection
        )
      return connection;
  }

  return NULL;
}
*/

static QString get_plugin_type_key(const SoundPluginType *type){
  return QString(type->type_name) + "/" + type->name;
}

#if !defined(RELEASE)
bool g_has_found_all_plugins = true;
#endif

static void set_instance_num(QString type_key, int num){
  auto audio_instrument = get_audio_instrument();
  auto patches = audio_instrument->patches;

  VECTOR_FOR_EACH(struct Patch *, patch, &patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (plugin==NULL)
      R_ASSERT(false);
    else if (get_plugin_type_key(plugin->type) ==  type_key){
        plugin->type->instance_num = num;
        return;
    }
  }END_VECTOR_FOR_EACH;

#if !defined(RELEASE)
  if (g_has_found_all_plugins)
    R_ASSERT(false);
#endif
}

static void MW_apply_num_instances_state(const hash_t *state){
  const dynvec_t dynvec = HASH_get_values(state);
  const vector_t keys = HASH_get_keys(state);

  for(int i = 0 ; i < dynvec.num_elements ; i++){
    if (dynvec.elements[i].type==INT_TYPE)
      set_instance_num((const char*)keys.elements[i], dynvec.elements[i].int_number);
    else
      R_ASSERT(false);
  }

}

static hash_t *MW_get_num_instances_state(void){
  auto audio_instrument = get_audio_instrument();
  auto patches = audio_instrument->patches;

  QSet<SoundPluginType*> has_set;
  
  hash_t *state = HASH_create(patches.num_elements);

  VECTOR_FOR_EACH(struct Patch *, patch, &patches){
    if (patch->is_visible) {
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      if (plugin==NULL)
        R_ASSERT(false);
      else if (!has_set.contains(plugin->type)){
        HASH_put_int(state, get_plugin_type_key(plugin->type).toUtf8().constData(), plugin->type->instance_num);
        has_set.insert(plugin->type);
      }
    }
  }END_VECTOR_FOR_EACH;

  return state;
}

// Only used when saving .rad and .mrec.
hash_t *MW_get_state(const vector_t *patches, bool is_saving_song){
  hash_t *state = HASH_create(6);

  HASH_put_hash(state, "chips", MW_get_audio_patches_state(patches, true));
  HASH_put_hash(state, "connections", MW_get_connections_state(patches, true, true, true));
  
  if (is_saving_song)
    HASH_put_hash(state, "ab_state", MW_get_ab_state());

  HASH_put_dyn(state, "mixer_strips_configuration", MW_get_mixer_strips_state());

  HASH_put_bool(state, "volume_applied_before_drywet", true);

  if (is_saving_song)
    HASH_put_hash(state, "num_instances_state", MW_get_num_instances_state());
  
  return state;
}

// Compatibility with old songs.
static void MW_create_chips_from_full_state(hash_t *chips, Buses buses, bool is_loading){

  R_ASSERT(is_loading);
  
  int64_t num_chips = HASH_get_int(chips, "num_chips");
  printf("number of chips: %d\n",(int)num_chips);
  
  for(int i=0;i<num_chips;i++) {
    hash_t *state = HASH_get_hash_at(chips, "", i);

    struct Patch *patch = PATCH_get_from_id(HASH_get_instrument(state, "patch"));
    if(patch==NULL){
      R_ASSERT(false);
      continue;
    }
    
    if (is_loading)
      GFX_ShowProgressMessage(talloc_format("Creating instrument %d / %d: %s", i, (int)num_chips, patch->name), true);

    PATCH_init_audio_when_loading_song(patch, state);
  }
}

// Only used when loading old songs
static void MW_create_bus_connections_for_all_chips(Buses &buses){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL)
      CHIP_create_bus_connections(chip, buses);
  }
}

// Called from audio/Presest.cpp. (Not used when loading song.)
static void MW_position_chips_from_state(const hash_t *chips, const vector_t *patches, const QHash<instrument_t, instrument_t> &patch_id_mapper, float x, float y){
  
  int64_t num_chips = HASH_get_int(chips, "num_chips");
  //printf("number of chips: %d\n",(int)num_chips);

  R_ASSERT_RETURN_IF_FALSE(patches->num_elements==num_chips);

  float min_x = HASH_get_float(chips, "min_x_pos");
  float min_y = HASH_get_float(chips, "min_y_pos");
  
  for(int i=0;i<num_chips;i++) {
    const hash_t *state = HASH_get_hash_at(chips, "", i);
    //printf("   Chip %d: %d\n", i, (int)HASH_get_instrument(state, "patch").id);

    const instrument_t instrument_old = HASH_get_instrument(state, "patch");

    if (!patch_id_mapper.contains(instrument_old)){
      R_ASSERT_NON_RELEASE(false);
      break;
    }

    const instrument_t instrument_new = patch_id_mapper[instrument_old];
    
    struct Patch *patch = NULL;
    VECTOR_FOR_EACH(struct Patch*, maybe, patches){
      if (maybe->id==instrument_new){
        patch=maybe;
        break;
      }
    }END_VECTOR_FOR_EACH;

    R_ASSERT_NON_RELEASE(patch!=NULL);
    
    if (patch!=NULL)
      CHIP_set_pos(patch, HASH_get_float(state, "x") + x - min_x, HASH_get_float(state, "y") + y - min_y);
  }
}

// Called from undo_mixer_connections.c
void MW_create_connections_from_state(const hash_t *connections){
  radium::Scheduled_RT_functions rt_functions;
  radium::SoloChanges solo_changes(rt_functions);
  CONNECTIONS_replace_all_with_state(&g_mixer->scene, connections,
                                     true, true, true, true,
                                     true,
                                     rt_functions, solo_changes);
}

static void add_undo_for_all_chip_positions(void){
  const QList<QGraphicsItem *> &das_items = g_mixer->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    const Chip *chip = dynamic_cast<const Chip*>(das_items.at(i));
    if(chip!=NULL)
      ADD_UNDO(ChipPos_CurrPos(CHIP_get_patch(chip)));
  }
}

// Called from audio/Presest.cpp. (Not used when loading song.)
void MW_create_from_state(const hash_t *state, const vector_t *patches, const QHash<instrument_t, instrument_t> &patch_id_mapper, float x, float y){
  R_ASSERT(patches != NULL);
  R_ASSERT(Undo_Is_Open() || Undo_Is_Currently_Ignoring());
 
  ADD_UNDO(MixerConnections_CurrPos());
  add_undo_for_all_chip_positions(); // We might kick any chip, so we need to add undo points for all chips
          
  MW_position_chips_from_state(HASH_get_hash(state, "chips"), patches, patch_id_mapper, x, y);

  hash_t *connections = HASH_get_hash(state, "connections");

  CONNECTIONS_create_from_presets_state(&g_mixer->scene, connections, patches, patch_id_mapper);
  
  if (patches->num_elements > 1)
    cleanup_chip_positions(&g_mixer->scene);
}


// FIXME/TODO!

static hash_t *get_chips_and_bus_chips(const hash_t *state){
  hash_t *old_chips = HASH_get_hash(state, "chips");
  int num_old_chips = HASH_get_int32(old_chips, "num_chips");
  
  hash_t *new_chips = HASH_create(17);
  hash_t *buses = HASH_create(5);

  int num_buses = 0;
  int num_chips = 0;
  
  for(int i=0;i<num_old_chips;i++) {
    hash_t *chip = HASH_get_hash_at(old_chips, "", i);
    hash_t *plugin = HASH_get_hash(chip, "plugin");    
    const char *type_name = HASH_get_chars(plugin, "type_name");
    const char *plugin_name = HASH_get_chars(plugin, "name");
    
    if (!strcmp(type_name,"Bus") && STRING_starts_with(STRING_create(plugin_name), "Bus ")) {
      fprintf(stderr, "\n   Bus %d/%d. Id: %d\n", num_buses, i, (int)HASH_get_instrument(chip, "patch").id);
      HASH_put_hash_at(buses, "", num_buses++, chip);
    } else
      HASH_put_hash_at(new_chips, "", num_chips++, chip);
  }

  if (num_buses != 2 && num_buses != 5)
    RError("num_buses should be 2 or 5, not %d. num_chips: %d", num_buses, num_chips);
    
  R_ASSERT(num_buses+num_chips == num_old_chips);
  
  HASH_put_int(new_chips, "num_chips", num_chips);
  HASH_put_int(buses, "num_chips", num_buses);

  hash_t *ret = HASH_create(3);

  HASH_put_hash(ret, "bus_chips", buses);
  HASH_put_hash(ret, "chips", new_chips);
                
  return ret;
}

// compatibility with old songs
static void create_missing_busses(hash_t *bus_chips_state){
  int num_chips = HASH_get_int32(bus_chips_state, "num_chips");
  printf("num_chips: %d\n",num_chips);
  for(int busnum=num_chips;busnum<NUM_BUSES;busnum++) {
    createAudioInstrument(talloc_strdup("Bus"), talloc_format("Bus %d", busnum+1), talloc_format("Aux %d Bus", busnum-num_chips+1), 0, 0, false, true);
  }
}

static void autoposition_missing_bus_chips(hash_t *bus_chips_state){
  int num_chips = HASH_get_int32(bus_chips_state, "num_chips");
  Buses buses = MIXER_get_buses();
  for(int busnum=num_chips;busnum<NUM_BUSES;busnum++) {
    SoundProducer *sp =
      busnum==0 ? buses.bus1 :
      busnum==1 ? buses.bus2 :
      busnum==2 ? buses.bus3 :
      busnum==3 ? buses.bus4 :
      buses.bus5;
      
    Chip *chip = CHIP_get(&g_mixer->scene, (struct Patch*)SP_get_plugin(sp)->patch);
    CHIP_autopos(chip);
  }
}

// Patches must be created before calling this one.
// However, patch->patchdata are created here.
//
// Only called when loading song. (earlier it was also used when undoing)
void MW_create_full_from_state(const hash_t *state, bool is_loading){

  R_ASSERT(is_loading);
  
  //MW_cleanup();

  hash_t *bus_chips_state;
  hash_t *chips_state;
  
  if (!HASH_has_key(state, "bus_chips")){
    hash_t *state2 = get_chips_and_bus_chips(state);
    bus_chips_state = HASH_get_hash(state2, "bus_chips");
    chips_state = HASH_get_hash(state2, "chips");
  }else{
    bus_chips_state = HASH_get_hash(state, "bus_chips");
    chips_state = HASH_get_hash(state, "chips");
  }
  
  Buses old_buses = MIXER_get_buses();

  Buses no_buses = {};
  MW_create_chips_from_full_state(bus_chips_state, no_buses, is_loading);

  Buses new_buses = MIXER_get_buses();

  printf("name1: %s, name2: %s\n",
         old_buses.bus1==NULL?"null":SP_get_plugin(old_buses.bus1)->type->name,
         new_buses.bus1==NULL?"null":SP_get_plugin(new_buses.bus1)->type->name
         );
         
  if (old_buses.bus1!=NULL && new_buses.bus1!=NULL)
    R_ASSERT(SP_get_id(old_buses.bus1) != SP_get_id(new_buses.bus1));
  else  
    R_ASSERT(old_buses.bus1 != new_buses.bus1);
  
  if (old_buses.bus2!=NULL && new_buses.bus2!=NULL)
    R_ASSERT(SP_get_id(old_buses.bus2) != SP_get_id(new_buses.bus2));
  else  
    R_ASSERT(old_buses.bus2 != new_buses.bus2);

  create_missing_busses(bus_chips_state); // compatibility with old songs

  if (is_loading)
    GFX_ShowProgressMessage("Creating instruments", true);
  
  MW_create_chips_from_full_state(chips_state, new_buses, is_loading);

  if (is_loading)
    GFX_ShowProgressMessage("Creating connections between sound objects", true);

  CONNECTIONS_create_from_state(&g_mixer->scene, HASH_get_hash(state, "connections"),
                                make_instrument(-1), make_instrument(-1), make_instrument(-1), make_instrument(-1)
                                );

  MW_create_bus_connections_for_all_chips(new_buses); // compatibility with old songs

  if (HASH_has_key(state, "ab_state"))
    MW_recreate_ab_from_state(HASH_get_hash(state, "ab_state"));
  
  AUDIO_update_all_permanent_ids();

  if (HASH_has_key(state, "num_instances_state"))
    MW_apply_num_instances_state(HASH_get_hash(state, "num_instances_state"));
    
  GFX_update_all_instrument_widgets();
  MW_update_mixer_widget(true);
  
  autoposition_missing_bus_chips(bus_chips_state);

  remakeMixerStrips(make_instrument(-1));

  if (HASH_has_key(state, "mixer_strips_configuration"))
    MW_apply_mixer_strips_state(HASH_get_dyn(state, "mixer_strips_configuration"));
  else
    gui_resetAllMixerStrips();

  SEND_RECEIVE_update_send_receivers();
  
  if (!HASH_has_key(state, "volume_applied_before_drywet")){
    evalScheme("(ra:schedule 100 (lambda () (ra:add-message "
               "\"Beware that this song was created with a version of Radium where volume was applied after drywet and bypass.\n"
               "If the song uses bypass or dry/wet, it might not sound exactly the same.\""
               ") #f))");
  }

}

// This function is called when loading a song saved with a version of radium made before the audio system was added.
void MW_create_plain(void){
  //MW_cleanup();
  gui_resetAllMixerStrips();        
}


// A/B

static hash_t *g_ab_states[MW_NUM_AB] = {}; // [NO_STATIC_ARRAY_WARNING]
static bool g_ab_is_valid[MW_NUM_AB] = {}; // [NO_STATIC_ARRAY_WARNING]
static int g_curr_ab = 0;


int MW_get_curr_ab(void){
  return g_curr_ab;
}

bool MW_is_ab_valid(int ab_num){
  return g_ab_is_valid[ab_num];
}

/*
  Complicated.

static hash_t *create_ab_state(void){
  hash_t *state = HASH_create(5);

  //const vector_t *patches = &get_audio_instrument()->patches;
  HASH_put_hash(state, "patches", PATCHES_get_state(NULL));
    
  HASH_put_hash(state, "chips", MW_get_audio_patches_state(NULL, false));
  
  HASH_put_hash(state, "connections", MW_get_connections_state(NULL));

  return state;
}

static hash_t *apply_ab_patch_state(hash_t *patches_state, hash_t *chips, hash_t *curr_patches, hash_t *curr_chips){
  int num_presets = HASH_get(patches_state, "num_patches");

  hash_t *patch_id_map = HASH_create(num_presets);
  
  for(int i = 0 ; i < num_presets ; i++) {
    hash_t *new_patch_state = HASH_get_hash_at(patches_state, "patch", i);
    const char *key = talloc_format("%" PRId64, HASH_get_instrument(new_patch_state, "id"));
    
    if (!HASH_has_key(curr_patches, key)) {
      
      struct Patch *patch = PATCH_create_audio(NULL, NULL, NULL, new_patch_state, 0, 0);
      HASH_put_instrument(patch_id_map, key, patch->id);
      
      hasht *chip_state = HASH_get_hash(chips, key);
      CHIP_set_pos(patch, HASH_get_float(chip_state, "x"), HASH_get_float(chip_state, "y"));
      
    } else {
      
        hash_t *curr_patch_state = HASH_get_hash(curr_patches, key);
        if (!HASH_equal(new_patch_state, curr_patch_state)){

          // Find out if it's enough to change the plugin state, or if the whole plugin needs to be reloaded
          
          
        }
  }

  return patch_id_map;
}
*/

static hash_t *create_ab_state(void){
  bool include_volume = includeVolumeInMixerConfig();
  bool include_audio = include_volume || includeAudioConnectionsInMixerConfig();
  bool include_event = includeEventConnectionsInMixerConfig();
  bool include_modulator_connections = includeRememberCurrentInstrumentInMixerConfig();
    
  hash_t *state = HASH_create(5);

  HASH_put_int(state, "version", 1);

  if (include_audio || include_event || include_modulator_connections)
    HASH_put_hash(state, "connections", MW_get_connections_state(NULL, include_audio, include_event, include_modulator_connections));

  if (include_volume || includePanningInMixerConfig() || includeMuteSoloBypassInMixerConfig() || includeSystemEffectsInMixerConfig() || includeInstrumentEffectsInMixerConfig() || includeInstrumentStatesInMixerConfig() || includeSystemVolumeInMixerConfig()) {
    
    vector_t *patches = &get_audio_instrument()->patches;
    hash_t *plugin_ab_states = HASH_create(patches->num_elements);
    
    for(int i=0;i<patches->num_elements;i++){
      struct Patch *patch = (struct Patch*)patches->elements[i];
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      if (plugin==NULL){
        R_ASSERT(false);
      } else {
        HASH_put_hash(plugin_ab_states, get_patch_key(patch), PLUGIN_get_ab_state(plugin));
      }
    }

    HASH_put_hash(state, "plugin_ab_states", plugin_ab_states);
  }
  
  if (includeMixerStripsConfigurationInMixerConfig())
    HASH_put_dyn(state, "mixer_strips_configuration", MW_get_mixer_strips_state());

  if (includeRememberCurrentInstrumentInMixerConfig())
    HASH_put_instrument(state, "current_instrument", getCurrentInstrument());

  return state;
}

static void apply_ab_plugin_ab_states(hash_t *plugin_ab_state, hash_t *curr_plugin_ab_state, radium::Scheduled_RT_functions &rt_functions, radium::SoloChanges &solo_changes){
  
  const vector_t &patches = get_audio_instrument()->patches;

  for(int i=0;i<patches.num_elements;i++){
    
    struct Patch *patch=(struct Patch*)patches.elements[i];
    const char *key = get_patch_key(patch);
    
    if (HASH_has_key(plugin_ab_state, key)){
      
      hash_t *ab_state = HASH_get_hash(plugin_ab_state, key);
      hash_t *curr_ab_state = HASH_get_hash(curr_plugin_ab_state, key);
      
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      if (plugin==NULL){
        R_ASSERT(false);
      }else{
        if (PLUGIN_apply_ab_state(plugin, ab_state, curr_ab_state, rt_functions, solo_changes)){
          //CHIP_update(plugin);
        }
      }
    }
  }
}

/*
static bool in_patches(const vector_t *patches, int64_t id){
  VECTOR_FOR_EACH(struct Patch *, patch, patches){
    if (patch->id==id)
      return true;
  }END_VECTOR_FOR_EACH;
  
  return false;
}
*/

static void apply_ab_connections_state(hash_t *connections, radium::Scheduled_RT_functions &rt_functions, radium::SoloChanges &solo_changes){

  bool include_audio = root->song->includeAudioConnectionsInMixerConfig;
  if (include_audio && HASH_has_key(connections, "includes_audio"))
    include_audio = HASH_get_bool(connections, "includes_audio");
  
  bool include_events = root->song->includeEventConnectionsInMixerConfig;
  if (include_events && HASH_has_key(connections, "includes_events"))
    include_events = HASH_get_bool(connections, "includes_events");

  if (include_audio || include_events)
    ADD_UNDO(MixerConnections_CurrPos());
    
  CONNECTIONS_replace_all_with_state(&g_mixer->scene, connections,
                                     include_audio, include_events, includeVolumeInMixerConfig(), includeModulatorConnectionsInMixerConfig(),
                                     false,
                                     rt_functions, solo_changes);
}

// 'curr_state' is used to compare states. We skip applying the new state if it isn't different.
static void apply_ab_state(hash_t *state, hash_t *curr_state){

  int version = 1;
  if (HASH_has_key(state, "version"))
    version = HASH_get_int32(state, "version");

  if (version > 1){
    GFX_addMessage("Can not load this ab state. It was saved in a newer version of radium that uses a different format");
    return;
  }
  
  radium::Scheduled_RT_functions rt_functions;
  radium::SoloChanges solo_changes(rt_functions);
      
  UNDO_OPEN();{
    if (HASH_has_key(state, "plugin_ab_states") && HASH_has_key(curr_state, "plugin_ab_states"))
      apply_ab_plugin_ab_states(HASH_get_hash(state, "plugin_ab_states"),
                                HASH_get_hash(curr_state, "plugin_ab_states"),
                                rt_functions, solo_changes
                                );
    
    if (HASH_has_key(state, "connections"))
      apply_ab_connections_state(HASH_get_hash(state, "connections"), rt_functions, solo_changes);

  }UNDO_CLOSE();

  if (includeMixerStripsConfigurationInMixerConfig() && HASH_has_key(state, "mixer_strips_configuration"))
    MW_apply_mixer_strips_state(HASH_get_dyn(state, "mixer_strips_configuration"));

  if (includeRememberCurrentInstrumentInMixerConfig() && HASH_has_key(state, "current_instrument")){
    instrument_t instrument = HASH_get_instrument(state, "current_instrument");
    if (isLegalInstrument(instrument) && instrumentIsOpen(instrument))
      setCurrentInstrument(instrument, false, false);
  }
}

void MW_change_ab(int ab_num, bool update_current_button){
  R_ASSERT_RETURN_IF_FALSE(ab_num>=0 && ab_num < MW_NUM_AB);
  
  int old_ab_num = g_curr_ab;
  int new_ab_num = ab_num;

  if (old_ab_num==new_ab_num)
    return;
  
  hash_t *curr_state = create_ab_state();
  
  // save old data
  {
    g_ab_states[old_ab_num] = curr_state;
    g_ab_is_valid[old_ab_num] = true;
  }

  // insert new data
  if (g_ab_is_valid[new_ab_num]){
    apply_ab_state(g_ab_states[new_ab_num], curr_state);
  }

  g_curr_ab = new_ab_num;
  printf("Curr ab: %d\n", new_ab_num);

  MW_update_mixer_widget(update_current_button);
}

void MW_reset_ab(int num){
  R_ASSERT_RETURN_IF_FALSE(num>=-1 && num < MW_NUM_AB);
  
  if (num==-1) {
    for(int i=0;i<MW_NUM_AB;i++)
      g_ab_is_valid[i]=false;
    g_curr_ab = 0;
  } else
    g_ab_is_valid[num]=false;

  MW_update_mixer_widget(true);
}

bool MW_ab_is_used(int num){
  R_ASSERT_RETURN_IF_FALSE2(num>=0 && num < MW_NUM_AB, false);
  return g_ab_is_valid[num];
}

// Only used when saving .rad
hash_t *MW_get_ab_state(void){
  hash_t *ab_state = HASH_create(MW_NUM_AB);

  HASH_put_int(ab_state, "curr_ab_num", g_curr_ab);
      
  for(int i=0;i<MW_NUM_AB;i++){
    bool is_valid = g_ab_is_valid[i];
    
    HASH_put_bool_at(ab_state,"is_valid",i,is_valid);

    if (is_valid)
      HASH_put_hash_at(ab_state, "ab_state", i, g_ab_states[i]);
  }

  HASH_put_bool(ab_state, "includeAudioConnectionsInMixerConfig", includeAudioConnectionsInMixerConfig());
  HASH_put_bool(ab_state, "includeEventConnectionsInMixerConfig", includeEventConnectionsInMixerConfig());
  HASH_put_bool(ab_state, "includeVolumeInMixerConfig", includeVolumeInMixerConfig());
  HASH_put_bool(ab_state, "includePanningInMixerConfig", includePanningInMixerConfig());
  HASH_put_bool(ab_state, "includeMuteSoloBypassInMixerConfig", includeMuteSoloBypassInMixerConfig());
  HASH_put_bool(ab_state, "includeSystemEffectsInMixerConfig", includeSystemEffectsInMixerConfig());
  HASH_put_bool(ab_state, "includeInstrumentEffectsInMixerConfig", includeInstrumentEffectsInMixerConfig());
  HASH_put_bool(ab_state, "includeInstrumentStatesInMixerConfig", includeInstrumentStatesInMixerConfig());
  HASH_put_bool(ab_state, "includeMixerStripsConfigurationInMixerConfig", includeMixerStripsConfigurationInMixerConfig());
  HASH_put_bool(ab_state, "includeRememberCurrentInstrumentInMixerConfig", includeRememberCurrentInstrumentInMixerConfig());
  HASH_put_bool(ab_state, "includeModulatorConnectionsInMixerConfig", includeModulatorConnectionsInMixerConfig());
  HASH_put_bool(ab_state, "includeSystemVolumeInMixerConfig", includeSystemVolumeInMixerConfig());
  
  return ab_state;
}

void MW_recreate_ab_from_state(hash_t *ab_state){
  
  g_curr_ab = HASH_get_int32(ab_state, "curr_ab_num");

  for(int i=0;i<NUM_AB;i++){
    g_ab_is_valid[i] = HASH_get_bool_at(ab_state, "is_valid", i);
    
    if (g_ab_is_valid[i])
      g_ab_states[i] = HASH_get_hash_at(ab_state, "ab_state", i);
  }

  if (HASH_has_key(ab_state, "includeAudioConnectionsInMixerConfig")){
    setIncludeAudioConnectionsInMixerConfig(HASH_get_bool(ab_state, "includeAudioConnectionsInMixerConfig"));
    setIncludeEventConnectionsInMixerConfig(HASH_get_bool(ab_state, "includeEventConnectionsInMixerConfig"));
    setIncludeVolumeInMixerConfig(HASH_get_bool(ab_state, "includeVolumeInMixerConfig"));
    setIncludePanningInMixerConfig(HASH_get_bool(ab_state, "includePanningInMixerConfig"));
    setIncludeMuteSoloBypassInMixerConfig(HASH_get_bool(ab_state, "includeMuteSoloBypassInMixerConfig"));
    setIncludeSystemEffectsInMixerConfig(HASH_get_bool(ab_state, "includeSystemEffectsInMixerConfig"));
    setIncludeInstrumentEffectsInMixerConfig(HASH_get_bool(ab_state, "includeInstrumentEffectsInMixerConfig"));
    setIncludeInstrumentStatesInMixerConfig(HASH_get_bool(ab_state, "includeInstrumentStatesInMixerConfig"));
    setIncludeMixerStripsConfigurationInMixerConfig(HASH_get_bool(ab_state, "includeMixerStripsConfigurationInMixerConfig"));
    setIncludeRememberCurrentInstrumentInMixerConfig(HASH_get_bool(ab_state, "includeRememberCurrentInstrumentInMixerConfig"));
    setIncludeModulatorConnectionsInMixerConfig(HASH_get_bool(ab_state, "includeModulatorConnectionsInMixerConfig"));
    if (HASH_has_key(ab_state, "includeSystemVolumeInMixerConfig"))
      setIncludeSystemVolumeInMixerConfig(HASH_get_bool(ab_state, "includeSystemVolumeInMixerConfig"));
  }
}

#include "mQM_MixerWidget.cpp"
