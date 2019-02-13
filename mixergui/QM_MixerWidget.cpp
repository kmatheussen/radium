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

#include <QTimer>
#include <QFont>
#include <QMainWindow>
#include <QGraphicsSceneMouseEvent>
#include <QAction>
#include <QMenu>
#include <QPainter>


#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "QM_MixerWidget.h"
#include "QM_view.h"
#include "QM_chip.h"

#include "../Qt/helpers.h"

#include "../Qt/Qt_MyQButton.h"
#include "../Qt/Qt_MyQSlider.h"
#include "../Qt/mQt_mixer_widget_callbacks.h"

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

#include "../embedded_scheme/s7extra_proc.h"


extern EditorWidget *g_editor;


class MyScene : public QGraphicsScene{
  Q_OBJECT
 public:
  MyScene(QWidget *parent);

 protected:
  void 	mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event ) override;
  void 	mouseMoveEvent ( QGraphicsSceneMouseEvent * event ) override;
  void 	mousePressEvent ( QGraphicsSceneMouseEvent * event ) override;
  void 	mouseReleaseEvent ( QGraphicsSceneMouseEvent * event ) override;


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

  EventConnection *_current_econnection;
  Chip *_ecurrent_from_chip;
  Chip *_ecurrent_to_chip;

  std::vector<Chip*>_moving_chips;

  QPointF _start_mouse_pos;

#if 0
  public slots:
    void on_scene_changed ( const QList<QRectF> & region ){
    printf("Hepp! changed\n");
  }
#endif
};


class MixerWidget : public QWidget
{
    Q_OBJECT
public:
    MixerWidget(QWidget *parent = 0);

    void setupMatrix();
    void populateScene();
    
    MyScene scene;
    MyQGraphicsView *view;

    RememberGeometry remember_geometry;

    void setVisible(bool visible) override {
      remember_geometry.setVisible_override<QWidget>(this, visible);
    }

    void hideEvent(QHideEvent *event_) override {
      remember_geometry.hideEvent_override(this);
    }
};

QGraphicsScene *get_scene(MixerWidget *mixer_widget){
  return &mixer_widget->scene;
}

QWidget *get_qwidget(MixerWidget *mixer_widget){
  return mixer_widget;
}

MixerWidget *create_mixer_widget(QWidget *parent){
  return new MixerWidget(parent);
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
       QColor color(59,68,155,40);
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

  QColor color(40,40,40);
  _slot_indicator = new SlotIndicatorItem(); //addRect(0,0,chip_width,chip_width, QPen(color));//, Qt::SolidPattern);
  _slot_indicator->setZValue(-20);

  //setAcceptDrops(true);
  
  addItem(_slot_indicator);
  //new HelpText(this);
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

static bool chip_body_is_placed_at(Chip *chip, float mouse_x, float mouse_y){
  if(mouse_x < (get_chip_slot_x(chip)*grid_width + port_width))
    return false;

  else if(mouse_x > (get_chip_slot_x(chip)*grid_width + grid_width - port_width))
    return false;

  else
    return true;
}

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

  QGraphicsView *view = g_mixer_widget->view;
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

        myscene->_parent->setCursor(QCursor(Qt::SizeBDiagCursor));

      }else{

        myscene->_parent->setCursor(QCursor(Qt::SizeFDiagCursor));

      }

    }else{

      //printf("got no chip\n");      
      myscene->_parent->setCursor(QCursor(Qt::SizeHorCursor));

    }
  }
}

void MW_set_selected_chip(Chip *chip){
  printf("MW_set_selected_chip called\n");
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip2 = dynamic_cast<Chip*>(das_items.at(i));
    if(chip2!=NULL && chip2!=chip)
      chip2->mySetSelected(false);
  }
  chip->mySetSelected(true);
}

void MW_update_all_chips(void){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip2 = dynamic_cast<Chip*>(das_items.at(i));
    if(chip2!=NULL)
      chip2->update();
  }
}

static void handle_chip_selection(MyScene *myscene, QGraphicsSceneMouseEvent * event, Chip *chip){
  bool was_selected = chip->isSelected();

  if(event->modifiers() & Qt::ControlModifier)
    chip->mySetSelected(!was_selected);

  else if(was_selected==false)
    MW_set_selected_chip(chip);

  myscene->_moving_chips.push_back(chip);
}

static void start_moving_chips(MyScene *myscene, QGraphicsSceneMouseEvent * event, Chip *main_chip, float mouse_x, float mouse_y){
  //EVENTLOG_add_event("start_moving_chips (open undo)");
  UNDO_OPEN();

  handle_chip_selection(myscene, event, main_chip);

  const QList<QGraphicsItem *> &das_items = g_mixer_widget->scene.items();

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

static bool move_chip_to_slot(Chip *chip, float from_x, float from_y){
  float x,y;
  get_slotted_x_y(from_x, from_y, x, y);
  //printf("   %f/%f,  %f/%f\n",chip->pos().x(),x, chip->pos().y(),y);

  chip->setPos(x,y);

  printf("       Remake: move_chip_to_slot\n");
  remakeMixerStrips(-2);
  
  return chip->_moving_start_pos!=chip->pos();
}

bool MW_move_chip_to_slot(Chip *chip, float x, float y){
  return move_chip_to_slot(chip, x, y);
}
  
bool MW_move_chip_to_slot(struct Patch *patch, float x, float y){  
  return move_chip_to_slot(CHIP_get(&g_mixer_widget->scene, patch),
                           x, y);
}
  
static AudioConnection *find_clean_connection_at(MyScene *scene, float x, float y);

// Also kicks.
static bool autoconnect_chip(MyScene *myscene, Chip *chip, float x, float y){
  bool do_autoconnect = chip->audio_connections.size()==0; // don't autoconnect if the chip already has connections.  
  
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
  return autoconnect_chip(&g_mixer_widget->scene, CHIP_get(&g_mixer_widget->scene, patch), x, y);
}
  
static bool cleanup_a_chip_position(MyScene *myscene){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
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
  cleanup_chip_positions(&g_mixer_widget->scene);
}

static bool stop_moving_chips(MyScene *myscene, const QPointF &mouse_pos){

  float mouse_x = mouse_pos.x();
  float mouse_y = mouse_pos.y();

  //myscene->removeItem(_slot_indicator);
  myscene->_parent->setCursor(QCursor(Qt::ArrowCursor));

  Chip *main_chip = myscene->_moving_chips.at(0);
  float main_chip_x = main_chip->pos().x();
  float main_chip_y = main_chip->pos().y();

  int size = (int)myscene->_moving_chips.size();
  bool has_updated = false;

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
  else if (myscene->_start_mouse_pos != mouse_pos)
    return true;
  else
    return false;
}

void MyScene::mouseMoveEvent ( QGraphicsSceneMouseEvent * event ){

  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();

  QPointF pos=event->scenePos();

  draw_slot(this, pos.x(), pos.y());

  if(_current_connection != NULL){

    int x1,y1;

    if(_current_from_chip != NULL){
      x1 = CHIP_get_output_port_x(_current_from_chip);
      y1 = CHIP_get_port_y(_current_from_chip);
    }else{
      x1 = CHIP_get_input_port_x(_current_to_chip);
      y1 = CHIP_get_port_y(_current_to_chip);
    }

    int x2 = pos.x();
    int y2 = pos.y();

    _current_connection->setLine(x1,y1,x2,y2);

    event->accept();

  }else if(_current_econnection != NULL){

    int x1,y1;

    if(_ecurrent_from_chip != NULL){
      x1 = CHIP_get_eport_x(_ecurrent_from_chip);
      y1 = CHIP_get_output_eport_y(_ecurrent_from_chip);
    }else{
      x1 = CHIP_get_eport_x(_ecurrent_to_chip);
      y1 = CHIP_get_input_eport_y(_ecurrent_to_chip);
    }

    int x2 = pos.x();
    int y2 = pos.y();

    _current_econnection->setLine(x1,y1,x2,y2);

    event->accept();

  } else if(_moving_chips.size()>0){

    move_moving_chips(this,pos.x(),pos.y());
        
  }else{
    
    QGraphicsScene::mouseMoveEvent(event);
  
  }

}

Chip *MW_get_chip_at(float x, float y, Chip *except){
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL && chip!=except){
      QPointF pos = chip->pos();
      //printf("%d / %d,    %d / %d\n",get_slot_x(pos.x()+grid_width/2), slot_x,    get_slot_y(pos.y()+grid_height/2), slot_y);
      if(get_slot_x(pos.x()+grid_width/2)==slot_x && get_slot_y(pos.y()+grid_height/2)==slot_y)
        return chip;
    }
  }

  return NULL;
}

static AudioConnection *find_clean_connection_at(MyScene *scene, float x, float y){
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

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
  QList<QGraphicsItem *> das_items = scene->items();

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

// Returns false if there are more than one before or after chip.
// Also returns false if there are 0 before chips, or 0 after chips.
// Neither 'before' nor 'after' is set if the function returns false.
static bool get_before_and_after_chip(Chip *chip, Chip **before, Chip **after){
  if(chip->audio_connections.size()!=2)
    return false;

  if(chip->audio_connections[0]->to==chip && chip->audio_connections[1]->to==chip)
    return false;

  if(chip->audio_connections[0]->from==chip && chip->audio_connections[1]->from==chip)
    return false;

  if(chip->audio_connections[0]->to==chip){
    *before = chip->audio_connections[0]->from;
    *after  = chip->audio_connections[1]->to;
  }else{
    *before = chip->audio_connections[1]->from;
    *after  = chip->audio_connections[0]->to;
  }

  return true;
}

static bool mousepress_delete_chip(MyScene *scene, QGraphicsItem *item, float mouse_x, float mouse_y){
  printf("Going to delete\n");
  Chip *chip = dynamic_cast<Chip*>(item);
  if(chip!=NULL){
    printf("Got chip\n");

    Chip *before=NULL;
    Chip *after=NULL;
    bool is_slash_was_connected = get_before_and_after_chip(chip, &before, &after);

    UNDO_OPEN_REC();{

      if(is_slash_was_connected){
        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_remove_chip_from_connection_sequence(scene, before, chip, after);
      }
      
      struct Patch *patch = CHIP_get_patch(chip);
      deleteInstrument(patch->id);
      
    }UNDO_CLOSE();
    
    return true;
  }

  return false;
}

static void delete_several_chips(const vector_t &patches){
  UNDO_OPEN_REC();{
    VECTOR_FOR_EACH(struct Patch *,patch,&patches){
      deleteInstrument(patch->id);
    }END_VECTOR_FOR_EACH;
  }UNDO_CLOSE();
}

static bool mousepress_start_connection(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y){

  Chip *chip = get_chip_with_port_at(scene,mouse_x,mouse_y);
  printf("chip: %p\n", chip);

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

      } else if(CHIP_is_at_input_port(chip,mouse_x,mouse_y)) {
        scene->_current_to_chip = chip;

        if (chip->_num_inputs==0)
          return false;

      } if(scene->_current_from_chip!=NULL || scene->_current_to_chip!=NULL){
        //printf("x: %d, y: %d. Item: %p. input/output: %d/%d\n",(int)mouse_x,(int)mouse_y,item,_current_input_port,_current_output_port);
        
        scene->_current_connection = new AudioConnection(scene);
        scene->addItem(scene->_current_connection);
        
        scene->_current_connection->setLine(mouse_x,mouse_y,mouse_x,mouse_y);
        
        event->accept();
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
        scene->_current_econnection = new EventConnection(scene);
        scene->addItem(scene->_current_econnection);
        
        scene->_current_econnection->setLine(mouse_x,mouse_y,mouse_x,mouse_y);
        
        event->accept();
        return true;
      }
    }
    
  }

  return false;
}

static bool mousepress_delete_connection(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y){
  SuperConnection *connection = dynamic_cast<SuperConnection*>(item);

  if(connection==NULL){
    QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
    for (int i = 0; i < das_items.size(); ++i) {
      connection = dynamic_cast<SuperConnection*>(das_items.at(i));
      if(connection!=NULL && connection->isUnderMouse()==true)
        break;
      else
        connection = NULL;
    }
  }

  if(connection!=NULL){
    ADD_UNDO(MixerConnections_CurrPos());
    CONNECTION_delete_connection(connection);
    event->accept();
    return true;
  }
  return false;
}

static bool mousepress_select_chip(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y, bool ctrl_pressed){
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
    event->accept();
    return true;
  }

  return false;
}

static bool mouserelease_replace_patch(MyScene *scene, float mouse_x, float mouse_y){
  Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
  if(chip_under!=NULL){
    if(chip_body_is_placed_at(chip_under, mouse_x, mouse_y)==true) {

      SoundPlugin *plugin = SP_get_plugin(chip_under->_sound_producer);
      Patch *patch = (struct Patch*)plugin->patch;
      R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);

      requestReplaceInstrument(patch->id, "", API_get_gui_from_existing_widget(g_mixer_widget->window()));
        
      return true;
    }
  }
  return false;
}


static QVector<Chip*> get_selected_chips(void){
  QVector<Chip*> ret;
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

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

static bool mouserelease_create_chip(MyScene *scene, float mouse_x, float mouse_y){
  printf("mouserelease_create_chip called\n");
  
  draw_slot(scene,mouse_x,mouse_y);

  float x, y;
  get_slotted_x_y(mouse_x, mouse_y, x, y);

  createInstrumentDescriptionPopupMenu(createNewInstrumentConf(x, y, false, true,
                                                               true,
                                                               false, false,
                                                               API_get_gui_from_existing_widget(g_mixer_widget->window())
                                                               )
                                       );

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
  QVector<Chip*> chips = get_selected_chips();
  
  vector_t patches = {};

  for(auto *chip : chips){
    struct Patch *patch = CHIP_get_patch(chip);
    R_ASSERT_RETURN_IF_FALSE2(!VECTOR_is_in_vector(&patches, patch), patches);
    VECTOR_push_back(&patches, patch);
  }

  return patches;
}

vector_t MW_get_selected_patches(void){
  return get_selected_patches();
}


void MW_solo(const vector_t patches, bool do_solo){

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }

  VECTOR_FOR_EACH(struct Patch *,patch,&patches){
    setInstrumentSolo(patch->id, do_solo);
  }END_VECTOR_FOR_EACH;
}

void MW_mute(const vector_t patches, bool do_mute){

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }

  UNDO_OPEN_REC();{
    VECTOR_FOR_EACH(struct Patch *,patch,&patches){
      setInstrumentMute(patch->id, do_mute);
    }END_VECTOR_FOR_EACH;
  }UNDO_CLOSE();
}

void MW_bypass(const vector_t patches, bool do_bypass){

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }

  UNDO_OPEN_REC();{
    VECTOR_FOR_EACH(struct Patch *,patch,&patches){
      setInstrumentBypass(patch->id, do_bypass);
    }END_VECTOR_FOR_EACH;
  }UNDO_CLOSE();
}

void MW_copy(void){
  vector_t patches = get_selected_patches();

  if (patches.num_elements==0){
    GFX_Message2(NULL, true, "No sound object selected");
    return;
  }

  PRESET_copy(&patches);
}

static void MW_delete2(float mouse_x, float mouse_y, bool has_mouse_coordinates){
  vector_t patches = get_selected_patches();
  
  if (patches.num_elements==1)
    mousepress_delete_chip(&g_mixer_widget->scene, get_selected_chips()[0], mouse_x, mouse_y);
  else {
    delete_several_chips(patches);
  }  
}

void MW_delete(void){
  MW_delete2(0,0,false);
}

static void MW_cut2(float mouse_x, float mouse_y, bool has_mouse_coordinates){
  vector_t patches = get_selected_patches();

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

int64_t MW_paste(float x, float y){
  if (PRESET_has_copy()==false)
    return -1;

  if (x <= -10000 || y <= -10000){
    QPoint viewPoint = g_mixer_widget->view->mapFromGlobal(QCursor::pos());
    QPointF scenePoint = g_mixer_widget->view->mapToScene(viewPoint);

    get_slotted_x_y(scenePoint.x(), scenePoint.y(), x, y);
  }

  return PRESET_paste(x, y);
}

bool MW_has_mouse_pointer(void){
  if (g_mixer_widget->isVisible()==false)
    return false;

  QPoint p = g_mixer_widget->mapFromGlobal(QCursor::pos());
  //printf("x: %d, y: %d. width: %d, height: %d\n", (int)p.x(), (int)p.y(), g_mixer_widget->width(), g_mixer_widget->height());
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

static bool g_connections_are_visible = true;
bool MW_get_connections_visibility(void){
  return g_connections_are_visible;
}

void MW_set_connections_visibility(bool show){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  for(auto *item : das_items){
    SuperConnection *connection = dynamic_cast<SuperConnection*>(item);
    if (connection != NULL){
      connection->setVisibility(show);
    }
  }
  g_connections_are_visible=show;
}

static const char *get_displayable_keybinding(const char *prefix, const char *racommand, const dynvec_t &args){
  const char *result = S7CALL2(charpointer_charpointer_dyn, "FROM_C-get-displayable-keybinding", racommand, DYN_create_array(args));

  if (!strcmp("", result))
    return prefix;
  else
    return talloc_format("[shortcut]%s[/shortcut]%s", result, prefix);
}

static int create_menu_entry(vector_t *v, const char *prefix, const char *racommand, const dynvec_t &args = *g_empty_dynvec.array){
  return VECTOR_push_back(v, get_displayable_keybinding(prefix, racommand, args));
}
  
                             
static bool mousepress_save_presets_etc(MyScene *scene, QGraphicsSceneMouseEvent * event, float mouse_x, float mouse_y){

  Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
  if(chip_under==NULL)
    return false;

  const vector_t patches = get_selected_patches();
  if (patches.num_elements==0)
    return false;
  
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
  int bypass = -1;
  int unmute_several = -1;
  int unsolo_all = -1;
  int mute_all = -1;
  int unmute_all = -1;
  int show_gui = -1;
  int receive_external_midi = -1;
  
  int64_t parentguinum = API_get_gui_from_existing_widget(g_mixer_widget->window());
  
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
    
    delete_ = VECTOR_push_back(&v, "Delete"); // sound objects");

    VECTOR_push_back(&v, "--------");
    
    save = VECTOR_push_back(&v, "Save multi preset (.mrec)");
    VECTOR_push_back(&v, "--------");
    show_mixer_strips = VECTOR_push_back(&v, "Create new mixer strips window for the selected objects");
    VECTOR_push_back(&v, "--------");
    solo_several = VECTOR_push_back(&v, "Solo all selected");
    unsolo_several = VECTOR_push_back(&v, "Un-solo all selected");
    VECTOR_push_back(&v, "--------");
    mute_several = VECTOR_push_back(&v, "Mute all selected");
    unmute_several = VECTOR_push_back(&v, "Un-mute all selected");
    VECTOR_push_back(&v, "--------");
    config_color = VECTOR_push_back(&v, "Configure color");
    generate_new_color = VECTOR_push_back(&v, "Generate new color");

    VECTOR_push_back(&v, "--------Mixer");
    unsolo_all = VECTOR_push_back(&v, "Un-solo all");
    mute_all = VECTOR_push_back(&v, "Mute all");
    unmute_all = VECTOR_push_back(&v, "Un-mute all");

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
    
    delete_ = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Delete" : "Delete"); // sound object");

    VECTOR_push_back(&v, "--------");

    if (ATOMIC_GET(plugin->solo_is_on)){
      solo = VECTOR_push_back(&v, "[check on]Solo");    
      //unsolo = VECTOR_push_back(&v, "Un-solo");
    }else{
      solo = VECTOR_push_back(&v, "[check off]Solo");    
      //unsolo = VECTOR_push_back(&v, "[disabled]Un-solo");
    }

    VECTOR_push_back(&v, "--------");
        
    if (!ATOMIC_GET(plugin->volume_is_on)){
      mute = VECTOR_push_back(&v, "[check on]Mute");
      //unmute = VECTOR_push_back(&v, "Un-mute");
    }else{
      mute = VECTOR_push_back(&v, "[check off]Mute");
      //unmute = VECTOR_push_back(&v, "[disabled]Un-mute");
    }

    if (PLUGIN_get_effect_value(plugin, plugin->type->num_effects + EFFNUM_EFFECTS_ONOFF, VALUE_FROM_PLUGIN) < 0.5){
      bypass = VECTOR_push_back(&v, "[check on]Bypass");
    }else{
      bypass = VECTOR_push_back(&v, "[check off]Bypass");
    }

    VECTOR_push_back(&v, talloc_format("--------Instrument: \"%s\"", patch->name));

    rename = VECTOR_push_back(&v, "Rename");
    
    VECTOR_push_back(&v, "---------");
        
    load = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Load preset" : "Load preset (.rec)");

    save = VECTOR_push_back(&v, AUDIO_is_permanent_patch(patch) ? "[disabled]Save preset" : "Save preset (.rec)");

    VECTOR_push_back(&v, "---------");
        
    config_color = VECTOR_push_back(&v, "Configure color");
    generate_new_color = VECTOR_push_back(&v, "Generate new color");
    
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
    
  }

  
  IsAlive is_alive(chip_under);

  // Not safe to store gc objects in a c++ closure. (At least I think so.)
  QVector<int64_t> patch_ids;
  VECTOR_FOR_EACH(struct Patch *,patch,&patches){
    patch_ids.push_back(patch->id);
  }END_VECTOR_FOR_EACH;

#define sels connect_to_main_pipe,insert,replace,solo,solo_several,unsolo_several,mute,mute_several,unmute_several,unsolo_all,mute_all,unmute_all,bypass,copy,cut,delete_,load,save,rename,show_mixer_strips,config_color,generate_new_color,show_gui,receive_external_midi,random,instrument_info
  
  GFX_Menu3(v,[is_alive, chip_under, scene, patch_ids, sels, mouse_x, mouse_y, parentguinum](int sel, bool onoff){
      
#undef sels
      
      if (!is_alive)
        return;

      vector_t patches = {};
      
      for(int64_t patch_id : patch_ids){
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

      R_ASSERT(parentguinum == API_get_gui_from_existing_widget(g_mixer_widget->window()));

      if (sel==-1) {
    
      } else if (sel==connect_to_main_pipe) {
        connectAudioInstrumentToMainPipe(CHIP_get_patch(chip_under)->id);
          
      } else if (sel==insert) {
    
        mouserelease_create_chip(scene, mouse_x, mouse_y);
        
      } else if (sel==replace) {
        
        mouserelease_replace_patch(scene, mouse_x, mouse_y);
        
      } else if (sel==solo) {
        
        MW_solo(patches, onoff);
        
      } else if (sel==solo_several) {
        
        MW_solo(patches, true);
        
      } else if (sel==unsolo_several) {
        
        MW_solo(patches, false);
        
      } else if (sel==mute) {
        
        MW_mute(patches, onoff);
        
      } else if (sel==bypass) {
        
        MW_bypass(patches, onoff);
        
      } else if (sel==mute_several) {
        
        MW_mute(patches, true);
        
      } else if (sel==unmute_several) {
        
        MW_mute(patches, false);
        
      } else if (sel==unsolo_all) {
        
        MW_solo(get_audio_instrument()->patches, false);
        
      } else if (sel==mute_all) {

        MW_mute(get_audio_instrument()->patches, true);
        
      } else if (sel==unmute_all) {
        
        MW_mute(get_audio_instrument()->patches, false);
        
      } else if (sel==copy) {
        
        MW_copy();
        
      } else if (sel==cut) {
        
        MW_cut2(mouse_x, mouse_y, true);
        
      } else if (sel==delete_) {
        
        MW_delete2(mouse_x, mouse_y, true);
        
      } else if (sel==rename) {

        R_ASSERT_RETURN_IF_FALSE(patch_ids.size() == 1);
        S7CALL2(void_int, "FROM_C-request-rename-instrument", patch_ids.at(0));
        
      } else if (sel==load) {

        R_ASSERT_RETURN_IF_FALSE(patch_ids.size() == 1);
        requestLoadInstrumentPreset(patch_ids.at(0), "", parentguinum);
        
      } else if (sel==save) {
        
        PRESET_save(&patches, false, parentguinum);
        
      } else if (sel==show_mixer_strips) {
        
        int num_rows = R_BOUNDARIES(1, 1 + (patches.num_elements / 6), 3);
        dynvec_t instruments = {};
        
        VECTOR_FOR_EACH(struct Patch *,patch,&patches){
          DYNVEC_push_back(&instruments, DYN_create_int(patch->id));
        }END_VECTOR_FOR_EACH;
        
        showMixerStrips2(num_rows, DYN_create_array(instruments));
        
      } else if (sel==config_color) {
        
        QString command = QString("(show-instrument-color-dialog ") + QString::number(parentguinum) + " " + QString::number(CHIP_get_patch(chip_under)->id);
        
        VECTOR_FOR_EACH(struct Patch *,patch,&patches){
          if (patch!=CHIP_get_patch(chip_under))
            command += " " + QString::number(patch->id);
        }END_VECTOR_FOR_EACH;
        
        command += ")";
        //}UNDO_CLOSE();
        
        evalScheme(talloc_strdup(command.toUtf8().constData()));
        
      } else if (sel==generate_new_color) {
        
        QString command = QString("(ra:set-instrument-color (ra:generate-new-color 0.9) ") + QString::number(CHIP_get_patch(chip_under)->id) + ")";
        evalScheme(talloc_strdup(command.toUtf8().constData()));
        
      } else if (sel==instrument_info) {
        
        showInstrumentInfo(DYN_create_int(CHIP_get_patch(chip_under)->id), parentguinum);
        
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
}

static bool event_can_delete(QGraphicsSceneMouseEvent *event){
  if(event->button()==Qt::MiddleButton)
    return true;

  else if(event->modifiers() & Qt::ShiftModifier)
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
    if (chip->myMouseDoubleClickEvent(pos.x()-chip->x(), pos.y()-chip->y())){
      event->accept();
      return;
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

static bool g_is_pressed = false; // Workaround for nasty Qt bug.

void MyScene::mousePressEvent(QGraphicsSceneMouseEvent *event){
  if (g_is_pressed==true)
    return;

  g_is_pressed = true;

  //printf("mousepress: %p\n",_current_connection);
  
  EVENTLOG_add_event(talloc_format(">>>>  MyScene::mousePressEvent. has_undo: %d, runs_custom_exec: %d, _current_connection: %p, _current_econnection: %p, _moving_chips.size(): %d", (int)Undo_Is_Open(), (int)g_radium_runs_custom_exec, _current_connection, _current_econnection, (int)_moving_chips.size()));

  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();
  
  QPointF pos=event->scenePos();
  float mouse_x = pos.x();
  float mouse_y = pos.y();

  _start_mouse_pos = pos;

  QGraphicsItem *item = itemAt(event->scenePos(), QTransform());

  printf("mouse button: %d %d\n",event->button(),Qt::MiddleButton);

  root->song->tracker_windows->must_redraw = true;

  bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
  
  if(event_can_delete(event)) {
    
    if(mousepress_delete_chip(this,item,mouse_x,mouse_y)==true) {
      event->accept();
      return;
    }

    if(mousepress_delete_connection(this,event,item,mouse_x,mouse_y)==true)
      return;

  }
  
  if(event->button()==Qt::LeftButton) {
    if(ctrl_pressed==false && mousepress_start_connection(this,event,item,mouse_x,mouse_y)==true)
      return;

    if(ctrl_pressed==true && mousepress_select_chip(this,event,item,mouse_x,mouse_y,ctrl_pressed)==true) // select
      return;

  } if(event->button()==Qt::RightButton){

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
      event->accept();
      return;
    }

    //if(mousepress_select_chip(this,event,item,mouse_x,mouse_y,ctrl_pressed)==true) // select
    //  return;
  }

  QGraphicsScene::mousePressEvent(event);
}

void MyScene::mouseReleaseEvent ( QGraphicsSceneMouseEvent * event ){
  g_is_pressed = false;

  printf("mouse release: %p\n",_current_connection);

  EVENTLOG_add_event(talloc_format("<<<< MyScene::mouseReleaseEvent. has_undo: %d, runs_custom_exec: %d, _current_connection: %p, _current_econnection: %p, _moving_chips.size(): %d", (int)Undo_Is_Open(), (int)g_radium_runs_custom_exec, _current_connection, _current_econnection, (int)_moving_chips.size()));

  root->song->tracker_windows->must_redraw = true; // why?

  QPointF pos=event->scenePos();
  float mouse_x = pos.x();
  float mouse_y = pos.y();

  Chip *chip = MW_get_chip_at(mouse_x, mouse_y, NULL);

  bool must_accept = false;
  
  if(_current_connection!=NULL){

    R_ASSERT(_current_econnection==NULL);
    R_ASSERT(_moving_chips.size()==0);
    
    if(chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_current_from_chip != NULL && chip != _current_from_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_connect_chips(this, _current_from_chip, chip);

      }else if(_current_to_chip != NULL && chip != _current_to_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_connect_chips(this, chip, _current_to_chip);

      }
    }

    removeItem(_current_connection);
    delete _current_connection;     
    _current_connection = NULL;
    _current_from_chip = NULL;
    _current_to_chip = NULL;

    must_accept = true;
  }

  if(_current_econnection!=NULL){

    R_ASSERT(_moving_chips.size()==0);
        
    if(can_internal_data_be_accessed_questionmark_safer()==true && chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_ecurrent_from_chip != NULL && chip != _ecurrent_from_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_econnect_chips(this, _ecurrent_from_chip, chip);

      }else if(_ecurrent_to_chip != NULL && chip != _ecurrent_to_chip){

        ADD_UNDO(MixerConnections_CurrPos());
        CHIP_econnect_chips(this, chip, _ecurrent_to_chip);

      }
    }

    removeItem(_current_econnection);
    delete _current_econnection;     
    _current_econnection = NULL;
    _ecurrent_from_chip = NULL;
    _ecurrent_to_chip = NULL;

    must_accept = true;
  }
    
  if(_moving_chips.size()>0 && stop_moving_chips(this, pos)) {

    printf("       Remake: mousereleaseevent\n");
    remakeMixerStrips(-2);
    
    must_accept = true;
  }

  if (must_accept){
    event->accept();
    return;
  }


  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();

  
  bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
  bool shift_pressed = (event->modifiers() & Qt::ShiftModifier);
  
  if (event->button()==Qt::RightButton && shift_pressed==false && ctrl_pressed==false){
    
    bool chip_is_under = false;
    
    Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
    if(chip_under!=NULL){
      //        if(chip_body_is_placed_at(chip_under, mouse_x, mouse_y)==true)
      chip_is_under = true;
    }
    
    if(chip_is_under==false){
      mouserelease_create_chip(this,mouse_x,mouse_y);
      event->accept();
      return;
    }
    
    if(mousepress_save_presets_etc(this,event,mouse_x,mouse_y)==true){
      event->accept();
      return;
    }
    /*
      && mouserelease_replace_patch(this,mouse_x,mouse_y)==true) {
      event->accept();
      return;
    */
  }
  
  QGraphicsScene::mouseReleaseEvent(event);
}


DEFINE_ATOMIC(bool, g_show_cpu_usage_in_mixer) = false;

MixerWidget *g_mixer_widget = NULL;

namespace{
  class MixerWidgetTimer : public QTimer{

    int64_t counter = 0;
    
    void 	timerEvent ( QTimerEvent * e ) override {
      counter++;

      RETURN_IF_DATA_IS_INACCESSIBLE();

      if (g_mixer_widget->isVisible()){

        //printf("UPDATING mixer\n");

        QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
        
        for (int i = 0; i < das_items.size(); ++i) {
          
          Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
          
          if(chip!=NULL){
            
            SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
            
            if(plugin != NULL){

              bool show_cpu_update = ATOMIC_GET_RELAXED(g_show_cpu_usage_in_mixer);
              
              if (show_cpu_update){
                CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET(plugin->cpu_usage);
                
                if (cpu_usage==NULL || cpu_usage->should_update() || chip->_name_text!=cpu_usage->_last_cpu_text)
                  chip->update();
              }

              ATOMIC_SET(plugin->is_selected, chip->isSelected()); // Ensurance. Unfortunately QGraphicsItem::setSelected is not virtual

              volatile struct Patch *patch = plugin->patch;
              
              if(patch!=NULL){
                
                if(ATOMIC_GET_RELAXED(patch->visual_note_intencity) > 0) {
                  ATOMIC_ADD_RETURN_OLD_RELAXED(patch->visual_note_intencity, -1);
                  //printf("intencity: %d\n",intencity);
                  int x1,y1,x2,y2;
                  CHIP_get_note_indicator_coordinates(x1,y1,x2,y2);
                  chip->update(x1,y1,x2-x1,y2-y1);
                }

                float volume = chip->get_slider_volume();
                bool is_muted = !ATOMIC_GET(plugin->volume_is_on);
                bool is_implicitly_muted = SP_mute_because_someone_else_has_solo_left_parenthesis_and_we_dont_right_parenthesis(chip->_sound_producer);
                bool is_solo = ATOMIC_GET(plugin->solo_is_on);
                bool is_bypass = !ATOMIC_GET(plugin->effects_are_on);
                bool is_recording = ATOMIC_GET(patch->is_recording);
                bool is_autosuspending = SP_is_autosuspending(plugin->sp);

                //printf("last: %f. vol: %f. Equal? %d\n", chip->_last_updated_volume, volume, chip->_last_updated_volume == volume);
                if (chip->_last_updated_volume != volume){
                  CHIP_update(chip, plugin);
                  chip->_last_updated_volume = volume;
                }
                  
                if (chip->_last_updated_mute != is_muted){
                  chip->update();
                  chip->_last_updated_mute = is_muted;
                }
                
                if (chip->_last_updated_implicitly_mute != is_implicitly_muted){
                  chip->update();
                  chip->_last_updated_implicitly_mute = is_implicitly_muted;
                }
                
                if (chip->_last_updated_solo != is_solo){
                  chip->update();
                  chip->_last_updated_solo = is_solo;
                }
                
                if (chip->_last_updated_bypass != is_bypass){
                  chip->update();
                  chip->_last_updated_bypass = is_bypass;
                }
                
                if (chip->_last_updated_recording != is_recording){
                  chip->update();
                  chip->_last_updated_recording = is_recording;
                }

                // turn on is_autosuspending (only gfx)
                if (chip->_last_updated_autosuspending==true &&  is_autosuspending==false){
                  chip->_last_updated_autosuspending = false;
                  chip->update();
                  //printf("Turned off autosuspending for %s\n", patch->name);
                }

                if (is_autosuspending==false)
                  chip->_autosuspend_on_time = counter;
                                  
                // turn off is_autosuspending (only gfx) (wait at least one second to turn if on, to avoid flickering)
                if (chip->_last_updated_autosuspending==false && is_autosuspending==true){
                  //printf("Turned on autosuspending for %s (%f)\n", patch->name, (TIME_get_ms() - _autosuspend_on_time));
                  if ( (counter - chip->_autosuspend_on_time) > (1000 / interval())){
                    chip->_last_updated_autosuspending = true;
                    chip->update();
                  }
                }

                
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
}

MixerWidget::MixerWidget(QWidget *parent)
    : QWidget(parent)
    , scene(this)
{

  if(g_mixer_widget!=NULL){
    fprintf(stderr,"Error. More than one MixerWidget created.\n");
    abort();
  }

  g_mixer_widget = this;
  g_static_toplevel_widgets.push_back(this);
  
    populateScene();

    auto *layout = new QVBoxLayout(this);  
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

#if 0
    View *view = new View("Das mixer",this);
    view->view()->setScene(&scene);
    layout->addWidget(view, 0, 0, 1, 1);

#else

    Mixer_widget *mixer_widget = new Mixer_widget(this);
    mixer_widget->view->setScene(&scene);
    layout->addWidget(mixer_widget);
    this->view = mixer_widget->view;

#endif

#if 0
    h1Splitter = new QSplitter;
    h2Splitter = new QSplitter;
    
    QSplitter *vSplitter = new QSplitter;
    vSplitter->setOrientation(Qt::Vertical);
    vSplitter->addWidget(h1Splitter);
    vSplitter->addWidget(h2Splitter);

    View *view = new View("Top left view");
    view->view()->setScene(scene);
    h1Splitter->addWidget(view);

    view = new View("Top right view");
    view->view()->setScene(scene);
    h1Splitter->addWidget(view);

    view = new View("Bottom left view");
    view->view()->setScene(scene);
    h2Splitter->addWidget(view);

    view = new View("Bottom right view");
    view->view()->setScene(scene);
    h2Splitter->addWidget(view);

    QHBoxLayout *layout = new QHBoxLayout;
    layout->addWidget(vSplitter);
    setLayout(layout);
#endif

    setWindowTitle(tr("Radium Mixer"));

    {
      MixerWidgetTimer *timer = new MixerWidgetTimer;
      timer->setInterval(50); // 3*16.666
      timer->start();
    }
}

bool GFX_MixerIsVisible(void){
  return !g_mixer_widget->isHidden();
}
void GFX_ShowMixer(void){
  GL_lock();{
    g_mixer_widget->show();
  }GL_unlock();
}
void GFX_HideMixer(void){
  g_mixer_widget->hide();
  set_editor_focus();
}

void GFX_showHideMixerWidget(void){
  GL_lock();{
    if(g_mixer_widget->isHidden())
      g_mixer_widget->show();
    else
      g_mixer_widget->hide();
  }GL_unlock();
  set_editor_focus();
}

static const int g_main_pipe_patch_id = 0;

SoundPlugin *get_main_pipe(void){
  struct Patch *patch = PATCH_get_from_id(g_main_pipe_patch_id);
  if (patch==NULL)
    return NULL;
  
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  return plugin;
  /*
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
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


void MixerWidget::populateScene()
{
#if 0
  SoundPluginType *type1 = PR_get_plugin(0);
  SoundPlugin *plugin1 = PLUGIN_create(type1, NULL);

  SoundPluginType *type2 = PR_get_plugin(1);
  SoundPlugin *plugin2 = PLUGIN_create(type2, NULL);

  SoundProducer *sound_producer1 = SP_create(plugin1);
  SoundProducer *sound_producer2 = SP_create(plugin2);

  Chip *from = new Chip(&scene,sound_producer1,20,30);    
  Chip *to = new Chip(&scene,sound_producer2,50,80);

  connect_chips(&scene,from, 0, to, 1);
#endif

  /*

  // NB! main_pipe must be created first. The patch id of main_pipe must be 0. (really?)
  SoundPluginType *pipe_type = PR_get_plugin_type_by_name(NULL, "Pipe","Pipe");
  SoundPlugin *main_pipe = add_new_audio_instrument_widget(pipe_type, grid_width, 0,false,"Main Pipe");
  g_main_pipe_patch_id = main_pipe->patch->id;


  SoundPluginType *bus1 = PR_get_plugin_type_by_name(NULL, "Bus","Bus 1");
  SoundPlugin *bus1_plugin = add_new_audio_instrument_widget(bus1, 0, 0,false,"Bus 1", NULL, NULL);

  SoundPluginType *bus2 = PR_get_plugin_type_by_name(NULL, "Bus","Bus 2");
  SoundPlugin *bus2_plugin = add_new_audio_instrument_widget(bus2, 0, grid_height,false,"Bus 2", NULL, NULL);

  SoundPluginType *system_out = PR_get_plugin_type_by_name(NULL, "Jack","System Out");
  SoundPlugin *system_out_plugin = add_new_audio_instrument_widget(system_out, grid_width*2, 0,false,"Main Out");

  CHIP_connect_chips(&scene, main_pipe, system_out_plugin);
  CHIP_connect_chips(&scene, bus1_plugin, main_pipe);
  CHIP_connect_chips(&scene, bus2_plugin, main_pipe);
  */
}

void MW_connect_plugin_to_main_pipe(SoundPlugin *plugin){
  SoundPlugin *main_pipe = get_main_pipe();

  if(plugin->type->num_outputs>0)
    CHIP_connect_chips(&g_mixer_widget->scene, plugin, main_pipe);
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
    Chip        *system_chip  = find_chip_for_plugin(&g_mixer_widget->scene, main_pipe);
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

void MW_connect(Patch *source, Patch *dest){
  Chip *chip_source = CHIP_get(&g_mixer_widget->scene, source);
  R_ASSERT_RETURN_IF_FALSE(chip_source!=NULL);
  
  Chip *chip_dest = CHIP_get(&g_mixer_widget->scene, dest);
  R_ASSERT_RETURN_IF_FALSE(chip_dest!=NULL);

  CHIP_connect_chips(&g_mixer_widget->scene, chip_source, chip_dest);
}

bool MW_disconnect(Patch *source, Patch *dest){

  QGraphicsScene *scene = &g_mixer_widget->scene;
    
  Chip *chip_from = CHIP_get(scene, source);
  Chip *chip_to = CHIP_get(scene, dest);
  
  return CHIP_disconnect_chips(scene, chip_from, chip_to);
}

void MW_econnect(Patch *source, Patch *dest){
  Chip *chip_source = CHIP_get(&g_mixer_widget->scene, source);
  R_ASSERT_RETURN_IF_FALSE(chip_source!=NULL);
  
  Chip *chip_dest = CHIP_get(&g_mixer_widget->scene, dest);
  R_ASSERT_RETURN_IF_FALSE(chip_dest!=NULL);

  CHIP_econnect_chips(&g_mixer_widget->scene, chip_source, chip_dest);
}

bool MW_edisconnect(Patch *source, Patch *dest){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
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
  Chip *a = CHIP_get(&g_mixer_widget->scene, source);
  Chip *b = CHIP_get(&g_mixer_widget->scene, dest);

  return CHIPS_are_connected(a, b);
}

bool MW_are_econnected(Patch *source, Patch *dest){
  Chip *a = CHIP_get(&g_mixer_widget->scene, source);
  Chip *b = CHIP_get(&g_mixer_widget->scene, dest);

  return CHIPS_are_econnected(a, b);
}


#if 0
static bool delete_a_connection(){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

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
    GFX_ShowProgressMessage("Deleting all connection between instruments");

  CONNECTIONS_remove_all(&g_mixer_widget->scene);
}



static bool delete_a_chip(bool is_loading){
  bool ret = false;
  
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  UNDO_OPEN_REC();{
    
    for (int i = 0; i < das_items.size(); ++i) {
      Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
      if(chip!=NULL){
        printf("  MAKING %p inactive (%s), by force\n",chip,CHIP_get_patch(chip)->name);

        if (is_loading)
          GFX_ShowProgressMessage(talloc_format("Deleting instrument %s", CHIP_get_patch(chip)->name));

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
    
    MW_update_mixer_widget();
    
  }Undo_stop_ignoring_undo_operations();
}

static void get_patches_min_x_y(const vector_t *patches, float &min_x, float &min_y){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  
  min_x = 0;
  min_y = 0;

  bool sat=false;
  
  for (int i = 0; i < das_items.size(); ++i) {
    
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    
    if(chip!=NULL) {
      
      struct Patch *patch = CHIP_get_patch(chip);
      
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

static char *get_patch_key(struct Patch *patch){
  static char temp[128];
  sprintf(temp, "%" PRId64, patch->id);
  return temp;
}


static hash_t *MW_get_audio_patches_state(const vector_t *patches, bool put_in_array){

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  hash_t *chips = HASH_create(das_items.size()/2);

  float min_x, min_y;
  get_patches_min_x_y(patches, min_x, min_y);

  HASH_put_float(chips, "min_x_pos", min_x);
  HASH_put_float(chips, "min_y_pos", min_y);

  int num_chips=0;
  for (int i = 0; i < das_items.size(); ++i) {
    
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    
    if(chip!=NULL) {
      
      struct Patch *patch = CHIP_get_patch(chip);
      
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

static QVector<SuperConnection*> get_connections(void){ 
  QVector<SuperConnection*> ret;
  
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  
  for (int i = 0; i < das_items.size(); ++i) {
    SuperConnection *connection = dynamic_cast<SuperConnection*>(das_items.at(i));
    if(connection!=NULL)
      if(connection->from!=NULL && connection->to!=NULL) // ongoing connections are not real connections.
        ret.push_back(connection);
  }

  return ret;
}

hash_t *MW_get_connections_state(const vector_t *patches){  
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  hash_t *connections = HASH_create(das_items.size());

  auto super_connections = get_connections();
  int num_connections = 0;
  
  for(auto *connection : super_connections){
    if (connection_is_in_patches(connection, patches))
      HASH_put_hash_at(connections, "", num_connections++, CONNECTION_get_state(connection, patches));
  }
  
  HASH_put_int(connections, "num_connections", num_connections);

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


hash_t *MW_get_state(const vector_t *patches, bool include_ab){
  hash_t *state = HASH_create(2);

  HASH_put_hash(state, "chips", MW_get_audio_patches_state(patches, true));
  HASH_put_hash(state, "connections", MW_get_connections_state(patches));
  
  if (include_ab)
    HASH_put_hash(state, "ab_state", MW_get_ab_state());

  HASH_put_dyn(state, "mixer_strips_configuration", MW_get_mixer_strips_state());

  HASH_put_bool(state, "volume_applied_before_drywet", true);
  return state;
}

// Only used when loading song
static void MW_create_chips_from_full_state(hash_t *chips, Buses buses, bool is_loading){

  R_ASSERT(is_loading);
  
  int64_t num_chips = HASH_get_int(chips, "num_chips");
  printf("number of chips: %d\n",(int)num_chips);
  
  for(int i=0;i<num_chips;i++) {
    hash_t *state = HASH_get_hash_at(chips, "", i);

    struct Patch *patch = PATCH_get_from_id(HASH_get_int(state, "patch"));
    if(patch==NULL){
      R_ASSERT(false);
      continue;
    }
    
    if (is_loading)
      GFX_ShowProgressMessage(talloc_format("Creating instrument %d / %d: %s", i, (int)num_chips, patch->name));

    PATCH_init_audio_when_loading_song(patch, state);
  }
}

static void MW_position_chips_from_state(const hash_t *chips, const vector_t *patches, float x, float y){
  
  int64_t num_chips = HASH_get_int(chips, "num_chips");
  printf("number of chips: %d\n",(int)num_chips);

  R_ASSERT_RETURN_IF_FALSE(patches->num_elements==num_chips);

  float min_x = HASH_get_float(chips, "min_x_pos");
  float min_y = HASH_get_float(chips, "min_y_pos");
  
  for(int i=0;i<num_chips;i++) {
    hash_t *state = HASH_get_hash_at(chips, "", i);

    struct Patch *patch = (struct Patch*)patches->elements[i];
    CHIP_set_pos(patch, HASH_get_float(state, "x") + x - min_x, HASH_get_float(state, "y") + y - min_y);
  }
}

// Called from undo_mixer_connections.c
void MW_create_connections_from_state(const hash_t *connections){
  CONNECTIONS_replace_all_with_state(&g_mixer_widget->scene, connections, true);
}

static void add_undo_for_all_chip_positions(void){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL)
      ADD_UNDO(ChipPos_CurrPos(CHIP_get_patch(chip)));
  }
}

// Called from audio/Presest.cpp. (Not used when loading song.)
void MW_create_from_state(const hash_t *state, const vector_t *patches, float x, float y){
  R_ASSERT(patches != NULL);
  R_ASSERT(Undo_Is_Open());
 
  ADD_UNDO(MixerConnections_CurrPos());
  add_undo_for_all_chip_positions(); // We might kick any chip, so we need to add undo points for all chips
          
  MW_position_chips_from_state(HASH_get_hash(state, "chips"), patches, x, y);

  hash_t *connections = HASH_get_hash(state, "connections");

  CONNECTIONS_create_from_presets_state(&g_mixer_widget->scene, connections, patches);
  
  if (patches->num_elements > 1)
    cleanup_chip_positions(&g_mixer_widget->scene);
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
    
    if (!strcmp(type_name,"Bus")) {
      fprintf(stderr, "\n   Bus %d/%d. Id: %d\n", num_buses, i, HASH_get_int32(chip, "patch"));
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
    createAudioInstrument(talloc_strdup("Bus"), talloc_format("Bus %d", busnum+1), talloc_format("Aux %d Bus", busnum-num_chips+1), 0, 0, false);
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
      
    Chip *chip = CHIP_get(&g_mixer_widget->scene, (struct Patch*)SP_get_plugin(sp)->patch);
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
    GFX_ShowProgressMessage("Creating instruments");
  
  MW_create_chips_from_full_state(chips_state, new_buses, is_loading);

  if (is_loading)
    GFX_ShowProgressMessage("Creating connections between sound objects");

  CONNECTIONS_create_from_state(&g_mixer_widget->scene, HASH_get_hash(state, "connections"), -1, -1, -1, -1);

  if (HASH_has_key(state, "ab_state"))
    MW_recreate_ab_from_state(HASH_get_hash(state, "ab_state"));
  
  AUDIO_update_all_permanent_ids();
  
  GFX_update_all_instrument_widgets();
  MW_update_mixer_widget();
  
  autoposition_missing_bus_chips(bus_chips_state);

  if (HASH_has_key(state, "mixer_strips_configuration"))
    MW_apply_mixer_strips_state(HASH_get_dyn(state, "mixer_strips_configuration"));
  else
    gui_resetAllMixerStrips();

  
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
  g_mixer_widget->populateScene();
  gui_resetAllMixerStrips();        
}


// A/B

static hash_t *g_ab_states[MW_NUM_AB] = {};
static bool g_ab_is_valid[MW_NUM_AB] = {};
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
    const char *key = talloc_format("%" PRId64, HASH_get_int(new_patch_state, "id"));
    
    if (!HASH_has_key(curr_patches, key)) {
      
      struct Patch *patch = PATCH_create_audio(NULL, NULL, NULL, new_patch_state, 0, 0);
      HASH_put_int(patch_id_map, key, patch->id);
      
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
  hash_t *state = HASH_create(5);
  HASH_put_hash(state, "connections", MW_get_connections_state(NULL));

  vector_t *patches = &get_audio_instrument()->patches;
  hash_t *plugin_ab_states = HASH_create(patches->num_elements);

  for(int i=0;i<patches->num_elements;i++){
    struct Patch *patch = (struct Patch*)patches->elements[i];
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    HASH_put_hash(plugin_ab_states, get_patch_key(patch), PLUGIN_get_ab_state(plugin));
  }

  HASH_put_hash(state, "plugin_ab_states", plugin_ab_states);

  HASH_put_dyn(state, "mixer_strips_configuration", MW_get_mixer_strips_state());

  return state;
}

static void apply_ab_plugin_ab_states(hash_t *plugin_ab_state, hash_t *curr_plugin_ab_state){
  
  const vector_t &patches = get_audio_instrument()->patches;

  bool updated = false;

  for(int i=0;i<patches.num_elements;i++){
    
    struct Patch *patch=(struct Patch*)patches.elements[i];
    const char *key = get_patch_key(patch);
    
    if (HASH_has_key(plugin_ab_state, key)){
      
      hash_t *ab_state = HASH_get_hash(plugin_ab_state, key);
      hash_t *curr_ab_state = HASH_get_hash(curr_plugin_ab_state, key);
      
      if (!HASH_equal(ab_state, curr_ab_state)){
        
        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        PLUGIN_apply_ab_state(plugin, ab_state);

        CHIP_update(plugin);
        updated = true;
      }
    }
  }

  if (updated){
    GFX_update_current_instrument_widget();
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

static void apply_ab_connections_state(hash_t *connections){

  // All the stuff that was done below should now be handled in QM_chip.cpp / SoundProducer.cpp instead when applying graph changes, and in much better ways.
  CONNECTIONS_replace_all_with_state(&g_mixer_widget->scene, connections, false);

  
#if 0
  const vector_t &patches = get_audio_instrument()->patches;

  vector_t connections_to_create = {0}; // Using vector_t instead of QVector so that the GC won't delete the contents.
  
  for(int i=0;i<HASH_get_int(connections, "num_connections");i++) {
    hash_t *connection_state = HASH_get_hash_at(connections, "", i);
    
    int64_t id_from = HASH_get_int(connection_state, "from_patch");
    int64_t id_to = HASH_get_int(connection_state, "to_patch");
    bool is_event_connection = HASH_get_bool(connection_state, "is_event_connection");

    if (in_patches(&patches, id_from) && in_patches(&patches, id_to)) {
      SuperConnection *connection = get_connection(id_from, id_to, is_event_connection);

      if (connection==NULL) {

        // We must delete old connections before creating new connections. If not, we risk creating recursive connections.
        VECTOR_push_back(&connections_to_create, connection_state);
        
        /*
        CONNECTION_create_from_state(&g_mixer_widget->scene,
                                     connection_state,
                                     -1, -1,
                                     false);
        connection = get_connection(id_from, id_to, is_event_connection);
        R_ASSERT_RETURN_IF_FALSE(connection!=NULL);
        */
      } else {

        if (HASH_has_key(connection_state, "enabled")){
          bool is_enabled = HASH_get_bool(connection_state, "enabled");
          setAudioConnectionEnabled(id_from, id_to, is_enabled, true);
        }

        if (HASH_has_key(connection_state, "gain")){
          float gain = HASH_get_float(connection_state, "gain");
          setAudioConnectionGain(id_from, id_to, gain, true);
        }

        connection->is_ab_touched = true;
        
      }
    
    }
  }

  auto super_connections = get_connections();
  
  for(auto *connection : super_connections){
    
    if (connection->is_ab_touched==false)
      CONNECTION_delete_connection(connection);
    else
      connection->is_ab_touched=false;
    
  }

  VECTOR_FOR_EACH(hash_t *, connection_state, &connections_to_create){
    int64_t id_from = HASH_get_int(connection_state, "from_patch");
    int64_t id_to = HASH_get_int(connection_state, "to_patch");
    bool is_event_connection = HASH_get_bool(connection_state, "is_event_connection");
    CONNECTION_create_from_state(&g_mixer_widget->scene,
                                 connection_state,
                                 -1, -1,
                                 false);
    R_ASSERT_RETURN_IF_FALSE(get_connection(id_from, id_to, is_event_connection) != NULL);
  }END_VECTOR_FOR_EACH;
#endif
}

// 'curr_state' is used to compare states. We skip applying the new state if it isn't different.
static void apply_ab_state(hash_t *state, hash_t *curr_state){

  UNDO_OPEN();{
    apply_ab_plugin_ab_states(HASH_get_hash(state, "plugin_ab_states"),
                              HASH_get_hash(curr_state, "plugin_ab_states")
                              );
    
    apply_ab_connections_state(HASH_get_hash(state, "connections"));

  }UNDO_CLOSE();

  if (HASH_has_key(state, "mixer_strips_configuration"))
    MW_apply_mixer_strips_state(HASH_get_dyn(state, "mixer_strips_configuration"));
}

void MW_change_ab(int ab_num){
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
}

void MW_reset_ab(int num){
  if (num==-1) {
    for(int i=0;i<MW_NUM_AB;i++)
      g_ab_is_valid[i]=false;
    g_curr_ab = 0;
  } else
    g_ab_is_valid[num]=false;
}

hash_t *MW_get_ab_state(void){
  hash_t *ab_state = HASH_create(MW_NUM_AB);

  HASH_put_int(ab_state, "curr_ab_num", g_curr_ab);
      
  for(int i=0;i<MW_NUM_AB;i++){
    bool is_valid = g_ab_is_valid[i];
    
    HASH_put_bool_at(ab_state,"is_valid",i,is_valid);

    if (is_valid)
      HASH_put_hash_at(ab_state, "ab_state", i, g_ab_states[i]);
  }

  return ab_state;
}

void MW_recreate_ab_from_state(hash_t *ab_state){
  
  g_curr_ab = HASH_get_int32(ab_state, "curr_ab_num");

  for(int i=0;i<NUM_AB;i++){
    g_ab_is_valid[i] = HASH_get_bool_at(ab_state, "is_valid", i);
    
    if (g_ab_is_valid[i])
      g_ab_states[i] = HASH_get_hash_at(ab_state, "ab_state", i);
  }

}

#include "mQM_MixerWidget.cpp"
