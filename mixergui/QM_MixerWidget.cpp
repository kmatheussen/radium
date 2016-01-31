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

#include <unistd.h>

#include <QTimer>
#include <QFont>
#include <QMainWindow>
#include <QGraphicsSceneMouseEvent>
#include <QAction>
#include <QMenu>
#include <QFileDialog>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "QM_MixerWidget.h"
#include "QM_view.h"
#include "QM_chip.h"

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


extern EditorWidget *g_editor;

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
                           "* Delete objects or connections by pressing SHIFT and click left.\n"
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


class SlotIndicatorItem : public QGraphicsItem
 {
 public:
     QRectF boundingRect() const
     {
         return QRectF(0,0,grid_width,grid_height);
     }

     void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
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
  _slot_indicator->setZValue(-2);

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
  if(x<0)
    return x/grid_width - 1;
  else
    return x/grid_width;
}

static int get_slot_y(int y){
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
    instrument->PP_Update(instrument,(struct Patch*)patch);
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
      chip2->setSelected(false);
  }
  chip->setSelected(true);
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
  if(chip->isSelected()==false){
    if(event->modifiers() & Qt::ControlModifier)
      chip->setSelected(true);
    else
      MW_set_selected_chip(chip);
  }

  myscene->_moving_chips.push_back(chip);
}

static void start_moving_chips(MyScene *myscene, QGraphicsSceneMouseEvent * event, Chip *main_chip, float mouse_x, float mouse_y){
  Undo_Open();

  handle_chip_selection(myscene, event, main_chip);

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL && chip->isSelected()==true){

      SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
      volatile struct Patch *patch = plugin->patch;
      R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

      Undo_ChipPos_CurrPos((struct Patch*)patch);

      chip->_moving_x_offset = chip->scenePos().x() - mouse_x;
      chip->_moving_y_offset = chip->scenePos().y() - mouse_y;

      if(chip!=main_chip)
        myscene->_moving_chips.push_back(chip);
    }
  }

  //myscene->addItem(_slot_indicator);
  draw_slot(myscene,mouse_x,mouse_y);
}

static bool move_chip_to_slot(Chip *chip, float x, float y){
  float x1,x2,y1,y2;

  get_slot_coordinates(get_slot_x(x), get_slot_y(y), x1,y1,x2,y2);

  if (chip->pos().x()==x1 && chip->pos().y()==y1)
    return false;

  chip->setPos(x1,y1);
  return true;
}

bool MW_move_chip_to_slot(Chip *chip, float x, float y){
  return move_chip_to_slot(chip, x, y);
}
  
static Connection *find_clean_connection_at(MyScene *scene, float x, float y);

// Also kicks.
static bool autoconnect_chip(MyScene *myscene, Chip *chip, float x, float y){
  bool do_autoconnect = chip->_connections.size()==0; // don't autoconnect if the chip already has connections.

  Chip *chip_under = MW_get_chip_at(x,y,chip);

  if(chip_under != NULL){
    if(x_is_placed_on_left_side(x)){

      CHIP_kick_right(chip_under);
      if(do_autoconnect){
        Undo_MixerConnections_CurrPos();
        CHIP_connect_right(myscene, chip, chip_under);
      }

    }else{

      CHIP_kick_left(chip_under);
      if(do_autoconnect){
        Undo_MixerConnections_CurrPos();
        CHIP_connect_left(myscene,chip_under, chip);
      }
    }

    return true;
    
  }else if(do_autoconnect){

    Connection *connection = find_clean_connection_at(myscene, x, y);
    if(connection!=NULL){
      Undo_MixerConnections_CurrPos();

      Chip *from = connection->from;
      Chip *to = connection->to;
      CONNECTION_delete_connection(connection);
      CHIP_connect_chips(myscene, from, chip);
      CHIP_connect_chips(myscene, chip, to);

      return true;
    }

  }

  return false;
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

static bool stop_moving_chips(MyScene *myscene, float mouse_x, float mouse_y){

  //myscene->removeItem(_slot_indicator);
  myscene->_parent->setCursor(QCursor(Qt::ArrowCursor));

  Chip *main_chip = myscene->_moving_chips.at(0);
  float main_chip_x = main_chip->pos().x();
  float main_chip_y = main_chip->pos().y();

  int size = myscene->_moving_chips.size();
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

  cleanup_chip_positions(myscene);

  Undo_Close();

  if (has_updated==false)
    Undo_CancelLastUndo();

  myscene->_moving_chips.clear();

  return has_updated;
}

void MyScene::mouseMoveEvent ( QGraphicsSceneMouseEvent * event ){

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
      if(get_slot_x(pos.x()+grid_width/2)==slot_x && get_slot_y(pos.y()+grid_height/2)==slot_y)
        return chip;
    }
  }

  return NULL;
}

static Connection *find_clean_connection_at(MyScene *scene, float x, float y){
  int slot_x = get_slot_x(x);
  int slot_y = get_slot_y(y);

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Connection *connection = dynamic_cast<Connection*>(das_items.at(i));
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
  std::vector<Connection*> connections=chip->_connections;
  if(connections.size()!=2)
    return false;

  if(connections.at(0)->to==chip && connections.at(1)->to==chip)
    return false;

  if(connections.at(0)->from==chip && connections.at(1)->from==chip)
    return false;

  if(connections.at(0)->to==chip){
    *before = connections.at(0)->from;
    *after  = connections.at(1)->to;
  }else{
    *before = connections.at(1)->from;
    *after  = connections.at(0)->to;
  }

  return true;
}

static bool mousepress_delete_chip(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y){
  printf("Going to delete\n");
  Chip *chip = dynamic_cast<Chip*>(item);
  if(chip!=NULL){
    printf("Got chip\n");

    Chip *before=NULL;
    Chip *after=NULL;
    get_before_and_after_chip(chip, &before, &after);
    
    struct Instruments *instrument = get_audio_instrument();
    VECTOR_FOR_EACH(struct Patch *,patch,&instrument->patches){
      if(patch->patchdata==SP_get_plugin(chip->_sound_producer)){
        printf("Found patch\n");
        PATCH_delete_CurrPos(patch);
        break;
      }
    }END_VECTOR_FOR_EACH;

    if(before!=NULL)
      CHIP_connect_chips(scene, before, after); // undo for the connections are made in PATCH_delete_CurrPos

    // Shouldn't there be a "delete chip" call here? (Guess it's deleted through PATCH_delete). TODO: Check that this is correct, and add a comment here why there is no "delete chip" call here.

    event->accept();
    return true;
  }

  return false;
}


static bool mousepress_start_connection(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y){

  Chip *chip = get_chip_with_port_at(scene,mouse_x,mouse_y);
  printf("chip: %p\n", chip);

  if(chip!=NULL){

    struct Instruments *instrument = get_audio_instrument();
    SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL,false);
    instrument->PP_Update(instrument,(struct Patch*)patch);
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
        
        scene->_current_connection = new Connection(scene);
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
        scene->_current_econnection = new Connection(scene, true);
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
  Connection *connection = dynamic_cast<Connection*>(item);

  if(connection==NULL){
    QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
    for (int i = 0; i < das_items.size(); ++i) {
      connection = dynamic_cast<Connection*>(das_items.at(i));
      if(connection!=NULL && connection->isUnderMouse()==true)
        break;
      else
        connection = NULL;
    }
  }

  if(connection!=NULL){
    Undo_MixerConnections_CurrPos();
    CONNECTION_delete_connection(connection);
    event->accept();
    return true;
  }
  return false;
}

static bool mousepress_select_chip(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y, bool ctrl_pressed){
  Chip *chip = dynamic_cast<Chip*>(item);

  if(chip!=NULL){
    SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);
    
    struct Instruments *instrument = get_audio_instrument();
    printf("Calling pp_update\n");
    instrument->PP_Update(instrument,(struct Patch*)patch);

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
      volatile struct Patch *patch = plugin->patch;
      R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);

      InstrumentWidget_replace((struct Patch*)patch);
        
      return true;
    }
  }
  return false;
}

static bool mousepress_create_chip(MyScene *scene, QGraphicsSceneMouseEvent * event, QGraphicsItem *item, float mouse_x, float mouse_y){

  Chip *chip_under = MW_get_chip_at(mouse_x,mouse_y,NULL);
  if(chip_under!=NULL){
    if(chip_body_is_placed_at(chip_under, mouse_x, mouse_y)==true)
      return false;
  }

  //scene->addItem(_slot_indicator);
  draw_slot(scene,mouse_x,mouse_y);

  Undo_Open();{

    SoundPlugin *plugin = add_new_audio_instrument_widget(NULL,mouse_x,mouse_y,false,NULL, MIXER_get_buses());

    if(plugin!=NULL){

      Chip *chip = find_chip_for_plugin(scene, plugin);
      
      autoconnect_chip(scene, chip, mouse_x, mouse_y);
    }

  }Undo_Close();
  
  //scene->removeItem(_slot_indicator);
  event->accept();
  return true;
}

static bool event_can_delete(QGraphicsSceneMouseEvent *event){
  if(event->button()==Qt::MiddleButton)
    return true;

  else if(event->button()==Qt::LeftButton && (event->modifiers() & Qt::ShiftModifier))
    return true;

  else
    return false;
}

void MyScene::mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event ){
  QPointF pos=event->scenePos();
  printf("Scene is double-clicked\n");

  Chip *chip = MW_get_chip_at(pos.x(), pos.y(), NULL);
  if(chip!=NULL){
    if(SP_get_plugin(chip->_sound_producer)->type->show_gui != NULL)
      SP_get_plugin(chip->_sound_producer)->type->show_gui(SP_get_plugin(chip->_sound_producer));
    event->accept();
  }
}

void MyScene::mousePressEvent(QGraphicsSceneMouseEvent *event){
  printf("mousepress: %p\n",_current_connection);

  QPointF pos=event->scenePos();
  float mouse_x = pos.x();
  float mouse_y = pos.y();

  QGraphicsItem *item = itemAt(event->scenePos());

  printf("mouse button: %d %d\n",event->button(),Qt::MiddleButton);

  GFX_ScheduleRedraw();

  bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
  
  if(event_can_delete(event))
    if(mousepress_delete_chip(this,event,item,mouse_x,mouse_y)==true)
      return;

  if(event_can_delete(event))
    if(mousepress_delete_connection(this,event,item,mouse_x,mouse_y)==true)
      return;

  if(event->button()==Qt::LeftButton) {
    if(ctrl_pressed==false && mousepress_start_connection(this,event,item,mouse_x,mouse_y)==true)
      return;

    if(ctrl_pressed==true && mousepress_select_chip(this,event,item,mouse_x,mouse_y,ctrl_pressed)==true) // select
      return;

  } if(event->button()==Qt::RightButton){
    
    if(ctrl_pressed==false && mousepress_create_chip(this,event,item,mouse_x,mouse_y)==true) // create
      return;

    // start moving chip    
    Chip *chip = dynamic_cast<Chip*>(item);
      
    if(chip!=NULL){
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
  printf("mouse release: %p\n",_current_connection);

  GFX_ScheduleRedraw();
    
  QPointF pos=event->scenePos();
  float mouse_x = pos.x();
  float mouse_y = pos.y();

  Chip *chip = MW_get_chip_at(mouse_x, mouse_y, NULL);

  if(_current_connection!=NULL){

    if(chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_current_from_chip != NULL && chip != _current_from_chip){

        Undo_MixerConnections_CurrPos();
        CHIP_connect_chips(this, _current_from_chip, chip);

      }else if(_current_to_chip != NULL && chip != _current_to_chip){

        Undo_MixerConnections_CurrPos();
        CHIP_connect_chips(this, chip, _current_to_chip);

      }
    }

    removeItem(_current_connection);
    delete _current_connection;     
    _current_connection = NULL;
    _current_from_chip = NULL;
    _current_to_chip = NULL;
    event->accept();
    

  }else if(_current_econnection!=NULL){

    if(chip!=NULL){ // TODO: Must check if the connection is already made.

      if(_ecurrent_from_chip != NULL && chip != _ecurrent_from_chip){

        Undo_MixerConnections_CurrPos();
        CHIP_econnect_chips(this, _ecurrent_from_chip, chip);

      }else if(_ecurrent_to_chip != NULL && chip != _ecurrent_to_chip){

        Undo_MixerConnections_CurrPos();
        CHIP_econnect_chips(this, chip, _ecurrent_to_chip);

      }
    }

    removeItem(_current_econnection);
    delete _current_econnection;     
    _current_econnection = NULL;
    _ecurrent_from_chip = NULL;
    _ecurrent_to_chip = NULL;
    event->accept();
    
  }else if(_moving_chips.size()>0 && stop_moving_chips(this,mouse_x,mouse_y)==true){

    event->accept();

  }else{

    bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
    
    if (event->button()==Qt::RightButton && ctrl_pressed==false && mouserelease_replace_patch(this,mouse_x,mouse_y)==true) {
      event->accept();
      return;
    }

    QGraphicsScene::mouseReleaseEvent(event);

  }
}

MixerWidget *g_mixer_widget = NULL;

namespace{
  class MixerWidgetTimer : public QTimer{
    void 	timerEvent ( QTimerEvent * e ){
      QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
      
      for (int i = 0; i < das_items.size(); ++i) {
        Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
        if(chip!=NULL){
          SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
          if(plugin != NULL){
            volatile struct Patch *patch = plugin->patch;
            if(patch!=NULL){
              if(ATOMIC_GET(patch->visual_note_intencity) > 0) {
                ATOMIC_ADD_RETURN_OLD(patch->visual_note_intencity, -1);
                //printf("intencity: %d\n",intencity);
                int x1,y1,x2,y2;
                CHIP_get_name_coordinates(x1,y1,x2,y2);
                chip->update(x1,y1,x2-x1,y2-y1);
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

    populateScene();

    QGridLayout *gridLayout = new QGridLayout(this);  
    gridLayout->setContentsMargins(0, 0, 0, 0);

#if 0
    View *view = new View("Das mixer",this);
    view->view()->setScene(&scene);
    gridLayout->addWidget(view, 0, 0, 1, 1);

#else

    Mixer_widget *mixer_widget = new Mixer_widget(this);
    mixer_widget->view->setScene(&scene);
    gridLayout->addWidget(mixer_widget, 0, 0, 1, 1);
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

    setWindowTitle(tr("Chip Demo"));

    {
      MixerWidgetTimer *timer = new MixerWidgetTimer;
      timer->setInterval(30);
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
}

void GFX_showHideMixerWidget(void){
  GL_lock();{
    if(g_mixer_widget->isHidden())
      g_mixer_widget->show();
    else
      g_mixer_widget->hide();
  }GL_unlock();
}

static int g_main_pipe_patch_id = 0;

SoundPlugin *get_main_pipe(void){
  struct Patch *patch = PATCH_get_from_id(g_main_pipe_patch_id);
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;

  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();
  for (int i = 0; i < das_items.size(); ++i){
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return plugin;
    }
  }

  RError("no system bus");
  return NULL;
}


void MixerWidget::populateScene()
{
#if 0
  SoundPluginType *type1 = PR_get_plugin(0);
  SoundPlugin *plugin1 = PLUGIN_create_plugin(type1, NULL);

  SoundPluginType *type2 = PR_get_plugin(1);
  SoundPlugin *plugin2 = PLUGIN_create_plugin(type2, NULL);

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

void MW_autoconnect_plugin(SoundPlugin *plugin){
  SoundPlugin *main_pipe = get_main_pipe();

  if(plugin->type->num_outputs>0)
    CHIP_connect_chips(&g_mixer_widget->scene, plugin, main_pipe);
}


static float find_next_autopos_y(Chip *system_chip){
  int x = system_chip->x()-grid_width;
  int y = system_chip->y();
  while(MW_get_chip_at(x,y,NULL)!=NULL)
    y+=grid_height;
  return y;
}

void MW_set_autopos(double *x, double *y){
  SoundPlugin *main_pipe    = get_main_pipe();
  Chip        *system_chip  = find_chip_for_plugin(&g_mixer_widget->scene, main_pipe);
  *x                         = system_chip->x()-grid_width;
  *y                         = find_next_autopos_y(system_chip);
  printf("Adding at pos %f %f\n",*x,*y);
}

// MW_add_plugin/MW_delete_plugin are one of two entry points for audio plugins.
// Creating/deleting a plugin goes through here, not through audio/.
//
// The other entry point is CHIP_create_from_state, which is called from undo/redo and load.
//
SoundPlugin *MW_add_plugin(SoundPluginType *plugin_type, double x, double y, Buses buses){
  if (PLAYER_is_running()==false)
    return NULL;

  SoundPlugin *plugin = PLUGIN_create_plugin(plugin_type, NULL);
  if(plugin==NULL)
    return NULL;

  if(x<=-100000)
    MW_set_autopos(&x, &y);
    
  SoundProducer   *sound_producer = SP_create(plugin, buses);
  Chip *chip = new Chip(&g_mixer_widget->scene,sound_producer,x,y);

  MW_move_chip_to_slot(chip, x, y);
    
  return plugin;
}

void MW_delete_plugin(SoundPlugin *plugin){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      SoundProducer *producer = chip->_sound_producer;
      if(SP_get_plugin(producer)==plugin){
        delete chip; // audio connections are deleted via ~Chip(). (Yes, it's somewhat messy)
        SP_delete(producer);
        volatile struct Patch *patch = plugin->patch;
        PLUGIN_delete_plugin(plugin);
        patch->patchdata = NULL; // Correct thing to do. A subtle bug in GFX_update_all_instrument_widgets prompted me to do add it (QT tabs are note updated right away). Somewhat messy this too.
        patch->is_usable = false; // Make sure we don't use this patch if pasting it.
        return;
      }
    }
  }
}

namespace{
  struct MyQAction : public QAction{
    MyQAction(const char* name, QMenu *menu, PluginMenuEntry entry)
      : QAction(name,menu)
      , entry(entry)
    {}
    PluginMenuEntry entry;
  };
}

static int menu_up(QMenu *menu, const radium::Vector<PluginMenuEntry> &entries, int i){
  while(i < entries.size()){
    PluginMenuEntry entry = entries[i];
    i++;

    if(entry.type==PluginMenuEntry::IS_SEPARATOR){
      menu->addSeparator();

    }else if(entry.type==PluginMenuEntry::IS_LEVEL_UP){
      const char *name = entry.level_up_name;
      QMenu *new_menu = new QMenu(name,menu);
      menu->addMenu(new_menu);
      i = menu_up(new_menu,entries, i);

    }else if(entry.type==PluginMenuEntry::IS_LEVEL_DOWN){
      return i;

    }else if(entry.type==PluginMenuEntry::IS_CONTAINER && entry.plugin_type_container->is_populated){
      SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;

      for(int i=0 ; i < plugin_type_container->num_types ; i++){
        SoundPluginType *type = plugin_type_container->plugin_types[i];
        const char *name = type->name;
        menu->addAction(new MyQAction(name,menu,PluginMenuEntry::normal(type)));
      }
    
    }else if(entry.type==PluginMenuEntry::IS_CONTAINER){
      SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;
      const char *name = plugin_type_container->name;
      menu->addAction(new MyQAction(name,menu,entry));

    }else if(entry.type==PluginMenuEntry::IS_LOAD_PRESET){
      menu->addAction(new MyQAction("Load Preset", menu, entry));
      menu->addSeparator();

    }else{
      const char *name = entry.plugin_type->name;
      menu->addAction(new MyQAction(name,menu,entry));
    }
  }

  return i;
}


SoundPluginType *MW_popup_plugin_selector(const char *name, double x, double y, bool autoconnect){
  QMenu menu(0);

  menu_up(&menu, PR_get_menu_entries(), 0);

  MyQAction *action;

  if (doModalWindows()) {
    
    GL_lock();{
      action = dynamic_cast<MyQAction*>(menu.exec(QCursor::pos()));
    }GL_unlock();
    
  } else {
    
    GL_lock();{
      GL_pause_gl_thread_a_short_while();
    }GL_unlock();    
    action = dynamic_cast<MyQAction*>(menu.exec(QCursor::pos()));
    
  }
  
  if (action==NULL)
    return NULL;

  struct PluginMenuEntry entry = action->entry;

  if (entry.type==PluginMenuEntry::IS_CONTAINER) {

    vector_t names={};

    SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;
    plugin_type_container->populate(plugin_type_container);

    if (plugin_type_container->num_types==0) {
      //GFX_Message(NULL, talloc_format("%s does not contain any plugin",plugin_type_container->name));
      return NULL; // no error message here. populate() must do that.
    }
     
    if (plugin_type_container->num_types==1)
      return plugin_type_container->plugin_types[0];

    for(int i=0 ; i < plugin_type_container->num_types ;  i++)
      VECTOR_push_back(&names,plugin_type_container->plugin_types[i]->name);

    char temp[1024];
    sprintf(temp,"Select plugin contained in %s", plugin_type_container->name);
    int selection=GFX_Menu(NULL,NULL,temp,&names);
    
    if (selection==-1)
      return NULL;
    else
      return plugin_type_container->plugin_types[selection];

   }else if(entry.type==PluginMenuEntry::IS_LOAD_PRESET){
    InstrumentWidget_new_from_preset(NULL, name, x, y, autoconnect);
    
    return NULL;
    
  } else {

    return entry.plugin_type;

  }
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

static void MW_cleanup_connections(void){
  radium::Vector<SoundProducer*> producers;
  radium::Vector<Connection*> connections;
  
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL)
      producers.add(chip->_sound_producer);
    else{
      Connection *connection = dynamic_cast<Connection*>(das_items.at(i));
      if(connection!=NULL)
        connections.add(connection);
    }
  }

  SP_remove_all_links(producers);

  for(auto producer : producers){
    SoundPlugin *plugin = SP_get_plugin(producer);
    volatile struct Patch *patch = plugin->patch;
    if (patch!=NULL)
      PATCH_remove_all_event_receivers((struct Patch*)patch);
  }

  for(auto connection : connections)
    CONNECTION_delete_a_connection_where_all_links_have_been_removed(connection);
}



static bool delete_a_chip(){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      MW_delete_plugin(SP_get_plugin(chip->_sound_producer));
      return true;
    }
  }
  return false;
}

void MW_cleanup(void){
  MW_cleanup_connections(); // All connecting connections are deleted when deleting a chip as well, but it's a lot faster to delete all connections in one go than deleting them one by one since we only have to wait for the audio thread one time.
  while(delete_a_chip()); // remove all chips. All connections are removed as well when removing all chips.
}

static hash_t *MW_get_chips_state(void){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  hash_t *chips = HASH_create(das_items.size()/2);
    
  int num_chips=0;
  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL)
      HASH_put_hash_at(chips, "", num_chips++, CHIP_get_state(chip));
  }
  
  HASH_put_int(chips, "num_chips", num_chips);

  return chips;
}

hash_t *MW_get_connections_state(void){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  hash_t *connections = HASH_create(das_items.size());

  int num_connections=0;
  for (int i = 0; i < das_items.size(); ++i) {
    Connection *connection = dynamic_cast<Connection*>(das_items.at(i));
    if(connection!=NULL)
      if(connection->from!=NULL && connection->to!=NULL) // dont save ongoing connections.
        HASH_put_hash_at(connections, "", num_connections++, CONNECTION_get_state(connection));
  }
  
  HASH_put_int(connections, "num_connections", num_connections);

  return connections;
}

hash_t *MW_get_state(void){
  hash_t *state = HASH_create(2);

  HASH_put_hash(state, "chips", MW_get_chips_state());
  HASH_put_hash(state, "connections", MW_get_connections_state());

  return state;
}

static void MW_create_chips_from_state(hash_t *chips, Buses buses){
  fprintf(stderr,"number of chips: %d\n",(int)HASH_get_int(chips, "num_chips"));
  for(int i=0;i<HASH_get_int(chips, "num_chips");i++)
    CHIP_create_from_state(HASH_get_hash_at(chips, "", i), buses);
}

static void MW_create_connections_from_state_internal(hash_t *connections, int patch_id_old, int patch_id_new){
  for(int i=0;i<HASH_get_int(connections, "num_connections");i++)
    CONNECTION_create_from_state(&g_mixer_widget->scene, HASH_get_hash_at(connections, "", i), patch_id_old, patch_id_new);
}

void MW_create_connections_from_state_and_replace_patch(hash_t *connections, int patch_id_old, int patch_id_new){
  MW_cleanup_connections();
  MW_create_connections_from_state_internal(connections, patch_id_old, patch_id_new);
}

void MW_create_connections_from_state(hash_t *connections){
  MW_create_connections_from_state_and_replace_patch(connections, -1, -1);
}

// FIXME/TODO!

static hash_t *convert_state_to_new_type(hash_t *state){
  hash_t *old_chips = HASH_get_hash(state, "chips");
  int num_old_chips = HASH_get_int(old_chips, "num_chips");
  
  hash_t *new_chips = HASH_create(17);
  hash_t *buses = HASH_create(2);

  int num_buses = 0;
  int num_chips = 0;
  
  for(int i=0;i<num_old_chips;i++) {
    hash_t *chip = HASH_get_hash_at(old_chips, "", i);
    hash_t *plugin = HASH_get_hash(chip, "plugin");    
    const char *type_name = HASH_get_chars(plugin, "type_name");
    
    if (!strcmp(type_name,"Bus"))
      HASH_put_hash_at(buses, "", num_buses++, chip);
    else
      HASH_put_hash_at(new_chips, "", num_chips++, chip);
  }

  R_ASSERT(num_buses==2);
  R_ASSERT(num_buses+num_chips == num_old_chips);
  
  HASH_put_int(new_chips, "num_chips", num_chips);
  HASH_put_int(buses, "num_chips", num_buses);

  hash_t *ret = HASH_create(3);

  HASH_put_hash(ret, "bus_chips", buses);
  HASH_put_hash(ret, "chips", new_chips);
  HASH_put_hash(ret, "connections", HASH_get_hash(state, "connections"));
                
  return ret;
}

// Patches must be created before calling this one.
// However, patch->patchdata are created here.
void MW_create_from_state(hash_t *state){

  MW_cleanup();

  if (!HASH_has_key(state, "bus_chips"))
    state = convert_state_to_new_type(state);

  Buses old_buses = MIXER_get_buses();

  Buses no_buses = {NULL, NULL};
  MW_create_chips_from_state(HASH_get_hash(state, "bus_chips"), no_buses);

  Buses new_buses = MIXER_get_buses();

  
  if (old_buses.bus1!=NULL && new_buses.bus1!=NULL)
    R_ASSERT(SP_get_id(old_buses.bus1) != SP_get_id(new_buses.bus1));
  else  
    R_ASSERT(old_buses.bus1 != new_buses.bus1);
  
  if (old_buses.bus2!=NULL && new_buses.bus2!=NULL)
    R_ASSERT(SP_get_id(old_buses.bus2) != SP_get_id(new_buses.bus2));
  else  
    R_ASSERT(old_buses.bus2 != new_buses.bus2);
  

  MW_create_chips_from_state(HASH_get_hash(state, "chips"), new_buses);
  MW_create_connections_from_state_internal(HASH_get_hash(state, "connections"), -1, -1);

  GFX_update_all_instrument_widgets();
}

// This function is called when loading a song saved with a version of radium made before the audio system was added.
void MW_create_plain(void){
  MW_cleanup();
  g_mixer_widget->populateScene();
}

#include "mQM_MixerWidget.cpp"
