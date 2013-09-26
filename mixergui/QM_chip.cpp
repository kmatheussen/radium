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

#include <algorithm>
#include <set>

#include <QMainWindow>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/instruments_proc.h"

#include "undo_mixer_proc.h"

#include "QM_MixerWidget.h"
#include "QM_chip.h"
#include "undo_chip_position_proc.h"

#include "../audio/audio_instrument_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../Qt/EditorWidget.h"
#include "../Qt/Qt_instruments_proc.h"
//#include "../Qt/Qt_MyQCheckBox.h"

#include "../common/patch_proc.h"

extern EditorWidget *g_editor;

//
// eport/econnection means "event port"/"event connection".
//


int get_text_width(const QFont &font, const QString &text){
  const QFontMetrics fn = QFontMetrics(font);
  return fn.width(text);
}

static const int left_border   = 2;
static const int right_border  = 2;
static const int top_border    = 2;
static const int bottom_border = 2;

//static const int chip_width = 120;
//static const int slider_height = 15;

static const int name_height = 17;
static const int button_width = 13;//name_height/2;
static const int button_height = 13;//name_height/2;



// coordinates

static int chip_box_x1 = port_width + left_border;
static int chip_box_x2 = chip_width - port_width - right_border;
static int chip_box_y1 = port_height + top_border;

static int get_chip_visible_height(void){
  //  int x1,y1,x2,y2;
  //get_name_coordinates(x1,y1,x2,y2);
  //return y2 + bottom_border;
  return chip_height-(port_height*2);
}

static void get_coordinates(int &x1, int &y1, int &x2, int &y2){
  x1=port_width;
  y1=port_height;
  x2=chip_width-(port_width*2);
  y2=y1+get_chip_visible_height();
  //  int t1,t2,t3;
  //  get_name_coordinates(t1,t2,t3,y2);
}

static void get_name_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);
  //get_slider2_coordinates(x1,y1,x2,y2);

  x1 += 2;
  x2 = x2-button_width-3;
  y1 = y2-name_height;

  //  y1+=slider_height;
  //  y2+=name_height;
}

static void get_slider1_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_name_coordinates(x1,y1,x2,y2);

  x1 = chip_box_x1;
  x2 = chip_box_x2;
  y2 = y1;
  y1 = chip_box_y1;
}


static void get_slider2_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_slider1_coordinates(x1,y1,x2,y2);
  //y1+=slider_height;
  //y2+=slider_height;
}


#if 0
static void get_input_slider_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_slider1_coordinates(x1,y1,x2,y2);
}

static void get_output_slider_coordinates(Chip *chip, int &x1, int &y1, int &x2, int &y2){
  if(chip->_num_inputs==0)
    get_slider1_coordinates(x1,y1,x2,y2);
  else
    get_slider2_coordinates(x1,y1,x2,y2);
}
#endif

#if 0
static void get_dontbypass_button_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_makesound_button_coordinates(x1,y1,x2,y2);
  y1 = y2;
  y2 = y1+button_height;
}
#endif

static void get_volume_onoff_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);

  x1 = x2-button_width-1;
  //y1 = 0;
  y2 = y1 + button_width-3;
}

static void get_effects_onoff_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);

  x1 = x2-button_width-1;
  y1 = y2-button_width+2;
}



// audio port coordinates (port coordinates)

int CHIP_get_input_port_x(Chip *chip){
  return chip->x() + port_width/2 + 2;
}

static int get_input_port_x1(Chip *chip){
  return CHIP_get_input_port_x(chip)-port_width/2;
}

static int get_input_port_x2(Chip *chip){
  return CHIP_get_input_port_x(chip) + port_width/2;
}

int CHIP_get_output_port_x(Chip *chip){
  QRectF rect = chip->boundingRect();
  return chip->x()+rect.width() - port_width/2 - 2;
}

static int get_output_port_x1(Chip *chip){
  return CHIP_get_output_port_x(chip)-port_width/2;
}

static int get_output_port_x2(Chip *chip){
  return CHIP_get_output_port_x(chip) + port_width/2;
}

int CHIP_get_port_y(Chip *chip){
  QRectF rect = chip->boundingRect();
  //printf("y: %d, y2: %d\n",(int)chip->y(),(int)(chip->y()+rect.height()));
  return chip->y() + rect.height()/2;
}

static int get_port_y1(Chip *chip){
  //QRectF rect = chip->boundingRect();
  return chip->y();
}

static int get_port_y2(Chip *chip){
  QRectF rect = chip->boundingRect();
  return chip->y() + rect.height();
}

bool CHIP_is_at_input_port(Chip *chip, int x, int y){
  return (x >= get_input_port_x1(chip))
    && (x < get_input_port_x2(chip))
    && (y >= get_port_y1(chip))
    && (y < get_port_y2(chip));
}

bool CHIP_is_at_output_port(Chip *chip, int x, int y){
  return (x >= get_output_port_x1(chip))
    && (x < get_output_port_x2(chip))
    && (y >= get_port_y1(chip))
    && (y < get_port_y2(chip));
}


// control port coordinates (eport coordinates)

int CHIP_get_eport_x(Chip *chip){
  return chip->x() + chip_width/2;
}

static int get_eport_x1(Chip *chip){
  return CHIP_get_eport_x(chip)-port_width/2;
}

static int get_eport_x2(Chip *chip){
  return CHIP_get_eport_x(chip) + port_width/2;
}

int CHIP_get_input_eport_y(Chip *chip){
  return chip->y() - port_height/2;
}

int CHIP_get_output_eport_y(Chip *chip){
  return chip->y() + chip_height - port_height/2;
}

static int get_input_eport_y1(Chip *chip){
  return CHIP_get_input_eport_y(chip)-port_height/2;
}

static int get_input_eport_y2(Chip *chip){
  return CHIP_get_input_eport_y(chip)+port_height/2;
}

static int get_output_eport_y1(Chip *chip){
  return CHIP_get_output_eport_y(chip)-port_height/2;
}

static int get_output_eport_y2(Chip *chip){
  return CHIP_get_output_eport_y(chip)+port_height/2;
}

bool CHIP_is_at_input_eport(Chip *chip, int x, int y){
  printf("x/y: %d/%d. x1/y1: %d/%d. x2/y2: %d/%d\n",x,y,
         get_eport_x1(chip),get_input_eport_y1(chip),
         get_eport_x2(chip),get_input_eport_y2(chip));

  return (x >= get_eport_x1(chip))
    && (x < get_eport_x2(chip))
    && (y >= get_input_eport_y1(chip))
    && (y < get_input_eport_y2(chip));
}

bool CHIP_is_at_output_eport(Chip *chip, int x, int y){
  return (x >= get_eport_x1(chip))
    && (x < get_eport_x2(chip))
    && (y >= get_output_eport_y1(chip))
    && (y < get_output_eport_y2(chip));
}



// stuff 


static void paint_checkbutton(QPainter *painter, int x1, int y1, int x2, int y2, bool is_on){
  painter->setPen(QPen(Qt::black, 2));

  painter->drawRect(x1, y1, x2-x1, y2-y1);
  if(is_on){
    QColor c(10,12,30,65);
    painter->setPen(QPen(c, 2));

    painter->fillRect(x1, y1, x2-x1-1, y2-y1, c);
  }
}

Chip *find_chip_for_plugin(QGraphicsScene *scene, SoundPlugin *plugin){
  QList<QGraphicsItem *> das_items = scene->items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return chip;
    }
  }
  return NULL;
}

void CHIP_update(SoundPlugin *plugin){
  Chip *chip = find_chip_for_plugin(&g_mixer_widget->scene, plugin);

  int eff1 = plugin->type->num_effects+EFFNUM_INPUT_VOLUME;
  int eff2 = plugin->type->num_effects+EFFNUM_VOLUME;

  int val1 = scale(PLUGIN_get_effect_value(plugin, eff1, VALUE_FROM_STORAGE), 0,1,0,10000);
  int val2 = scale(PLUGIN_get_effect_value(plugin, eff2, VALUE_FROM_STORAGE), 0,1,0,10000);

  SLIDERPAINTER_setValue(chip->_input_slider, val1);
  SLIDERPAINTER_setValue(chip->_output_slider, val2);

#if 0
  {
    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, eff1, buf, 64);
    SLIDERPAINTER_set_string(chip->_input_slider, QString(buf));
  }

  {
    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, eff2, buf, 64);
    SLIDERPAINTER_set_string(chip->_output_slider, QString(buf));
  }
#endif

  chip->update();
}

float CHIP_get_pos_x(struct Patch *patch){
  Chip *chip = find_chip_for_plugin(&g_mixer_widget->scene, (SoundPlugin*)patch->patchdata);
  return chip->x();
}

float CHIP_get_pos_y(struct Patch *patch){
  Chip *chip = find_chip_for_plugin(&g_mixer_widget->scene, (SoundPlugin*)patch->patchdata);
  return chip->y();
}

void CHIP_set_pos(struct Patch *patch,float x,float y){
  Chip *chip = find_chip_for_plugin(&g_mixer_widget->scene, (SoundPlugin*)patch->patchdata);
  chip->setPos(x,y);
}

static void CHIP_kick_left_rec(Chip *chip, std::set<Chip*> &kicks){
  QPointF pos=chip->pos();

#if 0
  // First move chips which are connected. (but only if they are placed to the right of this chip)
  for(unsigned int i=0;i<chip->_connections.size();i++){
    Connection *connection = chip->_connections.at(i);
    if(connection->from==chip){
      Chip *chip2 = connection->to;
      if(chip2->pos().x() >= pos.x())
        CHIP_kick_right_rec(chip2, kicks);
    }
  }
#endif

  // Then move chip placed to the right
  Chip *chip2 = MW_get_chip_at(pos.x()-grid_width + chip_width/2, pos.y() + chip_height/2,chip);
  if(chip2!=NULL)    
    CHIP_kick_left_rec(chip2, kicks);

  // Move the chip itself
  kicks.insert(chip);
}

void CHIP_kick_left(Chip *chip){
  std::set<Chip*> kicks;

  CHIP_kick_left_rec(chip, kicks);

  for(std::set<Chip*>::iterator chip = kicks.begin() ; chip!=kicks.end() ; chip++){
    QPointF pos=(*chip)->pos();
    Undo_ChipPos_CurrPos(SP_get_plugin((*chip)->_sound_producer)->patch);
    (*chip)->setPos(pos.x()-grid_width, pos.y());
  }
}

static void CHIP_kick_right_rec(Chip *chip, std::set<Chip*> &kicks){
  QPointF pos=chip->pos();

#if 0 // Disabled. Too messy.

  // First move chips which are connected. (but only if they are placed to the right of this chip)
  for(unsigned int i=0;i<chip->_connections.size();i++){
    Connection *connection = chip->_connections.at(i);
    if(connection->from==chip){
      Chip *chip2 = connection->to;
      if(chip2->pos().x() >= pos.x())
        CHIP_kick_right_rec(chip2, kicks);
    }
  }
#endif

  // Then move chip placed to the right
  Chip *chip2 = MW_get_chip_at(pos.x()+grid_width + chip_width/2, pos.y() + chip_height/2,chip);
  if(chip2!=NULL)    
    CHIP_kick_right_rec(chip2, kicks);

  // Move the chip itself
  kicks.insert(chip);
}

void CHIP_kick_right(Chip *chip){
  std::set<Chip*> kicks;

  CHIP_kick_right_rec(chip, kicks);

  for(std::set<Chip*>::iterator chip = kicks.begin() ; chip!=kicks.end() ; chip++){
    QPointF pos=(*chip)->pos();
    Undo_ChipPos_CurrPos(SP_get_plugin((*chip)->_sound_producer)->patch);
    (*chip)->setPos(pos.x()+grid_width, pos.y());
  }
}

static bool connect(QGraphicsScene *scene, Chip *from, int from_portnum, Chip *to, int to_portnum){
  return SP_add_link(to->_sound_producer, to_portnum, 
                     from->_sound_producer, from_portnum);
}

static bool econnect(QGraphicsScene *scene, Chip *from, Chip *to){
  return PATCH_add_event_receiver(SP_get_plugin(from->_sound_producer)->patch,
                                  SP_get_plugin(to->_sound_producer)->patch);
}

static bool chips_are_connected(Chip *from, Chip *to){
  for(unsigned int i=0;i<from->_connections.size();i++){
    Connection *connection = from->_connections.at(i);
    if(connection->from==from && connection->to==to)
      return true;
  }

  return false;
}

static bool chips_are_econnected(Chip *from, Chip *to){
  for(unsigned int i=0;i<from->_econnections.size();i++){
    Connection *econnection = from->_econnections.at(i);
    if(econnection->from==from && econnection->to==to)
      return true;
  }

  return false;
}

void CHIP_connect_chips(QGraphicsScene *scene, Chip *from, Chip *to){
  if(from->_num_outputs==0 || to->_num_inputs==0)
    return;

  if(chips_are_connected(from,to)==true)
    return;

  bool from_is_mono = from->_num_outputs==1;
  bool to_is_mono   = to->_num_inputs==1;

  if(from_is_mono==true){
    for(int to_portnum=0 ; to_portnum<to->_num_inputs ; to_portnum++){
      if(connect(scene, from, 0, to, to_portnum)==false)
        return; // trying to make recursive connection
    }
  }else if(to_is_mono==true){
    for(int from_portnum=0 ; from_portnum<to->_num_outputs ; from_portnum++){
      if(connect(scene, from, from_portnum, to, 0)==false)
        return; // trying to make recursive connection
    }
  }else{
    for(int portnum=0 ; portnum<std::min(from->_num_outputs,to->_num_inputs) ; portnum++){
      if(connect(scene, from, portnum, to, portnum)==false)
        return; // trying to make recursive connection
    }
  }

  Connection *connection = new Connection(scene);
  connection->from = from;
  connection->to = to;
  
  from->_connections.push_back(connection);
  to->_connections.push_back(connection);
  
  connection->update_position();
  scene->addItem(connection);
}

void CHIP_connect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to){
  CHIP_connect_chips(scene, find_chip_for_plugin(scene, from), find_chip_for_plugin(scene, to));
}

void CHIP_econnect_chips(QGraphicsScene *scene, Chip *from, Chip *to){
  if(chips_are_econnected(from,to)==true){
    printf("Chips are already econnected\n");
    return;
  }

  if(econnect(scene, from, to)==false)
    return; // trying to make recursive connection

  Connection *econnection = new Connection(scene, true);
  econnection->from = from;
  econnection->to = to;
  
  from->_econnections.push_back(econnection);
  to->_econnections.push_back(econnection);
  
  econnection->update_position();
  scene->addItem(econnection);
}

void CHIP_econnect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to){
  CHIP_econnect_chips(scene, find_chip_for_plugin(scene, from), find_chip_for_plugin(scene, to));
}

void CONNECTION_delete_a_connection_where_all_links_have_been_removed(Connection *connection){
  Chip *from = connection->from;
  Chip *to = connection->to;

  // connections
  if (connection->_is_event_connection == false) {
    {
      // Why does std::vector do this so incredibly complicated?
      std::vector<Connection*>::iterator newEnd = std::remove(from->_connections.begin(), from->_connections.end(), connection);
      from->_connections.erase(newEnd, from->_connections.end());
    }
    {
      std::vector<Connection*>::iterator newEnd = std::remove(to->_connections.begin(), to->_connections.end(), connection);
      to->_connections.erase(newEnd, to->_connections.end());
    }

  } else {
    {
      // Why does std::vector do this so incredibly complicated?
      std::vector<Connection*>::iterator newEnd = std::remove(from->_econnections.begin(), from->_econnections.end(), connection);
      from->_econnections.erase(newEnd, from->_econnections.end());
    }
    {
      std::vector<Connection*>::iterator newEnd = std::remove(to->_econnections.begin(), to->_econnections.end(), connection);
      to->_econnections.erase(newEnd, to->_econnections.end());
    }
  }

  delete connection;
}

void CONNECTION_delete_connection(Connection *connection){
  Chip *from = connection->from;
  Chip *to = connection->to;

  if (connection->_is_event_connection) {
    
    PATCH_remove_event_receiver(SP_get_plugin(from->_sound_producer)->patch,
                                SP_get_plugin(to->_sound_producer)->patch);

  } else {

    bool from_is_mono = from->_num_outputs==1;
    bool to_is_mono   = to->_num_inputs==1;

    if(from_is_mono==true){
      for(int to_portnum=0 ; to_portnum<to->_num_inputs ; to_portnum++){
        SP_remove_link(to->_sound_producer, to_portnum,
                       from->_sound_producer, 0);
      }
    }else if(to_is_mono==true){
      for(int from_portnum=0 ; from_portnum<to->_num_outputs ; from_portnum++){
        SP_remove_link(to->_sound_producer, 0,
                       from->_sound_producer, from_portnum);
      }
    }else{
      for(int portnum=0 ; portnum<std::min(from->_num_outputs,to->_num_inputs) ; portnum++){
        SP_remove_link(to->_sound_producer, portnum,
                       from->_sound_producer, portnum);
      }
    }
  }

  CONNECTION_delete_a_connection_where_all_links_have_been_removed(connection);
}


void Connection::update_position(void){
  if(_is_event_connection) {

    int x1 = CHIP_get_eport_x(from);
    int x2 = CHIP_get_eport_x(to);
    int y1 = CHIP_get_output_eport_y(from);
    int y2 = CHIP_get_input_eport_y(to);
  
    this->setLine(x1,y1,x2,y2);

  } else {

    int x1 = to->pos().x()+port_width/2;
    int x2 = from->pos().x()+grid_width-port_width/2;
    int y1 = CHIP_get_port_y(to);
    int y2 = CHIP_get_port_y(from);
  
    this->setLine(x1,y1,x2,y2);

  }
}

// 'right_chip' is inserted in the middle of 'left_chip' and all chips 'left_chip' sends to.
void CHIP_connect_left(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip){
  std::vector<Connection*> connections_to_delete;
  std::vector<Chip*> chips_to_connect_to;

  for(unsigned int i=0;i<left_chip->_connections.size();i++){
    Connection *connection = left_chip->_connections.at(i);
    if(connection->from==left_chip){
      connections_to_delete.push_back(connection);
      chips_to_connect_to.push_back(connection->to);
    }
  }

  for(unsigned int i=0;i<connections_to_delete.size();i++)
    CONNECTION_delete_connection(connections_to_delete.at(i));

  for(unsigned int i=0;i<chips_to_connect_to.size();i++)
    CHIP_connect_chips(scene,right_chip,chips_to_connect_to.at(i));

  CHIP_connect_chips(scene,left_chip,right_chip);
}

// 'left_chip' is inserted in the middle of 'right_chip' and all chips 'right_chip' receives from.
void CHIP_connect_right(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip){
  std::vector<Connection*> connections_to_delete;
  std::vector<Chip*> chips_to_connect_to;

  for(unsigned int i=0;i<right_chip->_connections.size();i++){
    Connection *connection = right_chip->_connections.at(i);
    if(connection->to==right_chip){
      connections_to_delete.push_back(connection);
      chips_to_connect_to.push_back(connection->from);
    }
  }

  for(unsigned int i=0;i<connections_to_delete.size();i++)
    CONNECTION_delete_connection(connections_to_delete.at(i));

  for(unsigned int i=0;i<chips_to_connect_to.size();i++)
    CHIP_connect_chips(scene,chips_to_connect_to.at(i),left_chip);

  CHIP_connect_chips(scene,left_chip,right_chip);
}


Chip::Chip(QGraphicsScene *scene, SoundProducer *sound_producer, float x, float y)
  : _scene(scene)
  , _sound_producer(sound_producer)
  , _num_inputs(SP_get_plugin(sound_producer)->type->num_inputs)
  , _num_outputs(SP_get_plugin(sound_producer)->type->num_outputs)
  , _color("white")
  , _slider_being_edited(0)
 {
   printf("New Chip. Inputs: %d, Ouptuts: %d\n",_num_inputs,_num_outputs);

   {
     int x1,x2,y1,y2;
     get_slider1_coordinates(x1,x2,y1,y2);
     _input_slider = SLIDERPAINTER_create(this,x1,x2,y1,y2);
     SLIDERPAINTER_set_alternative_color(_input_slider);
     SLIDERPAINTER_set_num_channels(_input_slider, _num_inputs);
   }

   {
     int x1,x2,y1,y2;
     get_slider2_coordinates(x1,x2,y1,y2);
     _output_slider = SLIDERPAINTER_create(this,x1,x2,y1,y2);
     SLIDERPAINTER_set_num_channels(_output_slider, _num_outputs);
   }

   setPos(QPointF(x,y));
   scene->addItem(this);

 #if 0
   _line_item = new QGraphicsLineItem(0,0,50,50);
   _line_item->setPen(QPen(Qt::black, 2));
   _line_item->setPos(QPointF(x+50,y+50));
   scene->addItem(_line_item);
 #endif

   setFlags(ItemIsSelectable | ItemIsMovable | ItemSendsGeometryChanges);
   setAcceptsHoverEvents(true);

   if(_num_outputs>0)
     SP_get_plugin(sound_producer)->volume_peak_values_for_chip = SLIDERPAINTER_obtain_peak_value_pointers(_output_slider,_num_outputs);
   else if(_num_inputs>0)
     SP_get_plugin(sound_producer)->input_volume_peak_values_for_chip = SLIDERPAINTER_obtain_peak_value_pointers(_input_slider,_num_inputs);

   CHIP_update(SP_get_plugin(sound_producer));
 }

Chip::~Chip(){
  while(_connections.size()>0){
    fprintf(stderr,"Deleting connection. Connections left: %d\n",(int)_connections.size());
    CONNECTION_delete_connection(_connections.at(0));
  }
  while(_econnections.size()>0){
    fprintf(stderr,"Deleting econnection. EConnections left: %d\n",(int)_econnections.size());
    CONNECTION_delete_connection(_econnections.at(0));
  }
  SP_get_plugin(_sound_producer)->input_volume_peak_values_for_chip = NULL;
  SP_get_plugin(_sound_producer)->volume_peak_values_for_chip = NULL;
  
  SLIDERPAINTER_delete(_input_slider);
  SLIDERPAINTER_delete(_output_slider);
}

QRectF Chip::boundingRect() const
{
  return QRectF(0, 0, chip_width, get_chip_visible_height());
}

QPainterPath Chip::shape() const
{
  QPainterPath path;
  path.addRect(0, 0, chip_width, get_chip_visible_height());
  return path;
}

inline static void CHECKBOX_paint_arc(QPainter *painter, bool is_checked, bool is_enabled, int width, int height){
    QColor col1; // off
    QColor col2; // on

    QColor c(30,20,20,40);

    if(is_enabled==true){
      col1 = c.light(90);
      col2 = c.light(82);
    }else{
      col1 = c.light(116);
      col2 = c.light(110);
    }

    //col2 = QColor(106, 104, 100, 255);
    //col2 = QColor(0, 107, 156, 255);

    if(is_checked==true){
      painter->setPen(col2);
      painter->setBrush(QBrush(col2,Qt::SolidPattern));

      painter->drawEllipse(1,1,width-2,height-2);

      //painter->setBrush(QBrush());
    }else{
      painter->setPen(col2);
      //painter->setBrush(QBrush(col2));

      painter->drawEllipse(1,1,width-2,height-2);

      //painter->setBrush(QBrush());

    }
}

void Chip::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
  Q_UNUSED(widget);

  SoundPlugin *plugin = SP_get_plugin(_sound_producer);

  bool is_selected = (option->state & QStyle::State_Selected);
  /*
    bool is_over = (option->state & QStyle::State_MouseOver);
  */

  bool is_current_patch = get_current_instruments_gui_patch()==plugin->patch;

  QColor c(g_editor->colors[1]);
  if(is_current_patch==false);
    c.setAlpha(160);

  painter->setPen(QPen(c, 2));

  painter->setFont(g_editor->main_window->font());

  // main box
  {
    int x1,y1,x2,y2;
    get_coordinates(x1,y1,x2,y2);

    if(is_current_patch==true){
      QColor c(g_editor->colors[2]);
      c.setAlpha(30);
      painter->setBrush(QBrush(c,Qt::SolidPattern));
    }

    painter->drawRect(x1,y1,x2-x1,y2-y1);
  }

  int button_x2;

  {
    int x1,y1,x2,y2;
    get_slider2_coordinates(x1,y1,x2,y2);      

    button_x2 = x2;

    // input slider
    if(plugin->type->num_outputs==0 && plugin->type->num_inputs>0){
      painter->translate(x1,y1);
      SLIDERPAINTER_paint(_input_slider, painter);
      painter->translate(-x1,-y1);
    }


    // output slider (not painted if input slider is painted)
    if(plugin->type->num_outputs>0){
      painter->translate(x1,y1);
      SLIDERPAINTER_paint(_output_slider, painter);
      painter->translate(-x1,-y1);
    }
  }

#if 0
  if(0){
  // makesound button
  {
    int x1,y1,x2,y2;
    get_makesound_button_coordinates(x1,y1,x2,y2);
    painter->translate(x1,y1);
    CHECKBOX_paint_arc(painter, plugin->volume_is_on, plugin->type->num_outputs>0, x2-x1, y2-y1);
    painter->translate(-x1,-y1);
  }

  // wet button
  {
    int x1,y1,x2,y2;
    get_dontbypass_button_coordinates(x1,y1,x2,y2);
    painter->translate(x1,y1);
    CHECKBOX_paint_arc(painter, !plugin->effects_are_on, plugin->type->num_inputs>0 && plugin->type->num_outputs>0 , x2-x1, y2-y1);
    painter->translate(-x1,-y1);
  }
  }
#endif


  // Draw text
  {
    int x1,y1,x2,y2;
    get_name_coordinates(x1,y1,x2,y2);

    if(is_selected){
      QColor c(30,25,70,60);
      //painter->fillRect(x1-1,y1+1,x2-x1+2,y2-y1-2, g_editor->colors[9].light(86));
      painter->fillRect(x1-1,y1+1,button_x2-x1+2,y2-y1-2, c);
    }else{
      QColor c(30,65,70,35);
      painter->fillRect(x1-1,y1+1,button_x2-x1+2,y2-y1-2, c);
    }


    QString text = SP_get_plugin(_sound_producer)->patch->name;
    float textlen = get_text_width(painter->font(),text);       
    float width = x2-x1;

    {
      QColor c(g_editor->colors[1]);
      if(is_current_patch==false)
        c.setAlpha(160);
      
      painter->setPen(QPen(c, 2));
    }

    if(textlen>=width){
      float s = width/textlen;
      painter->save();
      painter->scale(s,1.0);
      painter->drawText(x1/s, y1+2, width/s, y2-y1, Qt::AlignLeft, text);
      painter->restore();
    }else{
      painter->drawText(x1, y1+2, width, y2-y1, Qt::AlignLeft, text);
    }

    // checbuttons.
    {
      int x1,y1,x2,y2;
      get_volume_onoff_coordinates(x1,y1,x2,y2);
      paint_checkbutton(painter, x1,y1,x2,y2, plugin->volume_is_on);

      get_effects_onoff_coordinates(x1,y1,x2,y2);
      paint_checkbutton(painter, x1,y1,x2,y2, plugin->effects_are_on);
    }

#if 0
    {
      painter->setPen(QPen(Qt::black, 2));
     
      int x1,y1,x2,y2;
      get_coordinates(x1,y1,x2,y2);
 
      painter->drawRect(x2-button_width, 0, button_width, button_width-2);
      painter->drawRect(x2-button_width, y2-button_width+1, button_width, button_width-2);

      QColor c(10,12,30,65);
      painter->setPen(QPen(c, 2));

      painter->fillRect(x2-button_width, y2-button_width, button_width, button_width, c);
#if 0
      painter->drawLine(x2-button_width, y2-button_width, x2, y2);
      painter->drawLine(x2, y2-button_width, x2-button_width, y2);
#endif
    }
#endif
  }

  //printf("Paint Chip. Inputs: %d, Ouptuts: %d\n",_num_inputs,_num_outputs);

  // Ports
  {
    int x1,y1,x2,y2;
    get_coordinates(x1,y1,x2,y2);

    //painter->setPen(QPen(Qt::gray, 2));
    //QColor color(59,155,68,40);
    QColor color(80,90,80,20);
    painter->setPen(color);
    painter->setBrush(QBrush(color,Qt::SolidPattern));

    const int xborder = 5;
    const int yborder = 5;

    if(_num_inputs>0)
      painter->drawRoundedRect(xborder, y1+yborder,
                               port_width-2-xborder, y2-y1-yborder*2,
                               10,10);

    if(_num_outputs>0)
      painter->drawRoundedRect(x2+2, y1+yborder,
                               port_width-2-xborder, y2-y1-yborder*2,
                               10,10);

    painter->setBrush(QBrush());
  }
}

void Chip::mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event ){
  printf("I'm double clicked!\n");
  if(SP_get_plugin(_sound_producer)->type->show_gui != NULL)
    SP_get_plugin(_sound_producer)->type->show_gui(SP_get_plugin(_sound_producer));

  event->accept();
  //QGraphicsItem::mouseDoubleClickEvent(event);
}

void Chip::mousePressEvent(QGraphicsSceneMouseEvent *event)
{

  if(event->button()==Qt::LeftButton){

    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int num_effects = plugin->type->num_effects;

    QPointF pos = event->pos();

    printf("Pressed. %f / %f\n",pos.x(),pos.y());
    struct Instruments *instrument = get_audio_instrument();
    instrument->PP_Update(instrument,plugin->patch);

    if(event->modifiers() & Qt::ControlModifier)
      setSelected(true);
    else
      MW_set_selected_chip(this); //i.e. only set this one as the selected.

    // volume onoff
    {
      int x1,y1,x2,y2;
      get_volume_onoff_coordinates(x1,y1,x2,y2);
      if(pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2){
        Undo_AudioEffect_CurrPos(plugin->patch, num_effects+EFFNUM_VOLUME_ONOFF);

        printf("Setting volume_is_on. Before: %d. After: %d\n",plugin->volume_is_on, !plugin->volume_is_on);
        float new_value = plugin->volume_is_on?0.0f:1.0f;

        PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_VOLUME_ONOFF, new_value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);
        CHIP_update(plugin);
        GFX_update_instrument_widget(plugin->patch);

        event->accept();
        return;
      }
    }

    // effects onoff
    {
      int x1,y1,x2,y2;
      get_effects_onoff_coordinates(x1,y1,x2,y2);
      if(pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2){
        Undo_AudioEffect_CurrPos(plugin->patch, num_effects+EFFNUM_EFFECTS_ONOFF);

        float new_value = plugin->effects_are_on?0.0f:1.0f;

        PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, new_value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);
        CHIP_update(plugin);
        GFX_update_instrument_widget(plugin->patch);

        event->accept();
        return;
      }
    }

    for(int i=0;i<2;i++){
      int x1,y1,x2,y2;
      if(i==0)
        get_slider1_coordinates(x1,y1,x2,y2);      
      else
        get_slider2_coordinates(x1,y1,x2,y2);      

      //printf("Got it %d/%d - %d/%d. %d/%d/%d/%d\n",x1,y1,x2,y2,pos.x()>x1, pos.x()<x2, pos.y()>y1, pos.y()<y2);

      if(pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2){
        int effect_num;

        if(i==0 && plugin->type->num_inputs==0)
          continue;
        if(i==1 && plugin->type->num_outputs==0)
          continue;

        if(i==0)
          effect_num = num_effects+EFFNUM_INPUT_VOLUME;
        else
          effect_num = num_effects+EFFNUM_VOLUME;

        Undo_AudioEffect_CurrPos(plugin->patch, effect_num);

        float value = ::scale(pos.x(),x1,x2,0,1.0);
        PLUGIN_set_effect_value(plugin, -1, effect_num, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);

        CHIP_update(plugin);

        GFX_update_instrument_widget(plugin->patch);

        _slider_being_edited = i+1;
      }
    }
  }

  event->accept();
   
  //QGraphicsItem::mousePressEvent(event);
}

QVariant Chip::itemChange(GraphicsItemChange change, const QVariant &value) {
  if (change == ItemPositionHasChanged && this->scene()) {
    for(unsigned int i=0;i<_connections.size();i++)
      _connections[i]->update_position();
    for(unsigned int i=0;i<_econnections.size();i++)
      _econnections[i]->update_position();
    //printf("item pos changed\n");
  }

  return QGraphicsItem::itemChange(change, value);
}

void Chip::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
  if(_slider_being_edited>0){
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int num_effects = plugin->type->num_effects;

    QPointF pos = event->pos();

    int x1,y1,x2,y2;
    if(_slider_being_edited==0)
      get_slider1_coordinates(x1,y1,x2,y2);      
    else
      get_slider2_coordinates(x1,y1,x2,y2);      

    int effect_num;

    if(_slider_being_edited==1)
      effect_num = num_effects+EFFNUM_INPUT_VOLUME;
    else
      effect_num = num_effects+EFFNUM_VOLUME;

    float value = ::scale(pos.x(),x1,x2,0,1.0);
    if(value>1.0)
      value=1.0;
    if(value<0.0)
      value=0.0;

    PLUGIN_set_effect_value(plugin, -1, effect_num, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);

    CHIP_update(plugin);

    GFX_update_instrument_widget(plugin->patch);
  }

  event->accept();
  return;

  //QGraphicsItem::mouseMoveEvent(event);
}

void Chip::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
  if(_slider_being_edited>0)
    _slider_being_edited=0;

  event->accept();
}

static struct Patch *get_patch_from_chip(Chip *chip){
  SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  return plugin->patch;
}

static Chip *get_chip_from_patch_id(QGraphicsScene *scene, int patch_id){
  struct Patch *patch = PATCH_get_from_id(patch_id);
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;

  QList<QGraphicsItem *> das_items = scene->items();
  for (int i = 0; i < das_items.size(); ++i){
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return chip;
    }
  }
  
  return NULL;
}

hash_t *CHIP_get_state(Chip *chip){
  hash_t *state=HASH_create(4);

  SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  struct Patch *patch = get_patch_from_chip(chip);

  HASH_put_int(state, "patch", patch->id);
  HASH_put_float(state, "x", chip->x());
  HASH_put_float(state, "y", chip->y());

  HASH_put_hash(state, "plugin", PLUGIN_get_state(plugin));

  return state;
}

hash_t *CHIP_get_chip_state_from_patch(struct Patch *patch){
  Chip *chip = get_chip_from_patch_id(&g_mixer_widget->scene, patch->id);
  return CHIP_get_state(chip);
}

void CHIP_create_from_state(hash_t *state){
  struct Patch *patch = PATCH_get_from_id(HASH_get_int(state, "patch"));
  double x = HASH_get_float(state, "x");
  double y = HASH_get_float(state, "y");

  struct SoundPlugin *plugin = PLUGIN_create_from_state(HASH_get_hash(state, "plugin"));
  patch->patchdata = plugin;

  if(plugin!=NULL){
    plugin->patch = patch;
    Chip *chip = new Chip(&g_mixer_widget->scene,SP_create(plugin),x,y);
    printf("Made chip %p\n",chip);
  }else{
    printf("Unable to create chip\n"); // proper error message given elsewhere. (ladspa or vst)
  }
}

hash_t *CONNECTION_get_state(Connection *connection){
  hash_t *state=HASH_create(4);

  //struct Patch *patch = get_patch_from_chip(chip);

  HASH_put_int(state, "from_patch", get_patch_from_chip(connection->from)->id);
  HASH_put_int(state, "to_patch", get_patch_from_chip(connection->to)->id);
  HASH_put_int(state, "is_event_connection", connection->_is_event_connection ? 1 : 0);

  return state;
}

void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state){
  Chip *from_chip = get_chip_from_patch_id(scene, HASH_get_int(state, "from_patch"));
  Chip *to_chip   = get_chip_from_patch_id(scene, HASH_get_int(state, "to_patch"));

  if(from_chip==NULL || to_chip==NULL) {
    RError("Could not find chip from patch id. %d: 0x%p, %d: 0x%p",HASH_get_int(state, "from_patch"),from_chip,HASH_get_int(state, "to_patch"),to_chip);
    return;
  }

  if(HASH_has_key(state, "is_event_connection") && HASH_get_int(state, "is_event_connection")==1) // .rad files before 1.9.31 did not have even connections.
    CHIP_econnect_chips(scene, from_chip, to_chip);
  else
    CHIP_connect_chips(scene, from_chip, to_chip);
}
