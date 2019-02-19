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

#ifndef CHIP_H
#define CHIP_H

struct SoundPlugin;

#ifdef __cplusplus

#include <stdio.h>
#include <vector>

#include <QColor>
#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QStyleOptionGraphicsItem>

#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../Qt/Qt_SliderPainter_proc.h"
#include "../Qt/Qt_colors_proc.h"

#include "../api/api_proc.h"

class AudioConnection;
class EventConnection;

class Chip : public QGraphicsItem, public QObject
{
  
public:

  void init_new_plugin(void);
  
  Chip(QGraphicsScene *scene, SoundProducer *sound_producer, float x, float y);
  ~Chip();

  QRectF boundingRect() const override;
  QPainterPath shape() const override;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *item, QWidget *widget) override;

  bool positionedAtSlider(QPointF pos);

  bool myMouseDoubleClickEvent (float x, float y);

  QString _name_text;
  
protected:
    void mousePressEvent(QGraphicsSceneMouseEvent *event) override;
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event) override;
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override;

    QVariant itemChange(GraphicsItemChange change, const QVariant &value) override;

#if 0
  void hoverMoveEvent ( QGraphicsSceneHoverEvent * event ) override{
    printf("got hovermove event\n");
  }
#endif

public:
  QGraphicsScene *_scene;
  SoundProducer *_sound_producer;
  int _num_inputs;
  int _num_outputs;
  QColor _color;
  radium::Vector<AudioConnection*> audio_connections;
  radium::Vector<EventConnection*> event_connections;

  SliderPainter *_input_slider;
  SliderPainter *_output_slider;

  float _last_updated_volume = -1;
  bool _last_updated_mute = false;
  bool _last_updated_implicitly_mute = false;
  bool _last_updated_solo = false;
  bool _last_updated_bypass = false;
  bool _last_updated_recording = false;
  bool _last_updated_autosuspending = false;
  int64_t _autosuspend_on_time = 0;
  
  float _slider_start_value;
  float _slider_start_pos;
  int _slider_being_edited;
  bool _has_made_volume_effect_undo = false;
  
  QPointF _moving_start_pos;
  float _moving_x_offset;
  float _moving_y_offset;

  bool has_output_slider(void){
    if (_sound_producer==NULL)
      return false;
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    if (plugin==NULL)
      return false;
    return plugin->type->num_outputs > 0;
  }

  bool has_input_slider(void){
    if (_sound_producer==NULL)
      return false;
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    if (plugin==NULL)
      return false;
    return !has_output_slider() && plugin->type->num_inputs>0;
  }

  int get_volume_effect_num(void){
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int num_effects = plugin->type->num_effects;
    if(has_input_slider())
      return num_effects+EFFNUM_INPUT_VOLUME;
    else
      return num_effects+EFFNUM_VOLUME;
  }
  
  float get_slider_volume(void){
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int effect_num = get_volume_effect_num();
    return PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
  }
  
  void mySetSelected(bool selected);

};

static inline struct SoundProducer *CHIP_get_soundproducer(const Chip *chip){
  return chip->_sound_producer;
}


extern LANGSPEC bool MW_get_connections_visibility(void);

typedef Chip* ChipPointer;

class SuperConnection  : public QGraphicsLineItem {

public:

  bool is_selected; // for some reason isSelected() doesn't work.
  bool is_event_connection;
  enum ColorNums color_num;

  
private:
  Chip *_from; // Note: If we ever need to set this variable after constructor (we currently don't), we also need to ensure that the new from chip doesn't have more than one PLUGIN connection.
  Chip *_to;
  

public:
  
  const ChipPointer &from; // Set to const. See comment about _from above.
  const ChipPointer &to;

  //bool is_ab_touched = false; // used by a/b to determine wheter it should be deleted or not after changing ab.
  
  QColor getColor(void) {
    if (is_selected)
      return get_qcolor(color_num).lighter(198);
    else
      return get_qcolor(color_num);
  }
  
  QPen getPen(){
    QPen pen(Qt::gray, 50);
    if (is_selected)
      pen.setWidthF(2.2);
    else
      pen.setWidthF(1.2);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    //pen.setColor(QColor(30,25,70,6));

    const char *error = NULL;
    bool is_enabled = from==NULL||to==NULL ? true : SP_get_link_enabled(CHIP_get_soundproducer(to), CHIP_get_soundproducer(from), &error);
    //R_ASSERT_NON_RELEASE(error==NULL); // link doesn't exist during startup.
    
    QColor c = getColor();
    if (!is_enabled)
      c.setAlpha(40);
    else if(is_selected)
      c.setAlpha(250);
    else
      c.setAlpha(140);
    pen.setColor(c);
    return pen;
  }

  SuperConnection(QGraphicsScene *parent, Chip *from, Chip *to, bool is_event_connection, enum ColorNums color_num)
    : QGraphicsLineItem()
    , is_selected(false)
    , is_event_connection(is_event_connection)
    , color_num(color_num)
    , _from(from)
    , _to(to)
    , from(_from)
    , to(_to)
    , visible_line(this)
      //, arrow_line1(this)
  {
    QPen pen(Qt::red, 50);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setWidth(3);

    QColor c(30,25,70,0);
    c.setAlpha(0);
    pen.setColor(c);

    setPen(pen);
    //_line_item->setPen(QPen(Qt::black, 2));
    //_line_item->setPos(QPointF(x+50,y+50));

    setZValue(-2);    
    
    setAcceptHoverEvents(true);

    visible_line.setPen(getPen());
    parent->addItem(&visible_line);

    setVisibility(MW_get_connections_visibility());    
  }

  /*
  void setTo(Chip *new_to){
    _to = new_to;
  }
  
  void setFrom(Chip *new_from){
    _from = new_from;
  }
  */
  
  ~SuperConnection(){
    printf("       Remake: ~SuperConnectin\n");
    //remakeMixerStrips(-1);
  }
      
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
             QWidget *widget)
    override
  {
    update_colors();
    QGraphicsLineItem::paint(painter,option,widget);    
  }

  QGraphicsLineItem visible_line;
  //QGraphicsLineItem arrow_line1;

  void setLine ( qreal x1, qreal y1, qreal x2, qreal y2 ){
    visible_line.setLine(x1,y1,x2,y2);

    /*
    if(_is_event_connection){
      qreal x = x2;
      qreal y = y2;
      qreal xb,yb;
      if(x2>x1)
        xb=x-10;
      else
        xb=x+10;
      if(y2>y1)
        yb=y-10;
      else
        yb=y+10;
      arrow_line1.setLine(x,y,xb,yb);
    }
    QGraphicsLineItem::setLine(x1-14,y1,x2+14,y2);
    */

    QGraphicsLineItem::setLine(x1,y1,x2,y2);
  }

  void update_colors(void){
    visible_line.setPen(getPen());
  }
  
  virtual void update_position(void) = 0;

#if 0
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget){
    bool is_over = (option->state & QStyle::State_MouseOver);
    printf("is over: %d\n",is_over);
    QGraphicsLineItem::paint(painter,option,widget);
  }
#endif

  void setVisibility(bool show){
    visible_line.setVisible(show);
  }
  
  void hoverEnterEvent ( QGraphicsSceneHoverEvent * event ) override {
    //printf("hover enter\n");

    /*
    QPen pen(Qt::gray,6);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setColor(QColor(120,35,50,40));

    visible_line.setPen(pen);
    */
    
    {
      QPen pen(Qt::gray, 50);
      pen.setJoinStyle(Qt::RoundJoin);
      pen.setCapStyle(Qt::RoundCap);
      //pen.setMiterLimit(0);

      QColor c = getColor();
      c.setAlpha(100);
      pen.setColor(c);
      
      //pen.setColor(QColor(120,35,50,18));
      //pen.setColor(QColor(30,25,70,18));
      pen.setWidthF(26);
      
      setPen(pen);
    }

    setVisible(true);
  }

  void hoverLeaveEvent ( QGraphicsSceneHoverEvent * event ) override {
    //printf("hover leave\n");

    /*
    QPen pen(Qt::gray,6);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setColor(QColor(30,25,70,40));
    */

    //visible_line.setPen(getPen());

    {
      QPen pen(Qt::gray, 50);
      pen.setJoinStyle(Qt::RoundJoin);
      pen.setCapStyle(Qt::RoundCap);

      //pen.setColor(QColor(30,25,70,0));

      QColor c(30,25,70,0);
      c.setAlpha(0);
      pen.setColor(c);

      setPen(pen);
    }

    //setVisible(false);
  }

  void mySetSelected(bool selected){
    if (is_selected != selected){
      update_colors();
      is_selected = selected;
    }
    QGraphicsLineItem::setSelected(selected);    
  }

};

enum class ConnectionType{
  IS_SEND,
  IS_PLUGIN,
  NOT_SET  
};

/* If changing this order, also change order in instruments.scm */

static inline ConnectionType get_connection_type_from_int(int c){
  switch(c){
  case 0:
    return ConnectionType::IS_SEND;
  case 1:
    return ConnectionType::IS_PLUGIN;
  case 2:
    return ConnectionType::NOT_SET;
  default:
    R_ASSERT(false);
    return ConnectionType::NOT_SET;
  }
}

static inline int get_int_from_connection_type(ConnectionType connection_type){
  switch(connection_type){
  case ConnectionType::IS_SEND: return 0;
  case ConnectionType::IS_PLUGIN: return 1;
  case ConnectionType::NOT_SET: return 2;
  default:
    R_ASSERT(false);
    return get_int_from_connection_type(ConnectionType::NOT_SET);
  }
}

class AudioConnection : public SuperConnection {
  
  ConnectionType _connection_type;

public:
  
  AudioConnection(QGraphicsScene *parent, Chip *from, Chip *to, ConnectionType connection_type)
    : SuperConnection(parent, from, to, false, MIXER_AUDIO_CONNECTION_COLOR_NUM)
    , _connection_type(ConnectionType::NOT_SET)
  {
    if (from!=NULL){
      R_ASSERT_RETURN_IF_FALSE(to!=NULL);

      {
        bool gotit = false;
        
        for(auto *connection : from->audio_connections){
          if(connection->from==from && connection->to==to){
            R_ASSERT(false);
            gotit = true;
          }
        }
        
        if (gotit==false)
          from->audio_connections.push_back(this);
      }

      
      {
        bool gotit = false;
        
        for(auto *connection : to->audio_connections){
          if(connection->from==from && connection->to==to){
            R_ASSERT(false);
            gotit = true;
          }
        }
        
        if (gotit==false)
          to->audio_connections.push_back(this);
      }
    }

    if(from != NULL)
      set_connection_type(connection_type);
    else
      _connection_type = connection_type;

    if (from != NULL && to != NULL)
      update_position();
  }

  void update_position(void) override;


  ConnectionType get_connection_type(void) const {
    return _connection_type;
  }
  
  void set_connection_type(ConnectionType connection_type); // this function ensures there is only one plugin connection from 'from'.

  void set_connection_type(int connection_type){
    set_connection_type(get_connection_type_from_int(connection_type));
  }
};

class EventConnection : public SuperConnection {
  
public:

  EventConnection(QGraphicsScene *parent, Chip *from, Chip *to)
    : SuperConnection(parent, from, to, true, MIXER_EVENT_CONNECTION_COLOR_NUM)
  {
    
    if (from!=NULL){
      R_ASSERT_RETURN_IF_FALSE(to!=NULL);
      
      from->event_connections.push_back(this);
      to->event_connections.push_back(this);

      update_position();
    }
    
  }


  
  void update_position(void) override;
};



extern void CHIP_get_name_coordinates(int &x1, int &y1, int &x2, int &y2);
extern void CHIP_get_note_indicator_coordinates(int &x1, int &y1, int &x2, int &y2);

extern void CHIP_kick_left(Chip *chip);
extern void CHIP_kick_right(Chip *chip);

extern Chip *find_chip_for_plugin(QGraphicsScene *scene, SoundPlugin *plugin);
extern void CHIP_connect_chips(QGraphicsScene *scene, Chip *from, Chip *to, ConnectionType connection_type);
extern void CHIP_connect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to, ConnectionType connection_type);
extern bool CHIP_disconnect_chips(QGraphicsScene *scene, Chip *from, Chip *to);
extern bool CHIPS_are_connected(const Chip *from, const Chip *to);
extern bool CHIPS_are_econnected(const Chip *from, const Chip *to);

extern void CONNECTION_delete_an_audio_connection_where_all_links_have_been_removed(AudioConnection *connection);
extern void CONNECTION_delete_an_event_connection_where_all_links_have_been_removed(EventConnection *connection);
extern void CONNECTION_delete_audio_connection(AudioConnection *connection);
extern void CONNECTION_delete_event_connection(EventConnection *connection);
extern void CONNECTION_delete_connection(SuperConnection *connection);
extern void CONNECTION_update(struct Patch *source, struct Patch *target);
  
extern void CHIP_econnect_chips(QGraphicsScene *scene, Chip *from, Chip *to);
extern void CHIP_econnect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to);

extern void CHIP_connect_left(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip);
extern void CHIP_connect_right(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip);

int CHIP_get_input_port_x(Chip *chip);
int CHIP_get_output_port_x(Chip *chip);
int CHIP_get_port_y(Chip *chip);

bool CHIP_is_at_input_port(Chip *chip, int x, int y);
bool CHIP_is_at_output_port(Chip *chip, int x, int y);

int CHIP_get_input_eport_y(Chip *chip);
int CHIP_get_output_eport_y(Chip *chip);
int CHIP_get_eport_x(Chip *chip);
bool CHIP_is_at_input_eport(Chip *chip, int x, int y);
bool CHIP_is_at_output_eport(Chip *chip, int x, int y);

void CHIP_autopos(Chip *chip);
Chip *CHIP_get(const QGraphicsScene *scene, const Patch *patch);

void CHIP_update(Chip *chip, SoundPlugin *plugin);

struct Patch *CHIP_get_patch(const Chip *chip);

AudioConnection *CONNECTION_find_audio_connection(const Chip *from, const Chip *to);
AudioConnection *CONNECTION_find_audio_connection(const struct Patch *from, const struct Patch *to);
hash_t *CONNECTION_get_state(const SuperConnection *connection, const vector_t *patches);

void CONNECTIONS_remove_all(QGraphicsScene *scene);

void CHIP_remove_chip_from_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after);
void CHIP_add_chip_to_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after);

//void CONNECTION_create_from_state2(QGraphicsScene *scene, hash_t *state, int64_t patch_id_old, int64_t patch_id_new, int64_t patch_id_old2, int64_t patch_id_new2, bool all_patches_are_always_supposed_to_be_here);
//void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state, int64_t patch_id_old, int64_t patch_id_new);

void CONNECTIONS_create_from_state(QGraphicsScene *scene, const hash_t *connections,
                                   int patch_id_old, int patch_id_new,
                                   int64_t patch_id_old2, int64_t patch_id_new2
                                   );

void CONNECTIONS_replace_all_with_state(QGraphicsScene *scene, const hash_t *connections, bool all_patches_are_always_supposed_to_be_here);

void CONNECTIONS_create_from_presets_state(QGraphicsScene *scene, const hash_t *connections,
                                           const vector_t *patches
                                           );

#endif // __cplusplus

struct SoundProducer;
struct Patch;

extern LANGSPEC void CHIP_autopos(struct Patch *patch);

extern LANGSPEC void CHIP_create(struct SoundProducer *sound_producer, float x, float y);
extern LANGSPEC void CHIP_delete(struct Patch *patch);
  
//extern LANGSPEC void CHIP_init_because_it_has_new_plugin(struct SoundPlugin *plugin);

extern LANGSPEC void CHIP_create_from_state(hash_t *state, Buses buses);

extern LANGSPEC void CHIP_update(struct SoundPlugin *plugin);

extern LANGSPEC float CHIP_get_pos_x(const struct Patch *patch);
extern LANGSPEC float CHIP_get_pos_y(const struct Patch *patch);
extern LANGSPEC void CHIP_set_pos(const struct Patch *patch,float x, float y);

extern LANGSPEC bool CONNECTIONS_apply_changes(const dyn_t changes);

extern LANGSPEC int CHIP_get_num_in_connections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_connections(const struct Patch *patch);
  
extern LANGSPEC int CHIP_get_num_in_econnections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_econnections(const struct Patch *patch);

extern LANGSPEC struct Patch* CHIP_get_source(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_dest(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_esource(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_edest(const struct Patch *patch, int connectionnum);


#endif
