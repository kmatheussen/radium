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
#include <QPainter>

#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../Qt/Qt_SliderPainter_proc.h"
#include "../Qt/Qt_colors_proc.h"
#include "../Qt/Qt_mix_colors.h"

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

  void hoverEnterEvent ( QGraphicsSceneHoverEvent * event ) override;
  void hoverMoveEvent ( QGraphicsSceneHoverEvent * event ) override;
  void hoverLeaveEvent ( QGraphicsSceneHoverEvent * event ) override;
  
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
  
  radium::Vector<AudioConnection*> _input_audio_connections;
  radium::Vector<AudioConnection*> _output_audio_connections;
  
  radium::Vector<EventConnection*> _input_event_connections;
  radium::Vector<EventConnection*> _output_event_connections;

  SliderPainter *_input_slider;
  SliderPainter *_output_slider;

  float _last_updated_volume = -1;
  bool _last_updated_mute = false;
  //bool _last_updated_implicitly_mute = false;
  bool _last_updated_solo = false;
  bool _last_updated_bypass = false;
  bool _last_updated_recording = false;
  bool _last_updated_autosuspending = false;
  int64_t _autosuspend_on_time = 0;
  int _last_note_intencity = -1;
  
  float _slider_start_value;
  float _slider_start_pos;
  int _slider_being_edited;
  bool _has_made_volume_effect_undo = false;
  
  QPointF _moving_start_pos;
  float _moving_x_offset;
  float _moving_y_offset;

  bool has_output_slider(void) const {
    if (_sound_producer==NULL)
      return false;
    const SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    if (plugin==NULL)
      return false;
    return plugin->type->num_outputs > 0;
  }

  bool has_input_slider(void) const {
    if (_sound_producer==NULL)
      return false;
    const SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    if (plugin==NULL)
      return false;
    return !has_output_slider() && plugin->type->num_inputs>0;
  }

  int get_volume_effect_num(void) const {
    const SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int num_effects = plugin->type->num_effects;
    if(has_input_slider())
      return num_effects+EFFNUM_INPUT_VOLUME;
    else
      return num_effects+EFFNUM_VOLUME;
  }
  
  float get_slider_volume(void) const {
    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    int effect_num = get_volume_effect_num();
    return PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
  }
  
  void mySetSelected(bool selected);

};

static inline struct SoundProducer *CHIP_get_soundproducer(const Chip *chip){
  return chip->_sound_producer;
}

struct Patch *CHIP_get_patch(const Chip *chip); // (fast)


extern LANGSPEC bool MW_get_connections_visibility(void);
extern LANGSPEC bool MW_get_bus_connections_visibility(void);

typedef Chip* ChipPointer;

class SuperConnection  : public QGraphicsLineItem {

public:

  bool _is_selected; // for some reason isSelected() doesn't work.
  bool _is_event_connection;
  enum ColorNums _color_num;

  float _last_displayed_db_peak = 0.0f;
  
  Chip *_from; // Note: If we ever need to set this variable after constructor (we currently don't), we also need to ensure that the new from-chip doesn't have more than one PLUGIN connection.
  Chip *_to;

  bool _is_enabled;
  bool _is_implicitly_enabled; // Set to false if solo has caused this connection to be implicitly disabled.

private:

  bool _is_hovered_visible = false;
  
public:

  bool set_enabled(bool is_enabled, const char **error){
    //printf("set enabled1 %d -> %d: is_enabled: %d. _is_enabled: %d, is_implicitly_enabled: %d\n", (int)CHIP_get_patch(_from)->id, (int)CHIP_get_patch(_to)->id, is_enabled, _is_enabled, _is_implicitly_enabled);
    
    if (_is_enabled==is_enabled)
      return false;

    _is_enabled = is_enabled;
    
    if (_is_implicitly_enabled)
      SP_set_link_enabled(to->_sound_producer, from->_sound_producer, is_enabled, error);

    //printf("set enabled2 %d -> %d: is_enabled: %d. _is_enabled: %d, is_implicitly_enabled: %d\n", (int)CHIP_get_patch(_from)->id, (int)CHIP_get_patch(_to)->id, is_enabled, _is_enabled, _is_implicitly_enabled);

    update_shape(true, true);
    
    return true;
  }

  bool get_enabled(void) const {
    return _is_enabled;
  }

  bool set_implicitly_enabled(bool is_implicitly_enabled, const char **error){
    if (_is_implicitly_enabled==is_implicitly_enabled)
      return false;

    _is_implicitly_enabled = is_implicitly_enabled;
    
    if (_is_enabled)
      SP_set_link_enabled(to->_sound_producer, from->_sound_producer, is_implicitly_enabled, error);

    return true;
  }
    
  bool get_implicitly_enabled(void) const {
    return _is_implicitly_enabled;
  }

  void set_enabled_only_dont_update_link(bool is_enabled, bool is_implicitly_enabled){
    //printf(".............Setting %s -> %s to %d/%d (%d / %d)\n", CHIP_get_patch(_from)->name, CHIP_get_patch(_to)->name, is_enabled, is_implicitly_enabled, _is_enabled, _is_implicitly_enabled);
           
    if(_is_enabled != is_enabled){
      _is_enabled = is_enabled;
      update();
    }
    if (_is_implicitly_enabled != is_implicitly_enabled){
      _is_implicitly_enabled = is_implicitly_enabled;
      update();
    }
  }    
  
  const ChipPointer &from; // Set to const. See comment about _from above.
  const ChipPointer &to;

  void update_shape(bool update_line, bool update_arrow);

  bool _must_update_line_shape = false;
  bool _must_update_arrow_shape = false;

  void schedule_update_shape(bool update_line, bool update_arrow){
    _must_update_line_shape  = _must_update_line_shape || update_line;
    _must_update_arrow_shape = _must_update_arrow_shape || update_arrow;
  }
  
  void apply_update_shapes(void) {
    if (_must_update_line_shape || _must_update_arrow_shape)
      update_shape(_must_update_line_shape, _must_update_arrow_shape);
    
    _must_update_line_shape = false;
    _must_update_arrow_shape = false;
  }
  
  //bool is_ab_touched = false; // used by a/b to determine wheter it should be deleted or not after changing ab.

  QColor getColor(void) const {
    QColor col = get_qcolor(_color_num); // !_is_implicitly_enabled ? Qt::green :

    if (_from==NULL)
      return col;
    
    int intencity = ATOMIC_GET_RELAXED(CHIP_get_patch(_from)->visual_note_intencity);

    if (!_is_event_connection) {

      struct SoundPlugin *plugin = SP_get_plugin(_from->_sound_producer);
      
      if (plugin->type->num_outputs > 0){
        
        float db = _last_displayed_db_peak;
        
        if(db>=4.0f)
          
          col = mix_colors(get_qcolor(PEAKS_4DB_COLOR_NUM), QColor("410000"), ::scale(db, 4, MAX_DB, 1, 0));
        
        if(db>=0.0f)
          
          col = mix_colors(get_qcolor(PEAKS_0DB_COLOR_NUM), get_qcolor(PEAKS_4DB_COLOR_NUM), ::scale(db, 0, 4, 1, 0));
        
        else if(db>=-30.0f)
          
          col = mix_colors(get_qcolor(PEAKS_COLOR_NUM), get_qcolor(PEAKS_0DB_COLOR_NUM), ::scale(db, -30, 0, 1, 0));
        
        else
          
          col = get_qcolor(MIXER_AUDIO_CONNECTION_COLOR_NUM); //PEAKS_COLOR_NUM); //.darker(150);
        
      }
      
    }

    if(_is_event_connection && intencity>0)
      return mix_colors(Qt::white, col,
                        intencity < (MAX_NOTE_INTENCITY * 4.0 / 5.0)
                        ? 0.0
                        : ::scale(intencity,  // divide by 10 to make it 10 times faster.
                                  MAX_NOTE_INTENCITY, MAX_NOTE_INTENCITY * 4.0 / 5.0,
                                  1.0, 0.0));

    return col;
    /*
    if (is_selected)
      return col.lighter(198);
    else
      return col;
    */
  }
  
  QPen getPen() const {
    QColor c = getColor();

    if (!_is_enabled || !_is_implicitly_enabled)
      c.setAlpha(30);
    else if(_is_selected)
      c.setAlpha(250);
    else
      c.setAlpha(140);

    float pen_width = 1.0; //intencity < (MAX_NOTE_INTENCITY * 4.0 / 5.0)
    //      ? 1.0
    //                             : ::scale(intencity,  // divide by 10 to make it 10 times faster.
      //                                      MAX_NOTE_INTENCITY, MAX_NOTE_INTENCITY * 4.0 / 5.0,
    //                                    1.0, 2.0);

    float db = _last_displayed_db_peak;
    /*    
    if(db>=4.0f)
      
      pen_width *= ::scale(db, 4, MAX_DB, 8, 20);
    
    if(db>=0.0f)
      
      pen_width *= ::scale(db, 0, 4, 4.5, 8);
    
    else if(db>=-35.0f)

      pen_width *= ::scale(db, -35, 0, 0.3, 4.5);
    
    else

      pen_width *= 0.3;
    */

    if (db >= -35)
      pen_width *= ::scale(db, -35, MAX_DB, 0.3, 10);
    else
      pen_width *= 0.3;
    
    QPen pen(c, pen_width * (_is_selected ? 1.8 : 1.2));
    
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    //pen.setColor(QColor(30,25,70,6));

    //const char *error = NULL;
    //bool is_enabled = from==NULL||to==NULL ? true : SP_get_link_enabled(CHIP_get_soundproducer(to), CHIP_get_soundproducer(from), &error);
    //R_ASSERT_NON_RELEASE(error==NULL); // link doesn't exist during startup.
    
    return pen;
  }

  SuperConnection(QGraphicsScene *parent, Chip *from, Chip *to, bool is_event_connection, enum ColorNums color_num, bool is_enabled, bool is_implicitly_enabled)
    : QGraphicsLineItem()
    , _is_selected(false)
    , _is_event_connection(is_event_connection)
    , _color_num(color_num)
    , _from(from)
    , _to(to)
    , _is_enabled(is_enabled)
    , _is_implicitly_enabled(is_implicitly_enabled)
    , from(_from)
    , to(_to)
    , _visible_line(this)
    , _arrow(this)
  {

    QColor c(30,25,70,0);
    c.setAlpha(100);

    QPen pen(c, 26); // width of the line when hovered.
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);

    setPen(pen);

    setZValue(-2);
    _arrow.setZValue(1);
        
    setAcceptHoverEvents(true);

    {
      QPen pen = getPen();
      
      _visible_line.setPen(pen);
      parent->addItem(&_visible_line);
      
      //_arrow.setPen(pen);
      parent->addItem(&_arrow);
    }
    
    update_visibility();
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

  void update_visibility(void){
    if (to==NULL)
      setVisibility(true);
    else if (!_is_event_connection && SP_get_bus_num(to->_sound_producer) >= 0)
      setVisibility(MW_get_bus_connections_visibility());
    else
      setVisibility(MW_get_connections_visibility());
  }
  
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
             QWidget *widget)
    override
  {
    if (_is_hovered_visible)
      QGraphicsLineItem::paint(painter,option,widget);
  }

  QGraphicsLineItem _visible_line;
  QGraphicsPolygonItem _arrow;

  
  virtual void update_position(void) = 0;

#if 0
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget){
    bool is_over = (option->state & QStyle::State_MouseOver);
    printf("is over: %d\n",is_over);
    QGraphicsLineItem::paint(painter,option,widget);
  }
#endif

  void setVisibility(bool show){
    _visible_line.setVisible(show);
    _arrow.setVisible(show);
    setVisible(show);
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

#if 0
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
#endif
    //setVisible(true);
    _is_hovered_visible = true;
    update();
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
#if 0
    // Same size, but make invisible. (same size ensures hovering still works)
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
#endif
    //setVisible(false);
    _is_hovered_visible = false;
    update();
  }

  void mySetSelected(bool selected){
    if (_is_selected != selected){
      _is_selected = selected;
      update_shape(true, true);
      //setLine(line().x1(), line().y1(), line().x2(), line().y2());
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
  
  AudioConnection(QGraphicsScene *parent, Chip *from, Chip *to, ConnectionType connection_type, bool is_enabled, bool is_implicitly_enabled)
    : SuperConnection(parent, from, to, false, MIXER_AUDIO_CONNECTION_COLOR_NUM, is_enabled, is_implicitly_enabled)
    , _connection_type(ConnectionType::NOT_SET)
  {
    if (from!=NULL && to!=NULL){
      //R_ASSERT_RETURN_IF_FALSE(to!=NULL);

      // add connection to from
      {
        bool already_gotit = false;
        
        for(auto *connection : from->_output_audio_connections){
          R_ASSERT(connection->from == from);
          
          if(connection->to==to){
            R_ASSERT(false);
            already_gotit = true;
          }
        }
        
        if (already_gotit==false)
          from->_output_audio_connections.push_back(this);
      }

      // add connection to to
      {
        bool already_gotit = false;
        
        for(auto *connection : to->_input_audio_connections){
          R_ASSERT(connection->to == to);
          
          if(connection->from==from){
            R_ASSERT(false);
            already_gotit = true;
          }
        }
        
        if (already_gotit==false)          
          to->_input_audio_connections.push_back(this);
      }
    }

    if(from != NULL)
      set_connection_type(connection_type);
    else
      _connection_type = connection_type;

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
    : SuperConnection(parent, from, to, true, MIXER_EVENT_CONNECTION_COLOR_NUM, true, true)
  {

    if (from!=NULL && to!=NULL){
      //R_ASSERT_RETURN_IF_FALSE(to!=NULL);
      
      // add connection to from
      {
        bool already_gotit = false;
        
        for(auto *connection : from->_output_event_connections){
          R_ASSERT(connection->from == from);
          
          if(connection->to==to){
            R_ASSERT(false);
            already_gotit = true;
          }
        }
        
        if (already_gotit==false)
          from->_output_event_connections.push_back(this);
      }

      // add connection to to
      {
        bool already_gotit = false;
        
        for(auto *connection : to->_input_event_connections){
          R_ASSERT(connection->to == to);
          
          if(connection->from==from){
            R_ASSERT(false);
            already_gotit = true;
          }
        }
        
        if (already_gotit==false)
          to->_input_event_connections.push_back(this);
      }
      
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
extern bool CONNECTION_are_connected_somehow(const Chip *source, const Chip *target); // Returns true if source sends sound to target, either directly or via middle-chips.
extern bool CONNECTION_can_connect(const Chip *source, const Chip *target); // Returns true if 'source' can send audio to 'target' without creating a recursive connection.
extern bool CONNECTION_can_connect(struct Patch *source, struct Patch *target);

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
Chip *CHIP_get(const QGraphicsScene *scene, const Patch *patch); // scene can be NULL

void CHIP_update(Chip *chip, SoundPlugin *plugin);

AudioConnection *CONNECTION_find_audio_connection(const Chip *from, const Chip *to);
AudioConnection *CONNECTION_find_audio_connection(const struct Patch *from, const struct Patch *to);
hash_t *CONNECTION_get_state(const SuperConnection *connection, const vector_t *patches);

void CONNECTIONS_remove_all(QGraphicsScene *scene);

void CHIP_remove_chip_from_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after);
void CHIP_add_chip_to_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after);

//void CONNECTION_create_from_state2(QGraphicsScene *scene, hash_t *state, int64_t patch_id_old, int64_t patch_id_new, int64_t patch_id_old2, int64_t patch_id_new2, bool all_patches_are_always_supposed_to_be_here);
//void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state, int64_t patch_id_old, int64_t patch_id_new);

void CONNECTIONS_create_from_state(QGraphicsScene *scene, const hash_t *connections,
                                   instrument_t patch_id_old, instrument_t patch_id_new,
                                   instrument_t patch_id_old2, instrument_t patch_id_new2
                                   );

void CONNECTIONS_replace_all_with_state(QGraphicsScene *scene, const hash_t *connections, bool all_patches_are_always_supposed_to_be_here);

void CONNECTIONS_create_from_presets_state(QGraphicsScene *scene, const hash_t *connections,
                                           const vector_t *patches
                                           );

#endif // __cplusplus

struct SoundProducer;
struct Patch;

extern LANGSPEC void CHIP_autopos(struct Patch *patch);

#ifdef __cplusplus
extern Chip* CHIP_create(struct SoundProducer *sound_producer, float x, float y);
extern void CHIP_delete(struct Patch *patch);
extern void CHIP_create_bus_connections(Chip *chip, Buses &dasbuses); // Used when loading older songs.
/*
extern bool CHIP_is_implicitly_muted(Chip *chip);
extern bool CHIP_is_implicitly_muted(Patch *patch);
*/
#endif

//extern LANGSPEC void CHIP_init_because_it_has_new_plugin(struct SoundPlugin *plugin);

extern LANGSPEC void CHIP_create_from_state(hash_t *state, Buses buses);

extern LANGSPEC void CHIP_update(struct SoundPlugin *plugin);

extern LANGSPEC float CHIP_get_pos_x(const struct Patch *patch);
extern LANGSPEC float CHIP_get_pos_y(const struct Patch *patch);
extern LANGSPEC void CHIP_set_pos(const struct Patch *patch,float x, float y);

extern LANGSPEC bool CONNECTIONS_apply_changes(const dynvec_t changes);

extern LANGSPEC int CHIP_get_num_in_connections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_connections(const struct Patch *patch);
  
extern LANGSPEC int CHIP_get_num_in_econnections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_econnections(const struct Patch *patch);

extern LANGSPEC struct Patch* CHIP_get_source(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_dest(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_esource(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_edest(const struct Patch *patch, int connectionnum);


#endif
