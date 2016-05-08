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
#include "../Qt/Qt_SliderPainter_proc.h"
#include "../Qt/Qt_colors_proc.h"



class Chip;

class SuperConnection  : public QGraphicsLineItem {

public:

  bool is_event_connection;
  enum ColorNums color_num;
  
  QColor getColor(void) {
    return get_qcolor(color_num);
  }
  
  QPen getPen(){
    QPen pen(Qt::gray, 50);
    pen.setWidthF(1.2);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    //pen.setColor(QColor(30,25,70,6));
    QColor c = getColor();
    c.setAlpha(140);
    pen.setColor(c);
    return pen;
  }

  SuperConnection(QGraphicsScene *parent, bool is_event_connection, enum ColorNums color_num)
    : QGraphicsLineItem()
    , is_event_connection(is_event_connection)
    , color_num(color_num)
    , from(NULL)
    , to(NULL)
    , is_selected(false)
    , visible_line(this)
    , arrow_line1(this)
  {
    QPen pen(Qt::gray, 50);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setColor(QColor(30,25,70,6));
    pen.setWidth(3);
    
    setPen(pen);
    //_line_item->setPen(QPen(Qt::black, 2));
    //_line_item->setPos(QPointF(x+50,y+50));

    setZValue(-2);    
    
    setAcceptHoverEvents(true);

    visible_line.setPen(getPen());
    parent->addItem(&visible_line);

    /*
    if(_is_event_connection){
      arrow_line1.setPen(getPen());
      parent->addItem(&arrow_line1);
    }
    */
  }

  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
             QWidget *widget)
  {
    update_colors();
    QGraphicsLineItem::paint(painter,option,widget);    
  }

  Chip *from;
  Chip *to;
  bool is_selected;

  QGraphicsLineItem visible_line;
  QGraphicsLineItem arrow_line1;

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
  
  virtual void update_position(void) {
    R_ASSERT(false);
  }

#if 0
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget){
    bool is_over = (option->state & QStyle::State_MouseOver);
    printf("is over: %d\n",is_over);
    QGraphicsLineItem::paint(painter,option,widget);
  }
#endif

  void hoverEnterEvent ( QGraphicsSceneHoverEvent * event ){
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
  }

  void hoverLeaveEvent ( QGraphicsSceneHoverEvent * event ){
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
      pen.setColor(QColor(30,25,70,0));

      setPen(pen);
    }
  }
};

class AudioConnection : public SuperConnection {
  
public:
  
 AudioConnection(QGraphicsScene *parent)
   : SuperConnection(parent, false, MIXER_AUDIO_CONNECTION_COLOR_NUM)
  {}

  virtual void update_position(void);
};

class EventConnection : public SuperConnection {
  
public:

  EventConnection(QGraphicsScene *parent)
    : SuperConnection(parent, true, MIXER_EVENT_CONNECTION_COLOR_NUM)
  {}
  
  virtual void update_position(void);
};

class Chip : public QGraphicsItem
{
public:

  void init_new_plugin(void);
  
  Chip(QGraphicsScene *scene, SoundProducer *sound_producer, float x, float y);
  ~Chip();

  QRectF boundingRect() const;
  QPainterPath shape() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *item, QWidget *widget);

  bool positionedAtSlider(QPointF pos);
    
protected:
    void mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event );
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);

  QVariant itemChange(GraphicsItemChange change, const QVariant &value);

#if 0
  void hoverMoveEvent ( QGraphicsSceneHoverEvent * event ){
    printf("got hovermove event\n");
  }
#endif

public:
  QGraphicsScene *_scene;
  SoundProducer *_sound_producer;
  int _num_inputs;
  int _num_outputs;
  QColor _color;
  radium::Vector<AudioConnection*> audio_connections;   // TODO: use radium::Vector instead. TODO2: Use different types for econnections and connections, it's FAR to easy to mix them up.
  radium::Vector<EventConnection*> event_connections;  // TODO: use radium::Vector instead

  SliderPainter *_input_slider;
  SliderPainter *_output_slider;

  float _slider_start_value;
  float _slider_start_pos;
  int _slider_being_edited;

  float _moving_x_offset;
  float _moving_y_offset;
};

extern void CHIP_get_name_coordinates(int &x1, int &y1, int &x2, int &y2);

extern void CHIP_kick_left(Chip *chip);
extern void CHIP_kick_right(Chip *chip);

extern Chip *find_chip_for_plugin(QGraphicsScene *scene, SoundPlugin *plugin);
extern void CHIP_connect_chips(QGraphicsScene *scene, Chip *from, Chip *to);
extern void CHIP_connect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to);

extern void CONNECTION_delete_an_audio_connection_where_all_links_have_been_removed(AudioConnection *connection);
extern void CONNECTION_delete_an_event_connection_where_all_links_have_been_removed(EventConnection *connection);
extern void CONNECTION_delete_audio_connection(AudioConnection *connection);
extern void CONNECTION_delete_event_connection(EventConnection *connection);
extern void CONNECTION_delete_connection(SuperConnection *connection);
  
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

Chip *CHIP_get(QGraphicsScene *scene, const Patch *patch);

struct Patch *CHIP_get_patch(Chip *chip);

hash_t *CONNECTION_get_state(SuperConnection *connection);
void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state, int patch_id_old, int patch_id_new);

#endif // __cplusplus

struct SoundProducer;
struct Patch;

extern LANGSPEC void CHIP_create(struct SoundProducer *sound_producer, bool is_loading_song);
extern LANGSPEC void CHIP_delete(struct Patch *patch);
  
//extern LANGSPEC void CHIP_init_because_it_has_new_plugin(struct SoundPlugin *plugin);

extern LANGSPEC void CHIP_create_from_state(hash_t *state, Buses buses);

extern LANGSPEC void CHIP_update(struct SoundPlugin *plugin);

extern LANGSPEC float CHIP_get_pos_x(const struct Patch *patch);
extern LANGSPEC float CHIP_get_pos_y(const struct Patch *patch);
extern LANGSPEC void CHIP_set_pos(struct Patch *patch,float x, float y);

extern LANGSPEC int CHIP_get_num_in_connections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_connections(const struct Patch *patch);
  
extern LANGSPEC int CHIP_get_num_in_econnections(const struct Patch *patch);
extern LANGSPEC int CHIP_get_num_out_econnections(const struct Patch *patch);

extern LANGSPEC struct Patch* CHIP_get_source(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_dest(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_esource(const struct Patch *patch, int connectionnum);
extern LANGSPEC struct Patch* CHIP_get_edest(const struct Patch *patch, int connectionnum);


#endif
