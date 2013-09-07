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

#ifdef __cplusplus

#include <stdio.h>
#include <vector>

#include <QColor>
#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QStyleOptionGraphicsItem>
#include <QTimer>

#include "../audio/SoundProducer_proc.h"
#include "../Qt/Qt_SliderPainter_proc.h"



class Chip;

class Connection : public QGraphicsLineItem{

public:

  bool _is_event_connection;

  QPen getPen(){
    QPen pen(Qt::gray, 50);
    pen.setWidth(3);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    //pen.setColor(QColor(30,25,70,6));
    if(_is_event_connection)
      pen.setColor(QColor(30,95,70,140));
    else
      pen.setColor(QColor(50,25,70,140));
    return pen;
  }

 Connection(QGraphicsScene *parent, bool is_event_connection = false)
    : QGraphicsLineItem()
    , _is_event_connection(is_event_connection)
    , from(NULL)
    , to(NULL)
    , is_selected(false)
    , visible_line(this)
  {
    QPen pen(Qt::gray, 50);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setColor(QColor(30,25,70,6));

    setPen(pen);
    //_line_item->setPen(QPen(Qt::black, 2));
    //_line_item->setPos(QPointF(x+50,y+50));

    setZValue(-1);

    setAcceptHoverEvents(true);

    visible_line.setPen(getPen());
    parent->addItem(&visible_line);
  }

  Chip *from;
  Chip *to;
  bool is_selected;

  QGraphicsLineItem visible_line;

  void setLine ( qreal x1, qreal y1, qreal x2, qreal y2 ){
    visible_line.setLine(x1,y1,x2,y2);
    QGraphicsLineItem::setLine(x1-14,y1,x2+14,y2);
  }

  void update_position(void);

#if 0
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget){
    bool is_over = (option->state & QStyle::State_MouseOver);
    printf("is over: %d\n",is_over);
    QGraphicsLineItem::paint(painter,option,widget);
  }
#endif

  void hoverEnterEvent ( QGraphicsSceneHoverEvent * event ){
    //printf("hover enter\n");

    QPen pen(Qt::gray,6);
    pen.setJoinStyle(Qt::RoundJoin);
    pen.setCapStyle(Qt::RoundCap);
    pen.setColor(QColor(120,35,50,40));

    visible_line.setPen(pen);

    {
      QPen pen(Qt::gray, 50);
      pen.setJoinStyle(Qt::RoundJoin);
      pen.setCapStyle(Qt::RoundCap);
      pen.setColor(QColor(120,35,50,18));
      //pen.setColor(QColor(30,25,70,18));

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

    visible_line.setPen(getPen());

    {
      QPen pen(Qt::gray, 50);
      pen.setJoinStyle(Qt::RoundJoin);
      pen.setCapStyle(Qt::RoundCap);
      pen.setColor(QColor(30,25,70,0));

      setPen(pen);
    }
  }
};


class Chip : public QGraphicsItem
{
public:
  Chip(QGraphicsScene *scene, SoundProducer *sound_producer, float x, float y);
  ~Chip();

  QRectF boundingRect() const;
  QPainterPath shape() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *item, QWidget *widget);

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
  std::vector<Connection*> _connections;
  std::vector<Connection*> _econnections;

  SliderPainter *_input_slider;
  SliderPainter *_output_slider;

  int _slider_being_edited;

  float _moving_x_offset;
  float _moving_y_offset;
};

extern void CHIP_kick_left(Chip *chip);
extern void CHIP_kick_right(Chip *chip);

extern Chip *find_chip_for_plugin(QGraphicsScene *scene, SoundPlugin *plugin);
extern void CHIP_connect_chips(QGraphicsScene *scene, Chip *from, Chip *to);
extern void CHIP_connect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to);
extern void CONNECTION_delete_a_connection_where_all_links_have_been_removed(Connection *connection);
extern void CONNECTION_delete_connection(Connection *connection);

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

hash_t *CHIP_get_state(Chip *chip);

hash_t *CONNECTION_get_state(Connection *connection);
void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state);

#endif // __cplusplus

extern LANGSPEC void CHIP_create_from_state(hash_t *state);

extern LANGSPEC void CHIP_update(SoundPlugin *plugin);

extern LANGSPEC float CHIP_get_pos_x(struct Patch *patch);
extern LANGSPEC float CHIP_get_pos_y(struct Patch *patch);
extern LANGSPEC void CHIP_set_pos(struct Patch *patch,float x, float y);

extern LANGSPEC void CHIP_delete_from_patch(struct Patch *patch);
extern LANGSPEC hash_t *CHIP_get_chip_state_from_patch(struct Patch *patch);

#endif
