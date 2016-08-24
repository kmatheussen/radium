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

#ifndef QM_MIXERWIDGET_H
#define QM_MIXERWIDGET_H

#include "../audio/SoundPlugin.h"
#include "../common/disk_load_proc.h"

#ifdef __cplusplus

#include <QGraphicsSceneMouseEvent>
#include <QUrl>
#include <QMimeData>

//#include <QtGui/qwidget.h>
#include <QGraphicsScene>
//#include <QtGui/QGraphicsItem>

#include "QM_chip.h"



static const int chip_width = 120;
static const int chip_height = 32;
static const int grid_width = chip_width;
static const int grid_border = 5;
static const int grid_height = chip_height;
static const int port_width = chip_width/8;
static const int port_height = 2;//chip_height/8;

class MyScene : public QGraphicsScene{
  Q_OBJECT
 public:
  MyScene(QWidget *parent);

 protected:
  void 	mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event );
  void 	mouseMoveEvent ( QGraphicsSceneMouseEvent * event );
  void 	mousePressEvent ( QGraphicsSceneMouseEvent * event );
  void 	mouseReleaseEvent ( QGraphicsSceneMouseEvent * event );


  /*
  void dragEnterEvent(QGraphicsSceneDragDropEvent *e){
    printf("               GOT DRAG\n");
    //e->acceptProposedAction();
  }
  */
  
  void dragMoveEvent(QGraphicsSceneDragDropEvent *e){
    printf("               GOT MOVE\n");
    e->acceptProposedAction();
  }
  
  void dropEvent(QGraphicsSceneDragDropEvent *event){
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

#if 0
  public slots:
    void on_scene_changed ( const QList<QRectF> & region ){
    printf("Hepp! changed\n");
  }
#endif
};


class MyQGraphicsView;

class MixerWidget : public QWidget
{
    Q_OBJECT
public:
    MixerWidget(QWidget *parent = 0);

    void setupMatrix();
    void populateScene();
    
    MyScene scene;
    MyQGraphicsView *view;
};

extern MixerWidget *g_mixer_widget;

void MW_set_autopos(double *x, double *y);
  
bool MW_move_chip_to_slot(Chip *chip, float x, float y);
  
Chip *MW_get_chip_at(float x, float y, Chip *except);

void MW_set_selected_chip(Chip *chip);

#endif // __cplusplus

extern LANGSPEC void inc_plugin_usage_number(SoundPluginType *type);

extern LANGSPEC char *MW_request_load_preset_instrument_description(void);
extern LANGSPEC char *MW_popup_plugin_selector2(void);
extern LANGSPEC SoundPluginType *MW_popup_plugin_type_selector(void);

extern LANGSPEC void MW_autoconnect_plugin(SoundPlugin *plugin);

extern LANGSPEC void MW_connect(struct Patch *source, struct Patch *dest);
extern LANGSPEC void MW_econnect(struct Patch *source, struct Patch *dest);

extern LANGSPEC void MW_update_all_chips(void);

extern LANGSPEC void MW_cleanup(bool is_loading);

extern LANGSPEC hash_t *MW_get_connections_state(vector_t *patches);
extern LANGSPEC hash_t *MW_get_state(vector_t *patches);
extern LANGSPEC void MW_create_connections_from_state(hash_t *connections);
extern LANGSPEC void MW_create_connections_from_state_and_replace_patch(hash_t *connections, int patch_id_old, int patch_id_new);
extern LANGSPEC void MW_create_from_state(hash_t *state, vector_t *patches, float x, float y);
extern LANGSPEC void MW_create_full_from_state(hash_t *state, bool is_loading);

extern LANGSPEC void MW_create_plain(void);

extern LANGSPEC void MW_set_chip_position(struct Patch *patch, float x, float y);
extern LANGSPEC float MW_get_chip_x(const struct Patch *patch);
extern LANGSPEC float MW_get_chip_y(struct Patch *patch);

extern LANGSPEC SoundPlugin *get_main_pipe(void);

#endif // QM_MIXERWIDGET_H
