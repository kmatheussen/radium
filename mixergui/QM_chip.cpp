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

#include <inttypes.h>
#include <algorithm>
#include <set>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/instruments_proc.h"
#include "../common/OS_Player_proc.h"

#include "undo_mixer_proc.h"

#include "QM_MixerWidget.h"
#include "QM_chip.h"
#include "undo_chip_position_proc.h"
#include "undo_mixer_connections_proc.h"

#include "../audio/audio_instrument_proc.h"
#include "../audio/undo_audio_connection_gain_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/AudioMeterPeaks_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../audio/CpuUsage.hpp"
#include "../audio/Modulator_plugin_proc.h"

#include "../Qt/EditorWidget.h"
#include "../Qt/Qt_instruments_proc.h"
//#include "../Qt/Qt_MyQCheckBox.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"

#include "../api/api_common_proc.h"
#include "../api/api_gui_proc.h"
#include "../api/api_instruments_proc.h"

#include "../common/patch_proc.h"


extern EditorWidget *g_editor;

//
// eport/econnection means "event port"/"event connection".
//



/*
static int get_text_width(const QFont &font, const QString &text){
  const QFontMetrics fn = QFontMetrics(font);
  return fn.boundingRect(text).width();
}
*/

#if 0
static const int left_border   = 2;
static const int right_border  = 2;
static const int top_border    = 2;
//static const int bottom_border = 2;
#endif

//static const int chip_width = 120;
//static const int slider_height = 15;

static const int name_height = 14;
static const int button_width = 15;//name_height/2;
//static const int button_height = 13;//name_height/2;



// coordinates

static int chip_box_x1 = port_width;
static int chip_box_x2 = chip_width - port_width;
static int chip_box_y1 = port_height;
static int chip_box_y2 = chip_height-port_height;

static void get_coordinates(int &x1, int &y1, int &x2, int &y2){
  x1=chip_box_x1;
  y1=chip_box_y1;
  x2=chip_box_x2;
  y2=chip_box_y2;
}

static void get_name_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);
  //get_slider2_coordinates(x1,y1,x2,y2);

  x1 = x1 + name_height;
  y2 = y1 + name_height;

  //  y1+=slider_height;
  //  y2+=name_height;
}

static void get_note_indicator_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_name_coordinates(x1,y1,x2,y2);

  x2 = x1 - 3; 
  x1 = chip_box_x1 + 3;
  y1 += 3;
  y2 -= 3;
}

/*
static int get_note_indicator_x2(void){
  int x1,x2,y1,y2;
  get_note_indicator_coordinates(x1,y1,x2,y2);
  return x2;
}
*/

void CHIP_get_name_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_name_coordinates(x1,y1,x2,y2);
}

void CHIP_get_note_indicator_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_note_indicator_coordinates(x1,y1,x2,y2);
}

static int get_volume_onoff_x1(int x2){
  return x2 - button_width*3;
}

static void get_volume_onoff_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);

  x1 = get_volume_onoff_x1(x2);
  x2 = x2 - button_width*2;

  y1 = y1 + name_height;
  y2 = y1 + button_width - 1;
}

static void get_solo_onoff_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);

  x1 = x2 - button_width*2;
  x2 = x2 - button_width;

  y1 = y1 + name_height;
  y2 = y1 + button_width - 1;
}

static void get_effects_onoff_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_coordinates(x1,y1,x2,y2);

  x1 = x2 - button_width;

  y1 = y1 + name_height;
  y2 = y1 + button_width - 1;
}


static void get_slider1_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_name_coordinates(x1,y1,x2,y2);

  x1 = chip_box_x1;// + left_border;
  x2 = get_volume_onoff_x1(x2); //chip_box_x2;// - right_border;
  y1 = y2;
  y2 = chip_box_y2; // + top_border;
}


static void get_slider2_coordinates(int &x1, int &y1, int &x2, int &y2){
  get_slider1_coordinates(x1,y1,x2,y2);
  //y1+=slider_height;
  //y2+=slider_height;
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
  int x1,y1,x2,y2;
  get_note_indicator_coordinates(x1,y1,x2,y2);
  return chip->x() + (x1+x2)/2;
  //return chip->x() + chip_width/2;
}

static int get_eport_x1(Chip *chip){
  return chip->x();
  //return CHIP_get_eport_x(chip)-port_width/2;
}

static int get_eport_x2(Chip *chip){ 
  int x1,y1,x2,y2;
  get_effects_onoff_coordinates(x1,y1,x2,y2);
  return chip->x() + x1;
  //return CHIP_get_eport_x(chip) + port_width/2;
}

int CHIP_get_output_eport_y(Chip *chip){
  int x1,y1,x2,y2;
  get_note_indicator_coordinates(x1,y1,x2,y2);
  return chip->y() + y2;
}

int CHIP_get_input_eport_y(Chip *chip){
  int x1,y1,x2,y2;
  get_note_indicator_coordinates(x1,y1,x2,y2);
  return chip->y()+y1;
}

static int get_output_eport_y1(Chip *chip){
  int x1,y1,x2,y2;
  get_name_coordinates(x1,y1,x2,y2);

  return chip->y() + y1;
}

static int get_output_eport_y2(Chip *chip){
  int x1,y1,x2,y2;
  get_name_coordinates(x1,y1,x2,y2);
  return chip->y() + y2; //grid_height;
  //return CHIP_get_output_eport_y(chip)+port_height/2;
}

static int get_input_eport_y1(Chip *chip){
  //return get_output_eport_y1(chip);
  return chip->y();
}

static int get_input_eport_y2(Chip *chip){
  //return get_output_eport_y2(chip);
  return chip->y() + chip_box_y1;
}

bool CHIP_is_at_input_eport(Chip *chip, int x, int y){
  return (x >= get_eport_x1(chip))
    && (x < get_eport_x2(chip))
    && (y >= get_input_eport_y1(chip))
    && (y < get_input_eport_y2(chip));
}

bool CHIP_is_at_output_eport(Chip *chip, int x, int y){
  /*
  printf("x/y: %d/%d. x1/y1: %d/%d. x2/y2: %d/%d\n",x,y,
         get_eport_x1(chip),get_output_eport_y1(chip),
         get_eport_x2(chip),get_output_eport_y2(chip));
  */
  return (x >= get_eport_x1(chip))
    && (x < get_eport_x2(chip))
    && (y >= get_output_eport_y1(chip))
    && (y < get_output_eport_y2(chip));
}



// stuff 


static void paint_checkbutton(QPainter *painter, const QString &name, QColor patch_color, const QColor &text_color, const QColor &background_color, int x1, int y1, int x2, int y2, bool is_on, bool is_implicitly_on, bool is_hovered){

  {
    QColor c = get_qcolor(MIXER_BORDER_COLOR_NUM);
    painter->setPen(QPen(c, 0.5));
  
    //painter->drawRect(x1, y1, x2-x1, y2-y1);
    if (name=="M"){
      QLineF line(x1-0.25, y1+0.5, x1-0.25, y2-0.25);
      painter->drawLine(line);
    }
    
    if (name!="B"){
      QLineF line(x2-0.25, y1+0.5, x2-0.25, y2-0.25);
      painter->drawLine(line);
    }
  }

  if (is_hovered)
    patch_color = patch_color.lighter(150);

  QRectF rect(x1+0.25, y1+0.75, x2-x1-1, y2-y1-1.25);
  
  if(is_on || is_implicitly_on){

    /*
    painter->drawLine(x1, y1, x2, y2);
    painter->drawLine(x2, y1, x1, y2);
    */
    
    //#else
    //QColor c(10,12,30,65);
    //painter->setPen(QPen(c, 2));

    QColor c = background_color;
    c.setAlpha(200);

    if (is_hovered)
      c = c.lighter(150);

    if (is_on){

      painter->setPen(Qt::NoPen);
      painter->setBrush(c);
      painter->drawRoundedRect(rect,4,4);
      
    }else{
      
      rect.adjust(1,1,-1,-1);
      painter->setPen(QPen(c, 2));
      painter->setBrush(patch_color);
      painter->drawRoundedRect(rect,3,3);
      
    }
    //    if (is_on)
    //  painter->fillRect(rect, c);
    //else
    //#endif

    /*
    c.setAlpha(255);
    painter->drawLine(x1, y1, x2, y2);
    painter->drawLine(x2, y1, x1, y2);
    */
    
  } else {

      painter->setPen(Qt::NoPen);
      painter->setBrush(patch_color);
      painter->drawRect(rect);

  }

  painter->setBrush(Qt::NoBrush);
    
  {
    painter->setPen(QPen(text_color, 2));

    painter->drawText(rect.adjusted(0.5,0,0,0), Qt::AlignCenter, name); // Why do we have to move the rectange 0.5 pixels to the right?
  }
}

Chip *find_chip_for_plugin(const QGraphicsScene *scene, const SoundPlugin *plugin){
  const QList<QGraphicsItem *> &das_items = scene->items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return chip;
    }
  }
  return NULL;
}

void CHIP_update(Chip *chip, SoundPlugin *plugin){
  int eff1 = plugin->type->num_effects+EFFNUM_INPUT_VOLUME;
  int eff2 = plugin->type->num_effects+EFFNUM_VOLUME;

  float max_gain = ::scale(MAX_VOLUME_SLIDER_DB, MIN_DB, MAX_DB, 0, 1);
  
  float val1_f = R_BOUNDARIES(0,
                              scale(PLUGIN_get_effect_value(plugin, eff1, VALUE_FROM_STORAGE),
                                    0,max_gain,
                                    0,1),
                              1);
  
  float val2_f = R_BOUNDARIES(0,
                              scale(PLUGIN_get_effect_value(plugin, eff2, VALUE_FROM_STORAGE),
                                    0,max_gain,
                                    0,1),
                              1);
  
  int val1 = (val1_f*val1_f) * 10000;
  int val2 = (val2_f*val2_f) * 10000;
  
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

void CHIP_update(SoundPlugin *plugin){
  Chip *chip = find_chip_for_plugin(get_scene(g_mixer_widget), plugin);
  R_ASSERT_RETURN_IF_FALSE(chip!=NULL);
  CHIP_update(chip, plugin);
}

/*
void CHIP_init_because_it_has_new_plugin(SoundPlugin *plugin){
  Chip *chip = find_chip_for_plugin(get_scene(g_mixer_widget), plugin);
  chip->init_new_plugin();
}
*/

float CHIP_get_pos_x(const struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE2(patch->patchdata!=NULL, 0);
  Chip *chip = find_chip_for_plugin(get_scene(g_mixer_widget), (SoundPlugin*)patch->patchdata);
  if (chip==NULL)
    return 0;
  return chip->x();
}

float CHIP_get_pos_y(const struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE2(patch->patchdata!=NULL, 0);
  Chip *chip = find_chip_for_plugin(get_scene(g_mixer_widget), (SoundPlugin*)patch->patchdata);
  if (chip==NULL)
    return 0;
  return chip->y();
}

static void CHIP_set_pos(Chip *chip, float x, float y){
  chip->setPos(x,y);
  printf("       Remake: CHIP_set_pos\n");
  remakeMixerStrips(make_instrument(-2));
}

void CHIP_set_pos(const struct Patch *patch,float x,float y){
  R_ASSERT_RETURN_IF_FALSE(patch->patchdata!=NULL);
  Chip *chip = find_chip_for_plugin(get_scene(g_mixer_widget), (SoundPlugin*)patch->patchdata);
  if (chip!=NULL)
    CHIP_set_pos(chip, x, y);
}

static void CHIP_kick_left_rec(Chip *chip, std::set<Chip*> &kicks){
  QPointF pos=chip->pos();

#if 0
  // First move chips which are connected. (but only if they are placed to the right of this chip)
  for(unsigned int i=0;i<chip->audio_connections.size();i++){
    Connection *connection = chip->audio_connections.at(i);
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
    
    SoundPlugin *plugin = SP_get_plugin((*chip)->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
    
    ADD_UNDO(ChipPos_CurrPos((struct Patch*)patch));
    (*chip)->setPos(pos.x()-grid_width, pos.y());
  }

  printf("       Remake: CHIP_kick_left\n");
  remakeMixerStrips(make_instrument(-2));
}

static void CHIP_kick_right_rec(Chip *chip, std::set<Chip*> &kicks){
  QPointF pos=chip->pos();

#if 0 // Disabled. Too messy.

  // First move chips which are connected. (but only if they are placed to the right of this chip)
  for(unsigned int i=0;i<chip->audio_connections.size();i++){
    Connection *connection = chip->audio_connections.at(i);
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

    SoundPlugin *plugin = SP_get_plugin((*chip)->_sound_producer);
    volatile struct Patch *patch = plugin->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    ADD_UNDO(ChipPos_CurrPos((struct Patch*)patch));
    (*chip)->setPos(pos.x()+grid_width, pos.y());
  }

  printf("       Remake: CHIP_kick_right\n");
  remakeMixerStrips(make_instrument(-2));
}

static Chip *get_chip_from_patch_id(QGraphicsScene *scene, instrument_t patch_id, bool patch_is_always_supposed_to_be_here = true){
  struct Patch *patch = patch_id.id==-1 ? PATCH_get_current() : PATCH_get_from_id(patch_id);

  if (patch==NULL){
    if (patch_is_always_supposed_to_be_here)
      RError("Could not find patch from id #%d", (int)patch_id.id);
    
    return NULL;
  }

  if (patch->instrument != get_audio_instrument()){
    RError("Found patch from id #%d, but it is a MIDI instrument", (int)patch_id.id);    
    return NULL;
  }
  
  return CHIP_get(scene, patch);
}

/*
static bool connect(QGraphicsScene *scene, Chip *from, int from_portnum, Chip *to, int to_portnum){
  return SP_add_link(to->_sound_producer, to_portnum, 
                     from->_sound_producer, from_portnum);
}
*/

static bool econnect(QGraphicsScene *scene, Chip *from, Chip *to){
  if (SP_add_elink(to->_sound_producer, from->_sound_producer) == false)
    return false;

  SoundPlugin *plugin1 = SP_get_plugin(from->_sound_producer);
  volatile struct Patch *patch1 = plugin1->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch1!=NULL, true);

  SoundPlugin *plugin2 = SP_get_plugin(to->_sound_producer);
  volatile struct Patch *patch2 = plugin2->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch2!=NULL, true);

  if (PATCH_add_event_receiver((struct Patch*)patch1,
                               (struct Patch*)patch2)
      == false
      )
    {
      RError("Impossible situation: PATCH_add_event_receiver()==true and SP_add_elink()==false");
      //return false;
    }
  
  return true;
}

bool CHIPS_are_connected(const Chip *from, const Chip *to){
  R_ASSERT_RETURN_IF_FALSE2(from!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(to!=NULL, false);
  
  return CONNECTION_find_audio_connection(from, to) != NULL;
}

bool CHIPS_are_econnected(const Chip *from, const Chip *to){
  R_ASSERT_RETURN_IF_FALSE2(from!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(to!=NULL, false);
  
  for(EventConnection *connection : from->_output_event_connections)
    if(connection->to==to)
      return true;

  return false;
}

namespace radium{
  void LinkParameters::add(::Chip *source, int source_ch, ::Chip *target, int target_ch, float volume, EnableType link_enable_type){
    add(source->_sound_producer, source_ch, target->_sound_producer, target_ch, volume, link_enable_type);
  }
}

namespace{
  namespace changes{
    
  struct Parm{
    Chip *_from;
    Chip *_to;

    float _volume;

  private:
    radium::EnableType _enable_type = radium::EnableType::DONT_CHANGE;

    mutable bool _tried_to_get_connection = false;
    mutable AudioConnection *_connection = NULL;

  public:

    AudioConnection *get_connection(void) const {
      if (_connection==NULL && _tried_to_get_connection==false){
        _connection = CONNECTION_find_audio_connection(_from, _to);
        _tried_to_get_connection = true;
      }
      
      return _connection;
    }
    
    bool are_connected(void) const {
      return get_connection() != NULL;
    }

    radium::EnableType get_enable_type(void) const {
      return _enable_type;
    }

    /*
    bool is_enabled(void) const {
      return _enable_type==radium::EnableType::ENABLE;
    }
    */
    
    ConnectionType _connection_type;

  public:
    
    Parm(Chip *from, Chip *to, float volume, radium::EnableType enable_type, ConnectionType connection_type)
      : _from(from)
      , _to(to)
      , _volume(volume)
      , _enable_type(enable_type)
      , _connection_type(connection_type)
    {
      if (from!=NULL && to!=NULL){
        R_ASSERT(from!=NULL);
        R_ASSERT(to!=NULL);
      }
    }

    Parm(Chip *from, Chip *to)
      : Parm(from, to, -1.0, radium::EnableType::DONT_CHANGE, /*radium::EnableType::DONT_CHANGE,*/ ConnectionType::NOT_SET)
    {
    }

    // QVector requires an empty constructor in Parms::add
    Parm()
      : Parm(NULL, NULL)
    {}

    bool can_be_connected(void) const {
      if(_from->_num_outputs==0 || _to->_num_inputs==0)
        return false;
    
      return true;
    }

    bool can_be_disconnected(void) const {
      return are_connected();
    }
  };

  struct AudioGraph {
    QVector<Parm> to_add;
    QVector<Parm> to_remove;

  private:
    radium::Scheduled_RT_functions ___INTERNAL_rt_functions2;
    radium::SoloChanges ___INTERNAL_solo_changes2;

  public:
    
    radium::Scheduled_RT_functions &rt_functions;
    radium::SoloChanges &solo_changes;
      
    // Prevent heap allocation
    void *operator new (size_t) = delete;
    void *operator new[] (size_t) = delete;
    void  operator delete (void *) = delete;
    void  operator delete[] (void*) = delete;

    AudioGraph(const AudioGraph&) = delete;
    AudioGraph& operator=(const AudioGraph&) = delete;

    AudioGraph(radium::Scheduled_RT_functions &rt_functions, radium::SoloChanges &solo_changes)
      : rt_functions(rt_functions)
      , solo_changes(solo_changes)
    {
    }

    AudioGraph()
      : ___INTERNAL_solo_changes2(___INTERNAL_rt_functions2)
      , rt_functions(___INTERNAL_rt_functions2)
      , solo_changes(___INTERNAL_solo_changes2)
    {
    }

    int find_pos(const QVector<Parm> &parms, const Chip *from, const Chip *to) const {
      for(int i = 0 ; i < parms.size() ; i++)
        if (parms.at(i)._from==from && parms.at(i)._to==to)
          return i;
      
      return -1;
    }

    bool has_removed_connection(const Chip *from, const Chip *to) const {
      return find_pos(to_remove, from, to) >= 0;
    }
    
    bool has_added_connection(const Chip *from, const Chip *to) const {
      return find_pos(to_add, from, to) >= 0;
    }
    
    void add(Chip *from, Chip *to, float volume, radium::EnableType enable_type, ConnectionType connection_type){
      int pos = find_pos(to_remove, from, to);
      if (pos >= 0)
        to_remove.remove(pos); // This way we don't need to implement an AudioGraph 'diff' function. Instead we just remove all existing connections and add all connections in a state.
      
      to_add.push_back(Parm(from, to, volume, enable_type, connection_type)); // Although the link might already be there, volume and enabled might have different values.
    }
    
    void remove(Chip *from, Chip *to){
      int pos = find_pos(to_add, from, to);
      if (pos >= 0)
        to_add.remove(pos); // This way we don't need to implement an AudioGraph 'diff' function. Instead we just remove all existing connections and add all connections in a state.
      else
        to_remove.push_back(Parm(from, to));
    }
    
    void remove(AudioConnection *connection){
      remove(connection->from, connection->to);
    }
  };

  struct Connect : public AudioGraph {
    Connect(){
    }
    
    Connect(Chip *from, Chip *to, ConnectionType connection_type, float volume = -1.0, radium::EnableType enable_type = radium::EnableType::DONT_CHANGE){
      add(from, to, volume, enable_type, connection_type);
    }
  };
  
  struct Disconnect : public AudioGraph {
    Disconnect(){
    }
    
    Disconnect(Chip *from, Chip *to){
      remove(from, to);
    }
    
    Disconnect(AudioConnection *connection){
      remove(connection);
    }
  };

  struct Volume : public AudioGraph {
  };

  }
}

static bool CONNECTIONS_apply_changes(QGraphicsScene *scene, const changes::AudioGraph &changes){
  radium::LinkParameters add_linkparameters;
  radium::LinkParameters remove_linkparameters;

  // ADD: Create parameters
  for(const auto &parm : changes.to_add){
    if (parm.can_be_connected()){
      
      Chip *from = parm._from;
      Chip *to = parm._to;

      bool from_is_mono = from->_num_outputs==1;
      bool to_is_mono   = to->_num_inputs==1;
      //printf("  %d -> %d\n", from->_num_outputs, to->_num_inputs);

      radium::EnableType enable_type = parm.get_enable_type();

      if(from_is_mono==true){
        for(int to_portnum=0 ; to_portnum<to->_num_inputs ; to_portnum++)
          add_linkparameters.add(from, 0, to, to_portnum, parm._volume, enable_type);
      }else if(to_is_mono==true){
        for(int from_portnum=0 ; from_portnum<from->_num_outputs ; from_portnum++){
          add_linkparameters.add(from, from_portnum, to, 0, parm._volume, enable_type);
        }
      }else{
        for(int portnum=0 ; portnum<std::min(from->_num_outputs,to->_num_inputs) ; portnum++)
          add_linkparameters.add(from, portnum, to, portnum, parm._volume, enable_type);
      }
    }
  }

  
  // REMOVE: Create parameters
  for(const auto &parm : changes.to_remove){
    if (parm.can_be_disconnected()){

      Chip *from = parm._from;
      Chip *to = parm._to;

      bool from_is_mono = from->_num_outputs==1;
      bool to_is_mono   = to->_num_inputs==1;
      
      if(from_is_mono==true){
        for(int to_portnum=0 ; to_portnum<to->_num_inputs ; to_portnum++){
          remove_linkparameters.add(from, 0, to, to_portnum);
        }
      }else if(to_is_mono==true){
        for(int from_portnum=0 ; from_portnum<from->_num_outputs ; from_portnum++){
          remove_linkparameters.add(from, from_portnum, to, 0);
        }
      }else{
        for(int portnum=0 ; portnum<std::min(from->_num_outputs,to->_num_inputs) ; portnum++){
          remove_linkparameters.add(from, portnum, to, portnum);
        }
      }

    }
  }


  // Create/remove mixer connections
  if (SP_add_and_remove_links(add_linkparameters, remove_linkparameters, changes.solo_changes, changes.rt_functions)==false)
    return false;

  QSet<const struct Patch *> mixerstrips_to_remake;
  QSet<const struct Patch *> mixerstrips_to_redraw;
  
  
  // ADD: Create mixergui connections
  for(const auto &parm : changes.to_add){
    R_ASSERT(scene != NULL);
    
    if (parm.can_be_connected()) {

      if (parm.are_connected()) {

        if (parm.get_enable_type()!=radium::EnableType::DONT_CHANGE)
          parm.get_connection()->set_enabled_only_dont_update_link(parm.get_enable_type()==radium::EnableType::ENABLE);
        
        // We have only set link gain and enabled/disabled.
        mixerstrips_to_redraw.insert(CHIP_get_patch(parm._from));
        
      } else {

        Patch *from_patch = CHIP_get_patch(parm._from);
        Patch *to_patch = CHIP_get_patch(parm._to);
        
        //if (from_patch->is_visible && to_patch->is_visible) {
        if (from_patch->is_visible)
          mixerstrips_to_remake.insert(from_patch);

        if (to_patch->is_visible)
          mixerstrips_to_remake.insert(to_patch);
          //}
        
        AudioConnection *connection = new AudioConnection(scene, parm._from, parm._to, parm._connection_type, parm.get_enable_type()!=radium::EnableType::DISABLE);
        scene->addItem(connection);

        {
          int bus_num = SP_get_bus_num(parm._to->_sound_producer);
          
          if (bus_num >= 0){
            if (from_patch==PATCH_get_current())
              GFX_PP_Update(from_patch, false);
          }
        }

      }
    }
  }

  
  // REMOVE: Delete mixergui connections
  for(const auto &parm : changes.to_remove){
    if (parm.can_be_disconnected()){
      AudioConnection *connection = CONNECTION_find_audio_connection(parm._from, parm._to);
      if (connection==NULL)
        R_ASSERT(false);
      else {
        
        Patch *from_patch = CHIP_get_patch(parm._from);
        Patch *to_patch = CHIP_get_patch(parm._to);
        
        //if (from_patch->is_visible && to_patch->is_visible) {

        if (from_patch->is_visible)
          mixerstrips_to_remake.insert(from_patch);

        if (to_patch->is_visible)
          mixerstrips_to_remake.insert(to_patch);
          
        //}
        
        CONNECTION_delete_an_audio_connection_where_all_links_have_been_removed(connection);
      }
    }
  }

  
  //printf("       Remake: CONNECTIONS_apply_changes\n");
  for(const auto *patch : mixerstrips_to_redraw){
    if (patch==NULL){
      R_ASSERT_NON_RELEASE(false);
    }else{
      if(mixerstrips_to_remake.contains(patch)==false){
        redrawMixerStrips(false); // We might redraw mixer strips unnecessarily if volume and/or enabled wasn't changed, but redrawMixerStrips should be a light operation. (actually, it's not. Looks like recursively updating the mixer strips takes a lot of time. @$!%$^! qt widget system)
        break;
      }
    }
  }
  
  for(const auto *patch : mixerstrips_to_remake){
    if (patch==NULL){
      R_ASSERT_NON_RELEASE(false);
    }else
      remakeMixerStrips(patch->id);
  }

  return true;
}

bool CONNECTIONS_apply_changes(const dynvec_t dynchanges){
  changes::AudioGraph changes;

  auto *scene = get_scene(g_mixer_widget);
    
  for(int i = 0 ; i < dynchanges.num_elements ; i++){

    const dyn_t &element = dynchanges.elements[i];
    
    const char *errorstring = talloc_format("Element %d: ", i);
    
    if (!DYN_check_type(element, HASH_TYPE, errorstring))
      return false;

    if (!HASH_check_type(element.hash, ":type", STRING_TYPE, errorstring))
      return false;

    if (!HASH_check_type(element.hash, ":source", INSTRUMENT_TYPE, errorstring))
      return false;
    
    if (!HASH_check_type(element.hash, ":target", INSTRUMENT_TYPE, errorstring))
      return false;

    instrument_t source_id = HASH_get_instrument(element.hash, ":source");
    instrument_t target_id = HASH_get_instrument(element.hash, ":target");

    Chip *source = get_chip_from_patch_id(scene, source_id);
    Chip *target = get_chip_from_patch_id(scene, target_id);

    if (source==NULL){
      handleError("Source instrument #%d not found", (int)source_id.id);
      return false;
    }

    if (target==NULL){
      handleError("Target instrument #%d not found", (int)target_id.id);
      return false;
    }

    //bool are_connected = CHIPS_are_connected(source, target); // No, it's legal to disconnect/connect simultaneously.
          
    const wchar_t *type_name = HASH_get_string(element.hash, ":type");

    float gain = -1.0;
    radium::EnableType enable_type = radium::EnableType::DONT_CHANGE;

    if (STRING_equals(type_name, "connect")){
      
      if (HASH_has_key(element.hash, ":gain")){

        const dyn_t dyngain = HASH_get_dyn(element.hash, ":gain");
        if (!DYN_is_number(dyngain)){
          handleError("Element %d[\"gain\"]: Expected number, found \"%s\"", i, DYN_type_name(dyngain));
          return false;
        }

        gain = DYN_get_double_from_number(dyngain);        
      }

      if (HASH_has_key(element.hash, ":enabled")){

        const dyn_t dynenabled = HASH_get_dyn(element.hash, ":enabled");
        if (dynenabled.type!=INT_TYPE){
          handleError("Element %d[\"enabled\"]: Expected integer, found \"%s\"", i, DYN_type_name(dynenabled));
          return false;
        }

        enable_type = radium::get_enable_type_from_int(dynenabled.int_number);
      }

      ConnectionType connection_type = ConnectionType::NOT_SET;
      if (HASH_has_key(element.hash, ":connection-type"))
        connection_type = get_connection_type_from_int(HASH_get_int32(element.hash, ":connection-type"));
            
      changes.add(source, target, gain, enable_type, connection_type);
      
    } else if (STRING_equals(type_name, "disconnect")){

      changes.remove(source, target);
      
    } else {
      handleError("Element %d[\"type\"]: Expected \"connect\" or \"disconnect\", found \"%S\"", i, type_name);
      return false;
    }
      
  }

  
  return CONNECTIONS_apply_changes(scene, changes);
}

bool CHIP_connect_chips(QGraphicsScene *scene, Chip *from, Chip *to, ConnectionType connection_type){
//printf("Connect chips\n");
  return CONNECTIONS_apply_changes(scene, changes::Connect(from, to, connection_type));
}

bool CHIP_disconnect_chips(QGraphicsScene *scene, Chip *from, Chip *to){  
  return CONNECTIONS_apply_changes(scene, changes::Disconnect(from, to));
}

bool CHIP_connect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to, ConnectionType connection_type){
  return CHIP_connect_chips(scene, find_chip_for_plugin(scene, from), find_chip_for_plugin(scene, to), connection_type);
}

static void changeremove_all_audio_connections(const QGraphicsScene *scene, changes::AudioGraph &changes){
  const QList<QGraphicsItem *> &das_items = scene->items();

  for (int i = 0; i < das_items.size(); ++i) {
    AudioConnection *audio_connection = dynamic_cast<AudioConnection*>(das_items.at(i));      
    if(audio_connection!=NULL && audio_connection->from!=NULL && audio_connection->to!=NULL)
      changes.remove(audio_connection);
  }
}

static void CONNECTIONS_remove_all2(QGraphicsScene *scene, bool include_audio, bool include_events, changes::AudioGraph &changes){
  radium::Vector<SoundProducer*> producers;
  radium::Vector<EventConnection*> event_connections;

  if (include_audio)
    changeremove_all_audio_connections(scene, changes);

  if (include_events) {
    
    {
      const QList<QGraphicsItem *> &das_items = scene->items();
      
      for (int i = 0; i < das_items.size(); ++i) {
        Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
        if(chip!=NULL)
          producers.push_back(chip->_sound_producer);
        else{
          EventConnection *event_connection = dynamic_cast<EventConnection*>(das_items.at(i));
          if(event_connection!=NULL && event_connection->from!=NULL && event_connection->to!=NULL)
            event_connections.push_back(event_connection);
        }
      }
    }
    
    // Think this must be done before calling SP_remove_all_elinks. (it was opposite before)
    {
      radium::PlayerLockOnlyIfNeeded lock;
      
      int i=0;
      for(auto *producer : producers){
        lock.maybe_pause(i++);
        SoundPlugin *plugin = SP_get_plugin(producer);
        volatile struct Patch *patch = plugin->patch;
        if (patch!=NULL)
          PATCH_remove_all_event_receivers((struct Patch*)patch, lock);
      }
    }

    SP_remove_all_elinks(producers);

    for(auto event_connection : event_connections) {
      //if (is_loading)
      //  GFX_ShowProgressMessage(talloc_format("Deleting event connection between %s and %s", CHIP_get_patch(event_connection->from)->name, CHIP_get_patch(event_connection->to)->name));
      
      CONNECTION_delete_an_event_connection_where_all_links_have_been_removed(event_connection);
    }

  }

}

void CONNECTIONS_remove_all(QGraphicsScene *scene){
  changes::AudioGraph changes;
  CONNECTIONS_remove_all2(scene, true, true, changes);
  CONNECTIONS_apply_changes(scene, changes);
}

// Simultaneously do these three things:
// 1. Remove connection between before and middle
// 2. Remove connection between middle and after
// 3. Add connection between before and after
void CHIP_remove_chip_from_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after){

  ConnectionType connection_type = ConnectionType::NOT_SET;
  bool new_is_enabled = true;

  const AudioConnection *connection1 = CONNECTION_find_audio_connection(before, middle);
  const AudioConnection *connection2 = CONNECTION_find_audio_connection(middle, after);
  R_ASSERT_NON_RELEASE(connection1 != NULL);
  R_ASSERT_NON_RELEASE(connection2 != NULL);
    
  if (connection1 != NULL && connection2 != NULL){
    
    if (connection1->get_connection_type()==ConnectionType::IS_SEND || connection2->get_connection_type()==ConnectionType::IS_SEND)
      connection_type = ConnectionType::IS_SEND;
    
    else if (connection1->get_connection_type()==ConnectionType::IS_PLUGIN && connection2->get_connection_type()==ConnectionType::IS_PLUGIN)
      connection_type = ConnectionType::IS_PLUGIN;
    
    new_is_enabled = connection1->get_enabled() && connection2->get_enabled();
  }

  changes::AudioGraph changes;
  changes.remove(before, middle);
  changes.remove(middle, after);
  changes.add(before, after, -1.0, radium::get_enable_type_from_bool(new_is_enabled), connection_type);
  
  CONNECTIONS_apply_changes(scene, changes);
}

// Opposite of remove_chip_from_connection_sequence
void CHIP_add_chip_to_connection_sequence(QGraphicsScene *scene, Chip *before, Chip *middle, Chip *after){
  const ConnectionType connection_type1 = ConnectionType::IS_PLUGIN;
  ConnectionType connection_type2 = ConnectionType::NOT_SET;
  bool new_is_enabled = true;

  {
    const AudioConnection *connection = CONNECTION_find_audio_connection(before, after);
    R_ASSERT_NON_RELEASE(connection != NULL);
    if (connection != NULL){
      connection_type2 = connection->get_connection_type();
      new_is_enabled = connection->get_enabled();
    }
  }
  
  changes::AudioGraph changes;
  changes.add(before, middle, -1.0, radium::get_enable_type_from_bool(new_is_enabled), connection_type1);
  changes.add(middle, after, -1.0, radium::get_enable_type_from_bool(new_is_enabled), connection_type2);
  changes.remove(before, after);
  CONNECTIONS_apply_changes(scene, changes);
}

bool CHIP_econnect_chips(QGraphicsScene *scene, Chip *from, Chip *to){
  if(CHIPS_are_econnected(from,to)==true){
    printf("Chips are already econnected\n");
    return false;
  }

  if(econnect(scene, from, to)==false)
    return false; // trying to make recursive connection

  EventConnection *econnection = new EventConnection(scene, from, to);
  
  scene->addItem(econnection);
  return true;
}

bool CHIP_econnect_chips(QGraphicsScene *scene, SoundPlugin *from, SoundPlugin *to){
  return CHIP_econnect_chips(scene, find_chip_for_plugin(scene, from), find_chip_for_plugin(scene, to));
}

int CHIP_get_num_in_connections(const Patch *patch){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  return chip->_input_audio_connections.size();
}

int CHIP_get_num_out_connections(const Patch *patch){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  return chip->_output_audio_connections.size();
}

int CHIP_get_num_in_econnections(const Patch *patch){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  return chip->_input_event_connections.size();
}

int CHIP_get_num_out_econnections(const Patch *patch){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  return chip->_output_event_connections.size();
}

struct Patch* CHIP_get_source(const struct Patch *patch, int connectionnum){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  if (connectionnum >= chip->_input_audio_connections.size())
    return NULL;

  return CHIP_get_patch(chip->_input_audio_connections.at(connectionnum)->from);
}
  
struct Patch* CHIP_get_dest(const struct Patch *patch, int connectionnum){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  if (connectionnum >= chip->_output_audio_connections.size())
    return NULL;

  return CHIP_get_patch(chip->_output_audio_connections.at(connectionnum)->to);
}
  
struct Patch* CHIP_get_esource(const struct Patch *patch, int connectionnum){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  if (connectionnum >= chip->_input_event_connections.size())
    return NULL;

  return CHIP_get_patch(chip->_input_event_connections.at(connectionnum)->from);
}
  
struct Patch* CHIP_get_edest(const struct Patch *patch, int connectionnum){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL,0);

  if (connectionnum >= chip->_output_event_connections.size())
    return NULL;

  return CHIP_get_patch(chip->_output_event_connections.at(connectionnum)->to);
}


static SuperConnection *g_current_connection = NULL;

SuperConnection *SuperConnection::get_current_connection(void){
  return g_current_connection;
}

void SuperConnection::set_current_connection(SuperConnection *connection){
  g_current_connection = connection;
}


void CONNECTION_delete_an_audio_connection_where_all_links_have_been_removed(AudioConnection *connection){
  Chip *from = connection->from;
  Chip *to = connection->to;

  R_ASSERT_RETURN_IF_FALSE(from!=NULL);
  R_ASSERT_RETURN_IF_FALSE(to!=NULL);  

  from->_output_audio_connections.remove(connection);
  to->_input_audio_connections.remove(connection);

  delete connection;

  {
    int bus_num = SP_get_bus_num(to->_sound_producer);
    
    if (bus_num >= 0){
      struct Patch *patch = CHIP_get_patch(from);
      GFX_ScheduleInstrumentRedraw(patch); // We schedule it so that it only run once after all audio graph changes are finished.
    }
  }
}


void CONNECTION_delete_an_event_connection_where_all_links_have_been_removed(EventConnection *connection){
  Chip *from = connection->from;
  Chip *to = connection->to;

  R_ASSERT_RETURN_IF_FALSE(from!=NULL);
  R_ASSERT_RETURN_IF_FALSE(to!=NULL);
  
  from->_output_event_connections.remove(connection);
  to->_input_event_connections.remove(connection);
  
  delete connection;
}

void CONNECTION_delete_audio_connection(AudioConnection *connection){  
  CONNECTIONS_apply_changes(NULL, changes::Disconnect(connection));
}

void CONNECTION_delete_event_connection(EventConnection *connection){
  Chip *from = connection->from;
  Chip *to = connection->to;

  R_ASSERT_RETURN_IF_FALSE(from!=NULL);
  R_ASSERT_RETURN_IF_FALSE(to!=NULL);  

  SoundPlugin *plugin1 = SP_get_plugin(from->_sound_producer);
  volatile struct Patch *patch1 = plugin1->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch1!=NULL, );
  
  SoundPlugin *plugin2 = SP_get_plugin(to->_sound_producer);
  volatile struct Patch *patch2 = plugin2->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch2!=NULL, );
  
  
  PATCH_remove_event_receiver((struct Patch*)patch1,
                              (struct Patch*)patch2);
  
  SP_remove_elink(to->_sound_producer, from->_sound_producer);

  CONNECTION_delete_an_event_connection_where_all_links_have_been_removed(connection);
}

void CONNECTION_delete_connection(SuperConnection *connection){
  if (connection->_is_event_connection) {
    R_ASSERT_RETURN_IF_FALSE(dynamic_cast<EventConnection*>(connection) != NULL);
    CONNECTION_delete_event_connection(dynamic_cast<EventConnection*>(connection));
  } else {
    R_ASSERT_RETURN_IF_FALSE(dynamic_cast<AudioConnection*>(connection) != NULL);
    CONNECTION_delete_audio_connection(dynamic_cast<AudioConnection*>(connection));
  }
}

static bool are_connected_somehow(QSet<const Chip*> &already_checked, const Chip *source, const Chip *target){

  if (already_checked.contains(source))
    return false;
  
  if (source==target)
    return true;

  already_checked << source;
  
  for(const AudioConnection *source_connection : source->_output_audio_connections)
    if (are_connected_somehow(already_checked, source_connection->to, target)==true)
      return true;
  
  for(const EventConnection *source_connection : source->_output_event_connections)
    if (are_connected_somehow(already_checked, source_connection->to, target)==true)
      return true;
  
  return false;
}

bool CONNECTION_are_connected_somehow(const Chip *source, const Chip *target){
  QSet<const Chip*> already_checked;
  return are_connected_somehow(already_checked, source, target);
}

bool CONNECTION_can_connect(const Chip *source, const Chip *target){
  return !CONNECTION_are_connected_somehow(target, source);
}
  
bool CONNECTION_can_connect(struct Patch *source, struct Patch *target){
  Chip *start = CHIP_get(get_scene(g_mixer_widget), source);
  Chip *end = CHIP_get(get_scene(g_mixer_widget), target);

  return CONNECTION_can_connect(start, end);
}
  
void CONNECTION_update(struct Patch *source, struct Patch *target){
  Chip *from = CHIP_get(get_scene(g_mixer_widget), source);
  Chip *to = CHIP_get(get_scene(g_mixer_widget), target);

  for(AudioConnection *connection : from->_output_audio_connections)
    if(connection->to==to){
      connection->update();
      break;
    }

  for(auto *connection : from->_output_event_connections)
    if(connection->to==to){
      connection->update();
      break;
    }
}

static float get_connection_note_intencity(const SuperConnection *connection){
  if (connection->_from==NULL)
    return 0.0;
  
  int intencity = ATOMIC_GET_RELAXED(CHIP_get_patch(connection->_from)->visual_note_intencity);

  float min_intencity = MAX_NOTE_INTENCITY * 4.0 / 5.0;
  
  if(intencity < min_intencity)
    return 0.0;
  else
    return scale(intencity,
                 min_intencity, MAX_NOTE_INTENCITY,
                 0.0, 1.0);
}

static QColor get_arrow_color(const SuperConnection *connection, const QColor &connection_color){
  const QColor &col = connection_color;
  
  const Chip *from = connection->_from;
  
  if (from==NULL)
    return col;

  if (connection->_is_event_connection) {

    float intencity = get_connection_note_intencity(connection);
    if (intencity > 0.001)
      return mix_colors(Qt::white, col, intencity);
    
  } else {
    
    const struct SoundPlugin *plugin = SP_get_plugin(from->_sound_producer);

    R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, col);
    
    if (plugin->type->num_outputs > 0){
      
      float db = connection->_last_displayed_db_peak;
      
      if(db>=4.0f)
        
        return mix_colors(get_qcolor(PEAKS_4DB_COLOR_NUM), QColor("410000"),                ::scale(db,   4, MAX_DB, 1, 0));
      
      if(db>=0.0f)
        
        return mix_colors(get_qcolor(PEAKS_0DB_COLOR_NUM), get_qcolor(PEAKS_4DB_COLOR_NUM), ::scale(db,   0, 4,      1, 0));
      
      else if(db>=-30.0f)
        
        return mix_colors(get_qcolor(PEAKS_COLOR_NUM), get_qcolor(PEAKS_0DB_COLOR_NUM),   ::scale(db, -30, 0,      1, 0));
        
      else
        
        return get_qcolor(MIXER_AUDIO_CONNECTION_COLOR_NUM); //PEAKS_COLOR_NUM); //.darker(150);
      
    }
    
  }


  return col;
  /*
    if (is_selected)
    return col.lighter(198);
    else
    return col;
  */
}

static QColor get_line_color(const SuperConnection *connection, const QColor &connection_color){
  
  float intencity = get_connection_note_intencity(connection);
  
  if (intencity > 0.001)
    return mix_colors(connection_color, Qt::white, intencity);
  else
    return connection_color;
}

static QPolygonF create_arrow_polygon(const QLineF &line,
                                      qreal arrow_width  // Distance between the arrow corners.
                                      )
{
  QLineF gakk(line);
  gakk.setLength(arrow_width / 2.0);

  QPolygonF ret;
  ret << QPointF(line.p1().x() + gakk.dy(), line.p1().y() - gakk.dx()) // arrow corner above line
      << line.p2()
      << QPointF(line.p1().x() - gakk.dy(), line.p1().y() + gakk.dx()); // arrow corner below line
  
  return ret;
}

static void update_subline_connection_positions(SuperConnection *connection, bool update_line, bool update_arrow){
  const QLineF &line = connection->line();

  const double line_length = line.length();
  
  if (line_length< 0.001)
    return;

  float db = connection->_last_displayed_db_peak;
  
  qreal len = 12; //is_selected ? 10 : 6;
  
  if (connection->_is_event_connection)
    len *= 2;
  else if (db >= MIN_DB_A_LITTLE_BIT_ABOVE)
    len *= scale(db, MIN_DB_A_LITTLE_BIT_ABOVE, MAX_DB, 0.25, 5);

  qreal normalized_arrow_len = scale_double(len, 0, line_length, 0, 1);
  if(normalized_arrow_len > 0.99)
    normalized_arrow_len = 0.99;

  
  double normalized_p;

  if (connection->_is_event_connection)
    normalized_p = 0.5; //scale_double(0.5, 0, 1, normalized_arrow_len/2.0, 1.0-normalized_arrow_len/2.0);
  else
    normalized_p = scale_double(db, MIN_DB, MAX_DB, normalized_arrow_len/2.0, 1.0-normalized_arrow_len/2.0);
  
  /*
    double linear = db2linear(db, 1, 0);
  double normalized_p = scale_double(linear, 0, 1, normalized_arrow_len/2.0, 1.0-normalized_arrow_len/2.0);
  if (ATOMIC_GET(root->editonoff))
    normalized_p = scale_double(db, MIN_DB, MAX_DB, normalized_arrow_len/2.0, 1.0-normalized_arrow_len/2.0);
  */
  
  double normalized_p1 = R_MAX(0.0, normalized_p - normalized_arrow_len/2.0);
  double normalized_p2 = R_MIN(1.0, normalized_p + normalized_arrow_len/2.0);
  
  QPointF p1 = line.pointAt(normalized_p1); // arrow start pos (on line)
  QPointF p2 = line.pointAt(normalized_p2); // arrow end pos (on line)

  connection->_arrow.setPolygon(create_arrow_polygon(QLineF(p1, p2), len / 2.0));
  
  connection->_visible_line.setLine(QLineF(line.p1(), line.p2()));
}


static float get_connection_width(const SuperConnection *connection){
  if (connection->_is_event_connection)
    return 1.0;
    
  //float db = connection->_last_displayed_db_peak;
  
  float pen_width;
  
  //if (db >= -35)
  //  pen_width = ::scale(db, -35, MAX_DB, 0.3, 10);
  //else
  pen_width = 0.6;

  if (connection->_is_selected)
    pen_width *= 1.8;
  else
    pen_width *= 1.2;

  return pen_width;
}

static QPen get_connection_pen(const SuperConnection *connection, const QColor &color){

  float width = get_connection_width(connection);
  
  QPen pen(color, width);
  
  pen.setJoinStyle(Qt::RoundJoin);
  pen.setCapStyle(Qt::RoundCap);
  
  return pen;
}

static bool connection_is_implicitly_muted(SuperConnection *connection){
  if (connection->to==NULL || connection->from==NULL)
    return false;

  if (SP_get_link_implicitly_muted(connection->to->_sound_producer, connection->from->_sound_producer, NULL))
    return true;
  
  if (!root->song->mute_system_buses_when_bypassed)
    return false;

  SoundPlugin *from_plugin = SP_get_plugin(connection->from->_sound_producer);
  
  if (!is_bypassed(from_plugin))
    return false;
  
  return SP_get_bus_num(connection->to->_sound_producer) >= 0;
}

static float get_connection_color_alpha(SuperConnection *connection){
  if (!connection->get_enabled() || connection_is_implicitly_muted(connection))
    return 30;
  else if(connection->_is_selected)
    return 250;
  else
    return 140;
}

static void update_subline_connection_colors(SuperConnection *connection, const bool update_line, bool update_arrow){
  QColor connection_color = get_qcolor(connection->_color_num);
  int alpha = get_connection_color_alpha(connection);

  if (update_line) {
    QColor main_color = get_line_color(connection, connection_color);
    main_color.setAlpha(alpha);
    QPen visible_pen = get_connection_pen(connection, main_color);
    connection->_visible_line.setPen(visible_pen);
  }

  if (update_arrow){  
    QColor arrow_color = get_arrow_color(connection, connection_color);
    arrow_color.setAlpha(alpha);
    connection->_arrow.setBrush(arrow_color);
  }
}

void SuperConnection::update_shape(bool update_line, bool update_arrow){
  update_subline_connection_colors(this, update_line, update_arrow);
  update_subline_connection_positions(this, update_line, update_arrow);
}

void AudioConnection::update_position(void){
  if (from==NULL || to==NULL)
    return;
  
  int x1 = from->pos().x()+grid_width-port_width/2;
  int y1 = CHIP_get_port_y(from);
  
  int x2 = to->pos().x()+port_width/2;
  int y2 = CHIP_get_port_y(to);

  setLine(x1,y1,x2,y2);
  
  update_shape(true, true);
}

void AudioConnection::set_connection_type(ConnectionType connection_type){

  R_ASSERT_RETURN_IF_FALSE(from != NULL);

  bool has_set=false;

  // Ensure there is only one plugin connection from 'from'.
  if(connection_type==ConnectionType::IS_PLUGIN)
    for(auto *connection : from->_output_audio_connections)

      if(connection->to != to && connection->_connection_type==ConnectionType::IS_PLUGIN){
          
        R_ASSERT(has_set==false);
        
        connection->_connection_type=ConnectionType::NOT_SET;
        
        has_set=true;
        
      }

  _connection_type = connection_type;
}

void EventConnection::update_position(void){
  if (from==NULL || to==NULL)
    return;
  
  int x1 = CHIP_get_eport_x(from);
  int y1 = CHIP_get_output_eport_y(from);
  
  int x2 = CHIP_get_eport_x(to);
  int y2 = CHIP_get_input_eport_y(to);
  
  /*
    if (x2>x1) {
    y2 = ::scale(x2-10,x1,x2,y1,y2);
    x2 -= 10;
    } else if (x2<x1) {
    y2 = ::scale(x2+10,x1,x2,y1,y2);
    x2 += 10;
    }
  */

  setLine(x1,y1,x2,y2);
  
  update_shape(true, true);
}

// 'right_chip' is inserted in the middle of 'left_chip' and all chips 'left_chip' sends to.
void CHIP_connect_left(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip){
  changes::AudioGraph changes;
  
  for (AudioConnection *connection : left_chip->_output_audio_connections){
    changes.remove(connection);
    changes.add(right_chip, connection->to, -1.0, radium::EnableType::ENABLE, connection->get_connection_type());
  }

  changes.add(left_chip, right_chip, -1.0, radium::EnableType::ENABLE, ConnectionType::IS_PLUGIN);

  CONNECTIONS_apply_changes(scene, changes);
}

// 'left_chip' is inserted in the middle of 'right_chip' and all chips 'right_chip' receives from.
void CHIP_connect_right(QGraphicsScene *scene, Chip *left_chip, Chip *right_chip){
  changes::AudioGraph changes;

  for (AudioConnection *connection : right_chip->_input_audio_connections){
    changes.remove(connection);
    changes.add(connection->from, left_chip, -1.0, radium::EnableType::ENABLE, connection->get_connection_type());
  }

  changes.add(left_chip, right_chip, -1.0, radium::EnableType::ENABLE, ConnectionType::IS_PLUGIN);

  CONNECTIONS_apply_changes(scene, changes);
}

void Chip::init_new_plugin(void){

  struct SoundPlugin *plugin = SP_get_plugin(_sound_producer);

  _num_inputs = plugin->type->num_inputs;
  _num_outputs = plugin->type->num_outputs;

  if (_input_slider!=NULL)
    SLIDERPAINTER_delete(_input_slider);
  if (_output_slider!=NULL)
    SLIDERPAINTER_delete(_output_slider);
    
  {
    int x1,x2,y1,y2;
    get_slider1_coordinates(x1,x2,y1,y2);
    _input_slider = SLIDERPAINTER_create(this,x1,x2,y1,y2);
    SLIDERPAINTER_set_alternative_color(_input_slider, true);
    SLIDERPAINTER_set_num_channels(_input_slider, _num_inputs);
  }
  
  {
    int x1,x2,y1,y2;
    get_slider2_coordinates(x1,x2,y1,y2);
    _output_slider = SLIDERPAINTER_create(this,x1,x2,y1,y2);
    SLIDERPAINTER_set_num_channels(_output_slider, _num_outputs);
  }
  
  if (has_output_slider())
    SLIDERPAINTER_set_peak_value_pointers(_output_slider, _num_outputs, plugin->output_volume_peaks.decaying_dbs, true);
  //ATOMIC_SET(plugin->volume_peak_values_for_chip, SLIDERPAINTER_obtain_peak_value_pointers(_output_slider,_num_outputs));

  if (has_input_slider())
    SLIDERPAINTER_set_peak_value_pointers(_input_slider, _num_inputs, plugin->input_volume_peaks.decaying_dbs, true);
  //ATOMIC_SET(plugin->input_volume_peak_values_for_chip, SLIDERPAINTER_obtain_peak_value_pointers(_input_slider,_num_inputs));
  
  CHIP_update(plugin);
}

Chip::Chip(QGraphicsScene *scene, SoundProducer *sound_producer, float x, float y)
  : _scene(scene)
  , _sound_producer(sound_producer)
  , _color("white")
  , _input_slider(NULL)
  , _output_slider(NULL)
  , _last_updated_mute(false)
  , _last_updated_bypass(false)
  , _last_updated_recording(false)
  , _last_updated_autosuspending(false)
  , _slider_being_edited(0)
{
   bool is_visible = true;
   
   struct Patch *patch = CHIP_get_patch(this);
   if (!patch)
     R_ASSERT(false);
   else if (!patch->is_visible)
     is_visible = false;

   if (!is_visible) {
     setVisible(false);
     x = 100000;
     y = 100000;
   }


   setPos(QPointF(x,y));
     
   //MW_move_chip_to_slot(this, x, y); // unfortunately, this function very often moves the chip to the right unnecessarily.

   scene->addItem(this);
   
 #if 0
   _line_item = new QGraphicsLineItem(0,0,50,50);
   _line_item->setPen(QPen(Qt::black, 2));
   _line_item->setPos(QPointF(x+50,y+50));
   scene->addItem(_line_item);
 #endif

   setFlags(ItemIsSelectable | ItemIsMovable | ItemSendsGeometryChanges);
#ifdef USE_QT5
   setAcceptHoverEvents(true);
#else
   setAcceptsHoverEvents(true);
#endif
   setZValue(10);
   
   init_new_plugin();

   if (is_visible)
     MW_set_selected_chip(this); // To unselect previously selected chip.

   if (!is_visible)
     setVisible(false);

   printf("New Chip. Inputs: %d, Ouptuts: %d\n",_num_inputs,_num_outputs);
}

void CHIP_autopos(Chip *chip){
  double x,y;
  MW_set_autopos(&x, &y);
  MW_move_chip_to_slot(chip, x, y);
}

void CHIP_autopos(struct Patch *patch){
  CHIP_autopos(CHIP_get(get_scene(g_mixer_widget), patch));
}

Chip *CHIP_create(SoundProducer *sound_producer, float x, float y){
  return new Chip(get_scene(g_mixer_widget), sound_producer, x, y);
}

Chip::~Chip(){

  remakeMixerStrips(CHIP_get_patch(this)->id);
    
  while(_input_audio_connections.size()>0){
    fprintf(stderr,"Deleting input connection. Connections left: %d\n",(int)_input_audio_connections.size());
    CONNECTION_delete_audio_connection(_input_audio_connections[0]);
  }
  while(_output_audio_connections.size()>0){
    fprintf(stderr,"Deleting output_connection. Connections left: %d\n",(int)_output_audio_connections.size());
    CONNECTION_delete_audio_connection(_output_audio_connections[0]);
  }
  
  while(_input_event_connections.size()>0){
    fprintf(stderr,"Deleting input econnection. EConnections left: %d\n",(int)_input_event_connections.size());
    CONNECTION_delete_event_connection(_input_event_connections[0]);
  }
  while(_output_event_connections.size()>0){
    fprintf(stderr,"Deleting output econnection. EConnections left: %d\n",(int)_output_event_connections.size());
    CONNECTION_delete_event_connection(_output_event_connections[0]);
  }

  SLIDERPAINTER_delete(_input_slider);
  SLIDERPAINTER_delete(_output_slider);
}

void CHIP_delete(Patch *patch){
  Chip *chip = CHIP_get(get_scene(g_mixer_widget), patch);
  printf("     Deleting chip %p (%s)\n",chip,CHIP_get_patch(chip)->name);
  delete chip;
}

static bool is_bus_provider(Chip *chip, const QVector<SoundProducer*> &buses){
  for(SoundProducer *bus : buses){
    if(bus==NULL){
      if (!g_is_loading) // Happens when loading older songs that doesn't have all buses.
        R_ASSERT(false);
      continue;
    }
    Chip *bus_chip = CHIP_get(get_scene(g_mixer_widget), const_cast<struct Patch*>(SP_get_plugin(bus)->patch));
    if (CONNECTION_are_connected_somehow(bus_chip, chip))
      return false;
  }

  return true;
}

// Used when loading old songs.
void CHIP_create_bus_connections(Chip *chip, Buses &dasbuses){
  int num_outputs = chip->_num_outputs;
  
  if (num_outputs<=0){
    R_ASSERT(num_outputs==0);
    return;
  }
  
  SoundPlugin *chip_plugin = SP_get_plugin(chip->_sound_producer);

  if (chip_plugin->bus_on_off==NULL)
    return;

  bool bus_on_off[NUM_BUSES];
  memcpy(bus_on_off, chip_plugin->bus_on_off, sizeof(bool)*NUM_BUSES);
  
  V_free(chip_plugin->bus_on_off);
  chip_plugin->bus_on_off = NULL;

  int bus_num = SP_get_bus_num(chip->_sound_producer);
  if (bus_num >= 0)
    return;

  QVector<SoundProducer*> buses;
  buses.push_back(dasbuses.bus1);
  buses.push_back(dasbuses.bus2);
  buses.push_back(dasbuses.bus3);
  buses.push_back(dasbuses.bus4);
  buses.push_back(dasbuses.bus5);

  QGraphicsScene *scene = get_scene(g_mixer_widget);
  
  for(int i=0;i<NUM_BUSES;i++){
    
    if (bus_on_off[i] && is_bus_provider(chip, buses)) {
      
      SoundProducer *bus = buses.at(i);
      SoundPlugin *bus_plugin = SP_get_plugin(bus);
      
      CHIP_connect_chips(scene, chip_plugin, bus_plugin, ConnectionType::IS_SEND);

    }
    
  }
}


QRectF Chip::boundingRect() const
{
  return QRectF(0, 0, chip_width, chip_height);
}

QPainterPath Chip::shape() const
{
  QPainterPath path;
  path.addRect(0, 0, chip_width, chip_height);
  return path;
}

/*
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

    painter->setBrush(Qt::NoBrush);
}
*/

bool SuperConnection::should_be_visible(void) const {
  //R_ASSERT_NON_RELEASE(to!=NULL); (happens when creating new connection)
  
  if (to==NULL || _is_event_connection)
    return true;

  if (to->is_bus())
    return MW_get_bus_connections_visibility();
  else
    return MW_get_connections_visibility();
}

bool Chip::is_bus(void) const {
  const SoundPlugin *plugin = SP_get_plugin(_sound_producer);
  if (plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }
  
  const struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, false);

  if (patch->id==get_main_pipe_patch_id())
    return false;

  return !strcmp(plugin->type->type_name, "Bus");
  //return instrumentIsSeqtrackBus(patch->id);
}

bool Chip::input_audio_port_is_operational(void) const {
  
  if(_num_inputs==0)
    return false;
  
  if (MW_get_bus_connections_visibility())
    return true;

  return !is_bus();
}

void Chip::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
  Q_UNUSED(widget);
  
  SoundPlugin *plugin = SP_get_plugin(_sound_producer);
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
  
  const struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  bool is_selected = (option->state & QStyle::State_Selected);
  /*
    bool is_over = (option->state & QStyle::State_MouseOver);
  */

  bool is_current_patch = get_current_instruments_gui_patch()==patch;
  bool is_current_patch_under_mouse = patch->id.id == getCurrentInstrumentUnderMouse().id;

  QColor text_color = get_qcolor(is_current_patch ? SEQUENCER_TEXT_CURRENT_BLOCK_COLOR_NUM : MIXER_TEXT_COLOR_NUM);
  //if(is_current_patch==false)
  text_color.setAlpha(160);

  QColor patchcolor = get_displayed_instrument_color(patch);

  patchcolor = patchcolor.lighter(120);
  /*
    (false && is_selected)
    ? mix_colors(QColor(30,25,70,60), patchcolor, 0.45)
    : mix_colors(QColor(30,65,70,35), patchcolor, 0.05);
  */
  
  if (ATOMIC_GET(patch->is_recording))
    patchcolor = mix_colors(patchcolor, QColor(255,0,0), 0.1);



  QColor border_color = get_qcolor(MIXER_BORDER_COLOR_NUM);
  if(is_current_patch_under_mouse==false)
    border_color.setAlpha(160);

  painter->setPen(QPen(border_color, 2));

  QFont font = g_editor->main_window->font();
  font.setPointSize(10);
  font.setPixelSize(10);

  painter->setFont(font);

  // slider
  {
    int x1,y1,x2,y2;
    get_slider2_coordinates(x1,y1,x2,y2);      

    /*
    {
      QColor c = get_qcolor(MIXER_BORDER_COLOR_NUM);
      c.setAlpha(10);
      painter->setBrush(QBrush(c,Qt::SolidPattern));
      painter->setPen(Qt::NoPen);
      painter->drawRoundedRect(x1,y1,x2-x1,y2-y1, 2,2);
      painter->setBrush(Qt::NoBrush);
    }
    */

    QRectF slider_rect(x1,y1,x2-x1,y2-y1);
    
    myFillRect(*painter, slider_rect, Qt::black);

    bool is_input = false;
    
    // input slider
    if(has_input_slider()){
      //SLIDERPAINTER_set_string(_input_slider, buf);
      painter->translate(x1,y1);
      SLIDERPAINTER_paint(_input_slider, painter);
      painter->translate(-x1,-y1);
      is_input = true;
    }

    // output slider (not painted if input slider is painted)
    if(has_output_slider()){
      //SLIDERPAINTER_set_string(_output_slider, buf);
      painter->translate(x1,y1);
      SLIDERPAINTER_paint(_output_slider, painter);
      painter->translate(-x1,-y1);
    }

    int effect_num = get_volume_effect_num();
    
    char buf[64]={};
    PLUGIN_get_display_value_string(plugin, effect_num, buf, 64);

    QColor col = get_qcolor(TEXT_COLOR_NUM);
    col.setAlphaF((is_current_patch || is_input) ? 0.9 : 0.6);
    painter->setPen(col);
    myDrawText(*painter, slider_rect.adjusted(4,3,0,0), buf, Qt::AlignLeft|Qt::AlignVCenter, false, 0,
               false,//true,
               true//false
               );
  }


  //QFont font = g_editor->main_window->font();
  font.setPointSize(12);
  font.setPixelSize(12);

  painter->setFont(font);

  // Draw text
  {
    int x1,y1,x2,y2;
    get_name_coordinates(x1,y1,x2,y2);

    myFillRect(*painter, QRectF(chip_box_x1, y1, x2-chip_box_x1, y2-y1), _hover_item==HoverItem::NAME ? patchcolor.lighter(150) : patchcolor); // Background color of note indicator.
    
    //myFillRect(*painter, QRectF(x1, y1, x2-x1, y2-y1), _hover_item==HoverItem::NAME ? patchcolor.lighter(150) : patchcolor);
    /*
    painter->setPen(Qt::NoPen);
    painter->setBrush(c);
    painter->drawRoundedRect(x1, y1, x2-x1, y2-y1, 2,2);
    painter->setBrush(Qt::NoBrush);
    */
    x1 += 2;    
    x2 -= 1;
    
    if (ATOMIC_GET(g_show_cpu_usage_in_mixer)){
      
      CpuUsage *cpu_usage = (CpuUsage*)ATOMIC_GET_RELAXED(plugin->cpu_usage);

      if (cpu_usage==NULL){
        cpu_usage = new CpuUsage;
        ATOMIC_SET(plugin->cpu_usage, cpu_usage);
      }

      cpu_usage->maybe_update(_name_text);
      
    } else  {
      
      _name_text = patch->name;
      
    }

    //printf("updating %d\n",(int)::scale(patch->visual_note_intencity, MAX_NOTE_INTENCITY, 0, 150, 100));
    painter->setPen(QPen(text_color, 2));

    /*
    
    float textlen = get_text_width(painter->font(),_name_text);       
    float width = x2-x1;

    if(textlen>=width){
      float s = width/textlen;
      painter->save();
      painter->scale(s,1.0);
      painter->drawText(x1/s, y1, width/s, y2-y1, Qt::AlignLeft, _name_text);
      painter->restore();
    }else{
      painter->drawText(x1, y1, width, y2-y1, Qt::AlignLeft, _name_text);
    }
    */

    QRectF rect(x1, y1, x2-x1, y2-y1);
    //float reduce_x = (rect.width() / g_gfx_scale) / 2.0;
    //float reduce_y = (rect.height() / g_gfx_scale) / 2.0;
      
    myDrawText(*painter,
               //rect.adjusted(reduce_x,reduce_y,-reduce_x,-reduce_y),
               rect,
               _name_text,
               Qt::AlignLeft|Qt::AlignVCenter,
               false, // wrap
               0, // rotate
               false,//true, // scale
               true // cut_text_to_fit
               );
        
    // checbuttons.
    {

      font.setPointSize(10);
      font.setPixelSize(10);

      painter->setFont(font);

      int x1,y1,x2,y2;
      get_volume_onoff_coordinates(x1,y1,x2,y2);


      paint_checkbutton(painter, "M", patchcolor, text_color, Qt::green, x1,y1,x2,y2, is_muted_relaxed(plugin), plugin->is_implicitly_muted, _hover_item==HoverItem::MUTE_BUTTON);

      get_solo_onoff_coordinates(x1,y1,x2,y2);
      paint_checkbutton(painter, "S", patchcolor, text_color, Qt::yellow, x1,y1,x2,y2, ATOMIC_GET_RELAXED(plugin->solo_is_on), plugin->is_implicitly_soloed, _hover_item==HoverItem::SOLO_BUTTON);

      get_effects_onoff_coordinates(x1,y1,x2,y2);
      paint_checkbutton(painter, "B", patchcolor, text_color, get_qcolor(ZOOMLINE_TEXT_COLOR_NUM1), x1,y1,x2,y2, !ATOMIC_GET_RELAXED(plugin->effects_are_on), false, _hover_item==HoverItem::BYPASS_BUTTON);
    }
  }


  // Draw note indicator
  if(1){
    int x1,y1,x2,y2;
    get_note_indicator_coordinates(x1,y1,x2,y2);

    if (_hover_item==HoverItem::NAME)
      myFillRect(*painter, QRectF(x1, y1, x2-x1, y2-y1), patchcolor);
    
    QColor border_color = get_qcolor(NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM);
    
    int intencity = ATOMIC_GET_RELAXED(patch->visual_note_intencity);
    
    if(intencity>0){      
      //c = mix_colors(c,QColor(168,35,35),::scale(patch->visual_note_intencity, MAX_NOTE_INTENCITY, 0, 0, 1));
      //QColor c = mix_colors(background_color,g_editor->colors[12],::scale(patch->visual_note_intencity, MAX_NOTE_INTENCITY, 0, 0, 1));
      QColor c = get_qcolor(NOTE_EVENT_INDICATOR_COLOR_NUM);
      
      if (_hover_item==HoverItem::EVENT_PORT)
        c = c.lighter(150);
        
      c.setAlphaF(::scale(intencity, 0, MAX_NOTE_INTENCITY, 0.0, 1.0));
      //painter->setPen(c);
      painter->setBrush(QBrush(c,Qt::SolidPattern));
      painter->setPen(Qt::NoPen);
      painter->drawRoundedRect(x1,y1,x2-x1,y2-y1,0.5,0.5);
      painter->setBrush(Qt::NoBrush);
      
    } else {

      if (_hover_item==HoverItem::EVENT_PORT){
        painter->setBrush(patchcolor.lighter(150));
        border_color = border_color.lighter(150);
      }
      
    }
    
    //painter->setBrush(QBrush());
    painter->setPen(border_color);
    //painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(x1,y1,x2-x1,y2-y1,0.5,0.5);

    if (intencity==0 && _hover_item==HoverItem::EVENT_PORT)
      painter->setBrush(Qt::NoBrush);
  }


  // main box
  {
    int x1,y1,x2,y2;
    get_coordinates(x1,y1,x2,y2);

    //    if(is_current_patch==true){
      //}
    if (is_current_patch_under_mouse || is_selected){
      
      QColor c = is_current_patch_under_mouse
        ? (is_selected ? mix_colors(get_qcolor(MIXER_CURRENT_OBJECT_BORDER_COLOR_NUM), get_qcolor(MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM), 0.5) : get_qcolor(MIXER_CURRENT_OBJECT_BORDER_COLOR_NUM))
        : get_qcolor(MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM);
      
      painter->setPen(QPen(c, 3));
      
      painter->drawRoundedRect(QRectF(x1-1, y1-1,
                                      x2-x1+2, y2-y1+2),
                               1,1);

    } else {
      
      painter->setPen(QPen(border_color, 1));
      painter->drawRoundedRect(x1,y1,x2-x1,y2-y1,1,1);
      
    }


    //painter->fillRect(x1,y1,x2-x1,y2-y1);

    
    // Line between slider/buttons and name.
    {
      painter->setPen(QPen(border_color, 0.5));
      painter->drawLine(x1, y2-name_height, x2, y2-name_height);
    }

    /*
    // Greying if selected
    if (false && is_selected){// && !is_current_patch){
      QColor c = get_qcolor(MIXER_SELECTED_OBJECT_COLOR_NUM); //(40,40,40,100);
      painter->setPen(Qt::NoPen);
      painter->setBrush(c);      
      painter->drawRoundedRect(x1,y1,x2-x1,y2-y1,1,1);
      painter->setBrush(Qt::NoBrush);
    }
    */
    
    // Bluing if autosuspending
    if (_last_updated_autosuspending) { //SP_is_autosuspending(plugin->sp)){
      //printf("hepp. autosusp. %s\n", CHIP_get_patch(this)->name);
      //QColor c(0,0,60,80);
      QColor c = get_qcolor(MIXER_AUTOSUSPENSION_COLOR_NUM);
      //c.setAlpha(35);
      painter->setPen(Qt::NoPen);
      painter->setBrush(c);
      painter->drawRoundedRect(x1,y1,x2-x1,y2-y1,1,1);
      painter->setBrush(Qt::NoBrush);
    }
  }



  //printf("Paint Chip. Inputs: %d, Ouptuts: %d\n",_num_inputs,_num_outputs);

  // Ports
  {
    int x1,y1,x2,y2;
    get_coordinates(x1,y1,x2,y2);

    //painter->setPen(QPen(Qt::gray, 2));
    //QColor color(59,155,68,40);
    QColor color = get_qcolor(MIXER_AUDIO_PORT_COLOR_NUM);

    QColor border_color = color.darker();
    //painter->setPen(color);
    painter->setPen(border_color);

    QColor fill_color = color.lighter();
    painter->setBrush(QBrush(fill_color,Qt::SolidPattern));

    const int xborder = 5;
    const int yborder = 5;

    if(input_audio_port_is_operational()) { 
      
      if (_hover_item==HoverItem::AUDIO_INPUT_PORT){
        QColor col = fill_color;
        col.setAlphaF(R_MIN(1.0, col.alphaF()+0.3));
        painter->setBrush(col.lighter(150));
        painter->setPen(col.darker(150));
        //printf("Set input\n");
      }
      
      painter->drawRoundedRect(xborder, y1+yborder,
                               port_width-2-xborder, y2-y1-yborder*2,
                               2,2);
      
      if (_hover_item==HoverItem::AUDIO_INPUT_PORT){
        painter->setBrush(QBrush(fill_color,Qt::SolidPattern));
        painter->setPen(border_color);
      }
    }

    if(_num_outputs>0) {

      if (_hover_item==HoverItem::AUDIO_OUTPUT_PORT){
        QColor col = fill_color;
        col.setAlphaF(R_MIN(1.0, col.alphaF()+0.3));
        painter->setBrush(col.lighter(150));
        painter->setPen(col.darker(150));
        //printf("Set output\n");
      }
      
      painter->drawRoundedRect(x2+2, y1+yborder,
                               port_width-2-xborder, y2-y1-yborder*2,
                               2,2);
    }
    
    painter->setBrush(Qt::NoBrush);
  }
}

bool Chip::myMouseDoubleClickEvent (float x, float y) {

  int x1,y1,x2,y2;
  get_name_coordinates(x1,y1,x2,y2);
  
  //printf("I'm double clicked! %f,%f (%d,%d -> %d, %d)\n",x,y,x1,y1,x2,y2);

  if(x>x1 && x<x2 && y>y1 && y<y2){
    struct Patch *patch = CHIP_get_patch(this);
    int64_t gui = API_get_gui_from_existing_widget(get_qwidget(g_mixer_widget));
    return showInstrumentGui(patch->id, gui, showInstrumentWidgetWhenDoubleClickingSoundObject());
  }

  //QGraphicsItem::mouseDoubleClickEvent(event);

  return false;
}

bool Chip::positionedAtSlider(QPointF pos){
  SoundPlugin *plugin = SP_get_plugin(_sound_producer);

  for(int i=0;i<2;i++){
    int x1,y1,x2,y2;
    if(i==0)
      get_slider1_coordinates(x1,y1,x2,y2);      
    else
      get_slider2_coordinates(x1,y1,x2,y2);      

    //printf("%d - %f - %d,    %d - %f - %d\n",x1,pos.x(),x2,y1,pos.y(),y2);

    if(pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2){

      if(i==0 && plugin->type->num_inputs==0)
        continue;
      if(i==1 && plugin->type->num_outputs==0)
        continue;

      return true;
    }
  }

  return false;
}

static bool in_solo_button(const QPointF &pos){
  int x1,y1,x2,y2;
  get_solo_onoff_coordinates(x1,y1,x2,y2);

  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;
}

static bool in_volume_button(const QPointF &pos){
  int x1,y1,x2,y2;
  get_volume_onoff_coordinates(x1,y1,x2,y2);

  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;
}

static bool in_bypass_button(const QPointF &pos){
  int x1,y1,x2,y2;
  get_effects_onoff_coordinates(x1,y1,x2,y2);

  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;
}

static bool in_slider(const QPointF &pos){
  int x1,y1,x2,y2;
  get_slider1_coordinates(x1,y1,x2,y2);      

  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;     
}

static bool in_name(const QPointF &pos){
  int x1,y1,x2,y2;
  get_name_coordinates(x1,y1,x2,y2);      

  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;     
}

static bool in_note_indicator(const QPointF &pos){
  int x1,y1,x2,y2;
  get_note_indicator_coordinates(x1,y1,x2,y2);
  x1-=2;
  y1-=2;
  x2+=2;
  y2+=2;
  
  return pos.x()>x1 && pos.x()<x2 && pos.y()>y1 && pos.y()<y2;     
}

static int64_t g_statusbar_id = -1;

static void set_solo_statusbar(const Chip *chip){
  g_statusbar_id = S7CALL2(int_instrument, "FROM_C-display-solo-status-in-statusbar", CHIP_get_patch(chip)->id);
}

static void set_mute_statusbar(const Chip *chip){
  g_statusbar_id = S7CALL2(int_instrument, "FROM_C-display-mute-status-in-statusbar", CHIP_get_patch(chip)->id);
}

static void set_bypass_statusbar(const Chip *chip){
  g_statusbar_id = S7CALL2(int_instrument, "FROM_C-display-bypass-status-in-statusbar", CHIP_get_patch(chip)->id);
}

static void set_slider_statusbar(const Chip *chip){
  SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  const struct Patch *patch = const_cast<const struct Patch*>(plugin->patch);
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  int effect_num = chip->get_volume_effect_num();
  
  char temp[64] = {};
  PLUGIN_get_display_value_string(plugin, effect_num, temp, 62);
  
  g_statusbar_id = setStatusbarText(talloc_format("%s: %s", patch->name, temp));
}

static void set_name_statusbar(const Chip *chip){
  SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  const struct Patch *patch = const_cast<const struct Patch*>(plugin->patch);
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  g_statusbar_id = setStatusbarText(patch->name);
}

void Chip::set_hover_item(HoverItem hover_item){
  if (_hover_item != hover_item){
    
    if (_hover_item==HoverItem::VOLUME_SLIDER || hover_item==HoverItem::VOLUME_SLIDER)        
      SLIDERPAINTER_set_hovered(has_input_slider() ? _input_slider : _output_slider, hover_item==HoverItem::VOLUME_SLIDER);
    
    _hover_item = hover_item;
    update();
  }
}

static void handle_mouse_hover_chip(Chip *chip, const QGraphicsSceneHoverEvent * event ){
  const QPointF pos = event->pos();
  
  if (in_solo_button(pos)){

    chip->set_hover_item(Chip::HoverItem::SOLO_BUTTON);
    set_solo_statusbar(chip);
    
  } else if (in_volume_button(pos)){

    chip->set_hover_item(Chip::HoverItem::MUTE_BUTTON);
    set_mute_statusbar(chip);
        
  } else if (in_bypass_button(pos)){

    chip->set_hover_item(Chip::HoverItem::BYPASS_BUTTON);
    set_bypass_statusbar(chip);
    
  } else if(in_slider(pos)){

    chip->set_hover_item(Chip::HoverItem::VOLUME_SLIDER);
    set_slider_statusbar(chip);
    
  } else if(in_note_indicator(pos)){

    chip->set_hover_item(Chip::HoverItem::EVENT_PORT);    
    g_statusbar_id = setStatusbarText("Press mouse button to create new event-connection");
    
  } else if(in_name(pos)){

    chip->set_hover_item(Chip::HoverItem::NAME);
    set_name_statusbar(chip);
    
  } else if (pos.x() < chip_box_x1 || pos.x() >= chip_box_x2) {

    chip->set_hover_item(pos.x() < chip_box_x1 ? Chip::HoverItem::AUDIO_INPUT_PORT : Chip::HoverItem::AUDIO_OUTPUT_PORT);
    g_statusbar_id = setStatusbarText("Press mouse button to create new audio-connection");

  } else {

    chip->set_hover_item(Chip::HoverItem::NAME);
    set_name_statusbar(chip);
    
  }

  API_set_mousePointerCurrentlyPointsAtInstrument(true);

  setCurrentInstrumentUnderMouse(CHIP_get_patch(chip)->id);
}

void Chip::hoverEnterEvent ( QGraphicsSceneHoverEvent * event ){
  handle_mouse_hover_chip(this, event);
}

void Chip::hoverMoveEvent ( QGraphicsSceneHoverEvent * event ){
  handle_mouse_hover_chip(this, event);
}

void Chip::hoverLeaveEvent ( QGraphicsSceneHoverEvent * event ){
  API_set_mousePointerCurrentlyPointsAtInstrument(false);
  
  if (g_statusbar_id >= 0){
    
    removeStatusbarText(g_statusbar_id);
    g_statusbar_id=-1;
    
  }

  set_hover_item(Chip::HoverItem::NOTHING);
}

void Chip::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();
  //FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);

  _mouse_has_moved = false;
  
  bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
    
  if(event->button()==Qt::LeftButton){

    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    //int num_effects = plugin->type->num_effects;

    struct Patch *patch = (struct Patch*)plugin->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    QPointF pos = event->pos();

    //printf("Pressed. %f / %f\n",pos.x(),pos.y());
    struct Instruments *instrument = get_audio_instrument();
    instrument->PP_Update(instrument,(struct Patch*)patch,false);

    // solo onoff
    if(in_solo_button(pos)){
  
      bool solo_is_on = ATOMIC_GET(plugin->solo_is_on);
      
      //printf("Setting volume_is_on. Before: %d. After: %d\n",plugin->volume_is_on, !plugin->volume_is_on);
      //float new_value = solo_is_on?0.0f:1.0f;
      
      {
        radium::ScopedUndo scoped_undo;
        
        // Turn off all other solos if ctrl is pressed.
        if (ctrl_pressed){
          vector_t *patches = &get_audio_instrument()->patches;
          for(int i=0;i<patches->num_elements;i++){
            struct Patch *thispatch = (struct Patch*)patches->elements[i];
            SoundPlugin *plugin = (SoundPlugin*)thispatch->patchdata;
            if (thispatch != patch && plugin!=NULL && ATOMIC_GET(plugin->solo_is_on)) {
              int num_effects = plugin->type->num_effects;
              ADD_UNDO(AudioEffect_CurrPos(thispatch, num_effects+EFFNUM_SOLO_ONOFF, AE_NO_FLAGS));
              PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_SOLO_ONOFF, 0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
              //CHIP_update(plugin);
            }
          }
        }

        if (instrumentIsSelected(patch->id))
          setSoloForInstruments(getSelectedInstruments(), !solo_is_on);
        else
          setInstrumentSolo(!solo_is_on, patch->id);
        
        /*
        //ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, num_effects+EFFNUM_SOLO_ONOFF, AE_NOT_ALWAYS_CREATE_SOLO_AND_BYPASS_UNDO));
        
        PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_SOLO_ONOFF, new_value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
        //CHIP_update(plugin);
        //GFX_update_instrument_widget((struct Patch*)patch);
        
        */
        
      }

      set_solo_statusbar(this);
                       
      event->accept();
      return;
    }

    // volume onoff
    if(in_volume_button(pos)){

      bool is_on = !is_muted_relaxed(plugin);

      //printf("Setting volume_is_on. Before: %d. After: %d\n",plugin->volume_is_on, !plugin->volume_is_on);
      //float new_value = is_on?0.0f:1.0f;
      
      UNDO_OPEN();{
        
        // Turn off all other mutes if ctrl is pressed.
        if (ctrl_pressed){
          vector_t *patches = &get_audio_instrument()->patches;
          for(int i=0;i<patches->num_elements;i++){
            struct Patch *thispatch = (struct Patch*)patches->elements[i];
            SoundPlugin *plugin = (SoundPlugin*)thispatch->patchdata;
            if (thispatch != patch && plugin!=NULL && is_muted_relaxed(plugin)){
              int effect_num = get_mute_effectnum(plugin->type);
              ADD_UNDO(AudioEffect_CurrPos(thispatch, effect_num, AE_NO_FLAGS));
              PLUGIN_set_effect_value(plugin, -1, effect_num, 1, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
              //CHIP_update(plugin);
            }
          }
        }

        if (instrumentIsSelected(patch->id))
          setMuteForInstruments(getSelectedInstruments(), is_on);
        else
          setInstrumentMute(is_on, patch->id);

      }UNDO_CLOSE();

      set_mute_statusbar(this);
      
      event->accept();
      return;
    }

    // effects onoff (i.e. bypass)
    if(in_bypass_button(pos)){
        
      bool effects_are_on = ATOMIC_GET(plugin->effects_are_on);
      //float new_value = effects_are_on?0.0f:1.0f;
      
      {
        radium::ScopedUndo scoped_undo;
        
        // Turn off all other bypasses if ctrl is pressed.
        if (ctrl_pressed){
          vector_t *patches = &get_audio_instrument()->patches;
          for(int i=0;i<patches->num_elements;i++){
            struct Patch *thispatch = (struct Patch*)patches->elements[i];
            SoundPlugin *plugin = (SoundPlugin*)thispatch->patchdata;
            if (thispatch != patch && plugin!=NULL && is_bypassed(plugin)) {
              int num_effects = plugin->type->num_effects;
              ADD_UNDO(AudioEffect_CurrPos(thispatch, num_effects+EFFNUM_EFFECTS_ONOFF, AE_NO_FLAGS));
              PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, 1, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
              CHIP_update(plugin);
            }
          }
        }

        if (instrumentIsSelected(patch->id))
          setBypassForInstruments(getSelectedInstruments(), effects_are_on);
        else
          setInstrumentBypass(effects_are_on, patch->id);
      }

      set_bypass_statusbar(this);
      
      event->accept();
      return;
    }

    if (in_slider(pos)) {

      for(int i=0;i<2;i++){
        
        if (i==0 && !has_input_slider())
          continue;
        
        if (i==1 && !has_output_slider())
          continue;
        
        
        _has_made_volume_effect_undo=false;
        //ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, effect_num, AE_NO_FLAGS));
        
        vector_t chips = MW_get_selected_chips();
        if (VECTOR_is_in_vector(&chips, this)==false){
          VECTOR_clean(&chips);
          VECTOR_push_back(&chips, this);
        }

        VECTOR_FOR_EACH(Chip*,chip, &chips){
          float max_gain = ::scale(MAX_VOLUME_SLIDER_DB, MIN_DB, MAX_DB, 0, 1);
          float val = R_BOUNDARIES(0,
                                   ::scale(chip->get_slider_volume(), 0, max_gain, 0, 1),
                                   1);
          chip->_slider_start_value = val*val;
        }END_VECTOR_FOR_EACH;

        _slider_start_pos = pos.x();
        /*
        float value = ::scale(pos.x(),x1,x2,0,1.0);
        PLUGIN_set_effect_value(plugin, -1, effect_num, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);

        CHIP_update(plugin);

        GFX_update_instrument_widget((struct Patch*)patch);
        */
        _slider_being_edited = i+1;

        //printf("          RETURNING\n");
        event->accept();
        return;
      }
    }

    /*
      This stuff is handled in handle_chip_selection in QM_MixerWidget.cpp.

    // no slider or button selected.
    //
    if(event->modifiers() & Qt::ControlModifier) {
      printf(" *********** Setting selected to %d\n", !isSelected());
      setSelected(!isSelected());
    } else {
      MW_set_selected_chip(this); //i.e. only set this one as the selected.
    }
    */

  }

  event->accept();
   
  //QGraphicsItem::mousePressEvent(event);
}

QVariant Chip::itemChange(GraphicsItemChange change, const QVariant &value) {
  if (change == ItemPositionHasChanged && this->scene()) {
    for (AudioConnection *connection : _input_audio_connections)
      connection->update_position();
    for (AudioConnection *connection : _output_audio_connections)
      connection->update_position();
    for (EventConnection *connection : _input_event_connections)
      connection->update_position();
    for (EventConnection *connection : _output_event_connections)
      connection->update_position();
    //printf("item pos changed\n");
  }

  /*
  if (QGraphicsItem::ItemSelectedHasChanged==change){
    remakeMixerStrips(-1);
  }
  */

  return QGraphicsItem::itemChange(change, value);
}


void Chip::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();

  _mouse_has_moved = true;
  
  if (_slider_being_edited != 0){

    R_ASSERT_RETURN_IF_FALSE(_slider_being_edited == 1 || _slider_being_edited == 2);
    
    QPointF pos = event->pos();

    int x1,y1,x2,y2;
    if(_slider_being_edited==0)
      get_slider1_coordinates(x1,y1,x2,y2);      
    else
      get_slider2_coordinates(x1,y1,x2,y2);      

    bool ctrl_pressed = (event->modifiers() & Qt::ControlModifier);
    
    float delta = pos.x() - _slider_start_pos;
    if (ctrl_pressed)
      delta /= 10.0;
    
    _slider_start_pos = pos.x();

    SoundPlugin *plugin = SP_get_plugin(_sound_producer);
    struct Patch *patch = (struct Patch*)plugin->patch;
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

    vector_t chips = MW_get_selected_chips();
    if (VECTOR_is_in_vector(&chips, this)==false){
      VECTOR_clean(&chips);
      VECTOR_push_back(&chips, this);
    }

    if (_has_made_volume_effect_undo==false)
      UNDO_OPEN();

    VECTOR_FOR_EACH(Chip*,chip, &chips){
      
      struct Patch *patch = CHIP_get_patch(chip);

      if (patch != NULL){
        
        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        
        if (plugin != NULL){
          float value = chip->_slider_start_value + ::scale(delta,
                                                            0,x2-x1,
                                                            0,1.0);
          
          if(value>1)
            value=1;
          if(value<0.0)
            value=0.0;
          
          chip->_slider_start_value = value;

          float max_gain = ::scale(MAX_VOLUME_SLIDER_DB, MIN_DB, MAX_DB, 0, 1);

          value = ::scale(sqrtf(value),
                           0,1,
                           0,max_gain);
          
          int effect_num = chip->get_volume_effect_num();
          
          if (_has_made_volume_effect_undo==false)
            ADD_UNDO(AudioEffect_CurrPos(patch, effect_num, AE_NO_FLAGS));
          
          PLUGIN_set_effect_value(plugin, -1, effect_num, value, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
          
          CHIP_update(plugin);
        }
      }
      
    }END_VECTOR_FOR_EACH;

    if (_has_made_volume_effect_undo==false)
      UNDO_CLOSE();

    if (_has_made_volume_effect_undo==false)
      _has_made_volume_effect_undo=true;
      

    set_slider_statusbar(this);
    GFX_update_instrument_widget((struct Patch*)patch);
  }

  event->accept();
  return;

  //QGraphicsItem::mouseMoveEvent(event);
}

void Chip::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
  //RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();

  if(_slider_being_edited>0)
    _slider_being_edited=0;

#if 0
  // Commented out. When we are here, we have either just clicked the slider or the buttons.
  if (!_mouse_has_moved){
    struct Patch *patch = CHIP_get_patch(this);
    GFX_PP_Update_even_if_locked(patch, false);
  }
#endif
  
  event->accept();
}

void Chip::mySetSelected(bool selected) {
  for(auto audio_connection : _input_audio_connections)
    audio_connection->mySetSelected(selected);
  for(auto audio_connection : _output_audio_connections)
    audio_connection->mySetSelected(selected);
  
  for(auto event_connection : _input_event_connections)
    event_connection->mySetSelected(selected);
  for(auto event_connection : _output_event_connections)
    event_connection->mySetSelected(selected);
  
  QGraphicsItem::setSelected(selected);
  
  SoundPlugin *plugin = SP_get_plugin(_sound_producer);
  if (plugin!=NULL){
    plugin->is_selected = selected;

    if (plugin->patch == NULL || plugin->patch->is_visible)
      redrawMixerStrips(false);
    
    //remakeMixerStrips();
  }
}


struct Patch *CHIP_get_patch(const Chip *chip){
#if !defined(RELEASE)
  if(chip==NULL)
    abort();
#endif
  
  R_ASSERT_RETURN_IF_FALSE2(chip!=NULL, PATCH_get_current());
  
  const SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  struct Patch *patch = plugin->patch;
  R_ASSERT_RETURN_IF_FALSE2(patch!=NULL, PATCH_get_current());
  return patch;
}

Chip *CHIP_get(const QGraphicsScene *scene, const Patch *patch){
  R_ASSERT_RETURN_IF_FALSE2(patch != NULL, NULL);

  const SoundPlugin *plugin = (const SoundPlugin*)patch->patchdata;
  if(plugin==NULL){
    R_ASSERT_NON_RELEASE(false);
    return NULL;
  }
  
  const QList<QGraphicsItem *> &das_items = scene==NULL ? get_scene(g_mixer_widget)->items() : scene->items();
  for (int i = 0; i < das_items.size(); ++i){
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      if(plugin==SP_get_plugin(chip->_sound_producer))
        return chip;
    }
  }
  
  return NULL;
}

AudioConnection *CONNECTION_find_audio_connection(const Chip *from, const Chip *to){
  R_ASSERT_RETURN_IF_FALSE2(from!=NULL, NULL);
  R_ASSERT_RETURN_IF_FALSE2(to!=NULL, NULL);

  for(AudioConnection *connection : from->_output_audio_connections)
    if(connection->to==to)
      return connection;
  
  return NULL;
}

AudioConnection *CONNECTION_find_audio_connection(const struct Patch *from, const struct Patch *to){
  auto *scene = get_scene(g_mixer_widget);
  Chip *from_chip = CHIP_get(scene, from);
  Chip *to_chip = CHIP_get(scene, to);

  R_ASSERT_RETURN_IF_FALSE2(from_chip!=NULL, NULL);
  R_ASSERT_RETURN_IF_FALSE2(to_chip!=NULL, NULL);
  
  return CONNECTION_find_audio_connection(from_chip, to_chip);
}

static instrument_t get_saving_patch_id_or_index(struct Patch *patch, const vector_t *patches){
  if (patches==NULL)
    return patch->id;

  for(int pos=0;pos<patches->num_elements;pos++)
    if(patches->elements[pos]==patch)
      return make_instrument(pos); // chaotic code...

  RError("get_saving_patch_index: patch \"%s\" not found. patches length: %d",patch->name, patches->num_elements);
  
  return make_instrument(-1);
}

hash_t *CONNECTION_get_state(const SuperConnection *connection, const vector_t *patches){
  hash_t *state=HASH_create(4);

  struct Patch *from = CHIP_get_patch(connection->from);
  struct Patch *to = CHIP_get_patch(connection->to);

  R_ASSERT_RETURN_IF_FALSE2(from!=NULL, NULL);
  R_ASSERT_RETURN_IF_FALSE2(to!=NULL, NULL);
  
  HASH_put_instrument(state, "from_patch", get_saving_patch_id_or_index(from, patches));
  HASH_put_instrument(state, "to_patch",   get_saving_patch_id_or_index(to,   patches));
  HASH_put_bool(state, "is_event_connection", connection->_is_event_connection);
  if (!connection->_is_event_connection){
    HASH_put_float(state, "gain", getAudioConnectionGain(from->id, to->id, true));
    const AudioConnection *audio_connection = dynamic_cast<const AudioConnection *>(connection);
    if(audio_connection==NULL)
      R_ASSERT(false);
    else
      HASH_put_int(state, ":connection-type", get_int_from_connection_type(audio_connection->get_connection_type()));
  }
  HASH_put_bool(state, "enabled", connection->get_enabled());

  return state;
}



static void CONNECTION_create_from_state2(QGraphicsScene *scene, changes::AudioGraph &changes, const hash_t *state,
                                          instrument_t patch_id_old, instrument_t patch_id_new,
                                          instrument_t patch_id_old2, instrument_t patch_id_new2,
                                          bool include_audio, bool include_events, bool include_connection_gain,
                                          bool all_patches_are_always_supposed_to_be_here
                                          )
{
  instrument_t id_from = HASH_get_instrument(state, "from_patch");
  instrument_t id_to = HASH_get_instrument(state, "to_patch");

  if (id_from==patch_id_old)
    id_from = patch_id_new;
  
  if (id_to==patch_id_old)
    id_to = patch_id_new;
  
  if (id_from==patch_id_old2)
    id_from = patch_id_new2;
  
  if (id_to==patch_id_old2)
    id_to = patch_id_new2;
  
  Chip *from_chip = get_chip_from_patch_id(scene, id_from, all_patches_are_always_supposed_to_be_here);
  Chip *to_chip   = get_chip_from_patch_id(scene, id_to, all_patches_are_always_supposed_to_be_here);

  if(from_chip==NULL || to_chip==NULL) {

    // (get_chip_from_patch_id shows error.)
    //if (all_patches_are_always_supposed_to_be_here)
    //  RError("Could not find chip from patch id. %d: 0x%p, %d: 0x%p",HASH_get_instrument(state, "from_patch"),from_chip,HASH_get_instrument(state, "to_patch"),to_chip);
    
    return;
  }

  if(HASH_has_key(state, "is_event_connection") && HASH_get_bool(state, "is_event_connection")) { // .rad files before 1.9.31 did not have event connections.
    
    if (include_events)
      CHIP_econnect_chips(scene, from_chip, to_chip);
    
  } else if (include_audio || include_connection_gain) {
    
    float gain = -1.0;

    if (include_connection_gain && HASH_has_key(state, "gain"))
      gain = HASH_get_float(state, "gain");

    bool is_enabled = true;
    
    if (HASH_has_key(state, "enabled")){
      is_enabled = HASH_get_bool(state, "enabled");
    }

    ConnectionType connection_type = ConnectionType::NOT_SET;
    if (HASH_has_key(state, ":connection-type"))
      connection_type = get_connection_type_from_int(HASH_get_int32(state, ":connection-type"));

    bool doit = include_audio;
    
    if (!doit){
      
      R_ASSERT(!include_audio);
      
      R_ASSERT(include_connection_gain);
      
      doit = gain >= -0.001 && CHIPS_are_connected(from_chip, to_chip) && !equal_floats(gain, SP_get_link_gain(to_chip->_sound_producer, from_chip->_sound_producer, NULL));

      if (doit) {
        /*
        printf("---------------Gain %s->%s: %f / %f\n",
               CHIP_get_patch(from_chip)->name, CHIP_get_patch(to_chip)->name,
               gain, SP_get_link_gain(to_chip->_sound_producer, from_chip->_sound_producer, NULL)
               );
        */
        connection_type = ConnectionType::NOT_SET;
        ADD_UNDO(AudioConnectionGain_CurrPos(CHIP_get_patch(from_chip), CHIP_get_patch(to_chip))); // Only happens when changing mixer a/b, and then we are inside an undo block.
      }
    }

    if (doit)
      changes.add(from_chip, to_chip, gain, radium::get_enable_type_from_bool(is_enabled), connection_type);
  }
}
  
static void CONNECTIONS_create_from_state2(QGraphicsScene *scene, changes::AudioGraph &changes, const hash_t *connections,
                                           instrument_t patch_id_old, instrument_t patch_id_new,
                                           instrument_t patch_id_old2, instrument_t patch_id_new2,
                                           bool include_audio, bool include_events, bool include_connection_gain,
                                           bool all_patches_are_always_supposed_to_be_here
                                           )
{
  int num_connections = HASH_get_int32(connections, "num_connections");
  for(int i=0;i<num_connections;i++){
    const hash_t *state = HASH_get_hash_at(connections, "", i);
    CONNECTION_create_from_state2(scene, changes, state,
                                  patch_id_old, patch_id_new,
                                  patch_id_old2, patch_id_new2,
                                  include_audio, include_events, include_connection_gain,
                                  all_patches_are_always_supposed_to_be_here);
  }
}

// Only used when loading song.
void CONNECTIONS_create_from_state(QGraphicsScene *scene, const hash_t *connections,
                                   instrument_t patch_id_old, instrument_t patch_id_new,
                                   instrument_t patch_id_old2, instrument_t patch_id_new2
                                   )
{
  changes::AudioGraph changes;
  CONNECTIONS_create_from_state2(scene, changes, connections, patch_id_old, patch_id_new, patch_id_old2, patch_id_new2, true, true, true, true);
  CONNECTIONS_apply_changes(scene, changes);

  if (HASH_has_key(connections, "modulator_connections"))
    MODULATORS_apply_connections_state(HASH_get_dyn(connections, "modulator_connections"));
}

/*
static void CONNECTION_create_from_state(QGraphicsScene *scene, hash_t *state, instrument_t patch_id_old, instrument_t patch_id_new){
  changes::AudioGraph changes;

  CONNECTION_create_from_state2(scene, changes, state, patch_id_old, patch_id_new, -1, -1, true);
  CONNECTIONS_apply_changes(scene, changes);
}
*/

void CONNECTIONS_replace_all_with_state(QGraphicsScene *scene, const hash_t *connections,
                                        bool include_audio, bool include_events, bool include_connection_gain, bool include_modulator_connections,
                                        bool all_patches_are_always_supposed_to_be_here,
                                        radium::Scheduled_RT_functions &rt_functions, radium::SoloChanges &solo_changes
                                        )
{
  if (include_audio || include_events || include_connection_gain){
    changes::AudioGraph changes(rt_functions, solo_changes);

    if (include_audio || include_events)
      CONNECTIONS_remove_all2(scene, include_audio, include_events, changes);
    
    CONNECTIONS_create_from_state2(scene, changes, connections,
                                   make_instrument(-1), make_instrument(-1), make_instrument(-1), make_instrument(-1),
                                   include_audio, include_events, include_connection_gain,
                                   all_patches_are_always_supposed_to_be_here);
    
    CONNECTIONS_apply_changes(scene, changes);
  }
  
  if (include_modulator_connections && HASH_has_key(connections, "modulator_connections"))
    MODULATORS_apply_connections_state(HASH_get_dyn(connections, "modulator_connections"));
}

// Called from MW_create_from_state, which is called from Presets.cpp.
void CONNECTIONS_create_from_presets_state(QGraphicsScene *scene, const hash_t *connections,
                                           const vector_t *patches,
                                           const QHash<instrument_t, instrument_t> &patch_id_mapper
                                           )
{
  R_ASSERT(patches != NULL);
  R_ASSERT(Undo_Is_Open() || Undo_Is_Currently_Ignoring());
  //R_ASSERT_NON_RELEASE(Undo_Is_Open()); // not sure if Undo_Is_Currently_Ignoring() is legal as an alternative here. (commented out. seems to be legal)
  
  changes::AudioGraph changes;
    
  for(int i=0;i<HASH_get_int(connections, "num_connections");i++) {
    hash_t *connection_state = HASH_get_hash_at(connections, "", i);
      
    int index_from = HASH_get_instrument(connection_state, "from_patch").id; // sigh
    int index_to = HASH_get_instrument(connection_state, "to_patch").id; // sigh

    R_ASSERT_RETURN_IF_FALSE(index_from < patches->num_elements);
    R_ASSERT_RETURN_IF_FALSE(index_to < patches->num_elements);

    if (index_from < 0 || index_to < 0){
      R_ASSERT(index_from >= 0);
      R_ASSERT(index_to >= 0);
    } else {
      if (patches->elements[index_from]!=NULL && patches->elements[index_to]!=NULL) {
        instrument_t id_from = ((struct Patch*)patches->elements[index_from])->id;
        instrument_t id_to = ((struct Patch*)patches->elements[index_to])->id;
        
        CONNECTION_create_from_state2(scene,
                                      changes,
                                      connection_state,
                                      make_instrument(index_from), id_from,
                                      make_instrument(index_to), id_to,
                                      true, true, true,
                                      true
                                      );
      }
    }
  }

  CONNECTIONS_apply_changes(scene, changes);

  if (HASH_has_key(connections, "modulator_connections")){
    MODULATORS_apply_connections_state(HASH_get_dyn(connections, "modulator_connections"), patch_id_mapper);
  }
}
