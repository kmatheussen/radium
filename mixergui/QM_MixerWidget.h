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

#ifdef USE_QT4

#include <QGraphicsSceneMouseEvent>
#include <QUrl>
#include <QMimeData>

//#include <QtGui/qwidget.h>
#include <QGraphicsScene>
//#include <QtGui/QGraphicsItem>

#include "../Qt/helpers.h"
#include "QM_chip.h"



static const int chip_width = 120;
static const int chip_height = 32;
static const int grid_width = chip_width;
static const int grid_border = 5;
static const int grid_height = chip_height;
static const int port_width = chip_width/8;
static const int port_height = 2;//chip_height/8;



namespace radium{
  //  class MyQGraphicsView;
  class MixerWidget;
}

extern radium::MixerWidget *g_mixer_widget;

class QSplitter;

QGraphicsScene *get_scene(radium::MixerWidget *mixer_widget);
QWidget *get_mixer_bottom_bar(radium::MixerWidget *mixer_widget);
QSplitter *get_mixer_ysplitter(radium::MixerWidget *mixer_widget);
QWidget *get_qwidget(radium::MixerWidget *mixer_widget);
radium::MixerWidget *create_mixer_widget(QWidget *parent);

void MW_call_each_16ms(double ms);

void MW_set_autopos(double *x, double *y);
  
bool MW_move_chip_to_slot(Chip *chip, float x, float y);
  
Chip *MW_get_chip_at(float x, float y, const Chip *except);

void MW_set_selected_chip(Chip *chip);


#endif // __cplusplus

extern LANGSPEC DEFINE_ATOMIC(bool, g_show_cpu_usage_in_mixer);

extern LANGSPEC void MW_set_rotate(float rotate);
extern LANGSPEC void MW_update_mixer_widget(bool update_current_button);
extern LANGSPEC void MW_disable_include_instrument_checkbox(void);
extern LANGSPEC void MW_enable_include_instrument_checkbox(void);
#if USE_QT4
#include "../Qt/Qt_instruments_proc.h"
#include <QWidget>
extern void MW_instrument_widget_set_size(QWidget *audio_widget, SizeType old_size_type, SizeType new_size_type);
#endif
extern LANGSPEC void MW_hide_non_instrument_widgets(void);
extern LANGSPEC void MW_show_non_instrument_widgets(void);

extern LANGSPEC dyn_t MW_get_mixer_strips_state(void);
extern LANGSPEC void MW_apply_mixer_strips_state(dyn_t state);
extern LANGSPEC int64_t MW_get_mixer_strips_guinum(void);
extern LANGSPEC bool MW_modular_mixer_is_visible(void);
extern LANGSPEC void MW_set_modular_mixer_type(bool show_modular);

extern LANGSPEC void MW_set_window_mode(bool show_window);
extern LANGSPEC bool MW_in_window_mode(void);
extern LANGSPEC void MW_update_checkboxes(void);

extern LANGSPEC void MW_set_instrument_in_mixer(bool include_instrument_widget);
//extern LANGSPEC void MW_set_sequencer_in_mixer(bool include_instrument_widget);
extern LANGSPEC void MW_update_sequencer_in_mixer_checkbox(void);

//extern LANGSPEC const char *MW_request_load_preset_instrument_description(void);
extern LANGSPEC const char *MW_popup_plugin_selector2(bool must_have_inputs, bool must_have_outputs);
extern LANGSPEC SoundPluginType *MW_popup_plugin_type_selector(bool must_have_inputs, bool must_have_outputs);

extern LANGSPEC vector_t MW_get_selected_chips(void);
extern LANGSPEC vector_t MW_get_selected_patches(void);

extern LANGSPEC void MW_copy(void);
extern LANGSPEC void MW_delete(void);
extern LANGSPEC void MW_cut(void);
extern LANGSPEC instrument_t MW_paste(float x, float y);

extern LANGSPEC bool MW_has_mouse_pointer(void);

extern LANGSPEC bool MW_get_connections_visibility(void);
extern LANGSPEC void MW_set_connections_visibility(bool show);

extern LANGSPEC bool MW_get_bus_connections_visibility(void);
extern LANGSPEC void MW_set_bus_connections_visibility(bool show);

extern LANGSPEC void MW_zoom(int inc);
extern void MW_reset_zoom(void);

extern LANGSPEC void MW_connect_plugin_to_main_pipe(SoundPlugin *plugin);

#ifdef USE_QT4
extern void MW_connect(struct Patch *source, struct Patch *dest, ConnectionType connection_type);
#endif

extern LANGSPEC void MW_econnect(struct Patch *source, struct Patch *dest);

extern LANGSPEC bool MW_disconnect(struct Patch *source, struct Patch *dest);
extern LANGSPEC bool MW_edisconnect(struct Patch *source, struct Patch *dest);

extern LANGSPEC bool MW_are_connected(struct Patch *source, struct Patch *dest);
extern LANGSPEC bool MW_are_econnected(struct Patch *source, struct Patch *dest);

extern LANGSPEC bool MW_move_chip_to_slot(struct Patch *patch, float x, float y);
extern LANGSPEC bool MW_autoconnect(struct Patch *patch, float x, float y);

extern LANGSPEC void MW_update_all_chips(void);

extern LANGSPEC void MW_cleanup(bool is_loading); // Deletes all chips
extern LANGSPEC void MW_cleanup_chip_positions(void); // Cleans up chip positions.
extern LANGSPEC void MW_get_slotted_x_y(float from_x, float from_y, float *x, float *y); // Workaround.

#ifdef USE_QT4
#include <QHash>
extern LANGSPEC void MW_get_curr_mixer_slot(float &x, float &y);
extern LANGSPEC void MW_create_from_state(const hash_t *state, const vector_t *patches, const QHash<instrument_t, instrument_t> &patch_id_mapper, float x, float y);
#endif

extern LANGSPEC hash_t *MW_get_connections_state(const vector_t *patches, bool include_audio, bool include_event, bool include_modulator_connections);
extern LANGSPEC hash_t *MW_get_state(const vector_t *patches, bool include_ab);
extern LANGSPEC void MW_create_connections_from_state(const hash_t *connections);
extern LANGSPEC void MW_create_connections_from_state_and_replace_patch(const hash_t *connections, int patch_id_old, int patch_id_new);
extern LANGSPEC void MW_create_full_from_state(const hash_t *state, bool is_loading);

extern LANGSPEC void MW_create_plain(void);

#define MW_NUM_AB 8

extern LANGSPEC int MW_get_curr_ab(void);
extern LANGSPEC bool MW_is_ab_valid(int ab_num);
extern LANGSPEC void MW_change_ab(int ab_num, bool update_current_button);
extern LANGSPEC void MW_reset_ab(int ab_num); // -1 resets all
extern LANGSPEC bool MW_ab_is_used(int num);
extern LANGSPEC hash_t *MW_get_ab_state(void);
extern LANGSPEC void MW_recreate_ab_from_state(hash_t *ab_state);

extern LANGSPEC void MW_set_chip_position(struct Patch *patch, float x, float y);
extern LANGSPEC float MW_get_chip_x(const struct Patch *patch);
extern LANGSPEC float MW_get_chip_y(struct Patch *patch);

extern LANGSPEC struct Patch *get_main_pipe_patch(void);
extern LANGSPEC instrument_t get_main_pipe_patch_id(void);
extern LANGSPEC SoundPlugin *get_main_pipe(void);

#endif // QM_MIXERWIDGET_H
