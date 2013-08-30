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

#ifndef QT_INSTRUMENTS_PROC_H
#define QT_INSTRUMENTS_PROC_H

extern LANGSPEC hash_t *create_instrument_widget_order_state(void);
extern LANGSPEC void recreate_instrument_widget_order_from_state(hash_t *state);

extern LANGSPEC void GFX_update_instrument_widget(struct Patch *patch);

struct SoundPlugin;
struct SoundPluginType;
extern LANGSPEC SoundPlugin *add_new_audio_instrument_widget(SoundPluginType *plugin_type, int x, int y, bool autoconnect, const char *name);
extern LANGSPEC void close_all_instrument_widgets(void);

extern LANGSPEC struct Patch *get_current_instruments_gui_patch(void);

#ifdef __cplusplus
#include <QWidget>
QWidget *createInstrumentsWidget(void);
#endif // __cplusplus

#endif
