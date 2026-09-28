/* Copyright 2026 Kjetil S. Matheussen

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


#ifndef AUDIO_PD_PLUGIN2_PROC_H
#define AUDIO_PD_PLUGIN2_PROC_H

#include "Pd_plugin.h"

extern LANGSPEC const wchar_t *PD2_set_controller_name(struct SoundPlugin *plugin, int n, const wchar_t *name);
extern LANGSPEC Pd_Controller *PD2_get_controller(struct SoundPlugin *plugin, int n);
extern LANGSPEC void PD2_set_qtgui(struct SoundPlugin *plugin, void *qtgui);
extern LANGSPEC void PD2_delete_controller(struct SoundPlugin *plugin, int controller_num);

extern LANGSPEC void PD2_put_controllers_to_state(const struct SoundPlugin *plugin, hash_t *state);
extern LANGSPEC void PD2_recreate_controllers_from_state(struct SoundPlugin *plugin, const hash_t *state);

extern LANGSPEC void RT_PD2_set_absolute_time(int64_t time);
extern LANGSPEC void RT_PD2_set_realline(int64_t time, int64_t time_nextrealline, const Place *p);

extern void create_pd2_plugin(void);

#endif // AUDIO_PD_PLUGIN2_PROC_H
