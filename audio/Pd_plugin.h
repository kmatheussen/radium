/* Copyright 2013 Kjetil S. Matheussen

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



#ifndef AUDIO_PD_PLUGIN_H
#define AUDIO_PD_PLUGIN_H

#define NUM_PD_CONTROLLERS 40

#define PD_NAME_LENGTH 64
#define PD_FX_WHEN_NAME_LENGTH (PD_NAME_LENGTH+16)

//typedef struct SoundPlugin SoundPlugin;

typedef struct{
  int num;
  struct SoundPlugin *plugin;

  const wchar_t *display_name; // Same as name[], but wchar_t is (or at least should be) stored in some kind of unicode format. Should not be accessed in the player thread. May be NULL!
  
  char name[PD_NAME_LENGTH]; // Can not be set directly. Use PD_set_controller_name(SoundPlugin *plugin, int n, const char *name).
  char fx_when_start_name[PD_FX_WHEN_NAME_LENGTH];
  char fx_when_middle_name[PD_FX_WHEN_NAME_LENGTH];
  char fx_when_end_name[PD_FX_WHEN_NAME_LENGTH];
  char fx_when_single_name[PD_FX_WHEN_NAME_LENGTH];

  int type;
  float value;
  float min_value;
  float max_value;
  bool has_gui;
  bool config_dialog_visible;

  float set_new_value;
  void *pd_binding;

  bool calling_from_set_effect_value;
  bool calling_from_pd;

} Pd_Controller;

#endif // AUDIO_PD_PLUGIN_H

