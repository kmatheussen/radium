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

typedef struct{
  int num;
  SoundPlugin *plugin;

  char *name; // Can not be set directly. Use PD_set_controller_name(SoundPlugin *plugin, int n, const char *name).
  int type;
  float value;
  float min_value;
  float max_value;
  bool has_gui;

  float set_new_value;
  void *pd_binding;

  bool calling_from_set_effect_value;
  bool calling_from_pd;

} Pd_Controller;

#endif // AUDIO_PD_PLUGIN_H

