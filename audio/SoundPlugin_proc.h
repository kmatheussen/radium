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


#ifndef AUDIO_SOUNDPLUGIN_PROC_H
#define AUDIO_SOUNDPLUGIN_PROC_H

enum ValueType{
  PLUGIN_STORED_TYPE,
  PLUGIN_NONSTORED_TYPE
};

enum SetValueType{
  PLUGIN_STORE_VALUE,
  PLUGIN_DONT_STORE_VALUE
};

enum WhereToGetValue{
  VALUE_FROM_PLUGIN,
  VALUE_FROM_STORAGE
};

extern LANGSPEC SoundPlugin *PLUGIN_create_plugin(const SoundPluginType *plugin_type);
extern LANGSPEC void PLUGIN_delete_plugin(SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_update_smooth_values(SoundPlugin *plugin);

extern LANGSPEC int PLUGIN_get_num_visible_effects(SoundPlugin *plugin);

extern LANGSPEC int PLUGIN_get_effect_format(struct SoundPlugin *plugin, int effect_num);
extern LANGSPEC int PLUGIN_get_effect_num(struct SoundPlugin *plugin, const char *effect_name);
extern LANGSPEC const char *PLUGIN_get_effect_name(struct SoundPlugin *plugin, int effect_num);
extern LANGSPEC const char *PLUGIN_get_effect_description(const struct SoundPluginType *plugin_type, int effect_num);
extern LANGSPEC void PLUGIN_get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize);
extern LANGSPEC void PLUGIN_set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueType value_type, enum SetValueType set_type);
extern LANGSPEC float PLUGIN_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum WhereToGetValue where);

extern LANGSPEC hash_t *PLUGIN_get_effects_state(SoundPlugin *plugin);
extern LANGSPEC hash_t *PLUGIN_get_state(SoundPlugin *plugin);

extern LANGSPEC void PLUGIN_create_effects_from_state(SoundPlugin *plugin, hash_t *effects);
extern LANGSPEC SoundPlugin *PLUGIN_create_from_state(hash_t *state);

extern LANGSPEC void PLUGIN_reset(SoundPlugin *plugin);
extern LANGSPEC void PLUGIN_reset_one_effect(SoundPlugin *plugin, int effect_num);

#endif // AUDIO_SOUNDPLUGIN_PROC_H
