/* Copyright 2017- Kjetil S. Matheussen

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


#pragma once


extern LANGSPEC void API_set_mousePointerCurrentlyPointsAtInstrument(bool ispointing);
extern LANGSPEC void API_call_me_when_current_instrument_has_been_changed(void);

extern LANGSPEC void API_setInstrumentColor(const char *colorname, instrument_t instrument_id, bool create_undo); // same as setInstrumentColor(colorname, instrument_id, true)
  
extern LANGSPEC void API_instruments_call_regularly(void);

extern LANGSPEC void API_instrument_call_me_when_instrument_is_deleted(struct Patch *patch);

// In api_gui.cpp (because api_instrument.c is not c++. TODO: fix that)
extern LANGSPEC void API_incSoundPluginRegistryGeneration(void);
extern LANGSPEC void API_clearSoundPluginRegistryCache(void);

struct SoundPluginTypeContainer;
extern LANGSPEC void API_blacklist_container(const struct SoundPluginTypeContainer *container);
extern LANGSPEC void API_unblacklist_container(const struct SoundPluginTypeContainer *container);
extern LANGSPEC bool API_container_is_blacklisted(const struct SoundPluginTypeContainer *container);
extern LANGSPEC int API_get_num_entries_in_disk_container(struct SoundPluginTypeContainer *container);

#ifdef __cplusplus
enum class DiskOpReturn{
  ALL_MAY_FAIL,
  THIS_ONE_FAILED,
  SUCCEEDED
};
  
//DiskOpReturn API_add_disk_entries_from_populated_container(const SoundPluginTypeContainer *container, const QString path);
#endif


extern LANGSPEC void API_remove_effect_monitors_for_instrument(struct Patch *patch);
