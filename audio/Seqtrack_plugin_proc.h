/* Copyright 2017-2018 Kjetil S. Matheussen

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



#ifndef RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H
#define RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H

#define SEQTRACKPLUGIN_NAME "Sequencer audio file recorder/player"
#define SEQTRACKPLUGIN_BUS_NAME "Bus"

#include "SoundPlugin.h"


/*
No need for these two functions. Just set seqblock->t.interior_start / seqblock->t.interior_end while holding player lock.
extern LANGSPEC void SEQTRACKPLUGIN_set_interior_start(struct SoundPlugin *plugin, int64_t id, int64_t interior_start);
extern LANGSPEC void SEQTRACKPLUGIN_set_interior_end(struct SoundPlugin *plugin, int64_t id, int64_t interior_end);
*/

// Returns true if there is more to play
extern LANGSPEC bool RT_SEQTRACKPLUGIN_called_per_block(struct SoundPlugin *plugin, struct SeqTrack *seqtrack);  // Sets seqtrack->curr_sample_seqblock when starting/stopping playing audio file.

extern LANGSPEC void SEQTRACKPLUGIN_called_very_often(struct SoundPlugin *plugin);

#if __cplusplus
namespace radium{
  class FutureSignalTrackingSemaphore;
}
extern LANGSPEC void SEQTRACKPLUGIN_prepare_to_play(struct SoundPlugin *plugin, const struct SeqTrack *seqtrack, int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit);
#endif

extern LANGSPEC vector_t SEQTRACKPLUGIN_get_all_used_audio_filenames(struct SoundPlugin *plugin);
  
extern LANGSPEC void SEQTRACKPLUGIN_clear_resampler_warning_hashmap(void);
#if __cplusplus
extern int64_t SEQTRACKPLUGIN_add_sample(const struct SeqTrack *seqtrack, struct SoundPlugin *plugin, filepath_t filename, enum ResamplerType resampler_type, const struct SeqBlock *seqblock, Seqblock_Type type);
#endif

extern LANGSPEC void SEQTRACKPLUGIN_enable_recording(struct SeqTrack *seqtrack, struct SoundPlugin *plugin, filepath_t path); // Called when user enables the "R" checkbox.
extern LANGSPEC bool SEQTRACKPLUGIN_request_stop_recording(struct SeqTrack *seqtrack, struct SoundPlugin *plugin); // Called when user disables the "R" checkbox.

extern LANGSPEC void SEQTRACKPLUGIN_apply_gfx_samples(struct SoundPlugin *plugin);
extern LANGSPEC void SEQTRACKPLUGIN_assert_samples(const struct SoundPlugin *plugin);
extern LANGSPEC void SEQTRACKPLUGIN_assert_samples2(const struct SeqTrack *seqtrack);
#if __cplusplus
extern void SEQTRACKPLUGIN_request_remove_sample(struct SoundPlugin *plugin, int64_t id, Seqblock_Type type); // Note: Make sure seqtrack->curr_sample_seqblock is updated. In seqtrack.cpp, use prepare_remove_sample_from_seqblock instead.
#endif
extern LANGSPEC bool SEQTRACKPLUGIN_can_be_deleted(struct SoundPlugin *plugin);
extern LANGSPEC int SEQTRACKPLUGIN_get_num_samples(const struct SoundPlugin *plugin);

extern LANGSPEC int SEQTRACKPLUGIN_get_num_channels(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_total_num_frames_in_sample(const struct SoundPlugin *plugin, int64_t id); // Directly in sample
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_total_num_frames_for_sample(const struct SoundPlugin *plugin, int64_t id); // When resampled.
extern LANGSPEC filepath_t SEQTRACKPLUGIN_get_sample_name(const struct SoundPlugin *plugin, int64_t id, bool full_path);
extern LANGSPEC unsigned int SEQTRACKPLUGIN_get_sample_color(const struct SoundPlugin *plugin, int64_t id);
//extern LANGSPEC unsigned int SEQTRACKPLUGIN_get_sample_color(const struct SoundPlugin *plugin, int64_t id);
//extern LANGSPEC void SEQTRACKPLUGIN_set_sample_color(const SoundPlugin *plugin, int64_t id, unsigned int new_color);
extern LANGSPEC double SEQTRACKPLUGIN_get_resampler_ratio(const struct SoundPlugin *plugin, int64_t id);

extern LANGSPEC void SEQTRACKPLUGIN_set_grain_strict_no_jitter(struct SoundPlugin *plugin, int64_t id, bool new_strict_no_jitter);
extern LANGSPEC bool SEQTRACKPLUGIN_get_grain_strict_no_jitter(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC void SEQTRACKPLUGIN_set_grain_overlap(struct SoundPlugin *plugin, int64_t id, double new_gf);
extern LANGSPEC double SEQTRACKPLUGIN_get_grain_overlap(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC void SEQTRACKPLUGIN_set_grain_length(struct SoundPlugin *plugin, int64_t id, double new_gf);
extern LANGSPEC double SEQTRACKPLUGIN_get_grain_length(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC void SEQTRACKPLUGIN_set_grain_jitter(struct SoundPlugin *plugin, int64_t id, double new_jitter);
extern LANGSPEC double SEQTRACKPLUGIN_get_grain_jitter(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC void SEQTRACKPLUGIN_set_grain_ramp(struct SoundPlugin *plugin, int64_t id, double new_ramp);
extern LANGSPEC double SEQTRACKPLUGIN_get_grain_ramp(const struct SoundPlugin *plugin, int64_t id);

// Called when loading a song saved with radium 9.9.12 or older.
extern LANGSPEC void SEQTRACKPLUGIN_convert_old_granular_parameters(const struct SoundPlugin *plugin, struct SeqBlock *seqblock);

extern LANGSPEC enum ResamplerType SEQTRACKPLUGIN_get_resampler_type(const struct SoundPlugin *plugin, int64_t id);

#ifdef __cplusplus
#ifdef RADIUM_AUDIO_PEAKS_HPP
extern radium::Peakss SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id);
#endif
#endif

extern LANGSPEC const char *BUS_get_bus_name(int bus_num);
extern LANGSPEC void BUS_set_num_channels(int num_channels);

static inline bool PLUGIN_is_for_seqtrack(const SoundPlugin *plugin){
  if (!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name))
    return true;
  else if (!strcmp("Bus", plugin->type->type_name))
    return true;
  else
    return false;
}

#endif

