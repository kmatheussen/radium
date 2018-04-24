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

struct SoundPlugin;

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

extern LANGSPEC void SEQTRACKPLUGIN_clear_resampler_warning_hashmap(void);
extern LANGSPEC int64_t SEQTRACKPLUGIN_add_sample(struct SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock, enum Seqblock_Type type);
extern LANGSPEC void SEQTRACKPLUGIN_apply_gfx_samples(struct SoundPlugin *plugin);
extern LANGSPEC void SEQTRACKPLUGIN_assert_samples(const struct SoundPlugin *plugin);
extern LANGSPEC void SEQTRACKPLUGIN_assert_samples2(const struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACKPLUGIN_request_remove_sample(struct SoundPlugin *plugin, int64_t id, enum Seqblock_Type type);
extern LANGSPEC bool SEQTRACKPLUGIN_can_be_deleted(struct SoundPlugin *plugin);
extern LANGSPEC int SEQTRACKPLUGIN_get_num_samples(struct SoundPlugin *plugin);

extern LANGSPEC int SEQTRACKPLUGIN_get_num_channels(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_total_num_frames_in_sample(const struct SoundPlugin *plugin, int64_t id); // Directly in sample
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_total_num_frames_for_sample(const struct SoundPlugin *plugin, int64_t id); // When resampled.
extern LANGSPEC const wchar_t *SEQTRACKPLUGIN_get_sample_name(const struct SoundPlugin *plugin, int64_t id, bool full_path);
//extern LANGSPEC unsigned int SEQTRACKPLUGIN_get_sample_color(const struct SoundPlugin *plugin, int64_t id);
//extern LANGSPEC void SEQTRACKPLUGIN_set_sample_color(const SoundPlugin *plugin, int64_t id, unsigned int new_color);
extern LANGSPEC double SEQTRACKPLUGIN_get_resampler_ratio(const struct SoundPlugin *plugin, int64_t id);

#ifdef __cplusplus
namespace radium{
  class DiskPeaks;
}
extern const radium::DiskPeaks *SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id);
#endif

#endif

