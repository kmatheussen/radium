
#ifndef RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H
#define RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H

#define SEQTRACKPLUGIN_NAME "Sequencer audio file recorder/player"

struct SoundPlugin;

extern LANGSPEC void SEQTRACKPLUGIN_set_interior_start(struct SoundPlugin *plugin, int64_t id, int64_t interior_start);
extern LANGSPEC void SEQTRACKPLUGIN_set_interior_end(struct SoundPlugin *plugin, int64_t id, int64_t interior_end);

extern LANGSPEC void RT_SEQTRACKPLUGIN_called_per_block(struct SoundPlugin *plugin, struct SeqTrack *seqtrack);  // Sets seqtrack->curr_sample_seqblock when starting/stopping playing audio file.

extern LANGSPEC void SEQTRACKPLUGIN_called_very_often(struct SoundPlugin *plugin);

#if __cplusplus
namespace radium{
  class FutureSignalTrackingSemaphore;
}
extern LANGSPEC void SEQTRACKPLUGIN_prepare_to_play(struct SoundPlugin *plugin, const struct SeqTrack *seqtrack, int64_t seqtime, radium::FutureSignalTrackingSemaphore *gotit);
#endif

extern LANGSPEC int64_t SEQTRACKPLUGIN_add_sample(struct SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock);
extern LANGSPEC void SEQTRACKPLUGIN_request_remove_sample(struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC bool SEQTRACKPLUGIN_can_be_deleted(struct SoundPlugin *plugin);
extern LANGSPEC int SEQTRACKPLUGIN_get_num_samples(struct SoundPlugin *plugin);

extern LANGSPEC int SEQTRACKPLUGIN_get_num_channels(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_num_frames(const struct SoundPlugin *plugin, int64_t id);
extern LANGSPEC const wchar_t *SEQTRACKPLUGIN_get_sample_name(const struct SoundPlugin *plugin, int64_t id, bool full_path);
extern LANGSPEC unsigned int SEQTRACKPLUGIN_get_sample_color(const struct SoundPlugin *plugin, int64_t id);

#ifdef __cplusplus
namespace radium{
  class DiskPeaks;
}
extern const radium::DiskPeaks *SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id);
#endif

#endif

