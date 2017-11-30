
#ifndef RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H
#define RADIUM_AUDIO_SEQTRACK_PLUGIN_PROC_H

#define SEQTRACKPLUGIN_NAME "Sequencer audio file recorder/player"

struct SoundPlugin;

extern LANGSPEC void RT_SEQTRACKPLUGIN_called_per_block(SoundPlugin *plugin, const struct SeqTrack *seqtrack);
extern LANGSPEC void SEQTRACKPLUGIN_prepare_to_play(SoundPlugin *plugin, const struct SeqTrack *seqtrack, int64_t seqtime);

extern LANGSPEC int64_t SEQTRACKPLUGIN_add_sample(SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock);
extern LANGSPEC void SEQTRACKPLUGIN_remove_sample(SoundPlugin *plugin, int64_t id);

extern LANGSPEC int64_t SEQTRACKPLUGIN_get_num_channels(const SoundPlugin *plugin, int64_t id);
extern LANGSPEC int64_t SEQTRACKPLUGIN_get_num_frames(const SoundPlugin *plugin, int64_t id);
extern LANGSPEC const wchar_t *SEQTRACKPLUGIN_get_sample_name(const SoundPlugin *plugin, int64_t id);

#ifdef __cplusplus
namespace radium{
  class DiskPeaks;
}
extern const radium::DiskPeaks *SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id);
#endif

#endif

