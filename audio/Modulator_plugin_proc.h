
#ifndef _RADIUM_AUDIO_MODULATOR_PLUGIN_PROC_H
#define _RADIUM_AUDIO_MODULATOR_PLUGIN_PROC_H

enum ModulatorEnabledTypes{
  MET_DISABLED,
  MET_ONLY_ENABLE_WHEN_PLAYING,
  MET_ENABLED,

  MET_NUM_ENABLED_TYPES
};

extern LANGSPEC void RT_MODULATOR_process(void);
extern LANGSPEC int64_t MODULATOR_get_id(const struct Patch *patch, int effect_num);
extern LANGSPEC int64_t MODULATOR_get_id_from_modulator_patch(const struct Patch *patch);
extern LANGSPEC struct Patch *MODULATOR_get_modulator_patch(const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_add_target(int64_t modulator_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_maybe_create_and_add_target(const struct Patch *patch, int effect_num, bool do_replace);
extern LANGSPEC void MODULATOR_remove_target(int modulator_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_set_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num, bool enabled);
extern LANGSPEC bool MODULATOR_get_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_call_me_when_a_patch_is_made_inactive(const struct Patch *patch);
extern LANGSPEC int64_t *MODULATOR_get_ids(int *num_modulators);
extern LANGSPEC const char *MODULATOR_get_description(int64_t modulator_id);
extern LANGSPEC dynvec_t MODULATOR_get_modulator_targets(int64_t modulator_patch_id);
extern LANGSPEC bool MODULATOR_is_modulator(int64_t modulator_patch_id);
extern LANGSPEC dyn_t MODULATOR_get_connections_state(void);
extern LANGSPEC void MODULATOR_apply_connections_state(const dyn_t state);

#endif

