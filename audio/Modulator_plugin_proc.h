
#pragma once

enum ModulatorEnabledTypes{
  MET_DISABLED,
  MET_ONLY_ENABLE_WHEN_PLAYING,
  MET_ENABLED,

  MET_NUM_ENABLED_TYPES
};

#define MODULATOR_NAME "Modulator"


extern LANGSPEC void RT_MODULATOR_process(void);
extern LANGSPEC int64_t MODULATOR_get_id(const struct Patch *patch, int effect_num);
extern LANGSPEC int64_t MODULATOR_get_id_from_modulator_patch(const struct Patch *modulator_patch);
extern LANGSPEC bool MODULATOR_has_modulator(const struct Patch *patch, int effect_num);
extern LANGSPEC struct Patch *MODULATOR_get_modulator_patch(const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_add_target(int64_t modulator_id, const struct Patch *patch, int effect_num, bool do_replace);
extern LANGSPEC void MODULATOR_maybe_create_and_add_target(const struct Patch *patch, int effect_num, bool do_replace);
extern LANGSPEC void MODULATOR_remove_target(int64_t modulator_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_set_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num, bool enabled);
extern LANGSPEC bool MODULATOR_get_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_call_me_when_a_patch_is_made_inactive(const struct Patch *patch);
extern LANGSPEC int64_t *MODULATOR_get_ids(int *num_modulators);
extern LANGSPEC const char *MODULATOR_get_description(int64_t modulator_id);
extern LANGSPEC dynvec_t MODULATOR_get_modulator_targets(instrument_t modulator_patch_id);
extern LANGSPEC bool MODULATOR_is_modulator(instrument_t modulator_patch_id);

extern LANGSPEC dyn_t MODULATORS_get_connections_state(void);

#ifdef __cplusplus
void MODULATORS_apply_connections_state(const dyn_t dynstate, const QHash<instrument_t, instrument_t> &patch_id_mapper);
#endif

extern LANGSPEC void MODULATORS_apply_connections_state(const dyn_t state);
