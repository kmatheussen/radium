
#ifndef _RADIUM_AUDIO_MODULATOR_PLUGIN_PROC_H
#define _RADIUM_AUDIO_MODULATOR_PLUGIN_PROC_H

extern LANGSPEC void RT_MODULATOR_process(void);
extern LANGSPEC int64_t MODULATOR_get_controller_id(const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_add_target(int64_t controller_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_maybe_create_and_add_target(const struct Patch *patch, int effect_num);
extern LANGSPEC void MODULATOR_remove_target(int controller_id, const struct Patch *patch, int effect_num);
extern LANGSPEC int64_t *MODULATOR_get_controller_ids(int *num_controllers);
extern LANGSPEC const char *MODULATOR_get_description(int64_t controller_id);



#endif

