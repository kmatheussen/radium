
#ifndef _RADIUM_AUDIO_ENVELOPECONTROLLER_PLUGIN_PROC_H
#define _RADIUM_AUDIO_ENVELOPECONTROLLER_PLUGIN_PROC_H

extern LANGSPEC int64_t ENVELOPECONTROLLER_get_controller_id(const struct Patch *patch, int effect_num);
extern LANGSPEC void ENVELOPECONTROLLER_add_target(int64_t controller_id, const struct Patch *patch, int effect_num);
extern LANGSPEC void ENVELOPECONTROLLER_maybe_create_and_add_target(const struct Patch *patch, int effect_num);
extern LANGSPEC void ENVELOPECONTROLLER_remove_target(int controller_id, const struct Patch *patch, int effect_num);
extern LANGSPEC int64_t *ENVELOPECONTROLLER_get_controller_ids(int *num_controllers);
extern LANGSPEC const char *ENVELOPECONTROLLER_get_description(int64_t controller_id);



#endif

