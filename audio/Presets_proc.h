
#ifndef _RADIUM_AUDIO_PRESETS_PROC_H
#define _RADIUM_AUDIO_PRESETS_PROC_H

extern LANGSPEC vector_t PRESET_get_all_presets_in_path(const wchar_t *path);

extern LANGSPEC void PRESET_request_load_instrument_description(func_t *callback);

extern LANGSPEC int64_t PRESET_load(const wchar_t *filename, char *name, bool inc_usage_number, float x, float y);

extern LANGSPEC void PRESET_set_last_used_filename(const wchar_t *wfilename);

extern LANGSPEC void PRESET_save(const vector_t *patches, bool is_single_preset);

extern LANGSPEC int64_t PRESET_paste(float x, float y);
extern LANGSPEC bool PRESET_has_copy(void);
extern LANGSPEC void PRESET_copy(vector_t *patches);

#endif
