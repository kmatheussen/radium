
#ifndef _RADIUM_AUDIO_PRESETS_PROC_H
#define _RADIUM_AUDIO_PRESETS_PROC_H

extern LANGSPEC dynvec_t PRESET_get_all_rec_files_in_path(filepath_t wpath);
extern LANGSPEC dynvec_t PRESET_get_all_mrec_files_in_path(filepath_t wpath);

extern LANGSPEC void PRESET_request_load_instrument_description(int64_t parentgui, func_t *callback);

extern LANGSPEC instrument_t PRESET_load(filepath_t filename, const char *name, bool inc_usage_number, bool set_as_current, bool is_visible, float x, float y);

extern LANGSPEC void PRESET_set_last_used_filename(filepath_t wfilename);
extern LANGSPEC filepath_t PRESET_get_current_preset_dir(void);

extern LANGSPEC void PRESET_save(const vector_t *patches, bool is_single_preset, int64_t parentgui);

extern LANGSPEC instrument_t PRESET_paste(float x, float y);
extern LANGSPEC bool PRESET_has_copy(void);
extern LANGSPEC bool PRESET_copy(const vector_t *patches); // returns false if it failed to copy.

#endif
