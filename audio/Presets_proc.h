
#ifndef _RADIUM_AUDIO_PRESETS_PROC_H
#define _RADIUM_AUDIO_PRESETS_PROC_H

extern LANGSPEC char *PRESET_request_load_instrument_description(void);

extern LANGSPEC void PRESET_set_last_used_filename(const wchar_t *wfilename);

extern LANGSPEC void PRESET_save(vector_t *patches, bool is_single_preset);
  

#endif
