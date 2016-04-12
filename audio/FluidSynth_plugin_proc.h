extern LANGSPEC bool FLUIDSYNTH_set_new_preset(SoundPlugin *plugin, const wchar_t *_sf2_file, int bank_num, int preset_num);
extern LANGSPEC const wchar_t *FLUIDSYNTH_get_filename(struct SoundPlugin *plugin, bool *is_default_sound);
extern LANGSPEC const wchar_t *FLUIDSYNTH_get_filename_display(struct SoundPlugin *plugin);
