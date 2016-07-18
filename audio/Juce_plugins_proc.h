
#ifndef JUCE_PLUGIN_H_INCLUDED
#define JUCE_PLUGIN_H_INCLUDED

struct SoundPlugin;

extern LANGSPEC char *JUCE_download(const char *url_url); // the returned pointer must be freed manually using free().
extern LANGSPEC const char *JUCE_get_backtrace(void);

float JUCE_get_max_val(const float *array, const int num_elements);
  
void add_juce_plugin_type(const char *name, const wchar_t *file_or_identifier, const wchar_t *library_file_full_path);

void PLUGINHOST_load_fxbp(struct SoundPlugin *plugin, wchar_t *filename);
void PLUGINHOST_save_fxb(struct SoundPlugin *plugin, wchar_t *filename);
void PLUGINHOST_save_fxp(struct SoundPlugin *plugin, wchar_t *filename);

void PLUGINHOST_init(void);

#endif  // JUCE_PLUGIN_H_INCLUDED
