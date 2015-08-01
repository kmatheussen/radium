
#ifndef JUCE_PLUGIN_H_INCLUDED
#define JUCE_PLUGIN_H_INCLUDED

float JUCE_get_max_val(const float *array, const int num_elements);
  
void add_juce_plugin_type(const char *name, const wchar_t *file_or_identifier, const wchar_t *library_file_full_path);

void PLUGINHOST_init(void);

#endif  // JUCE_PLUGIN_H_INCLUDED
