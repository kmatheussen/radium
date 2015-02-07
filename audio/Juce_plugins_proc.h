
#ifndef JUCE_PLUGIN_H_INCLUDED
#define JUCE_PLUGIN_H_INCLUDED

void add_juce_plugin_type(const char *name, const wchar_t *file_or_identifier, int uid);

void PLUGINHOST_init(void);

#endif  // JUCE_PLUGIN_H_INCLUDED
