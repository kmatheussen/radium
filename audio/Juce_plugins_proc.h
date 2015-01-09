
#ifndef JUCE_PLUGIN_H_INCLUDED
#define JUCE_PLUGIN_H_INCLUDED

void add_juce_plugin_type(const char *name, const char *filepath);

void create_juce_plugins(void);

void PLUGINHOST_treatEvents(void);


#endif  // JUCE_PLUGIN_H_INCLUDED
