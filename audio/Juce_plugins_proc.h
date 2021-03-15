
#ifndef JUCE_PLUGIN_H_INCLUDED
#define JUCE_PLUGIN_H_INCLUDED

typedef bool (*JUCE_audio_device_callback) (const int num_frames, const double samplerate, void *callback_data);

extern int g_juce_num_input_audio_channels;
extern const float **g_juce_input_audio_channels;

extern int g_juce_num_output_audio_channels;  
extern float **g_juce_output_audio_channels;

extern bool JUCE_audio_set_audio_thread_priority_of_current_thread(void);
extern double JUCE_audio_get_sample_rate(void);
extern int JUCE_audio_get_buffer_size(void);
extern double JUCE_audio_time_at_cycle_start(void);
extern int JUCE_get_num_xruns(void);

extern void JUCE_audio_open_preferences_window(void);
extern void JUCE_audio_close_preferences_window(void);

extern bool JUCE_init_audio_device(JUCE_audio_device_callback callback, void *callback_data);
extern void JUCE_stop_audio_device(void);

struct SoundPlugin;

extern LANGSPEC bool JUCE_native_gui_grabs_keyboard(void);

extern LANGSPEC void *JUCE_lock(void);
extern LANGSPEC void JUCE_unlock(void *lock);

extern LANGSPEC char *JUCE_download(const char *url_url); // the returned pointer must be freed manually using free().
extern LANGSPEC const char *JUCE_get_backtrace(void);

void JUCE_add_sound(float *dst, const float *src, int num_frames);

void JUCE_get_min_max_val(const float *array, const int num_elements, float *min_val, float *max_val);
float JUCE_get_max_val(const float *array, const int num_elements);
  
void add_juce_plugin_type(const char *name, const wchar_t *file_or_identifier, const wchar_t *library_file_full_path, const char *container_type_name);

void PLUGINHOST_load_fxbp(struct SoundPlugin *plugin, wchar_t *filename);
void PLUGINHOST_save_fxb(struct SoundPlugin *plugin, wchar_t *filename);
void PLUGINHOST_save_fxp(struct SoundPlugin *plugin, wchar_t *filename);

void PLUGINHOST_init(void);
void PLUGINHOST_shut_down(void);

#ifdef __cplusplus
#include "../common/Mutex.hpp"
extern radium::Mutex JUCE_show_hide_gui_lock;
#endif

#endif  // JUCE_PLUGIN_H_INCLUDED
