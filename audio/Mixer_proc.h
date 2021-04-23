
#ifndef _RADIUM_AUDIO_MIXER_PROC_H
#define _RADIUM_AUDIO_MIXER_PROC_H 1

#ifdef MEMORY_DEBUG
extern LANGSPEC void PLAYER_memory_debug_wake_up(void);
#else
#define PLAYER_memory_debug_wake_up()
#endif


#define NUM_SYSTEM_INPUT_JACK_PORTS 8

#ifdef __jack_h__
extern jack_client_t *g_jack_client;
#endif

extern DEFINE_ATOMIC(double, g_curr_song_tempo_automation_tempo);

extern LANGSPEC void MIXER_start_dummy_driver(void);
extern LANGSPEC void MIXER_stop_dummy_driver(void);
extern LANGSPEC bool MIXER_dummy_driver_is_running(void);

extern LANGSPEC bool MIXER_start(void);
extern LANGSPEC void MIXER_stop(void);

extern LANGSPEC void MIXER_set_all_non_realtime(bool is_non_realtime);
  
extern LANGSPEC void MIXER_start_saving_soundfile(void);
extern LANGSPEC void MIXER_request_stop_saving_soundfile(void);

extern LANGSPEC void OS_InitAudioTiming(void);

extern LANGSPEC int64_t MIXER_TRANSPORT_set_pos(double abstime); // returns absabstime.
extern LANGSPEC void MIXER_TRANSPORT_play(double abstime);
extern LANGSPEC void MIXER_TRANSPORT_stop(void);
extern LANGSPEC void MIXER_set_jack_timebase_master(bool doit);

extern LANGSPEC void MIXER_call_very_often(void);
extern LANGSPEC STime MIXER_get_block_delta_time(STime time);
extern LANGSPEC int MIXER_get_num_xruns(void);
extern LANGSPEC int MIXER_get_main_inputs(const float **audio, int max_num_ch);
//extern LANGSPEC int64_t MIXER_get_time(void);

extern LANGSPEC float MIXER_get_curr_audio_block_cycle_fraction(void);
// Not quite accurate. Should not be called from the player thread or any other realtime thread.

extern DEFINE_ATOMIC(int64_t, g_last_mixer_time);

static inline int64_t MIXER_get_last_used_time(void){
  return ATOMIC_GET(g_last_mixer_time);
}

// Called very often.
static inline int64_t RT_MIXER_get_last_used_time(void){
  R_ASSERT_NON_RELEASE(THREADING_is_runner_thread() || PLAYER_current_thread_has_lock());
  
  return ATOMIC_NAME(g_last_mixer_time);
}


extern LANGSPEC bool MIXER_fill_in_time_position(time_position_t *time_position);

#ifdef __cplusplus

extern void MIXER_add_SoundProducer(SoundProducer *sound_producer);

extern void MIXER_remove_SoundProducer(SoundProducer *sound_producer);

//extern void MIXER_get_buses(SoundProducer* &bus1, SoundProducer* &bus2);
  
#ifdef USE_QT4
#include "../common/Vector.hpp"
//extern const radium::Vector<SoundProducer*> *MIXER_get_all_SoundProducers(void);
extern const radium::Vector<SoundProducer*> &MIXER_get_all_SoundProducers(void);
#endif

#endif // __cplusplus

extern LANGSPEC Buses MIXER_get_buses(void);
//extern LANGSPEC struct SoundProducer *MIXER_get_bus(int bus_num);
extern LANGSPEC void MIXER_set_bus(int bus_num, struct SoundProducer *producer);

extern LANGSPEC struct Patch **RT_MIXER_get_all_click_patches(int *num_click_patches);

extern LANGSPEC float MIXER_get_sample_rate(void);

extern LANGSPEC int64_t MIXER_get_recording_latency_compensation_from_system_in(void);
extern LANGSPEC int64_t MIXER_get_latency_for_main_system_out(void);

struct SoundPlugin *RT_get_system_out_plugin(void); // implemented in qt_bottom_bar_widget_callbacs.h. Fast, but might return NULL or pointer to a deleted SoundPlugin.
  
#ifdef __cplusplus
bool MIXER_is_connected_to_system_out(const SoundProducer *sp);
#endif

#ifdef USE_QT4
#include <QString>
namespace radium{
  static inline QString get_time_string(int64_t frames, bool include_centiseconds = true){
    return get_time_string((double)frames / (double)MIXER_get_sample_rate(), include_centiseconds);
  }
}
#endif

static inline int MIXER_get_buffer_size(void){
  return RADIUM_BLOCKSIZE;
}

extern int g_soundcardblock_size; // Player and runner threads.
extern DEFINE_ATOMIC(int, g_soundcardblock_size); // Any thread

extern int g_soundcardblock_delta_time; // same here.
extern int g_audio_system_input_latency;
extern int g_audio_system_output_latency;

extern LANGSPEC int MIXER_get_remaining_num_audioblock_frames(void);


extern LANGSPEC struct SoundPlugin *MIXER_get_soundplugin(const char *type_name, const char *name);
extern LANGSPEC struct Patch *MIXER_get_bus(int bus_num);

extern LANGSPEC void MIXER_called_regularly_by_main_thread(void);

extern LANGSPEC void MIXER_set_all_plugins_to_not_recording(void);

#endif
