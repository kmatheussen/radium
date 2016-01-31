#ifdef MEMORY_DEBUG
extern LANGSPEC void PLAYER_memory_debug_wake_up(void);
#else
#define PLAYER_memory_debug_wake_up()
#endif

extern LANGSPEC bool PLAYER_is_running(void);
  
extern LANGSPEC bool MIXER_start(void);
extern LANGSPEC void MIXER_stop(void);

extern LANGSPEC bool MIXER_is_saving(void);
extern LANGSPEC void MIXER_start_saving_soundfile(void);
extern LANGSPEC void MIXER_request_stop_saving_soundfile(void);

extern LANGSPEC void OS_InitAudioTiming(void);
extern LANGSPEC STime MIXER_get_block_delta_time(STime time);
//extern LANGSPEC int64_t MIXER_get_time(void);
extern LANGSPEC STime MIXER_get_accurate_radium_time(void);

#ifdef __cplusplus

extern void MIXER_add_SoundProducer(SoundProducer *sound_producer);

extern void MIXER_remove_SoundProducer(SoundProducer *sound_producer);

extern void MIXER_get_buses(SoundProducer* &bus1, SoundProducer* &bus2);
  
#ifdef USE_QT4
#include "../common/Vector.hpp"
extern radium::Vector<SoundProducer*> *MIXER_get_all_SoundProducers(void);
#endif

#endif // __cplusplus

extern LANGSPEC Buses MIXER_get_buses(void);
//extern LANGSPEC struct SoundProducer *MIXER_get_bus(int bus_num);

extern LANGSPEC struct Patch **RT_MIXER_get_all_click_patches(int *num_click_patches);

extern LANGSPEC float MIXER_get_sample_rate(void);

extern LANGSPEC int MIXER_get_buffer_size(void);
