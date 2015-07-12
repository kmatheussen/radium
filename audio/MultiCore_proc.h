
struct SoundProducer;

//void MULTICORE_run_all(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins);
void MULTICORE_run_all(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins);
int MULTICORE_get_num_threads(void);
void MULTICORE_set_num_threads(int num_threads);
void MULTICORE_init(void);

extern bool g_running_multicore;

