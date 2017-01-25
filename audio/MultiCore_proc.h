
#include "SoundProducer_proc.h"

#include "../common/Vector.hpp"


//void MULTICORE_run_all(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins);
void MULTICORE_start_block(void);
void MULTICORE_end_block(void);
void MULTICORE_run_all(const radium::Vector<SoundProducer*> &sp_all, int64_t time, int num_frames, bool process_plugins);
int MULTICORE_get_num_threads(void);
void MULTICORE_set_num_threads(int num_threads);
void MULTICORE_init(void);
void MULTICORE_shut_down(void);

