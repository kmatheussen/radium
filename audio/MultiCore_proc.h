
#ifndef _RADIUM_AUDIO_MULTICORE_PROC_H
#define _RADIUM_AUDIO_MULTICORE_PROC_H

#include "SoundProducer_proc.h"

#include "../common/Vector.hpp"

struct SoundProducer;

//void MULTICORE_run_all(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins);
void MULTICORE_enable_RT_priority(void);
void MULTICORE_disable_RT_priority(void);

void MULTICORE_start_block(void);
void MULTICORE_end_block(void);

void MULTICORE_run_all(const radium::Vector<SoundProducer*> &sp_all, int64_t time, int num_frames, bool process_plugins);

void MULTICORE_add_sp(SoundProducer *sp);
void MULTICORE_remove_sp(SoundProducer *sp);

int MULTICORE_get_num_threads(void);
void MULTICORE_set_num_threads(int num_threads);

void MULTICORE_init(void);

void MULTICORE_shut_down(void);

#endif
