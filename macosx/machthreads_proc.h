#pragma once

#ifdef _cplusplus
extern "C" {
#endif

extern bool MACH_THREADS_jack_acquire_real_time_scheduling(pthread_t thread /* , int priority */);
extern bool MACH_THREADS_jack_drop_real_time_scheduling(pthread_t thread);
extern void MACH_THREADS_set_period_and_buffer_size(double sample_rate, int buffer_size);

#ifdef _cplusplus
}
#endif

