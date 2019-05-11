
#if BENCHMARK_SMOOTHDELAY
#if !defined(RELEASE)
#error "hmm"
#endif
#endif

#include <stdio.h>
#include <math.h>

#include <QThread>

#define TEST_SMOOTHDELAY 1

#include "../common/nsmtracker.h"

#include "../audio/SmoothDelay.hpp"

static double get_ms(void){
  struct timeval now;

  int err = gettimeofday(&now, NULL);
  if (err != 0)
    abort();

  return (double)now.tv_sec*1000.0 + (double)now.tv_usec/1000.0;
}

double TIME_get_ms(void){
  return get_ms();
}

bool PLAYER_current_thread_has_lock(void){
  return false;
}

bool THREADING_is_player_or_runner_thread(void){
  return true;
}

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}

void JUCE_add_sound(float *dst, const float *src, int num_frames){
  for(int i=0;i<num_frames;i++)
    dst[i] += src[i];
}

void msleep(int ms){
  QThread::msleep(ms);
}

int main(void){
  SMOOTHDELAY_test();
  return 0;
}
