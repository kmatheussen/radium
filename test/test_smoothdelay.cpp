
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

//#include "../audio/Fade.cpp"

#include "test_dummies.c"

int main(void){
  SMOOTHDELAY_test();
  return 0;
}
