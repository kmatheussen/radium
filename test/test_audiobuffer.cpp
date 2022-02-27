
#include <QThread>

#define TEST_AUDIOBUFFER 1
#include "../audio/AudioBuffer.cpp"

#include "test_dummies.c"


int main(){

  AUDIOBUFFERS_test();
  
  return 0;
}
