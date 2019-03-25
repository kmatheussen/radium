
#include <QThread>

#define TEST_AUDIOBUFFER 1
#include "../audio/AudioBuffer.cpp"

bool PLAYER_current_thread_has_lock(void){
  return false;
}

bool THREADING_is_player_or_runner_thread(void){
  return true;
}

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}

void RError_internal(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  CRASHREPORTER_send_assert_message(CT_ERROR, "RError: %s",message);
  //show_message(IS_ERROR,message);
}

void msleep(int ms){
  QThread::msleep(ms);
}

int main(){
  AUDIOBUFFERS_test();
  return 0;
}
