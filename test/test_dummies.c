#include <sys/time.h>
#include <stdarg.h>
#include <unistd.h>

#ifdef __cplusplus
#include <mutex>
#endif

DEFINE_ATOMIC(bool, is_starting_up) = false;

bool g_qtgui_has_stopped = false;

float g_gfx_scale = 1.2;

struct Root *root = NULL;

int g_audio_block_size = 64;

bool chance(double p);

bool chance(double p){
  return (rand() % 1000) < (p * 1000);
}

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


#if !defined(RELEASE)
bool MIXER_is_saving(){
  return false;
}
#endif


void RWarning_internal(const char *fmt,...){
  abort();
}


__thread int g_thread_type = 0;

bool PLAYER_current_thread_has_lock(void){
  return g_thread_type==1;
}
bool THREADING_is_main_thread(void){
  return g_thread_type==0;
}

bool THREADING_is_player_or_runner_thread(void){
  return !THREADING_is_main_thread();
}

bool THREADING_is_player_thread(void){
  return !THREADING_is_main_thread();
}

#ifdef __cplusplus
static std::mutex g_player_lock;
void PLAYER_lock(void){
  g_player_lock.lock();
}

void PLAYER_unlock(void){
  g_player_lock.unlock();
}
#else
void PLAYER_lock(void){
  abort();
}

void PLAYER_unlock(void){
  abort();
}
#endif

#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
bool THREADING_has_player_thread_priority(void){
  return false;
}
#endif
#endif

bool THREADING_is_runner_thread(void){
  return g_thread_type==1;
  //return false;
}

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}

bool g_rt_message_internal_is_error = true;

void RT_message_internal(const char *fmt,...){
  if (g_rt_message_internal_is_error)
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
  usleep(1000*ms);
}

void JUCE_add_sound(float *dst, const float *src, int num_frames);
  
void JUCE_add_sound(float *dst, const float *src, int num_frames){
  for(int i=0;i<num_frames;i++)
    dst[i] += src[i];
}

void EndProgram(void);

void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

