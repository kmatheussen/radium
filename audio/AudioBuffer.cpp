
#include "../common/nsmtracker.h"
#include "../common/spinlock.h"
#include "../common/OS_visual_input.h"

#include "AudioBuffer.hpp"

/*
  Test by running make test_audiobuffer
 */


static radium::AudioBufferChannel *g_audio_channels = NULL;
static radium::Spinlock g_audio_channels_spinlock;


#if !defined(RELEASE)
#include "../common/LockAsserter.hpp"
static radium::LockAsserter lockAsserter;
#endif

static constexpr int g_num_elements = 1024; // We normally don't need much more than the maximum number of parallell running channels, so this is probably plenty.
static radium::AudioBufferChannel *g_channels;
static radium::AudioBufferChannel *g_highest_used_channel = NULL;

static radium::AudioBufferChannel g_error_audio_channel;

#if !defined(RELEASE)
static int g_num_free_elements = 0;
#endif

static int get_audio_buffer_channel_size(void){
  return sizeof(float)*RADIUM_BLOCKSIZE;
}

static radium::AudioBufferChannel *get_g_channel(int i){
  char *hack = (char*)g_channels;

  int blocksize = get_audio_buffer_channel_size();
  
  char *pos = hack + (i*blocksize);

  return (radium::AudioBufferChannel*)pos;
}

void AUDIOBUFFERS_init(void){
#if !defined(RELEASE)
  LOCKASSERTER_EXCLUSIVE(&lockAsserter);
#endif

  static bool has_inited = false;
  R_ASSERT_RETURN_IF_FALSE(has_inited==false);

  int blocksize = get_audio_buffer_channel_size();
  
  g_channels = (radium::AudioBufferChannel*)V_calloc(blocksize, g_num_elements);  

  for(int i=g_num_elements-1;i>=0;i--){
    
    auto *channel = get_g_channel(i);

    channel->next = g_audio_channels;
    g_audio_channels = channel;
  }

#if !defined(RELEASE)
  g_num_free_elements = g_num_elements;
#endif
  
  has_inited = true;
}

// Called between each audio block. We do this to keep cache warm by always starting to use the start of the channel and to lower the chance of channels not to be aligned sequentially in memory.
void RT_AUDIOBUFFERS_optimize(void){
#if !defined(RELEASE)
  if (g_num_free_elements != g_num_elements)
    abort();
#endif

  if (g_highest_used_channel==NULL)
    return;

  bool has_hit_highest = false;

  radium::AudioBufferChannel *next_channel = get_g_channel(0);
  
  for(int i=0 ; i<g_num_elements ; i++){
    radium::AudioBufferChannel *channel = next_channel;
    
    if (i==g_num_elements-1)
      next_channel = NULL;
    else
      next_channel = get_g_channel(i+1);
  
    if (channel==g_highest_used_channel)
      has_hit_highest = true;

    if (has_hit_highest && channel->next==next_channel)
      break;
    
    channel->next = next_channel;
  }

  g_highest_used_channel = NULL;
  g_audio_channels = g_channels;
}

void RT_AUDIOBUFFER_release_channel(radium::AudioBufferChannel *channel, radium::NeedsLock needs_lock){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());
  R_ASSERT_NON_RELEASE(channel!=NULL);
  
  if(channel==&g_error_audio_channel)
    return;
  
  radium::ScopedSpinlock lock(g_audio_channels_spinlock, needs_lock==radium::NeedsLock::YES);

#if !defined(RELEASE)
  LOCKASSERTER_EXCLUSIVE(&lockAsserter);
  g_num_free_elements++;
#endif
  
  channel->next = g_audio_channels;
  g_audio_channels = channel;
}

void RT_AUDIOBUFFER_release_channels(radium::AudioBufferChannel **channels, int num_ch, radium::NeedsLock needs_lock){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());
  
  radium::ScopedSpinlock lock(g_audio_channels_spinlock, needs_lock==radium::NeedsLock::YES);

  for(int ch=num_ch-1;ch>=0;ch--){ // Make sure the order of the individual channels is the same as when we started.
    if(channels[ch] == NULL)
      R_ASSERT(false);
    else {
      RT_AUDIOBUFFER_release_channel(channels[ch], radium::NeedsLock::NO);
      channels[ch] = NULL;
    }
  }
}

radium::AudioBufferChannel *RT_AUDIOBUFFER_get_channel(radium::NeedsLock needs_lock){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());
  
  radium::ScopedSpinlock lock(g_audio_channels_spinlock, needs_lock==radium::NeedsLock::YES);

#if !defined(RELEASE)
  LOCKASSERTER_EXCLUSIVE(&lockAsserter);
  g_num_free_elements--;
  
  //if(g_audio_channels==NULL)
  //  abort();
#endif

  if (g_audio_channels==NULL){
    
    RT_message("No more free audio buffers. The audio will not be correct. If you have more than %d parallell audio channels, please request the program to allocate a higher number of audio buffers. If not, please file a bug report.", g_num_elements);
    
#if !defined(RELEASE)
    g_num_free_elements++;
#endif
    
    return &g_error_audio_channel; // Sound will probably be garbled, but this way we don't have to take care of handling NULL pointers.
  }
  
  auto *ret = g_audio_channels;
  g_audio_channels = ret->next;

  if (ret > g_highest_used_channel)
    g_highest_used_channel = ret;
  
  return ret;
}

void RT_AUDIOBUFFER_get_channels(radium::AudioBufferChannel **channels, int num_channels, radium::NeedsLock needs_lock){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());
  
  radium::ScopedSpinlock lock(g_audio_channels_spinlock, needs_lock==radium::NeedsLock::YES);

  for(int ch=0;ch<num_channels;ch++){
    R_ASSERT_NON_RELEASE(channels[ch] == NULL);
    channels[ch] = RT_AUDIOBUFFER_get_channel(radium::NeedsLock::NO);
  }
}

#if TEST_AUDIOBUFFER

int g_audio_block_size = 64;

void RT_message_internal(const char *fmt,...){
  //abort();
}
  
static void validate_channels(void){

  radium::AudioBufferChannel *it = g_audio_channels;
  
  for(int i=0 ; i<g_num_elements ; i++){  
    radium::AudioBufferChannel *channel = get_g_channel(i);

    assert(it==channel);

    it = it->next;
  }

  assert(it==NULL);
}

static void testrun1(int num_elements){
  QVector<radium::AudioBufferChannel*> stuff;

  for(int i = 0 ; i < num_elements ; i++)
    stuff.push_back(RT_AUDIOBUFFER_get_channel(radium::NeedsLock::NO));

  if(num_elements==g_num_elements-1)
    assert(stuff.last()->next == get_g_channel(g_num_elements-1));
  
  if(num_elements==g_num_elements)
    assert(stuff.last()->next == NULL);
  
  for(auto *channel : stuff)
    RT_AUDIOBUFFER_release_channel(channel, radium::NeedsLock::NO);
  
  RT_AUDIOBUFFERS_optimize();
  validate_channels();
  
  RT_AUDIOBUFFERS_optimize();
  validate_channels();
}

// Release in a random order.
static void testrun2(int num_elements){
  QVector<radium::AudioBufferChannel*> stuff;

  for(int i = 0 ; i < num_elements ; i++){
    stuff.insert(stuff.size()==0 ? 0 : (qrand() % stuff.size()), RT_AUDIOBUFFER_get_channel(radium::NeedsLock::NO));
  }
  
  for(auto *channel : stuff)
    RT_AUDIOBUFFER_release_channel(channel, radium::NeedsLock::NO);
  
  RT_AUDIOBUFFERS_optimize();
  validate_channels();
  
  RT_AUDIOBUFFERS_optimize();
  validate_channels();
}

static void testrun(int num_elements){
  testrun1(num_elements);
  for(int i =0 ; i < 500; i++)
    testrun2(num_elements);
}


static void AUDIOBUFFERS_test(void){
  AUDIOBUFFERS_init();

  testrun(0);
  testrun(1);
  testrun(2);
  testrun(g_num_elements/2-1);
  testrun(g_num_elements/2+1);
  testrun(g_num_elements-1);
  testrun(g_num_elements);

  testrun(g_num_elements+1);
  testrun(g_num_elements*2);
  
}
#endif
