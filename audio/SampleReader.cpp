
#include <stdio.h>
#include <stdint.h>

namespace{
struct RemoveAdd{
  // a_ = request add
  // r_ = request remove

  int64_t a_slice_start = -1;
  int64_t a_slice_end = -1;
  
  int64_t r_slice_start = -1;
  int64_t r_slice_end = -1;
  
  int64_t a_extra_slice_start = -1;
  int64_t a_extra_slice_end = -1;

  int64_t r_extra_slice_start = -1;
  int64_t r_extra_slice_end = -1;
  
  RemoveAdd(int64_t new_start, int64_t new_end, int64_t old_start, int64_t old_end){
    if (new_end <= old_start || old_end <= new_start ) {
      /*       oooo
       *   nnnn
       */
      /*   oooo
       *       nnnn
       */
      //printf("9\n");
      a_slice_start = new_start;
      a_slice_end = new_end;
      r_slice_start = old_start;
      r_slice_end = old_end;
    } else if (new_start < old_start) {
    
      if (new_end > old_end){
        /*     ooo
         *   nnnnnnn
         */
        //printf("a\n");
        a_slice_start = new_start;
        a_slice_end = old_start;
        
        a_extra_slice_start = old_end;
        a_extra_slice_end = new_end;
      } else if (old_end > old_start) {
        /*     oooooo
         *   nnnnn
         */
        //printf("b\n");
        a_slice_start = new_start;
        a_slice_end = old_start;

        r_slice_start = new_end;
        r_slice_end = old_end;
      }
      
    } else if (old_start < new_start) {

      if (old_end > new_end ) {
        /*   ooooooo
         *     nnn
         */
        //printf("c\n");
        r_slice_start = old_start;
        r_slice_end = new_start;
        
        r_extra_slice_start = new_end;
        r_extra_slice_end = old_end;
      } else if (new_end > old_end) {
        /*   ooooooo
         *     nnnnnnnn
         */
        //printf("d\n");
        r_slice_start = old_start;
        r_slice_end = new_start;
        
        a_slice_start = old_end;
        a_slice_end = new_end;
      }

    }
  }
};

}

#if defined(TEST_MAIN)

// g++ SampleReader.cpp -DTEST_MAIN -Wall -std=gnu++11 && ./a.out 

#include <stdio.h>
#include <assert.h>

int main(){  
  {RemoveAdd x(0,0,0,0);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(0,0,1,1);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(1,1,0,0);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(0,0,1,1);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(0,1,0,1);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(0,10,0,0);
    assert(x.a_slice_start==0);
    assert(x.a_slice_end==10);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(0,0,0,10);
    assert(x.r_slice_start==0);
    assert(x.r_slice_end==10);
    assert(x.a_slice_start==x.a_slice_end);}

  {RemoveAdd x(5,15,0,10);
    assert(x.a_slice_start==10);
    assert(x.a_slice_end==15);
    assert(x.r_slice_start==0);
    assert(x.r_slice_end==5);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);
  }
  {RemoveAdd x(0,10,5,15);
    assert(x.a_slice_start==0);
    assert(x.a_slice_end==5);
    assert(x.r_slice_start==10);
    assert(x.r_slice_end==15);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);
  }
  {RemoveAdd x(0,15,5,10);
    assert(x.a_slice_start==0);
    assert(x.a_slice_end==5);
    assert(x.r_slice_start==x.r_slice_end);
    assert(x.a_extra_slice_start==10);
    assert(x.a_extra_slice_end=15);
  }
  {RemoveAdd x(5,10,0,15);
    assert(x.r_slice_start==0);
    assert(x.r_slice_end==5);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_extra_slice_start==10);
    assert(x.r_extra_slice_end=15);
  }

  {RemoveAdd x(5,10,10,15);
    assert(x.r_slice_start==10);
    assert(x.r_slice_end==15);
    assert(x.a_slice_start==5);
    assert(x.a_slice_end==10);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);
  }
  {RemoveAdd x(10,15,5,10);
    assert(x.r_slice_start==5);
    assert(x.r_slice_end==10);
    assert(x.a_slice_start==10);
    assert(x.a_slice_end==15);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);
  }

  return 0;
}

#else

#define __STDC_FORMAT_MACROS 1

#include <inttypes.h>

#include <QHash>
#include <QThread>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/Queue.hpp"
#include "../common/OS_visual_input.h"
#include "../common/LockAsserter.hpp"

#include "Peaks.hpp"

#include "SampleReader_proc.h"


//#define SLICE_SIZE 4096 // in frames (TODO: Set this MUCH lower to test RT_get spanning several slices)
#define SLICE_SIZE 3 // in frames (TODO: Set this MUCH lower to test RT_get spanning several slices)

namespace radium {


class SampleProvider;
static QHash<QString, SampleProvider*> g_sample_providers;


class SampleProvider{

public:

  int _num_ch;
  int64_t _num_slices;
  int64_t _num_frames;

  bool _is_valid = false;

private:

  int _num_users = 0;

  struct Slice{
    DEFINE_ATOMIC(float*, samples); // Written to by the SPT thread, read by a RT thread. Contains all channels after each other.
    int num_users; // Only read by the SPT thread.
  };

  Slice *_slices;

public:

  const wchar_t *_filename;

  SNDFILE *create_sndfile2(SF_INFO *sf_info){
    return radium_sf_open(_filename,SFM_READ,sf_info);
  }

  SNDFILE *create_sndfile(void){
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    return create_sndfile2(&sf_info);
  }

  SampleProvider(const wchar_t *filename)
    : _filename(filename)
  {
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    SNDFILE *sndfile = create_sndfile2(&sf_info);
    if (sndfile==NULL){
      GFX_addMessage("Could not open file %s", STRING_get_chars(filename));
      return;
    }

    sf_close(sndfile);

    _num_ch = sf_info.channels;
    _num_frames = sf_info.frames;
    _num_slices = _num_frames / SLICE_SIZE;

    _slices = (Slice*)calloc(_num_slices, SLICE_SIZE*_num_ch*sizeof(float));

    _is_valid = true;
    g_sample_providers[STRING_get_qstring(filename)] = this;
  }

  ~SampleProvider(){
    if(_is_valid==false)
      return;

    R_ASSERT_RETURN_IF_FALSE(_slices!=NULL);

    for(int64_t slice_num = 0 ; slice_num < _num_slices ; slice_num++){
      Slice &slice = _slices[slice_num];
      R_ASSERT(slice.num_users==0);
      float *samples = ATOMIC_GET(slice.samples);
      R_ASSERT(samples==NULL);
      free(samples);
    }
    
    free(_slices);

    g_sample_providers.remove(STRING_get_qstring(_filename));
  }

  void inc_users(void){
    _num_users++;
  }

  void dec_users(void){
    R_ASSERT(_num_users>0);
    _num_users--;
    if (_num_users == 0)
      delete this;
  }

  void SPT_fill_slice(SNDFILE *sndfile, Slice &slice, int64_t slice_num){
    int64_t pos = slice_num*SLICE_SIZE;
    
    R_ASSERT_RETURN_IF_FALSE(pos>=0);
    R_ASSERT_RETURN_IF_FALSE(pos<_num_frames);
    
    int64_t num_samples = SLICE_SIZE * _num_ch;
    int64_t num_bytes = num_samples * sizeof(float);

    float interleaved_samples[num_samples];

    bool samples_are_valid = false;

    if (sf_seek(sndfile, pos, SEEK_SET) != pos){
      QString s = STRING_get_qstring(_filename); 
      RT_message("Unable to seek to pos %" PRId64 " in file %s. Max pos: %" PRId64 ". (%s)" , pos, s.toLocal8Bit().constData(), _num_frames, sf_strerror(sndfile));
    } else {
      if (sf_readf_float(sndfile, interleaved_samples, SLICE_SIZE) <=0 ){
        QString s = STRING_get_qstring(_filename); 
        RT_message("Unable to read from pos %" PRId64 " in file %s. Max pos: %" PRId64 ". (%s)", pos, s.toLocal8Bit().constData(), _num_frames, sf_strerror(sndfile));
      } else {
        samples_are_valid = true;
      }
    }

    float *samples = (float*)malloc(num_bytes);

    // convert from interleaved to channels after each other.
    if(false==samples_are_valid){

      memset(samples, 0, num_bytes);

    } else {

      if(_num_ch > 1){
        int read_pos=0;
        for(int i=0;i<SLICE_SIZE;i++){
          for(int ch=0;ch<_num_ch;ch++)
            samples[ch*SLICE_SIZE + i] = interleaved_samples[read_pos++];
        }
      } else {
        memcpy(samples, interleaved_samples, num_bytes);
      }

    }

    ATOMIC_SET(slice.samples, samples);
  }

  void SPT_read_slices(SNDFILE *sndfile, int64_t slice_start, int64_t slice_end, radium::Semaphore *gotit){
    R_ASSERT_RETURN_IF_FALSE(slice_start >= 0);
    R_ASSERT_RETURN_IF_FALSE(slice_end <= _num_slices);
    R_ASSERT_RETURN_IF_FALSE(slice_end > slice_start);

    for(int64_t slice_num = slice_start ; slice_num < slice_end ; slice_num++){
      Slice &slice = _slices[slice_num];

      R_ASSERT(slice.num_users >= 0);

#if !defined(RELEASE)
      if(slice.num_users==0 && ATOMIC_GET(slice.samples) != NULL)
        abort();
      if(slice.num_users > 0 && ATOMIC_GET(slice.samples) == NULL)
        abort();
#endif

      slice.num_users++;

      if (slice.num_users==1){
        R_ASSERT_NON_RELEASE(ATOMIC_GET(slice.samples)==NULL);
        SPT_fill_slice(sndfile, slice, slice_num);
      }
    }

    if (gotit!=NULL){
      fprintf(stderr, "SIGNALLING %p\n", gotit);
      gotit->signal();
    }
  }

  void SPT_release_slice(int64_t slice_num){
    R_ASSERT_RETURN_IF_FALSE(slice_num >= 0);
    R_ASSERT_RETURN_IF_FALSE(slice_num < _num_slices);

    Slice &slice = _slices[slice_num];

    R_ASSERT_RETURN_IF_FALSE(slice.num_users > 0);

    slice.num_users--;

    if(slice.num_users==0){
      float *samples = ATOMIC_GET(slice.samples);
      ATOMIC_SET(slice.samples, NULL);
      free(samples);
    }
  }

  float *RT_get_samples(int64_t slice_num){
    R_ASSERT_RETURN_IF_FALSE2(slice_num >= 0, NULL);
    R_ASSERT_RETURN_IF_FALSE2(slice_num < _num_slices, NULL);

    return ATOMIC_GET(_slices[slice_num].samples);
  }
};

static SampleProvider *get_sample_provider(const wchar_t *filename){
  auto *maybe = g_sample_providers[STRING_get_qstring(filename)];
  if (maybe != NULL)
    return maybe;

  auto *provider = new SampleProvider(filename);

  if (provider->_is_valid==false){
    delete provider;
    return NULL;
  }

  return provider;
}

class SampleProviderThread : public QThread {

  SampleProviderThread(const SampleProviderThread&) = delete;
  SampleProviderThread& operator=(const SampleProviderThread&) = delete;

  struct Command{
    SampleProvider *provider; // Is NULL if shutting down
    SNDFILE *sndfile;
    int64_t slice_start;
    int64_t slice_end; // Is -1 if releasing
    radium::Semaphore *gotit;
  };

  radium::Queue<Command, 4096> _queue;

public:

  SampleProviderThread(){
  }

private:

  void run() override {

    while(true){
      Command command = _queue.get();

      if (command.provider==NULL)
        break;

      else if(command.slice_end==-1)
        command.provider->SPT_release_slice(command.slice_start);

      else
        command.provider->SPT_read_slices(command.sndfile, command.slice_start, command.slice_end, command.gotit);
    }
  }

public:

  bool shut_down(void){
    if (isRunning()==false)
      return true;

    Command command = {NULL, NULL, -1, -1, NULL};
    _queue.put(command);

    return wait(2000);
  }

  bool RT_request_read_slices(SampleProvider *provider, SNDFILE *sndfile, int64_t slice_start, int64_t slice_end, radium::Semaphore *gotit){
    Command command = {provider, sndfile, slice_start, slice_end, gotit};
    return _queue.tryPut(command);
  }

  bool RT_request_release_slice(SampleProvider *provider, int64_t slice_num){
    Command command = {provider, NULL, slice_num, -1, NULL};
    return _queue.tryPut(command);
  }
};


static SampleProviderThread g_spt;


class SampleReader {

  friend class SampleProviderThread;

  int64_t _pos;

  int64_t _requested_slice_start = 0;
  int64_t _requested_slice_end = 0;

  SNDFILE *_sndfile = NULL; // Only used by the SPT thread. We keep a separate sndfile instance for each reader in case sndfile caches forward.

  LockAsserter lockAsserter;
  
public:

  SampleProvider *_provider;

  SampleReader(SampleProvider *provider)
    : _provider(provider)
  {
    if (g_spt.isRunning()==false){
      g_spt.start();
      while(g_spt.isRunning()==false)
        msleep(10);
    }

    _provider->inc_users();
  }

  ~SampleReader(){
    if (_sndfile != NULL)
      sf_close(_sndfile);
    _provider->dec_users();
  }

private:

  void wait_for_queue(void){
#if !defined(RELEASE)
    printf("Warning, waiting for queue...\n");
#endif
    R_ASSERT(!PLAYER_current_thread_has_lock());
    msleep(5);
  }

  void request_read(int64_t slice_start, int64_t slice_end, bool wait, radium::Semaphore *gotit, int &num_waitings){
    if (wait) {

      num_waitings++;
      R_ASSERT(num_waitings==1);
      
      while(g_spt.RT_request_read_slices(_provider, _sndfile, slice_start, slice_end, gotit)==false)
        wait_for_queue();
    
    } else {
      
      while(g_spt.RT_request_read_slices(_provider, _sndfile, slice_start, slice_end, NULL)==false)
        wait_for_queue();
      
    }
  }

  void request_read2(int64_t slice_start, int64_t slice_end, int64_t wait_pos, radium::Semaphore *gotit, int &num_waitings){
    if (slice_end <= slice_start){
      R_ASSERT_NON_RELEASE(slice_start==slice_end);
      return;
    }

    if (wait_pos < slice_end) {
      request_read(slice_start, wait_pos, true, gotit, num_waitings);
      request_read(wait_pos, slice_end, false, gotit, num_waitings);
    } else {
      request_read(slice_start, slice_end, true, gotit, num_waitings);
    }

  }

public:

  // This function is either called from the main thread when preparing to play, or the prepare_disk_cache thread when playing
  //
  void prepare_to_play(int64_t pos, int64_t how_much_to_prepare){ // TODO: Remove "how_much_to_prepare". We only need "how_much_to_wait_for". Extra buffering happens in RT_read anyway.

    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    if (_sndfile==NULL)
      _sndfile = _provider->create_sndfile();
    
    if (_sndfile==NULL){
      RT_message("Could not open file %s", STRING_get_chars(_provider->_filename));
      return;
    }

    //bool called_by_preparation_thread;
    /*
    if (pos==-1){
      //called_by_preparation_thread = true;
      pos = _pos;
    } else {
      //called_by_preparation_thread = false;
    }
    */
    R_ASSERT_RETURN_IF_FALSE(pos >= 0);
    
    _pos = pos;

    int64_t wait_slice_num = 1 + (pos+how_much_to_prepare) / SLICE_SIZE;
    
    int64_t new_slice_start = pos / SLICE_SIZE;
    int64_t new_slice_end = R_MIN(_provider->_num_slices, 1 + (pos + how_much_to_prepare) / SLICE_SIZE);
  
    int64_t old_slice_start = _requested_slice_start;
    int64_t old_slice_end = _requested_slice_end;
    
    const RemoveAdd x(new_slice_start, new_slice_end, old_slice_start, old_slice_end);
    

    // Request read
    {
      radium::Semaphore gotit;
      
      bool has_extra = x.a_extra_slice_end > x.a_extra_slice_start;
      bool wait_in_extra = has_extra && wait_slice_num >= x.a_extra_slice_end;
      
      int num_waitings = 0;

      if (wait_in_extra) {
        request_read (x.a_slice_start,       x.a_slice_end,       false,          &gotit, num_waitings);
        request_read2(x.a_extra_slice_start, x.a_extra_slice_end, wait_slice_num, &gotit, num_waitings); // Extra is always after non-extra.
      } else {
        request_read2(x.a_slice_start,       x.a_slice_end,       wait_slice_num, &gotit, num_waitings);
        if (has_extra)
          request_read (x.a_extra_slice_start, x.a_extra_slice_end, false,          &gotit, num_waitings);
      }
      
      fprintf(stderr, "  SEMAPHORE %p: %d\n", &gotit, num_waitings);

      if (num_waitings > 0){
        gotit.wait(num_waitings);
      }
    }

    
    // request release
    {
      for(int64_t i=x.r_slice_start ; i<x.r_slice_end ; i++)
        while(g_spt.RT_request_release_slice(_provider, i)==false)
          wait_for_queue();
      
      for(int64_t i=x.r_extra_slice_start ; i<x.r_extra_slice_end ; i++)
        while(g_spt.RT_request_release_slice(_provider, i)==false)
          wait_for_queue();
    }

    _requested_slice_start = new_slice_start;
    _requested_slice_end = new_slice_end;
  }

private:

  int RT_fill_empty(float **samples, int num_frames) const {
    for(int ch = 0 ; ch<_provider->_num_ch ; ch++)
      memset(samples[ch], 0, sizeof(float)*num_frames);
    
    return num_frames;
  }

public:

  // It probably makes more sense if prepare_to_play was called regulary, than the how_much_to_prepare variable.

  // Fills in samples and returns the number of samples that was filled in.
  int RT_read(float **samples, const int num_ch, const int num_frames, const int64_t how_much_to_prepare){
    
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    int64_t slice_num = _pos / SLICE_SIZE;

    if(slice_num >= _provider->_num_slices || _sndfile==NULL)
      return num_frames;

    float *slice_samples = _provider->RT_get_samples(slice_num);
    if (slice_samples==NULL){
      RT_message("RT_get failed\n");
      return num_frames;
      //return RT_fill_empty(samples, num_frames);
    }

    int slice_frame_pos = _pos - (slice_num * SLICE_SIZE);

    int ret = R_MIN(SLICE_SIZE - slice_frame_pos, num_frames);

    for(int ch=0 ; ch<R_MIN(num_ch, _provider->_num_ch) ; ch++){
      float *from = &slice_samples[ch*SLICE_SIZE + slice_frame_pos];
      for(int i=0;i<ret;i++){
        //fprintf(stderr,"ch: %d. i: %d. from: %p. samples: %p, samples[ch]: %p\n", ch, i, from, samples, samples[ch]);
        //fflush(stderr);
        samples[ch][i] += from[i];
      }
    }

    _pos += ret;

    // Maybe free slice
    {
      int64_t next_slice_num = _pos / SLICE_SIZE;
      R_ASSERT(next_slice_num == slice_num || next_slice_num == slice_num+1); // We never read from more than one slice (even if num_frames > SLICE_SIZE).
      
      for(int64_t slice_num = _requested_slice_start ; slice_num < next_slice_num ; slice_num++){
        
        if (g_spt.RT_request_release_slice(_provider, slice_num)==false) {
          RT_message("Queue full 2");
          break; // try again next time.
        } else {
          _requested_slice_start++;
        }
      }
    }

    // Maybe read in more slices
    {
      int64_t new_slice_end = R_MIN(_provider->_num_slices, 1 + (_pos + how_much_to_prepare) / SLICE_SIZE);

      if (new_slice_end > _requested_slice_end){
        if (g_spt.RT_request_read_slices(_provider, _sndfile, _requested_slice_end, new_slice_end, NULL)==false)
          RT_message("Queue full 1");
        else
          _requested_slice_end = new_slice_end;
      }
    }

    return ret;
  }
  
};

}

using namespace radium;


SampleReader *SAMPLEREADER_create(const wchar_t *filename){  
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return NULL;

  if (provider->_num_frames==0){
    GFX_Message(NULL, "Sample %s contains no samples", STRING_get_chars(filename));
    delete provider;
    return NULL;
  }
  
  return new SampleReader(provider);
}


void SAMPLEREADER_delete(SampleReader *reader){
  delete reader;
}

int SAMPLEREADER_get_num_channels(SampleReader *reader){
  return reader->_provider->_num_ch;
}

int64_t SAMPLEREADER_get_num_frames(SampleReader *reader){
  return reader->_provider->_num_frames;
}

const wchar_t *SAMPLEREADER_get_sample_name(SampleReader *reader){
  return reader->_provider->_filename;
}

void SAMPLEREADER_prepare_to_play(SampleReader *reader, int64_t pos, int64_t how_much_to_prepare){
  reader->prepare_to_play(pos, how_much_to_prepare);
}

int RT_SAMPLEREADER_read(SampleReader *reader, float **samples, int num_ch, int num_frames, int64_t how_much_to_prepare){
  return reader->RT_read(samples, num_ch, num_frames, how_much_to_prepare);
}

void SAMPLEREADER_shut_down(void){
  g_spt.shut_down();
}



#endif
