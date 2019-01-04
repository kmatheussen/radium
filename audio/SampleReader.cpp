/* Copyright 2017-2018 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#include <stdio.h>
#include <stdint.h>

#if defined(RELEASE)
#  define DO_DEBUG_PRINTING 0
#else
#  define DO_DEBUG_PRINTING 0
#endif

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
    //printf("%d %d\n", new_end <= old_start, old_end <= new_start);

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
    
      if (new_end >= old_end){
        /*     ooo
         *   nnnnnnn
         */
        //printf("a\n");
        a_slice_start = new_start;
        a_slice_end = old_start;
        
        a_extra_slice_start = old_end;
        a_extra_slice_end = new_end;
      } else if (old_end >= old_start) {
        /*     oooooo
         *   nnnnn
         */
        //printf("b\n");
        a_slice_start = new_start;
        a_slice_end = old_start;

        r_slice_start = new_end;
        r_slice_end = old_end;
      }
      
    } else if (old_start <= new_start) {

      if (old_end >= new_end ) {
        /*   ooooooo
         *     nnn
         */
        //printf("c\n");
        r_slice_start = old_start;
        r_slice_end = new_start;
        
        r_extra_slice_start = new_end;
        r_extra_slice_end = old_end;
      } else if (new_end >= old_end) {
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

// g++ SampleReader.cpp -DTEST_MAIN -Wall -std=gnu++11 -g && ./a.out 

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

  {RemoveAdd x(0,2,0,1);
    assert(x.r_slice_start==x.r_slice_end);
    assert(x.a_slice_start==1);
    assert(x.a_slice_end==2);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);}
  {RemoveAdd x(0,2,1,2);
    assert(x.r_slice_start==x.r_slice_end);
    assert(x.a_slice_start==0);
    assert(x.a_slice_end==1);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);}

  {RemoveAdd x(0,1,0,2);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_extra_slice_start==1);
    assert(x.r_extra_slice_end==2);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_slice_start==x.r_slice_end);}
  {RemoveAdd x(1,2,0,2);
    assert(x.a_slice_start==x.a_slice_end);
    assert(x.r_slice_start==0);
    assert(x.r_slice_end==1);
    assert(x.a_extra_slice_start==x.a_extra_slice_end);
    assert(x.r_extra_slice_start==x.r_extra_slice_end);}

  return 0;
}

#else

#define __STDC_FORMAT_MACROS 1

#include <inttypes.h>

#include <QHash>
#include <QThread>
#include <QFileInfo>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/Queue.hpp"
#include "../common/OS_visual_input.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/LockAsserter.hpp"

#include "../Qt/Qt_colors_proc.h"

#include "../api/api_proc.h"

#include "Peaks.hpp"


#include "SampleReader_proc.h"


static void SAMPLEREADER_mark_ready_for_deletion(radium::SampleReader *reader);

int g_sample_reader_filenames_generation = 0;

namespace radium {

namespace{


class SampleProvider;
static radium::Mutex g_sample_providers_mutex; // Obtained when accessing g_sample_providers from a different thread than the main thread.
static QHash<QString, SampleProvider*> g_sample_providers;

  
struct SliceBuffer{
  struct SliceBuffer *next;
  float samples[SLICE_SIZE];
};

static float g_empty_slicebuffer_buffer[SLICE_SIZE] = {};
 
static SliceBuffer *g_slicebuffers = NULL;

static SliceBuffer *STP_create_SliceBuffer_rec(SliceBuffer *slicebuffer, int num_ch_left){
  if (num_ch_left==0)
    return slicebuffer;
  
  SliceBuffer *new_slicebuffer = g_slicebuffers;

  if (new_slicebuffer==NULL)
    new_slicebuffer = (SliceBuffer*)V_calloc(1, sizeof(SliceBuffer)); // Using V_calloc to allocate to avoid getting copy-on-write memory and other types of lazily allocated memory.
  else
    g_slicebuffers = g_slicebuffers->next;
  
  new_slicebuffer->next = slicebuffer;

  return STP_create_SliceBuffer_rec(new_slicebuffer, num_ch_left-1);
}

static SliceBuffer *STP_create_SliceBuffer(int num_ch){
  return STP_create_SliceBuffer_rec(NULL, num_ch);
}
   
static void STP_release_SliceBuffers(SliceBuffer *slicebuffers){
  if (slicebuffers==NULL)
    return;
  
  STP_release_SliceBuffers(slicebuffers->next);
  
  slicebuffers->next = g_slicebuffers;
  g_slicebuffers = slicebuffers;
}

struct Slice{
  DEFINE_ATOMIC(struct SliceBuffer*, slicebuffers); // Contains a linked list of slicebuffers. Length of the list is the same as the number channels in file.
  int num_users; // Only read by the SPT thread.
};

 

// Note: The destructor is called from the g_spt thread.
struct SampleProviderClient{

  SampleProvider *_provider;
  
  SNDFILE *_sndfile = NULL; // Only used by the SPT thread (no one else are allowed to access it). We keep a separate sndfile instance for each reader in case sndfile (or something deeper down) caches forward.

  SampleReader *_reader;
  
  SampleProviderClient(SampleProvider *provider, SampleReader *reader)
    : _provider(provider)
    , _reader(reader)
  {}
};
    
 


class SampleProvider{

public:

  int _num_ch;
  double _samplerate;
  int64_t _num_slices;
  int64_t _num_frames;

  bool _is_valid = false;

  unsigned int _color;

  bool _can_be_deleted = false;
  bool _file_can_be_deleted = false; // Recorded files should be deleted when unavailable.

  enum WhatToDoWithDeletableFileWhenLoadingOrQuitting _what_to_do_with_deletable_file_when_loading_or_quitting = WTT_DONT_KNOW;
  
private:

  int _num_users = 0;

  Slice *_slices;

public:

  const wchar_t *_filename;
  const wchar_t *_filename_without_path;

  SNDFILE *create_sndfile2(SF_INFO *sf_info){
    return radium_sf_open(_filename,SFM_READ,sf_info);
  }

  SNDFILE *create_sndfile(void){
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    return create_sndfile2(&sf_info);
  }

  // Note: Probably no need to optimize this constructor (by lazily allocating what we need).
  // (This is the Provider (shared between readers), not the Reader.)
  SampleProvider(const wchar_t *filename)
    : _filename(wcsdup(filename))
  {
    {
      QFileInfo info(STRING_get_qstring(filename));
      _filename_without_path = wcsdup(STRING_create(info.fileName()));
    }
    
    {
      /*
      QColor color(GFX_MakeRandomColor());
      color = color.lighter(170);
      //QColor color = get_qcolor(SEQUENCER_BLOCK_DEFAULT_AUDIO_FILE_COLOR_NUM);
      _color = color.rgb();
      */
      _color = GFX_MakeRandomBlockColor();
    }
    
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));
    SNDFILE *sndfile = create_sndfile2(&sf_info);
    if (sndfile==NULL){
      GFX_addMessage("Could not open file \"%S\"", filename);
      return;
    }

    sf_close(sndfile);

    _num_ch = sf_info.channels;
    _samplerate = sf_info.samplerate;
    _num_frames = sf_info.frames;
    _num_slices = ceil((double)_num_frames / (double)SLICE_SIZE);

    _slices = (Slice*)V_calloc(_num_slices, sizeof(Slice)); //SLICE_SIZE*_num_ch*sizeof(float));

    _is_valid = true;
    {
      radium::ScopedMutex lock(g_sample_providers_mutex);
      g_sample_providers[STRING_get_qstring(filename)] = this;
      g_sample_reader_filenames_generation++;
    }
  }

  ~SampleProvider(){
    R_ASSERT(THREADING_is_main_thread());
    
    if(_is_valid==false)
      return;

    R_ASSERT_RETURN_IF_FALSE(_slices!=NULL);

    for(int64_t slice_num = 0 ; slice_num < _num_slices ; slice_num++){
      Slice &slice = _slices[slice_num];
      R_ASSERT(slice.num_users==0);

      SliceBuffer *slicebuffers = ATOMIC_GET(slice.slicebuffers);
      R_ASSERT(slicebuffers==NULL);
      // Memory leak here if slicebuffers!=NULL. Something is very wrong if that happens though, so best not to free anything.
    }
    
    V_free(_slices);

    {
      radium::ScopedMutex lock(g_sample_providers_mutex);
      g_sample_providers.remove(STRING_get_qstring(_filename));
      g_sample_reader_filenames_generation++;
    }

    BS_UpdateBlockList();
    
    if (_file_can_be_deleted)
      force_file_deletion();
    
    free((void*)_filename);
    free((void*)_filename_without_path);

  }

  void force_file_deletion(void){
    
    bool do_delete = false;
    
    if (_what_to_do_with_deletable_file_when_loading_or_quitting != WTT_DONT_KNOW){

      if (_what_to_do_with_deletable_file_when_loading_or_quitting == WTT_DELETE)      
        do_delete = true;
      
    } else {
      
      if (doAutoDeleteSequencerRecordings())
        do_delete = true;
      
    }

    //printf("\n\n\n====================== Maybe deleting \"%S\". Going to? (i.e. allowed by user?): %d. ===================\n\n\n", _filename, do_delete);

    if (do_delete){
      DISK_delete_file(_filename);
      DISKPEAKS_delete_file(_filename);
    }
  }

  bool delete_me_if_no_users(void){
    if (_num_users==0){
      
      delete this;
      return true;
      
    } else {

      return false;
      
    }
  }
  
  void inc_users(void){
    R_ASSERT(THREADING_is_main_thread());
    
    _num_users++;
  }

  void dec_users(void){
    R_ASSERT(THREADING_is_main_thread());
    
    R_ASSERT_RETURN_IF_FALSE(_num_users>0);

    _num_users--;
    
    // Not much point. Commented out so that the color doesn't change if deleting and adding later.
    //if (_num_users == 0)
    //  delete this;
    
    if (_can_be_deleted)
      delete_me_if_no_users();
  }

  int num_users(void) const{
    return _num_users;
  }
  
  bool mark_as_deletable_and_delete_if_unused(void){
    R_ASSERT_RETURN_IF_FALSE2(_can_be_deleted==false, false);

    _can_be_deleted=true;
    return delete_me_if_no_users();
  }
  
  void SPT_fill_slice(SNDFILE *sndfile, Slice &slice, int64_t slice_num){
    int64_t pos = slice_num*SLICE_SIZE;

    //printf("Obtaining slice %d\n", (int)slice_num);
    
    R_ASSERT_RETURN_IF_FALSE(pos>=0);
    R_ASSERT_RETURN_IF_FALSE(pos<_num_frames);
    
    SliceBuffer *slicebuffer = STP_create_SliceBuffer(_num_ch);
    R_ASSERT_RETURN_IF_FALSE(slicebuffer->samples!=NULL);
    
    float interleaved_samples[SLICE_SIZE * _num_ch];

    bool samples_are_valid = false;

    //RT_message("Testing RT message. pos %" PRId64 " in file %S. Max pos: %" PRId64 ". (%s)" , pos, _filename, _num_frames, sf_strerror(sndfile));
    
    if (sndfile != NULL){
      
      if (sf_seek(sndfile, pos, SEEK_SET) != pos){

        RT_message("Unable to seek to pos %" PRId64 " in file %S. Max pos: %" PRId64 ". (%s)" , pos, _filename, _num_frames, sf_strerror(sndfile));
        
      } else {
        
        float *buffer = _num_ch==1 ? slicebuffer->samples : interleaved_samples;
        
        int num_frames_read = sf_readf_float(sndfile, buffer, SLICE_SIZE);
        
        if (num_frames_read <= 0){
          
          RT_message("Unable to read from pos %" PRId64 " in file %S. Max pos: %" PRId64 ". (%s)", pos, _filename, _num_frames, sf_strerror(sndfile));

        } else {

          if (num_frames_read < SLICE_SIZE) {

            if (slice_num != _num_slices-1)            
              RT_message("Unable to read full slice from pos %" PRId64 " in file %S. Slice %d / %d. Number of frames read: %d. Max pos: %" PRId64 ". (%s)",
                         pos, _filename,
                         (int)num_frames_read, (int)slice_num, (int)_num_slices,
                         _num_frames, sf_strerror(sndfile));
              
            // reached end of file. (unless there was an error)
            memset(buffer + (num_frames_read*_num_ch), 0, sizeof(float)*(SLICE_SIZE-num_frames_read)*_num_ch);

          }
          
          samples_are_valid = true;
          
        }
          
      }
        
    }


    SliceBuffer *sb = slicebuffer;
    for(int ch=0 ; ch<_num_ch ; ch++){
      
      float *samples = sb->samples;
      
      if (samples==NULL){
        
        R_ASSERT(false);
        
      } else if (samples_are_valid == false) {
        
        memset(samples, 0, sizeof(float) * SLICE_SIZE);
        
      } else if (_num_ch > 1) {
        
        // convert from interleaved to non-interleaved.
        
        const int num_ch = _num_ch;
        
        int read_pos=ch;
        
        for(int i=0;i<SLICE_SIZE;i++){
          samples[i] = interleaved_samples[read_pos];
          read_pos += num_ch;
        }
        
        sb = sb->next;
        
        if(ch==_num_ch-1)
          R_ASSERT(sb==NULL);
      }      
    }
      

    ATOMIC_SET(slice.slicebuffers, slicebuffer);
  }

  void SPT_obtain_slices(SampleProviderClient *client, int64_t slice_start, int64_t slice_end, radium::FutureSignalTrackingSemaphore *gotit){
    R_ASSERT_RETURN_IF_FALSE(slice_start >= 0);
    R_ASSERT_RETURN_IF_FALSE(slice_end <= _num_slices);
    R_ASSERT_RETURN_IF_FALSE(slice_end > slice_start);

#if DO_DEBUG_PRINTING
    int64_t read_slice_start=-1;
    int64_t read_slice_end=-1;
#endif
    
    for(int64_t slice_num = slice_start ; slice_num < slice_end ; slice_num++){
      Slice &slice = _slices[slice_num];

      R_ASSERT(slice.num_users >= 0);

#if !defined(RELEASE)
      if(slice.num_users==0 && ATOMIC_GET(slice.slicebuffers) != NULL)
        abort();
      if(slice.num_users > 0 && ATOMIC_GET(slice.slicebuffers) == NULL)
        abort();
#endif

      slice.num_users++;

      if (slice.num_users==1){
        R_ASSERT_NON_RELEASE(ATOMIC_GET(slice.slicebuffers)==NULL);

#if DO_DEBUG_PRINTING
        if (read_slice_start==-1)
          read_slice_start = slice_num;
        read_slice_end = slice_num;
#endif
        
        if (client->_sndfile==NULL){
          client->_sndfile = client->_provider->create_sndfile();
          if (client->_sndfile==NULL)
            RT_message("Could not open file \"%S\"", client->_provider->_filename);
        }

        SPT_fill_slice(client->_sndfile, slice, slice_num);          
      }
    }

#if DO_DEBUG_PRINTING
    if (read_slice_start != -1)
      printf("     >>>> %S: Obtain audio data from %f to %f (slice %d -> %d (%d -> %d))\n", client->_provider->_filename_without_path, double(read_slice_start*SLICE_SIZE)/pc->pfreq, double((1+read_slice_end)*SLICE_SIZE)/pc->pfreq, (int)read_slice_start, (int)read_slice_end, (int)slice_start, (int)slice_end);
#endif
    
    if (gotit!=NULL){
      //fprintf(stderr, "SIGNALLING %p\n", gotit);
      gotit->signal();
    }
  }

  void SPT_release_slices(SampleProviderClient *client, int64_t slice_start, int64_t slice_end){
    R_ASSERT_RETURN_IF_FALSE(slice_start >= 0);
    R_ASSERT_RETURN_IF_FALSE(slice_end <= _num_slices);
    R_ASSERT_RETURN_IF_FALSE(slice_end > slice_start);

#if DO_DEBUG_PRINTING
    printf("      <<<< %S: Release audio data from %f to %f (slice %d -> %d)\n",  client->_provider->_filename_without_path, double(slice_start*SLICE_SIZE)/pc->pfreq, double((1+slice_end)*SLICE_SIZE)/pc->pfreq, (int)slice_start, (int)slice_end);
#endif

    for(int64_t slice_num = slice_start ; slice_num < slice_end ; slice_num++){
      Slice &slice = _slices[slice_num];

#if !defined(RELEASE)
      if(slice.num_users==0)
        abort();
#endif
      R_ASSERT_RETURN_IF_FALSE(slice.num_users > 0);

      slice.num_users--;
      
      if(slice.num_users==0){
        auto *slicebuffers = ATOMIC_GET(slice.slicebuffers);
        R_ASSERT(slicebuffers != NULL);
        
        ATOMIC_SET(slice.slicebuffers, NULL);
        STP_release_SliceBuffers(slicebuffers);
      }
    }
  }

  SliceBuffer *RT_get_slicebuffers(int64_t slice_num){
    R_ASSERT_RETURN_IF_FALSE2(slice_num >= 0, NULL);
    R_ASSERT_RETURN_IF_FALSE2(slice_num < _num_slices, NULL);

    return ATOMIC_GET(_slices[slice_num].slicebuffers);
  }
};

static SampleProvider *get_sample_provider(const wchar_t *filename){
  R_ASSERT(THREADING_is_main_thread());

  QString key = STRING_get_qstring(filename);
  auto *maybe = g_sample_providers.value(key);
  if (maybe!=NULL)
    return maybe;
  
  R_ASSERT_NON_RELEASE(g_sample_providers.contains(key)==false);
  
  auto *provider = new SampleProvider(filename);

  if (provider->_is_valid==false){
    delete provider;
    return NULL;
  }

  BS_UpdateBlockList();
  
  return provider;
}


class SampleProviderThread : public QThread {

  SampleProviderThread(const SampleProviderThread&) = delete;
  SampleProviderThread& operator=(const SampleProviderThread&) = delete;

  struct Command{
    enum class Type{
      SHUT_DOWN,
      OBTAIN,
      RELEASE_,
      CLOSE_SNDFILE_,
      DELETE_,
    } type;
    SampleProviderClient *client;
    int64_t slice_start;
    int64_t slice_end;
    radium::FutureSignalTrackingSemaphore *gotit;
  };

  radium::Queue<Command, 4096> _queue; // Should have more than one queue.

public:

  SampleProviderThread(){
  }

  int size(void){
    return _queue.size();
  }

private:

  void run() override {

    while(true){
      Command command = _queue.get();

      switch(command.type){
      case Command::Type::SHUT_DOWN:
        return;

      case Command::Type::OBTAIN:
        command.client->_provider->SPT_obtain_slices(command.client, command.slice_start, command.slice_end, command.gotit);
        break;

      case Command::Type::RELEASE_:
        command.client->_provider->SPT_release_slices(command.client, command.slice_start, command.slice_end);
        break;

      case Command::Type::CLOSE_SNDFILE_:
      case Command::Type::DELETE_:
        //printf("Finished for now %p. gotit: %p\n", command.client, command.gotit);

        if (command.client->_sndfile != NULL){
          sf_close(command.client->_sndfile); // Lower the number of simultaneously open file handlers.        
          command.client->_sndfile = NULL;
        }
        
        if (command.type==Command::Type::DELETE_){ // i.e. command.type != Command::Type::CLOSE_SNDFILE_:
          
          R_ASSERT(command.gotit==NULL);
          
          SAMPLEREADER_mark_ready_for_deletion(command.client->_reader);

        } else if (command.type==Command::Type::CLOSE_SNDFILE_) {
          
          if (command.gotit!=NULL){
            fprintf(stderr, "CLOSE_SNDFILE_ SIGNALLING %p\n", command.gotit);
            command.gotit->signal();
          }

        } else {
          
          R_ASSERT(false);
          
        }
        
        break;
      }
    }
  }

public:

  bool shut_down(void){
    if (isRunning()==false)
      return true;

    Command command = {SampleProviderThread::Command::Type::SHUT_DOWN, NULL, -1, -1, NULL};
    _queue.put(command);

#if defined(RELEASE)
    return wait(2000);
#else
    return wait();
#endif
  }

  bool RT_request_obtain_slices(SampleProviderClient *client, int64_t slice_start, int64_t slice_end, radium::FutureSignalTrackingSemaphore *gotit){
    if(slice_start==slice_end)
      return true;
    
    R_ASSERT_NON_RELEASE(slice_end>slice_start);
    R_ASSERT_NON_RELEASE(slice_end<=client->_provider->_num_slices);
    
    Command command = {SampleProviderThread::Command::Type::OBTAIN, client, slice_start, slice_end, gotit};
#if DO_DEBUG_PRINTING
    printf("..........Req.Obtain %p (%d -> %d)\n", command.client, (int)command.slice_start, (int)command.slice_end);
#endif
    return _queue.tryPut(command);
  }

  bool RT_request_release_slices(SampleProviderClient *client, int64_t slice_start, int64_t slice_end){
    if(slice_start==slice_end)
      return true;
    
    R_ASSERT_NON_RELEASE(slice_end>slice_start);
    
    //printf("Req release %p\n", client);

    Command command = {SampleProviderThread::Command::Type::RELEASE_, client, slice_start, slice_end, NULL};
#if DO_DEBUG_PRINTING
    printf("..........Req.Release %p (%d -> %d)\n%s\n", command.client, (int)command.slice_start, (int)command.slice_end, "");//JUCE_get_backtrace());
#endif
    return _queue.tryPut(command);
  }

  bool RT_request_close_sndfile(SampleProviderClient *client, radium::FutureSignalTrackingSemaphore *gotit){
    //if (client->_sndfile == NULL)
    //  return true;

    //printf("Req clear_cache %p\n", client);
    R_ASSERT_RETURN_IF_FALSE2(gotit!=NULL, true);

    Command command = {SampleProviderThread::Command::Type::CLOSE_SNDFILE_, client, -1, -1, gotit};
    return _queue.tryPut(command);
  }

  bool RT_request_delete(SampleProviderClient *client){
    Command command = {SampleProviderThread::Command::Type::DELETE_, client, -1, -1, NULL};
    return _queue.tryPut(command);
  }
};

}
  

static SampleProviderThread g_spt;


// Note: The destructor is called from the g_spt thread.
class SampleReader : public SampleProviderClient{

  int64_t _pos;
  int64_t *_ch_pos;

  int64_t _first_slice = 0;
  int64_t _first_slice2 = 0;

  bool _pos_used = false;
  bool _ch_pos_used = false;
  
  int64_t _requested_slice_start = 0;
  int64_t _requested_slice_end = 0;

  LockAsserter lockAsserter;

public:

  bool _has_requested_deletion = false;
  
  const int _num_ch;
  const double _samplerate;

  SampleReader(SampleProvider *provider)
    : SampleProviderClient(provider, this)
    , _ch_pos((int64_t*)V_calloc(provider->_num_ch, sizeof(int64_t)))
    , _num_ch(provider->_num_ch)
    , _samplerate(provider->_samplerate)
  {
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    if (g_spt.isRunning()==false){
      g_spt.start();
      while(g_spt.isRunning()==false)
        msleep(10);
    }

    //printf("     =========== SampleReader Constructor: %p %d\n", this, _provider->num_users()+1);
    
    _provider->inc_users();    
  }

  // Called from the g_spt thread.
  ~SampleReader(){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    
    R_ASSERT(_has_requested_deletion);
    
    //printf("     =========== SampleReader Destructor: %p %d\n", this, _provider->num_users()-1);
    
    _provider->dec_users();

    V_free((void*)_ch_pos);
  }

  // Called from the main thread.
  void request_deletion(void){
    {
      LOCKASSERTER_EXCLUSIVE(&lockAsserter);
      
      release_permanent_slices();

      if(release_all_cached_data(true)){
#if DO_DEBUG_PRINTING
        printf("    request_deletion for %p\n", this);
#endif
      }

      _has_requested_deletion=true; // Must do this before calling "g_spt.RT_request_delete" since 'this' can be deleted at any time after that call.
    }
    
    g_spt.RT_request_delete(this);  // Moved outside LOCKASSERTER_EXCLUSIVE since, theoretically, the desctructor can be called before 'request_deletion' has finished.
  }
  
private:

  bool has_waited_for_queue = false;
  
  void wait_for_queue(void){
#if !defined(RELEASE)
    printf("Warning, waiting for queue...\n");
#endif
    has_waited_for_queue = true;
    R_ASSERT(!PLAYER_current_thread_has_lock());
    msleep(5);
  }

  void request_obtain(int64_t slice_start, int64_t slice_end, bool wait, radium::FutureSignalTrackingSemaphore *gotit){
    if (wait) {

      gotit->is_going_to_be_signalled_another_time_in_the_future();
        
      while(g_spt.RT_request_obtain_slices(this, slice_start, slice_end, gotit)==false)
        wait_for_queue();
    
    } else {
      
      while(g_spt.RT_request_obtain_slices(this, slice_start, slice_end, NULL)==false)
        wait_for_queue();
      
    }
  }

  void request_obtain2(int64_t slice_start, int64_t slice_end, int64_t wait_pos, radium::FutureSignalTrackingSemaphore *gotit){
    if (slice_end <= slice_start){
      R_ASSERT_NON_RELEASE(slice_start==slice_end);
      return;
    }

    if (wait_pos < slice_end) {
      request_obtain(slice_start, wait_pos, true, gotit);
      request_obtain(wait_pos, slice_end, false, gotit);
    } else {
      request_obtain(slice_start, slice_end, true, gotit);
    }

  }

  bool release_all_cached_data(bool sleep_if_queue_is_full){
    if (_requested_slice_start == _requested_slice_end)
      return false;

    while(g_spt.RT_request_release_slices(this, _requested_slice_start, _requested_slice_end)==false){
      if (sleep_if_queue_is_full){
        wait_for_queue();
      }else{
        RT_message("Queue full 3");
        return false;
      }
    }

    //printf("   << RT_RELEASE: %d -> %d ===> %d %d. %p\n", (int)_requested_slice_start, (int)_requested_slice_end, (int)_requested_slice_start, (int)_requested_slice_start, this);
    _requested_slice_end = _requested_slice_start;
    
    return true;
  }

public:

  bool RT_release_all_cached_data(void){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);
    return release_all_cached_data(false);
  }
  
    
private:
  
  bool release_permanent_slices(void){
    if(_first_slice==_first_slice2)
      return false;

    while(g_spt.RT_request_release_slices(this, _first_slice, _first_slice2)==false)
      wait_for_queue();    
    
    _first_slice = 0;
    _first_slice2 = 0;

    return true;
  }

public:
  
  // Called very often
  void set_and_obtain_first_slice(int64_t new_first_slice, int64_t new_first_slice2){
    if (new_first_slice2 > _provider->_num_slices){
      R_ASSERT_NON_RELEASE(new_first_slice2 - 2 <= _provider->_num_slices);
      new_first_slice2 = _provider->_num_slices;
    }

    R_ASSERT_RETURN_IF_FALSE(new_first_slice >= 0);
    R_ASSERT_RETURN_IF_FALSE(new_first_slice2 >= new_first_slice);

    if(new_first_slice==_first_slice && new_first_slice2==_first_slice2)
      return;

    const RemoveAdd x(new_first_slice, new_first_slice2, _first_slice, _first_slice2);

#if DO_DEBUG_PRINTING
    printf("                 set_and_obtain %p.   Old: %d -> %d.  New: %d -> %d. a: %d -> %d. x: %d -> %d\n", this, (int)_first_slice, (int)_first_slice2, (int)new_first_slice, (int)new_first_slice2, (int)x.a_slice_start, (int)x.a_slice_end, (int)x.a_extra_slice_start, (int)x.a_extra_slice_end);
#endif

    request_obtain(x.a_slice_start,       x.a_slice_end,       false,          NULL);
    request_obtain(x.a_extra_slice_start, x.a_extra_slice_end, false,          NULL);


    while(g_spt.RT_request_release_slices(this, x.r_slice_start, x.r_slice_end)==false)
      wait_for_queue();

    while(g_spt.RT_request_release_slices(this, x.r_extra_slice_start, x.r_extra_slice_end)==false)
      wait_for_queue();

    _first_slice = new_first_slice;
    _first_slice2 = new_first_slice2;
  }


  // This function is either called from the main thread when preparing to play
  //
  void prepare_to_play(int64_t pos, int64_t how_much_to_prepare, radium::FutureSignalTrackingSemaphore *gotit){ // TODO: Remove "how_much_to_prepare". We only need "how_much_to_wait_for". Extra buffering happens in RT_read anyway.

    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

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
    for(int ch=0;ch<_num_ch;ch++)
      _ch_pos[ch] = pos;

    _pos_used = false;
    _ch_pos_used = false;
    
    int64_t wait_slice_num = 1 + (pos+how_much_to_prepare) / SLICE_SIZE;
    
    int64_t new_slice_start = pos / SLICE_SIZE;
    int64_t new_slice_end = R_MIN(_provider->_num_slices, 1 + (pos + how_much_to_prepare) / SLICE_SIZE);
  
    int64_t old_slice_start = _requested_slice_start;
    int64_t old_slice_end = _requested_slice_end;
    
    const RemoveAdd x(new_slice_start, new_slice_end, old_slice_start, old_slice_end);
    

    //int num_waitings = 0;
      

    // Request obtain
    {
      bool has_extra = x.a_extra_slice_end > x.a_extra_slice_start;
      bool wait_in_extra = has_extra && wait_slice_num >= x.a_extra_slice_end;
      
      if (wait_in_extra) {
        request_obtain (x.a_slice_start,       x.a_slice_end,       false,          gotit);
        request_obtain2(x.a_extra_slice_start, x.a_extra_slice_end, wait_slice_num, gotit); // Extra is always after non-extra.
      } else {
        request_obtain2(x.a_slice_start,       x.a_slice_end,       wait_slice_num, gotit);
        request_obtain (x.a_extra_slice_start, x.a_extra_slice_end, false,          gotit);
      }
      
      //fprintf(stderr, "  SEMAPHORE %p: %d\n", &gotit, num_waitings);
    }

    
    // request release
    {
      while(g_spt.RT_request_release_slices(this, x.r_slice_start, x.r_slice_end)==false)
        wait_for_queue();

      while(g_spt.RT_request_release_slices(this, x.r_extra_slice_start, x.r_extra_slice_end)==false)
        wait_for_queue();
    }

    //printf("   <<>> Prepare: %d -> %d ===> %d %d. %p\n", (int)_requested_slice_start, (int)_requested_slice_end, (int)new_slice_start, (int)new_slice_end, this);
    _requested_slice_start = new_slice_start;
    _requested_slice_end = new_slice_end;
  }

  int RT_return_empty(float **samples, const int num_frames){
    for(int ch=0 ; ch<_num_ch ; ch++)
      memset(samples[ch], 0, num_frames*sizeof(float));
    
    return num_frames;
  }
  
  float *RT_return_empty2(int &num_frames){
    num_frames = SLICE_SIZE;
    return g_empty_slicebuffer_buffer;
  }
  
public:

  //int _min_pos = 0;
  
  // Call this function now and then if using RT_get_sample instead of RT_read
  void RT_called_per_block(const int64_t how_much_to_prepare){
    
    int64_t min_pos;
    int64_t max_pos;

    if (_pos_used) {
      R_ASSERT(_ch_pos_used==false);
      min_pos = _pos;
      max_pos = _pos;
    } else if (_ch_pos_used){
      min_pos = _ch_pos[0];
      max_pos = _ch_pos[0];
      for(int ch=1; ch<_num_ch ; ch++){
        min_pos = R_MIN(min_pos, _ch_pos[ch]);
        max_pos = R_MAX(max_pos, _ch_pos[ch]);
      }
      min_pos -= SLICE_SIZE; // In this mode, the previously read slice is being used until next reading.
      max_pos -= SLICE_SIZE; // In this mode, the previously read slice is being used until next reading.
    } else
      return;
    
    // Maybe read in more slices
    {
      int64_t new_slice_end = R_MIN(_provider->_num_slices, 1 + (max_pos + how_much_to_prepare) / SLICE_SIZE);

      if (new_slice_end > _requested_slice_end){
        //printf("   >> req end %d -> %d. %p\n", (int)_requested_slice_end, (int)new_slice_end,this);
        if (g_spt.RT_request_obtain_slices(this, _requested_slice_end, new_slice_end, NULL)==false)
          RT_message("Queue full 1b");
        else
          _requested_slice_end = new_slice_end;
        //printf("   <<>> CALL per block. New slice_end: %d\n", (int)new_slice_end);
      }
    }

    // Maybe release slice
    {
      int64_t min_slice_num = min_pos / SLICE_SIZE;
      
      if (min_slice_num > _requested_slice_start){
        //printf("   << req start %d -> %d. %p\n", (int)_requested_slice_start, (int)next_slice_num, this);
        if (g_spt.RT_request_release_slices(this, _requested_slice_start, min_slice_num)==false){
          RT_message("Queue full 2b");
        } else {
          _requested_slice_start = min_slice_num;
        }
      }
    }
  }

  // Returns a buffer that is legal to use until the next call to RT_get_buffer with the same 'ch' argument or a call to prepare_to_play().
  // Can probably not be used at the same time as RT_read
  // Note: Currently not used, but it was used for a long time.
  float *RT_get_buffer(const int ch, int &num_frames){
    int64_t slice_num = _ch_pos[ch] / SLICE_SIZE;

    //printf("   ,,pIcking up slice %d\n", (int)slice_num);

    _ch_pos_used = true;
    
    if(slice_num >= _provider->_num_slices)
      return RT_return_empty2(num_frames);

    // Check this one after slice_num >= _provider->_num_slices, since _sndfile is supposed to be NULL when that happens.
    /*
      Commented out since _sndfile can be NULL if all slices were already there when calling prepare_to_play
    if (_sndfile==NULL){
      R_ASSERT(false);
      return RT_return_empty2(num_frames);
    }
    */
      
    // Assert that we have requested the data before calling.
    if (has_waited_for_queue==false){
      if (slice_num < _requested_slice_start || slice_num >= _requested_slice_end){

        R_ASSERT(slice_num >=  _requested_slice_start);

#if defined(RELEASE)
        R_ASSERT(slice_num < _requested_slice_end+17); // add 17 to avoid slightly wrong value caused by rounding errors.
#else
        R_ASSERT(slice_num < _requested_slice_end+1); // add 1 to avoid slightly wrong value caused by rounding errors.
#endif
        return RT_return_empty2(num_frames);
      }
    }

    SliceBuffer *slicebuffers = _provider->RT_get_slicebuffers(slice_num);
    if (slicebuffers==NULL){
#if !defined(RELEASE)
      abort();
#endif
      RT_message("Couldn't read from disk fast enough. Try increasing the disk buffer.");
      return RT_return_empty2(num_frames);
    }

    _ch_pos[ch] += SLICE_SIZE;

    SliceBuffer *sb = slicebuffers;
    
    for(int i=0;i<ch;i++)
      sb = sb->next;

    num_frames = SLICE_SIZE;
    return sb->samples;
  }


  // Fills in samples and returns the number of samples that was filled in.
  // Can probably not be used at the same time as RT_get_buffer
  int RT_read(float **samples, const int num_frames){
    LOCKASSERTER_EXCLUSIVE(&lockAsserter);

    _pos_used = true;
    
    int64_t slice_num = _pos / SLICE_SIZE;

    //printf("slice_num: %d. num_slices: %d. num_frames: %d\n", (int)slice_num, (int)_provider->_num_slices, (int)_provider->_num_frames);

    if(slice_num >= _provider->_num_slices){
      return RT_return_empty(samples, num_frames);
    }
    
    // Check this one after slice_num >= _provider->_num_slices, since _sndfile is supposed to be NULL when that happens.
    /*
      Commented out since _sndfile can be NULL if all slices were already there when calling prepare_to_play
      if (_sndfile==NULL){
        R_ASSERT(false);
        return RT_return_empty(samples, num_frames, do_add);
      }
    */

    // Assert that we have requested the data before calling.
    if (has_waited_for_queue==false){
      if (slice_num < _requested_slice_start){
        R_ASSERT(false);
        return RT_return_empty(samples, num_frames);
      }
    }
    
    SliceBuffer *slicebuffers = _provider->RT_get_slicebuffers(slice_num);
    if (slicebuffers==NULL){
      RT_message("Couldn't read from disk fast enough. Try increasing the disk buffer.");
      return RT_return_empty(samples, num_frames);
    }

    const int slice_frame_pos = int(_pos - (slice_num * SLICE_SIZE));

    const int ret = R_MIN(SLICE_SIZE - slice_frame_pos, num_frames);

    SliceBuffer *sb = slicebuffers;
    
    for(int ch=0 ; ch<_num_ch ; ch++){
      
      R_ASSERT_RETURN_IF_FALSE2(sb!=NULL, num_frames);

      memcpy(samples[ch], &sb->samples[slice_frame_pos], ret * sizeof(float));

      sb = sb->next;
      if (ch==_num_ch-1)
        R_ASSERT(sb==NULL);
    }
    
    _pos += ret;

    return ret;
  }

};
}

using namespace radium;

// Called very often
void SAMPLEREADER_set_permanent_samples(SampleReader *reader, int64_t first_sample, int64_t first_sample2){
  R_ASSERT(THREADING_is_main_thread());

  // TODO: Have two queues, one high priority, and one low priority. This data would be put on the low priority queue, and then we could remove "if(is_playing_song()==true) return;".
  // Now the user has to stop playing for a while in order to cache the start of the sample, which isn't ideal.
  
  if(is_playing_song()==true)
    return; // Don't cache start of sample while playing other audio files.

  int64_t first_slice = first_sample / SLICE_SIZE;
  int64_t first_slice2 = 1 + (first_sample2 / SLICE_SIZE);
  reader->set_and_obtain_first_slice(first_slice, first_slice2);
}

SampleReader *SAMPLEREADER_create(const wchar_t *filename){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return NULL;

  if (provider->_num_frames==0){
    GFX_Message(NULL, "Audio file \"%S\" contains no samples", filename);
    delete provider;
    return NULL;
  }
  
  return new SampleReader(provider);
}

vector_t SAMPLEREADER_get_all_filenames(void){
  R_ASSERT(THREADING_is_main_thread());
  
  vector_t ret = {};
  for(const auto key : g_sample_providers.keys())
    VECTOR_push_back(&ret, STRING_create(key));

  return ret;
}

bool SAMPLEREADER_remove_filename_from_filenames(const wchar_t *filename){
  SampleProvider *provider = g_sample_providers.value(STRING_get_qstring(filename));
  //R_ASSERT_RETURN_IF_FALSE(provider!=NULL); Commented out since we might have asked to remove a deletable filename, which could have been deleted in between gathering the filename and calling this function.  
  if(provider==NULL)
    return true;
  
  if(provider->_can_be_deleted)
    return false;

  return provider->mark_as_deletable_and_delete_if_unused();
}

vector_t SAMPLEREADER_get_all_deletable_filenames(void){
  vector_t ret = {};
  for(const auto *provider : g_sample_providers.values()){
    R_ASSERT(provider!=NULL);
    if (provider!=NULL && provider->_file_can_be_deleted)
      VECTOR_push_back(&ret, talloc_wcsdup(provider->_filename));
  }
  
  return ret;
}

// Called right after recording file
bool SAMPLEREADER_register_deletable_audio_file(const wchar_t *filename){
  R_ASSERT_RETURN_IF_FALSE2(SAMPLEREADER_has_file(filename)==false, false);

  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return false; // No need for assertion. Error message has already been shown if file couldn't be opened.

  R_ASSERT_RETURN_IF_FALSE2(provider->_file_can_be_deleted==false, false);
  
  provider->_can_be_deleted = true;
  provider->_file_can_be_deleted = true;
  
  return true;
}

bool SAMPLEREADER_is_deletable_audio_file(const wchar_t *filename){
  R_ASSERT_RETURN_IF_FALSE2(SAMPLEREADER_has_file(filename)==true, false);

  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return false; // No need for assertion. Error message has already been shown if file couldn't be opened.

  return provider->_file_can_be_deleted;
}
  
void SAMPLEREADER_mark_what_to_do_with_deletable_file_when_loading_or_quitting(const wchar_t *filename, enum WhatToDoWithDeletableFileWhenLoadingOrQuitting wtt){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return;

  R_ASSERT_NON_RELEASE(provider->_can_be_deleted==true);
  R_ASSERT_NON_RELEASE(provider->_file_can_be_deleted==true);
  
  provider->_what_to_do_with_deletable_file_when_loading_or_quitting = wtt;
}
  
// Called for all used sound files when saving (but not for audio files which are only available through undo/redo).
void SAMPLEREADER_maybe_make_audio_file_undeletable(const wchar_t *filename){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return;

  if (provider->_file_can_be_deleted==true)
    provider->_file_can_be_deleted = false;
}

// Called when quitting.
void SAMPLEREADER_delete_all_deletable_audio_files(void){
  R_ASSERT(THREADING_is_main_thread());

  for(auto *provider : g_sample_providers.values()){
    if(provider==NULL)
      R_ASSERT(false);
    else
      if (provider->_file_can_be_deleted==true)
        provider->force_file_deletion();
  }
}


void SAMPLEREADER_inc_users(const wchar_t *filename){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return;

  provider->inc_users();
}
  
void SAMPLEREADER_dec_users(const wchar_t *filename){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return;

  provider->dec_users();
}

void SAMPLEREADER_dec_users_undo_callback(void *data){
  R_ASSERT(THREADING_is_main_thread());
  SAMPLEREADER_dec_users((const wchar_t*)data);
}
  

// can be called from any thread, but not while holding player lock.
bool SAMPLEREADER_has_file(const wchar_t *filename){
  R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock());
  
  QString key = STRING_get_qstring(filename);

  {
    radium::ScopedMutex lock(g_sample_providers_mutex);

    auto *maybe = g_sample_providers.value(key);
    if (maybe!=NULL)
      return true;

    R_ASSERT(g_sample_providers.contains(key)==false);

    return false;
  }
}
   
 
bool SAMPLEREADER_add_audiofile(const wchar_t *filename){
  return get_sample_provider(filename) != NULL;
}

int64_t SAMPLEREADER_get_sample_duration(const wchar_t *filename){  
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return -1;

  return provider->_num_frames;
}

double SAMPLEREADER_get_samplerate(const wchar_t *filename){
  R_ASSERT(THREADING_is_main_thread());
  
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL)
    return -1;

  return provider->_samplerate;
}


//#include "Juce_plugins_proc.h"
void SAMPLEREADER_delete(SampleReader *reader){
  R_ASSERT(THREADING_is_main_thread());
  
  //printf("Delete:\n%s\n", JUCE_get_backtrace());
  R_ASSERT_RETURN_IF_FALSE(reader->_has_requested_deletion==false);
  
  reader->request_deletion(); // We don't want to simply call "delete reader" here since there might be things in the sample provider thread queue.
}

static radium::Mutex g_readers_ready_for_deletion_mutex;
static QVector<SampleReader*> g_readers_ready_for_deletion;

bool SAMPLEREADER_call_very_often(void){
  SampleReader *reader;

  // Just delete one reader for each visit to avoid blocking the sample provider thread for too long, and also avoid blocking the main thread for too long (since we would probably sleep a little bit between each reader deletion to avoid blocking the sample provider thread for too long)
  {
    radium::ScopedMutex lock(g_readers_ready_for_deletion_mutex);
    if (g_readers_ready_for_deletion.isEmpty())
      return false;
    
    reader = g_readers_ready_for_deletion.takeFirst();
  }

  delete reader;
  return true;
}
  
static void SAMPLEREADER_mark_ready_for_deletion(SampleReader *reader){
  radium::ScopedMutex lock(g_readers_ready_for_deletion_mutex);
  g_readers_ready_for_deletion.push_back(reader);
}
  
int SAMPLEREADER_get_num_channels(SampleReader *reader){
  return reader->_num_ch;
}

double SAMPLEREADER_get_samplerate(SampleReader *reader){
  return reader->_samplerate;
}

int64_t SAMPLEREADER_get_total_num_frames_in_sample(SampleReader *reader){
  return reader->_provider->_num_frames;
}

const wchar_t *SAMPLEREADER_get_sample_name(SampleReader *reader){
  return reader->_provider->_filename_without_path;
}

const wchar_t *SAMPLEREADER_get_filename(SampleReader *reader){
  return reader->_provider->_filename;
}

unsigned int SAMPLEREADER_get_sample_color(const wchar_t *filename){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL){
    GFX_addMessage("Error: No audio file \"%S\" in the system.\n", filename);
    return 0;
  }

  return provider->_color;
}

void SAMPLEREADER_set_sample_color(const wchar_t *filename, unsigned int color){
  SampleProvider *provider = get_sample_provider(filename);
  if (provider==NULL){
    GFX_addMessage("Error: No audio file \"%S\" in the system.\n", filename);
    return;
  }

  provider->_color = color;
}

void SAMPLEREADER_prepare_to_play(SampleReader *reader, int64_t pos, int64_t how_much_to_prepare, radium::FutureSignalTrackingSemaphore *gotit){
  reader->prepare_to_play(pos, how_much_to_prepare, gotit);
}

bool RT_SAMPLEREADER_release_all_cached_data(SampleReader *reader){
  return reader->RT_release_all_cached_data();
}

void RT_SAMPLEREADER_called_per_block(SampleReader *reader, const int64_t how_much_to_prepare_for_next_time){
  reader->RT_called_per_block(how_much_to_prepare_for_next_time);
}
    
float *RT_SAMPLEREADER_get_buffer(SampleReader *reader, const int ch, int &num_frames){
  return reader->RT_get_buffer(ch, num_frames);
}

int RT_SAMPLEREADER_read(SampleReader *reader, float **samples, int num_frames){
  return reader->RT_read(samples, num_frames);
}

void SAMPLEREADER_shut_down(void){
  R_ASSERT(THREADING_is_main_thread());

  g_spt.shut_down();

  while(SAMPLEREADER_call_very_often()); // Drain the g_readers_ready_for_deletion queue. More readers might have been added since last time we did this.
}



#endif
