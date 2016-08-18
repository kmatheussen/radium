/* Copyright 2016 Kjetil S. Matheussen

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




#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.60.0/boost_1_60_0.tar.bz2 ; tar xvjf boost_1_60_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>

#include <sndfile.h>

#include <QThread>
#include <QString>
#include <QDir>
#include <QFile>

#include "../common/nsmtracker.h"
#include "../common/Queue.hpp"
#include "../common/OS_visual_input.h"
#include "../common/OS_string_proc.h"
#include "../common/threading.h"

#include "Sampler_plugin_proc.h"
#include "Mixer_proc.h"
#include "undo_sample_proc.h"

#include "SampleRecorder_proc.h"


namespace{

struct RecordingSlice{
  struct Patch *patch;

  enum {
    Start_Recording,
    Stop_Recording,
    Sample_Data
  } command;

  // if command==Sample_Data
  struct Slice{
    int ch;
    float samples[RADIUM_BLOCKSIZE];
  };
    
  // if command==START_Recording
  struct StartRecording{
    int num_channels;
    const wchar_t *path;
    float middle_note;
  };

  union{
    Slice slice;
    StartRecording start_recording;
  };
};
 
}


#define MAX_QUEUE_SIZE (65536-2)       // boost::lockfree::queue limit

static radium::Queue<RecordingSlice*, MAX_QUEUE_SIZE> *g_recording_slice_queue;

static boost::lockfree::queue<RecordingSlice*, boost::lockfree::capacity<MAX_QUEUE_SIZE> > *g_free_slices;

static RecordingSlice *RT_get_free_slice(void){
  RecordingSlice *slice;
  
  if (g_free_slices->pop(slice)==false){
    RT_message("Error. Recording buffer is full.\nThe recorded sample will either not be recorded at all, or not sound correctly.");
    return NULL;
  }

  return slice;
}

static void put_slice(RecordingSlice *slice){
  if (g_recording_slice_queue->tryPut(slice)==false){
    g_free_slices->bounded_push(slice);
    R_ASSERT(false); // Starting assertion reporter since this is not supposed to happen (both queues have the same size, none of them can overflow the other).
  }
}


namespace{

static double midi_to_hz(float midi){
  if(midi<=0)
    return 0;
  else
    //  return 1;
    return 8.17579891564*(expf(.0577622650*midi));
}

struct RecordingFile{
  struct Patch *patch = NULL;
  SNDFILE *sndfile = NULL;
  QString filename;
  int num_channels;
  float middle_note;
  radium::Vector<RecordingSlice*> non_written_slices;

  QString get_unique_filename(const wchar_t *path, bool &success){
    QString base = STRING_get_qstring(path);

    success=true;

    QString filename;
    
    for(int counter = 1 ; counter <= 100 ; counter++){
      filename = base + " take "+QString::number(counter)+".wav";
      if (!QFile(filename).exists())
        return filename;
    }

    success=false;
    return filename;
  }
  
  RecordingFile(struct Patch *patch, const wchar_t *path, int num_channels, float middle_note)
    : patch(patch)
    , num_channels(num_channels)
    , middle_note(middle_note)
  {
    SF_INFO sf_info = {};
    if (middle_note < 0.01)
      middle_note = 0.01;
    sf_info.samplerate = MIXER_get_sample_rate() * midi_to_hz(48)/midi_to_hz(middle_note);
    sf_info.channels = num_channels;
    sf_info.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;

    bool success;
    filename = get_unique_filename(path, success);

    if (success==false){
      RT_message("Unable to create new file in \"%s\".\nPerhaps you have more than 100 takes there?",STRING_get_qstring(path).toUtf8().constData());
      return;
    }
    
    sndfile = sf_open(filename.toUtf8().constData(), SFM_WRITE, &sf_info);

    if(sndfile==NULL)
      RT_message("Unable to create file \"%s\": %s",filename.toUtf8().constData(), sf_strerror(NULL));
  }

  bool close(void){
    R_ASSERT(non_written_slices.is_empty());
    
    int close_result = sf_close(sndfile);

    if(close_result!=0){
      RT_message("Unable to close sound file \"%s\": %s",filename.toUtf8().constData(), sf_error_number(close_result));
      return false;
    }

    return true;
  }

private:
  void flush(void){
    R_ASSERT_RETURN_IF_FALSE(sndfile!=NULL);

    R_ASSERT_RETURN_IF_FALSE(!non_written_slices.is_empty());
    
    float *channels[num_channels];
    
    for(auto *slice : non_written_slices)
      channels[slice->slice.ch] = &slice->slice.samples[0];

    float interleaved_data[RADIUM_BLOCKSIZE*num_channels];

    int pos=0;
    for(int i=0;i<RADIUM_BLOCKSIZE;i++)
      for(int ch=0;ch<num_channels;ch++)
        interleaved_data[pos++] = channels[ch]==NULL ? 0.0f : channels[ch][i]; // channels[ch]==NULL is not supposed to happen, but things are semi-messy, so we add the check just in case. (assertion window is shown in RT_put_slice if buffer couldn't be written to the queue.

    int num_written_frames = sf_writef_float(sndfile, interleaved_data, RADIUM_BLOCKSIZE);
    if(num_written_frames != RADIUM_BLOCKSIZE)
      RT_message("Unable to write all data to file \"%s\": %d - %s",filename.toUtf8().constData(), num_written_frames, sf_strerror(sndfile));

    for(auto *slice : non_written_slices)
      g_free_slices->bounded_push(slice);

    non_written_slices.clear();
  }

public:
  void treat_slice(RecordingSlice *slice){
    if (sndfile==NULL)
      return;

    non_written_slices.push_back(slice);

    if (slice->slice.ch==num_channels-1)
      flush();
  }
  
};
  
struct SampleRecorderThread : public QThread {

  SampleRecorderThread(const SampleRecorderThread&) = delete;
  SampleRecorderThread& operator=(const SampleRecorderThread&) = delete;

  radium::Queue<RecordingFile*, 128> recorded_files;
  radium::Vector<RecordingFile*>     recording_files;
  
public:

  SampleRecorderThread(){
  }

  RecordingFile *get_recording_file(struct Patch *patch){
    for(auto *recording_file : recording_files)
      if (recording_file->patch==patch)
        return recording_file;

    return NULL;
  }
  
  void start_recording(struct Patch *patch, const wchar_t *path, int num_channels, float middle_note){    
    recording_files.push_back(new RecordingFile(patch, path, num_channels, middle_note));
  }
  
  void stop_recording(struct Patch *patch){
    RecordingFile *recording_file = get_recording_file(patch);

    if (recording_file==NULL)
      return;

    recording_files.remove(recording_file);

    if (recording_file->close())
      recorded_files.put(recording_file);    
    else
      delete recording_file;
  }
  
  void treat_slice(struct Patch *patch, struct RecordingSlice *slice){
    RecordingFile *recording_file = get_recording_file(patch);

    if (recording_file==NULL)
      g_free_slices->bounded_push(slice);
    else
      recording_file->treat_slice(slice);
  }
  
  void run() override {

    while(true){
      RecordingSlice *slice = g_recording_slice_queue->get();
      //printf("got slice: %p\n",slice);
      
      switch(slice->command){
        
        case RecordingSlice::Start_Recording:
          start_recording(slice->patch, slice->start_recording.path, slice->start_recording.num_channels, slice->start_recording.middle_note);
          g_free_slices->bounded_push(slice);
          break;
          
        case RecordingSlice::Stop_Recording:
          stop_recording(slice->patch);
          g_free_slices->bounded_push(slice);
          break;
          
        case RecordingSlice::Sample_Data:
          treat_slice(slice->patch, slice);
          break;
      }
    }
  }
};
 
} // end anon. namespace


static SampleRecorderThread g_sample_recorder_thread;


void SampleRecorder_called_regularly(void){
  R_ASSERT(THREADING_is_main_thread());

  while(true){
    bool gotit;
    RecordingFile *recorded_file = g_sample_recorder_thread.recorded_files.tryGet(gotit);

    if(!gotit)
      break;

    if (recorded_file->sndfile != NULL){
      ADD_UNDO(Sample_CurrPos(recorded_file->patch));
    
      SAMPLER_set_new_sample((SoundPlugin*)recorded_file->patch->patchdata,
                             STRING_create(recorded_file->filename),
                             0);
    }
    
    delete recorded_file;
  }
}


void RT_SampleRecorder_start_recording(struct Patch *patch, const wchar_t *path, int num_channels, float middle_note){
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
  R_ASSERT_RETURN_IF_FALSE(path!=NULL);
  R_ASSERT_RETURN_IF_FALSE(num_channels>0);

  QDir dir = QFileInfo(STRING_get_qstring(path)).dir();
  if (dir.exists()==false){
    RT_message("Error. Could not find the directory \"%s\".\n", dir.absolutePath().toUtf8().constData());
    return;
  }
    

  RecordingSlice *slice = RT_get_free_slice();

  if(slice==NULL)
    return;

  slice->patch = patch;
  slice->command = RecordingSlice::Start_Recording;
  slice->start_recording.num_channels = num_channels;
  slice->start_recording.path = path;
  slice->start_recording.middle_note = middle_note;

  put_slice(slice);
}

void RT_SampleRecorder_stop_recording(struct Patch *patch){
  R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

  RecordingSlice *slice;
  while(g_free_slices->pop(slice)==false); // buzy looping. Close message must be sent.
  
  slice->patch = patch;
  slice->command = RecordingSlice::Stop_Recording;

  put_slice(slice);
}

void RT_SampleRecorder_add_audio(struct Patch *patch, float **audio, int num_channels){
  RecordingSlice *slices[num_channels];

  for(int ch=0;ch<num_channels;ch++){
    slices[ch] = RT_get_free_slice();
    
    if (slices[ch]==NULL){
      for(int c=0;c<ch;c++)
        g_free_slices->bounded_push(slices[c]);
      
      return;
    }
    
  }
        
  for(int ch=0;ch<num_channels;ch++){
    RecordingSlice *slice = slices[ch];

    slice->patch = patch;
    slice->command = RecordingSlice::Sample_Data;
    slice->slice.ch = ch;
    memcpy(slice->slice.samples, audio[ch], sizeof(float)*RADIUM_BLOCKSIZE);

    put_slice(slice);
  }
}

void SampleRecorder_Init(void){
  g_recording_slice_queue = new radium::Queue<RecordingSlice*, MAX_QUEUE_SIZE>;
  g_free_slices = new boost::lockfree::queue<RecordingSlice*, boost::lockfree::capacity<MAX_QUEUE_SIZE> >;

  RecordingSlice *slices = (RecordingSlice*)V_calloc(MAX_QUEUE_SIZE, sizeof(RecordingSlice));

  for(int i=0;i<MAX_QUEUE_SIZE;i++)
    g_free_slices->push(&slices[i]);

  g_sample_recorder_thread.start();
}

