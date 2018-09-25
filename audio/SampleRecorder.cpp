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



#include <math.h>

#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.63.0/boost_1_63_0.tar.bz2 ; tar xvjf boost_1_63_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>

#include <QThread>
#include <QString>
#include <QDir>
#include <QFile>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"
#include "../common/Queue.hpp"
#include "../common/OS_visual_input.h"
#include "../common/OS_string_proc.h"
#include "../common/threading.h"
#include "../common/Vector.hpp"

#include "Mixer_proc.h"
#include "Juce_plugins_proc.h"
#include "SampleReader_proc.h"

#include "SampleRecorder_proc.h"


namespace{

struct PeakSlice{
  int64_t instance_id; // Can't store the instance itself since it can be deleted before the queue is drained.
  int ch;
  float min;
  float max;
};
  
struct RecordingSlice{
  radium::SampleRecorderInstance *instance;

  enum {
    Start_Recording,
    Stop_Recording,
    Sample_Data,
    Quit
  } command;

  // if command==Sample_Data
  struct Slice{
    int ch;
    int num_frames;
    float samples[RADIUM_BLOCKSIZE];
  };

  Slice slice;
};
 
}


#define MAX_QUEUE_SIZE (65536-2)       // boost::lockfree::queue limit

static radium::Queue<RecordingSlice*, MAX_QUEUE_SIZE> *g_recording_slice_queue;

static boost::lockfree::queue<RecordingSlice*, boost::lockfree::capacity<MAX_QUEUE_SIZE> > *g_free_slices;

static boost::lockfree::queue<PeakSlice, boost::lockfree::capacity<MAX_QUEUE_SIZE> > *g_peak_slice_queue;


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

static DEFINE_ATOMIC(int, g_num_recording_files) = 0;

namespace{

struct RecordingFile{
  radium::SampleRecorderInstance *instance = NULL;
  SNDFILE *sndfile = NULL;
  QString filename;
  radium::Vector<RecordingSlice*> non_written_slices;

  bool success = false;

private:

  QString get_take_number_filename(const wchar_t *path){
    QString base = STRING_get_qstring(path);

    QString take;
    
    if (QFileInfo(base).isDir())
      take = "last_take_number.txt";
    else
      take = " last_take_number.txt";
    
    return base + take;
  }
  
  int read_old_take_number_from_disk(const wchar_t *path){
    QString filename = get_take_number_filename(path);

    int takenum = 0;
    
    auto *file = DISK_open_for_reading(filename);
    if (file!=NULL){
      
      QString line = DISK_read_qstring_line(file);
      takenum = atoi(line.trimmed().toUtf8().constData());
      
      DISK_close_and_delete(file);
      
    }

    return takenum;
  }
  
  bool write_new_take_number_to_disk(const wchar_t *path, int take_number){
    QString filename = get_take_number_filename(path);

    auto *file = DISK_open_for_writing(filename);
    if (file==NULL)
      return false;
    
    DISK_write_qstring(file, QString::number(take_number));
    
    DISK_close_and_delete(file);
    
    return true;
  }
  
  bool is_valid_new_soundfile_name(QString filename){
    wchar_t *wfilename = STRING_create(filename, false);
    
    bool has_file = SAMPLEREADER_has_file(wfilename);
    bool file_exists = DISK_file_exists(wfilename);
    
    free(wfilename);
    
    if (has_file==false && false==file_exists) // Check for SAMPLEREADER_has_file since the user could have manually deleted an earlier recording.
      return true;
    else
      return false;
  }
  
  QString get_unique_filename(const wchar_t *path, bool &unique_success){
    QString base = STRING_get_qstring(path);

    unique_success=true;

    QString take;
    if (QFileInfo(base).isDir())
      take = "take ";
    else
      take = " take ";

    int take_number = read_old_take_number_from_disk(path);
    take_number++;

    for(int counter = 1 ; counter <= 1000 ; counter++){
      
      QString filename = base + take + QString::number(take_number) + ".wav";
      
      if (is_valid_new_soundfile_name(filename)) {
        
        write_new_take_number_to_disk(path, take_number);
        
        return filename;
        
      } else {
          
        take_number++;
        
      }
    }

    unique_success=false;
    return "";
  }

public:
  
  // Remember that this is not the main thread.
  //
  RecordingFile(radium::SampleRecorderInstance *instance)
    : instance(instance)
  {
    ATOMIC_ADD(g_num_recording_files, 1);
    
    SF_INFO sf_info = {};
    float middle_note = instance->middle_note;
    if (middle_note < 0.01)
      middle_note = 0.01;
    sf_info.samplerate = MIXER_get_sample_rate();
    sf_info.channels = instance->num_ch;
    sf_info.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;

    bool unique_success;
    filename = get_unique_filename(instance->recording_path.get_from_another_thread(), unique_success);
    
    if (unique_success==false){
      RT_message("Unable to create new file in \"%s\".\nPerhaps you have more than 1000 takes there?",STRING_get_qstring(instance->recording_path.get_from_another_thread()).toUtf8().constData());
      return;
    }

    sndfile = radium_sf_open(filename, SFM_WRITE, &sf_info);

    if(sndfile==NULL)
      RT_message("Unable to create file \"%s\": %s",filename.toUtf8().constData(), sf_strerror(NULL));

    success = true;
  }

  ~RecordingFile(){
    ATOMIC_ADD(g_num_recording_files, -1);
  }
  
  void write_uint32(QFile &f, uint32_t i, bool &success){
    char temp[4];
#if IS_LITTLE_ENDIAN
    temp[3] = (i >> 24) & 0xff;
    temp[2] = (i >> 16) & 0xff;
    temp[1] = (i >> 8) & 0xff;
    temp[0] = i & 0xff;
#else
#error "really?" // Most likely, it's a build error if we get this one.
    temp[0] = (i >> 24) & 0xff;
    temp[1] = (i >> 16) & 0xff;
    temp[2] = (i >> 8) & 0xff;
    temp[3] = i & 0xff;
#endif
    if (f.write(temp, 4) != 4)
      success=false;
  }

  bool close(void){
    R_ASSERT(non_written_slices.is_empty());

    if (sndfile==NULL)
      return false;

    int close_result = sf_close(sndfile); // Note, sndfile must NOT be set to NULL here. We use sndfile==NULL to identify whether to load new data in the main thread.

    if(close_result!=0){
      RT_message("Unable to close sound file \"%s\": %s",filename.toUtf8().constData(), sf_error_number(close_result));
      return false;
    }

    // Append a smpl chunk
    QFile f(filename);
    if (f.open(QIODevice::WriteOnly | QIODevice::Append)==false){
      RT_message("Unable to append smpl chunk to \"%s\". That is strange.",filename.toUtf8().constData());
      return true;
    }

    bool s = true;

    // append chunk
    {
      f.write("smpl");
      
      write_uint32(f, 36, s); // chunk length
      
      write_uint32(f, 0, s); // manufacturer
      write_uint32(f, 0, s); // product
      
      write_uint32(f, 1000000000 / MIXER_get_sample_rate(), s); // sample period

      double middle_note = instance->middle_note + 12.0; // for some reason, radium's middle note is 48. It should have been 60.
        
      int unity_note = int(middle_note);
      double fraction = middle_note - unity_note;
      write_uint32(f, unity_note, s); // unity note
      int64_t temp = 1;
      temp = temp << 32;
      write_uint32(f, scale_double(fraction,0,1,0,temp), s); // pitch fraction
      
      write_uint32(f, 0, s); // smpte format
      write_uint32(f, 0, s); // smpte offset
      
      write_uint32(f, 0, s); // number of loops
      write_uint32(f, 0, s); // cbSamplerData. sampler specific data
    }
    
    // set new WAVE length
    {
      if (f.seek(4)==false)
        s = false;
      else
        write_uint32(f, (int)f.size()-8, s);
    }
    
    f.close();

    if(!s)
      RT_message("Unable to write smpl chunk properly to \"%s\": %s.",filename.toUtf8().constData(), sf_error_number(close_result));
    
    return true;
  }

private:
  void flush(void){
    R_ASSERT_RETURN_IF_FALSE(sndfile!=NULL);

    R_ASSERT_RETURN_IF_FALSE(!non_written_slices.is_empty());
    
    float *channels[instance->num_ch];

    int num_frames = RADIUM_BLOCKSIZE;
    
    for(auto *slice : non_written_slices) {
      channels[slice->slice.ch] = &slice->slice.samples[0];
      num_frames = R_MIN(num_frames, slice->slice.num_frames);
    }
    
    float interleaved_data[num_frames*instance->num_ch];

    int pos=0;
    for(int i=0;i<num_frames;i++)
      for(int ch=0;ch<instance->num_ch;ch++)
        interleaved_data[pos++] = channels[ch]==NULL ? 0.0f : channels[ch][i]; // channels[ch]==NULL is not supposed to happen, but things are semi-messy, so we add the check just in case. (assertion window is shown in RT_put_slice if buffer couldn't be written to the queue.

    int num_written_frames = (int)sf_writef_float(sndfile, interleaved_data, num_frames);
    if(num_written_frames != num_frames)
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

    if (slice->slice.ch==instance->num_ch-1)
      flush();
  }
  
};
  
struct SampleRecorderThread : public QThread {

  SampleRecorderThread(const SampleRecorderThread&) = delete;
  SampleRecorderThread& operator=(const SampleRecorderThread&) = delete;

  radium::Queue<RecordingFile*, 128> recorded_files; // 128 is NOT the limit on the number of files that can be recorded simultaneously. It's just the queue size. The recording thread will wait until there's more space available if it is full when adding new entry.
  radium::Vector<RecordingFile*>     recording_files;
  
public:

  SampleRecorderThread(){
  }

  ~SampleRecorderThread(){
    stop_thread();
  }

  void stop_thread(void){
    if (!isRunning())
      return;
    
    RecordingSlice *slice;
    while(g_free_slices->pop(slice)==false);
  
    slice->command = RecordingSlice::Quit;
    
    put_slice(slice);

    wait(5000);
  }
  
  RecordingFile *get_recording_file(const radium::SampleRecorderInstance *instance) const {
    for(auto *recording_file : recording_files)
      if (recording_file->instance==instance)
        return recording_file;

    return NULL;
  }
  
  void start_recording(radium::SampleRecorderInstance *instance){
    QDir dir = QFileInfo(STRING_get_qstring(instance->recording_path.get_from_another_thread())).dir();
    if (dir.exists()==false){
      RT_message("Error. Could not find the directory \"%s\".\n", dir.absolutePath().toUtf8().constData());
      return;
    }

    auto *recording_file = new RecordingFile(instance);
    if (recording_file->success==false)
      recorded_files.put(recording_file);
    else
      recording_files.push_back(recording_file);
  }
  
  void stop_recording(radium::SampleRecorderInstance *instance){
    RecordingFile *recording_file = get_recording_file(instance);

    if (recording_file==NULL)
      return;

    recording_files.remove(recording_file);

    recording_file->close();

    recorded_files.put(recording_file);    
  }
  
  void treat_slice(radium::SampleRecorderInstance *instance, struct RecordingSlice *slice){
    RecordingFile *recording_file = get_recording_file(instance);

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
          start_recording(slice->instance);
          g_free_slices->bounded_push(slice);
          break;
          
        case RecordingSlice::Stop_Recording:
          stop_recording(slice->instance);
          g_free_slices->bounded_push(slice);
          break;
          
        case RecordingSlice::Sample_Data:
          treat_slice(slice->instance, slice);
          break;

        case RecordingSlice::Quit:
          return;
      }
    }
  }
};
 
} // end anon. namespace


static SampleRecorderThread g_sample_recorder_thread;

static QHash<int64_t, radium::SampleRecorderInstance*> g_instances;
static int64_t g_instance_id = 0;

void SampleRecorder_register_instance(radium::SampleRecorderInstance *instance){
  instance->id = g_instance_id++;
  g_instances[instance->id] = instance;
}

void SampleRecorder_unregister_instance(radium::SampleRecorderInstance *instance){
  g_instances.remove(instance->id);
}

void SampleRecorder_called_regularly(void){
  R_ASSERT(THREADING_is_main_thread());

  int64_t last_instance_id = -1;
  radium::SampleRecorderInstance *instance = NULL;
  
  while(true){
    PeakSlice peak_slice;

    if (g_peak_slice_queue->pop(peak_slice)==false)
      break;

    if (peak_slice.instance_id != last_instance_id){
      instance = g_instances.value(peak_slice.instance_id);
      last_instance_id = peak_slice.instance_id;
    }
    
    if (instance != NULL)
      instance->add_recorded_peak(peak_slice.ch,
                                  peak_slice.min,
                                  peak_slice.max
                                  );                       
  }
  
  while(true){
    bool gotit;
    RecordingFile *recorded_file = g_sample_recorder_thread.recorded_files.tryGet(gotit);

    if(!gotit)
      break;

    recorded_file->instance->is_finished(recorded_file->success, STRING_create(recorded_file->filename));

    delete recorded_file;
  }
}

void RT_SampleRecorder_start_recording(radium::SampleRecorderInstance *instance, int64_t pos){
  R_ASSERT_RETURN_IF_FALSE(instance!=NULL);
  R_ASSERT_RETURN_IF_FALSE(instance->recording_path.get_from_another_thread()!=NULL);
  R_ASSERT_RETURN_IF_FALSE(instance->num_ch>0);    

  RecordingSlice *slice = RT_get_free_slice();

  if(slice==NULL)
    return;

  slice->instance = instance;
  slice->command = RecordingSlice::Start_Recording;
  instance->start = pos;
  instance->end = pos;
  
  put_slice(slice);
}

void RT_SampleRecorder_stop_recording(radium::SampleRecorderInstance *instance){
  R_ASSERT_RETURN_IF_FALSE(instance!=NULL);

  RecordingSlice *slice;
  while(g_free_slices->pop(slice)==false); // buzy looping. The close message must be sent.
  
  slice->instance = instance;
  slice->command = RecordingSlice::Stop_Recording;

  put_slice(slice);
}

void RT_SampleRecorder_add_audio(radium::SampleRecorderInstance *instance, const float **audio, int num_frames){
  R_ASSERT_NON_RELEASE(num_frames <= RADIUM_BLOCKSIZE);

  instance->end += num_frames;
    
  RecordingSlice *slices[instance->num_ch];

  for(int ch=0;ch<instance->num_ch;ch++){
    slices[ch] = RT_get_free_slice();
    
    if (slices[ch]==NULL){
      for(int c=0;c<ch;c++)
        g_free_slices->bounded_push(slices[c]);
      
      return;
    }
    
  }
        
  for(int ch=0;ch<instance->num_ch;ch++){
    {
      RecordingSlice *slice = slices[ch];

      slice->instance = instance;
      slice->command = RecordingSlice::Sample_Data;
      slice->slice.ch = ch;
      slice->slice.num_frames = num_frames;
      memcpy(slice->slice.samples, audio[ch], sizeof(float)*num_frames);

      put_slice(slice);
    }

    {
      float min_peak,max_peak;
      JUCE_get_min_max_val(audio[ch], num_frames, &min_peak, &max_peak);
      
      PeakSlice peak_slice;
      peak_slice.instance_id = instance->id;
      peak_slice.ch = ch;
      peak_slice.min = min_peak;
      peak_slice.max = max_peak;
      
      g_peak_slice_queue->bounded_push(peak_slice);
    }
  }
}

int SampleRecorder_Get_Num_Instances(void){
  return ATOMIC_GET(g_num_recording_files);
}

void SampleRecorder_Init(void){
  g_recording_slice_queue = new radium::Queue<RecordingSlice*, MAX_QUEUE_SIZE>;
  g_free_slices = new boost::lockfree::queue<RecordingSlice*, boost::lockfree::capacity<MAX_QUEUE_SIZE> >;

  RecordingSlice *slices = (RecordingSlice*)V_calloc(MAX_QUEUE_SIZE, sizeof(RecordingSlice));

  for(int i=0;i<MAX_QUEUE_SIZE;i++)
    g_free_slices->push(&slices[i]);

  g_peak_slice_queue = new boost::lockfree::queue<PeakSlice, boost::lockfree::capacity<MAX_QUEUE_SIZE> >;
    
  g_sample_recorder_thread.start();
}


void SampleRecorder_shut_down(void){
  g_sample_recorder_thread.stop_thread();
}

