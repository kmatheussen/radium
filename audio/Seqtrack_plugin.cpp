#define __STDC_FORMAT_MACROS 1

#include <inttypes.h>

#include <math.h>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "Peaks.hpp"
#include "SampleReader_proc.h"

#include "SoundPluginRegistry_proc.h"


#include "Seqtrack_plugin_proc.h"


#define NUM_INPUTS 8
#define NUM_OUTPUTS 8


// This is the instrument that is bounded to ONE track in the sequencer.

/*
  Possible to change pitch while playing? (Hard to calculate sample seek then)
  Possible to change stretch while playing? Also makes it hard to calculate sample seek.
 */

enum{
  //EFF_SAMPLENUM,
  //EFF_PITCH,
  EFF_NUM_EFFECTS
};


namespace{

static int64_t g_id = 0;

struct Sample{
  int64_t _id = g_id++;
  const wchar_t *_filename;
  radium::SampleReader *_reader;
  radium::DiskPeaks *_peaks;

  const struct SeqBlock *_seqblock;
  
  float _volume = 1.0;
  float _pitch = 1.0;
  int _num_ch;

  bool _is_playing = false;
  int _click_avoidance_fade_in = 0; // frames left while fading in.
  bool _do_fade_out = false;
  
  Sample(const wchar_t *filename, radium::SampleReader *reader, const struct SeqBlock *seqblock)
    : _filename(wcsdup(filename))
    , _reader(reader)
    , _peaks(new radium::DiskPeaks(filename))
    , _seqblock(seqblock)
  {
    _num_ch = SAMPLEREADER_get_num_channels(reader);
  }

  ~Sample(){
    SAMPLEREADER_delete(_reader);
    delete _peaks;
    free((void*)_filename);
  }

  void set_playing(bool is_playing){
    if (is_playing != _is_playing) // usually they have the same value.
      _is_playing = is_playing;
  }
             
  void start_playing(void){
    R_ASSERT(_is_playing==false);
    _is_playing = true;
  }

  void stop_playing(void){
    R_ASSERT(_is_playing==true);
    _is_playing = false;
  }

  void RT_called_per_block(int64_t curr_start_time, int64_t curr_end_time){ // (end_time-start_time is usually RADIUM_BLOCKSIZE.)

    bool inside = curr_start_time >= _seqblock->time && curr_start_time < _seqblock->time2;

    if (inside==_is_playing)
      return;
    
    if (inside) {
      _is_playing = true;      
      _click_avoidance_fade_in = 2 * pc->pfreq / 1000; // 2ms click-avoidance fade-in.
    } else {
      _is_playing = false;
    }
    
  }
  
  void prepare_to_play(int64_t seqtime){
    if (seqtime >= _seqblock->time2)
      return;

    int64_t sample_start_pos = R_MAX(0, seqtime - _seqblock->time);

    // TODO: This number should be dynamically calculated somehow depending on how long time it takes to read from disk.
    int64_t how_much_to_prepare = 48000;
    
    SAMPLEREADER_prepare_to_play(_reader, sample_start_pos, how_much_to_prepare);
  }

  void RT_add_samples(int num_frames, float **outputs){

    R_ASSERT_NON_RELEASE(is_playing_song()==true);

    if (_is_playing==false)
      return;

    float dummy[num_frames];

    int num_ch = R_MIN(NUM_OUTPUTS, _num_ch);

    float *outputs2[_num_ch];

    if (_num_ch > num_ch){
      memset(dummy, 0, sizeof(float)*num_frames); // Avoid nan/inf/denominals/etc.
      for(int ch = num_ch ; ch < _num_ch ; ch++)
        outputs2[ch] = dummy;
    }
      

    int total_read = 0;

    while(total_read < num_frames){

      for(int ch = 0 ; ch < num_ch ; ch++)
        outputs2[ch] = &outputs[ch][total_read];
      
      int num_frames_left = num_frames - total_read;
      
      int num_read = RT_SAMPLEREADER_read(_reader, outputs2, num_ch, num_frames_left, 48000);
      R_ASSERT_RETURN_IF_FALSE(num_read>0);

      total_read += num_read;
      R_ASSERT(total_read <= num_frames);
    }
  }

};

struct Data{
  radium::Vector<Sample*> _samples;

  float _sample_rate;

  Data(float sample_rate)
    :_sample_rate(sample_rate)
  {
  }
  
  void RT_called_per_block(const struct SeqTrack *seqtrack){
    R_ASSERT_NON_RELEASE(is_playing() && pc->playtype==PLAYSONG);
    
    int64_t start_time = seqtrack->start_time;
    int64_t end_time = seqtrack->end_time;
    
    for(auto *sample : _samples){
#if !defined(RELEASE)
      int num = 0;
      VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
        if(seqblock==sample->_seqblock){
          R_ASSERT(seqblock->block==NULL);
          R_ASSERT(seqblock->sample_id == sample->_id);
          num++;
        }
      }END_VECTOR_FOR_EACH;
      if (num != 1)
        abort();
#endif
      sample->RT_called_per_block(start_time, end_time);
    }
  }

  void prepare_to_play(const struct SeqTrack *seqtrack, int64_t seqtime){
    for(auto *sample : _samples){
#if !defined(RELEASE)
      int num = 0;
      VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
        if(seqblock==sample->_seqblock){
          R_ASSERT(seqblock->block==NULL);
          R_ASSERT(seqblock->sample_id == sample->_id);
          num++;
        }
      }END_VECTOR_FOR_EACH;
      if (num != 1){
        fprintf(stderr,"NUM: %d (%d)\n", num, num != 1);
        abort();
      }
#endif
      sample->prepare_to_play(seqtime);
    }
  }
};
}

static int64_t add_sample(Data *data, const wchar_t *filename, const struct SeqBlock *seqblock){
  radium::SampleReader *reader = SAMPLEREADER_create(filename);

  if(reader==NULL)
    return -1;

  Sample *sample = new Sample(filename, reader, seqblock);

  data->_samples.push_back_in_realtime_safe_manner(sample);

  return sample->_id;
}

int64_t SEQTRACKPLUGIN_add_sample(SoundPlugin *plugin, const wchar_t *filename, const struct SeqBlock *seqblock){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Data *data = (Data*)plugin->data;
  return add_sample(data, filename, seqblock);
}

static Sample *get_sample(const SoundPlugin *plugin, int64_t id){
  Data *data = (Data*)plugin->data;

  for(auto *sample : data->_samples){
    if (sample->_id==id)
      return sample;
  }

  R_ASSERT(false);
}

void SEQTRACKPLUGIN_remove_sample(SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name));
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return;

  Data *data = (Data*)plugin->data;

  PLAYER_lock();{
    data->_samples.remove(sample);
  }PLAYER_unlock();
}

int64_t SEQTRACKPLUGIN_get_num_channels(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return -1;

  return SAMPLEREADER_get_num_channels(sample->_reader);
}

int64_t SEQTRACKPLUGIN_get_num_frames(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), -1);
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return -1;

  return SAMPLEREADER_get_num_frames(sample->_reader);
}

const wchar_t *SEQTRACKPLUGIN_get_sample_name(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), L"");
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return L"";

  return SAMPLEREADER_get_sample_name(sample->_reader);
}

const radium::DiskPeaks *SEQTRACKPLUGIN_get_peaks(const SoundPlugin *plugin, int64_t id){
  R_ASSERT_RETURN_IF_FALSE2(!strcmp(SEQTRACKPLUGIN_NAME, plugin->type->type_name), NULL);
  
  Sample *sample = get_sample(plugin, id);
  if (sample==NULL)
    return NULL;

  return sample->_peaks;
}

/*
void RT_HDSAMPLER_start_playing_sample(SoundPlugin *plugin, int64_t id){
  Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->start_playing();  
}

void RT_HDSAMPLER_stop_playing_sample(SoundPlugin *plugin, int64_t id){
  Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->stop_playing();  
}

void RT_HDSAMPLE_set_sample_playing(SoundPlugin *plugin, int64_t id, bool is_playing){
 Sample *sample = get_sample(plugin, id);
  if (sample!=NULL)
    sample->set_sample_playing(is_playing);  
}
*/

void RT_SEQTRACKPLUGIN_called_per_block(SoundPlugin *plugin, const struct SeqTrack *seqtrack){
  Data *data = (Data*)plugin->data;

  data->RT_called_per_block(seqtrack);
}

void SEQTRACKPLUGIN_prepare_to_play(SoundPlugin *plugin, const struct SeqTrack *seqtrack, int64_t seqtime){
  Data *data = (Data*)plugin->data;

  data->prepare_to_play(seqtrack, seqtime);
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;

  // Null out channels
  for(int ch = 0 ; ch < NUM_OUTPUTS ; ch++)
    memset(outputs[ch], 0, num_frames*sizeof(float));

  if (is_playing_song()==false)
    return;
  
  // Read samples
  for(Sample *sample : data->_samples)
    sample->RT_add_samples(num_frames, outputs);
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  //Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);
  //data->volume = value;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  //Data *data = (Data*)plugin->data;
  return 0;//data->volume;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  //Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%f",0.1);//data->volume);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Data *data = new Data(sample_rate);
  printf("####################################################### Setting sine volume to 0.5f (create_plugin_data)\n");

  //add_sample(L"/home/kjetil/radium/bin/sounds/bbs2.wav");
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  delete data;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}


void create_seqtrack_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = SEQTRACKPLUGIN_NAME;
  plugin_type->name                     = SEQTRACKPLUGIN_NAME;
  plugin_type->num_inputs               = NUM_INPUTS;
  plugin_type->num_outputs              = NUM_OUTPUTS;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = EFF_NUM_EFFECTS,
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;

  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  PR_add_plugin_type_no_menu(plugin_type);
  //PR_add_plugin_type(plugin_type);
}
