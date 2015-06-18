
#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "Resampler_proc.h"

#include "SoundPluginRegistry_proc.h"


#define POLYPHONY 256


static int recording_data_size = 0;
static float *recording_data[2];


struct _Data;
typedef struct _Data Data;


typedef struct _Voice{
  struct _Voice *prev;
  struct _Voice *next;

  int64_t num_frames;
  float *audio;

  double rate;

  void *resampler;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  Data *data;
} Voice;



struct _Data {
  
  bool is_recording; // "is_recording" contains the value of the "recording" button. The plugin will only record when both "is_recording" and "is_playing" is true.
  bool is_playing;   // "is_playing" is true if a note is currently playing. If so, it will either play back or record sound, depending on the value of "is_recording".

  int curr_ch;

  Panvals pan;
  float velocity;
  
  int64_t num_frames;

  Voice *voices_playing;
  Voice *voices_not_playing;

  Voice voices[POLYPHONY];
  
  float *audio[2];
};


static long RT_src_callback(void *cb_data, float **out_data){
  Data  *data = cb_data;

  *out_data = audio[data->curr_ch];
  return data->num_frames;
}


static int RT_get_resampled_data(Data *data, float *out, int num_frames){
  return 
}

static void RT_add_voice(Voice **root, Voice *voice){
  voice->next = *root;
  if(*root!=NULL)
    (*root)->prev = voice;
  *root = voice;
  voice->prev = NULL;
}

static void RT_remove_voice(Voice **root, Voice *voice){
  if(voice->prev!=NULL)
    voice->prev->next = voice->next;
  else
    *root=voice->next;

  if(voice->next!=NULL)
    voice->next->prev = voice->prev;
}

enum VoiceOp{
  VOICE_KEEP,
  VOICE_REMOVE
};

static VoiceOp RT_play_voice(Data *data, Voice *voice, int num_frames, float **inputs, float **outputs, int *start_process){

  int delta_pos_at_start = voice->delta_pos_at_start;
  int delta_pos_at_end = voice->delta_pos_at_end;

  if(delta_pos_at_start==0 && delta_pos_at_end==-1){

    // continue playing;


  }else if(delta_pos_at_start>0 && delta_pos_at_end==-1){

    // start playing

  }else{

    // end playing
  }

  return VOICE_KEEP;
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  int num_outputs = plugin->type->num_outputs;

  for(int i=0;i<num_outputs;i++)
    memset(outputs[i],0,num_frames*sizeof(float));

  float *tempsounds[num_outputs];
  float tempdata[num_outputs][num_frames];
  for(int i=0;i<num_outputs;i++)
    tempsounds[i] = &tempdata[i][0];

  Voice *voice = data->voices_playing;

  while(voice!=NULL){
    Voice *next = voice->next;

    if(RT_play_voice(data, voice, num_frames, outputs)==VOICE_REMOVE){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);
    }

    for(int ch=0;ch<num_outputs;ch++){
      float *source = tempsounds[ch];
      float *target = outputs[ch];
      for(int i=start_process;i<num_frames;i++)
        target[i] += source[i];
    }


    voice = next;
  }
}

static void play_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume,float pan){
  Data *data = (Data*)plugin->data;
  data->is_playing = true;
  data->velocity = volume;
  data->pan = get_pan_vals_vector(pan,2);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume){
  Data *data = (Data*)plugin->data;
  data->velocity = volume;
}

static void set_note_pitch(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float pitch){
  Data *data = (Data*)plugin->data;
  //data->rate = ...;
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id){
  Data *data = (Data*)plugin->data;
  data->is_playing = false;
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
}

float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return 0.0f
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%f",0.0f);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  Data *data = (Data*)calloc(1,sizeof(Data));

  data->resampler_type = RESAMPLER_SINC1;

  for(int i=0;i<POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    voice->resampler = RESAMPLER_create(RT_src_callback, 1, voice, data->resampler_type);
    RT_add_voice(&data->voices_not_playing, voice);
  }

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  free(plugin->data);
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}


void create_audio_recorder_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "Audio Recorder";
  plugin_type->name                     = "Audio Recorder";
  plugin_type->num_inputs               = 2;
  plugin_type->num_outputs              = 2;
  plugin_type->is_instrument            = true;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = 1;
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;
  plugin_type->play_note        = play_note;
  plugin_type->set_note_volume  = set_note_volume;
  plugin_type->set_note_pitch   = set_note_pitch;
  plugin_type->stop_note        = stop_note;
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  PR_add_plugin_type(plugin_type);
}
