/* Copyright 2012 Kjetil S. Matheussen

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

#include <glib.h>
//#include <endian.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <sndfile.h>

#include <semaphore.h>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_settings_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "Resampler_proc.h"
#include "Envelope_proc.h"
#include "SoundFonts_proc.h"

#include "Sampler_plugin_proc.h"

#define POLYPHONY 256
#define MAX_NUM_SAMPLES 256

#define DEFAULT_A 20
#define DEFAULT_H 5
#define DEFAULT_D 20
#define DEFAULT_S 0.5
#define DEFAULT_R 20

#define MAX_A 1000
#define MAX_H 40
#define MAX_D 1000
#define MAX_S 1.0
#define MAX_R 2000

// Effect order
enum{
  EFF_STARTPOS,
  EFF_FINETUNE,
  EFF_NOTE_ADJUST,
  //  EFF_OCTAVE_ADJUST,
  EFF_A,
  EFF_H,
  EFF_D,
  EFF_S,
  EFF_R,
  EFF_NUM_EFFECTS
  };

typedef struct{
  float volume;

  int num_frames;
  int loop_start;
  int loop_end;

  int ch;        // -1 means both channels.
  float *data;

  double frequency_table[128];
} Sample;

// A voice object points to only one sample. Stereo-files uses two voice objects. Soundfonts using x sounds to play a note, needs x voice objects to play that note.
typedef struct _Voice{
  struct _Voice *prev;
  struct _Voice *next;

  float last_finetune_value;

  int note_num;

  float start_volume;
  float end_volume;
  //double gain;
  int pos;

  void *resampler;
  void *adsr;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  bool is_fading_out;
  int fading_pos;
  int fading_len;

  int num_samples;
  const Sample *sample;
} Voice;

typedef struct{
  int num_samples;
  const Sample *samples[MAX_NUM_SAMPLES];
} Note;

typedef struct _Data{
  float finetune; // -100 -> +100
  float note_adjust; // -6 -> +6      (must be float because of conversions)
  //float octave_adjust; // -10 -> +10. (must be float because of conversions)

  float startpos;

  float a,h,d,s,r;

  float samplerate; // The system samplerate. I.e. the jack samplerate, not soundfile samplerate.

  int resampler_type;
  const char *filename;
  int instrument_number;

  //int num_channels; // not used for anything, I think.

  const Note notes[128];

  Voice *voices_playing;
  Voice *voices_not_playing;

  int num_different_samples;  // not used for anything (important).

  Voice voices[POLYPHONY];
  const Sample samples[MAX_NUM_SAMPLES];

  // These two are used when switching sound on the fly
  struct _Data *new_data;
  sem_t signal_from_RT;

} Data;


static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

// input is between 0 and 1.
// output is between 0 and 1.
static float velocity2gain(float val){
  return val;
#if 0
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
#endif
}


static void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = num_frames+1.0f;
  int i;
  float val = (num_frames / num_frames_plus_1);
  float inc = val - ( (num_frames-1) / num_frames_plus_1);

  for(i=0;i<num_frames;i++){
    sound[i] *= val;
    val -= inc;
  }
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


static long RT_src_callback(void *cb_data, float **data){
  Voice *voice=cb_data;
  const Sample *sample=voice->sample;
  int pos = voice->pos;

  *data = &voice->sample->data[pos];

  //printf("Supplying from sample %p. offset: %d. loop start: %d, loop end: %d\n",voice->sample->interleaved_samples,voice->pos,sample->loop_start,sample->loop_end);

  if(sample->loop_end > sample->loop_start){
    voice->pos = sample->loop_start;

    if(pos >= sample->loop_end) // just in case. not sure if this can happen
      return 0;

    return sample->loop_end - pos;
  }else{

    if(pos==sample->num_frames)
      return 0;

    voice->pos=sample->num_frames;
    return sample->num_frames - pos;
  }
}

static double RT_get_src_ratio(Data *data, Voice *voice){
  const Sample *sample = voice->sample;

  //int notenum = voice->note_num + (int)data->octave_adjust*12 + (int)data->note_adjust;
  int notenum = voice->note_num + (int)data->note_adjust;
  if(notenum<1)
    notenum=1;
  if(notenum>126)
    notenum=126;

  return data->samplerate / scale(data->finetune, 0, 1, sample->frequency_table[notenum-1], sample->frequency_table[notenum+1]);
}

static int RT_get_resampled_data(Data *data, Voice *voice, float *out, int num_frames){
#if 0
  printf("samplerate: %f, sample frequency: %f, ratio: %f\n",
         data->samplerate,
         voice->sample->frequency_table[voice->note_num],
         data->samplerate / voice->sample->frequency_table[voice->note_num]);
#endif

  return RESAMPLER_read(voice->resampler, RT_get_src_ratio(data,voice), num_frames, out);
}

static bool RT_play_voice(Data *data, Voice *voice, int num_frames_to_produce, float **outputs){
  int startpos = voice->delta_pos_at_start;
  int endpos = voice->delta_pos_at_end;

  if(startpos>=0)
    voice->delta_pos_at_start = 0;
  if(endpos>=0)
    voice->delta_pos_at_end = -1;

  if(endpos>=0 && endpos<startpos) // Should not happen. Test for it just in case. It's a bit messy when notes are generated by the keyboard, player and other places at the same time.
    {
      printf("Oops. Endpos: %d. startpos: %d\n",endpos,startpos);
      return false;
    }

  float resampled_data[num_frames_to_produce-startpos];
  int frames_created_by_resampler = RT_get_resampled_data(data,voice,resampled_data,num_frames_to_produce-startpos);

  int frames_created_by_envelope;

  float *adsr_sound_data[1]={&resampled_data[0]};

  if(endpos>=0){
    int pre_release_len = endpos-startpos;

    //printf("********** endpos>0: %d. prelen: %d, frames_created_by_resampler: %d\n",endpos,prelen,frames_created_by_resampler);

    if(frames_created_by_resampler <= pre_release_len){ // i.e. we reached the end of sound before beginning to release the ADSR envelope.

      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);

    }else{
      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, pre_release_len);

      ADSR_release(voice->adsr);

      int post_release_len = frames_created_by_resampler - frames_created_by_envelope;
      adsr_sound_data[0] = &resampled_data[frames_created_by_envelope];        
      frames_created_by_envelope += ADSR_apply(voice->adsr, adsr_sound_data, 1, post_release_len);
    }

  }else{

    frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);

  }

  const Sample *sample = voice->sample;

  if(sample->ch == -1){
    {
      float *out=outputs[0] + startpos;
      SMOOTH_mix_sounds_raw(out, resampled_data, frames_created_by_envelope, voice->start_volume, voice->end_volume);
    }{
      float *out=outputs[1] + startpos;
      SMOOTH_mix_sounds_raw(out, resampled_data, frames_created_by_envelope, voice->start_volume, voice->end_volume);
    }
  }else{
    float *out=outputs[sample->ch] + startpos;
    SMOOTH_mix_sounds_raw(out, resampled_data, frames_created_by_envelope, voice->start_volume, voice->end_volume);
  }

  voice->start_volume = voice->end_volume;

  if(startpos+frames_created_by_envelope < num_frames_to_produce)
    return true;
  else
    return false;
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  memset(outputs[0],0,num_frames*sizeof(float));
  memset(outputs[1],0,num_frames*sizeof(float));

  while(voice!=NULL){
    Voice *next = voice->next;

    if(RT_play_voice(data, voice, num_frames, outputs)==true){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);
    }

    voice = next;
  }

  if(data->new_data != NULL){
    RT_fade_out(outputs[0],num_frames);
    RT_fade_out(outputs[1],num_frames);

    plugin->data = data->new_data; // Bang! (hmm.)
    data->new_data = NULL;

    sem_post(&data->signal_from_RT);
  }
}

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;

  //printf("playing note %d\n",note_num);

  const Note *note = &data->notes[note_num];

  int i;
  for(i=0;i<note->num_samples;i++){

    if(data->voices_not_playing==NULL){
      printf("No more free voices\n");
      return;
    }

    Voice *voice = data->voices_not_playing;
    
    RT_remove_voice(&data->voices_not_playing, voice);
    RT_add_voice(&data->voices_playing, voice);
    
    voice->last_finetune_value = data->finetune;
    
    voice->note_num = note_num;

    voice->start_volume = velocity2gain(volume);
    voice->end_volume = voice->start_volume;
    
    voice->sample = note->samples[i];
    
    if(voice->sample->loop_end > voice->sample->loop_start)
      voice->pos=scale(data->startpos, // set startpos between 0 and loop_end
                       0,1,
                       0,voice->sample->loop_end);
    else
      voice->pos=scale(data->startpos,  // set startpos between 0 and sound length
                       0,1,
                       0,voice->sample->num_frames);
    
    RESAMPLER_reset(voice->resampler);
    ADSR_reset(voice->adsr);
    ADSR_set_adsr(voice->adsr, data->a, data->h, data->d, data->s, data->r);

    voice->delta_pos_at_start=time;
    voice->delta_pos_at_end=-1;
    voice->is_fading_out=false;
  }
}


static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  while(voice!=NULL){
    //printf("Setting volume to %f. note_num: %d. voice: %d\n",volume,note_num,voice->note_num);

    if(voice->note_num==note_num)
      voice->end_volume = velocity2gain(volume);

    voice = voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  if(time==-1){
    RError("time==-1 at stop_note. Will cause hanging note.");
    time=0;
  }

  while(voice!=NULL){
    if(voice->note_num==note_num && voice->delta_pos_at_end == -1){
      voice->delta_pos_at_end = time;
      voice->end_volume = velocity2gain(volume);
    }

    voice = voice->next;
  }
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  if(value_format==PLUGIN_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      data->startpos = value;
      break;
    case EFF_FINETUNE:
      data->finetune = value;
      break;
    case EFF_A:
      data->a = scale(value,
                      0.0,1.0,
                      0,MAX_A);
      break;
    case EFF_H:
      data->h = scale(value,
                      0.0,1.0,
                      0,MAX_H);
      break;
    case EFF_D:
      data->d = scale(value,
                      0.0,1.0,
                      0,MAX_D);
      break;
    case EFF_S:
      data->s = scale(value,
                      0.0,1.0,
                      0,MAX_S);
      break;
    case EFF_R:
      data->r = scale(value,
                      0.0,1.0,
                      0,MAX_R);
      break;
    case EFF_NOTE_ADJUST:
      data->note_adjust = scale(value,
                                0.0,1.0,
                                -6.99,6.99);
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->octave_adjust = scale(value,
                                  0,1,
                                  -10.99,10.99);
      break;
#endif
    default:
      RError("Unknown effect number %d\n",effect_num);
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      data->startpos = value;
      break;
    case EFF_FINETUNE:
      data->finetune = value;
      break;
    case EFF_A:
      data->a = value;
      break;
    case EFF_H:
      data->h = value;
      break;
    case EFF_D:
      data->d = value;
      break;
    case EFF_S:
      data->s = value;
      break;
    case EFF_R:
      data->r = value;
      break;
    case EFF_NOTE_ADJUST:
      data->note_adjust = value;
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->octave_adjust = value;
      break;
#endif
    default:
      RError("Unknown effect number %d\n",effect_num);
    }
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  if(value_format==PLUGIN_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      return data->startpos;
    case EFF_FINETUNE:
      return data->finetune;
    case EFF_A:
      return scale(data->a,
                   0,MAX_A,
                   0.0,1.0);
    case EFF_H:
      return scale(data->h,
                   0,MAX_H,
                   0.0,1.0);
    case EFF_D:
      return scale(data->d,
                   0,MAX_D,
                   0.0,1.0);
    case EFF_S:
      return scale(data->s,
                   0,MAX_S,
                   0.0,1.0);
    case EFF_R:
      return scale(data->r,
                   0,MAX_R,
                   0.0,1.0);
    case EFF_NOTE_ADJUST:
      return scale(data->note_adjust,
                   -6.99,6.99,
                   0,1);
#if 0
    case EFF_OCTAVE_ADJUST:
      return scale(data->octave_adjust,
                   -10.99,10.99,
                   0,1);
#endif
    default:
      RError("Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      return data->startpos;
    case EFF_FINETUNE:
      return data->finetune;
    case EFF_A:
      return data->a;
    case EFF_H:
      return data->h;
    case EFF_D:
      return data->d;
    case EFF_S:
      return data->s;
    case EFF_R:
      return data->r;
    case EFF_NOTE_ADJUST:
      return data->note_adjust;
#if 0
    case EFF_OCTAVE_ADJUST:
      return data->octave_adjust;
#endif
    default:
      RError("Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  switch(effect_num){
  case EFF_STARTPOS:
    snprintf(buffer,buffersize-1,"%d%%",(int)scale(data->startpos,0.0,1.0,0,100));
    break;
  case EFF_FINETUNE:
    snprintf(buffer,buffersize-1,"%f cents",scale(data->finetune,0.0,1.0,-100,100));
    break;
  case EFF_A:
    snprintf(buffer,buffersize-1,"%f ms",data->a);
    break;
  case EFF_H:
    snprintf(buffer,buffersize-1,"%f ms",data->h);
    break;
  case EFF_D:
    snprintf(buffer,buffersize-1,"%f ms",data->d);
    break;
  case EFF_S:
    snprintf(buffer,buffersize-1,"%f",data->s);
    break;
  case EFF_R:
    snprintf(buffer,buffersize-1,"%f ms",data->r);
    break;
  case EFF_NOTE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      int adjust = data->note_adjust;
      snprintf(buffer,buffersize-1,"%s%d note%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
    }
    break;
#if 0
  case EFF_OCTAVE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      int adjust=data->octave_adjust;
      snprintf(buffer,buffersize-1,"%s%d octave%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
    }
    break;
#endif
  default:
    RError("Unknown effect number %d\n",effect_num);
  }
}
             

static double midi_to_hz(float midi){
  if(midi<=0)
    return 0;
  else
    //  return 1;
  return 8.17579891564*(expf(.0577622650*midi));
}

#if 0
static double get_ratio(int sample_note_num, int play_note_num){
  return midi_to_hz(sample_note_num) / midi_to_hz(play_note_num);
}
#endif

// based on code from soundtracker
static int32_t get_le_32 (char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int32_t*)src;
#else
    return (src[0] << 0) + (src[1] << 8) + (src[2] << 16) + (src[3] << 24);
#endif
}

static int read_le32int(FILE *file){
  char size_chars[4] = {0}; // {0} is here to keep valgrind quiet.
  fread(size_chars,4,1,file);
  return get_le_32(size_chars);
}


// based on code from soundtracker
static int16_t get_le_16 (char *src)
{
#if IS_LITTLE_ENDIAN
    return *(int16_t*)src;
#else
    return (src[0] << 0) + (src[1] << 8);
#endif
}

static void convert_16_bit_little_endian_to_native(int16_t *src, int num_frames){
#if IS_LITTLE_ENDIAN
  return;
#else
#error "Something is probably wrong. We dont target any big endian platforms."
  int i;
  for(i=0;i<num_frames;i++){
    src[i] = get_le_16(&src[i]);
  }
#endif
}

static int read_le16int(FILE *file){
  char size_chars[2];
  fread(size_chars,2,1,file);
  return get_le_16(size_chars);
}


static unsigned int read_8int(FILE *file){
  unsigned char size_chars[1];
  fread(size_chars,1,1,file);
  return size_chars[0];
}

static int read_8int_signed(FILE *file){
  int8_t size_chars[1];
  fread(size_chars,1,1,file);
  return size_chars[0];
}


static void set_legal_loop_points(Sample *sample, int start, int end){
  if(start<0)
    start=0;

  if(end>sample->num_frames)
    end=sample->num_frames;

  if(end<=start){
    sample->loop_start=0;
    sample->loop_end=0;
  }else{
    sample->loop_start=start;
    sample->loop_end=end;
  }
}

#include "Sampler_plugin_wav_parse.c"
#include "Sampler_plugin_xi_load.c"
#include "Sampler_plugin_sf2_load.c"

static bool load_sample_with_libsndfile(Data *data, const char *filename){
  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  data->num_different_samples = 1;

  SNDFILE *sndfile = sf_open(filename,SFM_READ,&sf_info);

  if(sndfile==NULL){
    fprintf(stderr,"could not open file\n");
    return false;
  }

  {
    int num_channels = sf_info.channels;

    printf("Num channels: %d\n",num_channels);

    if(num_channels > 2) // TODO
      num_channels = 2;

    int ch;
    for(ch=0;ch<num_channels;ch++){
      Sample *sample=(Sample*)&data->samples[ch];
      sample->num_frames   = sf_info.frames;
      sample->data = malloc(sizeof(float)*sample->num_frames);
    }

    int frames_read = 0;
    while(frames_read < sf_info.frames){
      float samples[1024*sf_info.channels];

      int read_now = sf_readf_float(sndfile, samples, 1024);
      //printf("Reading %d frames. Size: %d. Read: %d. samples[0]: %f. channels: %d/%d\n",read_now,sample->num_frames,frames_read,samples[0],sample->num_channels,sf_info.channels);

      int interleaved_pos=0;
      int i;
      for(i=0;i<read_now;i++){
        for(ch=0;ch<sf_info.channels;ch++){
          if(ch<2){
            Sample *sample=(Sample*)&data->samples[ch];
            sample->data[frames_read+i] = samples[interleaved_pos];
          }
          interleaved_pos++;
        }
      }

      frames_read += read_now;
    }

    sf_close(sndfile);

    for(ch=0;ch<num_channels;ch++){     
      Sample *sample=(Sample*)&data->samples[ch];

      if((sf_info.format&0xffff0000) == SF_FORMAT_WAV){
        printf("format: 0x%x. sections: %d, num_frames: %d. SF_FORMAT_WAV: 0x%x. og: 0x%x\n",sf_info.format,sf_info.sections,(int)sf_info.frames,SF_FORMAT_WAV,sf_info.format&SF_FORMAT_WAV);
        set_wav_loop_points(sample,filename);
      }

      if(num_channels==1)
        sample->ch = -1; // i.e play the sample in both channels.
      else
        sample->ch = ch;

      int i;
      for(i=1;i<128;i++){
        Note *note=(Note*)&data->notes[i];
        
        note->num_samples = num_channels;
        note->samples[ch] = sample;
          
        sample->frequency_table[i] = sf_info.samplerate * midi_to_hz(i)/midi_to_hz(48);
        
        //printf("%d: %f, data: %f, sample: %f, midifreq: %f\n",i,sample->samplerate,(float)data->samplerate,(float)sf_info.samplerate,midi_to_hz(i));
      }
    }
  }

  return true;
}

static bool load_sample(Data *data, const char *filename, int instrument_number){
  if(load_xi_instrument(data,filename)==false)
    if(load_sample_with_libsndfile(data,filename)==false)
      if(load_sf2_instrument(data,filename,instrument_number)==false)
        return false;

  //data->num_channels = data->samples[0].num_channels; // All samples must contain the same number of channels.

  int i=0;
  for(i=0;i<POLYPHONY;i++){
    Voice *voice = &data->voices[i];

    voice->resampler = RESAMPLER_create(RT_src_callback, 1, voice, data->resampler_type);
    voice->adsr = ADSR_create(data->samplerate);

    RT_add_voice(&data->voices_not_playing, voice);
  }

  return true;
}

static Data *create_data(float samplerate, Data *old_data, const char *filename, int instrument_number, int resampler_type){
  Data *data = calloc(1,sizeof(Data));

  sem_init(&data->signal_from_RT,0,0);

  if(old_data==NULL){
    data->finetune = 0.5f;

    data->a=DEFAULT_A;
    data->d=DEFAULT_D;
    data->s=DEFAULT_S;
    data->r=DEFAULT_R;

  }else{

    data->startpos = old_data->startpos;
    data->finetune = old_data->finetune;
    data->a = old_data->a;
    data->d = old_data->d;
    data->s = old_data->s;
    data->r = old_data->r;
  }
  
  data->samplerate = samplerate;
  data->resampler_type = resampler_type;
  data->filename = strdup(filename);
  data->instrument_number = instrument_number;

  return data;
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float samplerate, int block_size){

  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/etrnpadl.xi"; // one sample
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/elecpad.xi"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/flpad2.xi"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/PAD/NAMED1/BIGPAD1.XI"; // multile samples
  //const char *filename="/home/kjetil/brenn/downloaded/temp/waveworld/wav/synths/temp/prophe07.wav";
  //const char *filename="/gammelhd/home/kjetil/poing-imperatif/karin.wav";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/BASS/ACOUSTIC/acbs02r.xi";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/SYNTH/SEQUENCE/SH101/sh101sq1.xi";
  //const char *filename="/home/kjetil/brenn/downloaded/temp/CATEGORY/BELL/CHURCH/CHRBEL01.XI";
  //const char *filename="/gammelhd/gammelhd/gammel_lyd/2_channel_short.wav";
  //const char *filename="/gammelhd/gammelhd/gammelhd/gammel_lyd/d_lydfiler/instrument/keyboard/mellotron.sf2";
  char filename[1024];
  sprintf(filename,"%s%ssounds%s%s",OS_get_program_path(),OS_get_directory_separator(),OS_get_directory_separator(),"016.WAV");

  Data *data = create_data(samplerate,NULL,filename,0,RESAMPLER_CUBIC); // cubic is the default

  if(load_sample(data,filename,0)==false){
    free(data);
    return NULL;
  }

  return data;
}

static void delete_data(Data *data){
  int i;

  float *prev=NULL;

  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample *sample=(Sample*)&data->samples[0];

    if(sample->data != prev){
      prev = sample->data;
      free(sample->data);
    }
  }

  for(i=0;i<POLYPHONY;i++)
    RESAMPLER_delete(data->voices[i].resampler);

  free((char*)data->filename);

  free(data);
}

static bool set_new_sample(struct SoundPlugin *plugin, const char *filename, int instrument_number, int resampler_type){
  bool success=false;

  Data *old_data = plugin->data;

  Data *data = create_data(old_data->samplerate, plugin->data, filename, instrument_number, resampler_type);

  if(load_sample(data,filename,instrument_number)==false)
    goto exit;

  if(SP_is_plugin_running(plugin)){

    PLAYER_lock();{  
      old_data->new_data = data;
    }PLAYER_unlock();

    sem_wait(&old_data->signal_from_RT);

  } else {

    plugin->data = data;

  }

  delete_data(old_data);
  
  success = true;
 exit:
  if(success==false)
    free(data);

  return success;
}

bool SAMPLER_set_new_sample(struct SoundPlugin *plugin, const char *filename, int instrument_number){
  Data *data=plugin->data;
  return set_new_sample(plugin,filename,instrument_number,data->resampler_type);
}

bool SAMPLER_set_resampler_type(struct SoundPlugin *plugin, int resampler_type){
  Data *data=plugin->data;
  return set_new_sample(plugin,data->filename,data->instrument_number,resampler_type);
}

int SAMPLER_get_resampler_type(struct SoundPlugin *plugin){
  Data *data=plugin->data;
  return data->resampler_type;
}

// Has been used for debugging. Not sure if I planned to use it for anything else.
void SAMPLER_save_sample(struct SoundPlugin *plugin, const char *filename, int sample_number){
  Data *data = (Data*)plugin->data;
  const Sample *sample = &data->samples[sample_number];

  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  sf_info.samplerate = 22050;
  sf_info.channels = 1;
  sf_info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;

  if(sf_format_check(&sf_info)==0){
    fprintf (stderr, "\nFileformat not supported by libsndfile.\n");
    return;
  }

  SNDFILE *sndfile = sf_open(filename,SFM_WRITE,&sf_info);

  if(sndfile==NULL){
    fprintf(stderr,"could not open file\n");
    return;
  }

  sf_writef_float(sndfile,sample->data,sample->num_frames);

  sf_close(sndfile);
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  Data *data=(Data*)plugin->data;

  delete_data(data);
}

static const char *get_effect_name(const struct SoundPluginType *plugin_type, int effect_num){
  switch(effect_num){
  case EFF_STARTPOS:
    return "Start Position";
  case EFF_FINETUNE:
    return "Finetune";
  case EFF_A:
    return "Attack";
  case EFF_H:
    return "Hold";
  case EFF_D:
    return "Decay";
  case EFF_S:
    return "Sustain";
  case EFF_R:
    return "Release";
  case EFF_NOTE_ADJUST:
    return "Note adjustment";
#if 0
  case EFF_OCTAVE_ADJUST:
    return "Octave adjustment";      
#endif
  default:
    RError("Unknown effect number %d\n",effect_num);
    return NULL;
  }
}

static int get_effect_num(const struct SoundPluginType *plugin_type, const char *effect_name){
  int i;
  for(i=0;i<EFF_NUM_EFFECTS;i++)
    if(!strcmp(effect_name,get_effect_name(plugin_type,i)))
      return i;

  RError("Unknown effect name \"%s\"",effect_name);

  return 0;
}

static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state){
  const char *filename          = OS_get_resolved_file_path(HASH_get_string(state, "filename"));
  int         instrument_number = HASH_get_int(state, "instrument_number");
  int         resampler_type    = HASH_get_int(state, "resampler_type");

  if(filename==NULL)
    return;

  if(set_new_sample(plugin,filename,instrument_number,resampler_type)==false)
    RWarning("Could not load soundfile \"%s\". (instrument number: %d)\n",filename,instrument_number);
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  HASH_put_string(state, "filename",data->filename);
  HASH_put_int(state, "instrument_number",data->instrument_number);
  HASH_put_int(state, "resampler_type",data->resampler_type);
}

static SoundPluginType plugin_type = {
 type_name                : "Sample Player",
 name                     : "Sample Player",
 info                     : "Sample Player can load XI intruments, Soundfonts, and all types of sample formats supported by libsndfile. WAV files are looped using \"Loop Start\" and \"Loop End\" cue id's.\n\nSoundFonts often sound better when played with FluidSynth instead (the Soundfont handling in Sample Player needs more care). However, the Sample Player uses less memory, are faster to create, has sample-accurate note scheduling, supports polyphonic aftertouch, and has configurable options such as attack, decay, sustain, and release.",
 num_inputs               : 0,
 num_outputs              : 2,
 is_instrument            : true,
 note_handling_is_RT      : false,
 num_effects              : EFF_NUM_EFFECTS,
 get_effect_format        : NULL,
 get_effect_num           : get_effect_num,
 get_effect_name          : get_effect_name,
 get_display_value_string : NULL,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : play_note,
 set_note_volume  : set_note_volume,
 stop_note        : stop_note,
 set_effect_value : set_effect_value,
 get_effect_value : get_effect_value,
 get_display_value_string : get_display_value_string,

 recreate_from_state : recreate_from_state,
 create_state        : create_state,

 data                     : NULL
};

void create_sample_plugin(void){
  PR_add_plugin_type(&plugin_type);
}
