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

//#include <glib.h>
//#include <endian.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <sndfile.h>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/read_binary.h"
#include "../common/PEQ_LPB_proc.h"
#include "../common/PEQ_Signature_proc.h"
#include "../common/visual_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "Mixer_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "Resampler_proc.h"
#include "Envelope_proc.h"
#include "SoundFonts_proc.h"

#include "Sampler_plugin_proc.h"

#define POLYPHONY 256
#define MAX_NUM_SAMPLES 256
#define CROSSFADE_BUFFER_LENGTH 128
#define MAX_CROSSFADE_LENGTH (48000*5) // in samples.

//#define DEFAULT_A 20
//#define DEFAULT_H 5
//#define DEFAULT_D 20
//#define DEFAULT_S 0.5
//#define DEFAULT_R 20

#define DEFAULT_A 0
#define DEFAULT_H 0
#define DEFAULT_D 0
#define DEFAULT_S 1.0
#define DEFAULT_R 0

#define MAX_A 1000
#define MAX_H 40
#define MAX_D 1000
#define MAX_S 1.0
#define MAX_R 2000

#define MAX_VIBRATO_SPEED 20
#define MAX_VIBRATO_DEPTH 5

#define MAX_TREMOLO_SPEED 50
#define MAX_TREMOLO_DEPTH 1

const char *g_click_name = "Click";

// Effect order.
//
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
  EFF_VIBRATO_SPEED,
  EFF_VIBRATO_DEPTH,
  EFF_TREMOLO_SPEED,
  EFF_TREMOLO_DEPTH,
  EFF_LOOP_ONOFF,
  EFF_CROSSFADE_LENGTH,
  EFF_REVERSE,
  EFF_PINGPONG,
  EFF_NUM_EFFECTS
  };

#define SAMPLES_PER_PEAK 64

struct _Data;
typedef struct _Data Data;

typedef struct{
  float volume;

  int num_frames;
  int loop_start;
  int loop_end;

  int ch;        // -1 means both channels.
  float *sound;

  float *min_peaks;
  float *max_peaks;

  double frequency_table[128];

  Data *data;
} Sample;

// A voice object points to only one sample. Stereo-files uses two voice objects. Soundfonts using x sounds to play a note, need x voice objects to play that note.
typedef struct _Voice{
  struct _Voice *prev;
  struct _Voice *next;

  float last_finetune_value;

  float note_num;
  int64_t note_id;

  // These two variables are used when setting velocity after a note has started playing.
  float start_volume;
  float end_volume;
  //double gain;

  float crossfade_buffer[CROSSFADE_BUFFER_LENGTH];

  // Same for pitch
  float pitch;
  float start_pitch;
  float end_pitch;

  Panvals pan;

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

// The part of "Data" that can be memcpy-ed when creating a new Data from old Data.
typedef struct{
  float startpos;

  float finetune; // -100 -> +100
  float note_adjust; // -6 -> +6      (must be float because of conversions)
  //float octave_adjust; // -10 -> +10. (must be float because of conversions)

  float a,h,d,s,r;

  DEFINE_ATOMIC(bool, loop_onoff);
  int crossfade_length;

  DEFINE_ATOMIC(bool, reverse);
  DEFINE_ATOMIC(bool, pingpong);
  
  double vibrato_depth;
  double vibrato_speed;
  double vibrato_phase_add;
  double vibrato_phase;
  double vibrato_value;

  float tremolo_depth;
  float tremolo_speed;

} CopyData;

struct _Data{

  CopyData p;

  // Should loop start/length be placed in CopyData?
  int loop_start;
  int loop_length;   // if 0, use loop data in sample file instead, if there is any. (loop_start has no meaning if loop_length is 0)

  struct SoundPlugin *tremolo;
  
  float samplerate; // The system samplerate. I.e. the jack samplerate, not soundfile samplerate.

  int resampler_type;
  const wchar_t *filename;
  int instrument_number;
  bool using_default_sound;
  
  //int num_channels; // not used for anything, I think.

  const Note notes[128];

  Voice *voices_playing;
  Voice *voices_not_playing;

  int num_different_samples;  // not used for anything (important).

  Voice voices[POLYPHONY];
  const Sample samples[MAX_NUM_SAMPLES];

  // These two are used when switching sound on the fly
  struct _Data *new_data;
  RSemaphore *signal_from_RT;

};



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


/**********************************
  Crossfading code
***********************************/
static void RT_fade_replace(float *dst, float *src, int num_frames, float start_val, float end_val){
  float mul = start_val;
  float inc = (end_val-start_val)/(float)num_frames;
  int i;
  for(i=0;i<num_frames;i++){
    dst[i] = src[i]*mul;
    mul += inc;
  }
}

static void RT_fade_add(float *dst, float *src, int num_frames, float start_val, float end_val){
  float mul = start_val;
  float inc = (end_val-start_val)/(float)num_frames;
  int i;
  for(i=0;i<num_frames;i++){
    dst[i] += src[i]*mul;
    mul += inc;
  }
}

static long RT_crossfade(int start_pos, int end_pos, int crossfade_start, int crossfade_end, float *out_data, float *in_data){
  int num_frames = end_pos - start_pos;

  float start_fade_val = scale(start_pos,
                               crossfade_start, crossfade_end,
                               1.0f, 0.0f
                               );

  float end_fade_val  = scale(end_pos,
                              crossfade_start, crossfade_end,
                              1.0f, 0.0f
                              );

  //printf("fade out: %d -> %d\n",start_pos, start_pos+num_frames);
  //printf("fade in:  %d -> %d\n\n", start_pos2, start_pos2+num_frames);
  //len_in_data-end_pos, len_in_data-end_pos+num_frames);
  //printf("%f -> %f\n\n",start_fade_val,end_fade_val);

  RT_fade_replace(
                  out_data,
                  in_data + start_pos,
                  num_frames,
                  start_fade_val, end_fade_val
                  );
  RT_fade_add(
                 out_data,
                 in_data + (start_pos - crossfade_start),
                 num_frames,
                 1.0f - start_fade_val, 1.0f - end_fade_val
              );

  return num_frames;
}

static int RT_legal_crossfade_length(const Sample *sample, Data *data){
  int crossfade_length = data->p.crossfade_length;
  int loop_length = sample->loop_end - sample->loop_start;

  return R_MIN(crossfade_length, loop_length/2);
}

static long RT_src_callback_with_crossfade_do_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  *out_data = voice->crossfade_buffer;
  int len_out_data = CROSSFADE_BUFFER_LENGTH;

  int end_pos = start_pos + len_out_data;
  if (end_pos > sample->loop_end)
    end_pos = sample->loop_end;

  int legal_crossfade_length = RT_legal_crossfade_length(sample, data);

  voice->pos = end_pos; // next
  if (voice->pos==sample->loop_end)
    voice->pos = sample->loop_start + legal_crossfade_length;
  //printf("crossfading %d -> %d-%d -> %d (%d)\n",sample->loop_start,start_pos,end_pos,sample->loop_end,voice->pos);

  //printf("do looping %d\n");

  float *in_data = voice->sample->sound;

  return RT_crossfade(start_pos, end_pos,
                      sample->loop_end - legal_crossfade_length, sample->loop_end,
                      *out_data, in_data
                      );
}

static long RT_src_callback_with_crossfade_between_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  *out_data = sample->sound + voice->pos;
  int prev_voice_pos = voice->pos;
  voice->pos = sample->loop_end - RT_legal_crossfade_length(sample, data); // next
  return voice->pos - prev_voice_pos;
}

static long RT_src_callback_with_crossfade_before_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  *out_data = sample->sound;
  voice->pos = sample->loop_end - RT_legal_crossfade_length(sample, data); // next
  return voice->pos; // start_pos==0 here.
}

static long RT_src_callback_with_crossfade_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  //printf("crossfading %d -> %d -> %d (%d)\n",sample->loop_start,start_pos,sample->loop_end,voice->pos);

  if(start_pos==0 && sample->loop_start>0)
    return RT_src_callback_with_crossfade_before_looping(voice, sample, data, start_pos, out_data);

  else if (start_pos >= sample->loop_end - RT_legal_crossfade_length(sample, data))
    return RT_src_callback_with_crossfade_do_looping(voice, sample, data, start_pos, out_data);

  else
    return RT_src_callback_with_crossfade_between_looping(voice, sample, data, start_pos, out_data);
}
/**********************************
  End of crossfading code
***********************************/



static long RT_src_callback_with_normal_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  voice->pos = sample->loop_start; // next

  if(start_pos >= sample->loop_end) // just in case. not sure if this can happen
    return 0;

  else
    return sample->loop_end - start_pos;
}



static long RT_src_callback_nolooping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  *out_data = &sample->sound[start_pos];

  if(start_pos==sample->num_frames)
    return 0;

  voice->pos=sample->num_frames; // next

  long ret = sample->num_frames - start_pos;

  if (ret < 0){
#if defined(RELEASE)
    ret = 0;
#else
    fprintf(stderr, "num_frames: %d, start_pos: %d\n",sample->num_frames, start_pos);
    abort();
#endif
  }

  return ret;
}

static long RT_src_callback_reverse(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data, bool do_looping){
  *out_data = &voice->crossfade_buffer[0];

  if(start_pos==sample->num_frames) {
    if (do_looping)
      start_pos = 0;
    else
      return 0;
  }

  int samples_left = sample->num_frames - start_pos;

  int num_samples_to_return = R_MIN(samples_left, CROSSFADE_BUFFER_LENGTH);

  float *source_sound = sample->sound;
  float *dest_sound = &voice->crossfade_buffer[0];
  int sample_pos = sample->num_frames-1 - start_pos;
 
  int i;
  for(i=0 ; i< num_samples_to_return ; i++)
    dest_sound[i] = source_sound[sample_pos--];

  
  voice->pos = start_pos + num_samples_to_return; // i.e. next pos

  return num_samples_to_return;
}

static long RT_src_callback_ping_pong_looping(Voice *voice, const Sample *sample, Data *data, int start_pos, float **out_data){
  const int num_sample_frames = sample->num_frames;
  
  R_ASSERT(start_pos <= num_sample_frames*2);
  
  if (start_pos >= num_sample_frames*2)
    start_pos = 0;

  if (start_pos >= num_sample_frames) {
    int ret = RT_src_callback_reverse(voice, sample, data, start_pos - num_sample_frames, out_data, false);
    voice->pos += num_sample_frames;
    return ret;
  } else
    return RT_src_callback_nolooping(voice, sample, data, start_pos, out_data);
}



static long RT_src_callback(void *cb_data, float **out_data){
  Voice *voice         = cb_data;
  const Sample *sample = voice->sample;
  int start_pos        = voice->pos;
  Data  *data          = sample->data;

  bool pingpong = ATOMIC_GET(sample->data->p.pingpong);
  bool reverse = ATOMIC_GET(sample->data->p.reverse);
  bool loop = ATOMIC_GET(sample->data->p.loop_onoff);
  
  if (pingpong) {
    
    return RT_src_callback_ping_pong_looping(voice, sample, data, start_pos, out_data); // ping pong looping

  } else {

    if (start_pos >= sample->num_frames) // Happens when switching from ping-pong to non-ping-pong while playing.
      start_pos = sample->num_frames - (start_pos - sample->num_frames); // Keep same sample position.
    
    if (reverse && loop)
      return RT_src_callback_reverse(voice, sample, data, start_pos, out_data, true); //loop reverse
    
    else if (reverse && !loop)
      return RT_src_callback_reverse(voice, sample, data, start_pos, out_data, false); //only reverse, no looping
    
    else if(!loop || sample->loop_end <= sample->loop_start)
      return RT_src_callback_nolooping(voice, sample, data, start_pos, out_data);
    
    else if(data->p.crossfade_length > 0)
      return RT_src_callback_with_crossfade_looping(voice, sample, data, start_pos, out_data);
    
    else
      return RT_src_callback_with_normal_looping(voice, sample, data, start_pos, out_data);

  }
}




static double RT_get_src_ratio3(Data *data, const Sample *sample, float pitch){    
  if(pitch<=0.0)
    pitch=0.0f;
  if(pitch>126)
    pitch=126.0f;

  int notenum = (int)pitch;
  float finetune = pitch - notenum;

  return data->samplerate / scale(finetune, 0, 1, sample->frequency_table[notenum], sample->frequency_table[notenum+1]);
}

// Note: Also called from get_peaks
static double RT_get_src_ratio2(Data *data, const Sample *sample, float pitch){

  //printf("note_adjust: %d (%f)\n",(int)data->p.note_adjust,data->p.note_adjust);
  double adjusted_pitch = pitch + scale(data->p.finetune, 0, 1, -1, 1) + (int)data->p.note_adjust;
  return RT_get_src_ratio3(data, sample, adjusted_pitch);
}

static double RT_get_src_ratio(Data *data, Voice *voice){
  const Sample *sample = voice->sample;

  //int notenum = voice->note_num + (int)data->octave_adjust*12 + (int)data->p.note_adjust;
  //int notenum = voice->note_num + (int)data->p.note_adjust;
  //float pitch = voice->end_pitch + scale(data->p.finetune, 0, 1, -1, 1) + (int)data->p.note_adjust;

  float pitch = voice->pitch;

  // Add vibrato here instead of in get_src_ratio3 to avoid weird peaks
  if (data->p.vibrato_phase_add > 0.0) {
    pitch += data->p.vibrato_value;
    //printf("%f ,%f",data->p.vibrato_depth,data->p.vibrato_value);
  }

  return RT_get_src_ratio2(data,sample,pitch);
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

#if 0
static float get_peak(float *samples, int num_samples){
  float ret=0.0f;
  int i;
  for(i=0;i<num_samples;i++){
    float m=fabsf(samples[i]);
    if(m>ret)
      ret=m;
  }
  return ret;
}
#endif

static bool RT_play_voice(Data *data, Voice *voice, int num_frames_to_produce, float **outputs){
  // portamento
  {
#if 1
    voice->pitch = voice->end_pitch;
#else
    const float how_much = 0.1;
    if (voice->end_pitch > voice->pitch){
      voice->pitch += how_much;
      if (voice->pitch > voice->end_pitch)
        voice->pitch = voice->end_pitch;
    } else if (voice->end_pitch < voice->pitch){
      voice->pitch -= how_much;
      if (voice->pitch < voice->end_pitch)
        voice->pitch = voice->end_pitch;
    }
#endif
  }
  
  int startpos = voice->delta_pos_at_start;
  int endpos = voice->delta_pos_at_end;

  if(startpos>=0)
    voice->delta_pos_at_start = 0;
  if(endpos>=0)
    voice->delta_pos_at_end = -1;


  if(endpos>=0 && endpos<startpos) // Should not happen. Test for it just in case. It's a bit messy when notes are generated by the keyboard, player and other places at the same time.
    {
#if !defined(RELEASE)
      printf("Oops. Endpos: %d. startpos: %d\n",endpos,startpos); // FIX: We get here when the CPU is very buzy.
#endif
      return false;
    }

  int num_frames = num_frames_to_produce-startpos;

  R_ASSERT_NON_RELEASE(num_frames >= 0);
    
  if (num_frames <= 0)
    return false;
  
  float resampled_data[num_frames];
  int frames_created_by_resampler = RT_get_resampled_data(data,voice,resampled_data,num_frames);
  //printf("Frames created by resampler: %d\n",frames_created_by_resampler);
  //printf("peak: %f\n",get_peak(resampled_data,frames_created_by_resampler));

  int frames_created_by_envelope;

  float *adsr_sound_data[1]={&resampled_data[0]};

  if(endpos>=0){
    int pre_release_len = endpos-startpos;

    //printf("********** endpos>0: %d. prelen: %d, frames_created_by_resampler: %d\n",endpos,prelen,frames_created_by_resampler);

    if(frames_created_by_resampler <= pre_release_len){ // i.e. we reached the end of sound before beginning to release the ADSR envelope.

      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);

    }else{
      frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, pre_release_len);

      //printf("************************ Calling adsr release\n");
      ADSR_release(voice->adsr);

      int post_release_len = frames_created_by_resampler - frames_created_by_envelope;
      adsr_sound_data[0] = &resampled_data[frames_created_by_envelope];        
      frames_created_by_envelope += ADSR_apply(voice->adsr, adsr_sound_data, 1, post_release_len);
    }

  }else{

    frames_created_by_envelope = ADSR_apply(voice->adsr, adsr_sound_data, 1, frames_created_by_resampler);
    //printf("Frames created by envelope: %d, peak: %f\n",frames_created_by_envelope,get_peak(resampled_data,frames_created_by_envelope));
    //printf("peak: %f\n",get_peak(resampled_data,frames_created_by_resampler));
  }

  //float peak_in = get_peak(resampled_data,frames_created_by_envelope);

  const Sample *sample = voice->sample;

#define mix(input_channel, output_channel) do{                          \
    float panval = voice->pan.vals[input_channel][output_channel];      \
    if(panval>0.0f){                                                    \
      float *out          = outputs[output_channel] + startpos;         \
      float  start_volume = voice->start_volume*panval;                 \
      float  end_volume   = voice->end_volume*panval;                   \
      SMOOTH_mix_sounds_raw(out, resampled_data, frames_created_by_envelope, start_volume, end_volume); \
    }                                                                   \
  }while(0)

  if(sample->ch == -1){
    mix(0,0);
    mix(0,1);
  }else{
    mix(sample->ch,0);
    mix(sample->ch,1);
  }

  //printf("peak in/out: %.3f - %.3f\n",peak_in,get_peak(outputs[0], num_frames_to_produce));

  voice->start_volume = voice->end_volume;
  voice->start_pitch = voice->end_pitch;

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

  if (data->p.vibrato_phase_add > 0.0) {
    data->p.vibrato_value = data->p.vibrato_depth * sin(data->p.vibrato_phase);
    data->p.vibrato_phase += data->p.vibrato_phase_add*(double)num_frames;
  }
  
  bool was_playing_something = data->voices_playing != NULL;
  
  while(voice!=NULL){
    Voice *next = voice->next;

    if(RT_play_voice(data, voice, num_frames, outputs)==true){
      RT_remove_voice(&data->voices_playing, voice);
      RT_add_voice(&data->voices_not_playing, voice);
    }

    voice = next;
  }

  if (was_playing_something)
    data->tremolo->type->RT_process(data->tremolo, time, num_frames, outputs, outputs);
          
  if(data->new_data != NULL){
    RT_fade_out(outputs[0],num_frames);
    RT_fade_out(outputs[1],num_frames);

    plugin->data = data->new_data; // Bang! (hmm.)
    data->new_data = NULL;

    RSEMAPHORE_signal(data->signal_from_RT,1);
  }
}


static void play_note(struct SoundPlugin *plugin, int64_t time, note_t note2){
  Data *data = (Data*)plugin->data;

  //fprintf(stderr,"playing note %d. Pitch: %d, time: %d\n",(int)note_id,(int)note_num,(int)time);

  const Note *note = &data->notes[(int)note2.pitch];

  int i;
  for(i=0;i<note->num_samples;i++){

    if(data->voices_not_playing==NULL){
      printf("No more free voices\n");
      return;
    }

    Voice *voice = data->voices_not_playing;
    
    RT_remove_voice(&data->voices_not_playing, voice);
    RT_add_voice(&data->voices_playing, voice);
    
    voice->last_finetune_value = data->p.finetune;
    
    voice->note_num = note2.pitch;
    voice->note_id = note2.id;

    voice->start_volume = velocity2gain(note2.velocity);
    voice->end_volume = voice->start_volume;

    voice->pitch       = note2.pitch;
    voice->start_pitch = note2.pitch;
    voice->end_pitch   = note2.pitch;

    const Sample *sample = note->samples[i];
    
    voice->sample = sample;

    if(ATOMIC_GET(data->p.loop_onoff)==true && sample->loop_end > sample->loop_start)
      voice->pos=scale(data->p.startpos, // set startpos between 0 and loop_end
                       0,1,
                       0,sample->loop_end);
    else
      voice->pos=scale(data->p.startpos,  // set startpos between 0 and sound length
                       0,1,
                       0,sample->num_frames);

    voice->pan = get_pan_vals_vector(note2.pan,voice->sample->ch==-1?1:2);
        
    RESAMPLER_reset(voice->resampler);
    ADSR_reset(voice->adsr);
    ADSR_set_adsr(voice->adsr, data->p.a, data->p.h, data->p.d, data->p.s, data->p.r);

    voice->delta_pos_at_start=time;
    voice->delta_pos_at_end=-1;
    voice->is_fading_out=false;
  }
}


static void set_note_volume(struct SoundPlugin *plugin, int64_t time, note_t note){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  while(voice!=NULL){
    //printf("Setting volume to %f. note_num: %d. voice: %d\n",volume,note_num,voice->note_num);

    if(voice->note_id==note.id)
      voice->end_volume = velocity2gain(note.velocity);

    voice = voice->next;
  }
}

static void set_note_pitch(struct SoundPlugin *plugin, int64_t time, note_t note){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  //printf("Setting pitch for %d (%f) to %f.\n",(int)note_id,note_num,pitch);

  while(voice!=NULL){

    if(voice->note_id==note.id){
      voice->end_pitch = note.pitch;
      //printf("Got it\n");
    }

    voice = voice->next;
  }
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, note_t note){
  Data *data = (Data*)plugin->data;

  Voice *voice = data->voices_playing;

  if(time==-1){
    RError("time==-1 at stop_note. Will cause hanging note.");
    time=0;
  }

  while(voice!=NULL){
    if(voice->note_id==note.id){
      if(voice->delta_pos_at_end == -1)
        voice->delta_pos_at_end = time;
      //voice->end_volume = velocity2gain(volume); // no no no. end_volume is for change velocity only. If volume==0, note ends here, not when release is finished. (the volume argument for stop_note is probably completely useless)
    }

    voice = voice->next;
  }
}

static bool note_has_sample(const Note *note){
  int samplenum;
  
  for(samplenum=0;samplenum<note->num_samples;samplenum++)
    if (note->samples[samplenum]!=NULL)
      return true;

  return false;
}

static int time_to_frame(Data *data, double time, float f_note_num){

  int i_note_num = (int)f_note_num;
  
  const Sample *sample=NULL;
  int samplenum = 0;
  
  const Note *note=&data->notes[(int)i_note_num];
  
  for(;;){
    sample = note->samples[samplenum];
    if (sample!=NULL)
      break;
    
    samplenum++;
    if (samplenum==note->num_samples) {
      RError("samplenum==num_samples. %f\n",f_note_num);
      return data->p.startpos*sample->num_frames + time/30000.0f;
    }
  }
  

  double src_ratio = RT_get_src_ratio2(data, sample, f_note_num);

  return
    data->p.startpos*sample->num_frames 
    + time/src_ratio ;
}


static void apply_adsr_to_peak(Data *data, int64_t time, float *min_value, float *max_value){
  float ms = time*1000 / data->samplerate;
  float mul;

  if(ms >= data->p.a+data->p.h+data->p.d)
    mul = data->p.s;

  else if(ms >= data->p.a+data->p.h)
    mul = scale(ms,
                (data->p.a+data->p.h),
                (data->p.a+data->p.h+data->p.d), 
                1.0f,
                data->p.s);

  else if(ms >= data->p.a)
    mul = 1.0f;

  else
    mul = scale(ms,
                0.0f,data->p.a,
                0.0f,1.0f);


  *min_value = *min_value * mul;
  *max_value = *max_value * mul;
}

static bool get_peak_sample(const Sample *sample, int64_t framenum, float *min_value, float *max_value){

  if(ATOMIC_GET(sample->data->p.loop_onoff)==true && framenum>=sample->loop_end && sample->loop_end>sample->loop_start){

    framenum -= sample->loop_end; // i.e. how far after loop end are we?

    int loop_length = sample->loop_end - sample->loop_start;
    int num_loops = framenum / loop_length;
    framenum -= (num_loops*loop_length);

    framenum += sample->loop_start;

  }

  if(framenum >= sample->num_frames)
    return false;

  int peak_pos = framenum/SAMPLES_PER_PEAK;
  *min_value = sample->min_peaks[peak_pos];
  *max_value = sample->max_peaks[peak_pos];

  return true;
}

static void get_peaks_from_sample(const Sample *sample, int64_t start_frame, int64_t end_frame, float *min_value, float *max_value){
  float min=0.0f;
  float max=0.0f;

  int interval = R_MAX(1, (end_frame-start_frame) / 5); //SAMPLES_PER_PEAK);
  //  if (interval < SAMPLES_PER_PEAK / 2)
  //  interval = SAMPLES_PER_PEAK / 2;
  
  int framenum;
  for(framenum=start_frame ; framenum<end_frame ; framenum+=interval){ //SAMPLES_PER_PEAK){
    float min2;
    float max2;

    if(get_peak_sample(sample,framenum,&min2,&max2)==false)
      break;
    if(min2<min)
      min=min2;
    if(max2>max)
      max=max2;
  }

  *min_value = min;
  *max_value = max;
}

static int get_peaks(struct SoundPlugin *plugin,
                     float note_num,
                     int ch,
                     float das_pan,
                     int64_t start_time,
                     int64_t end_time,
                     float *min_value, float *max_value
                     )
{
  Data *data = (Data*)plugin->data;

  if(ch==-1){
    int i;
    for(i=0;i<MAX_NUM_SAMPLES;i++){
      Sample *sample=(Sample*)&data->samples[i];
      if(sample->sound!=NULL){
        if(sample->ch==1)
          return 2;
      }
    }
    return 1;
  }

  R_ASSERT_RETURN_IF_FALSE2(note_num >= 0.0f, 2);

  const Note *note=&data->notes[(int)note_num];

  if (!note_has_sample(note)){   
    *min_value = 0.0f;
    *max_value = 0.0f;
    return 2;
  } 
  
  int start_frame = time_to_frame(data, start_time, note_num);
  int end_frame = time_to_frame(data, end_time, note_num);

  {

    float min=0.0f;
    float max=0.0f;

    int samplenum;

    for(samplenum=0;samplenum<note->num_samples;samplenum++){
      const Sample *sample=note->samples[samplenum];

      Panvals pan = get_pan_vals_vector(das_pan, sample->ch==-1 ? 1 : 2);
      int input_channel = sample->ch==-1 ? 0 : sample->ch;
      float panval = pan.vals[input_channel][ch];

      if(panval>0.0f){
        
        float min2;
        float max2;
        
        get_peaks_from_sample(sample, start_frame, end_frame, &min2, &max2);
        
        min2 *= panval;
        max2 *= panval;
        
        if(min2<min)
          min=min2;
        if(max2>max)
          max=max2;
        
      }
    }
  
    *min_value = min;
    *max_value = max;
  }

  apply_adsr_to_peak(data, (start_time+end_time)/2, min_value, max_value);
  
  return 2;
}

static void update_peaks(SoundPlugin *plugin){
#if 0
  struct Tracker_Windows *window=root->song->tracker_windows;
  struct WBlocks *wblock=window->wblock;
  TRACKREALLINES_update_peak_tracks(window,plugin->patch);
  DrawUpAllPeakWTracks(window,wblock,plugin->patch);
#endif

#if USE_OPENGL
  GFX_ScheduleEditorRedraw();

#else
  if(plugin->patch!=NULL)
    RT_TRACKREALLINES_schedule_update_peak_tracks(plugin->patch);
#endif
}

static void set_loop_onoff(Data *data, bool loop_onoff){
  ATOMIC_SET(data->p.loop_onoff, loop_onoff);
}

static bool get_loop_onoff(Data *data){
  return ATOMIC_GET(data->p.loop_onoff);
}

static bool can_crossfade(Data *data){
  return ATOMIC_GET(data->p.reverse)==false && ATOMIC_GET(data->p.pingpong)==false;
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  if(value_format==PLUGIN_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      data->p.startpos = value;
      update_peaks(plugin);
      break;
    case EFF_FINETUNE:
      data->p.finetune = value;
      update_peaks(plugin);
      break;
    case EFF_A:
      data->p.a = scale(value,
                      0.0,1.0,
                      0,MAX_A);
      update_peaks(plugin);
      break;
    case EFF_H:
      data->p.h = scale(value,
                      0.0,1.0,
                      0,MAX_H);
      update_peaks(plugin);
      break;
    case EFF_D:
      data->p.d = scale(value,
                      0.0,1.0,
                      0,MAX_D);
      update_peaks(plugin);
      break;
    case EFF_S:
      data->p.s = scale(value,
                      0.0,1.0,
                      0,MAX_S);
      update_peaks(plugin);
      break;
    case EFF_R:
      data->p.r = scale(value,
                      0.0,1.0,
                      0,MAX_R);
      break;
    case EFF_VIBRATO_SPEED:
      data->p.vibrato_speed = scale(value,
                                  0.0,1.0,
                                  0,MAX_VIBRATO_SPEED);

      if (data->p.vibrato_speed <= 0.001) {
        data->p.vibrato_value = 0.0;
        data->p.vibrato_phase = 4.71239;
        data->p.vibrato_phase_add = -1;
      } else
        data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      
      break;
    case EFF_VIBRATO_DEPTH:
      data->p.vibrato_depth = scale(value,
                                  0.0,1.0,
                                  0,MAX_VIBRATO_DEPTH);
      if (data->p.vibrato_depth <= 0.001) {
        data->p.vibrato_value = 0.0;
        data->p.vibrato_phase = 4.71239;
        data->p.vibrato_phase_add = -1;    
      } else
        data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      break;
    case EFF_TREMOLO_SPEED:
      data->p.tremolo_speed = scale(value,
                                  0.0,1.0,
                                  0,MAX_TREMOLO_SPEED);
      data->tremolo->type->set_effect_value(data->tremolo, time, 0, data->p.tremolo_speed, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFF_TREMOLO_DEPTH:
      data->p.tremolo_depth = scale(value,
                                  0.0,1.0,
                                  0,MAX_TREMOLO_DEPTH);
      data->tremolo->type->set_effect_value(data->tremolo, time, 1, data->p.tremolo_depth, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFF_NOTE_ADJUST:
      data->p.note_adjust = scale(value,
                                0.0,1.0,
                                -6.99,6.99);
      update_peaks(plugin);
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->p.octave_adjust = scale(value,
                                  0,1,
                                  -10.99,10.99);
      break;
#endif
    case EFF_LOOP_ONOFF:
      set_loop_onoff(data, value>=0.5f);
      update_peaks(plugin);
      break;
      
    case EFF_CROSSFADE_LENGTH:
      if (can_crossfade(data))
        data->p.crossfade_length = scale(value,
                                         0.0, 1.0,
                                         0, MAX_CROSSFADE_LENGTH
                                         );
      else
        data->p.crossfade_length = 0;
      
      break;
      
    case EFF_REVERSE:      
      ATOMIC_SET(data->p.reverse, value>=0.5f);
      if (!can_crossfade(data)){
        //printf("Doing it %p\n",plugin->patch);
        if (when==FX_single)
          PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, when); // i.e. not automation
        else
          data->p.crossfade_length = 0; // i.e. automation
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
                                 
      break;
      
    case EFF_PINGPONG:
      ATOMIC_SET(data->p.pingpong, value>=0.5f);
      if (!can_crossfade(data)){
        if (when==FX_single)
          PLUGIN_set_effect_value(plugin, time, EFF_CROSSFADE_LENGTH, 0, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, when); // i.e. not automation
        else
          data->p.crossfade_length = 0; // i.e. automation
        if (plugin->patch != NULL)
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
      }
      break;
      
    default:
      RError("S1. Unknown effect number %d\n",effect_num);
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      R_ASSERT(value >= 0.0f);
      R_ASSERT(value <= 1.0f);
      data->p.startpos = value;
      update_peaks(plugin);
      break;
    case EFF_FINETUNE:
      data->p.finetune = value;
      update_peaks(plugin);
      break;
    case EFF_A:
      data->p.a = value;
      update_peaks(plugin);
      break;
    case EFF_H:
      data->p.h = value;
      update_peaks(plugin);
      break;
    case EFF_D:
      data->p.d = value;
      update_peaks(plugin);
      break;
    case EFF_S:
      data->p.s = value;
      update_peaks(plugin);
      break;
    case EFF_R:
      data->p.r = value;
      break;
    case EFF_VIBRATO_SPEED:
      data->p.vibrato_speed = value;
      data->p.vibrato_phase_add = data->p.vibrato_speed * 2.0 * M_PI / data->samplerate;
      break;
    case EFF_VIBRATO_DEPTH:
      data->p.vibrato_depth = value;
      break;
    case EFF_TREMOLO_SPEED:
      data->p.tremolo_speed = value;
      data->tremolo->type->set_effect_value(data->tremolo, time, 0, data->p.tremolo_speed, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFF_TREMOLO_DEPTH:
      data->p.tremolo_depth = value;
      data->tremolo->type->set_effect_value(data->tremolo, time, 1, data->p.tremolo_depth, PLUGIN_FORMAT_NATIVE, when);
      break;
    case EFF_NOTE_ADJUST:
      data->p.note_adjust = value;
      update_peaks(plugin);
      break;
#if 0
    case EFF_OCTAVE_ADJUST:
      data->p.octave_adjust = value;
      break;
#endif
    case EFF_LOOP_ONOFF:
      set_loop_onoff(data, value>=0.5f);
      update_peaks(plugin);
      break;

    case EFF_CROSSFADE_LENGTH:
      data->p.crossfade_length = value;
      break;

    case EFF_REVERSE:
      ATOMIC_SET(data->p.reverse, value>=0.5f);
      break;
      
    case EFF_PINGPONG:
      ATOMIC_SET(data->p.pingpong, value>=0.5f);
      break;
      
    default:
      RError("S2. Unknown effect number %d\n",effect_num);
    }
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;

  if(value_format==PLUGIN_FORMAT_SCALED){
    switch(effect_num){
    case EFF_STARTPOS:
      return data->p.startpos;
    case EFF_FINETUNE:
      return data->p.finetune;
    case EFF_A:
      return scale(data->p.a,
                   0,MAX_A,
                   0.0,1.0);
    case EFF_H:
      return scale(data->p.h,
                   0,MAX_H,
                   0.0,1.0);
    case EFF_D:
      return scale(data->p.d,
                   0,MAX_D,
                   0.0,1.0);
    case EFF_S:
      return scale(data->p.s,
                   0,MAX_S,
                   0.0,1.0);
    case EFF_R:
      return scale(data->p.r,
                   0,MAX_R,
                   0.0,1.0);
    case EFF_VIBRATO_SPEED:
      return scale(data->p.vibrato_speed,
                   0,MAX_VIBRATO_SPEED,
                   0.0,1.0
                   );
    case EFF_VIBRATO_DEPTH:
      return scale(data->p.vibrato_depth,
                   0,MAX_VIBRATO_DEPTH,
                   0.0,1.0
                   );      
    case EFF_TREMOLO_SPEED:
      return scale(data->p.tremolo_speed,
                   0,MAX_TREMOLO_SPEED,
                   0.0,1.0
                   );
    case EFF_TREMOLO_DEPTH:
      return scale(data->p.tremolo_depth,
                   0,MAX_TREMOLO_DEPTH,
                   0.0,1.0
                   );      
    case EFF_NOTE_ADJUST:
      return scale(data->p.note_adjust,
                   -6.99,6.99,
                   0,1);
#if 0
    case EFF_OCTAVE_ADJUST:
      return scale(data->p.octave_adjust,
                   -10.99,10.99,
                   0,1);
#endif
    case EFF_LOOP_ONOFF:
      return get_loop_onoff(data)==true?1.0f:0.0f;
      break;

    case EFF_CROSSFADE_LENGTH:
      return scale(data->p.crossfade_length,0,MAX_CROSSFADE_LENGTH,0,1);
      break;

    case EFF_REVERSE:
      return ATOMIC_GET(data->p.reverse)==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return ATOMIC_GET(data->p.pingpong)==true?1.0f:0.0f;
      break;

    default:
      RError("S3. Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }else{
    switch(effect_num){
    case EFF_STARTPOS:
      return data->p.startpos;
    case EFF_FINETUNE:
      return data->p.finetune;
    case EFF_A:
      return data->p.a;
    case EFF_H:
      return data->p.h;
    case EFF_D:
      return data->p.d;
    case EFF_S:
      return data->p.s;
    case EFF_R:
      return data->p.r;
    case EFF_VIBRATO_SPEED:
      return data->p.vibrato_speed;
    case EFF_VIBRATO_DEPTH:
      return data->p.vibrato_depth;
    case EFF_TREMOLO_SPEED:
      return data->p.tremolo_speed;
    case EFF_TREMOLO_DEPTH:
      return data->p.tremolo_depth;
    case EFF_NOTE_ADJUST:
      return data->p.note_adjust;
#if 0
    case EFF_OCTAVE_ADJUST:
      return data->p.octave_adjust;
#endif
    case EFF_LOOP_ONOFF:
      return get_loop_onoff(data)==true?1.0f:0.0f;

    case EFF_CROSSFADE_LENGTH:
      return data->p.crossfade_length;

    case EFF_REVERSE:
      return ATOMIC_GET(data->p.reverse)==true?1.0f:0.0f;
      break;

    case EFF_PINGPONG:
      return ATOMIC_GET(data->p.pingpong)==true?1.0f:0.0f;
      break;

    default:
      RError("S4. Unknown effect number %d\n",effect_num);
      return 0.5f;
    }
  }
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  switch(effect_num){
  case EFF_STARTPOS:
    snprintf(buffer,buffersize-1,"%.2f%%",scale(data->p.startpos,0.0,1.0,0,100));
    break;
  case EFF_FINETUNE:
    snprintf(buffer,buffersize-1,"%f cents",scale(data->p.finetune,0.0,1.0,-100,100));
    break;
  case EFF_A:
    snprintf(buffer,buffersize-1,"%f ms",data->p.a);
    break;
  case EFF_H:
    snprintf(buffer,buffersize-1,"%f ms",data->p.h);
    break;
  case EFF_D:
    snprintf(buffer,buffersize-1,"%f ms",data->p.d);
    break;
  case EFF_S:
    snprintf(buffer,buffersize-1,"%f",data->p.s);
    break;
  case EFF_R:
    snprintf(buffer,buffersize-1,"%f ms",data->p.r);
    break;
  case EFF_VIBRATO_SPEED:
    snprintf(buffer,buffersize-1,"%.1fHz",data->p.vibrato_speed);
    break;
  case EFF_VIBRATO_DEPTH:
    snprintf(buffer,buffersize-1,"%.2f",data->p.vibrato_depth);
    break;
  case EFF_TREMOLO_SPEED:
    snprintf(buffer,buffersize-1,"%.1fHz",data->p.tremolo_speed);
    break;
  case EFF_TREMOLO_DEPTH:
    snprintf(buffer,buffersize-1,"%.2f",data->p.tremolo_depth);
    break;
  case EFF_NOTE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      int adjust = data->p.note_adjust;
      snprintf(buffer,buffersize-1,"%s%d note%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
    }
    break;
#if 0
  case EFF_OCTAVE_ADJUST:
    if(false && data->num_different_samples>1)
      snprintf(buffer,buffersize-1,"disabled (multi-sample instrument)");
    else{
      int adjust=data->p.octave_adjust;
      snprintf(buffer,buffersize-1,"%s%d octave%s",adjust>0?"+":"",adjust,adjust==-1?"":adjust==1?"":"s");
    }
    break;
#endif
  case EFF_CROSSFADE_LENGTH:
    snprintf(buffer,buffersize-1,"%d samples",(int)data->p.crossfade_length);
    break;

  default:
    RError("S5. Unknown effect number %d\n",effect_num);
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



// Note, if start==-1 and end==-1, loop_start is set to 0 and loop_end is set to sample->num_frames, and loop_onoff is not set.
static void set_legal_loop_points(Sample *sample, int start, int end, bool set_loop_on_off){
  if(start==-1 && end==-1){ 
    sample->loop_start=0;
    sample->loop_end=sample->num_frames;
    return;
  }

  if(start<0)
    start=0;

  if(end>sample->num_frames)
    end=sample->num_frames;

  if(end<=start){
    sample->loop_start=0;
    sample->loop_end=sample->num_frames;
  }else{
    sample->loop_start=start;
    sample->loop_end=end;
    if (set_loop_on_off)
      ATOMIC_SET(sample->data->p.loop_onoff, true);
  }
}

#include "Sampler_plugin_wav_parse.c"
#include "Sampler_plugin_xi_load.c"
#include "Sampler_plugin_sf2_load.c"

static float *load_interleaved_samples(const wchar_t *filename, SF_INFO *sf_info){
  SNDFILE *sndfile          = sf_open(STRING_get_chars(filename),SFM_READ,sf_info);
  if(sndfile==NULL)
    return NULL;

  float   *ret              = talloc_atomic(sizeof(float) * sf_info->channels * sf_info->frames);
  int      allocated_frames = sf_info->frames;

  int total_read_frames     = sf_readf_float(sndfile, ret, sf_info->frames);

  if(total_read_frames==0)
    return NULL;

  while(true){
    float samples[1024*sf_info->channels];
    int read_now = sf_readf_float(sndfile, samples, 1024);
    if(read_now==0)
      break;

    if(total_read_frames + read_now > allocated_frames){
      allocated_frames = (total_read_frames+read_now) * 2;
      ret = talloc_realloc(ret, allocated_frames * sizeof(float) * sf_info->channels);
    }

    memcpy(ret + (total_read_frames*sf_info->channels), samples, sizeof(float)*1024*sf_info->channels);

    total_read_frames += read_now;
  }

  sf_close(sndfile);

  sf_info->frames = total_read_frames;
  return ret;
}

static bool load_sample_with_libsndfile(Data *data, const wchar_t *filename, bool set_loop_on_off){
  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  data->num_different_samples = 1;

  float *samples = load_interleaved_samples(filename, &sf_info);

  if(samples==NULL){
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
      Sample *sample     = (Sample*)&data->samples[ch];
      sample->num_frames = sf_info.frames;
      sample->sound       = V_malloc(sizeof(float)*sample->num_frames);
    }

    int interleaved_pos=0;
    int i;
    for(i=0;i<sf_info.frames;i++){
      for(ch=0;ch<sf_info.channels;ch++){
        if(ch<2){
          Sample *sample=(Sample*)&data->samples[ch];
          sample->sound[i] = samples[interleaved_pos];
        }
        interleaved_pos++;
      }
    }

    for(ch=0;ch<num_channels;ch++){     
      Sample *sample=(Sample*)&data->samples[ch];

      set_legal_loop_points(sample,-1,-1, set_loop_on_off); // By default, don't loop, but if set, loop all.
              
      if((sf_info.format&0xffff0000) == SF_FORMAT_WAV){
        printf("format: 0x%x. sections: %d, num_frames: %d. SF_FORMAT_WAV: 0x%x. og: 0x%x\n",sf_info.format,sf_info.sections,(int)sf_info.frames,SF_FORMAT_WAV,sf_info.format&SF_FORMAT_WAV);
        set_wav_loop_points(sample,filename,set_loop_on_off);
      }

      if(num_channels==1)
        sample->ch = -1; // i.e play the sample in both channels.
      else
        sample->ch = ch;

      int i;
      for(i=0;i<128;i++){
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

static void generate_peaks(Data *data){
  float *prev=NULL;
  int sample_num;

  for(sample_num=0;sample_num<MAX_NUM_SAMPLES;sample_num++){
    Sample *sample=(Sample*)&data->samples[sample_num];

    if(sample->sound!=NULL && sample->sound != prev){
      prev = sample->sound;

      float *samples = sample->sound;

      int num_peaks = (sample->num_frames / SAMPLES_PER_PEAK)+10;
      sample->min_peaks = V_malloc(sizeof(float)*num_peaks);
      sample->max_peaks = V_malloc(sizeof(float)*num_peaks);
      
      int i;
      int peaknum=0;
      float min=samples[0];
      float max=min;

      for(i=1;i<sample->num_frames;i++){
        if( (i%SAMPLES_PER_PEAK)==0 || i==sample->num_frames-1){
          sample->min_peaks[peaknum] = min;
          sample->max_peaks[peaknum] = max;
          peaknum++;
          min=0.0f;
          max=0.0f;
        }
        if(samples[i]<min)
          min = samples[i];
        if(samples[i]>max)
          max = samples[i];
      }
    }
  }
}


static bool load_sample(Data *data, const wchar_t *filename, int instrument_number, bool set_loop_on_off){
  if(load_xi_instrument(data,filename, set_loop_on_off)==false)
    if(load_sample_with_libsndfile(data,filename, set_loop_on_off)==false)
      if(load_sf2_instrument(data,filename,instrument_number, set_loop_on_off)==false)
        return false;
  
  //data->num_channels = data->samples[0].num_channels; // All samples must contain the same number of channels.

  generate_peaks(data);

  int i=0;
  for(i=0;i<POLYPHONY;i++){
    Voice *voice = &data->voices[i];

    voice->resampler = RESAMPLER_create(RT_src_callback, 1, voice, data->resampler_type);
    voice->adsr = ADSR_create(data->samplerate);

    RT_add_voice(&data->voices_not_playing, voice);
  }

  return true;
}

static SoundPlugin *create_tremolo(bool is_loading){
  SoundPlugin *ret = V_calloc(1, sizeof(SoundPlugin));
  
  ret->type = PR_get_plugin_type_by_name(NULL, "Faust", "System Tremolo");
  ret->data = ret->type->create_plugin_data(ret->type, ret, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size(), is_loading);
  
  return ret;
}

static void free_tremolo(SoundPlugin *tremolo){
  tremolo->type->cleanup_plugin_data(tremolo);
  V_free(tremolo);
}

static Data *create_data(float samplerate, Data *old_data, const wchar_t *filename, int instrument_number, int resampler_type, bool is_loading){
  Data *data = V_calloc(1,sizeof(Data));

  data->signal_from_RT = RSEMAPHORE_create(0);

  data->tremolo = create_tremolo(is_loading);
      
  if(old_data==NULL){
    
    data->p.finetune = 0.5f;
    
    data->p.a=DEFAULT_A;
    data->p.d=DEFAULT_D;
    data->p.s=DEFAULT_S;
    data->p.r=DEFAULT_R;

    data->p.vibrato_phase_add = -1;
        
  }else{

    memcpy(&data->p, &old_data->p, sizeof(CopyData));
    
    data->p.vibrato_value = 0.0;
    data->p.vibrato_phase = 4.71239;
    
    data->tremolo->type->set_effect_value(data->tremolo, 0, 0, data->p.tremolo_speed, PLUGIN_FORMAT_NATIVE, FX_single);
    data->tremolo->type->set_effect_value(data->tremolo, 0, 1, data->p.tremolo_depth, PLUGIN_FORMAT_NATIVE, FX_single);
    
  }
  
  data->samplerate = samplerate;
  data->resampler_type = resampler_type;
  data->filename = wcsdup(filename);
  data->instrument_number = instrument_number;

  int i;
  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample *sample=(Sample*)&data->samples[i];
    sample->data = data;
  }

  return data;
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int block_size, bool is_loading){

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
  wchar_t *default_sound_filename = STRING_append(OS_get_program_path2(),
                                    STRING_append(STRING_create(OS_get_directory_separator()),
                                    STRING_append(STRING_create("sounds"),
                                    STRING_append(STRING_create(OS_get_directory_separator()),
                                                  !strcmp(plugin_type->name, "Click")
                                                  ? STRING_create("243749__unfa__metronome-1khz-weak-pulse.flac")
                                                  : STRING_create("016.WAV")))));
    
  Data *data = create_data(samplerate,NULL,default_sound_filename,0,RESAMPLER_CUBIC, is_loading); // cubic is the default
  
  if(load_sample(data,default_sound_filename,0, true)==false){
    V_free(data);
    return NULL;
  }

  data->using_default_sound = true;

  return data;
}

static void delete_data(Data *data){
  int i;

  float *prev=NULL;

  for(i=0;i<MAX_NUM_SAMPLES;i++){
    Sample *sample=(Sample*)&data->samples[i];

    if(sample->sound!=NULL && sample->sound != prev){
      prev = sample->sound;
      V_free(sample->sound);
      V_free(sample->min_peaks);
      V_free(sample->max_peaks);
    }
  }

  for(i=0;i<POLYPHONY;i++) {
    RESAMPLER_delete(data->voices[i].resampler);
    ADSR_delete(data->voices[i].adsr);
  }
  
  free((char*)data->filename);

  RSEMAPHORE_delete(data->signal_from_RT);

  free_tremolo(data->tremolo);

  V_free(data);
}

static void set_loop_data(Data *data, int start, int length, bool set_loop_on_off){
  
  if (set_loop_on_off) {
    if (length==0)
      ATOMIC_SET(data->p.loop_onoff, false);
    if (length > 0)
      ATOMIC_SET(data->p.loop_onoff, true);
  }
  
  data->loop_start = start;
  data->loop_length = length;
  
  if (length!=0) {
    
    int i;
    for(i=0;i<MAX_NUM_SAMPLES;i++){
      Sample *sample=(Sample*)&data->samples[i];
      if(sample->sound!=NULL){
        if (start < sample->num_frames)
          sample->loop_start = start;
        else
            sample->loop_start = sample->num_frames - 1;
        
        int loop_end = sample->loop_start + length;
        if (loop_end <= sample->num_frames)
          sample->loop_end = loop_end;
        else
          sample->loop_end = sample->num_frames;
      }
    }
  }
}

void SAMPLER_set_loop_data(struct SoundPlugin *plugin, int start, int length){
  Data *data=plugin->data;

  PLAYER_lock();{  
    set_loop_data(data, start, length, true);
    PLUGIN_set_effect_value(plugin, -1, EFF_LOOP_ONOFF, ATOMIC_GET(data->p.loop_onoff)==true?1.0f:0.0f, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
  }PLAYER_unlock();
}

static bool set_new_sample(struct SoundPlugin *plugin, const wchar_t *filename, int instrument_number, int resampler_type, int loop_start, int loop_end, bool is_loading){
  bool success=false;

  Data *data = NULL;
  Data *old_data = plugin->data;

  filename = OS_loading_get_resolved_file_path(filename);
  if (filename==NULL)
    goto exit;

  data = create_data(old_data->samplerate, old_data, filename, instrument_number, resampler_type, is_loading);

  if(load_sample(data,filename,instrument_number, false)==false)
    goto exit;

  set_loop_data(data, loop_start, loop_end, false);
  
  // Put loop_onoff into storage.
  PLUGIN_set_effect_value2(plugin, -1, EFF_LOOP_ONOFF, ATOMIC_GET(data->p.loop_onoff)==true?1.0f:0.0f, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE, FX_single, PLAYERLOCK_NOT_REQUIRED, PLUGIN_FORMAT_SCALED, false);

  if(SP_is_plugin_running(plugin)){

    //fprintf(stderr, "    *************** 11111. plugin IS running **********\n");
    
    PLAYER_lock();{  
      old_data->new_data = data;
    }PLAYER_unlock();

    if (PLAYER_is_running())
      RSEMAPHORE_wait(old_data->signal_from_RT,1);

  } else {

    //fprintf(stderr, "    *************** 0000. plugin is NOT running **********\n");
    
    plugin->data = data;

  }

  delete_data(old_data);

  update_peaks(plugin);

  volatile struct Patch *patch = plugin->patch;
  if(patch!=NULL)
    GFX_update_instrument_widget((struct Patch*)patch); // Update "loop" button.

  success = true;

 exit:
  if(success==false)
    V_free(data);

  return success;
}


bool SAMPLER_set_new_sample(struct SoundPlugin *plugin, const wchar_t *filename, int instrument_number){
  Data *data=plugin->data;
  return set_new_sample(plugin,filename,instrument_number,data->resampler_type,data->loop_start,data->loop_length, false);
}

bool SAMPLER_set_resampler_type(struct SoundPlugin *plugin, int resampler_type){
  Data *data=plugin->data;
  return set_new_sample(plugin,data->filename,data->instrument_number,resampler_type,data->loop_start,data->loop_length, false);
}

int SAMPLER_get_resampler_type(struct SoundPlugin *plugin){
  Data *data=plugin->data;
  return data->resampler_type;
}

// Has been used for debugging. Not sure if I planned to use it for anything else.
void SAMPLER_save_sample(struct SoundPlugin *plugin, const wchar_t *filename, int sample_number){
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

  SNDFILE *sndfile = sf_open(STRING_get_chars(filename),SFM_WRITE,&sf_info);

  if(sndfile==NULL){
    fprintf(stderr,"could not open file\n");
    return;
  }

  sf_writef_float(sndfile,sample->sound,sample->num_frames);

  sf_close(sndfile);
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  Data *data=(Data*)plugin->data;

  delete_data(data);
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
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
  case EFF_VIBRATO_SPEED:
    return "Vibrato Speed";
  case EFF_VIBRATO_DEPTH:
    return "Vibrato Depth";
  case EFF_TREMOLO_SPEED:
    return "Tremolo Speed";
  case EFF_TREMOLO_DEPTH:
    return "Tremolo Depth";
  case EFF_NOTE_ADJUST:
    return "Note adjustment";
#if 0
  case EFF_OCTAVE_ADJUST:
    return "Octave adjustment";      
#endif
  case EFF_LOOP_ONOFF:
    return "Loop";
  case EFF_CROSSFADE_LENGTH:
    return "Crossfade";
  case EFF_REVERSE:
    return "Reverse";
  case EFF_PINGPONG:
    return "Ping-Pong Loop";
  default:
    RError("S6. Unknown effect number %d\n",effect_num);
    return NULL;
  }
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  if(effect_num==EFF_LOOP_ONOFF || effect_num==EFF_REVERSE || effect_num==EFF_PINGPONG)
    return EFFECT_FORMAT_BOOL;
  else if (effect_num==EFF_CROSSFADE_LENGTH)
    return EFFECT_FORMAT_INT;
  else
    return EFFECT_FORMAT_FLOAT;
}

/*
static QString get_final_embedded_filename(QString org_filename, QString new_filename){
}
*/

static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
  const wchar_t *filename;
  int            instrument_number = HASH_get_int(state, "instrument_number");
  int            resampler_type    = HASH_get_int(state, "resampler_type");
  int            loop_start        = 0; if (HASH_has_key(state, "loop_start"))  loop_start  = HASH_get_int(state, "loop_start");
  int            loop_length       = 0; if (HASH_has_key(state, "loop_length")) loop_length = HASH_get_int(state, "loop_length");

  bool audiodata_is_included = HASH_has_key(state, "audiofile");
  
  if (audiodata_is_included)
    filename = DISK_base64_to_file(NULL, HASH_get_chars(state, "audiofile"));
  else
    filename = HASH_get_string(state, "filename");
  
  if(filename==NULL){
    RError("filename==NULL");
    return;
  }

  if(set_new_sample(plugin,filename,instrument_number,resampler_type,loop_start,loop_length, is_loading)==false)
    GFX_Message(NULL, "Could not load soundfile \"%s\". (instrument number: %d)\n",STRING_get_chars(filename),instrument_number);

  // Can not delete now. file is still used when creating/recreating states. Deleting at program end.
  //if (audiodata_is_included)
  //  DISK_delete_base64_file(filename);
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  const wchar_t *maybe_relative_filename = OS_saving_get_relative_path_if_possible(data->filename);
  //printf("maybe: -%s- -%s-\n", data->filename, maybe_relative_filename);
  
  HASH_put_string(state, "filename", maybe_relative_filename);
  HASH_put_int(state, "instrument_number",data->instrument_number);
  HASH_put_int(state, "resampler_type",data->resampler_type);

  HASH_put_int(state, "loop_start",data->loop_start);
  HASH_put_int(state, "loop_length",data->loop_length);

  if (g_embed_samples){
    const char *audiofile = DISK_file_to_base64(maybe_relative_filename);
    if (audiofile != NULL)
      HASH_put_chars(state, "audiofile", audiofile);
  }
}

const wchar_t *SAMPLER_get_filename(struct SoundPlugin *plugin, bool *is_default_sound){
  Data *data=(Data*)plugin->data;
  *is_default_sound = data->using_default_sound;
  return data->filename;
}

const wchar_t *SAMPLER_get_filename_display(struct SoundPlugin *plugin){
  Data *data=(Data*)plugin->data;
  return data->filename;
}

static SoundPluginType plugin_type = {
 type_name                : "Sample Player",
 name                     : "Sample Player",
 info                     : "Sample Player can load XI intruments, Soundfonts, and all types of sample formats supported by libsndfile. WAV files are looped if they have loops defined in the \"sampl\" chunk, or they have \"Loop Start\" and \"Loop End\" cue id's.\n\nSoundFonts often sound better when played with FluidSynth instead of the Sample Player. The Soundfont handling in Sample Player needs more care. However, the Sample Player uses less memory, are faster to create, has sample-accurate note scheduling, supports pitch changes and polyphonic aftertouch (velocity can be changed while a note is playing), and has configurable options such as attack, decay, sustain, and release.",
 num_inputs               : 0,
 num_outputs              : 2,
 is_instrument            : true,
 note_handling_is_RT      : false,
 num_effects              : EFF_NUM_EFFECTS,
 get_effect_format        : get_effect_format,
 get_effect_name          : get_effect_name,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 RT_process       : RT_process,
 play_note        : play_note,
 set_note_volume  : set_note_volume,
 set_note_pitch   : set_note_pitch,
 stop_note        : stop_note,
 get_peaks        : get_peaks,
 set_effect_value : set_effect_value,
 get_effect_value : get_effect_value,
 get_display_value_string : get_display_value_string,

 recreate_from_state : recreate_from_state,
 create_state        : create_state,

 data                     : NULL
};

static SoundPluginType click_type;

void create_sample_plugin(void){
  PR_add_plugin_type(&plugin_type);

  static bool has_inited = false;

  if (has_inited==false) {
    memcpy((void*)&click_type, (void*)&plugin_type, sizeof(SoundPluginType));

    click_type.name = g_click_name;

    has_inited = true;
  }

  PR_add_plugin_type(&click_type);
}
