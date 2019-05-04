#ifndef _RADIUM_COMMON_SEQUENCER_TEMPOS_PROC_H
#define _RADIUM_COMMON_SEQUENCER_TEMPOS_PROC_H

extern StaticRatio g_rt_sequencer_signature;
extern double g_rt_sequencer_bpm;
extern double g_rt_sequencer_ppq;
extern double g_rt_sequencer_ppq_of_last_bar_start;
extern int g_rt_beatnum;
extern int g_rt_barnum;

extern LANGSPEC bool RT_SEQUENCER_TIMING_call_before_start_of_audio_block(struct SeqTrack *seqtrack, bool is_playing_song);

extern LANGSPEC double RT_SEQUENCER_TEMPO_get_value(double seqtime);
extern LANGSPEC double RT_SEQUENCER_TEMPO_get_num_quarters(double seqtime);
extern LANGSPEC dyn_t SEQUENCER_TEMPO_get_state(void);

extern LANGSPEC StaticRatio RT_SEQUENCER_SIGNATURE_get_value(double seqtime);
extern LANGSPEC dyn_t SEQUENCER_SIGNATURE_get_state(void);

#ifdef __cplusplus

#include "seqtrack_proc.h"

// Note that calling either of these two functions might change both tempos and signatures (required to ensure tempos are aligned with beats, and signatures are aligned with tempos)
// Use SEQUENCER_TIMING_get_state/SEQUENCER_TIMING_create_from_state instead to avoid this if possible.
void SEQUENCER_TEMPO_create_from_state(const dyn_t &state, double state_samplerate);
void SEQUENCER_SIGNATURE_create_from_state(const dyn_t &state, double state_samplerate);


void SEQUENCER_iterate_sequencer_time(int64_t start_seqtime, int64_t end_seqtime, GridType what_to_find, std::function<bool(int64_t,int,int,int)> callback);
#endif

extern LANGSPEC void SEQUENCER_TIMING_create_from_state(const hash_t *state, double state_samplerate);
extern LANGSPEC hash_t *SEQUENCER_TIMING_get_state(void);

extern LANGSPEC dyn_t SEQUENCER_MARKER_get_state(void);
extern LANGSPEC void SEQUENCER_MARKER_create_from_state(dyn_t markers, double state_samplerate);

extern LANGSPEC void SEQUENCER_TIMING_set_default_values(float default_bpm, StaticRatio default_signature);
extern LANGSPEC void SEQUENCER_TIMING_init(float default_bpm, StaticRatio default_signature);


#endif
