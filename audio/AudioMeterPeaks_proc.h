
extern LANGSPEC void AUDIOMETERPEAKS_call_very_often(int ms);

extern LANGSPEC AudioMeterPeaks AUDIOMETERPEAKS_create(int num_channels);
extern LANGSPEC void AUDIOMETERPEAKS_delete(AudioMeterPeaks peaks);

#ifdef __cplusplus
static inline void RT_AUDIOMETERPEAKS_add(const AudioMeterPeaks &peaks, int ch, float val){
  for(;;){
    float old_val = peaks.RT_max_gains[ch];
    float max_val = R_MAX(old_val, val);
    if(atomic_compare_and_set_float(&peaks.RT_max_gains[ch], old_val, max_val)==true)
      break;
  }
}
#endif
