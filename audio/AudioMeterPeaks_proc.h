
#define MIN_DB_THRESHOLD -35

static inline float gain2db(float gain){
  if(gain<=0.0f)
    return -100.0f;

  const float threshold_gain  = powf(10,
                                     MIN_DB_THRESHOLD / 20.0f);

  if (gain <= threshold_gain) {

    // We need to do linear conversion below MIN_DB_THRESHOLD here in order to convert back and forth between gain2db and db2gain. (that's probably the only reason)
    
    return scale(gain, 0, threshold_gain, MIN_DB, MIN_DB_THRESHOLD);
    
  } else {
    
    return 20*log10(gain);
    
  }
}

static inline float db2gain(float db){
  if (db <= MIN_DB_THRESHOLD){

    // do linear scale down to zero when db is less than -35 (if not, we won't get 0)
    
    if (db <= MIN_DB)
      return 0.0f;
    
    const float threshold_gain  = powf(10,
                                       MIN_DB_THRESHOLD / 20.0f);
    
    return scale(db, MIN_DB, MIN_DB_THRESHOLD, 0, threshold_gain);
    
  }else{
    
    if (db > MAX_DB)
      db = MAX_DB;
    
    return powf(10, db / 20.0f);
  }
}


static inline float db2linear(float db, float y1, float y2){

  if(db<MIN_DB)
    return y2;

  else if(db>6)
    return y1;

  else if (db < -30) {
    float pos_m_30 = scale(0.9, 0, 1, y1, y2);
    return scale(db, -30, MIN_DB, pos_m_30, y2);

  } else if (db < -20) {
    float pos_m_20 = scale(0.8, 0, 1, y1, y2);
    float pos_m_30 = scale(0.9, 0, 1, y1, y2);
    return scale(db, -20, -30, pos_m_20, pos_m_30);

  } else if (db < -10) {
    float pos_m_10 = scale(0.6, 0, 1, y1, y2);
    float pos_m_20 = scale(0.8, 0, 1, y1, y2);
    return scale(db, -10, -20, pos_m_10, pos_m_20);

  } else if (db < 0) {
    float pos_0  = scale(0.3, 0, 1, y1, y2);
    float pos_m_10 = scale(0.6, 0, 1, y1, y2);
    return scale(db, 0, -10, pos_0, pos_m_10);

  } else {
    float pos_0  = scale(0.3, 0, 1, y1, y2);
    float pos_6  = y1;
    return scale(db, 6, 0, pos_6, pos_0);
  }
}


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
