


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

static inline double db2linear_double(double db, double y1, double y2){

  if(db<MIN_DB)
    return y2;

  else if(db>6)
    return y1;

  else if (db < -30) {
    double pos_m_30 = scale_double(0.9, 0, 1, y1, y2);
    return scale_double(db, -30, MIN_DB, pos_m_30, y2);

  } else if (db < -20) {
    double pos_m_20 = scale_double(0.8, 0, 1, y1, y2);
    double pos_m_30 = scale_double(0.9, 0, 1, y1, y2);
    return scale_double(db, -20, -30, pos_m_20, pos_m_30);

  } else if (db < -10) {
    double pos_m_10 = scale_double(0.6, 0, 1, y1, y2);
    double pos_m_20 = scale_double(0.8, 0, 1, y1, y2);
    return scale_double(db, -10, -20, pos_m_10, pos_m_20);

  } else if (db < 0) {
    double pos_0  = scale_double(0.3, 0, 1, y1, y2);
    double pos_m_10 = scale_double(0.6, 0, 1, y1, y2);
    return scale_double(db, 0, -10, pos_0, pos_m_10);

  } else {
    double pos_0  = scale_double(0.3, 0, 1, y1, y2);
    double pos_6  = y1;
    return scale_double(db, 6, 0, pos_6, pos_0);
  }
}


/*
  what_to_update:
     -1 => Update all
      0 => First half
      1 => Second half
 */
extern void AUDIOMETERPEAKS_call_very_often(int what_to_update);

extern LANGSPEC AudioMeterPeaks AUDIOMETERPEAKS_create(int num_channels);
extern LANGSPEC void AUDIOMETERPEAKS_delete(AudioMeterPeaks peaks);

#ifdef __cplusplus

#if 0

// Use this one if there are multiple writers, which I'm pretty sure we don't have.
static inline void RT_AUDIOMETERPEAKS_add(AudioMeterPeaks &peaks, int ch, float val){
  for(;;){
    float old_val = atomic_get_float_relaxed(&ATOMIC_NAME(peaks.RT_max_gains)[ch]);
    float max_val = R_MAX(old_val, val);
    if(atomic_compare_and_set_float(&ATOMIC_NAME(peaks.RT_max_gains)[ch], old_val, max_val)==true)
      break;
  }
}

#else

static inline void RT_AUDIOMETERPEAKS_add(AudioMeterPeaks &peaks, int ch, float val){
  float *pos = &ATOMIC_NAME(peaks.RT_max_gains)[ch];
  
  float old_val = atomic_get_float(pos);

  // This is fine.
  if (val <= old_val)
    return;

  atomic_set_float(pos, val);
}

#endif

#endif
