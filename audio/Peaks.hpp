/* Copyright 2017 Kjetil S. Matheussen

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



#include "Juce_plugins_proc.h"



#define SAMPLES_PER_PEAK 64 // TODO: This value must be dynamic and placed in the Peaks class. The value must be set higher than 64 if the sample is so long that a 32 bit int becomes too small to use as index. (Need around a 32 day (maybe +32/-16, haven't calculated exactly) long sample (16 days for 96Khz samples) for this to be a problem though, but you never know)



static inline int unit_ceiling(int value, int unit){
  if ( (value % unit) == 0)
    return value;
  else
    return unit * (1 + int(value/unit));
}


static inline int unit_floor(int value, int unit){
  return unit * int(value/unit);
}


  


namespace radium{

struct Peak{

private:
  float min, max;
  
public:
  
  Peak(const float min, const float max)
    : min(min)
    , max(max)
  {    
  }

  Peak(const Peak &other)
    : Peak(other.min, other.max)
  {}
  
  Peak()
    : Peak(FLT_MAX, FLT_MIN)
  {}

  float get_min(void) const {
    R_ASSERT_RETURN_IF_FALSE2(has_data(), 0);
    return min;
  }

  float get_max(void) const {
    R_ASSERT_RETURN_IF_FALSE2(has_data(), 0);
    return max;
  }

  /*
  void set_min(float new_min){
    min = new_min;
  }

  void set_max(float new_max){
    max = new_max;
  }
  */
  
  void scale(float scaleval){
    R_ASSERT_RETURN_IF_FALSE(has_data());
    
    min *= scaleval;
    max *= scaleval;
  }
  
  bool has_data(void) const {
    return min != FLT_MAX;
  }
  
  void merge(const Peak &peak) {
    if (!peak.has_data())
      return;
    else if (!has_data()){
      *this = peak;
    } else {
      min = std::min(peak.min, min);
      max = std::max(peak.max, max);
    }
  }
};

  
#define PEAKS_DIV 4 // I.e. need four peak values to create one peak value in _up.

  
// See bin/scheme/algorithm_doodles.scm for algorithm.
class Peaks{
  
  QVector<Peak> _peaks; // array index is in block pos (i.e. frame pos / SAMPLES_PER_PEAK)

  Peaks *_up = NULL;
  
public:
  
  ~Peaks(){
    delete _up;
  }
  

private:

  Peak _curr_uplevel_peak;
  
  void add(const Peak &peak){    
    int uplevel_pos = _peaks.size() % PEAKS_DIV;
    
    _peaks.push_back(peak);

    if (uplevel_pos == 0) {

      _curr_uplevel_peak = peak;
        
    } else {

      _curr_uplevel_peak.merge(peak);
      
      if (uplevel_pos == PEAKS_DIV-1) {
        
        if (_up==NULL)
          _up = new Peaks();
      
        _up->add(_curr_uplevel_peak);
        
        
      }
      
    }
    
  }

  void merge_in_plain(int start, int end, Peak &peak) const {
    R_ASSERT_RETURN_IF_FALSE(start >= 0);
    R_ASSERT_RETURN_IF_FALSE(end >= start);
    R_ASSERT_RETURN_IF_FALSE(end <= _peaks.size());

    //printf("   merge: Iterating %d\n", end-start);
    for(int pos = start ; pos < end ; pos++)
      peak.merge(_peaks.at(pos));
  }
  
  void merge_in(int start, int end, Peak &peak) const {
    //printf("  merge_in %d -> %d (%d)\n", start, end, end-start);
           
    if (_up==NULL) {
      merge_in_plain(start, end, peak);
      return;
    }

    int next_array_start = unit_ceiling(start, PEAKS_DIV);
    int next_array_end = unit_floor(end, PEAKS_DIV);

    //printf("     next_array: %d -> %d (%d)\n", next_array_start, next_array_end, next_array_end-next_array_start);

    if (next_array_end > next_array_start){
      
      merge_in_plain(start, next_array_start, peak);
      _up->merge_in(next_array_start / PEAKS_DIV, next_array_end / PEAKS_DIV, peak);
      merge_in_plain(next_array_end, end, peak);
      
    } else {
      
      merge_in_plain(start, end, peak);
      
    }
  }
  
private:
  int64_t _creation_time = 0;
  
public:

  enum Add_Samples_Type{
    MORE_IS_COMING_LATER,
    THIS_IS_THE_LAST_CALL
  };
  
  // This function must be called sequentially. E.g.:
  //   create(0, samples, 128)
  //   create(128, samples, 64)
  //   create(192, samples, 256)
  //   etc.
  // Not:
  //   create(128, samples, 64)
  //   create(0, samples, 128)
  //   etc.
  //
  // The 'num_samples' argument can only be non-dividable by SAMPLES_PER_PEAK if add_samples_type==THIS_IS_THE_LAST_CALL.
  void add_samples(float *samples, int num_samples, Add_Samples_Type add_samples_type){
    R_ASSERT_RETURN_IF_FALSE(_creation_time >= 0);
    R_ASSERT_RETURN_IF_FALSE((_creation_time % SAMPLES_PER_PEAK) == 0);
    R_ASSERT_RETURN_IF_FALSE(add_samples_type==THIS_IS_THE_LAST_CALL || (num_samples % SAMPLES_PER_PEAK) == 0);
    
    R_ASSERT_NON_RELEASE(num_samples > 0);

    for(int64_t time = 0 ; time < num_samples ; time += SAMPLES_PER_PEAK){
      int duration = R_MIN(num_samples-time, SAMPLES_PER_PEAK);
      float min,max;
      JUCE_get_min_max_val(&samples[time], duration, &min, &max);
      add(Peak(min, max));
    }

    if (add_samples_type==THIS_IS_THE_LAST_CALL)
      _creation_time = -1;
    else
      _creation_time += num_samples;
  }

  
  const Peak get(int64_t start_time, int64_t end_time) const {
    //printf("Peak.get: %d -> %d (%d)\n", (int)start_time, (int)end_time, (int)(end_time-start_time));

    Peak peak;

    int start = start_time / SAMPLES_PER_PEAK;
    if(start >= _peaks.size())
      return peak;
       
    int end = R_MIN(_peaks.size(), end_time / SAMPLES_PER_PEAK);

    // Asked for so little data that the indexes are the same.
    if (start>=end){
      R_ASSERT_RETURN_IF_FALSE2(start>=0, peak);
      R_ASSERT_RETURN_IF_FALSE2(start==end, peak);
      return _peaks.at(start);
    }
    
    merge_in(start, end, peak);

    return peak;
  }
  
};
  
}
