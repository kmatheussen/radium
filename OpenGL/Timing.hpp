
#include <vlCore/Time.hpp>

// All time values are in milliseconds
namespace{

struct VBlankEstimator{
  vl::Time time;

  double last_time;

  int i_trainings;
  int num_trainings;
  double base_interval;
  double last_base_interval;
  double last_diff;

  struct Result{
    double period;
    int num_periods;
    Result(double period, int num_periods)
      : period(period)
      , num_periods(num_periods)
    {}
  };

  VBlankEstimator(int num_trainings, double base_interval = 0.0)
    : last_time(0)
    , i_trainings(0)
    , num_trainings(num_trainings)
    , base_interval(base_interval)
    , last_base_interval(0)
    , last_diff(500)
  {
    R_ASSERT(num_trainings>=60);
  }
  
  void set_vblank(double period){
    if (period != base_interval){
      base_interval = period;
      time.start();
      printf("\n\n\n\n ************* Setting vblank to %f ************\n\n\n",period);
    }
  }

  bool train(){
    if(i_trainings==60)
      time.start();

    //printf("__________________ i_tr: %d, num: %d (%d)\n",i_trainings,num_trainings,(int)time.elapsed());
    i_trainings++;

    if (i_trainings < num_trainings)
      return true;

    else {
      double elapsed = time.elapsed() * 1000.0;
      base_interval = elapsed / (double)(i_trainings-60);
      double diff = fabs(last_base_interval-base_interval);
      last_base_interval = base_interval;

      if(i_trainings>600 || (diff<0.0001 && last_diff<0.0001)) {
        printf("**************************** Time: %f, num: %d. Estimated vblank to be %f ms (change: %f)\n",elapsed,num_trainings,base_interval,diff);
        return false;

      }else{
        last_diff = diff;
        return true;
      }
    }
  }

  const Result get(void) {
    double time_now = time.elapsed();
    double interval = (time_now - last_time)*1000.0;
    int num_periods = (interval+(base_interval/2.0)) / base_interval;
    last_time = time_now;
#if 0 //!defined(RELEASE)
    if (num_periods != 1)
      printf("interval: %f, num_periods: %d, base_interval: %f\n",interval,num_periods,base_interval);
#endif
    return Result(base_interval, num_periods); //16.6666666666666666666666666666666666666666666666666666666667, 1);
  }
};


static const double smallest_diff = 0.0001;

// one-pole filter (low-pass filter). Logic copied from code written for Faust by Julius O. Smith.
struct Smoother{
  double _smoothness;
  double _last_val;

  Smoother(double smoothness)
    : _smoothness(smoothness)
    , _last_val(0)
  { }

  void reset(){
    _last_val=0.0;
  }

  double get(double to){
    float ret = _last_val;
    if(ret == to)
      return to;
    else if(fabs(to-_last_val) < smallest_diff)
      _last_val = to;
    else
      _last_val = (float) (to*(1.0-_smoothness) + _last_val*_smoothness);
    return ret;
  }
};


static const double k_num_periods_to_adjust = 3;
static const double k_num_periods_to_adjust_max = 8;
static const double k_num_periods_to_correct = 20;


// static int counter=0;

#define DEBUG_PRINT 0

struct TimeEstimator{
  Smoother _smoother;
  VBlankEstimator _vblank_estimator;
  double _last_value;

  bool _is_adjusting;
  bool _adjusting_down;
  bool _adjusting_up;
  double _adjustment;

  TimeEstimator()
    : _smoother(0.9)
    , _vblank_estimator(360)
    , _last_value(0.0)
    , _is_adjusting(false)
    , _adjusting_up(false)
    , _adjustment(0.0)
  { }

  bool train(){
    return _vblank_estimator.train();
  }

  void reset(){
    _last_value = 0.0;
  }

  void set_vblank(double period){
    _vblank_estimator.set_vblank(period);
  }

  double get_vblank(){
    if (_vblank_estimator.base_interval<=0.1)
      return 1000.0 / 60.0; // 60hz
    else
      return _vblank_estimator.base_interval;
  }

  void set_time(double correct){
    _last_value = correct;
    _adjustment = 0.0;
    _is_adjusting = false;
    _adjusting_up = false;
    _smoother.reset();
  }
  
  double get(double approx_correct, double period_multiplier){
    const VBlankEstimator::Result vblank=_vblank_estimator.get();

    //return approx_correct;

    double new_value = _last_value + (vblank.num_periods * vblank.period * period_multiplier);

    double wrong = new_value - approx_correct;
    double num_periods_wrong = wrong / vblank.period;
    double a_num_periods_wrong = fabs(num_periods_wrong);

    if (a_num_periods_wrong > (period_multiplier*k_num_periods_to_correct)) {
      #if !defined(RELEASE)
      printf("NOT RETURNING NEW_VALUE. New_Value (calculated): %f. Returning instead (approx correct): %f\n",(float)new_value,(float)approx_correct);
      #endif
      
      set_time(approx_correct);
      return approx_correct;

    } else if (a_num_periods_wrong > (period_multiplier*k_num_periods_to_adjust)) {      
      // try to adjust.
      _adjustment = scale_double(a_num_periods_wrong,
                                period_multiplier*k_num_periods_to_adjust, period_multiplier*k_num_periods_to_adjust_max,
                                0,0.3);
      _adjustment = -wrong*_adjustment;
      
      _is_adjusting = true;

      if (_adjustment < 0) {
        _adjusting_down = true;
        _adjusting_up = false;
      } else {
        _adjusting_down = false;
        _adjusting_up = true;
      }

      if(DEBUG_PRINT) printf("adjusting   %f: %f",num_periods_wrong,_adjustment / vblank.period);

    } else if (_is_adjusting) {

      if(DEBUG_PRINT) printf("--adjusting %f: %f",num_periods_wrong,_adjustment / vblank.period);
        
      if (_adjusting_up && wrong>0) {
        _adjustment = 0.0;
        _is_adjusting = false;
        if(DEBUG_PRINT) printf("---got it");

      } else if(_adjusting_down && wrong<0) {
        _adjustment = 0.0;
        _is_adjusting = false;
        if(DEBUG_PRINT) printf("---got it");

      }

    } else
      _adjustment = 0.0;

    double smoothed_adjustment = _smoother.get(_adjustment);
    if(DEBUG_PRINT) printf(". smoothed_adjustment: %f\n",smoothed_adjustment);

    new_value += smoothed_adjustment;
    _last_value = new_value;

    return new_value;
  }
};


} // anon.namespace
