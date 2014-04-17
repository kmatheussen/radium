
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
    assert(num_trainings>=60);
  }

  void set_vblank(double period){
    base_interval = period;
    time.start();
    printf("\n\n\n\n ************* Setting vblank to %f ************\n\n\n",period);
  }

  bool train(){
    if(i_trainings==30)
      time.start();

    //printf("__________________ i_tr: %d, num: %d (%d)\n",i_trainings,num_trainings,(int)time.elapsed());
    i_trainings++;

    if (i_trainings < num_trainings)
      return true;

    else {
      double elapsed = time.elapsed() * 1000.0;
      base_interval = elapsed / (double)(i_trainings-30);
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

  Result get(){
    double time_now = time.elapsed();
    double interval = (time_now - last_time)*1000.0;
    int num_periods = (interval+(base_interval/2.0)) / base_interval;
    last_time = time_now;
    //printf("interval: %f, num_periods: %d, base_interval: %f\n",interval,num_periods,base_interval);
    return Result(base_interval, num_periods); //16.6666666666666666666666666666666666666666666666666666666667, 1);
  }
};


static const double smallest_diff = 0.0001;

struct Smoother{
  double smoothness;
  double last_val;

  Smoother(double smoothness)
    : smoothness(smoothness)
    , last_val(0)
  { }

  void reset(){
    last_val=0.0;
  }

  double get(double to){
    float ret = last_val;
    if(ret == to)
      return to;
    else if(fabs(to-last_val) < smallest_diff)
      last_val = to;
    else
      last_val = (float) (to*(1.0-smoothness) + last_val*smoothness);
    return ret;
  }
};


static const int num_periods_to_adjust = 3;
static const int num_periods_to_adjust_max = 8;
static const int num_periods_to_correct = 20;

// static int counter=0;

#define DEBUG_PRINT 0

struct TimeEstimator{
  Smoother smoother;
  VBlankEstimator vblank_estimator;
  double last_value;

  bool is_adjusting;
  bool adjusting_down;
  bool adjusting_up;
  double adjustment;

  TimeEstimator()
    : smoother(0.9)
    , vblank_estimator(360)
    , last_value(0.0)
    , is_adjusting(false)
    , adjusting_up(false)
    , adjustment(0.0)
  { }

  bool train(){
    return vblank_estimator.train();
  }

  void reset(){
    last_value = 0.0;
  }

  void set_vblank(double period){
    vblank_estimator.set_vblank(period);
  }

  double get_vblank(){
    return vblank_estimator.base_interval;
  }

  double get(double approx_correct, double period_multiplier){
    VBlankEstimator::Result vblank=vblank_estimator.get();

    //return approx_correct;

    double ideally = last_value + (vblank.num_periods * vblank.period * period_multiplier);

    double wrong = ideally - approx_correct;
    double num_periods_wrong = wrong / vblank.period;
    double a_num_periods_wrong = fabs(num_periods_wrong);

    if (a_num_periods_wrong > num_periods_to_correct) {
      double new_value = approx_correct;
      last_value = new_value;
      adjustment = 0.0;
      is_adjusting = false;
      smoother.reset();
      printf("NOT RETURNING IDEALLY. Ideally (calculated): %f. Returning instead (approx correct): %f\n",(float)ideally,(float)new_value);

      return new_value;

    } else if (a_num_periods_wrong > num_periods_to_adjust) {      
      // try to adjust.
      adjustment = scale_double(a_num_periods_wrong,
                                num_periods_to_adjust,num_periods_to_adjust_max,
                                0,0.3);
      adjustment = -wrong*adjustment;
      
      if (adjustment < 0) {
        adjusting_down = true;
        adjusting_up = false;
      } else {
        adjusting_down = false;
        adjusting_up = true;
      }

      if(DEBUG_PRINT) printf("adjusting   %f: %f",num_periods_wrong,adjustment / vblank.period);

      is_adjusting = true;

    } else if (is_adjusting) {

      if(DEBUG_PRINT) printf("--adjusting %f: %f",num_periods_wrong,adjustment / vblank.period);
        
      if (adjusting_up && wrong>0) {
        adjustment = 0.0;
        is_adjusting = false;
        if(DEBUG_PRINT) printf("---got it");

      } else if(adjusting_down && wrong<0) {
        adjustment = 0.0;
        is_adjusting = false;
        if(DEBUG_PRINT) printf("---got it");

      }

    } else
      adjustment = 0.0;

    double smoothed_adjustment = smoother.get(adjustment);
    if(DEBUG_PRINT) printf(". smoothed_adjustment: %f\n",smoothed_adjustment);

    double new_value = ideally + smoothed_adjustment;
    last_value = new_value;

    return new_value;
  }
};


} // anon.namespace
