
#include <vlCore/Time.hpp>


// All time values are in milliseconds
namespace{

struct VBlankEstimator{
  vl::Time time;

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

  VBlankEstimator(int num_trainings)
    : i_trainings(0)
    , num_trainings(num_trainings)
    , base_interval(0)
    , last_base_interval(0)
    , last_diff(500)
  {
    assert(num_trainings>=60);
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
    //time.restart();
    //double elapsed = time.elapsed();
    //i_trainings++;
    //base_interval = elapsed / (double)i_trainings;
    //base_interval = 16.666666666666666666666666666666666666666666666666666666666666667;
    //printf("**************************** Estimated vblank to be %f ms\n",base_interval);
    return Result(base_interval, 1); //16.6666666666666666666666666666666666666666666666666666666667, 1);
  }
};

struct TimeSmoother{
  double get(double time, double approx_correct){
    return time;
  }
};


static const int num_periods_to_adjust = 4;
static  const int num_periods_to_correct = 8;

struct TimeEstimator{
  TimeSmoother smoother;
  VBlankEstimator vblank_estimator;
  double last_value;

  TimeEstimator()
    : vblank_estimator(360)
    , last_value(0.0)
  { }

  bool train(){
    return vblank_estimator.train();
  }

  void reset(){
    last_value = 0.0;
  }

  double get(double approx_correct, double period_multiplier){
    VBlankEstimator::Result vblank=vblank_estimator.get();

    //return approx_correct;

    double ideally = last_value + (vblank.num_periods * vblank.period * period_multiplier);
    double new_value;

    double num_periods_wrong = fabs(approx_correct - ideally);

    if (num_periods_wrong > vblank.period*num_periods_to_correct) {
      new_value = approx_correct;
      printf("NOT RETURNING IDEALLY. Ideally: %f. Returning instead: %f\n",(float)ideally,(float)new_value);

    } else if (num_periods_wrong > vblank.period*num_periods_to_adjust) {
      new_value = smoother.get(ideally, approx_correct);

    } else
      new_value = ideally;


    last_value = new_value;
    return new_value;
  }
};


} // anon.namespace
