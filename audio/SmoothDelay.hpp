
#ifndef _RADIUM_AUDIO_SMOOTHDELAY_HPP
#define _RADIUM_AUDIO_SMOOTHDELAY_HPP

#if BENCHMARK_SMOOTHDELAY
static double g_benchmark_time = 0.0;
double g_fade_benchmark_time = 0.0;
#endif


#include "../common/LockAsserter.hpp"
#include "Delay.hpp"
#include "Juce_plugins_proc.h"
#include "Fade.hpp"

#ifdef __OPTIMIZE__
#  if !defined(RADIUM_OMIT_FRAME_POINTERS)
#    error "OPTIMIZE + -fno-omit-frame-pointers"
#  endif
#else
#  if defined(RADIUM_OMIT_FRAME_POINTERS)
#    error "-O0 + -fomit-frame-pointers"
#  endif
#endif

  //#define B(A) A
  #define B(A)


namespace radium{


namespace SmoothDelayState{
enum class State{
  NO_DELAY,

  FILL_1, // No delay. Fills delay1 buffer
  FADE_IN_DELAY, // No delay + Fading in delay 1

  PLAIN_DELAY,

  FILL_2, // Plain delay. Fills delay2 buffer
  CROSSFADE, // Fade out delay 1 + Fade in delay 2. When finished, _delay1 and _delay2 swaps.

  FADE_OUT_DELAY // Fade out delay 1 + No delay
};

  static inline const char *get_state_name(State state){
    switch(state){
      case State::NO_DELAY: return "NO_DELAY";
        
      case State::FILL_1: return "FILL1";
      case State::FADE_IN_DELAY: return "FADE_IN_DELAY";
        
      case State::PLAIN_DELAY: return "PLAIN_DELAY";
        
      case State::FILL_2: return "FILL_2";
      case State::CROSSFADE: return "CROSSFADE";
        
      case State::FADE_OUT_DELAY: return "FADE_OUT_DELAY";
    }

    R_ASSERT(false);
    return "error";
  }

}

using namespace SmoothDelayState;

#define SMOOTH_DELAY_FADE_LEN ms_to_frames(100)

 
  // returns new ringbuffer_pos
template <typename T> 
static inline int copy_to_ringbuffer(T *__restrict__ ringbuffer, int ringbuffer_pos, const int size_ringbuffer, const T *__restrict__ input, const int size_input) {
  R_ASSERT_NON_RELEASE(input != ringbuffer);
  R_ASSERT_NON_RELEASE(size_input > 0);
  R_ASSERT_NON_RELEASE(ringbuffer_pos < size_ringbuffer);
  R_ASSERT_NON_RELEASE(size_ringbuffer > 0);
  R_ASSERT_NON_RELEASE(ringbuffer_pos >= 0);

  if (size_input >= size_ringbuffer){
    int input_pos = size_input - size_ringbuffer;
    memcpy(ringbuffer, input + input_pos, sizeof(T)*size_ringbuffer);
    return 0;
  }

  int ringbuffer_left = size_ringbuffer - ringbuffer_pos;
  int dur1 = R_MIN(ringbuffer_left, size_input);
  
  memcpy(ringbuffer + ringbuffer_pos, input, dur1*sizeof(T));

  ringbuffer_pos += size_input;

  if (ringbuffer_pos==size_ringbuffer) {

    return 0;

  } else if (ringbuffer_pos < size_ringbuffer) {

    return ringbuffer_pos;
    
  } else {

    int size = ringbuffer_pos - size_ringbuffer;
    memcpy(ringbuffer, input + dur1, size*sizeof(T));

    return size;
  }
}


template <typename T> 
class SmoothDelayDelay{
  T *_buffer;
  int _max_delay_size;
  int _num_samples_in_buffer;
  int _delay_size;
  int IOTA;

  FadeIn _fade_in;
  FadeOut _fade_out;

#if !defined(RELEASE)
  bool _is_fading_in;
  const State &_state; // Used for assertions.
#endif

public:

  SmoothDelayDelay(const State &state, int max_delay_size)
    : _buffer((T*)V_calloc(sizeof(T), max_delay_size))
    , _max_delay_size(max_delay_size)
    , _num_samples_in_buffer(0)
#if !defined(RELEASE)
    , _state(state)
#endif
  {
    R_ASSERT(max_delay_size > 0);
  }

  ~SmoothDelayDelay(){
    V_free(_buffer);
  }

  int get_max_delay_size(void) const {
    return _max_delay_size;
  }

  void RT_start_filling_buffer(int delay_size) {
    R_ASSERT_NON_RELEASE(_state==State::NO_DELAY || _state==State::PLAIN_DELAY);
    R_ASSERT_NON_RELEASE(delay_size > 0); // Not only shouldn't RT_start_filling_buffer ever be called with a delay size of 0, but SmoothDelayDealy can't handle it either.

    if(delay_size > get_max_delay_size()){
      R_ASSERT_NON_RELEASE(false);
      delay_size = _max_delay_size;
    }

    _delay_size = delay_size;
    IOTA = 0;
    _num_samples_in_buffer = 0;

    B(printf("       start filling: smoothdelay->_delay_size set to %d\n", _delay_size));
  }

  int RT_get_delay_size(void) const {
    return _delay_size;
  }

  void RT_set_delay_size_while_filling(int delay_size){
    R_ASSERT_NON_RELEASE(_state==State::FILL_1 || _state==State::FILL_2);

    return; // Not working. Don't quite understand why, but it's not important.

    if(delay_size > _delay_size){ // Can only change it if incrasing size. If decreasing size, we can't set IOTA to 0 when starting to use the buffer. (things get complicated)
      _delay_size = delay_size;
      B(printf("       while filling: smoothdelay->_delay_size set to %d\n", _delay_size));
    }
  }

  void RT_start_fading_in(void){
    R_ASSERT_NON_RELEASE(_state==State::FILL_1 || _state==State::FILL_2);

    _fade_in.RT_start_fading_in(SMOOTH_DELAY_FADE_LEN);
#if !defined(RELEASE)
    _is_fading_in = true;
#endif
  }

  void RT_start_fading_out(void){
    R_ASSERT_NON_RELEASE(_state==State::FILL_2 || _state==State::PLAIN_DELAY);

    _fade_out.RT_start_fading_out(SMOOTH_DELAY_FADE_LEN);
#if !defined(RELEASE)
    _is_fading_in = false;
#endif
  }

  bool RT_is_finished_filling_buffer(void) const {
    if (_num_samples_in_buffer >= _delay_size){
      //R_ASSERT_NON_RELEASE(_num_samples_in_buffer == _delay_size);
      return true;
    } else {
      return false;
    }
  }

  void RT_process_fill_buffer(int num_frames, const T *__restrict__ input){
    R_ASSERT_NON_RELEASE(_state==State::FILL_1 || _state==State::FILL_2);

    IOTA = copy_to_ringbuffer(_buffer, IOTA, _delay_size, input, num_frames);

    _num_samples_in_buffer += num_frames; //R_MIN(_delay_size, _num_samples_in_buffer + num_frames);
  }


  // overwrites output (plain delay)
  void RT_process_overwrite(const int num_frames, const T *__restrict__ input, T *__restrict__ output) {
    R_ASSERT_NON_RELEASE(input!=output);
    R_ASSERT_NON_RELEASE(RT_is_finished_filling_buffer());

#if BENCHMARK_SMOOTHDELAY
    double time = TIME_get_ms();
#endif

    if (_delay_size >= num_frames)
    {
	    int io_pos = 0;
	    
	    while(true)
	    {
		    bool do_continue = false;
		    
		    int num_to_copy = num_frames - io_pos;
		    const int max_to_copy = _delay_size - IOTA;
		    
		    R_ASSERT_NON_RELEASE(max_to_copy > 0);
		    
		    if (num_to_copy > max_to_copy)
		    {
			    num_to_copy = max_to_copy;
			    do_continue = true;
		    }
		    
		    memcpy(output + io_pos,    _buffer + IOTA,     num_to_copy*sizeof(T));
		    memcpy(_buffer + IOTA,     input + io_pos,     num_to_copy*sizeof(T));
			    
		    IOTA += num_to_copy;
		    
		    if (IOTA >= _delay_size)
		    {
			    R_ASSERT_NON_RELEASE(IOTA==_delay_size);
			    IOTA = 0;
		    }
		    
		    io_pos += num_to_copy;
		    
		    if (!do_continue)
			    break;
	    }
	    
	    R_ASSERT_NON_RELEASE(io_pos == num_frames);
    }
    else
    {
	    for(int i=0;i<num_frames;i++)
	    {
		    output[i] = _buffer[IOTA];
		    _buffer[IOTA] = input[i];
			    
		    // Using the and-trick will probably usually _lower_ performance significantly, and not increase performance.
		    // In large programs, minimizing memory usage (to lower the number of cache misses) seems more important than optimizing inner loops.
		    IOTA++;
		    
		    if(IOTA==_delay_size)
			    IOTA=0;
		    
	    }
    }

    R_ASSERT_NON_RELEASE(IOTA < _delay_size);
    

#if BENCHMARK_SMOOTHDELAY
    g_benchmark_time += TIME_get_ms()-time;
#endif
  }

	
  void RT_process_overwrite_fade_in(int num_frames, const T *__restrict__ input, T *__restrict__ output){
    R_ASSERT_NON_RELEASE(_state==State::FADE_IN_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(_is_fading_in);

    RT_process_overwrite(num_frames, input, output);
    _fade_in.RT_fade(num_frames, output);
  }

	
  void RT_process_overwrite_fade_out(int num_frames, const T *__restrict__ input, T *__restrict__ output){
    R_ASSERT_NON_RELEASE(_state==State::FADE_OUT_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(!_is_fading_in);

    RT_process_overwrite(num_frames, input, output);
    _fade_out.RT_fade(num_frames, output);
  }

	
  void RT_process_add_fade_in(int num_frames, const T *__restrict__ input, T *__restrict__ output){
    R_ASSERT_NON_RELEASE(_state==State::FADE_IN_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(_is_fading_in);

    T *temp_output = RT_ALLOC_ARRAY_STACK(T, num_frames);

    RT_process_overwrite(num_frames, input, temp_output);
    _fade_in.RT_fade(num_frames, temp_output);

    JUCE_add_sound(output, temp_output, num_frames);
  }

	
  void RT_process_add_fade_out(int num_frames, const T *__restrict__ input, T *__restrict__ output){
    R_ASSERT_NON_RELEASE(_state==State::FADE_OUT_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(!_is_fading_in);

    T *temp_output = RT_ALLOC_ARRAY_STACK(T, num_frames);

    RT_process_overwrite(num_frames, input, temp_output);
    _fade_out.RT_fade(num_frames, temp_output);

    JUCE_add_sound(output, temp_output, num_frames);
  }

	
  bool RT_is_finished_fading_in(void) const {
    R_ASSERT_NON_RELEASE(_state==State::FADE_IN_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(_is_fading_in);
    return _fade_in.RT_is_finished_fading();
  }

  bool RT_is_finished_fading_out(void) const {
    R_ASSERT_NON_RELEASE(_state==State::FADE_OUT_DELAY || _state==State::CROSSFADE);
    R_ASSERT_NON_RELEASE(!_is_fading_in);
    return _fade_out.RT_is_finished_fading();
  }
};


class SmoothDelay{

  // It's possible to optimize a little bit by letting _delay1 and _delay2 share the same buffer. (I've tried but it's quite complicated)
  SmoothDelayDelay<float> *_delay1;
  SmoothDelayDelay<float> *_delay2;

  Fade _fade;

  State _state = State::NO_DELAY;

  void swap_delays(void){
    auto *temp = _delay1;
    _delay1 = _delay2;
    _delay2 = temp;
  }


public:
  SmoothDelay(int max_delay_size)
    : _delay1(new SmoothDelayDelay<float>(_state, max_delay_size))
    , _delay2(new SmoothDelayDelay<float>(_state, max_delay_size))
  {}
  
  ~SmoothDelay(){
    delete _delay1;
    delete _delay2;
  }

  int get_max_delay_size(void) const {
    return _delay1->get_max_delay_size();
  }

private:

  bool RT_process2(int num_frames, const float *__restrict__ input, float *__restrict__ output, int delay_size){

  again:

#define SET_STATE(State) do{                                            \
      B(printf("%d: Setting state %s -> %s\n", (int)_time, get_state_name(_state), get_state_name(State))); \
      _state = State;                                                   \
      goto again;                                                       \
    } while(0)
    
    
    switch(_state){
      case State::NO_DELAY:
        if (delay_size > 0){
          _delay1->RT_start_filling_buffer(delay_size);
          SET_STATE(State::FILL_1);
        }

        return false;
          
      case State::FILL_1:
        _delay1->RT_set_delay_size_while_filling(delay_size);

        if (_delay1->RT_is_finished_filling_buffer()==true){
          _delay1->RT_start_fading_in();
          _fade.RT_start_fading_out(SMOOTH_DELAY_FADE_LEN);
          SET_STATE(State::FADE_IN_DELAY);
        }

        _delay1->RT_process_fill_buffer(num_frames, input);
        return false;

      case State::FADE_IN_DELAY:
        R_ASSERT_NON_RELEASE(!_fade.is_fading_in());

        if (_delay1->RT_is_finished_fading_in()){
          R_ASSERT_NON_RELEASE(_fade.RT_is_finished_fading());
          SET_STATE(State::PLAIN_DELAY);
        }

        {
			float *temp = RT_ALLOC_ARRAY_STACK(float, num_frames);
			memcpy(temp, input, sizeof(float)*num_frames);
			
			_fade.RT_fade_out(num_frames, temp);
			
			_delay1->RT_process_overwrite_fade_in(num_frames, input, output);
			
			JUCE_add_sound(output, temp, num_frames);
        }

        return true;

      case State::PLAIN_DELAY:
        if (delay_size != _delay1->RT_get_delay_size()){
          if (delay_size==0){
            _delay1->RT_start_fading_out();
            _fade.RT_start_fading_in(SMOOTH_DELAY_FADE_LEN);
            SET_STATE(State::FADE_OUT_DELAY);
          } else {
            _delay2->RT_start_filling_buffer(delay_size);
            SET_STATE(State::FILL_2);
          }
        }

        _delay1->RT_process_overwrite(num_frames, input, output);
        return true;


      case State::FILL_2:
        _delay2->RT_set_delay_size_while_filling(delay_size);

        if (_delay2->RT_is_finished_filling_buffer()==true){
          _delay1->RT_start_fading_out();
          _delay2->RT_start_fading_in();
          SET_STATE(State::CROSSFADE);
        }        

        _delay2->RT_process_fill_buffer(num_frames, input);
        _delay1->RT_process_overwrite(num_frames, input, output);
        return true;

      case State::CROSSFADE:
        if (_delay1->RT_is_finished_fading_out()){
          R_ASSERT_NON_RELEASE(_delay2->RT_is_finished_fading_in());
          swap_delays();
          SET_STATE(State::PLAIN_DELAY);
        }

        _delay1->RT_process_overwrite_fade_out(num_frames, input, output);
        _delay2->RT_process_add_fade_in(num_frames, input, output);
        return true;

      case State::FADE_OUT_DELAY:
        R_ASSERT_NON_RELEASE(_fade.is_fading_in());

        if (_delay1->RT_is_finished_fading_out()){
          R_ASSERT_NON_RELEASE(_fade.RT_is_finished_fading());
          SET_STATE(State::NO_DELAY);
        }

        {
			float *temp = RT_ALLOC_ARRAY_STACK(float, num_frames);
			memcpy(temp, input, sizeof(float)*num_frames);
			
			_fade.RT_fade_in(num_frames, temp);
			
			_delay1->RT_process_overwrite_fade_out(num_frames, input, output);
			
			JUCE_add_sound(output, temp, num_frames);
        }

        return true;
    }

    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  bool RT_process(int num_frames, const float *input, float *output, int delay_size){
#if !defined(TEST_SMOOTHDELAY)
    R_ASSERT_NON_RELEASE(num_frames <= RADIUM_BLOCKSIZE);
#endif

    _total_num_processed_empty_frames = 0;

    if (_state== State::NO_DELAY && delay_size==0) // Common situation. Avoid using temporary memory if input==output.
      return false;

    if (input!=output)
      return RT_process2(num_frames, input, output, delay_size);

    {
		float *temp = RT_ALLOC_ARRAY_STACK(float, num_frames);
		memcpy(temp, input, sizeof(float)*num_frames);
		return RT_process2(num_frames, temp, output, delay_size);
    }
  }

public:

  int _delay_size = 0;
  int _total_num_processed_empty_frames = 0;

#if defined(RELEASE)
#define CHECK_NOISE 0
#else
#define CHECK_NOISE 0
#endif

#if CHECK_NOISE
  int64_t _time = 0;
  float _prev = 0.0;
#endif

  bool RT_process(int num_samples, const float *input, float *output){

    bool ret = RT_process(num_samples, input, output, _delay_size);

#if CHECK_NOISE
    if(ret){
      for(int i=0;i<num_samples;i++){
        if (fabsf(output[i]-_prev) > 0.2)
          printf("!!!! %d: dx: %d %s: %f -> %f\n", (int)_time, i, get_state_name(_state), _prev, output[i]);
        _prev = output[i];
      }
    }else{
      if (fabsf(input[0]-_prev) > 0.2)
        printf("!!!! %d: dx: %d %s: %f -> %f\n", (int)_time, -1, get_state_name(_state), _prev, input[0]);
      _prev = input[num_samples-1];
    }
    _time += num_samples;
#endif

#undef CHECK_NOISE

    return ret;
  }

  // output_sound can be NULL.
  float *RT_call_instead_of_process_if_no_sound(int num_frames, const float *empty_sound, float *output_sound){

    if (RT_delay_line_is_empty())
      return NULL;

    float *ret = output_sound;

    if (output_sound==NULL){

		float *dev_null = RT_ALLOC_ARRAY_STACK(float, num_frames);
		RT_process(num_frames, empty_sound, dev_null);
      
    }else{

      if (!RT_process(num_frames, empty_sound, output_sound))
        ret = NULL;
    }

    _total_num_processed_empty_frames += num_frames;
    
    return ret;
  }

  bool RT_delay_line_is_empty(void) const {
    if (_state==State::PLAIN_DELAY || _state==State::NO_DELAY)
      return _total_num_processed_empty_frames >= _delay_size;
    else
      return false;
  }

  void setSize(int delay_size){

    if (delay_size != _delay_size){
#if !defined(RELEASE)
#if !defined(TEST_SMOOTHDELAY)
      printf("    Setting delay to %d\n", delay_size);
#endif
#endif

      _total_num_processed_empty_frames = 0;

      if (delay_size > get_max_delay_size()){
        R_ASSERT_NON_RELEASE(false);
        delay_size = get_max_delay_size();
      }

      _delay_size = delay_size;
    }
  }

  int getSize(void) const {
    return _delay_size;
  }

};

}


#if TEST_SMOOTHDELAY

// Run: make test_smoothdelay


#include <assert.h>

#include "../common/Random.hpp"

static PlayerClass player_class;
PlayerClass *pc = &player_class;

using namespace radium;

static void copy_to_ringbuffer_test(void){
  const int input_size = 9;
  const int input[input_size] = {1,2,3,4,5,6,7,8,9};

  const int buffer_size = 5;
  int buffer[buffer_size] = {-1,-1,-1,-1,-1};

  int buffer_pos = 0;

  for(int i=0;i<5;i++){
    buffer_pos = copy_to_ringbuffer(buffer, i, buffer_size, input, input_size);

    assert(buffer_pos==0);

    assert(buffer[0]==5);
    assert(buffer[1]==6);
    assert(buffer[2]==7);
    assert(buffer[3]==8);
    assert(buffer[4]==9);

    for(int i = 0 ; i < 5 ; i++)
      buffer[i] = -1;
  }

  {
    buffer_pos = 0;
    buffer_pos = copy_to_ringbuffer(buffer, buffer_pos, buffer_size, input, 4);

    assert(buffer_pos==4);

    assert(buffer[0]==1);
    assert(buffer[1]==2);
    assert(buffer[2]==3);
    assert(buffer[3]==4);
    assert(buffer[4]==-1);

    for(int i = 0 ; i < 5 ; i++)
      buffer[i] = -1;
  }

  {
    buffer_pos = 1;
    buffer_pos = copy_to_ringbuffer(buffer, buffer_pos, buffer_size, input, 4);

    assert(buffer_pos==0);

    assert(buffer[1]==1);
    assert(buffer[2]==2);
    assert(buffer[3]==3);
    assert(buffer[4]==4);
    assert(buffer[0]==-1);

    for(int i = 0 ; i < 5 ; i++)
      buffer[i] = -1;
  }

  {
    buffer_pos = 4;
    buffer_pos = copy_to_ringbuffer(buffer, buffer_pos, buffer_size, input, 4);

    assert(buffer_pos == 3);

    assert(buffer[4]==1);
    assert(buffer[0]==2);
    assert(buffer[1]==3);
    assert(buffer[2]==4);
    assert(buffer[3]==-1);

    for(int i = 0 ; i < 5 ; i++)
      buffer[i] = -1;
  }
}

static void smoothdelaydelay_test2(int delay_size, int buffer_size){
  State state=State::NO_DELAY;

  SmoothDelayDelay<int> delay(state, 1024);


  delay.RT_start_filling_buffer(delay_size);

  assert(delay.RT_get_delay_size()==delay_size);

  int *input = (int*)alloca(sizeof(int)*(buffer_size+delay_size));
  int *output = (int*)alloca(sizeof(int)*buffer_size);

  for(int i=0;i<buffer_size;i++){
    input[i] = i+1;
    output[i] = -1;
  }
  for(int i=buffer_size;i<delay_size+buffer_size;i++){
    input[i] = i+1;
  }

  state=State::FILL_1;
  delay.RT_process_fill_buffer(delay_size, input);
  
  state=State::PLAIN_DELAY;
  delay.RT_process_overwrite(buffer_size, input+delay_size, output);

  /*
  assert(input[0]==1);
  assert(input[1]==2);
  assert(input[2]==3);
  assert(input[3]==4);
  assert(input[4]==5);

  assert(output[0]==1);
  assert(output[1]==2);
  assert(output[2]==3);
  assert(output[3]==4);
  */
  for(int i=0;i<delay_size;i++)
    assert(input[i]==output[i]);
}


static void smoothdelaydelay_test(void){
  smoothdelaydelay_test2(1, 4);
  smoothdelaydelay_test2(2, 4);
}

static void SMOOTHDELAY_test(void){
  g_audio_block_size = 501;
  
  pc->pfreq = 48000;

  printf("  testing copy_to_ringbuffer\n");
  copy_to_ringbuffer_test();

  printf("  testing smootdelaydelay\n");
  smoothdelaydelay_test();

  printf("  testing smoothdelay: ");

  double phase = 0;
  double phase_add = 0.002;

  const int max_delay_size = 10000;

  radium::Random random;
  //random.set_seed(time(NULL));

  radium::SmoothDelay delay(max_delay_size);
  delay.setSize(1);

#if !BENCHMARK_SMOOTHDELAY
  float prev = 0.0;
#endif

#define M_PI2 (2.0*M_PI)

  double gendata_time = 0;
  double time = TIME_get_ms();

  float *input = (float*)alloca(sizeof(float)*2048);
  float *output = (float*)alloca(sizeof(float)*2048);

  for(int testnum0 = 0 ; testnum0 < 500 ; testnum0++){
#if !BENCHMARK_SMOOTHDELAY
    printf("%d ", testnum0);fflush(stdout);
#endif
    for(int testnum = 0 ; testnum < 10000 ; testnum++){
      
      int num_frames = 64;
      while(random.get_next() > 0.5 && num_frames < 2048)
        num_frames *= 2;
      //printf("num_frames: %d\n", num_frames);

	  //float input[num_frames]; // hmm.....
	  //float output[num_frames];

      double time5 = TIME_get_ms();
      for(int i=0;i<num_frames;i++)
      {
	      input[i] = sinf(phase);
	      phase += phase_add;
	      phase = fmodf(phase, M_PI2);
      }
      gendata_time += TIME_get_ms() - time5;
      
      if(random.get_next() < 0.1)
        delay.setSize(0);
      else if (random.get_next() > 0.85)
        delay.setSize(random.get_next() * max_delay_size);
      
      if (false==delay.RT_process(num_frames, input, output))
        memcpy(output, input, sizeof(float)*num_frames);

#if !BENCHMARK_SMOOTHDELAY
      for(int i=0;i<num_frames;i++){
        assert(fabsf(output[i]-prev) < 0.003);
        prev = output[i];
      }
#endif
    }
  }

  printf("\nTotal duration: %f. (Generating sine: %f.) RT_fade duration: %f. RT_process_overwrite duration: %f\n",
	 (TIME_get_ms()-time-gendata_time) / 1000.0,
	 gendata_time / 1000.0,
#if BENCHMARK_SMOOTHDELAY
         g_fade_benchmark_time/1000.0, g_benchmark_time/1000.0
#else
	 -1.0,-1.0
#endif
         );
  
}
#endif

#undef B


#endif

