

#ifndef _RADIUM_COMMON_MOVING_AVERAGE_HPP
#define _RADIUM_COMMON_MOVING_AVERAGE_HPP

namespace radium{


struct MovingAverage{
  int _pos = 0;
  const int _i_array_size;
  const double _d_array_size;
  
  double *_array;
  double _sum;

  MovingAverage(int array_size, double value = 0)
    : _i_array_size(array_size)
    , _d_array_size(array_size)
  {
    _array = new double[array_size];
    reset(value);
  }

  ~MovingAverage(){
    delete[] _array;
  }

  void reset(double value){
    _sum = value * _d_array_size;
    for(int i=0;i<_i_array_size;i++)
      _array[i] = value;
  }

  double get(double new_value){
    int last_pos = _pos+1;
    if(last_pos==_i_array_size)
      last_pos = 0;

    _sum -= _array[last_pos];
    _sum += new_value;

    _array[_pos] = new_value;

    _pos = last_pos;

    return _sum / _d_array_size;
  }

};


}

#endif
