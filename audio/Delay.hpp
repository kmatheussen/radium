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


#ifndef _RADIUM_AUDIO_DELAY_HPP
#define _RADIUM_AUDIO_DELAY_HPP

static inline int find_next_power2(int i){
  int ret = 2;
  while (ret < i)
    ret *= 2;
  
  return ret;
}

namespace radium{

template <typename T> struct Delay {

private:

  int _buffer_size;  
  int _anding;
  T *_buffer;


  int IOTA = 0;

public:

  Delay(int max_size)
    : _buffer_size(find_next_power2(max_size))
    , _anding(_buffer_size - 1)
    , _buffer((T*)V_calloc(sizeof(T), _buffer_size))
  {
  }

  ~Delay(){
    V_free(_buffer);
    _buffer = NULL;
  }

  void clear(void){
    memset(_buffer, 0, sizeof(float)*_buffer_size);
  }
  
  void write(T value){
    int write_pos = IOTA & _anding;
    //printf("............ write: %d\n", write_pos);
    _buffer[write_pos] = value;
    
    IOTA++;
  }

  T tap(int tap_pos) const {
    int read_pos = (IOTA-tap_pos) & _anding;
    //printf("............ read(%d): %d\n", tap_pos, read_pos);
    return _buffer[read_pos];
  }
};


}

#endif
