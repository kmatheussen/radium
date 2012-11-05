/* Copyright 2012 Kjetil S. Matheussen

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


#include <sndfile.h>


static bool is_saving = false;
static SNDFILE *sndfile;

bool SOUNDFILESAVER_is_saving(void){
  return is_saving;
}

bool SOUNDFILESAVER_write(float **outputs, int num_frames){
  int i;
  float interleaved_data[num_frames*2];

  int pos=0;
  for(i=0;i<num_frames;i++){
    interleaved_data[pos++] = outputs[0][i];
    interleaved_data[pos++] = outputs[1][i];
  }

  if(sf_writef_float(sndfile, interleaved_data, num_frames) != num_frames)
    return false;
  else
    return true;
}

bool SOUNDFILESAVER_open(const char *filename, float samplerate, int libsndfile_format){
  SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

  sf_info.samplerate = samplerate;
  sf_info.channels = 2;
  sf_info.format = libsndfile_format;

  sndfile = sf_open(filename,SFM_READ,&sf_info);
  if(sndfile==NULL)
    return false;

  is_saving = true;
  return true;
}

bool SOUNDFILESAVER_close(void){
  is_saving = false;
  if(sf_close(sndfile)==0)
    return true;
  else
    return false;
}

