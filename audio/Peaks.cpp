/* Copyright 2017-2018 Kjetil S. Matheussen

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


#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"

#include "Peaks.hpp"


namespace radium{
  radium::Mutex g_peak_read_lock; // We don't want to generate peaks for more than one file at the time
}

static QHash<QString,radium::DiskPeaks*> g_diskpeaks;

radium::DiskPeaks *DISKPEAKS_get(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  radium::DiskPeaks *diskpeaks = g_diskpeaks[filename];

  if (diskpeaks==NULL || diskpeaks->has_valid_peaks_on_disk()==false) {
    diskpeaks = new radium::DiskPeaks(wfilename);
    g_diskpeaks[filename] = diskpeaks;
  } 

  diskpeaks->num_visitors++;
  return diskpeaks;
}

void DISKPEAKS_remove(radium::DiskPeaks *diskpeaks){
  diskpeaks->num_visitors--;

  if (diskpeaks->num_visitors==0){
    g_diskpeaks.remove(STRING_get_qstring(diskpeaks->_filename));
    delete diskpeaks;
  }
}


