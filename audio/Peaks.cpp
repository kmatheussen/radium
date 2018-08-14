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

#include <QThread>
#include <QHash>
#include <QString>


#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1

#include "../common/nsmtracker.h"

#include "Peaks.hpp"


namespace radium{
  radium::Queue<DiskPeaks*, 1024> g_disk_peaks_queue;
}


namespace{
class DiskPeaksThread : public QThread{
public:
  DiskPeaksThread(){
    start();
  }
private:
  void run(void) override {
    while(true){      
      radium::DiskPeaks *disk_peaks = radium::g_disk_peaks_queue.get();
      if (isInterruptionRequested()==true)
        break;
      if (disk_peaks==NULL)
        R_ASSERT(false);
      else
        disk_peaks->run();
    }
  }
};

static DiskPeaksThread g_disk_peaks_thread;
}

// Note: Is called twice.
void DISKPEAKS_stop(void){
  if (g_disk_peaks_thread.isRunning()){
    g_disk_peaks_thread.requestInterruption();
    radium::g_disk_peaks_queue.put(NULL);
    g_disk_peaks_thread.wait(10000);
  }  
}

static QHash<QString,radium::DiskPeaks*> g_diskpeaks;

radium::DiskPeaks *DISKPEAKS_get(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  radium::DiskPeaks *diskpeaks = g_diskpeaks.value(filename);

  if (diskpeaks==NULL || diskpeaks->has_valid_peaks_on_disk()==false) {
    diskpeaks = new radium::DiskPeaks(wfilename);
    g_diskpeaks[filename] = diskpeaks;
  } 

  ATOMIC_ADD(diskpeaks->num_visitors, 1);
  return diskpeaks;
}

static bool delete_if_empty(radium::DiskPeaks *diskpeaks){
  if (ATOMIC_GET(diskpeaks->num_visitors)==0){
    g_diskpeaks.remove(STRING_get_qstring(diskpeaks->_filename));
    delete diskpeaks;
    return true;
  } else
    return false;
}

void DISKPEAKS_remove(radium::DiskPeaks *diskpeaks){
  R_ASSERT(ATOMIC_ADD(diskpeaks->num_visitors, -1) > 0);
}

void DISKPEAKS_delete_file(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  radium::DiskPeaks *diskpeaks = g_diskpeaks.value(filename);
  
  if (diskpeaks != NULL){
    g_diskpeaks.remove(filename);
    delete diskpeaks;
  }

  DISK_delete_file(get_peak_filename(wfilename));
}
  

void DISKPEAKS_call_very_often(void){
 again:
  for(auto diskpeaks : g_diskpeaks){
    if(delete_if_empty(diskpeaks))
      goto again;
  }
}
