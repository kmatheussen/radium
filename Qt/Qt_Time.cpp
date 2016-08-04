/* Copyright 2014 Kjetil S. Matheussen

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

//#include <QTime>

#include <errno.h>
#include <sys/time.h>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

//static QTime time234;
static double g_start_time;

//extern double monotonic_seconds();

static double get_ms(void){
  struct timeval now;

  int err = gettimeofday(&now, NULL);
  if (err != 0){
#if defined(RELEASE)
    static int counter = 0;
    counter++;
    if (counter==100)
#endif
      GFX_Message(NULL, "Timing failed: %s. Please report this error.",strerror(errno));
    return g_start_time + 10000;
  }

  return (double)now.tv_sec*1000.0 + (double)now.tv_usec/1000.0;
}

double TIME_get_ms(void){
  return (get_ms() - g_start_time);
}

//return (monotonic_seconds() - g_start_time) * 1000.0;
  //return time234.elapsed();
//}

void TIME_init(void){
  g_start_time = get_ms();//monotonic_seconds();
  //time234.start();
}
