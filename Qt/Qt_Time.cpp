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

#include "../common/nsmtracker.h"

//static QTime time234;
static double g_start_time;

extern double monotonic_seconds();

double TIME_get_ms(void){
  return (monotonic_seconds() - g_start_time) * 1000.0;
  //return time234.elapsed();
}

void TIME_init(void){
  g_start_time = monotonic_seconds();
  //time234.start();
}
