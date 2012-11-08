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



#include <QSemaphore>

#include "../common/nsmtracker.h"


struct _RSemaphore : QSemaphore{
  _RSemaphore(int n) : QSemaphore(n) {}
};

RSemaphore *RSEMAPHORE_create(int num_signallers){
  return new RSemaphore(num_signallers);
}

void RSEMAPHORE_delete(RSemaphore *semaphore){
  delete semaphore;
}

void RSEMAPHORE_reset(RSemaphore *semaphore){
  int n = semaphore->available();
  if(n<0)
    semaphore->release(-n);
  else
    semaphore->acquire(n);
}

#if 0
void RSEMAPHORE_set_num_waiters(RSemaphore *semaphore, int num_waiters){
}

void RSEMAPHORE_get_num_waiters(RSemaphore *semaphore){
}

void RSEMAPHORE_set_num_signallers(RSemaphore *semaphore, int num_signallers){
}

void RSEMAPHORE_get_num_signallers(RSemaphore *semaphore){
}
#endif

void RSEMAPHORE_wait(RSemaphore *semaphore, int num_waiters){
  semaphore->acquire(num_waiters);
}

void RSEMAPHORE_signal(RSemaphore *semaphore, int num_signallers){
  semaphore->release(num_signallers);
}

