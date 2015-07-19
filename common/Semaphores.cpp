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



#include "nsmtracker.h"

#include "Semaphores.h"

#include "OS_Semaphores.h"

 
struct _RSemaphore : radium::Semaphore{
  _RSemaphore(int n) : radium::Semaphore(n) {}
};


RSemaphore *RSEMAPHORE_create(int num_signallers){
  return new RSemaphore(num_signallers);
}

void RSEMAPHORE_delete(RSemaphore *semaphore){
  RSEMAPHORE_reset(semaphore);
  delete semaphore;
}

void RSEMAPHORE_reset(RSemaphore *semaphore){
  int n = semaphore->numSignallers();
  if (n>0)
    semaphore->wait(n);
}

#if 0
void RSEMAPHORE_set_num_signallers(RSemaphore *semaphore, int num_signallers){
  RSEMAPHORE_reset(semaphore);
  semaphore->release(num_signallers);
}
#endif

int RSEMAPHORE_get_num_signallers(RSemaphore *semaphore){
  return semaphore->numSignallers();
}


#if 0
void RSEMAPHORE_set_num_waiters(RSemaphore *semaphore, int num_waiters){
}

#endif

int RSEMAPHORE_get_num_waiters(RSemaphore *semaphore){
  return semaphore->numWaiters();
}


void RSEMAPHORE_wait(RSemaphore *semaphore, int num_waiters){
  semaphore->wait(num_waiters);
}

#if 0
bool RSEMAPHORE_trywait(RSemaphore *semaphore, int num_waiters){
  return semaphore->tryAcquire(num_waiters);
}

bool RSEMAPHORE_trywait_timeout(RSemaphore *semaphore, int num_waiters, int msecs){
  return semaphore->tryAcquire(num_waiters, msecs);
}
#endif

void RSEMAPHORE_signal(RSemaphore *semaphore, int num_signallers){
  semaphore->signal(num_signallers);
}

