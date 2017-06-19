/* Copyright 2000 Kjetil S. Matheussen

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


struct _RSemaphore;
typedef struct _RSemaphore RSemaphore;

// waiters/signallers??? It's not complicated:
// num_signallers == -num_waiters
// num_waiters == -num_signallers

extern LANGSPEC RSemaphore *RSEMAPHORE_create(int num_signallers);
extern LANGSPEC void RSEMAPHORE_delete(RSemaphore *semaphore);

extern LANGSPEC void RSEMAPHORE_reset(RSemaphore *semaphore); // Must not be called if someone is waiting.

extern LANGSPEC void RSEMAPHORE_set_num_waiters(RSemaphore *semaphore, int num_waiters);
extern LANGSPEC int RSEMAPHORE_get_num_waiters(RSemaphore *semaphore);

extern LANGSPEC void RSEMAPHORE_set_num_signallers(RSemaphore *semaphore, int num_signallers);
extern LANGSPEC int RSEMAPHORE_get_num_signallers(RSemaphore *semaphore);

extern LANGSPEC void RSEMAPHORE_wait(RSemaphore *semaphore, int num_waiters);
extern LANGSPEC bool RSEMAPHORE_trywait(RSemaphore *semaphore, int num_waiters);
extern LANGSPEC bool RSEMAPHORE_trywait_timeout(RSemaphore *semaphore, int num_waiters, int msecs);

extern LANGSPEC void RSEMAPHORE_signal(RSemaphore *semaphore, int num_signallers);
extern LANGSPEC void RSEMAPHORE_signal_all(RSemaphore *semaphore);
