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







/************* overview ********************************

	The player-task sometimes needs to let the main-task
	do some of its work. The Ptask2Mtask function is
	called from the player-task, and
	recieves a pointer to a function that must be called
	from the main-task with 'pointer' as argument.

	The player-task could do all this work by itself, but
	that would slow down and allso make it pretty complicated
	because semaphores or things like that has to be
	added.

	Used when scrolling, change of block, etc.

	NOTE. The main-task doesn't have to be finished processing
	the function before Ptask2Mtask is called again.

*******************************************************/


#include "nsmtracker.h"
#include <exec/types.h>
#include <proto/exec.h>


ULONG ptask2mtasksig;
extern struct Task *mytask;

__inline void Ptask2Mtask(void){
	Signal(mytask,ptask2mtasksig);
}




