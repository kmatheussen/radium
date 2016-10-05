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





/************* OVERVIEW *****************************************
  PlayerEventQueue elements doesn't have a "type" field. So
  the only way to find out its type is to check the
  TreatMe function pointer. (ie. there is (at least one) another way, but
  that (those) one(s) is (are) pretty complex and totally unnecesarry when you
  can use this one instead.)
****************************************************************/


#include "nsmtracker.h"
#include "playerclass.h"

#include "PEQ_type_proc.h"

#if 0

extern bool PlayerNewBlock(STime time,struct PEventQueue *peq);
extern bool PE_HandleFirstFX(STime time,struct PEventQueue *peq);
extern bool PE_HandleFX(STime time,struct PEventQueue *peq);
extern bool PE_StartNote(STime time,struct PEventQueue *peq);
extern bool PE_StopNote(STime time,struct PEventQueue *peq);
extern bool PlayerNewRealline(STime time,struct PEventQueue *peq);
extern bool PE_ChangeVelocityFromStart(STime time,struct PEventQueue *peq);
extern bool PE_ChangeVelocity(STime time,struct PEventQueue *peq);
extern bool PE_ChangeVelocityToEnd(STime time,struct PEventQueue *peq);
extern bool PE_ChangeVelocityFromStartToEnd(STime time,struct PEventQueue *peq);

void **PE_functions;

int PEQ_GetType(struct PEventQueue *peq){
	void *function=(void *)peq->TreatMe;
	int lokke=0;

	while(PE_functions[lokke]!=function && lokke<9) lokke++;

	return lokke;
}

#endif

void PEQ_GetType_Init(void){
#if 0
	PE_functions=V_malloc(sizeof(void *)*10);
	PE_functions[0]=(void *)PlayerNewBlock;
	PE_functions[1]=(void *)PE_HandleFirstFX;
	PE_functions[2]=(void *)PE_HandleFX;
	PE_functions[3]=(void *)PE_StartNote;
	PE_functions[4]=(void *)PE_StopNote;
	PE_functions[5]=(void *)PlayerNewRealline;
	PE_functions[6]=(void *)PE_ChangeVelocityFromStart;
	PE_functions[7]=(void *)PE_ChangeVelocity;
	PE_functions[8]=(void *)PE_ChangeVelocityToEnd;
	PE_functions[9]=(void *)PE_ChangeVelocityFromStartToEnd;
#endif
}


