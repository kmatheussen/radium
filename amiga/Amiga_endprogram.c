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





#include <proto/intuition.h>
#include <proto/diskfont.h>
#include <proto/graphics.h>
#include <proto/input.h>
#include <proto/reqtools.h>

#include "nsmtracker.h"
#include "amiga_player_proc.h"
#include "Amiga_bs.h"
#include "Amiga_bs_edit_proc.h"
#include "instrprop/Amiga_instrprop.h"

#include <proto/exec.h>

#include "../common/OS_endprogram_proc.h"

extern LONG Bptask2mtasksig;
extern LONG Bdebugsig;
extern LONG inputsig;


void OS_EndProgram(void){

	printf("Closing down Amiga spesific things 1/2.\n");

	printf("...player.\n");
	Amiga_endplayer();

	printf("...blockselector window.\n");
	CloseBlockSelectWindow();

	printf("...help window\n");
	if(HelpWnd!=NULL) CloseHelpWindow();

	printf("...camd patch properties window\n");
	if(CPPWindowWnd!=NULL) CloseCPPWindowWindow();

	printf("...player/gui communication signal\n");
	if(Bptask2mtasksig!=-1) FreeSignal(Bptask2mtasksig);

	printf("...debug signal\n");
	if(Bdebugsig!=-1) FreeSignal(Bdebugsig);

	printf("Amiga spesific things 1/2 closed.\n\n");

}

extern struct TextFont *sysfont;
extern bool useworkbench;
extern struct IOStdReq *InputIO;
extern struct MsgPort *keyPort;

void OS_EndProgram2(void){
//	uinit_memory();
	printf("Closing down Amia spesific things 2/2.\n");

	printf("...screen.\n");
	if(Scr!=NULL){
		if(useworkbench){
			UnlockPubScreen( NULL, Scr );
		}else{
			CloseScreen( Scr );
		}
	}

	if(sysfont!=NULL){
		printf("...system font.\n");
		CloseFont(sysfont);
	}
	Scr = NULL;

	printf("...input device 1/2.\n");
	CloseDevice((struct IORequest *)InputIO);

	printf("...input device 2/2.\n");
	DeleteExtIO((struct IORequest *)InputIO);

	printf("...keyport.\n");
	DeletePort(keyPort);

	printf("...reqtools.library.\n");
	CloseLibrary((struct Library *)ReqToolsBase);

	printf("Amiga spesific things 2/2 closed.\n\n");
}

