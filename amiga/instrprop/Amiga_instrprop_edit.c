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










#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <proto/gadtools.h>

#include "../nsmtracker.h"
#include "../../common/list_proc.h"
#include "../plug-ins/camd_i_plugin.h"

#include "Amiga_instrprop.h"

#include "Amiga_instrprop_edit_proc.h"

extern struct Root *root;

struct Patch *currpatch=NULL;


const int G2CC[8]={GD_Cr,GD_Re,GD_At,GD_Rl,GD_Cu,GD_Rs,GD_Va,GD_Mo};
const int GB2CC[8]={GD_B1,GD_B2,GD_B3,GD_B4,GD_B5,GD_B6,GD_B7,GD_B8};


void CAMDPP_Update(struct Instruments *instrument,struct Patch *patch){
	if(currpatch==patch) return;
	CAMDPP_Update_doit(instrument,patch);
}

void CAMDPP_Update_doit(struct Instruments *instrument,struct Patch *patch){
	NInt patchnum;
	NInt num_patches;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink;
	struct ChannelSpesific *cs;
	int lokke;

	if(patch==NULL){
		patchnum=-1;
	}else{
		patchnum=patch->l.num;
	}

	currpatch=patch;

	if(currpatch==NULL){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_name],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			GTST_String,"No Patch",
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Panonoff],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Volumeonoff],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Number],
			CPPWindowWnd,
			NULL,
			GTSL_Level,0,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Cluster],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);

		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Channel],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
	
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_LSB],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
	
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_MSB],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);		
	
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Preset],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
	
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Volume],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
	
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Velocity],
			CPPWindowWnd,
			NULL,
			GTSL_Level,root->standardvel,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GD_Pan],
			CPPWindowWnd,
			NULL,
			GA_Disabled,TRUE,
			TAG_END
		);
	
		for(lokke=0;lokke<8;lokke++){
			GT_SetGadgetAttrs(
				CPPWindowGadgets[GB2CC[lokke]],
				CPPWindowWnd,
				NULL,
				GA_Disabled,TRUE,
				TAG_END
			);
			GT_SetGadgetAttrs(
				CPPWindowGadgets[G2CC[lokke]],
				CPPWindowWnd,
				NULL,
				GA_Disabled,TRUE,
				TAG_END
			);
		}
		return;
	}

	num_patches=ListFindNumElements1(&instrument->patches->l);
	patchdata=(struct PatchData *)patch->patchdata;
	mymidilink=patchdata->mymidilink;
	cs=&mymidilink->channelspesific[patchdata->channel];

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_name],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTST_String,patch->name,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Number],
		CPPWindowWnd,
		NULL,
		GTSL_Min,0,
		GTSL_Max,num_patches,
		GTSL_Level,patchnum+1,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Cluster],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTST_String,mymidilink->name,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Channel],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTSL_Level,patchdata->channel+1,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_LSB],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->LSB,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_MSB],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->MSB,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Preset],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTIN_Number,patchdata->preset+1,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Velocity],
		CPPWindowWnd,
		NULL,
		GTSL_Level,patch->standardvel,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Volumeonoff],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTCB_Checked,cs->volumeonoff?TRUE:FALSE,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Volume],
		CPPWindowWnd,
		NULL,
		GA_Disabled,cs->volumeonoff?FALSE:TRUE,
		GTSL_Level,cs->volume,
		TAG_END
	);

	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Pan],
		CPPWindowWnd,
		NULL,
		GA_Immediate,TRUE,
		GA_Disabled,cs->panonoff?FALSE:TRUE,
		GTSL_Level,cs->pan,
		TAG_END
	);
	GT_SetGadgetAttrs(
		CPPWindowGadgets[GD_Panonoff],
		CPPWindowWnd,
		NULL,
		GA_Disabled,FALSE,
		GTCB_Checked,cs->panonoff?TRUE:FALSE,
		TAG_END
	);

	for(lokke=0;lokke<8;lokke++){
		GT_SetGadgetAttrs(
			CPPWindowGadgets[GB2CC[lokke]],
			CPPWindowWnd,
			NULL,
			GA_Disabled,FALSE,
			TAG_END
		);
		GT_SetGadgetAttrs(
			CPPWindowGadgets[G2CC[lokke]],
			CPPWindowWnd,
			NULL,
			GA_Disabled,mymidilink->standardccs[lokke]==-1?TRUE:cs->ccsonoff[lokke]?FALSE:TRUE,		//Nice. :)
			GTSL_Level,cs->ccvalues[lokke],
			TAG_END
		);
	}

}










