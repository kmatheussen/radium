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












/***************************************************************
	This file is supposed to be portable. Therefore, when
	porting, an exec/types.h replacement has to be made.
	And big/small endian conversion has to be done too.

	ULONG is u32, LONG is 32, WORD is 16, UWORD is u16,
	BYTE is 8, and UBYTE is u8. (no surprises there)

	(allso, there is some midi-spesific things, but that
	 has to be done anyway, or something.)
***************************************************************/



#include <exec/types.h>
#include <proplayer.h>

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/clipboard_track_cut_proc.h"
#include "../common/notes_proc.h"
#include "../common/placement_proc.h"
#include "../common/windows_proc.h"
#include "../common/reallines_proc.h"
#include "../common/undo.h"
#include "../Amiga/Amiga_bs_edit_proc.h"
#include "../common/visual_proc.h"
#include "../Amiga/plug-ins/camd_i_plugin.h"
#include "../Amiga/plug-ins/camd_i_plugin_proc.h"
#include "../Amiga/plug-ins/camd_get_clustername_proc.h"
#include "../Amiga/plug-ins/camd_getMidiLink_proc.h"
#include "../Amiga/plug-ins/camd_playfromstart_proc.h"
#include "../Amiga/plug-ins/camd_fx_proc.h"
#include <string.h>
#include "../Amiga/instrprop/Amiga_instrprop_edit_proc.h"
#include "../common/trackreallineelements_proc.h"
#include "../common/blocklist_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/time_proc.h"

struct TrackLine{
	UBYTE note;
	UBYTE instrument;
	UBYTE cmd;
	UBYTE val;
};

struct CmdTrackLine{
	UBYTE cmd;
	UBYTE val;
};


static UBYTE flags2;
static UBYTE tempo2;

extern struct Root *root;

bool MMD_findEndNote(
	NInt track,
	NInt numtracks,
	int line,
	struct TrackLine *trackline,
	int numpages,
	struct CmdTrackLine **cmdpagetable
){
	int lokke;
	struct CmdTrackLine *cmdtrackline;

	if(trackline->cmd==0xf && trackline->val==0xff) return true;

	if(numpages>0){
		for(lokke=0;lokke<numpages;lokke++){
			cmdtrackline=cmdpagetable[lokke];
			cmdtrackline=&cmdtrackline[numtracks*line+track];
			if(cmdtrackline==NULL){
				printf("cmdtrackline==NULL, page: %d, numpages: %d, line: %d, track: %d, numtracks: %d\n",lokke,numpages,line,track,numtracks);
				return false;
			}

			if(cmdtrackline->cmd==0xf && cmdtrackline->val==0xff) return true;
		}
	}
	return false;
}


int MMD_findVelocity(
	NInt track,
	NInt numtracks,
	int line,
	struct TrackLine *trackline,
	int numpages,
	struct CmdTrackLine **cmdpagetable,
	struct Patch *patch
){
	int lokke;
	struct CmdTrackLine *cmdtrackline;

	if(trackline->cmd==0xc) return (int)max(0,min(trackline->val*2-1,127));

	if(numpages>0){
		for(lokke=0;lokke<numpages;lokke++){
			cmdtrackline=cmdpagetable[lokke];
			cmdtrackline=&cmdtrackline[numtracks*line+track];
			if(cmdtrackline->cmd==0xc) return (int)max(0,min(cmdtrackline->val*2-1,127));
		}
	}
	if(patch==NULL){
		return 100;
	}
	return patch->standardvel;
}


void MMD_HandleTempo(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track,
	NInt numtracks,
	int line,
	struct TrackLine *trackline,
	int numpages,
	struct CmdTrackLine **cmdpagetable
){
	int lokke;
	struct CmdTrackLine *cmdtrackline;

	struct Tempos *tempo=NULL;
	struct LPBs *lpb=NULL;

	if(trackline->cmd==0xf && trackline->val>0 && trackline->val<=0xf0){
		tempo=talloc(sizeof(struct Tempos));
		tempo->Tline=line;
		tempo->Tdividor=1;
		tempo->tempo=trackline->val;
		ListAddElement3(&wblock->block->tempos,&tempo->l);
	}else{
		if(trackline->cmd==9){
			lpb=talloc(sizeof(struct LPBs));
			lpb->Tline=line;
			lpb->Tdividor=1;
			lpb->lpb=flags2*trackline->val/6;
			ListAddElement3(&wblock->block->lpbs,&lpb->l);
		}
	}

	if(numpages>0){
		for(lokke=0;lokke<numpages;lokke++){
			cmdtrackline=cmdpagetable[lokke];
			cmdtrackline=&cmdtrackline[numtracks*line+track];
			if(tempo==NULL && cmdtrackline->cmd==0xf && cmdtrackline->val>0 && cmdtrackline->val<=0xf0){
				tempo=talloc(sizeof(struct Tempos));
				tempo->Tline=line;
				tempo->Tdividor=1;
				tempo->tempo=trackline->val;
				ListAddElement3(&wblock->block->tempos,&tempo->l);
			}else{
				if(lpb==NULL && trackline->cmd==9){
					lpb=talloc(sizeof(struct LPBs));
					lpb->Tline=line;
					lpb->Tdividor=1;
					lpb->lpb=flags2*trackline->val/6;
					ListAddElement3(&wblock->block->lpbs,&lpb->l);
				}
			}
		}
	}

}


void LoadOctaBlock(
	struct Tracker_Windows *window,
	NInt blocknum,
	NInt numtracks,
	int numlines,
	int numpages,
	char *blockname,
	struct TrackLine *octablock,
	struct CmdTrackLine **cmdpagetable
){
	struct WBlocks *wblock=ListFindElement1_r0(&window->wblocks->l,blocknum);
	struct TempoNodes *temponode;
	struct WTracks *wtrack;
	struct TrackLine *trackline;
	struct Notes *note;
	struct Stops *stop;
	struct Patch *patch;

	NInt track;
	int line;

	bool free;

	if(wblock==NULL){
		AppendWBlock_spes(window,numlines,numtracks);
		wblock=ListFindElement1(&window->wblocks->l,blocknum);
		if(wblock==NULL){
			RError("Error in function 'LoadOctaBlock' in file mmd2loader/mmd2load.c. wblock==NULL\n");
			return;
		}
//		FreeASpesifiedWBlockTREelement(window,wblock);
//		Block_Set_num_tracks(wblock->block,numtracks);
//		Block_Set_num_lines(wblock->block,numlines);
	}else{
//		FreeASpesifiedWBlockTREelement(window,wblock);
		Block_Set_num_tracks(wblock->block,numtracks);
		Block_Set_num_lines(wblock->block,numlines);
//		SelectWBlock(window,wblock);
//		Block_Properties(wblock->block,numtracks,numlines);
		wtrack=wblock->wtracks;
		while(wtrack!=NULL){
			CB_CutTrack_Force(wblock,wtrack);
			wtrack=NextWTrack(wtrack);
		}
	}

	wblock->block->name=blockname;

	for(track=0;track<numtracks;track++){
		note=NULL;
		patch=NULL;
		free=false;
		wtrack=ListFindElement1(&wblock->wtracks->l,track);
		for(line=0;line<numlines;line++){
			trackline=&octablock[numtracks*line+track];
			trackline->note&=0x7f;
			trackline->instrument&=0x3f;

			MMD_HandleTempo(window,wblock,track,numtracks,line,trackline,numpages,cmdpagetable);

			if(trackline->note!=0 || MMD_findEndNote(track,numtracks,line,trackline,numpages,cmdpagetable)){
				if(note==NULL){
					if(!free && trackline->note==0){
						stop=talloc(sizeof(struct Stops));
						stop->Tline=line;
						stop->Tdividor=1;
						ListAddElement3(&wtrack->track->stops,&stop->l);
					}
				}else{
					note->noend=0;
					note->end.line=line;
					note->end.dividor=1;
					ListAddElement3(&wtrack->track->notes,&note->l);
					note=NULL;
				}
			}

			if(trackline->instrument!=0 && patch==NULL){
				patch=ListFindElement1_r0(&root->song->instruments->patches->l,(NInt)trackline->instrument-1);
			}
			if(trackline->note!=0){
				note=talloc(sizeof(struct Notes));
				note->note=trackline->note-1;
				if(note->note>0){
					note->velocity=note->velocity_end=MMD_findVelocity(
						track,
						numtracks,
						line,
						trackline,
						numpages,
						cmdpagetable,
						patch
					);
					note->Tline=line;
					note->Tdividor=1;
					free=true;
				}
			}
		}
		if(note!=NULL){
			note->noend=1;
			PlaceSetLastPos(wblock->block,&note->end);
			ListAddElement3(&wtrack->track->notes,&note->l);
		}
		wtrack->track->patch=patch;
	}

	/* Lots of cut and paste. Shouln't do things like this, but its so fast. */
	temponode=talloc(sizeof(struct TempoNodes));
	temponode->l.p.dividor=1;
	temponode->reltempo=0.0f;
	wblock->block->temponodes=temponode;

	temponode=talloc(sizeof(struct TempoNodes));			//Doubt its safe to make this one atomic.
	temponode->l.p.line=wblock->block->num_lines-1;
	temponode->l.p.counter=MAX_UINT32-1;
	temponode->l.p.dividor=MAX_UINT32;
	temponode->reltempo=0.0f;
	wblock->block->temponodes->l.next= &temponode->l;
	wblock->block->lasttemponode=temponode;

	UpdateReallinesDependens(window,wblock);
	SelectWBlock(window,wblock);
}


char *MMD_GetInstrumentName(FILE *file,NInt num){
	ULONG expdata;
	ULONG iinfo;
	UWORD i_ext_entries;
	UWORD i_ext_entrsz;

	char temp[400];
	char *name;

	fseek(file,32,SEEK_SET);
	fread(&expdata,4,1,file);
	fseek(file,expdata+20,SEEK_SET);
	fread(&iinfo,4,1,file);
	fread(&i_ext_entries,2,1,file);
	fread(&i_ext_entrsz,2,1,file);

	if(num>=i_ext_entries) return "NN";

	fseek(file,iinfo+(num*i_ext_entrsz),SEEK_SET);
	fread(temp,i_ext_entrsz,1,file);

	if(strlen(temp)<2) return "NN";

	name=talloc_atomic(strlen(temp)+1);
	sprintf(name,"%s",temp);

	return name;
}


void MMD_LoadInstruments(FILE *file,ULONG mmd0song){
	NInt lokke;
	struct Instruments *instrument=root->song->instruments;
	struct Patch *patch;
	struct PatchData *patchdata;
	struct MyMidiLinks *mymidilink=NULL;
	char *clustername=NULL;

	struct MMD0sample *mmd0sample=talloc_atomic(sizeof(struct MMD0sample)*63);

	while(mymidilink==NULL){
		while(clustername==NULL){
			clustername=CAMD_getClusterName(NULL);
		}
		mymidilink=CAMD_getMyMidiLink(clustername);
	}

	fseek(file,mmd0song,SEEK_SET);
	fread(mmd0sample,sizeof(struct MMD0sample)*63,1,file);

	for(lokke=0;lokke<63;lokke++){		//Why 63 and not 64?

		patch=ListFindElement1_r0(&instrument->patches->l,lokke);
		if(patch==NULL){
			patch=talloc(sizeof(struct Patch));
			patch->l.num=lokke;
			ListAddElement1(&instrument->patches,&patch->l);
		}

		patch->name=MMD_GetInstrumentName(file,lokke);
		patch->minvel=0;
		patch->maxvel=127;
		patch->standardvel=max(0,min(mmd0sample->svol*2-1,127));
		patch->playnote=CAMDplaynote;
		patch->stopnote=CAMDstopnote;
		patch->changevelocity=CAMDchangevelocity;
		patch->closePatch=CAMDclosePatch;
		patch->changeTrackPan=CAMDchangeTrackPan;

		if(patch->patchdata==NULL){
			patchdata=talloc(sizeof(struct PatchData));
		}else{
			patchdata=(struct PatchData *)patch->patchdata;
		}

		patchdata->channel=max(0,mmd0sample->midich-1);
		patchdata->preset=mmd0sample->midipreset-1;
		patchdata->MSB=-1;
		patchdata->LSB=-1;

		patchdata->mymidilink=mymidilink;

		patch->patchdata=patchdata;

		CAMDPP_Update_doit(instrument,patch);

		mmd0sample+=1;
	}

	tfree(mmd0sample);

}


/* Uses the first Playseq only. */

void MMD_LoadPlayList(struct Tracker_Windows *window,FILE *file,ULONG mmd2song){
	ULONG playseq;
	UWORD length;
	UWORD blocknum;
	int lokke;
	struct WBlocks *wblock;

	fseek(file,mmd2song+(sizeof(struct MMD0sample)*63)+4,SEEK_SET);
	fread(&playseq,4,1,file);
	printf("play: %x\n",playseq);
	fseek(file,playseq,SEEK_SET);
	fread(&playseq,4,1,file);
	printf("play: %x\n",playseq);
	fseek(file,playseq+40,SEEK_SET);
	fread(&length,2,1,file);
	printf("length: %x\n",length);

	root->song->length=length;
	root->song->playlist=talloc_atomic(sizeof(struct Blocks *)*length);

	for(lokke=0;lokke<length;lokke++){
		fread(&blocknum,2,1,file);
		wblock=ListFindElement1(&window->wblocks->l,(NInt)blocknum);
		root->song->playlist[lokke]=wblock->block;
	}
}



bool LoadMMP2(struct Tracker_Windows *window,char *filename){
	FILE *file;
	NInt lokke;
	int lokke2;

	ULONG ID;
	ULONG mmd0song;
	UWORD numblocks;
	ULONG blockarr;
	ULONG blockpointer;
	ULONG cmdpagepointer;
	UWORD numpages;
	struct CmdTrackLine **cmdpagetable=NULL;
	ULONG pagepointer;
	UWORD numtracks;
	UWORD numlines;
	ULONG blockinfo;
	ULONG blocknamelen;
	char *blockname;
	ULONG blocknamepos;
	UWORD deftempo;

	struct TrackLine *octablock;

	file=fopen(filename,"rb");

	if(file==NULL){
		RError("Could not open file\n");
		return false;
	}

	fread(&ID,4,1,file);
	if(ID!=0x4d4d4432 && ID!=0x4d4d4433){
		RError("This is not an MMD2 or MMD3 octamed module\n");
		return false;
	}

	ResetUndo();

	fseek(file,8,SEEK_SET);
	fread(&mmd0song,4,1,file);

	MMD_LoadInstruments(file,mmd0song);

	fseek(file,mmd0song+(sizeof(struct MMD0sample)*63),SEEK_SET);
	fread(&numblocks,2,1,file);

	fseek(file,mmd0song+764,SEEK_SET);
	fread(&deftempo,2,1,file);
	root->tempo=deftempo;
	fseek(file,mmd0song+768,SEEK_SET);
	fread(&flags2,1,1,file);
	fread(&tempo2,1,1,file);
	flags2&=0x1f;
	flags2+=1;
	root->lpb=flags2*(tempo2/6);

	fseek(file,16,SEEK_SET);
	fread(&blockarr,4,1,file);

	printf("numblocks: %d,blockarr: %x\n",numblocks,blockarr);

	for(lokke=0;lokke<numblocks;lokke++){
		fseek(file,blockarr+(lokke*4),SEEK_SET);
		fread(&blockpointer,4,1,file);
		fseek(file,blockpointer,SEEK_SET);
		fread(&numtracks,2,1,file);
		fread(&numlines,2,1,file);
		numlines++;							//Strange, had this "problem" when making nsm too. Wonder why Teijjo did it this way?
		fread(&blockinfo,4,1,file);
		if(blockinfo>0){
			fseek(file,blockinfo+4,SEEK_SET);
			fread(&blocknamepos,4,1,file);
			fread(&blocknamelen,4,1,file);
			fread(&cmdpagepointer,4,1,file);

			if(blocknamelen>0 && blocknamepos>0){
				blockname=talloc_atomic((size_t)(blocknamelen));
				fseek(file,blocknamepos,SEEK_SET);
				fread(blockname,(size_t)blocknamelen,1,file);
			}else{
				blockname="NN";
			}
		}else{
			cmdpagepointer=0;
			blockname="NN";
		}

		printf("block: %s: blocknamelen: %d, numtracks: %d, numlines %d\n",blockname,blocknamelen,numtracks,numlines);

		fseek(file,blockpointer+8,SEEK_SET);
		octablock=talloc_atomic(numlines*numtracks*sizeof(struct TrackLine));
		fread(octablock,numlines*numtracks*sizeof(struct TrackLine),1,file);

		if(cmdpagepointer>0){
			fseek(file,cmdpagepointer,SEEK_SET);
			fread(&numpages,2,1,file);
			cmdpagetable=talloc(numpages*sizeof(struct CmdTrackLine *));
			for(lokke2=0;lokke2<numpages;lokke2++){
				fseek(file,cmdpagepointer+4+(lokke2*4),SEEK_SET);		
				fread(&pagepointer,4,1,file);
				cmdpagetable[lokke2]=talloc_atomic(sizeof(struct CmdTrackLine)*numtracks*numlines);
				fseek(file,pagepointer,SEEK_SET);
				fread(cmdpagetable[lokke2],sizeof(struct CmdTrackLine)*numtracks*numlines,1,file);
			}
		}else{
			numpages=0;
		}

		LoadOctaBlock(
			window,
			lokke,
			(NInt)numtracks,
			(int)numlines,
			(int)numpages,
			blockname,
			octablock,
			cmdpagetable
		);

		tfree(octablock);
		if(cmdpagepointer>0){
			for(lokke2=0;lokke2<numpages;lokke2++){
				tfree(cmdpagetable[lokke2]);
			}
			tfree(cmdpagetable);
		}
	}

	MMD_LoadPlayList(window,file,mmd0song);

	fclose(file);

	UpdateAllSTimes();

//	DrawUpTrackerWindow(window);
	SelectWBlock(window,window->wblocks);

	ResetUndo();

	root->curr_playlist=0;
	BS_UpdatePlayList();
	BS_UpdateBlockList();

	return true;
}
/*
void main(){
	FILE *file;
	file=fopen("music:medmodules3/radiumtest.mmd2","rb");
	LoadMMP2(NULL,file);
	fclose(file);
}
*/





