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


#include <string.h>

#ifdef __AMIGA__
#include <exec/types.h>
#include "proplayer.h"

#include "../Amiga/Amiga_bs_edit_proc.h"
#include "../Amiga/plug-ins/camd_i_plugin.h"
#include "../Amiga/plug-ins/camd_i_plugin_proc.h"
#include "../Amiga/plug-ins/camd_get_clustername_proc.h"
#include "../Amiga/plug-ins/camd_getMidiLink_proc.h"
#include "../Amiga/plug-ins/camd_playfromstart_proc.h"
#include "../Amiga/plug-ins/camd_fx_proc.h"
#include "../Amiga/instrprop/Amiga_instrprop_edit_proc.h"

#else
#include <stdint.h>
typedef uint32_t ULONG;
typedef uint16_t UWORD;
typedef int16_t WORD;
typedef int8_t BYTE;
typedef uint8_t UBYTE;

#define EXEC_TYPES_H 1
#include "proplayer.h"
#endif

#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../api/api_proc.h"
#include "../common/list_proc.h"
#include "../common/vector_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/clipboard_track_cut_proc.h"
#include "../common/notes_proc.h"
#include "../common/placement_proc.h"
#include "../common/windows_proc.h"
#include "../common/reallines_proc.h"
#include "../common/undo.h"
#include "../common/visual_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/time_proc.h"
#include "../common/Beats_proc.h"
#include "../common/instruments_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/read_binary.h"
#include "../common/OS_disk_proc.h"


#include "mmd2load_proc.h"


typedef uint8_t A_UBYTE;

struct TrackLine{
	A_UBYTE note;
	A_UBYTE instrument;
	A_UBYTE cmd;
	A_UBYTE val;
};

struct CmdTrackLine{
	A_UBYTE cmd;
	A_UBYTE val;
};


static A_UBYTE flags2;
static A_UBYTE tempo2;

extern struct Root *root;

static bool MMD_findEndNote(
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

static int mmd_volume_to_radium_velocity(int vel){
  vel = scale(vel,0,64,0,MAX_VELOCITY);
  return R_BOUNDARIES(0,vel,MAX_VELOCITY);
}

static int MMD_findVelocity(
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

	if(trackline->cmd==0xc) return mmd_volume_to_radium_velocity(trackline->val);

	if(numpages>0){
		for(lokke=0;lokke<numpages;lokke++){
			cmdtrackline=cmdpagetable[lokke];
			cmdtrackline=&cmdtrackline[numtracks*line+track];
			if(cmdtrackline->cmd==0xc) return mmd_volume_to_radium_velocity(trackline->val);
		}
	}
	if(patch==NULL){
		return 100;
	}
	return root->standardvel;
}


static void MMD_HandleTempo(
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
          tempo=(struct Tempos *)talloc(sizeof(struct Tempos));
		tempo->Tline=line;
		tempo->Tdividor=1;
		tempo->tempo=trackline->val;
		ListAddElement3(&wblock->block->tempos,&tempo->l);
	}else{
		if(trackline->cmd==9){
                  lpb=(struct LPBs *)talloc(sizeof(struct LPBs));
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
                          tempo=(struct Tempos *)talloc(sizeof(struct Tempos));
				tempo->Tline=line;
				tempo->Tdividor=1;
				tempo->tempo=trackline->val;
				ListAddElement3(&wblock->block->tempos,&tempo->l);
			}else{
				if(lpb==NULL && trackline->cmd==9){
                                  lpb=(struct LPBs *)talloc(sizeof(struct LPBs));
					lpb->Tline=line;
					lpb->Tdividor=1;
					lpb->lpb=flags2*trackline->val/6;
					ListAddElement3(&wblock->block->lpbs,&lpb->l);
				}
			}
		}
	}

}


static void LoadOctaBlock(
	struct Tracker_Windows *window,
	NInt blocknum,
	NInt numtracks,
	int numlines,
	int numpages,
	char *blockname,
	struct TrackLine *octablock,
	struct CmdTrackLine **cmdpagetable
){
        struct WBlocks *wblock=(struct WBlocks *)ListFindElement1_r0(&window->wblocks->l,blocknum);
	struct TempoNodes *temponode;
	struct WTracks *wtrack;
	struct TrackLine *trackline;
	struct Notes *note;
	//struct Stops *stop;
	struct Patch *patch;

	NInt track;
	int line;

	bool free;

	if(wblock==NULL){
		AppendWBlock_spes(window,numlines,numtracks);
		wblock=(struct WBlocks *)ListFindElement1(&window->wblocks->l,blocknum);
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
                  radium::PlayerPauseOnlyIfNeeded pause_player;
                  bool swings_have_changed = false;
                  CB_ClearTrack_Force(wblock->block, wtrack->track, pause_player, swings_have_changed);
                  wtrack=NextWTrack(wtrack);
		}
	}

	wblock->block->name=blockname;
        g_editor_blocks_generation++;

	for(track=0;track<numtracks;track++){
		note=NULL;
		patch=NULL;
		free=false;
		wtrack=(struct WTracks *)ListFindElement1(&wblock->wtracks->l,track);
		for(line=0;line<numlines;line++){
                        r::TimeData<r::Stop>::Writer writer(wtrack->track->stops2);
                        
			trackline=&octablock[numtracks*line+track];
			trackline->note&=0x7f;
			trackline->instrument&=0x3f;

			MMD_HandleTempo(window,wblock,track,numtracks,line,trackline,numpages,cmdpagetable);

			if(trackline->note!=0 || MMD_findEndNote(track,numtracks,line,trackline,numpages,cmdpagetable)){
				if(note==NULL){
					if(!free && trackline->note==0){
                                          r::Stop stop2(make_ratio(line, 1));
                                          writer.add(stop2);
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
                          //patch=ListFindElement1_r0(&root->song->instruments->patches->l,(NInt)trackline->instrument-1);
                          patch=(struct Patch*)get_MIDI_instrument()->patches.elements[trackline->instrument-1];
			}
			if(trackline->note!=0){
                                note=NewNote();
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
                if (patch!=NULL)
                  patch->has_been_assigned_to_editor_track = true;
		wtrack->track->patch=patch;
	}

	/* Lots of cut and paste. Shouln't do things like this, but its so fast. */
	temponode=(struct TempoNodes*)talloc(sizeof(struct TempoNodes));
	temponode->l.p.dividor=1;
	temponode->reltempo=0.0f;
	wblock->block->temponodes=temponode;

	temponode=(struct TempoNodes*)talloc(sizeof(struct TempoNodes));			//Doubt its safe to make this one atomic.
	temponode->l.p.line=wblock->block->num_lines-1;
	temponode->l.p.counter=MAX_UINT32-1;
	temponode->l.p.dividor=MAX_UINT32;
	temponode->reltempo=0.0f;
	wblock->block->temponodes->l.next= &temponode->l;
	wblock->block->lasttemponode=temponode;

	UpdateReallinesDependens(window,wblock);
	SelectWBlock(window,wblock,true);
}


static const char *MMD_GetInstrumentName(disk_t *file,NInt num){
	ULONG expdata;
	ULONG iinfo;
	UWORD i_ext_entries;
	UWORD i_ext_entrsz;

	char temp[400];
	char *name;

	DISK_set_pos(file,32);
        expdata = read_be32uint(file);

	DISK_set_pos(file,expdata+20);
        iinfo = read_be32uint(file);

        i_ext_entries = read_be16uint(file);
        i_ext_entrsz = read_be16uint(file);
        /*
	fread(&i_ext_entries,2,1,file);
	fread(&i_ext_entrsz,2,1,file);
        */

	if(num>=i_ext_entries) return "NN";

	DISK_set_pos(file,iinfo+(num*i_ext_entrsz));        
	DISK_read_binary(file, temp, i_ext_entrsz);

	if(strlen(temp)<2) return "NN";

	name=(char*)talloc_atomic((int)strlen(temp)+1);
	sprintf(name,"%s",temp);

	return name;
}


static void MMD_LoadInstruments(disk_t *file,ULONG mmd0song){
	NInt lokke;

	struct MMD0sample *mmd0sample=(struct MMD0sample *)talloc_atomic(sizeof(struct MMD0sample)*63);

#if 0
        // FIX
	struct MyMidiLinks *mymidilink=NULL;
	char *clustername=NULL;

	while(mymidilink==NULL){
		while(clustername==NULL){
			clustername=CAMD_getClusterName(NULL);
		}
		mymidilink=CAMD_getMyMidiLink(clustername);
	}
#endif

	DISK_set_pos(file,mmd0song);
	DISK_read_binary(file, mmd0sample,sizeof(struct MMD0sample)*63);


#if 1
	for(lokke=0;lokke<63;lokke++){		//Why 63 and not 64?
          createMIDIInstrument(MMD_GetInstrumentName(file,lokke));
        }
#else
        struct Instruments *instrument=get_MIDI_instrument();

	for(lokke=0;lokke<63;lokke++){		//Why 63 and not 64?
                

          	struct Patch *patch=instrument->patches.num_elements>=lokke ? NULL : instrument->patches.elements[lokke];
		//patch=ListFindElement1_r0(&instrument->patches->l,lokke);
		if(patch==NULL){
                  patch=PATCH_create_midi("name");
		}

		PATCH_set_name(patch, MMD_GetInstrumentName(file,lokke));
                /*
                  // FIX
		patch->minvel=0;
		patch->maxvel=127;
		patch->standardvel=max(0,min(mmd0sample->svol*2-1,127));

		patch->playnote=CAMDplaynote;
		patch->stopnote=CAMDstopnote;
		patch->changevelocity=CAMDchangevelocity;
		patch->closePatch=CAMDclosePatch;
		patch->changeTrackPan=CAMDchangeTrackPan;

                struct PatchData *patchdata;

		if(patch->patchdata==NULL){
			patchdata=talloc(sizeof(struct PatchData));
		}else{
			patchdata=(struct PatchData *)patch->patchdata;
		}

		patchdata->channel=max(0,mmd0sample->midich-1);
		patchdata->preset=mmd0sample->midipreset-1;
		patchdata->MSB=-1;
		patchdata->LSB=-1;

                // FIX!!
		//patchdata->mymidilink=mymidilink;

		patch->patchdata=patchdata;

		CAMDPP_Update_doit(instrument,patch);
                */

		mmd0sample+=1;
	}
#endif

	tfree(mmd0sample);

}


/* Uses the first Playseq only. */

static void MMD_LoadPlayList(struct Tracker_Windows *window,disk_t *file,ULONG mmd2song){
	ULONG playseq;
	UWORD length;
	UWORD blocknum;
	int lokke;

	DISK_set_pos(file,mmd2song+(sizeof(struct MMD0sample)*63)+4);
	//fread(&playseq,4,1,file);
        playseq = read_be32uint(file);

	printf("play: %x\n",playseq);
	DISK_set_pos(file,playseq);
        playseq = read_be32uint(file);
	//fread(&playseq,4,1,file);

	printf("play: %x\n",playseq);
	DISK_set_pos(file,playseq+40);
        length = read_be16uint(file);
	//fread(&length,2,1,file);

	printf("length: %x\n",length);

        int *playlist = (int*)talloc_atomic(sizeof(int) * length);
        
	for(lokke=0;lokke<length;lokke++){
          blocknum = read_be16uint(file);
          playlist[lokke] = blocknum;
	}

        struct SeqTrack *new_seqtrack = SEQTRACK_create_from_playlist(playlist, length);

        while(root->song->seqtracks.num_elements > 1)
          SEQUENCER_delete_seqtrack(0);

        SEQUENCER_replace_seqtrack(new_seqtrack, 0);
}



bool LoadMMP2(struct Tracker_Windows *window, filepath_t filename){
	disk_t *file;
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

	file=DISK_open_binary_for_reading(filename);

	if(file==NULL){
          perror(STRING_get_chars(filename.id));
          GFX_Message(NULL, "Could not open file \"%S\"\n", filename.id);
          return false;
	}

        ID = read_be32uint(file);
	//fread(&ID,4,1,file);
	if(ID!=0x4d4d4432 && ID!=0x4d4d4433){
          GFX_Message(NULL, "This is not an MMD2 or MMD3 octamed module: 0x%x\n",ID);
          return false;
	}

	ResetUndo();

	DISK_set_pos(file,8);
        mmd0song = read_be32uint(file);
	//fread(&mmd0song,4,1,file);

	MMD_LoadInstruments(file,mmd0song);

	DISK_set_pos(file,mmd0song+(sizeof(struct MMD0sample)*63));
        numblocks = read_be16uint(file);
	//fread(&numblocks,2,1,file);

	DISK_set_pos(file,mmd0song+764);
        deftempo = read_be16uint(file);
	//fread(&deftempo,2,1,file);

	root->tempo=deftempo;
	DISK_set_pos(file,mmd0song+768);
	DISK_read_binary(file, &flags2,1);
	DISK_read_binary(file, &tempo2,1);
        printf("flags2 before: %x\n",(uint32_t)flags2);

	flags2&=0x1f;
	flags2+=1;
	//root->lpb=flags2*(tempo2/6);
	root->lpb=flags2*tempo2/6;

        printf("flags2 after: %x, temp2: %d, lpb: %d\n",(uint32_t)flags2,(int)tempo2,root->lpb);

        //char gakk[1024];
        //fgets(gakk,100,stdin);

	DISK_set_pos(file,16);
        blockarr = read_be32uint(file);
	//fread(&blockarr,4,1,file);


	printf("numblocks: %d,blockarr: %x\n",numblocks,blockarr);

	for(lokke=0;lokke<numblocks;lokke++){
		DISK_set_pos(file,blockarr+(lokke*4));
                blockpointer = read_be32uint(file);
		//fread(&blockpointer,4,1,file);

		DISK_set_pos(file,blockpointer);
                numtracks = read_be16uint(file);
		//fread(&numtracks,2,1,file);
                numlines = read_be16uint(file);
		//fread(&numlines,2,1,file);
		numlines++;							//Strange, had this "problem" when making nsm too. Wonder why Teijjo did it this way?

                blockinfo = read_be32uint(file);
		//fread(&blockinfo,4,1,file);
		if(blockinfo>0){
			DISK_set_pos(file,blockinfo+4);
                        blocknamepos = read_be32uint(file);
			//fread(&blocknamepos,4,1,file);

                        blocknamelen = read_be32uint(file);
			//fread(&blocknamelen,4,1,file);

                        cmdpagepointer = read_be32uint(file);
			//fread(&cmdpagepointer,4,1,file);

			if(blocknamelen>0 && blocknamepos>0){
                          blockname=(char*)talloc_atomic(blocknamelen);
                          DISK_set_pos(file,blocknamepos);
                          DISK_read_binary(file, blockname,(size_t)blocknamelen);
			}else{
                          blockname=talloc_strdup("NN");
			}
		}else{
			cmdpagepointer=0;
			blockname=talloc_strdup("NN");
		}

		//printf("block: %s: blocknamelen: %d, numtracks: %d, numlines %d\n",blockname,blocknamelen,numtracks,numlines);

		DISK_set_pos(file,blockpointer+8);
		octablock=(struct TrackLine *)talloc_atomic(numlines*numtracks*sizeof(struct TrackLine));
		DISK_read_binary(file, octablock,numlines*numtracks*sizeof(struct TrackLine));

		if(cmdpagepointer>0){
			DISK_set_pos(file,cmdpagepointer);
                        numpages = read_be16uint(file);
			//fread(&numpages,2,1,file);

			cmdpagetable=(struct CmdTrackLine **)talloc(numpages*sizeof(struct CmdTrackLine *));
			for(lokke2=0;lokke2<numpages;lokke2++){
				DISK_set_pos(file,cmdpagepointer+4+(lokke2*4));		
                                pagepointer = read_be32uint(file);
				//fread(&pagepointer,4,1,file);
				cmdpagetable[lokke2]=(struct CmdTrackLine *)talloc_atomic(sizeof(struct CmdTrackLine)*numtracks*numlines);
				DISK_set_pos(file,pagepointer);
				DISK_read_binary(file, cmdpagetable[lokke2],sizeof(struct CmdTrackLine)*numtracks*numlines);
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

	if (DISK_close_and_delete(file)==false)
          exit(0);

        TIME_everything_has_changed();

//	DrawUpTrackerWindow(window);
	SelectWBlock(window,window->wblocks,true);

	ResetUndo();

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





