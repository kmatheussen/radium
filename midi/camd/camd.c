/* Copyright 2003 Kjetil S. Matheussen

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




/***** Following code ripped from playmf.c by Dan Baker/Commodore *****/

/* Compiler glue: stub functions for midi.library */
struct MidiNode *CreateMidi(Tag tag, ...)
{	return CreateMidiA((struct TagItem *)&tag );
}

/*
BOOL SetMidiAttrs(struct MidiNode *mi, Tag tag, ...)
{	return SetMidiAttrsA(mi, (struct TagItem *)&tag );
}
*/

struct MidiLink *AddMidiLink(struct MidiNode *mi, LONG type, Tag tag, ...)
{	return AddMidiLinkA(mi, type, (struct TagItem *)&tag );
}


/***** End of code ripped from playmf.c by Dan Baker/Commodore *****/


struct MidiNode *midinode=NULL;

extern ULONG waitforamigatimersig;



void MyPutMidi(
	struct MyMidiLinks *mymidilink,
//	uint32_t msg,
	int cc,
	int data1,
	int data2,
	int maxbuff,
	int skip
){
	APTR driverdata;

	struct MidiLink *midilink=mymidilink->midilink;


	if(cc<0x80 || cc>0xef || (data1&0xff)>0x7f || (data2&0xff)>0x7f){
		RError("Error. Faulty midi-message. status: %x, data1: %x, data2: %x\n",cc,data1,data2);
		return;
	}

	driverdata=GoodPutMidi(midilink,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff);

	if(driverdata!=NULL){
		while(Midi2Driver(driverdata,(uint32_t)((cc<<24)|(data1<<16)|(data2<<8)),(uint32_t)maxbuff)==FALSE){
			if(skip==1) break;
//			Pdebug("Waiting so hard\n");
			if(!pc->isplaying){
				WaitTOF();
			}else{
				Wait(waitforamigatimersig);
			}
		}
	}

	OnOffNotesTrack(mymidilink,cc,data1,data2);

}

void MIDI_Delete(void){

	if(midinode!=NULL) DeleteMidi(midinode);

#ifndef USEMINIMIDI
	CloseLibrary(CamdBase);
#else
	UninitMiniCamd();
#endif

	if(inputsig!=-1) FreeSignal(inputsig);

}


bool MIDI_New(struct Instruments *instrument){

	FILE *file;
	char temp[100];

#ifdef USEMINIMIDI
	if(InitMiniCamd()==FALSE){
		fprintf(stderr,"Can't open midi.library\n");
		return INSTRUMENT_FAILED;
	}
#endif

	inputsig=AllocSignal(-1);

	midinode=CreateMidi(
		MIDI_Name,"radium",
		MIDI_MsgQueue, 5000L,
		MIDI_SignalTask,FindTask(0),
		MIDI_RecvSignal,inputsig,
		NULL
	);

	if(midinode==NULL){
		fprintf(stderr,"Can't get midinode from MIDI\n");
#ifndef USEMINIMIDI
		CloseLibrary(CamdBase);
#else
		UninitMiniCamd();
#endif
		return INSTRUMENT_FAILED;
	}

	inlinkname=talloc_atomic(100);

	file=fopen("radium:inlinkname.txt","r");
	if(file==NULL){
		sprintf(inlinkname,"%s","mmp.in.0");
		file=fopen("radium:inlinkname.txt","w");
		fprintf(file,"%s",inlinkname);
		fclose(file);
	}else{
		fgets(temp,90,file);
		sprintf(inlinkname,"%s",temp);
		fclose(file);
	}

	inputmidilink=AddMidiLink(midinode,MLTYPE_Receiver,
		MLINK_Name, "sortoftracker.in",
		MLINK_Location,inlinkname,
		MLINK_EventMask,CMF_Channel,
		NULL
	);


	instrument->PP_Update=MIDIPP_Update;
}


