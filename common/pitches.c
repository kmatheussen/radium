/* Copyright 2013 Kjetil S. Matheussen

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

#include <string.h>
#include <ctype.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "notes_proc.h"
#include "player_pause_proc.h"
#include "player_proc.h"
#include "visual_proc.h"

static int get_chroma(char chromachar){
  chromachar = tolower(chromachar);

  switch(chromachar){
  case 'c':
    return 0;
  case 'd':
    return 2;
  case 'e':
    return 4;
  case 'f':
    return 5;
  case 'g':
    return 7;
  case 'a':
    return 9;
  case 'h':
    return 11;
  case 'b':
    return 11;
  default:
    return -1;
  }
}

static int get_octave(char octavechar){
  octavechar = tolower(octavechar);

  switch(octavechar){
  case '0':
    return 0;
  case '1':
    return 1;
  case '2':
    return 2;
  case '3':
    return 3;
  case '4':
    return 4;
  case '5':
    return 5;
  case '6':
    return 6;
  case '7':
    return 7;
  case '8':
    return 8;
  case '9':
    return 9;
  case 'a':
    return 10;
  case 'b':
    return 11;
  case 'c':
    return 12;
  default:
    return -1;
  }
}

static int get_sharp(char sharptext){
  if(sharptext=='#')
    return 1;
  else if(sharptext=='b')
    return -1;
  else if(sharptext=='B')
    return -1;
  else if(sharptext=='-')
    return 0;
  else
    return -2;
}

static int notenum_from_notetext(char *notetext){
  int chroma, octave, sharp;

  if(strlen(notetext)==2){

    chroma = get_chroma(notetext[0]);
    sharp = 0;
    octave = get_octave(notetext[1]);

  } else if(strlen(notetext)==3){

    chroma = get_chroma(notetext[0]);
    sharp = get_sharp(notetext[1]);
    octave = get_octave(notetext[2]);

  } else
    return -1;

  if(chroma==-1 || sharp==-2 || octave==-1)
    return -1;

  int notenum = octave*12 + chroma + sharp;

  if(notenum<=0 || notenum>127)
    return -1;
  else
    return notenum;
}

struct Pitches *AddPitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float notenum){
  struct Pitches *pitch = talloc(sizeof(struct Pitches));
  PlaceCopy(&pitch->l.p,place);
  pitch->note = notenum;
  //pitch->note_note = note;

  int pos;
  PC_Pause(); {
    pos=ListAddElement3_ns(&note->pitches, &pitch->l);
  }PC_StopPause();

  if(pos==-1)
    return NULL;
  
#if !USE_OPENGL
  ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
  UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
#endif
  return pitch;
}

void DeletePitch(struct Tracks *track, struct Pitches *pitchtodelete){
  struct Notes *note = track->notes;
  while (note != NULL) {
    struct Pitches *pitch = note->pitches;
    while (pitch != NULL) {
      if (pitch==pitchtodelete){
        ListRemoveElement3(&note->pitches, &pitch->l);
        return;
      }
      pitch = NextPitch(pitch);
    }
    note = NextNote(note);
  }

  RError("Pitch not found in track %d",track->l.num);
}

void SetPitchCurrPos(struct Tracker_Windows *window){
  struct WBlocks       *wblock        = window->wblock;
  struct WTracks       *wtrack        = wblock->wtrack;    
  int                   realline_num  = wblock->curr_realline;  
  struct LocalZooms    *realline= wblock->reallines[realline_num];

  struct Notes *note = FindNote(wtrack->track, &realline->l.p);

  float notenum = 0.0f;

  if (note != NULL) {
    
    printf("note: %f\n",note==NULL ? -1 : note->note);
    ReqType reqtype=GFX_OpenReq(window,30,12,"Set Pitch");
    char *notetext;

    do{
      notetext = GFX_GetString(
                               window,
                               reqtype,
                               "Write note to pitch from (for example: c4 or c#5) >"
                               );
      if(notetext==NULL)
        break;
      
      notenum=notenum_from_notetext(notetext);
    }while(notenum==-1);
    
    GFX_CloseReq(window,reqtype);
    
    printf("notenum: %f\n",notenum);

    if(notetext!=NULL)
      AddPitch(window, wblock, wtrack, note, &realline->l.p, notenum);
  }
}
