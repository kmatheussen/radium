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

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define SEQBLOCK_USING_VECTOR 1

#include "nsmtracker.h"
#include "TimeData.hpp"
#include "TallocWithDestructor.hpp"
#include "list_proc.h"
#include "vector_proc.h"
#include "placement_proc.h"
#include "wtracks_proc.h"
#include "realline_calc_proc.h"
#include "undo_notes_proc.h"
#include "cursor_updown_proc.h"
#include "playerclass.h"
#include "pitches_proc.h"
#include "trackreallines2_proc.h"
#include "OS_Player_proc.h"
#include "windows_proc.h"
#include "visual_proc.h"
#include "OS_settings_proc.h"
#include "range_proc.h"
#include "notestext_proc.h"
#include "scheduler_proc.h"
#include "instruments_proc.h"

//#include "Note.hpp"

#include "notes_proc.h"

#ifndef TEST_NOTES

static int FindFirstFreePolyphony_num(const std::vector<Ratio> &end_places, const Ratio &ratio){

  int i = 0;
  for(const Ratio &end_place : end_places)
    if (ratio >= end_place)
      return i;
    else
      i++;
  
  return (int)end_places.size();
}

// Notes are also sorted by pitch (after first being sorted by time)
// so that pitch order is always displayed from left to right in the editor for polyphonic notes.
void r::NoteTimeData::sortit(TimeDataVector *vector){
  vector->sort([](const NotePtr &a, const NotePtr &b){
    //printf("  COMP called %p %p\n", &a, &b);
    if (a.get_time() == b.get_time())
      return a.get_val() < b.get_val();
    else
      return a.get_time() < b.get_time();
  });
}

static void set_note_polyphony_num(r::NoteTimeData *notes, r::NoteTimeData::Writer &writer){
  //printf("**************  Track: %d\n", track->l.num);
  static std::vector<Ratio> s_end_places;
  s_end_places.clear();
  
  notes->_polyphony = 1;

  for(r::NotePtr &note : writer){
    //printf("**************  Note at: %s\n", PlaceToString(&note->l.p));
    int polyphony_num = FindFirstFreePolyphony_num(s_end_places, note->get_time());

    if (note->d._polyphony_num != polyphony_num){
      r::ModifyNote new_note(note);
      new_note->d._polyphony_num = polyphony_num;
    }

    if (polyphony_num+1 > notes->_polyphony)
      notes->_polyphony = polyphony_num+1;

    if (polyphony_num==(int)s_end_places.size())
      s_end_places.push_back(note->d._end);
    else
      s_end_places[polyphony_num] = note->d._end;

    if (note->_val<=0.0f){
      RError("notenum<=0.0f: %f. Setting to 0.01",note->_val);
      note->_val=0.01f;
    }
    else if(note->_val>=128.0f){
      RError("notenum>=128.0f: %f. Setting to 127.99",note->_val);
      note->_val=127.99;
    }

    if (note->get_time() < 0){
      R_ASSERT(false);
      note->set_time(make_ratio(0,1));
    }
    
    if (note->d._end < note->get_time()){
      R_ASSERT(false);
      note->d._end = note->get_time();
    }

  } 
}

static void set_note_min_max_pitch(r::NoteTimeData *notes, r::NoteTimeData::Writer &writer){
  float min_pitch = 10000.0f;
  float max_pitch = -1.0f;

  int num_pitches = 0;
  
  // find min_pitch and max_pitch
  for(const r::NotePtr &note : writer){
    min_pitch = R_MIN(note->get_val(), min_pitch);
    max_pitch = R_MAX(note->get_val(), max_pitch);
    num_pitches ++;
    if (note->d._pitch_end > 0){
      min_pitch = R_MIN(note->d._pitch_end, min_pitch);
      max_pitch = R_MAX(note->d._pitch_end, max_pitch);
      num_pitches ++;
    }

    r::PitchTimeData::Reader reader(&note->_pitches);
    for(const r::Pitch &pitch : reader){
      min_pitch = R_MIN(pitch._val, min_pitch);
      max_pitch = R_MAX(pitch._val, max_pitch);
      num_pitches ++;
    }
  }

  notes->_min_pitch = min_pitch;
  notes->_max_pitch = max_pitch;

  if (num_pitches <= 3) {
    
    notes->_min_display_pitch = 0;
    notes->_max_display_pitch = 127;
    
  }else{
    
    float pitch_range = max_pitch - min_pitch;
    
    min_pitch = min_pitch - pitch_range/8.0f;
    if(min_pitch < 0)
      min_pitch = 0;
    
    max_pitch = max_pitch + pitch_range/8.0f;
    if(max_pitch > 127)
      max_pitch = 127;
    
    notes->_min_display_pitch = min_pitch;
    notes->_max_display_pitch = max_pitch;
  }
}

static void update_line_notes(r::NoteTimeData *notetimedata, const r::NoteTimeData::Writer &writer){
  int last_used_line = -1;
  
  for(const r::NotePtr &note : writer){
    int line_start = note->_time.num / note->_time.den;
    int line_end = note->d._end.num / note->d._end.den;

    printf("  (%d). %d->%d. Sizeof(r::LineNotes): %d\n", last_used_line, line_start, line_end, (int)sizeof(r::LineNotes));

    // Push empty lines. (i.e. delete old notes from last call to 'update_line_notes')
    for(int i = last_used_line+1 ; i < line_start ; i++) {
      
      if (i >= notetimedata->_line_notes.size()) {
        
        notetimedata->_line_notes.push_back(NULL);

      } else {
        
        r::LineNotes *notes = notetimedata->_line_notes.at_ref(i);
        if (notes != NULL)
          notes->clear();

      }
      
    }

    // Push lines for this note.
    for(int i = line_start ; i <= line_end ; i++){

      r::LineNotes *notes;

      if (i >= notetimedata->_line_notes.size()) {

        R_ASSERT_NON_RELEASE(i == notetimedata->_line_notes.size());
        
        notes = new r::LineNotes();
        notetimedata->_line_notes.push_back(notes);
        
      } else {
        
        notes = notetimedata->_line_notes.at_ref(i);

        if (notes == NULL) {
          
          notes = new r::LineNotes();
          notetimedata->_line_notes.replace(i, notes);
          
        } else if (i > last_used_line) {
          
          notes->clear();
          
        }

      }

      //printf(".....pushing note %f to line %d\n", note->get_val(), i);
      notes->push_back(note.get_mutable());
    }

    last_used_line = R_MAX(last_used_line, line_end);
  }

  notetimedata->_num_valid_elements_in_line_notes = last_used_line + 1;

#if !defined(RELEASE)
  for(int i = 0 ; i < notetimedata->_num_valid_elements_in_line_notes ; i++) {
    const auto *notes = notetimedata->_line_notes.at_ref(i);
    if (notes && notes->size() > 0)
      printf("%d: %d\n", i, notes->size());
  }
#endif
}

void r::NoteTimeData::writer_finalizer(Writer &writer){
  
  R_ASSERT(THREADING_is_main_thread()); // This function is not thread safe.

  {
    for(const auto &note : writer)
      printf("\n. 1111 Note: %d.\n", (int)note->_id);
    printf("\n");
  }

  set_note_polyphony_num(this, writer);

  {
    for(const auto &note : writer)
      printf("\n. 2222 Note: %d.\n", (int)note->_id);
    printf("\n");
  }

  set_note_min_max_pitch(this, writer);

  {
    for(const auto &note : writer)
      printf("\n. 3333 Note: %d.\n", (int)note->_id);
    printf("\n");
  }
  
  update_line_notes(this, writer);

  {
    for(const auto &note : writer)
      printf("\n. 4444 Note: %d.\n", (int)note->_id);
    printf("\n");
  }
}

int GetNoteSubtrack(const struct WTracks *wtrack, struct Notes *note){
  return WTRACK_num_non_polyphonic_subtracks(wtrack) + note->polyphony_num;
}

int GetNoteSubtrack2(const struct WTracks *wtrack, const r::NotePtr &note){
  return WTRACK_num_non_polyphonic_subtracks(wtrack) + note->d._polyphony_num;
}

int GetNumSubtracks(const struct WTracks *wtrack){
  return WTRACK_num_subtracks(wtrack);
}

/**************************************************************
  FUNCTION
    Returns the current note (under the cursor).
**************************************************************/
const r::NotePtr GetCurrNote(struct Tracker_Windows *window){
	struct WBlocks       *wblock        = window->wblock;
	struct WTracks       *wtrack        = wblock->wtrack;

        Trs trs = TRS_get(wblock, wtrack, wblock->curr_realline);
        
        if (trs.size()==0)
          return r::NotePtr();

        const TrackRealline2 &tr2 = trs[0];

        return tr2.note;
}



/**************************************************************
  FUNCTION
    Set the _end attributes for note 'note'.
    Finds next note to stop at, or block length.
**************************************************************/
static void SetEndAttributes(
	const struct Blocks *block,
	const struct Tracks *track,
	struct Notes *note
){
	const Place *place;
	const Place *p1=NULL,*p2=NULL;

        bool endSetEarlier = note->end.den>0 && note->end > place2ratio(note->l.p); //PlaceGreaterThan(&note->end, &note->l.p);
        const Ratio earliest = endSetEarlier ? note->end : place2ratio(note->l.p);

        struct ListHeader3 *nextnote=track->notes==NULL ? NULL : &track->notes->l;
        while(nextnote!=NULL){
          Ratio r = place2ratio(nextnote->p);
          if (r > earliest) {
            p1 = &nextnote->p;
            break;
          }
          nextnote=nextnote->next;
        }

        Place helper_p2;
        
        const r::StopTimeData::Reader reader(track->stops2);
        for(const r::Stop &stop : reader) {
          if (stop._time > earliest){
            helper_p2 = ratio2place(stop._time);
            p2 = &helper_p2;
            break;
          }            
        }
        
	place=PlaceMin(p1,p2);

	if(place!=NULL){
          note->end = place2ratio(*place);
	}else{
          note->end = place2ratio(p_Last_Pos(block));
          note->noend=1;
	}

        //ValidatePlace(&note->end);
}


static void SetEndAttributes2(
	const struct Blocks *block,
	const struct Tracks *track,
	r::Note *note
){
        const bool endSetEarlier = note->d._end.den>0 && note->d._end > note->get_time();
        const Ratio earliest = endSetEarlier ? note->d._end : note->get_time();

        bool has_r1 = false;
        Ratio r1;

        {
          const r::NoteTimeData::Reader reader(track->_notes2);
          
          for(const r::NotePtr &note : reader) {
            Ratio r = note.get_time();
            if (r > earliest) {
              r1 = r;
              has_r1 = true;
              break;
            }
          }
        }
        
        bool has_r2 = false;
        Ratio r2;

        {
          const r::StopTimeData::Reader reader(track->stops2);
          
          for(const r::Stop &stop : reader) {
            Ratio r = stop.get_time();
            if (r > earliest){
              r2 = r;
              has_r2 = true;
              break;
            }            
          }
        }
        
        if (has_r1 || has_r2) {
          
          if (has_r1 && has_r2)            
            note->d._end = R_MIN(r1, r2);
          else
            note->d._end = has_r1 ? r1 : r2;
          
        } else {
          
          note->d._end = make_ratio(block->num_lines, 1);
          note->d._noend = true;
          
        }
}



/**************************************************************
  FUNCTION
    Stops all notes before line+(counter/dividor) at
    line+(counter/dividor, if they last that long.
**************************************************************/
void StopAllNotesAtPlace(
                         struct Blocks *block,
                         struct Tracks *track,
                         const Place *placement
){
  if (PLAYER_current_thread_has_lock()==false && is_playing()==true){
    RError("StopAllNotesAtPlace. PLAYER_current_thread_has_lock(): %d, is_playing(): %d", PLAYER_current_thread_has_lock()==false, is_playing()==true);
  }
          
	struct Notes *temp;

	temp=track->notes;

        Ratio r_placement = place2ratio(*placement);
        
	while(temp!=NULL && PlaceLessThan(&temp->l.p,placement)){
              if(temp->end > r_placement) {
                //CutListAt(&temp->velocities,placement);

                        {
                          radium::PlayerUnlock unlock(PLAYER_current_thread_has_lock()); // Writer allocates memory. (Shouldn't be a problem temporarily unlocking here.)

                          {
                            r::VelocityTimeData::Writer writer(temp->_velocities);
                            writer.remove_everything_after(ratio_from_place(*placement), true);
                          }

                          {
                            r::PitchTimeData::Writer writer(temp->_pitches);
                            writer.remove_everything_after(ratio_from_place(*placement), true);
                          }
                        }
                        
			//CutListAt(&temp->pitches,placement);
			temp->end = r_placement;
                        NOTE_validate(block, track, temp);
		}
		temp=NextNote(temp);
	}
}


static void StopAllNotesAtRatio(
                         struct Blocks *block,
                         struct Tracks *track,
                         r::NoteTimeData::Writer &writer,
                         const Ratio &ratio
){
  for(r::NotePtr &note : writer) {
    if (note.get_time() >= ratio)
      break;
    
    if (note->d._end > ratio) {
      r::ModifyNote new_note(note);
      
      {
        r::VelocityTimeData::Writer writer(&new_note->_velocities);
        writer.remove_everything_after(ratio, true);
      }
      
      {
        r::PitchTimeData::Writer writer(&new_note->_pitches);
        writer.remove_everything_after(ratio, true);
      }
      
      new_note->d._end = ratio;
    }
  }
}


int64_t new_note_id(void){
  static int64_t curr_id = -1;

  if(curr_id==-1)
    curr_id = NotenumId(1024);

  int64_t ret = curr_id;
  curr_id += NUM_PATCH_VOICES; // Temp hack. Maybe.

  return ret;
}

static void NOTE_init(struct Notes *note){
  note->id = new_note_id();
}

static struct Notes *alloc_note(void){
  struct Notes *note = talloc_with_finalizer<struct Notes>([](struct Notes *note){
      delete note->_velocities;
      delete note->_pitches;
    });
  
  note->_velocities = new r::VelocityTimeData;
  note->_pitches = new r::PitchTimeData;
  
  return note;
}

struct Notes *NewNote(void){
  struct Notes *note = alloc_note();
  
  NOTE_init(note);
  note->chance = MAX_PATCHVOICE_CHANCE;

  return note;
}

r::NotePtr NewNote2(const Ratio &time,
                    float notenum,
                    int velocity)
{
  return r::NotePtr(new r::Note(time, notenum, velocity));
}


r::NotePtr NewNoteFromOldNote(const struct Notes *note){
  r::NotePtr new_note = NewNote2(place2ratio(note->l.p), note->note, note->velocity);

  r::NoteData &d = new_note->d;
  
  d._pitch_first_logtype = note->pitch_first_logtype;
  d._velocity_first_logtype = note->velocity_first_logtype;

  d._end = note->end;

  d._velocity_end = note->velocity_end;
  d._pitch_end = note->pitch_end;

  d._noend = note->noend;

  d._chance = note->chance;

  new_note->_velocities.replace_with(note->_velocities);
  new_note->_pitches.replace_with(note->_pitches);
  
  return new_note;
}

struct Notes *CopyNote(const struct Notes *old_note){
  struct Notes *note = alloc_note();
  
  auto *_velocities = note->_velocities;
  auto *_pitches = note->_pitches;
  {
    memcpy(note, old_note, sizeof(struct Notes));
  }
  note->_pitches = _pitches;
  note->_velocities = _velocities;
  
  note->l.next = NULL;
  //note->velocities = NULL;
  //note->pitches = NULL;
  
  NOTE_init(note);

  return note;
}

static struct Notes *sort_notes_by_pitch_a_little_bit(struct Notes *notes){
  struct Notes *ret = notes;
  struct Notes *note_to_place_after = NULL;
  
  while(notes != NULL){
    struct Notes *next = NextNote(notes);

    R_ASSERT_RETURN_IF_FALSE2(next!=NULL, NULL);
    
    if (PlaceEqual(&notes->l.p, &next->l.p))
      if (notes->note > next->note) {
        note_to_place_after = notes;
        break;
      }
    
    notes = next;
  }

  ListRemoveElement3(&ret, &note_to_place_after->l);
  ListAddElement3_a(&ret, &note_to_place_after->l);
  
  return ret;
}

bool NOTES_sorted_by_pitch_questionmark(struct Notes *notes){
  while(notes != NULL){
    struct Notes *next = NextNote(notes);    
    if (next==NULL)
      return true;
    if (PlaceEqual(&notes->l.p, &next->l.p))
      if (notes->note > next->note)
        return false;
    notes = next;
  }
  return true;
}

struct Notes *NOTES_sort_by_pitch(struct Notes *notes){
  while(NOTES_sorted_by_pitch_questionmark(notes)==false) {
    struct Notes *better = sort_notes_by_pitch_a_little_bit(notes);
    if (better==NULL) // should not be possible, but in case the impossible happens, we don't want to lose all notes
      return notes;
    
    notes = better;
  }

  return notes;
}

/*
void NOTES_sort_by_pitch(r::NoteTimeData::Writer &writer){
  for(int i = 0 ; i < writer.size()-1){
    const NotePtr &a = writer.at_ref(i);
    const NotePtr &b = writer.at_ref(i+1);
    if (a.get_time() == b.get_time()){
      if (a->get_val() > b->get_val())
        
    }
  }
}
*/

static void set_new_position(struct Tracks *track, struct Notes *note, Place *start, Ratio *end){

  bool has_lock = PLAYER_current_thread_has_lock();

  if (track==NULL && has_lock)
    RError("track==NULL && has_lock");

  bool need_lock = (track!=NULL && has_lock==false && is_playing()==false);

  {
    radium::PlayerLock lock(need_lock);
    
    if (track!=NULL)
      ListRemoveElement3(&track->notes, &note->l);
    
    if (start!=NULL && start!=&note->l.p)
      note->l.p = *start;
    
    if (end!=NULL)
      note->end = *end;
    
    if (track!=NULL)
      ListAddElement3(&track->notes, &note->l);
  }
}

static void set_legal_start_and_end_pos(const struct Blocks *block, struct Tracks *track, struct Notes *note){
  Place *start = &note->l.p;
  Ratio end = note->end;
  Place endplace;

  PlaceSetLastPos(block,&endplace);

  Ratio r_endplace = place2ratio(endplace);
  
  if(PlaceGreaterOrEqual(start,&endplace)) {
    RError("note is placed after block end. start: %f, end: %f", GetfloatFromPlace(&note->l.p), make_double_from_ratio(note->end));
    set_new_position(track, note, PlaceCreate(block->num_lines - 2, 0, 1), NULL);
    start = &note->l.p;
  }
  
  if (start->line < 0) {
    RError("note is placed before block start. start: %f, end: %f", GetfloatFromPlace(&note->l.p), make_double_from_ratio(note->end));
    set_new_position(track, note, PlaceCreate(0,1,1), NULL);
    start = &note->l.p;
  }
  
  if(end > r_endplace) {
    RError("note end is placed after block end. start: %f, end: %f. block end: %f", GetfloatFromPlace(&note->l.p), make_double_from_ratio(note->end), GetfloatFromPlace(&endplace));
    set_new_position(track, note, NULL, &r_endplace);
    end = note->end;
  }

  /*
  if (note->velocities != NULL) {
    {
      struct Velocities *first_velocity = note->velocities;
      if(PlaceGreaterThan(start, &first_velocity->l.p)){
        RError("note start is placed after first velocity. start: %f, first: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&first_velocity->l.p), GetfloatFromPlace(&note->end));
        float e = p_float(first_velocity->l.p);
        e -= 0.01;
        e = R_MAX(0.0, e);
        Place new_start;
        Float2Placement(e, &new_start);
        set_new_position(track, note, &new_start, NULL);
        start = &note->l.p;
      }
    }

    struct Velocities *last_velocity = (struct Velocities*)ListLast3(&note->velocities->l);
    if(PlaceLessThan(end, &last_velocity->l.p)){
      RError("note end is placed before last velocity. start: %f, last: %f, end: %f", GetfloatFromPlace(&note->l.p), GetfloatFromPlace(&last_velocity->l.p), GetfloatFromPlace(&note->end));
      float e = p_float(last_velocity->l.p);
      e += 0.01;
      Place new_end;
      Float2Placement(e, &new_end);
      set_new_position(track, note, NULL, &new_end);
      end = &note->end;
    }

  }
  */
  
  if(end < place2ratio(*start)) {
    RError("note end is placed before (or on) note start. start: %f, end: %f", GetfloatFromPlace(&note->l.p), make_double_from_ratio(note->end));
    float e = p_float(*start);
    e += 0.01;
    Ratio new_end = make_ratio_from_double(e);
    set_new_position(track, note, NULL, &new_end);
  }

}

void NOTE_validate(const struct Blocks *block, struct Tracks *track, struct Notes *note){
  R_ASSERT_RETURN_IF_FALSE(block!=NULL);
  R_ASSERT_RETURN_IF_FALSE(note!=NULL);

  R_ASSERT(track==NULL || PLAYER_current_thread_has_lock() || is_playing()==false);
  
  if (note->note<=0.0f){
    RError("notenum<=0.0f: %f. Setting to 0.01",note->note);
    note->note=0.01f;
  }
  if(note->note>=128.0f){
    RError("notenum>=128.0f: %f. Setting to 127.99",note->note);
    note->note=127.99;
  }

  set_legal_start_and_end_pos(block, track, note);

  ValidatePlace(&note->l.p);
  //ValidatePlace(&note->end);
}



static struct Notes *make_note(const struct Blocks *block,
                               const struct Tracks *track,
                               const Place *placement,
                               const Place *end_placement,
                               float notenum,
                               int velocity)
{
  struct Notes *note=NewNote();
  PlaceCopy(&note->l.p,placement);
  
  note->note=notenum;
  note->velocity=velocity;
  //	note->velocity=(*wtrack->track->instrument->getStandardVelocity)(wtrack->track);
  note->velocity_end=note->velocity;

  if (end_placement != NULL)
    note->end = place2ratio(*end_placement);
  else
    SetEndAttributes(block,track,note);

  return note;
}

static r::NotePtr make_note2(const struct Blocks *block,
                             const struct Tracks *track,
                             const Ratio &time,
                             const Place *end_placement,
                             float notenum,
                             int velocity)
{
  r::NotePtr note = NewNote2(time, notenum, velocity);

  r::Note *note2 = note.get_mutable();
  
  if (end_placement != NULL)
    note2->d._end = place2ratio(*end_placement);
  else
    SetEndAttributes2(block,track,note2);

  return note;
}


// Note: GfxNotes are always polyphonic.
struct Notes *InsertGfxNote(struct WBlocks *wblock,
                            struct WTracks *wtrack,
                            const Place *placement, 
                            const Place *end_placement,
                            float notenum,
                            int velocity
                            )
{
  struct Blocks *block=wblock->block;
  struct Tracks *track=wtrack->track;

  {
    r::NoteTimeData::Writer writer(track->_gfx_notes2);
    writer.add(make_note2(block, track, place2ratio(*placement), end_placement, notenum, velocity));
  }
  
  {
    struct Notes *note = make_note(block, track, placement, end_placement, notenum, velocity);

    ListAddElement3(&track->gfx_notes,&note->l);
    return note;
  }
}


  
#if !defined(RELEASE)
static void test(
                 struct Blocks *block,
                 struct Tracks *track
                 );
#endif

int64_t InsertNote(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	const Place *placement,
        const Place *end_placement,
	float notenum,
	int velocity,
	bool polyphonic
){
    
	struct Blocks *block=wblock->block;
	struct Tracks *track=wtrack->track;

#if !defined(RELEASE)
        test(block, track);
#endif

        int64_t id;
        
        {
          const Ratio ratio = place2ratio(*placement);

          auto note = make_note2(block, track, ratio, end_placement, notenum, velocity);

          id = note->_id;
          
          r::NoteTimeData::Writer writer(track->_notes2);
          writer.add(std::move(note));


          if(polyphonic==false)
            StopAllNotesAtRatio(block, track, writer, ratio);

        }
          
        {
          struct Notes *note = make_note(block, track, placement, end_placement, notenum, velocity);
          
          {
            SCOPED_PLAYER_LOCK_IF_PLAYING();
            
            ListAddElement3(&track->notes,&note->l);
            
            if(polyphonic==false)
              StopAllNotesAtPlace(block,track,placement);
            
            track->notes = NOTES_sort_by_pitch(track->notes);
          }
          
          NOTE_validate(block, NULL, note);
        }

        return id;
}

bool drunk_velocity=false;
static int64_t last_velocity = MAX_VELOCITY / 2;

int NOTE_get_velocity(struct Tracks *track){

  if(drunk_velocity==false)
    return root->standardvel;

  int64_t new_velocity = last_velocity + scale_int64(rand(),0,RAND_MAX,-(MAX_VELOCITY/3), MAX_VELOCITY/3);
  if(new_velocity>=root->standardvel)
    new_velocity = root->standardvel - MAX_VELOCITY/50;
  if(new_velocity<root->min_standardvel)
    new_velocity = root->min_standardvel + MAX_VELOCITY / 4;

  last_velocity = new_velocity;

  //printf("returning %d\n",(int)new_velocity);

  return (int)new_velocity;
}

static bool maybe_scroll_down(struct Tracker_Windows *window, const int64_t dont_play_this_note){
  if(window->curr_track_sub==-1) 
    return MaybeScrollEditorDownAfterEditing(window, dont_play_this_note);
  else
    return false;
}

// return true if it was scroll-playing (i.e. may have played the note)
bool InsertNoteCurrPos(struct Tracker_Windows *window, float notenum, bool polyphonic, float org_velocity){
  
  if(notenum<0.001 || notenum>127.9) return false;

  ADD_UNDO(Notes_CurrPos(window));

  struct WBlocks *wblock        = window->wblock;
  struct WTracks *wtrack        = wblock->wtrack;
  struct Tracks  *track         = wtrack->track;
  int             curr_realline = wblock->curr_realline;

  R_ASSERT_RETURN_IF_FALSE2(curr_realline >= 0 && curr_realline<wblock->num_reallines, false);
  
  int velocity = org_velocity < 0.0 ? NOTE_get_velocity(wtrack->track) : org_velocity*MAX_VELOCITY;
    
  const Trs &trs = TRS_get(wblock, wtrack, curr_realline);

  if (polyphonic==false && trs.size() > 0) {
    const TrackRealline2 &tr2 = trs[0];

    //struct Notes *note = tr2.note;
    //    R_ASSERT_RETURN_IF_FALSE2(note==NULL, false);

    switch(tr2.type){
      case TR2_NOTE_START:
      case TR2_NOTE_END:
        {
          R_ASSERT_RETURN_IF_FALSE2(tr2.note, false);
          
          // lock not necessary
          if (tr2.type==TR2_NOTE_END)
            tr2.note->d._pitch_end = notenum;
          else
            tr2.note->_val = notenum;
          
          if (org_velocity >= 0){
            tr2.note->d._velocity = velocity;
            if (r::VelocityTimeData::Reader(&tr2.note->_velocities).size()==0)
              tr2.note->d._velocity_end = velocity;
          }
          
          window->must_redraw=true;
          return maybe_scroll_down(window, tr2.note->_id);
        }
      case TR2_PITCH:
        R_ASSERT_RETURN_IF_FALSE2(tr2.note, false);
        {
          r::PitchTimeData::Writer writer(&tr2.note->_pitches);
          if (tr2.pitchnum < 0 || tr2.pitchnum >= writer.size())
            R_ASSERT(false);
          else        
            writer.at_ref(tr2.pitchnum)._val = notenum;
        }
        window->must_redraw=true;
        return maybe_scroll_down(window, tr2.note->_id);
      case TR2_STOP:
        // Going to replace a stop with a note.
        r::StopTimeData::Writer writer(track->stops2);
        writer.remove_at_time(place2ratio(tr2.p));
        break;
    }
  }

  const struct LocalZooms *realline = wblock->reallines[curr_realline];
  
  int64_t id = InsertNote(
                          wblock,wtrack,&realline->l.p,NULL,notenum,
                          velocity,
                          polyphonic
                          );

  window->must_redraw=true;
  
  //if(wtrack->l.num==wblock->right_track && polyphonic)
  //  UpdateAllWTracksCoordinates(window,wblock);

  if (!polyphonic)
    return maybe_scroll_down(window, id);

  return false;
}

static void InsertStop(
                       struct Blocks *block,
                       struct Tracks *track,
                       const Place *placement
){
  
        r::StopTimeData::Writer writer(track->stops2);

        /*
	struct Stops *stop;

        stop = (struct Stops*)talloc(sizeof(struct Stops));
	PlaceCopy(&stop->l.p,placement);

        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();

          StopAllNotesAtPlace(block,track,placement); // NOTE: This line is still needed.
          
  	  ListAddElement3_ns(&track->stops,&stop->l);          
        }
        */

#if 0
        {
          SCOPED_PLAYER_LOCK_IF_PLAYING();
          StopAllNotesAtPlace(block,track,placement);
        }
#else
        {
          r::NoteTimeData::Writer writer(track->_notes2);
          StopAllNotesAtRatio(block,track,writer, place2ratio(*placement));
        }
#endif
        
        r::Stop stop2(ratio_from_place(*placement));
        writer.add(stop2);
}

/**********************************************************************
  FUNCTION
    Set the end attributes of all notes that previously was stopped
    at position 'placement' to the next stop wherever that may be.
**********************************************************************/
void LengthenNotesTo(
	struct Blocks *block,
	struct Tracks *track,
	const Place *placement
){
	struct Notes *note=track->notes;
	while(note!=NULL){
		if(PlaceGreaterThan(&note->l.p,placement)) break;
		if(note->end == place2ratio(*placement))
                  SetEndAttributes(block,track,note);
		note=NextNote(note);
	}
}

static void LengthenNotesTo2(
                             struct Blocks *block,
                             struct Tracks *track,
                             r::NoteTimeData::Writer &writer,
                             const Ratio &ratio
){
  for(r::NotePtr &note: writer) {
    if (note->get_time() >= ratio)
      break;

    if (note->d._end == ratio){
      r::ModifyNote new_note(note);
      SetEndAttributes2(block, track, new_note.get());
    }
  }
}

/**********************************************************************
  FUNCTION
    Set the end attributes of all notes that previously was stopped
    at position 'old_placement' to 'new_placement'.
**********************************************************************/
void ReplaceNoteEnds(
	struct Blocks *block,
	struct Tracks *track,
	const Place *old_placement,
        const Place *new_placement,
        int polyphony_num
){
        R_ASSERT(PLAYER_current_thread_has_lock() || is_playing()==false);
  
	struct Notes *note=track->notes;
	while(note!=NULL){
          if (note->polyphony_num == polyphony_num) {
            if(PlaceGreaterThan(&note->l.p,old_placement)) break;
            if(note->end == place2ratio(*old_placement)) {
              note->end = place2ratio(*old_placement);
              NOTE_validate(block, track, note);
            }
          }
          note=NextNote(note);
	}
}

void ReplaceNoteEnds2(
                      struct Blocks *block,
                      struct Tracks *track,
                      r::NoteTimeData::Writer &writer,
                      const Ratio &old_ratio,
                      const Ratio &new_ratio,
                      int polyphony_num
                      )
{
  for(r::NotePtr &note : writer) {
    if (note->d._polyphony_num == polyphony_num) {
      
      if(note->get_time() > old_ratio)
        break;
      
      if(note->d._end == old_ratio) {
        r::ModifyNote new_note(note);
        new_note->d._end = new_ratio;
      }
    }
  }
}

void RemoveNote(
	struct Blocks *block,
	struct Tracks *track,
	const struct Notes *note
){
  R_ASSERT(PLAYER_current_thread_has_lock() || is_playing()==false);
  ListRemoveElement3(&track->notes,&note->l);
  LengthenNotesTo(block,track,&note->l.p);
}

void RemoveNote2(
                 struct Blocks *block,
                 struct Tracks *track,
                 r::NoteTimeData::Writer &writer,
                 const r::NotePtr &note
                 )
{
  R_ASSERT_RETURN_IF_FALSE(note);

  printf("!!!!! About to remove note with id %d\n", (int)note->_id);
  Ratio time = note->get_time();
  
  bool removed = writer.removeElement(note);

  R_ASSERT_NON_RELEASE(removed);
  
  LengthenNotesTo2(block,track,writer, time);
}

void RemoveNoteCurrPos(struct Tracker_Windows *window){
  struct WBlocks       *wblock        = window->wblock;
  struct Blocks        *block         = wblock->block;
  struct WTracks       *wtrack        = wblock->wtrack;
  struct Tracks        *track         = wtrack->track;
  const struct LocalZooms    *realline      = wblock->reallines[wblock->curr_realline];
  int                   curr_realline = wblock->curr_realline;
  
  const Trs &trs = TRS_get(wblock, wtrack, curr_realline);

  ADD_UNDO(Notes_CurrPos(window));

  if (trs.size()==0) {
    InsertStop(block,track,&realline->l.p);
    window->must_redraw=true;
    maybe_scroll_down(window, -1);
    return;
  }

  
  const TrackRealline2 &tr2 = trs.at(0);

  switch(tr2.type){
    
    case TR2_NOTE_START:
      {
#if 0
        if (track->notes!=NULL && isInList3(&track->notes->l, &tr2.note->l)) {
          EVENTLOG_add_event("RemoveNoteCurrPos 2");
          {
            SCOPED_PLAYER_LOCK_IF_PLAYING();
            ListRemoveElement3(&track->notes,&tr2.note->l);
            LengthenNotesTo(block,track,&realline->l.p);
          }
          ValidateCursorPos(window);
          window->must_redraw=true;
          if (trs.size()==1)
            maybe_scroll_down(window, -1);
        }
#else
        bool was_removed = false;
        {
          r::NoteTimeData::Writer writer(track->_notes2);
          
          if (!writer.removeElement(tr2.note)){
            R_ASSERT_NON_RELEASE(false);
          } else {
            LengthenNotesTo2(block, track, writer, place2ratio(realline->l.p));
            was_removed = true;
          }
        }

        if (was_removed){
          ValidateCursorPos(window);
        
          window->must_redraw=true;
          if (trs.size()==1)
            maybe_scroll_down(window, -1);
        }
#endif
      }
      break;
      
    case TR2_NOTE_END:
      {
        const r::PitchTimeData::Reader reader(&tr2.note->_pitches);
        if (reader.size() > 0)
          tr2.note->d._pitch_end = reader.at_last()._val;
        else
          tr2.note->d._pitch_end = 0;
      }
      break;
      
    case TR2_PITCH:
      {
        EVENTLOG_add_event("RemoveNoteCurrPos 1");

#if 0
        DeletePitch(track, tr2.note, tr2.pitchnum);
#else
        r::PitchTimeData::Writer writer(&tr2.note->_pitches);
        if (!writer.remove_at_pos(tr2.pitchnum)){
          R_ASSERT_NON_RELEASE(false);
        }
#endif
        
        window->must_redraw=true;
        if (trs.size()==1)
          maybe_scroll_down(window, -1);
      }
      break;
      
    case TR2_STOP:
      {
        EVENTLOG_add_event("RemoveNoteCurrPos 3");
        r::StopTimeData::Writer writer(track->stops2);
        if (writer.remove_at_time(place2ratio(tr2.p))){
          //SCOPED_PLAYER_LOCK_IF_PLAYING();

          r::NoteTimeData::Writer writer(track->_notes2);
          
          LengthenNotesTo2(block, track, writer, place2ratio(realline->l.p));
        }        
        window->must_redraw=true;
        
        if (trs.size()==1)
          maybe_scroll_down(window, -1);
      }
      break;
  }
}

struct Notes *FindPrevNoteOnSameSubTrack(const struct Tracks *track, const struct Notes *note){
  struct Notes *notes = track->notes;
  struct Notes *prev = NULL;
  
  while(notes != note){
    if (notes->polyphony_num == note->polyphony_num)
      prev = notes;
    notes = NextNote(notes);
  }

  return prev;
}

r::NotePtr FindPrevNoteOnSameSubTrack2(const radium::Vector<r::NotePtr> &notes, const r::NotePtr &note){

  r::NotePtr ret;
  
  for(const r::NotePtr &note2 : notes) {
    if (note2==note)
      break;
      
    if (note2->d._polyphony_num == note->d._polyphony_num)
      ret = note2;
  }

  return ret;
}

struct Notes *FindNextNoteOnSameSubtrack(struct Notes *note){
  int polyphony_num = note->polyphony_num;

  note = NextNote(note);
  while(note!=NULL){
    if (note->polyphony_num==polyphony_num)
      return note;
    else
      note = NextNote(note);
  }
  return NULL;
}

r::NotePtr FindNextNoteOnSameSubtrack2(const radium::Vector<r::NotePtr> &notes, const r::NotePtr &note){

  int polyphony_num = note->d._polyphony_num;

  bool gotit = false;
  
  for(const r::NotePtr &note2 : notes)
    if (!gotit){
      if (note2==note)
        gotit = true;
    }else{
      if (note2->d._polyphony_num==polyphony_num)
        return note;
    }

  return r::NotePtr();
}


struct Notes *FindNoteOnSubTrack(
                                 const struct WTracks *wtrack,
                                 int subtrack,
                                 const Place *placement
){
        struct Notes *note = wtrack->track->notes;
        
        while (note != NULL) {
          Place p = ratio2place(note->end);
          if(PlaceIsBetween2(placement,&note->l.p,&p))
            if (NOTE_subtrack2(wtrack, note->polyphony_num)==subtrack)
              return note;
          
          note = NextNote(note);
        }

        return NULL;
}

r::NotePtr FindNoteOnSubTrack2(const struct WTracks *wtrack,
                               int subtrack,
                               const Ratio &ratio
                               )
{
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE(); // Must provide NoteTimeData::ReaderWriter& to use it outside the main thread (to ensure it's not deleted while using it).
  
        const r::NoteTimeData::Reader reader(wtrack->track->_notes2);

        for(const r::NotePtr &note : reader)
          if (ratio >= note->get_time() && ratio < note->d._end)
            if (NOTE_subtrack2(wtrack, note->d._polyphony_num)==subtrack)
              return note;
          
        return r::NotePtr();
}

struct Notes *FindNote(
                       struct Tracks *track,
                       const Place *placement
                       )
{
  struct Notes *note = track->notes;
  while(note != NULL) {
    Place p = ratio2place(note->end);
    if (PlaceIsBetween2(placement, &note->l.p, &p))
      break;
    note = NextNote(note);
  }
  return note;
}

static r::NotePtr FindNote2(
                            struct Tracks *track,
                            const Ratio &ratio
                            )
{
  const r::NoteTimeData::Reader reader(track->_notes2);
          
  for(const r::NotePtr &note : reader)
    if (ratio >= note.get_time() && ratio < note->d._end)
      return note;

  return r::NotePtr();
}

vector_t FindAllNotes(
                      struct Tracks *track,
                      const Place *placement
                      )
{
  vector_t ret = {};
  
  struct Notes *note = track->notes;
  while(note != NULL) {
    Place p = ratio2place(note->end);
    if (PlaceIsBetween2(placement, &note->l.p, &p))
      VECTOR_push_back(&ret, note);
    note = NextNote(note);
  }

  return ret;
}

struct Notes *FindNextNote(
                       struct Tracks *track,
                       const Place *placement
                       )
{
  struct Notes *note = track->notes;
  while(note != NULL) {
    if (PlaceGreaterOrEqual(&note->l.p, placement))
      break;
    note = NextNote(note);
  }
  return note;
}

static bool is_at_last_line_of_note(const struct WBlocks *wblock, const r::NotePtr &note, int realline){
  Place p = ratio2place(note->d._end);
  int last_note_line = FindRealLineFor(wblock, 0, &p);
  //printf("last_note_line/realline: %d %d\n",last_note_line,realline);

  if (last_note_line == realline)
    return true;

  if (realline>=wblock->num_reallines-1) // Shouldn't happen, but just for safety.
    return false;
  
  const struct LocalZooms *realline_plus1 = wblock->reallines[realline+1];
  if (realline_plus1->l.p.counter==0 && last_note_line == realline+1)
    return true;

  return false;
}

vector_t FindAllNotesCurrPos(struct Tracker_Windows *window){
  struct WBlocks    *wblock   = window->wblock;
  struct WTracks    *wtrack   = wblock->wtrack;
  struct Tracks     *track    = wtrack->track;
  const struct LocalZooms *realline = wblock->reallines[wblock->curr_realline];

  if(is_track_ranged(wblock,wtrack) && is_realline_ranged(wblock,wblock->curr_realline))
    return get_all_ranged_notes(wblock);
  
  int subtrack=window->curr_track_sub;
  
  struct Notes *note = FindNoteOnSubTrack(wtrack, subtrack, &realline->l.p);
  if (note!=NULL){
    vector_t ret = {};
    VECTOR_push_back(&ret, note);
    return ret;
  }
  
  return FindAllNotes(track, &realline->l.p);
}

struct Notes *FindNoteCurrPos(struct Tracker_Windows *window){
  vector_t notes = FindAllNotesCurrPos(window);
  if (notes.num_elements==0)
    return NULL;
  else
    return (struct Notes*)notes.elements[0];
}

#endif // TEST_NOTES


/******************/
/* General RETURN */
/******************/

static int get_chroma(char chromachar){
  switch(tolower(chromachar)){
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

static const char *chroma_to_string(int chroma){
  switch(chroma){
  case 0:
    return "c";
  case 1:
    return "c#";
  case 2:
    return "d";
  case 3:
    return "d#";
  case 4:
    return "e";
  case 5:
    return "f";
  case 6:
    return "f#";
  case 7:
    return "g";
  case 8:
    return "g#";
  case 9:
    return "a";
  case 10:
    return "a#";
  case 11:
    return "b";
  default:
    return talloc_format("<error: \"%d\" is outside note range>", chroma);
  }
}


static int get_octave(char octavechar){
  switch(tolower(octavechar)){
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

static const char *octave_to_string(int octave){
  switch(octave){
  case 0:
    return "0";
  case 1:
    return "1";
  case 2:
    return "2";
  case 3:
    return "3";
  case 4:
    return "4";
  case 5:
    return "5";
  case 6:
    return "6";
  case 7:
    return "7";
  case 8:
    return "8";
  case 9:
    return "9";
  case 10:
    return "a";
  case 11:
    return "b";
  case 12:
    return "c";
  default:
    return talloc_format("<error: \"%d\" is outside octave range>", octave);   
  }
}


static int get_sharp(const char sharptext){
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

static char *substring(const char *s,int start,int end){
  char *ret       = (char*)talloc(end-start+1);
  int   read_pos  = start;
  int   write_pos = 0;

  while(read_pos<end)
    ret[write_pos++] = s[read_pos++];

  return ret;
}

static int string_charpos(const char *s, char c){
  int pos=0;
  while(s[pos]!=0){
    if(s[pos]==c)
      return pos;
    pos++;
  }
  return -1;
}

static char *strip_whitespace(const char *s){
  char *ret=talloc_strdup(s);

  // strip before
  while(isspace(ret[0]))
    ret++;

  int len = (int)strlen(ret);

  // strip after
  int pos=(int)len-1;
  while(pos>0 && isspace(ret[pos])){
    ret[pos]=0;
    pos--;
  }


  return ret;
}

float notenum_from_notetext(const char *notetext){
  int chroma, octave, sharp;

  const char *stripped = strip_whitespace(notetext);

  const char *notename;
    
  int comma_pos = string_charpos(stripped, ',');
  if (comma_pos != -1)
    notename = substring(stripped,0,comma_pos);
  else
    notename = stripped;

  printf("stripped: -%s-, notename: -%s-\n",stripped,notename);
  
  if(strlen(notename)==2){

    chroma = get_chroma(notename[0]);
    sharp = 0;
    octave = get_octave(notename[1]);

  } else if(strlen(notename)==3){

    chroma = get_chroma(notename[0]);
    sharp = get_sharp(notename[1]);
    octave = get_octave(notename[2]);

  } else
    return -1;

  if(chroma==-1 || sharp==-2 || octave==-1)
    return -1;

  float decimals = 0.0f;

  if (comma_pos != -1) {
    const char *decimaltext = stripped + comma_pos + 1;
#ifdef TEST_NOTES
    decimals = atof(decimaltext) / 100.0f;
#else
    decimals = OS_get_double_from_string(decimaltext) / 100.0f;
#endif
    printf("decimaltext: -%s-, dec: %f\n",decimaltext,decimals);
  }
  
  float notenum = octave*12 + chroma + sharp + decimals;

  if(notenum<=0 || notenum>127.99)
    return -1;
  else
    return notenum;
}

const char *notetext_from_notenum(float notenumf){
  int notenum = notenumf;
  int cents = 0;
  
  if (!equal_floats(notenumf, floorf(notenum))){
    float decimals = notenumf - floorf(notenum);
    //printf("___ decimals: %f\n",decimals);
    cents = roundf(decimals*100.0f);
  }
  
  int octave = notenum / 12;
  int chroma = notenum - (octave*12);

  //printf("************************************************************************** octave: %d, chroma: %d, cents: %d\n",octave,chroma,cents);
  char *wholenotestring = talloc_format("%s%s",chroma_to_string(chroma), octave_to_string(octave));

  if (cents != 0)
    return talloc_format("%s,%d", wholenotestring, cents);
  else
    return wholenotestring;
}


#ifndef TEST_NOTES

static float request_notenum(struct Tracker_Windows *window, const char *title, float old_notenum){
  float notenum = -1;

  ReqType reqtype=GFX_OpenReq(window,30,12,"");

  if (old_notenum>0.0f)
    GFX_SetString(reqtype, notetext_from_notenum(old_notenum));

  char temp[1024];
  sprintf(temp, "%s (for example: \"c4\" or \"c#5,50\") >", title);
  
  while(notenum <= 0.0f){
    const char *notetext = GFX_GetString(
                                         window,
                                         reqtype,
                                         temp,
                                         true
                                   );
    if(notetext==NULL)
      break;
    
    notenum = notenum_from_notetext(notetext);
  }
  
  GFX_CloseReq(window,reqtype);

  printf("notenum: %f\n",notenum);
  
  return notenum;
}

static void r_add_pitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, r::NotePtr &note, const Ratio &r){
  float notenum = request_notenum(window, "Add pitch", -1);
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    if (AddPitch2(window, wblock, wtrack, note, r, notenum) < 0)
      UNDO_CANCEL_LAST_UNDO();
  }
}

static void r_add_last_pitch2(struct Tracker_Windows *window, r::NotePtr &note){
  float notenum = request_notenum(window, "Add last pitch", -1);
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    note->d._pitch_end = notenum;
  }
}

static void r_add_note(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, const Ratio &r){
  float notenum = request_notenum(window, "New note", -1);
  
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    Place place = ratio2place(r);
    InsertNote(wblock, wtrack, &place, NULL, notenum, NOTE_get_velocity(wtrack->track), false);
  }
}

static void r_edit_pitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, const r::NotePtr &note, int pitchnum) {
  R_ASSERT_RETURN_IF_FALSE(pitchnum >= 0);
    
  float notenum;
  {
    const r::PitchTimeData::Reader reader(&note->_pitches);
                              
    R_ASSERT_RETURN_IF_FALSE(pitchnum < reader.size());

    notenum = request_notenum(window, "Edit pitch", reader.at_ref(pitchnum)._val);
  }
  
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    
    r::PitchTimeData::Writer writer(&note->_pitches);

    R_ASSERT_RETURN_IF_FALSE(pitchnum < writer.size());

    writer.at_ref(pitchnum)._val = notenum;
  }
}

static void r_edit_end_pitch(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, const r::NotePtr &note){
  float notenum = request_notenum(window, "Edit last pitch", note->d._pitch_end);
  
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    note->d._pitch_end = notenum; // lock not necessary
  }
}

static void r_edit_note(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, const r::NotePtr &note){
  float notenum = request_notenum(window, "Edit note", note->get_val());
  
  if(notenum > 0.0f){
    ADD_UNDO(Notes_CurrPos(window));
    note->_val = notenum; // lock not necessary
  }
}


void EditNoteCurrPos(struct Tracker_Windows *window){
  struct WBlocks       *wblock        = window->wblock;
  struct WTracks       *wtrack        = wblock->wtrack;    
  int                   curr_realline = wblock->curr_realline;
  
  const Trs &trs = TRS_get(wblock, wtrack,curr_realline);

  if (trs.size()==0) {

    const struct LocalZooms *realline = wblock->reallines[curr_realline];
    /*
    const Place             *p        = &realline->l.p;
    struct Notes            *note     = FindNote(wtrack->track, p);
    */
    const Ratio &r = place2ratio(realline->l.p);
    r::NotePtr note = FindNote2(wtrack->track, r);
                                 
    if (note) {
      if (is_at_last_line_of_note(wblock, note, wblock->curr_realline))
        r_add_last_pitch2(window, note);
      else        
        r_add_pitch2(window, wblock, wtrack, note, r);
    } else
      r_add_note(window, wblock, wtrack, r);
      
  } else {

    const TrackRealline2 &tr2 = trs[0];

    switch(tr2.type){
      case TR2_NOTE_START:
        r_edit_note(window, wblock, wtrack, tr2.note);
        break;
      case TR2_NOTE_END:
        r_edit_end_pitch(window, wblock, wtrack, tr2.note);
        break;
      case TR2_PITCH:
        r_edit_pitch(window, wblock, wtrack, tr2.note, tr2.pitchnum);
        break;
      case TR2_STOP:
        break;
    }
  }
  
}



  /******************************/
 /* Not General RETURN anymore */
/******************************/

void CutNoteAt(const struct Blocks *block, const struct Tracks *track,struct Notes *note, const Place *place){

  //R_ASSERT(PLAYER_current_thread_has_lock() || is_playing()==false);

  Place p = ratio2place(note->end);
  
  if (PlaceGreaterOrEqual(place, &p)){
    RError("Illegal argument for CutNoteAt 1. %f >= %f\n",GetfloatFromPlacement(place),GetfloatFromPlacement(&p));
    return;
  }
  
  if (PlaceLessOrEqual(place, &note->l.p)){
    RError("Illegal argument for CutNoteAt 2. %f <= %f\n",GetfloatFromPlacement(place),GetfloatFromPlacement(&note->l.p));
    return;
  }
  
  {
    r::VelocityTimeData::Writer writer(note->_velocities);
    writer.remove_everything_after(ratio_from_place(*place), true);
  }

  {
    r::PitchTimeData::Writer writer(note->_pitches);
    writer.remove_everything_after(ratio_from_place(*place), true);
  }

  note->end = place2ratio(*place);
}

void CutNoteAt2(const struct Blocks *block, const struct Tracks *track, r::ModifyNote &note, const Ratio &ratio){

  if (ratio >= note->d._end){
    RError("Illegal argument for CutNoteAt 1. %f >= %f\n",make_double_from_ratio(ratio), make_double_from_ratio(note->d._end));
    return;
  }
  
  if (ratio <= note->get_time()){
    RError("Illegal argument for CutNoteAt 2. %f <= %f\n",make_double_from_ratio(ratio), make_double_from_ratio(note->get_time()));
    return;
  }
  
  {
    r::VelocityTimeData::Writer writer(&note->_velocities);
    writer.remove_everything_after(ratio, true);
  }

  {
    r::PitchTimeData::Writer writer(&note->_pitches);
    writer.remove_everything_after(ratio, true);
  }

  note->d._end = ratio;
}

void StopVelocityCurrPos(struct Tracker_Windows *window,int noend){
	struct WBlocks *wblock;
	struct WTracks *wtrack;
	int reallinerealline;
	const struct LocalZooms *realline;
	int subtrack;

	wblock=window->wblock;
	wtrack=wblock->wtrack;
	reallinerealline=wblock->curr_realline;
	realline=wblock->reallines[reallinerealline];
	subtrack=window->curr_track_sub;

        const Ratio ratio = place2ratio(realline->l.p);
        
        r::NotePtr read_note = FindNoteOnSubTrack2(wtrack,subtrack,ratio);
	if(!read_note)
          return;

        ADD_UNDO(Notes_CurrPos(window));

        r::NoteTimeData::Writer writer(wtrack->track->_notes2);
        
        r::ModifyNote note(writer, read_note);
        
        {
          
          if(note->get_time() >= ratio) {
            
            RemoveNote2(wblock->block,wtrack->track,writer,note.get_noteptr());
            ValidateCursorPos(window);
            
          }else{
            
            CutNoteAt2(wblock->block, wtrack->track, note, ratio);
            
          }

          note->d._noend = noend;
          
        }
        
        window->must_redraw=true;
}



/*********** Some testing ************************/

#if !defined(RELEASE)

namespace r{
int g_num_allocated_notes = 0;
  void debug_note_added(const r::Note *note, const char *where){
    static int s_counter = 0;
    g_num_allocated_notes++;

    s_counter++;
      
    if (!(s_counter%1024))
      printf("   (ADD: note %p / %d from \"%s\". Size now: %d)\n", note, (int)note->_id, where, g_num_allocated_notes);
  }

  void debug_note_removed(const r::Note *note){
    g_num_allocated_notes--;
    //printf("   (DEC: note %p / %d. Size now: %d)\n", note, (int)note->_id, g_num_allocated_notes);
  }
}

static void test(struct Blocks *block,
                 struct Tracks *track)
{
  return;
  
  static bool s_has_tested = false;
  if (s_has_tested)
    return;

  s_has_tested = true;

  //R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 0);
  
  printf("-1. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
  
  {
    r::NotePtr note = NewNote2(make_ratio(0,1), 64, 100);

    R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 1);
    printf("0. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
  }

  R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 0);
  printf("1. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);

  {
    r::NoteTimeData::Writer writer(track->_notes2);
    
    printf("2. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
    
        
    r::NotePtr note = NewNote2(make_ratio(0,1), 64, 100);
    note->d._end = make_ratio(1,1);

    R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 1);
    
    printf("3. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
    
    writer.add(note);

    printf("4. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
    
    r::ModifyNote(writer, note);

    R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 2);
    printf("5. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
  }

  R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 1);
  printf("6. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);

  {
    r::NoteTimeData::Writer writer(track->_notes2);
    writer.clear();
  }

  R_ASSERT_NON_RELEASE(r::g_num_allocated_notes == 0);
  printf("7. g_num_allocated_notes: %d\n", r::g_num_allocated_notes);
  
  //getchar();
}



#endif // !defined(RELEASE)


/*********** RT playing notes ************************/



namespace{
  class VelocityIterateCallback : public r::IterateCallback<int> {

    struct Patch *_patch;
    struct Notes *_note;
    
    static_assert(std::is_same<int, typeof(_note->velocity)>::value, "wrong template type for r::IterateCallback");

  public:
    
    VelocityIterateCallback(struct Patch *patch, struct Notes *note)
      : _patch(patch)
      , _note(note)
    {}

    void callback(struct SeqTrack *seqtrack,
                  const struct SeqBlock *seqblock,
                  const struct Tracks *track,
                  int index,
                  int val,
                  int64_t time,
                  FX_when when,
                  bool is_new_node
                  ) const override
    {
      _note->curr_velocity = TRACK_get_velocity(track, val);
      _note->curr_velocity_time = time;
      
      //printf("Velocity: %f. Time: %d\n", _note->curr_velocity, (int)time);

      _note->has_sent_seqblock_volume_automation_this_block = true;

      RT_PATCH_change_velocity(seqtrack,
                               _patch,
                               create_note_t(seqblock,
                                             _note->id,
                                             _note->note,
                                             _note->curr_velocity * seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted,
                                             0,
                                             ATOMIC_GET(track->midi_channel),
                                             0,
                                             0
                                             ),
                               time
                               );
    }

  };

  class VelocityIterateCallback2 : public r::IterateCallback<int> {

    struct Patch *_patch;
    r::Note *_note;

    static_assert(std::is_same<int, typeof(_note->d._velocity)>::value, "wrong template type for r::VelocityIterateCallback2");

  public:
    
    VelocityIterateCallback2(struct Patch *patch, r::Note *note)
      : _patch(patch)
      , _note(note)
    {}

    void callback(struct SeqTrack *seqtrack,
                  const struct SeqBlock *seqblock,
                  const struct Tracks *track,
                  int index,
                  int val,
                  int64_t time,
                  FX_when when,
                  bool is_new_node
                  ) const override
    {
      _note->d._curr_velocity = TRACK_get_velocity(track, val);
      _note->d._curr_velocity_time = time;
      
      //printf("Velocity: %f. Time: %d\n", _note->curr_velocity, (int)time);

      _note->d._has_sent_seqblock_volume_automation_this_block = true;

      RT_PATCH_change_velocity(seqtrack,
                               _patch,
                               create_note_t(seqblock,
                                             _note->_id,
                                             _note->get_val(),
                                             _note->d._curr_velocity * seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted,
                                             0,
                                             ATOMIC_GET(track->midi_channel),
                                             0,
                                             0
                                             ),
                               time
                               );
    }

  };

  class PitchIterateCallback : public r::IterateCallback<float> {

    struct Patch *_patch;
    struct Notes *_note;
    const r::PitchTimeData::Reader &_reader;
    r::PitchSeqBlock *_pitch_seqblock;
    
    static_assert(std::is_same<float, typeof(_note->note)>::value, "wrong template type for r::IterateCallback");

  public:
    
    PitchIterateCallback(struct Patch *patch, struct Notes *note, const r::PitchTimeData::Reader &reader)
      : _patch(patch)
      , _note(note)
      , _reader(reader)
      , _pitch_seqblock(reader.get_player_cache())
    {}

    int rnd(int max) const {
      return std::rand() % max;
    }

    bool get_enabled(const r::Pitch &pitch) const {
      bool enabled;

      if (pitch._chance==0){
        
        enabled = _patch->last_chance_decision_value;
        
      } else {
        
        if (pitch._chance==MAX_PATCHVOICE_CHANCE)
          enabled = true;
        else if (pitch._chance > rnd(MAX_PATCHVOICE_CHANCE))
          enabled = true;
        else
          enabled = false;
        
        _patch->last_chance_decision_value = enabled;
        
      }
      
      return enabled;
    }

    void callback(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct Tracks *track, int index, float val, int64_t time, FX_when when, bool is_new_node) const override {

      R_ASSERT_NON_RELEASE(_pitch_seqblock != NULL);
      
      if (_pitch_seqblock != NULL) {
        
        if (is_new_node && index >= 0)
          _pitch_seqblock->_enabled = get_enabled(_reader.at_ref(index));
        
        if (!_pitch_seqblock->_enabled)
          return;        
      }

      //printf("Setting new pitch. Old: %f. New: %f\n", _note->curr_pitch, val);
      _note->curr_pitch = val;
      _note->curr_pitch_time = time;

      /*
      printf("Pitch: %s. Cents: %d. Time: %d. When: \"%s\". Is new node: %d. _pitch_seqblock: %p. Index: %d\n",
             get_notename(NotesTexts3, val),
             (int)R_BOUNDARIES(0,round((val - (int)val)*100.0),99),
             (int)time,
             get_FX_when_name(when),
             is_new_node,
             _pitch_seqblock,
             index);
      */
      
      RT_PATCH_change_pitch(seqtrack,
                            _patch,
                            create_note_t2(seqblock, _note->id, _note->curr_pitch),
                            time);
    }

  };

  class PitchIterateCallback2 : public r::IterateCallback<float> {

    struct Patch *_patch;
    r::Note *_note;
    const r::PitchTimeData::Reader &_reader;
    r::PitchSeqBlock *_pitch_seqblock;
    
    static_assert(std::is_same<float, typeof(_note->_val)>::value, "wrong template type for r::IterateCallback2");

  public:
    
    PitchIterateCallback2(struct Patch *patch, r::Note *note, const r::PitchTimeData::Reader &reader)
      : _patch(patch)
      , _note(note)
      , _reader(reader)
      , _pitch_seqblock(reader.get_player_cache())
    {}

    int rnd(int max) const {
      return std::rand() % max;
    }

    bool get_enabled(const r::Pitch &pitch) const {
      bool enabled;

      if (pitch._chance==0){
        
        enabled = _patch->last_chance_decision_value;
        
      } else {
        
        if (pitch._chance==MAX_PATCHVOICE_CHANCE)
          enabled = true;
        else if (pitch._chance > rnd(MAX_PATCHVOICE_CHANCE))
          enabled = true;
        else
          enabled = false;
        
        _patch->last_chance_decision_value = enabled;
        
      }
      
      return enabled;
    }

    void callback(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const struct Tracks *track, int index, float val, int64_t time, FX_when when, bool is_new_node) const override {

      R_ASSERT_NON_RELEASE(_pitch_seqblock != NULL);
      
      if (_pitch_seqblock != NULL) {
        
        if (is_new_node && index >= 0)
          _pitch_seqblock->_enabled = get_enabled(_reader.at_ref(index));
        
        if (!_pitch_seqblock->_enabled)
          return;        
      }
      
      _note->d._curr_pitch = val;
      _note->d._curr_pitch_time = time;

      /*
      printf("Pitch: %s. Cents: %d. Time: %d. When: \"%s\". Is new node: %d. _pitch_seqblock: %p. Index: %d\n",
             get_notename(NotesTexts3, val),
             (int)R_BOUNDARIES(0,round((val - (int)val)*100.0),99),
             (int)time,
             get_FX_when_name(when),
             is_new_node,
             _pitch_seqblock,
             index);
      */
      
      RT_PATCH_change_pitch(seqtrack,
                            _patch,
                            create_note_t2(seqblock, _note->_id, _note->d._curr_pitch),
                            time);
    }

  };
}

template <class ValType, class RT_CacheHandler>
static void call_callback_between_nodes(struct SeqTrack *seqtrack,
                                        const struct SeqBlock *seqblock,
                                        const struct Tracks *track,
                                        
                                        const int64_t seqtime_start,
                                        const Ratio &ratio, // ratio for seqtime_start
                                        
                                        const r::IterateCallback<ValType> &callback,
                                        
                                        const r::TimeDataSimpleNode<ValType> &n1,
                                        const r::TimeDataSimpleNode<ValType> &n2,

                                        RT_CacheHandler &cache
                                        )
{
  ValType value;

  if (cache.is_same_value(n1._val, n2._val) || n1._logtype==LOGTYPE_HOLD) { // Probably the most common situation.

    R_ASSERT_NON_RELEASE(ratio >= n1._time);
    R_ASSERT_NON_RELEASE(ratio <= n2._time);
    
    value = n1._val;
    
  } else if (ratio <= n1._time) {

    R_ASSERT_NON_RELEASE(ratio == n1._time);

    value = n1._val;

  } else if (ratio >= n2._time) {

    R_ASSERT_NON_RELEASE(ratio == n2._time);

    value = n2._val;
    
  } else if (n1._time==n2._time){
    
    R_ASSERT_NON_RELEASE(false);
    
    value = n1._val;
    
  } else {

    value = scale_double(make_double_from_ratio(ratio),
                         make_double_from_ratio(n1._time), make_double_from_ratio(n2._time),
                         n1._val, n2._val);
    
  }

  if (!cache.is_same_value(value)) {
    //printf("Has: %d. is_same: %d. Value: %d. Cached value: %d\n", cache.has_value(), cache.has_value() && cache.is_same_value(value, cache.get_value()), value, cache.get_value());
    callback.callback(seqtrack, seqblock, track, -1, value, seqtime_start, FX_middle, false); // Note: FX_When is currently ignored by all these callbacks.
    cache.update_value(value);
  }
}


template <class T, class SeqBlockT>
template <typename TimeData, typename TimeDataVector>
template <typename ValType>
void r::TimeData<T,SeqBlockT>::ReaderWriter<TimeData, TimeDataVector>::iterate_extended(struct SeqTrack *seqtrack,
                                                                                        const struct SeqBlock *seqblock,
                                                                                        const struct Tracks *track,
                                                                                        int play_id,
                                                                                        
                                                                                        const int64_t seqtime_start,
                                                                                        const r::RatioPeriod &track_period, // Note: track_period._start corresponds to seqtime_start
                                                                                        
                                                                                        const r::IterateCallback<ValType> &callback,
                                                                                        
                                                                                        const r::TimeDataSimpleNode<ValType> &node_start,
                                                                                        const r::TimeDataSimpleNode<ValType> &node_end
                                                                                        ) const
{

  if (track_period._start < node_start._time) {
    
    // Commented out. For now (before notes are converted to TimeData), this can happen if moving start position of note while playing
    //R_ASSERT_NON_RELEASE(false);
    return;
  }
  if (track_period._start > node_end._time) {

    // Commented out. For now (before notes are converted to TimeData), this can happen if moving end position of note while playing
    //R_ASSERT_NON_RELEASE(false);
    return;
  }

  /*
  printf("Velocities. period start/end: %d/%d - %d/%d.\n",
         (int)track_period._start.num, (int)track_period._start.den,
         (int)track_period._end.num, (int)track_period._end.den
         );
  */
  
  RT_CacheHandler cache(get_player_cache(), play_id);

  const int das_size = size();
  
  if (das_size==0) {
    
    call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                node_start,
                                node_end,
                                cache);

    return;
  }
 
  {
    const T &first_t = _vector->at_first();
    
    if (track_period._start < first_t._time) {
      
      call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                  node_start,
                                  r::TimeDataSimpleNode<ValType>(first_t._time, first_t._val),
                                  cache);
      
      if (das_size==1 && track_period._end >= first_t._time) {

        int64_t node_time = get_seqblock_ratio_time2(seqblock, track, first_t._time);
        callback.callback(seqtrack, seqblock, track, 0, first_t._val, node_time, FX_single, true);

        cache.update_value(first_t._val);

      }
      
    }
  }

  if (das_size > 1)
    iterate(seqtrack, seqblock, track, play_id, seqtime_start, track_period, callback);
  
  {
    const T &last_t = _vector->at_last();
    
    if (track_period._start > last_t._time) {
      
      call_callback_between_nodes(seqtrack, seqblock, track, seqtime_start, track_period._start, callback,
                                  r::TimeDataSimpleNode<ValType>(last_t._time, last_t._val, last_t._logtype),
                                  node_end,
                                  cache
                                  );
      
    }

  }

}


static void RT_VELOCITIES_called_each_block_for_each_note(struct SeqTrack *seqtrack,
                                                          const int play_id,
                                                          const struct SeqBlock *seqblock,
                                                          const struct Tracks *track,
                                                          const int64_t seqtime_start,
                                                          const r::RatioPeriod &track_period,
                                                          struct Patch *patch,
                                                          struct Notes *note
                                                          )
{

  VelocityIterateCallback callback(patch, note);

  const r::VelocityTimeData::Reader reader(note->_velocities, seqblock->cache_num);
      
  reader.iterate_extended(seqtrack,
                          seqblock,
                          track,
                          play_id,
                          seqtime_start,
                          track_period,
                          callback,
                          r::TimeDataSimpleNode<typeof(note->velocity)>(place2ratio(note->l.p), note->velocity, note->velocity_first_logtype),
                          r::TimeDataSimpleNode<typeof(note->velocity)>(note->end, note->velocity_end)
                          );
}

static void RT_VELOCITIES_called_each_block_for_each_note2(struct SeqTrack *seqtrack,
                                                           const int play_id,
                                                           const struct SeqBlock *seqblock,
                                                           const struct Tracks *track,
                                                           const int64_t seqtime_start,
                                                           const r::RatioPeriod &track_period,
                                                           struct Patch *patch,
                                                           struct r::Note *note
                                                           )
{

  VelocityIterateCallback2 callback(patch, note);

  const r::VelocityTimeData::Reader reader(&note->_velocities, seqblock->cache_num);
      
  reader.iterate_extended(seqtrack,
                          seqblock,
                          track,
                          play_id,
                          seqtime_start,
                          track_period,
                          callback,
                          r::TimeDataSimpleNode<typeof(note->d._velocity)>(note->get_time(), note->d._velocity, note->d._velocity_first_logtype),
                          r::TimeDataSimpleNode<typeof(note->d._velocity)>(note->d._end, note->d._velocity_end)
                          );
}



static void RT_PITCHES_called_each_block_for_each_note(struct SeqTrack *seqtrack,
                                                       const int play_id,
                                                       const struct SeqBlock *seqblock,
                                                       const struct Tracks *track,
                                                       const int64_t seqtime_start,
                                                       const r::RatioPeriod &track_period,
                                                       struct Patch *patch,
                                                       struct Notes *note
                                                       )
{
  
  const r::PitchTimeData::Reader reader(note->_pitches, seqblock->cache_num);

  PitchIterateCallback callback(patch, note, reader);

  int first_logtype;
  if (reader.size()==0 && equal_floats(note->pitch_end, 0.0))
    first_logtype = LOGTYPE_HOLD;
  else
    first_logtype = note->pitch_first_logtype;

#if 0
  printf("first logtype: %d\n", first_logtype);
  if (reader.size() > 0){
    printf("second logtype: %d\n", reader.at_ref(0)._logtype);
  }
#endif
  
  reader.iterate_extended(seqtrack,
                          seqblock,
                          track,
                          play_id,
                          seqtime_start,
                          track_period,
                          callback,
                          r::TimeDataSimpleNode<typeof(note->note)>(place2ratio(note->l.p), note->note, first_logtype),
                          r::TimeDataSimpleNode<typeof(note->note)>(note->end, note->pitch_end)
                          );
}

static void RT_PITCHES_called_each_block_for_each_note2(struct SeqTrack *seqtrack,
                                                        const int play_id,
                                                        const struct SeqBlock *seqblock,
                                                        const struct Tracks *track,
                                                        const int64_t seqtime_start,
                                                        const r::RatioPeriod &track_period,
                                                        struct Patch *patch,
                                                        struct r::Note *note
                                                        )
{
  
  const r::PitchTimeData::Reader reader(&note->_pitches, seqblock->cache_num);

  PitchIterateCallback2 callback(patch, note, reader);

  int first_logtype;
  if (reader.size()==0 && equal_floats(note->d._pitch_end, 0.0))
    first_logtype = LOGTYPE_HOLD;
  else
    first_logtype = note->d._pitch_first_logtype;

#if 0
  printf("first logtype: %d\n", first_logtype);
  if (reader.size() > 0){
    printf("second logtype: %d\n", reader.at_ref(0)._logtype);
  }
#endif
  
  reader.iterate_extended(seqtrack,
                          seqblock,
                          track,
                          play_id,
                          seqtime_start,
                          track_period,
                          callback,
                          r::TimeDataSimpleNode<typeof(note->_val)>(note->get_time(), note->get_val(), first_logtype),
                          r::TimeDataSimpleNode<typeof(note->_val)>(note->d._end, note->d._pitch_end)
                          );
}

static void RT_SEQTRACK_reserve_hanging_notes_tracks(struct SeqTrack *seqtrack,
                                                     int num_tracks)
{
  for(int i=seqtrack->hanging_notes->size() ; i < num_tracks ; i++){
    //void *mem = malloc(sizeof(radium::RT_NoteVector)); //
    void *mem = RT_alloc_raw(sizeof(radium::RT_HangingNoteVector), "RT_SEQTRACK_reserve_hanging_notes_tracks");
    seqtrack->hanging_notes->push_back(new(mem) radium::RT_HangingNoteVector);
  }
}


static void RT_SEQTRACK_add_hanging_note(struct SeqTrack *seqtrack,
                                         const struct SeqBlock *seqblock,
                                         const struct Tracks *track,
                                         struct Patch *patch,
                                         r::Note *note)
{
  int tracknum = track->l.num;

  RT_SEQTRACK_reserve_hanging_notes_tracks(seqtrack, tracknum + 1);
  
  auto *pn = seqtrack->hanging_notes->at_ref(track->l.num);
      
  pn->push_back(radium::HangingNote({r::NotePtr(note), // Note: the reference counter variable is placed in r::Note, not in r::NotePtr.
                                     patch,
                                     seqblock,
                                     ATOMIC_GET(track->midi_channel)}));
}

static bool note_continues_next_seqblock(const struct SeqBlock *seqblock, const r::Note *note){
  if (note->d._noend==0)
    return false;
  
  const struct Blocks *block = seqblock->block;

  if (p_Equal(seqblock->t.end_place, p_Absolute_Last_Pos(block)))
    return note_continues_next_block2(block, note);
      
  return note->get_time() > place2ratio(seqblock->t.end_place);
}


static int rnd(int max){
  return rand() % max;
}

static void RT_start_note(struct SeqTrack *seqtrack,
                          const struct SeqBlock *seqblock,
                          const struct Tracks *track,
                          r::Note *note,
                          int64_t note_time,
                          int64_t sample_pos
                          )
{
  R_ASSERT_NON_RELEASE(sample_pos >= 0);
  
  struct Patch *patch = track->patch;

  bool doit;  // Set this here, and not in RT_schedule_note, since note->chance might change between RT_schedule_note and RT_scheduled_note.

  if (patch==NULL){
    
    doit = true;

  } else if (note->d._chance==0){

    doit = patch->last_chance_decision_value;
    //printf("   track: %d. Using last decision %d\n", track->l.num, doit);

  } else {

    if (note->d._chance==MAX_PATCHVOICE_CHANCE)
      doit = true;
    else if (note->d._chance > rnd(MAX_PATCHVOICE_CHANCE))
      doit = true;
    else
      doit = false;

    patch->last_chance_decision_value = doit;
    //printf("   track: %d. Setting last decision to %d\n", track->l.num, doit);
  }

  if(doit && track->onoff==1 && patch!=NULL){

    // TODO/FIX: Check that this one is correct.
    if (pc->playtype != PLAYSONG && sample_pos != 0){
      double reltempo = ATOMIC_DOUBLE_GET(seqblock->block->reltempo);
      if (fabs(reltempo-1.0) > 0.00001){
        double new_sample_pos = sample_pos;
        new_sample_pos /= reltempo;
        sample_pos = new_sample_pos;
      }
    }
    
    note->d._curr_velocity = TRACK_get_velocity(track,note->d._velocity); // The logical behavior would be to use note->velocity, but we don't have access to track in the function 'RT_PATCH_voice_volume_has_changed.
    note->d._curr_velocity_time = note_time;

    note->d._curr_pitch = note->get_val();
    note->d._curr_pitch_time = note_time;

    note->d._scheduler_may_send_velocity_next_block = false;
    note->d._scheduler_may_send_pitch_next_block = false;

    //printf("   Vol: %f. Sample_pos: %d\n", note->d._curr_velocity, (int)sample_pos);
    note_t note2 = create_note_t(seqblock,
                                 note->_id,
                                 note->d._curr_pitch,
                                 note->d._curr_velocity,
                                 TRACK_get_pan(track),
                                 ATOMIC_GET(track->midi_channel),
                                 0,
                                 sample_pos
                                 );

    // Note: envelope volume is applied in RT_PATCH_play_note, not here. (Not quite sure why, but it's probably complicated)

    note->d._has_sent_seqblock_volume_automation_this_block = true;
    
    RT_PATCH_play_note2(seqtrack, patch, note2, note_time);
  }
}


static vector_t g_RT_audio_patches;
static vector_t g_RT_midi_patches;

void RT_NOTES_called_before_scheduler(void){
  
  // Note: RT-lock is held when changing Instrument::patches->elements.
  
  g_RT_audio_patches = get_audio_instrument()->patches;
  g_RT_midi_patches = get_MIDI_instrument()->patches;
}

#if 0
// not important.
void RT_prepare_patches_after_rt_usage(void){
  memset(&g_RT_audio_patches, 0, sizeof(vector_t));
  memset(&g_RT_midi_patches, 0, sizeof(vector_t));
}
#endif

static void RT_stop_all_hanging_notes_for_track(struct SeqTrack *seqtrack,
                                                const struct Tracks *track,
                                                int64_t stop_time)
{
  R_ASSERT_RETURN_IF_FALSE(seqtrack->hanging_notes != NULL);

  // 1. memcpy audio/midi_instrument->patches->elements to global area, where GC can find them.
  //    (we only have to do this once in the start of the RT-block)
  // 2. iterate those instead, it won't disappear.
  // 3. clean that area after usage (end of RT-block).
  
  int tracknum = track->l.num;

  if (tracknum >= seqtrack->hanging_notes->size())
    return; // The seqblock->playing_notes vector is not expanded to cover all tracks unless there are playing notes on all tracks.

  radium::RT_HangingNoteVector *hanging_notes = seqtrack->hanging_notes->at_ref(track->l.num);

  if (hanging_notes == NULL){
    R_ASSERT(false);
    return;
  }
  
  for(const radium::HangingNote &hanging_note : *hanging_notes) {
    r::Note *note = hanging_note.note.get_mutable();

    // Check that seqblock is still alive
    //
    if (&g_block_seqtrack_seqblock == hanging_note.seqblock)
      goto got_seqblock;
    
    VECTOR_FOR_EACH(const struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock == hanging_note.seqblock)
        goto got_seqblock;
    }END_VECTOR_FOR_EACH;

    //R_ASSERT_NON_RELEASE(false); // don't think it should happen actually...
    
    continue;
    
  got_seqblock:

    // Check that patch is still alive (can we iterate patches in a non-main-thread? Doesn't seem like we can...)
    // Must find out when an instrument is deleted.
    //
    VECTOR_FOR_EACH(struct Patch *, maybe, &g_RT_audio_patches){
      if (maybe == hanging_note.patch)
        goto got_patch;
    }END_VECTOR_FOR_EACH;
    
    VECTOR_FOR_EACH(struct Patch *, maybe, &g_RT_midi_patches){
      if (maybe == hanging_note.patch)
        goto got_patch;
    }END_VECTOR_FOR_EACH;

    R_ASSERT_NON_RELEASE(false); // don't think it should happen actually...
    
    continue;
    
  got_patch:

    R_ASSERT(hanging_note.patch != NULL);

    if (hanging_note.patch != NULL) {
      note_t note2 = create_note_t3(hanging_note.seqblock,
                                    note->_id,
                                    note->get_val(),
                                    hanging_note.midi_channel
                                    );
      
      note->d._curr_velocity_time = 0;
      note->d._curr_pitch_time = 0;
      
      printf("     --Hang-Stopping note: %f\n", note->get_val());
      RT_PATCH_stop_note(seqtrack, hanging_note.patch, note2, stop_time);
    }else{
      R_ASSERT(false);
    }
  }


  hanging_notes->clear();  
}

static void handle2(struct SeqTrack *seqtrack,
                    const int play_id,
                    const struct SeqBlock *seqblock,
                    const struct Tracks *track,
                    const int64_t seqtime_start,
                    const int64_t seqtime_end,
                    const r::RatioPeriod &period,
                    bool is_enabled
                    )
{
  R_ASSERT_NON_RELEASE(period._end >= period._start);
    
  struct Patch *patch = track->patch;
  if (patch==NULL)
    is_enabled = false;

  // Check if we can exit early if track is disabled and there are no hanging notes.
  if (!is_enabled) {

    if (track->l.num >= seqtrack->hanging_notes->size())
      return;

    radium::RT_HangingNoteVector *hanging_notes = seqtrack->hanging_notes->at_ref(track->l.num);

    if (hanging_notes == NULL){
      R_ASSERT(false);
      return;
    }

    if (hanging_notes->is_empty())
      return;
  }
  
  const int line_start = period._start.num / period._start.den;
  const int line_end = period._end.num / period._end.den;

  const bool period_spans_only_one_line = line_end==line_start;

  //printf("--------\n");
  
  for(int line = line_start ; line <= line_end ; line++){
    
    printf("line: %d. Num valid: %d\n", line, track->_notes2->_num_valid_elements_in_line_notes);
    
    if (line >= track->_notes2->_num_valid_elements_in_line_notes)
      break;

    r::LineNotes *notes = track->_notes2->_line_notes.at_ref(line);

    /*
    int num_notes = 0;
    if (notes != NULL)
      for(r::Note *note : *notes){
        (void)note;
        num_notes++;
      }

    if (num_notes > 0)
      printf("Num notes at line %d: %d. \n", line, num_notes);
    */
    
    if (notes != NULL)
      for(r::Note *note : *notes){

        bool note_has_started_or_ended = false;
        
#if 0
        printf("note: %f -> %f. Period: %f -> %f. is_inside: %d. only_one_line: %d. other_thing: %d. Id: %d.\n",
               ratio2double(note->get_time()), ratio2double(note->d._end),
               ratio2double(period._start), ratio2double(period._end),
               period.is_inside(note->get_time()), period_spans_only_one_line, r::RatioPeriod(line, line+1).is_inside(note->get_time()),
               (int)note->_id);
#endif

        //
        // 1. START NOTE
        //
        if (period.is_inside(note->get_time())
            && (period_spans_only_one_line || r::RatioPeriod(line, line+1).is_inside(note->get_time()))) { // The check for period_spans_only_one_line is just an optimization.

          note_has_started_or_ended = true;
          
          int64_t time = get_seqblock_ratio_time2(seqblock, track, note->get_time());
          
          RT_stop_all_hanging_notes_for_track(seqtrack, track, time);

          printf("  --Starting note: %f (id: %d). Line: %d. Note start: %f. Period: %s. Time: %d. Seqtime: %d -> %d\n",
                 note->get_val(),
                 (int)note->_id,
                 line, ratio2double(note->get_time()),
                 period.to_string(),
                 (int)time,
                 (int)seqtime_start, (int)seqtime_end);

          if (is_enabled)
            RT_start_note(seqtrack, seqblock, track, note, time, 0);
        }

        
        if (!is_enabled)
          continue;

        
        //
        // 2. Send velocity and pitch messages.
        // 
        
        // Note: 'RT_VELOCITIES_called_each_block_for_each_note2' and 'RT_PITCHES_called_each_block_for_each_note2'
        // never sends out velocity or pitch messages before a note has started or after a note has ended.
        // In other words: we don't have to ensure that the 'time' argument is valid for these two functions.
        //
        RT_VELOCITIES_called_each_block_for_each_note2(seqtrack, play_id, seqblock, track, seqtime_start, period, patch, note);
        RT_PITCHES_called_each_block_for_each_note2(seqtrack, play_id, seqblock, track, seqtime_start, period, patch, note);



        //
        // 3. Stop note.
        //

        if (period.is_inside_inclusive(note->d._end) // Fix: only inclusive if note->d._end is at block end.
            && (period_spans_only_one_line || r::RatioPeriod(line, line+1).is_inside(note->d._end))) { // The check for period_spans_only_one_line is just an optimization.

          note_has_started_or_ended = true;
          
          if (note_continues_next_seqblock(seqblock, note)) {
            
            RT_SEQTRACK_add_hanging_note(seqtrack, seqblock, track, patch, note);
            
          } else {
            
            const int64_t stop_time = get_seqblock_ratio_time2(seqblock, track, note->d._end);
            
            printf("     --Stopping note: %f. Line: %d. Note end: %f. Period: %s. Stop time: %d. Seqtime: %d -> %d\n",
                   note->get_val(), line, ratio2double(note->d._end), period.to_string(),
                   (int)stop_time,
                   (int)seqtime_start, (int)seqtime_end);
            
            note_t note2 = create_note_t3(seqblock,
                                          note->_id,
                                          note->get_val(),
                                          ATOMIC_GET(track->midi_channel)
                                          );
            
            note->d._curr_velocity_time = 0;
            note->d._curr_pitch_time = 0;

            RT_PATCH_stop_note(seqtrack, patch, note2, stop_time);
          }
        }
      }
  }
}

void RT_notes_called_each_block(struct SeqTrack *seqtrack,
                                const int play_id,
                                const struct SeqBlock *seqblock,
                                const struct Tracks *track,
                                const int64_t seqtime_start,
                                const int64_t seqtime_end,
                                const r::RatioPeriod &period,
                                const bool is_enabled
                                )
{
  if (true){
    handle2(seqtrack, play_id, seqblock, track, seqtime_start, seqtime_end, period, is_enabled);
    return;
  }
  
  R_ASSERT_NON_RELEASE(period._end >= period._start);

  struct Patch *patch = track->patch;
  if (patch==NULL)
    return;
  
  int tracknum = track->l.num;

  if( tracknum >= seqblock->playing_notes->size())
    return; // The seqblock->playing_notes vector is not expanded to cover all tracks unless there are playing notes on all tracks.

  const radium::RT_NoteVector &playing_notes = *seqblock->playing_notes->at_ref(tracknum);

  
#if !defined(RELEASE)
  if (tracknum==0) {
    static radium::RT_NoteVector prev_playing_notes;
    
    if (prev_playing_notes.size() != playing_notes.size())
      goto not_equal;

    for(int i = 0 ; i < playing_notes.size() ; i++)
      if (prev_playing_notes.at_ref(i) != playing_notes.at_ref(i))
        goto not_equal;

    goto equal;
    
  not_equal:

    printf("=========== Playing notes:");
    
    prev_playing_notes.clear();
    for(auto *note : playing_notes){
      printf(" %d,", (int)note->note);
      prev_playing_notes.push_back(note);
    }
    printf("\n");
  }
 equal:
#endif // !defined(RELEASE)
  

  for(struct Notes *note : playing_notes){

    //printf("Handling velocities and pitches for note %f. Track %d\n", note->note, tracknum); 
    RT_VELOCITIES_called_each_block_for_each_note(seqtrack, play_id, seqblock, track, seqtime_start, period, patch, note);
    RT_PITCHES_called_each_block_for_each_note(seqtrack, play_id, seqblock, track, seqtime_start, period, patch, note);
                                     
  }
  
  return;
}




#endif // !TEST_NOTES



#ifdef TEST_NOTES

#include <stdarg.h>
#include <assert.h>

bool g_is_starting_up = false;


extern "C"{
void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}
void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

bool THREADING_is_main_thread(void){
  return true;
}

void RWarning_internal(const char *fmt,...){
  abort();
}
void RError_internal(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}
bool PLAYER_current_thread_has_lock(void){
  return false;
}
bool THREADING_is_runner_thread(void){
  return false;
}

}

static void cmp(const char *text, float value){
  fprintf(stderr,"\n\nComparing \"%s\" against %f\n",text,value);
  
  float from_text = notenum_from_notetext(text);

  const char *from_value = notetext_from_notenum(value);
  float from_text_from_value = notenum_from_notetext(from_value);

  fprintf(stderr,"from_text: %f, from_value: \"%s\", from_text_from_value: %f (value: %f)\n",
          from_text,
          from_value,
          from_text_from_value,
          value
          );
  
  assert(fabsf(from_text-value) < 0.001);
  assert(fabsf(from_text_from_value-value) < 0.001);
}

int main(void){

  assert(notenum_from_notetext("") < 0);
  assert(notenum_from_notetext("y5") < 0);
  assert(notenum_from_notetext("cd") < 0);

  assert(notenum_from_notetext("cb0") < 0);

  cmp("c0,1",0.01);
  cmp("c0,10",0.1);
  cmp("c#0",1);
  cmp("c#0,99",1.99);
  cmp("ga",127);
  cmp("ga,99",127.99);
  cmp("g-a",127);
  cmp("g-a,99",127.99);

  cmp("C0,1",0.01);
  cmp("C0,10",0.1);
  cmp("C#0",1);
  cmp("C#0,99",1.99);
  cmp("G-A",127);
  cmp("G-A,99",127.99);

  int cents;
  for(cents=0;cents<100;cents++){
    char *a = talloc_format("c9,%d",cents);
    cmp(a,108.0 + (float)cents/100.0f);
  }

  printf("Success, no errors\n");

  return 0;
}

#endif
