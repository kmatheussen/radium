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








#include "nsmtracker.h"
#include "TimeData.hpp"
#include "FX.hpp"
#include "windows_proc.h"
#include "list_proc.h"
#include "ratio_funcs.h"
#include "vector_proc.h"
#include "reallines_proc.h"
#include "notes_proc.h"
#include "placement_proc.h"
#include "notes_legalize_proc.h"
#include "temponodes_legalize_proc.h"
#include "fxlines_legalize_proc.h"
#include "time_proc.h"
#include "visual_proc.h"
#include "undo_blocks_proc.h"
#include "undo_sequencer_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "wblocks_proc.h"
#include "block_properties_proc.h"
#include "Beats_proc.h"
#include "sequencer_proc.h"
#include "expand_proc.h"

#include "../api/api_proc.h"

#include "lines_proc.h"



extern struct Root *root;
struct Blocks *blocktobelongtoforinsertlines_notes_a_terrible_hack;

static void InsertLines_notes(
	void *tonote,
	struct ListHeader3 *l,
	int line,
	int toinsert
){
	struct Notes *note=(struct Notes *)l;
	struct Blocks *block=blocktobelongtoforinsertlines_notes_a_terrible_hack;
	Place p2;

	PlaceSetLastPos(block,&p2);
//	p2.line-=toinsert;

	//	printf("toinsert: %d, note->end.line: %d, p2->line: %d\n",toinsert,note->end.line,p2.line);
	if(ratio2place(note->end).line>=line){
//		printf("block: %d, note->end.line: %d, p2->line: %d\n",block->l.num,note->end.line,p2.line);

          if(note->end >= place2ratio(p2) && note->l.p.line<line){

			PlaceSetLastPos(block,&p2);
			note->end = place2ratio(p2);
			note->noend=1;
		}else{
                  Ratio new_end = note->end + toinsert;
                  if (new_end > make_ratio(block->num_lines, 1))
                    note->end = make_ratio(block->num_lines, 1);
                  note->end = new_end;
		}
          /*
                if(note->velocities!=NULL) // need check to avoid ubsan/asan hit
                  List_InsertLines3(&note->velocities,&note->velocities->l,line,toinsert,NULL);
          */
          
                {
                  r::VelocityTimeData::Writer writer(note->_velocities);
                  writer.insert_lines(make_ratio(line, 1), make_ratio(toinsert, 1));
                }

                {
                  r::PitchTimeData::Writer writer(note->_pitches);
                  writer.insert_lines(make_ratio(line, 1), make_ratio(toinsert, 1));
                }

                
                //if(note->pitches!=NULL) // need check to avoid ubsan/asan hit
                //  List_InsertLines3(&note->pitches,&note->pitches->l,line,toinsert,NULL);
	}
}


static void InsertLines_localzooms(
	void *tolocalzoom,
	struct ListHeader3 *l,
	int line,
	int toinsert
){
	struct LocalZooms *localzoom=(struct LocalZooms *)l;

	if(localzoom->Tline>=line && localzoom->level==0){
		localzoom->zoomline+=toinsert;
	}
	if(localzoom->uplevel!=NULL){
		List_InsertLines3(
			&localzoom->uplevel,
			&localzoom->uplevel->l,
			line,toinsert,
			InsertLines_localzooms
		);
	}
}


/* Argh. This one was difficult, I don`t think it works correctly
   in all situations yet either.
*/
static void InsertLines2(
	struct Blocks *block,
	const int line,
	int toinsert,
        radium::PlayerPauseOnlyIfNeeded &player_pause
){
	int lokke;
	struct LocalZooms *localzoom;
	struct Tracker_Windows *window=root->song->tracker_windows;
	struct WBlocks *wblock;
	struct Tracks *track=block->tracks;

	int num_lines=block->num_lines;

	if(toinsert==0)
          return;

	if(line>num_lines+toinsert){
          toinsert=line-num_lines;
	}

        if(num_lines+toinsert < 2)
          toinsert = -(num_lines - 2);
        
        if (line==num_lines-1) { // special case
          Block_Properties(block, block->num_tracks, block->num_lines + toinsert);
          return;
        }
        
	if( line<0 || line>=num_lines){
          R_ASSERT_NON_RELEASE(false);
          return;
        }

        if(num_lines+toinsert<2 || num_lines+toinsert>=MAX_UINT32)
          return;
        
        {

          player_pause.need_it();
        
          blocktobelongtoforinsertlines_notes_a_terrible_hack=block;
          
          block->num_lines=num_lines+toinsert;

          if(block->temponodes!=NULL) // need check to avoid ubsan/asan hit
            List_InsertLines3(&block->temponodes,block->temponodes->l.next,line,toinsert,NULL);
          
          LegalizeTempoNodes(block);
          
          if(block->signatures!=NULL) // need check to avoid ubsan/asan hit
            List_InsertLines3(&block->signatures,&block->signatures->l,line,toinsert,NULL);
          if(block->lpbs!=NULL) // need check to avoid ubsan/asan hit
            List_InsertLines3(&block->lpbs,&block->lpbs->l,line,toinsert,NULL);
          if(block->tempos!=NULL) // need check to avoid ubsan/asan hit
            List_InsertLines3(&block->tempos,&block->tempos->l,line,toinsert,NULL);

          TIME_block_num_lines_have_changed(block);

          while(track!=NULL){
            if(track->notes!=NULL) // need check to avoid ubsan/asan hit
              List_InsertLines3(&track->notes,&track->notes->l,line,toinsert,InsertLines_notes);
            LegalizeNotes(block,track);

            const Ratio rlines = make_ratio(line, 1);
            const Ratio rtoinsert = make_ratio(toinsert, 1);
            
            r::StopTimeData::Writer(track->stops2).insert_lines(rlines, rtoinsert);
            
            /*
            if(track->stops!=NULL) // need check to avoid ubsan/asan hit
              List_InsertLines3(&track->stops,&track->stops->l,line,toinsert,NULL);
            */

            {
              std::vector<struct FXs*> to_remove;
              
              VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
                r::FXTimeData::Writer writer(fxs->_fxnodes);
                writer.insert_lines(rlines, rtoinsert);
                
                if (LegalizeFXlines2(block->num_lines,fxs->fx,writer)==false){
                  writer.cancel();
                  to_remove.push_back(fxs);
                }
                /*
                  if(fxs->fxnodelines!=NULL) // need check to avoid ubsan/asan hit
                  List_InsertLines3(&fxs->fxnodelines,&fxs->fxnodelines->l,line,toinsert,NULL);
                */
              }END_VECTOR_FOR_EACH;
              
              for(auto *fxs : to_remove)
                VECTOR_remove(&track->fxs, fxs);
            }
            
            track=NextTrack(track);
          }

          while(window!=NULL){
            wblock=(struct WBlocks*)ListFindElement1(&window->wblocks->l,block->l.num);
            if(wblock->localzooms!=NULL) // need check to avoid ubsan/asan hit
              List_InsertLines3(
                                &wblock->localzooms,
                                &wblock->localzooms->l,
                                line,
                                toinsert,
                                InsertLines_localzooms
                                //			NULL
                                );
            for(lokke=line;lokke<line+toinsert;lokke++){
              localzoom=(struct LocalZooms*)talloc(sizeof(struct LocalZooms));
              localzoom->Tline=lokke;
              localzoom->Tdividor=1;
              localzoom->zoomline=lokke;
              ListAddElement3(&wblock->localzooms,&localzoom->l);
            }
            UpdateWBlockWidths(window,wblock);
            UpdateRealLines_dont_change_curr_realline(window, wblock);
            UpdateReallinesDependens(window,wblock);
            if(wblock->curr_realline>=wblock->num_reallines){
              wblock->curr_realline=wblock->num_reallines-1;
            }
            window=NextWindow(window);
          }
          
          blocktobelongtoforinsertlines_notes_a_terrible_hack=NULL;

        }
}

static bool has_element_at_last_line3(struct ListHeader3 *list, int last_line){
  if(list==NULL)
    return false;
  
  auto *last_node = (struct ListHeader3 *)ListLast3(list);
  if (last_node->p.line >= last_line){
    R_ASSERT_NON_RELEASE(last_node->p.line == last_line);
    return true;
  }

  return false;
}
                                      
static bool last_line_contains_something(struct WBlocks *wblock){
  struct Blocks *block = wblock->block;
  int last_line = block->num_lines-1;
  
  const Ratio rlast_line = make_ratio(block->num_lines-1, 1);
  const Ratio rnum_lines = make_ratio(block->num_lines, 1);
    
  struct TempoNodes *secondlast_temponode = (struct TempoNodes *)ListSecondLast3((struct ListHeader3*)block->temponodes);
  if (secondlast_temponode->l.p.line==last_line)
    return true;

  /*
    Commented out since last temponode is automatically set to last line, so the function will always return if last node has been edited.
  struct TempoNodes *last_temponode = (struct TempoNodes *)ListLast3((struct ListHeader3*)block->temponodes);
  if (last_temponode->reltempo != 0.0)
    return true;
  */
  
  if(has_element_at_last_line3((struct ListHeader3 *)block->signatures, last_line))
    return true;
  
  if(has_element_at_last_line3((struct ListHeader3 *)block->lpbs, last_line))
    return true;
  if(has_element_at_last_line3((struct ListHeader3 *)block->tempos, last_line))
    return true;
  if(has_element_at_last_line3((struct ListHeader3 *)block->swings, last_line))
    return true;

  struct Tracks *track = block->tracks;
  
  while(track != NULL){

    struct Notes *note = track->notes;
    while(note != NULL){
      if ((note->end.num/note->end.den)==last_line)
        return true;      
      note = NextNote(note);
    }

    if (r::StopTimeData::Reader(track->stops2).has_element_between(rlast_line, rnum_lines))
      return true;
    
    //if(has_element_at_last_line3((struct ListHeader3 *)track->stops, last_line))
    //  return true;

    if(has_element_at_last_line3((struct ListHeader3 *)track->swings, last_line))
      return true;

    VECTOR_FOR_EACH(struct FXs *, fxs, &track->fxs){
      if (r::FXTimeData::Reader(fxs->_fxnodes).has_element_between(rlast_line, rnum_lines))
        return true;
      /*
      if(has_element_at_last_line3((struct ListHeader3 *)fxs->fxnodelines, last_line))
        return true;
      */
    }END_VECTOR_FOR_EACH;

    track = NextTrack(track);
  }

  return false;
}

static void fix_curr_realline_and_exit(struct Tracker_Windows *window,
                                       struct WBlocks *wblock,
                                       int org_curr_realline)
{
  wblock->curr_realline = org_curr_realline;
  if(org_curr_realline >= wblock->num_reallines)
    wblock->curr_realline = wblock->num_reallines-1;
  
}

void InsertLines(
                 struct Blocks *block,
                 const Place place,
                 Ratio toinsert
                 )
{

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = (struct WBlocks *)ListFindElement1(&root->song->tracker_windows->wblocks->l, block->l.num);
  R_ASSERT_RETURN_IF_FALSE(wblock!=NULL);

  int org_curr_realline = wblock->curr_realline;
  

  radium::PlayerPauseOnlyIfNeeded player_pause;

  if (toinsert.den==1 && place.counter==0){
    InsertLines2(block, place.line, toinsert.num, player_pause);
    
    fix_curr_realline_and_exit(window, wblock, org_curr_realline);
    return;
  }
  
  const Ratio place_ratio = place2ratio(place);

#if 1
  int expand = place_ratio.den * toinsert.den / ratio_gcd(place_ratio.den, toinsert.den); // It's correct. It's been tested.
#else
  int expand;
  {
    // This one also works, but can create a less optimal number.
    if (place_ratio.den == toinsert.den)
      expand = place_ratio.den;
    else
      expand = place_ratio.den * toinsert.den;
  }
#endif

  if (expand==1){
    R_ASSERT_NON_RELEASE(false);
    InsertLines2(block, place.line, R_MAX(1, (double)toinsert.num / (double)toinsert.den), player_pause);
    
    fix_curr_realline_and_exit(window, wblock, org_curr_realline);
    return;
  }

  //printf("\n\n\n =================== Expand: %d ==================\n\n", expand);
  
  //struct LocalZooms *localzooms = wblock->localzooms;
  
  int org_num_lines = block->num_lines;
  
  EXPAND_Block2(window, wblock, org_num_lines*expand);

  const Ratio new_place_ratio = place_ratio * make_ratio(expand, 1);
  R_ASSERT_NON_RELEASE(new_place_ratio.den==1);
  
  const Ratio new_toinsert = toinsert * make_ratio(expand, 1);
  R_ASSERT_NON_RELEASE(new_toinsert.den==1);

  {
    InsertLines2(block, new_place_ratio.num, new_toinsert.num, player_pause);
  }

  bool added_extra_lines = false;
  int num_lines = block->num_lines;
  while( (num_lines % expand) != 0){
    num_lines++;
    added_extra_lines = true;
  }

  printf("org: %d. Num_lines: %d. expand: %d\n", org_num_lines,num_lines,expand);
  
  if (added_extra_lines){
    Block_Set_num_lines2(block, num_lines, player_pause);
  }
  
  EXPAND_Block2(window, wblock, num_lines / expand);

  if (added_extra_lines){
    if(last_line_contains_something(wblock)==false){
      Block_Set_num_lines2(block, block->num_lines-1, player_pause);
    }
  }


  fix_curr_realline_and_exit(window, wblock, org_curr_realline);
  return;

  /*
    This is just too complicated, and too much wasted time. In case, it should be done in scheme instead. But perhaps just remove localzooms from the program.
    After introducing LZ (line zoom), localzooms doesn't seem to have much purpose anymore. LZ is both simpler to understand, and faster to use, than localzooms.
    (Also, when localzooms were introduced, it didn't take much time to recompile and start radium. Now it takes around 40 seconds, so fixing things like this takes much longer time than before.)

  if(0){
    int line = place.line;
    int toinsert_num_lines = (double)toinsert.num / (double)toinsert.den;

    wblock->localzooms = localzooms;
    
    if(toinsert_num_lines>0 && localzooms!=NULL) // need this unnecessary check to avoid ubsan/asan hit
      List_InsertLines3(
                        &wblock->localzooms,
                        &wblock->localzooms->l,
                        line,
                        toinsert_num_lines,
                        InsertLines_localzooms
                        );
    
    for(int lokke=line;lokke<line+toinsert_num_lines;lokke++){
      struct LocalZooms *localzoom=(struct LocalZooms*)talloc(sizeof(struct LocalZooms));
      localzoom->Tline=lokke;
      localzoom->Tdividor=1;
      localzoom->zoomline=lokke;
      ListAddElement3(&wblock->localzooms,&localzoom->l);
    }
    
    UpdateWBlockWidths(window,wblock);
    
    UpdateRealLines_dont_change_curr_realline(window, wblock);
    
    UpdateReallinesDependens(window,wblock);
    
    if(wblock->curr_realline>=wblock->num_reallines){
      wblock->curr_realline=wblock->num_reallines-1;
    }
    
  }

  */
  
}


void InsertLines_CurrPos(
                         struct Tracker_Windows *window,
                         Ratio toinsert
                         )
{
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;
        const Place place = wblock->reallines[curr_realline]->l.p;

        if(toinsert.num==0){

          Ratio lz_ratio;
          {
            dyn_t lz = getLineZoomBlockRatio(window->wblocks->l.num, window->l.num);
            
            if (lz.type==INT_TYPE)
              lz_ratio = make_ratio(lz.int_number, 1);
            else if (lz.type==RATIO_TYPE)
              lz_ratio = *lz.ratio;
            else {
              R_ASSERT(false);
              return;
            }
          }

          //int curr_line = wblock->curr_realline;
          
          //Ratio num_lines_ratio = make_ratio(wblock->block->num_lines, 1) * lz_ratio;
          //int num_lines = ceil((double)num_lines_ratio.num / (double)num_lines_ratio.den);

	  const char *s = GFX_GetString(window,NULL,"Number of lines to insert\n(number can be negative): ",true);

          if(s==NULL || !strcmp(s,""))
            return;

          StaticRatio static_ratio = STATIC_RATIO_from_string(STRING_create(s));
          printf("Static: %d / %d\n", static_ratio.numerator, static_ratio.denominator);

          if(static_ratio.numerator==0 || static_ratio.denominator==0)
            return;
          
          Ratio ratio = make_ratio_from_static_ratio(static_ratio);
          
          printf("num_lines_toinsert: %s.\n", ratio_to_string(ratio));

          //toinsert = make_ratio(num_lines_toinsert,1) / lz_ratio;
          toinsert = ratio / lz_ratio;

          printf("toinsert: %s.\n", ratio_to_string(toinsert));
        }
        
        UNDO_OPEN();
        ADD_UNDO(Sequencer());
	ADD_UNDO(Block_CurrPos(window));
        UNDO_CLOSE();

	InsertLines(window->wblock->block,place,toinsert);

	window=root->song->tracker_windows;
	while(window!=NULL){
		window->must_redraw = true;
		window=NextWindow(window);
	}
}


