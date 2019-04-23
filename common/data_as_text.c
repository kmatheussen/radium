/* Copyright 2016 Kjetil S. Matheussen

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


#include <math.h>


#include "nsmtracker.h"
#include "veltext_proc.h"
#include "fxtext_proc.h"
#include "centtext_proc.h"
#include "chancetext_proc.h"
#include "bpmtext_proc.h"
#include "lpbtext_proc.h"
#include "swingtext_proc.h"
#include "signaturetext_proc.h"
#include "cursor_updown_proc.h"
#include "wtracks_proc.h"

#include "data_as_text_proc.h"


extern struct TEvent tevent;



static int get_val_from_key(int key){
  int val = -1;
  
  switch (key){ 
    case EVENT_DEL: val = 0; break;
    case EVENT_RETURN: val = 8; break;
    case EVENT_0: val = 0; break;
    case EVENT_1: val = 1; break;
    case EVENT_2: val = 2; break;
    case EVENT_3: val = 3; break;
    case EVENT_4: val = 4; break;
    case EVENT_5: val = 5; break;
    case EVENT_6: val = 6; break;
    case EVENT_7: val = 7; break;
    case EVENT_8: val = 8; break;
    case EVENT_9: val = 9; break;    
    case EVENT_A: val = 10; break;
    case EVENT_B: val = 11; break;
    case EVENT_C: val = 12; break;
    case EVENT_D: val = 13; break;
    case EVENT_E: val = 14; break;
    case EVENT_F: val = 15; break;
    case EVENT_G: val = 15; break;
    case EVENT_T: val = 15; break;
    case EVENT_X: val = 15; break;
    case EVENT_LR3: val = 15; break; // TODO: Investigate why this isnt working.
  }

  return val;
}

data_as_text_t DAT_get_newvalue(int subsubtrack,
                                int key,
                                int default_value,
                                int default_logtype,
                                int min_value, int max_value,
                                int min_return_value, int max_return_value,
                                bool is_hex, bool has_logtype, bool highest_value_is_one_more)
{
  data_as_text_t dat;
  dat.is_valid = false;
  
  int logtype = default_logtype; //LOGTYPE_LINEAR;
  
  /*
  if (CapsLock(tevent.keyswitch))
    logtype = LOGTYPE_HOLD;
  printf("Caps1: %d\n", CapsLock(tevent.keyswitch));
  */

  int val = get_val_from_key(key);
  if (val==-1)
    return dat;

  int base = 10;
  if (is_hex)
    base = 0x10;

  int num_subtracks = 1 + log(max_value) / log(base);

  int highest_value = pow(base, num_subtracks) - 1;  
  //int highest_value = 0xff;
  if (highest_value_is_one_more)
    highest_value++;

  int value = 0;
  
  if (key==EVENT_G){
    value = max_return_value;

  } else if (key==EVENT_T){
    value = default_value;
    logtype = LOGTYPE_HOLD;
  
  }else if (subsubtrack == 0) {
    value = round(scale_double(val * pow(base, num_subtracks-1), min_return_value, highest_value, min_return_value, max_return_value));
    
  } else if (subsubtrack == 1 && num_subtracks > 1) {
    value = round(scale_double(val * pow(base, num_subtracks-2), min_return_value, highest_value, min_return_value, max_return_value));
    
  } else if (subsubtrack == 2 && num_subtracks > 2) {
    value = round(scale_double(val * pow(base, num_subtracks-3), min_return_value, highest_value, min_return_value, max_return_value));
    
  } else if (subsubtrack == num_subtracks) {
    value = default_value;
    logtype=LOGTYPE_HOLD;
    
  } else {
    RError("Unknown subsubtrack: %d",subsubtrack);
    return dat;
  }

  if (value > max_return_value)
    value = max_return_value;
  if (value < min_return_value)
    value = min_return_value;
  
  dat.value = value;
  dat.logtype = logtype;
  dat.is_valid = true;
  
  return dat;
}

data_as_text_t DAT_get_overwrite(int old_value, int logtype, int subsubtrack, int key, int min_value, int max_value, int min_return_value, int max_return_value, bool is_hex, bool highest_value_is_one_more){

  /*
  if (CapsLock(tevent.keyswitch))
    logtype = LOGTYPE_HOLD;
  printf("Caps2: %d\n", CapsLock(tevent.keyswitch));
  */

  data_as_text_t dat;
  dat.is_valid = false;

  int val = get_val_from_key(key);
  if (val==-1)
    return dat;

  int base = 10;
  if (is_hex)
    base = 0x10;

  int num_subtracks = 1 + log(max_value) / log(base);

  int vs[num_subtracks];
  for(int i=0;i<num_subtracks;i++){
    int div = pow(base, num_subtracks-i-1);
    vs[i] = old_value / div;
    old_value -= vs[i] * div;
  }
  
  /*
  int v1 = old_value / base;  
  //int v1 = (old_value & 0xf0) / 0x10;
  int v2 = old_value - (v1 * base); //& 0x0f;
  */
  
  if (key==EVENT_G){
    for(int i=0;i<num_subtracks;i++)
      vs[i] = base-1;
    
  } else if (key==EVENT_T){
    if (logtype==LOGTYPE_LINEAR)
      logtype = LOGTYPE_HOLD;
    else
      logtype = LOGTYPE_LINEAR;

  } else if (subsubtrack < num_subtracks) {
    vs[subsubtrack] = val;
    
  } else if (subsubtrack==num_subtracks) {
    if (val == 0)
      logtype=LOGTYPE_LINEAR;
    else
      logtype=LOGTYPE_HOLD;
  
  } else
    RError("Unknown subsubtrack: %d",subsubtrack);

  int highest_value = pow(base, num_subtracks) - 1;  
  //int highest_value = base*base - 1;
  if (highest_value_is_one_more)
    highest_value++;
    
  int v = 0;
  for(int i =0;i<num_subtracks;i++)
    v += vs[i] * pow(base, num_subtracks-i-1);

  int scaled = round(scale_double(v, min_value, highest_value, min_return_value, max_return_value));
  if (scaled > max_return_value)
    scaled = max_return_value;
  
  if (v<min_value)
    scaled = min_return_value;
  
  //if (v==base-1 && v2==base-1)
  //  scaled = max_value;
      
  //printf("old_value: %d, v1: %x, v2: %x, val: %x, v: %x, scaled: %d\n",old_value,v1,v2,val,v,scaled);
  
  dat.value = scaled;
  dat.logtype = logtype;
  dat.is_valid = true;

  return dat;
}


extern struct TEvent tevent;


// We circumvent the normal keyboard configuration system here.
bool DAT_keypress(struct Tracker_Windows *window, int key, bool is_keydown){

  if (AnyModifierKeyPressed(tevent.keyswitch))
    if(!AnyShift(tevent.keyswitch))
      return false;
  
  struct WBlocks *wblock = window->wblock;
  struct WTracks *wtrack = wblock->wtrack;

  int subtrack = window->curr_track_sub;

  if (window->curr_track >= 0){
    if (subtrack < 0 || subtrack >= WTRACK_num_non_polyphonic_subtracks(wtrack)) // subtrack -1 = note text
      return false;
  } else {
    if (window->curr_track != TEMPOTRACK && window->curr_track != LPBTRACK && window->curr_track != SIGNATURETRACK && window->curr_track != SWINGTRACK)
      return false;
  }
      
  
  if (get_val_from_key(key)==-1)
    return false;

  if (is_keydown==false)
    return true;

  int realline = wblock->curr_realline;
  if (realline < 0 || realline >= wblock->num_reallines){
    EVENTLOG_add_event(strdup(talloc_format("curr_realline: %d (%d)", wblock->curr_realline, wblock->num_reallines)));
    R_ASSERT(false);
    return false;
  }
  
  const Place *place = &wblock->reallines[realline]->l.p;

  if (VELTEXT_keypress(window, wblock, wtrack, realline, place, key) == false) {
    if (FXTEXT_keypress(window, wblock, wtrack, realline, place, key) == false) {
      if (CHANCETEXT_keypress(window, wblock, wtrack, realline, place, key) == false) {
        if (CENTTEXT_keypress(window, wblock, wtrack, realline, place, key) == false) {
          if (BPMTEXT_keypress(window, wblock, realline, place, key) == false) {
            if (LPBTEXT_keypress(window, wblock, realline, place, key) == false) {
              if (SIGNATURETEXT_keypress(window, wblock, realline, place, key) == false) {
                if (SWINGTEXT_keypress(window, wblock, window->curr_track < 0 ? NULL : wtrack, realline, place, key) == false) {
                  return false;
                }
              }
            }
          }
        }
      }
    }
  }
  
  window->must_redraw_editor = true;

  if(!AnyShift(tevent.keyswitch))
    MaybeScrollEditorDownAfterEditing(window,NULL);
  
  return true;
}

