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
#include "TimeData.hpp"
#include "FX.hpp"
#include "vector_proc.h"
#include "list_proc.h"
#include "realline_calc_proc.h"
#include "undo_fxs_proc.h"
#include "undo.h"
#include "data_as_text_proc.h"
#include "fxlines_proc.h"

#include "fxtext_proc.h"

#if 1
static void add_fxtext(const struct WBlocks *wblock, FXText_trss &fxtexts, const struct FX *fx, const r::FXNode &fxnode, int fxnodenum){

  int realline = FindReallineForRatio(wblock, 0, fxnode._time);
  FXText_trs &v = fxtexts[realline];

  r::FXText fxtext(fxnode);

  fxtext.fx = fx;
  fxtext.fxnodenum = fxnodenum;
  
  fxtext.value = round(scale_double(fxnode._val, fx->min, fx->max, 0, 0x100));
  if (fxtext.value==0x100)
    fxtext.value=0xff;
  fxtext.logtype = fxnode._logtype;

  v.push_back(fxtext);
  //TRS_INSERT_PLACE(v, fxtext);
}
#else
static void add_fxtext(const struct WBlocks *wblock, FXText_trss &fxtexts, const struct FX *fx, struct FXNodeLines *fxnodeline){

  int realline = FindRealLineFor(wblock, 0, &fxnodeline->l.p);      
  FXText_trs &v = fxtexts[realline];

  r::FXText fxtext = {};

  fxtext.p = fxnodeline->l.p;
  fxtext.fx = fx;
  fxtext.fxnodeline = fxnodeline;
  
  fxtext.value = round(scale_double(fxnodeline->val, fx->min, fx->max, 0, 0x100));
  if (fxtext.value==0x100)
    fxtext.value=0xff;
  fxtext.logtype = fxnodeline->logtype;

  TRS_INSERT_PLACE(v, fxtext);
}
#endif

static void move_fxtexts_to_unique_reallines(const struct WBlocks *wblock, FXText_trss &fxtexts){
  for(int realline=0;realline<wblock->num_reallines-1;realline++){

    if (fxtexts.contains(realline) && !fxtexts.contains(realline+1)) {
      
      FXText_trs &trs1 = fxtexts[realline];

      if (trs1.size() > 1) {
        FXText_trs &trs2 = fxtexts[realline+1];
        
        trs2.push_back(trs1.takeFirst());

        std::swap(fxtexts[realline], fxtexts[realline+1]);

      }
    }
  }
}


// Returns a pointer to AN ARRAY of vectors (one vector for each realline), not a pointer to a vector (as one would think).
const FXText_trss FXTEXTS_get(const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){
  FXText_trss fxtexts;

#if 1
  
  r::TimeData<r::FXNode>::Reader reader(fxs->_fxnodes);
  int i =0;
  for(const r::FXNode &node : reader){
    add_fxtext(wblock, fxtexts, fxs->fx, node, i);
    i++;
  }
  
#else
  
  struct FXNodeLines *fxnodeline = fxs->fxnodelines;
  while(fxnodeline!=NULL){
    add_fxtext(wblock, fxtexts, fxs->fx, fxnodeline);
    fxnodeline = NextFXNodeLine(fxnodeline);
  }
  
#endif
  
  move_fxtexts_to_unique_reallines(wblock, fxtexts);
  
  return fxtexts;
}

int FXTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack, struct FXs **to_fxs){
  if (wtrack->fxtext_on == false)
    return -1;

  int subsubtrack = window->curr_track_sub;

  if (wtrack->swingtext_on == true)
    subsubtrack -= 3;
  
  if (wtrack->centtext_on == true)
    subsubtrack -= 2;
  
  if (wtrack->chancetext_on == true)
    subsubtrack -= 2;
  
  if (wtrack->veltext_on == true)
    subsubtrack -= 3;

  if (subsubtrack < 0)
    return -1;

  VECTOR_FOR_EACH(struct FXs *, fxs, &wtrack->track->fxs){
    if (subsubtrack == 0 || subsubtrack == 1 || subsubtrack == 2){
      if (to_fxs!=NULL)
        *to_fxs = fxs;
      return subsubtrack;
    }
    
    subsubtrack -= 3;
  }END_VECTOR_FOR_EACH;
  
  return -1;
}

bool FXTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key){
  struct FXs *fxs;
  
  int subsubtrack = FXTEXT_subsubtrack(window, wtrack, &fxs);
  if (subsubtrack==-1)
    return false;

  struct FX *fx = fxs->fx;
  //printf("FXmin: %d, fxmax: %d\n",fx->min, fx->max);
  
  const FXText_trss &fxtexts = FXTEXTS_get(wblock, wtrack, fxs);

  const FXText_trs &fxtext = fxtexts[realline];

  //  if (fxtext->num_elements == 0 && val==0)
  //   return true;

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  if (fxtext.size() > 1){

    // MORE THAN ONE ELEMENT
    
    if (key == EVENT_DEL){

      std::vector<int> fxnodenums;
      for (auto vt : fxtext)
        fxnodenums.push_back(vt.fxnodenum);
        
      DeleteFxNodes(window, wtrack, fxs, fxnodenums);
      
    } else {
      
      UNDO_CANCEL_LAST_UNDO();
      
    }
    
  } else if (fxtext.size() == 0){

    // NO ELEMENTS

    if (fx == NULL){

      fprintf(stderr, "Can this happen?\n");
      UNDO_CANCEL_LAST_UNDO();
      
    } else {

      data_as_text_t dat = DAT_get_newvalue(subsubtrack, key, round(scale_double(0x80, 0, 0x100, fx->min, fx->max)), LOGTYPE_LINEAR, 0, 0xff, fx->min, fx->max, true, true, true);
      if (dat.value > fx->max)
        dat.value = fx->max;
      
      if (dat.is_valid==false)
        return false;

      r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);

      Ratio ratio = ratio_from_place(*place);
      auto node = r::FXNode(*fxs->fx, ratio, dat.value, dat.logtype);
      writer.add(node);
    }

  } else {

    // ONE ELEMENT
    
    const r::FXText &vt = fxtext.at(0);
    
    //struct FXNodeLines *fxnodeline = vt.fxnodeline;
  
    if (key == EVENT_DEL) {

      if (subsubtrack == 2) {
        r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
        r::FXNode &node = writer.at_ref(vt.fxnodenum);
        node._logtype = LOGTYPE_LINEAR;
      } else {
        DeleteFxNode(window, wtrack, fxs, vt.fxnodenum);
      }
      
    } else {

      data_as_text_t dat = DAT_get_overwrite(vt.value, vt.logtype, subsubtrack, key, 0, 0xff, fx->min, fx->max, true, true);
      //printf("fx->min: %x, fx->max: %x, vt.value: %x, dat.value: %x\n",fx->min,fx->max,vt.value,dat.value);

      if (dat.is_valid==false)
        return false;

      r::TimeData<r::FXNode>::Writer writer(fxs->_fxnodes);
      r::FXNode &node = writer.at_ref(vt.fxnodenum);
      node._val = dat.value;
      node._logtype = dat.logtype;
      
    }    
  }

  return true;
}
  


