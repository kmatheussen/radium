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
#include "vector_proc.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "tracks_proc.h"
#include "pitches_proc.h"
#include "gfx_subtrack_proc.h"

#include "nodelines_proc.h"

namespace r{
  int64_t g_node_id = 0;
}

typedef struct Node Node;

static const Node *get_node_from_nodeline1(const struct NodeLine *nodeline, float y_offset){
  struct Node *ret = (struct Node*)talloc(sizeof(Node));
  ret->x = nodeline->x1;
  ret->y = nodeline->y1 + y_offset;
  ret->element = nodeline->element1;
  return ret;
}

static const Node *get_node_from_nodeline2(const struct NodeLine *nodeline, float y_offset){
  struct Node *ret = (struct Node*)talloc(sizeof(Node));
  ret->x = nodeline->x2;
  ret->y = nodeline->y2 + y_offset;
  ret->element = nodeline->element2;
  return ret;
}

static const Node2 *get_node_from_nodeline1_2(const struct NodeLine2 *nodeline, float y_offset){
  struct Node2 *ret = (struct Node2*)talloc(sizeof(Node2));
  ret->x = nodeline->x1;
  ret->y = nodeline->y1 + y_offset;
  //ret->n = nodeline->n1;
  ret->id = nodeline->id1;
  return ret;
}

static const Node2 *get_node_from_nodeline2_2(const struct NodeLine2 *nodeline, float y_offset){
  struct Node2 *ret = (struct Node2*)talloc(sizeof(Node2));
  ret->x = nodeline->x2;
  ret->y = nodeline->y2 + y_offset;
  //ret->n = nodeline->n2;
  ret->id = nodeline->id2;
  return ret;
}

const vector_t *get_nodeline_nodes(const struct NodeLine *nodelines, float y_offset){
  vector_t *vector = (vector_t*)talloc(sizeof(vector_t));
  while(nodelines != NULL) {
    if (nodelines->is_node)
      VECTOR_push_back(vector, get_node_from_nodeline1(nodelines, y_offset));

    struct NodeLine *next = nodelines->next;

    if (next==NULL) {
      VECTOR_push_back(vector, get_node_from_nodeline2(nodelines, y_offset));
      break;
    }else{
      nodelines = next;
    }
  }
  return vector;
}

const vector_t *get_nodeline_nodes2(const struct NodeLine2 *nodelines, float y_offset){
  vector_t *vector = (vector_t*)talloc(sizeof(vector_t));
  while(nodelines != NULL) {
    if (nodelines->is_node)
      VECTOR_push_back(vector, get_node_from_nodeline1_2(nodelines, y_offset));

    struct NodeLine2 *next = nodelines->next;

    if (next==NULL) {
      VECTOR_push_back(vector, get_node_from_nodeline2_2(nodelines, y_offset));
      break;
    }else{
      nodelines = next;
    }
  }
  return vector;
}


// Note that 'y' can be outside the range of the nodeline. If that happens, nodelines is not modified.
static void insert_nonnode_nodeline(struct NodeLine *nodelines, const struct ListHeader3 *element, float y){

  if(y <= nodelines->y1)
    return;

  while(nodelines != NULL) {
    if(y>nodelines->y1 && y<nodelines->y2){

      // put it after

      struct NodeLine *n = (struct NodeLine *)talloc(sizeof(struct NodeLine));
      n->element1 = element;
      n->y1 = y;

      n->x1 = scale(GetfloatFromPlace(&element->p),
                    GetfloatFromPlace(&nodelines->element1->p),
                    GetfloatFromPlace(&nodelines->element2->p),
                    nodelines->x1, nodelines->x2
                    );

      //n->x1 = scale(y, nodelines->y1, nodelines->y2, nodelines->x1, nodelines->x2);

      n->next = nodelines->next ;
      nodelines->next = n;

      n->x2 = nodelines->x2;
      n->y2 = nodelines->y2;
      if (n->y2 < n->y1) {
        printf("\n\n\n3. y2 < y1: %f < %f",n->y2,n->y1);
        R_ASSERT_NON_RELEASE(false);
        n->y2 = n->y1;
      }
      n->element2 = nodelines->element2;

      nodelines->x2 = n->x1;
      nodelines->y2 = n->y1;
      if (nodelines->y2 < nodelines->y1) {
        printf("\n\n\n3. y2 < y1: %f < %f",nodelines->y2,nodelines->y1);
        R_ASSERT_NON_RELEASE(false);
        nodelines->y2 = nodelines->y1;
      }
      nodelines->element2 = n->element1;

      return;
    }

    nodelines = nodelines->next;
  }
}


// Note that 'y' can be outside the range of the nodeline. If that happens, nodelines is not modified.
static void insert_nonnode_nodeline2(struct NodeLine2 *nodelines,
                                     const struct ListHeader3 *element,
                                     float y,
                                     Ratio ratio)
{

  if(y <= nodelines->y1)
    return;

  while(nodelines != NULL) {
    if(y>nodelines->y1 && y<nodelines->y2){

      // put it after

      struct NodeLine2 *n = (struct NodeLine2 *)talloc(sizeof(struct NodeLine2));
      //n->element1 = element;
      n->id1 = -1;
      n->y1 = y;

      Ratio time1 = nodelines->time1 < 0 ? make_ratio(0,1) : nodelines->time1;
      Ratio time2 = nodelines->time2 < 0 ? make_ratio(0,1) : nodelines->time2;

      double dtime1 = make_double_from_ratio(time1);
      double dtime2 = make_double_from_ratio(time2);
      
      n->time1 = ratio;
                             
      /*
        n->time1 = make_ratio_from_double(scale_double(y,
        nodelines->y1, nodelines->y2,
        dtime1, dtime2
        ));*/
                       
      n->x1 = scale(GetfloatFromPlace(&element->p),
                    dtime1, dtime2,
                    nodelines->x1, nodelines->x2
                    );

      //n->x1 = scale(y, nodelines->y1, nodelines->y2, nodelines->x1, nodelines->x2);
      
      n->next = nodelines->next ;
      nodelines->next = n;

      n->x2 = nodelines->x2;
      n->y2 = nodelines->y2;
      if (n->y2 < n->y1) {
        printf("\n\n\n3. y2 < y1: %f < %f",n->y2,n->y1);
        R_ASSERT_NON_RELEASE(false);
        n->y2 = n->y1;
      }
      //n->element2 = nodelines->element2;
      n->time2 = nodelines->time2;
      n->id2 = nodelines->id2;
      
      nodelines->x2 = n->x1;
      nodelines->y2 = n->y1;
      if (nodelines->y2 < nodelines->y1) {
        printf("\n\n\n3. y2 < y1: %f < %f",nodelines->y2,nodelines->y1);
        R_ASSERT_NON_RELEASE(false);

        nodelines->y2 = nodelines->y1;
      }
      //nodelines->element2 = n->element1;
      nodelines->time2 = n->time1;
      nodelines->id2 = n->id1;
      
      return;
    }

    nodelines = nodelines->next;
  }
}


static const struct NodeLine *create_nodelines(
                                        const struct Tracker_Windows *window,
                                        const struct WBlocks *wblock,
                                        const struct ListHeader3 *list,                                  
                                        float (*get_x)(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype), // should return a value between 0 and 1.
                                        const struct ListHeader3 *last_element // may be null. may also contain more than one element.
                                        )
{
  struct NodeLine *nodelines = NULL;

  R_ASSERT(list != NULL);
  R_ASSERT(list->next != NULL || last_element!=NULL);

#if !defined(RELEASE) // somewhat serious bug, but the backtrace we get at this point is not very useful, so it's very little point bothering the user with it.
  if (last_element!=NULL){
    const Place *start = &list->p;
    const Place *end = &last_element->p;
    R_ASSERT(PlaceGreaterThan(end, start));
  }
#endif
  
  // 1. Create straight forward nodelines from the list
  {
    float reallineF = 0.0f;
    struct NodeLine *nodelines_last = NULL;

    while(list != NULL){
      struct NodeLine *nodeline = (struct NodeLine *)talloc(sizeof(struct NodeLine));

      nodeline->x1 = get_x(wblock, list, &nodeline->logtype);
      reallineF = FindReallineForF(wblock, reallineF, &list->p);
      nodeline->y1 = get_realline_y(window, reallineF);
      nodeline->element1 = list;
      nodeline->is_node = true;

      if(nodelines_last==NULL)
        nodelines = nodelines_last = nodeline;
      else {
        nodelines_last->next = nodeline;
        nodelines_last = nodeline;
      }

      list = list->next;
      if (list==NULL) {
        list = last_element;
        last_element = NULL;
      }
    }
  }


  // 2. Insert x2, y2 and element2 attributes, and remove last element.
  {
    R_ASSERT_RETURN_IF_FALSE2(nodelines!=NULL, NULL); // shouldn't be possible, but I got a crash report that indicates that this might have happened.
    R_ASSERT_RETURN_IF_FALSE2(nodelines->next!=NULL, NULL); // shouldn't be possible either, but more likely than the line above.

    struct NodeLine *ns = nodelines;
    struct NodeLine *next = ns->next;
    
    for(;;){
      ns->x2 = next->x1;//ns->logtype==LOGTYPE_HOLD ? ns->x1 : next->x1;
      ns->y2 = next->y1;

      if (ns->y2 < ns->y1) {
        printf("\n\n\n3. y2 < y1: %f < %f",ns->y2,ns->y1);
        R_ASSERT_NON_RELEASE(false);
        ns->y2 = ns->y1;
      }

      ns->element2 = next->element1;
      if(next->next==NULL)
        break;
      ns = next;
      next = next->next;
    }
    ns->next = NULL; // Cut the last element
  }


  // 3. Insert all non-node break-points. (caused by realline level changes)
  {
    const struct LocalZooms **reallines=wblock->reallines;
    int curr_level = reallines[0]->level;
    int realline;
    float reallineF = 0.0f;
    
    for(realline = 1; realline < wblock->num_reallines ; realline++) {
          
      const struct LocalZooms *localzoom = reallines[realline];
      
      if (localzoom->level != curr_level){
        reallineF = FindReallineForF(wblock, reallineF, &localzoom->l.p);
        insert_nonnode_nodeline(nodelines, &localzoom->l, get_realline_y(window, reallineF));
        curr_level = localzoom->level;
      }
    }
  }


  return nodelines;
}

//template<typename T2> 
static const struct NodeLine2 *create_nodelines2(
                                                 const struct Tracker_Windows *window,
                                                 const struct WBlocks *wblock,
                                                 //const Iterator &iterator;
                                                 //const T &reader,
                                                 //std::function<T2&(int n)> get_element,
                                                 int num_elements,
                                                 std::function<void(int n, float &x, int &logtype, Ratio &time, int64_t &id)> get_node_info
                                                 //float (*get_x)(const struct WBlocks *wblock, const T2 &node, int *logtype) // should return a value between 0 and 1.
                                                 )
{
  struct NodeLine2 *nodelines = NULL;

  // 1. Create straight forward nodelines from the list
  {
    float reallineF = 0.0f;
    struct NodeLine2 *nodelines_last = NULL;

    for(int i=0 ; i < num_elements ; i++) {

      //T2 &node = get_element(i);
      
      struct NodeLine2 *nodeline = (struct NodeLine2 *)talloc(sizeof(struct NodeLine2));

      get_node_info(i, nodeline->x1, nodeline->logtype, nodeline->time1, nodeline->id1);
      
      //nodeline->x1 = get_x(wblock, node, &nodeline->logtype);
      reallineF = FindReallineForRatioF(wblock, reallineF, nodeline->time1);
      nodeline->y1 = get_realline_y(window, reallineF);
      //Place da = ratio2place(node._time);
      //printf("reallinef: %f. time: %f. y1: %f. Place: %s\n", reallineF, (double)node._time.num/(double)node._time.den, nodeline->y1, PlaceToString(&da));
      //nodeline->element1 = list;
      //nodeline->time1 = node._time;
      //nodeline->id1 = node._id;
      nodeline->is_node = true;

      if(nodelines_last==NULL)
        nodelines = nodelines_last = nodeline;
      else {
        nodelines_last->next = nodeline;
        nodelines_last = nodeline;
      }
    }
  }


  // 2. Insert x2, y2 and element2 attributes, and remove last element.
  {
    R_ASSERT_RETURN_IF_FALSE2(nodelines!=NULL, NULL); // shouldn't be possible, but I got a crash report that indicates that this might have happened.
    R_ASSERT_RETURN_IF_FALSE2(nodelines->next!=NULL, NULL); // shouldn't be possible either, but more likely than the line above.

    struct NodeLine2 *ns = nodelines;
    struct NodeLine2 *next = ns->next;
    
    for(;;){
      ns->x2 = next->x1;//ns->logtype==LOGTYPE_HOLD ? ns->x1 : next->x1;
      ns->y2 = next->y1;

      if (ns->y2 < ns->y1) {
        if (fabs(ns->y2-ns->y1) > 0.001) { // Don't report if the error could have been caused by floating point rounding error.
          printf("\n\n\n3. y2 < y1: %f < %f",ns->y2,ns->y1);
          R_ASSERT_NON_RELEASE(false);
        }
        ns->y2 = ns->y1;
      }

      //ns->element2 = next->element1;
      ns->time2 = next->time1;
      ns->id2 = next->id1;
      if(next->next==NULL)
        break;
      ns = next;
      next = next->next;
    }
    ns->next = NULL; // Cut the last element
  }


  // 3. Insert all non-node break-points. (caused by realline level changes)
  {
    const struct LocalZooms **reallines=wblock->reallines;
    int curr_level = reallines[0]->level;
    int realline;
    float reallineF = 0.0f;
    
    for(realline = 1; realline < wblock->num_reallines ; realline++) {
          
      const struct LocalZooms *localzoom = reallines[realline];
      
      if (localzoom->level != curr_level){
        reallineF = FindReallineForF(wblock, reallineF, &localzoom->l.p);
        insert_nonnode_nodeline2(nodelines, &localzoom->l, get_realline_y(window, reallineF), ratio_from_place(localzoom->l.p));
        curr_level = localzoom->level;
      }
    }
  }


  return nodelines;
}


// temponodes
///////////////////////////////////////////////////////////

static float get_temponode_x(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype){
  struct TempoNodes *temponode = (struct TempoNodes*)element;
  *logtype = LOGTYPE_LINEAR; // Anything else is very complicated.
  return scale(temponode->reltempo,
               (float)(-wblock->reltempomax+1.0f),(float)(wblock->reltempomax-1.0f),
               wblock->temponodearea.x, wblock->temponodearea.x2
               );
}

const struct NodeLine *GetTempoNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return create_nodelines(window,
                          wblock,
                          &wblock->block->temponodes->l,
                          get_temponode_x,
                          NULL
                          );
}

const vector_t *GetTempoNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return get_nodeline_nodes(GetTempoNodeLines(window, wblock),
                            wblock->t.y1);
}



// pitchlines
///////////////////////////////////////////////////////////

/*
static float track_notearea_x1, track_notearea_x2;
static float track_pitch_min;
static float track_pitch_max;

static float get_pitch_x(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype){
  struct Pitches *pitch = (struct Pitches*)element;
  *logtype = pitch->logtype;

  if (equal_floats(track_pitch_min, track_pitch_max)){
    R_ASSERT_NON_RELEASE(equal_floats(track_pitch_min, pitch->note));
    return (track_notearea_x1 + track_notearea_x2) / 2.0f;
  }

  return scale(pitch->note,
               track_pitch_min, track_pitch_max,
               track_notearea_x1, track_notearea_x2
               );
}


const struct NodeLine *GetPitchNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
    
  track_notearea_x1 = wtrack->notearea.x;
  track_notearea_x2 = wtrack->notearea.x2;

  if (equal_floats(track_notearea_x1, track_notearea_x2)){
    track_notearea_x1 = 0;
    track_notearea_x2 = 1;
  }
    
  TRACK_get_min_and_max_pitches(wtrack->track, &track_pitch_min, &track_pitch_max);

  struct Pitches *first_pitch = (struct Pitches *)talloc(sizeof(struct Pitches));
  first_pitch->l.p = note->l.p;
  first_pitch->l.next = note->pitches==NULL ? NULL : &note->pitches->l;
  first_pitch->note = note->note;
  first_pitch->logtype = note->pitch_first_logtype;
  
  struct Pitches *last_pitch = (struct Pitches *)talloc(sizeof(struct Pitches));
  last_pitch->l.p = ratio2place(note->end);
  last_pitch->l.next = NULL;

  if (note->pitch_end>0)
    last_pitch->note = note->pitch_end;
  else
    last_pitch->note = note->note;

  last_pitch->logtype = LOGTYPE_IRRELEVANT;
  
  return create_nodelines(window,
                          wblock,
                          &first_pitch->l,
                          get_pitch_x,
                          &last_pitch->l
                          );
}

const vector_t *GetPitchNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  return get_nodeline_nodes(GetPitchNodeLines(window, wblock, wtrack, note),
                            wblock->t.y1);
}
*/


const struct NodeLine2 *GetPitchNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const float track_pitch_min, const float track_pitch_max, const r::PitchTimeData::Reader &reader){
    
  double track_notearea_x1 = wtrack->notearea.x;
  double track_notearea_x2 = wtrack->notearea.x2;

  if (equal_floats(track_notearea_x1, track_notearea_x2)){
    track_notearea_x1 = 0;
    track_notearea_x2 = 1;
  }

  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size() + 2,
                                [&reader, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max, note](int n, float &x, int &logtype, Ratio &time, int64_t &id) {

                                  float val;
                                  
                                  if (n==0){
                                    
                                    val = note->note;
                                    logtype = note->pitch_first_logtype;
                                    time = ratio_from_place(note->l.p);
                                    id = -1; // FIX
                                    
                                  } else if (n==reader.size()+1) {

                                    if (note->pitch_end > 0)
                                      val = note->pitch_end;
                                    else
                                      val = note->note;
                                    
                                    logtype = LOGTYPE_IRRELEVANT;
                                    time = note->end;
                                    id = -2; // FIX
                                    
                                  } else {
                                  
                                    const r::Pitch &node = reader.at_ref(n-1);
                                    val = node._val;
                                    
                                    logtype = node._logtype;
                                    time = node._time;
                                    id = node._id;
                                    
                                  }

                                  if (equal_floats(track_pitch_min, track_pitch_max)){
                                    
                                    R_ASSERT_NON_RELEASE(equal_floats(track_pitch_min, val));
                                    x = (track_notearea_x1 + track_notearea_x2) / 2.0f;
                                    
                                  } else {

                                    x = scale_double(val,
                                                     track_pitch_min, track_pitch_max,
                                                     track_notearea_x1, track_notearea_x2);
                                  }
                                  
                                }
                                );
  return ret;
}

const vector_t *GetPitchNodes2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note, const float track_pitch_min, const float track_pitch_max){

  const r::PitchTimeData::Reader pitch_reader(note->_pitches);
  
  return get_nodeline_nodes2(GetPitchNodeLines2(window, wblock, wtrack, note, track_pitch_min, track_pitch_max, pitch_reader),
                             wblock->t.y1);
}

const struct NodeLine2 *GetPitchNodeLines3(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const r::Note *note, const float track_pitch_min, const float track_pitch_max, const r::PitchTimeData::Reader &reader){
    
  double track_notearea_x1 = wtrack->notearea.x;
  double track_notearea_x2 = wtrack->notearea.x2;

  if (equal_floats(track_notearea_x1, track_notearea_x2)){
    track_notearea_x1 = 0;
    track_notearea_x2 = 1;
  }

  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size() + 2,
                                [&reader, track_notearea_x1, track_notearea_x2, track_pitch_min, track_pitch_max, &note](int n, float &x, int &logtype, Ratio &time, int64_t &id) {

                                  float val;
                                  
                                  if (n==0){
                                    
                                    val = note->get_val();
                                    logtype = note->d._pitch_first_logtype;
                                    time = note->get_time();
                                    id = -1; // FIX
                                    
                                  } else if (n==reader.size()+1) {

                                    if (note->d._pitch_end > 0)
                                      val = note->d._pitch_end;
                                    else
                                      val = note->get_val();
                                    
                                    logtype = LOGTYPE_IRRELEVANT;
                                    time = note->d._end;
                                    id = -2; // FIX
                                    
                                  } else {
                                  
                                    const r::Pitch &node = reader.at_ref(n-1);
                                    val = node._val;
                                    
                                    logtype = node._logtype;
                                    time = node._time;
                                    id = node._id;
                                    
                                  }

                                  if (equal_floats(track_pitch_min, track_pitch_max)){
                                    
                                    R_ASSERT_NON_RELEASE(equal_floats(track_pitch_min, val));
                                    x = (track_notearea_x1 + track_notearea_x2) / 2.0f;
                                    
                                  } else {

                                    x = scale_double(val,
                                                     track_pitch_min, track_pitch_max,
                                                     track_notearea_x1, track_notearea_x2);
                                  }
                                  
                                }
                                );
  return ret;
}



// pianoroll notes
///////////////////////////////////////////////////////////

/*
static const struct WTracks *pianoroll_wtrack;

static float get_pianoroll_x(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype){
  struct Pitches *pitch = (struct Pitches*)element;

  *logtype = pitch->logtype;
  
  //int octave = pitch->note / 12;
  //float chroma = pitch->note - (float)(octave*12);

  float gfx_width = pianoroll_wtrack->pianoroll_area.x2 - pianoroll_wtrack->pianoroll_area.x;
  float notespan = pianoroll_wtrack->pianoroll_highkey - pianoroll_wtrack->pianoroll_lowkey;
  float skew = (gfx_width / notespan) / 2.0;
  
  return
    skew +
    scale(pitch->note,
          pianoroll_wtrack->pianoroll_lowkey, pianoroll_wtrack->pianoroll_highkey,
          pianoroll_wtrack->pianoroll_area.x, pianoroll_wtrack->pianoroll_area.x2
          );
}


const struct NodeLine *GetPianorollNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){

  pianoroll_wtrack = wtrack;

  struct Pitches *first_pitch = (struct Pitches *)talloc(sizeof(struct Pitches));
  first_pitch->l.p = note->l.p;
  first_pitch->l.next = note->pitches==NULL ? NULL : &note->pitches->l;
  first_pitch->note = note->note;
  first_pitch->logtype = note->pitch_first_logtype;
    
  struct Pitches *last_pitch = (struct Pitches *)talloc(sizeof(struct Pitches));
  last_pitch->l.p = ratio2place(note->end);
  last_pitch->l.next = NULL;
    
  if (note->pitch_end>0)
    last_pitch->note = note->pitch_end;
  else
    last_pitch->note = note->note;

  last_pitch->logtype = LOGTYPE_IRRELEVANT;

  return create_nodelines(window,
                          wblock,
                          &first_pitch->l,
                          get_pianoroll_x,
                          &last_pitch->l
                          );
}

const vector_t *GetPianorollNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  return get_nodeline_nodes(GetPianorollNodeLines(window, wblock, wtrack, note),
                            wblock->t.y1);
}
*/

const struct NodeLine2 *GetPianorollNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const r::Note *note, const r::PitchTimeData::Reader &reader){

  float gfx_width = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  float notespan = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  float skew = (gfx_width / notespan) / 2.0;
    
  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size() + 2,
                                [&reader, wtrack, note, skew](int n, float &x, int &logtype, Ratio &time, int64_t &id) {

                                  float val;
                                  
                                  if (n==0){
                                    
                                    val = note->_val;
                                    logtype = note->d._pitch_first_logtype;
                                    time = note->get_time();
                                    id = -1; // FIX
                                    
                                  } else if (n==reader.size()+1) {

                                    if (note->d._pitch_end > 0)
                                      val = note->d._pitch_end;
                                    else
                                      val = note->_val;
                                    
                                    logtype = LOGTYPE_IRRELEVANT;
                                    time = note->d._end;
                                    id = -2; // FIX
                                    
                                  } else {
                                  
                                    const r::Pitch &node = reader.at_ref(n-1);
                                    val = node._val;
                                    
                                    logtype = node._logtype;
                                    time = node._time;
                                    id = node._id;
                                    
                                  }

                                  x = skew + scale(val,
                                                   wtrack->pianoroll_lowkey, wtrack->pianoroll_highkey,
                                                   wtrack->pianoroll_area.x, wtrack->pianoroll_area.x2
                                                   );
                                }
                                );
  return ret;
  /*
  return create_nodelines(window,
                          wblock,
                          &first_pitch->l,
                          get_pianoroll_x,
                          &last_pitch->l
                          );
  */
}

/*
const vector_t *GetPianorollNodes2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  return get_nodeline_nodes2(GetPianorollNodeLines2(window, wblock, wtrack, note),
                             wblock->t.y1);
}
*/


// velocities
///////////////////////////////////////////////////////////

#if 0

static float g_subtrack_x1, g_subtrack_x2;

static float get_velocity_x(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype){
  struct Velocities *velocity = (struct Velocities*)element;
  *logtype = velocity->logtype;
  return scale_double(velocity->velocity, 0, MAX_VELOCITY, g_subtrack_x1, g_subtrack_x2);
}

const struct NodeLine *GetVelocityNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  struct Velocities *first_velocity = (struct Velocities*)&note->first_velocity;
  first_velocity->l.p = note->l.p;
  first_velocity->l.next = note->velocities==NULL ? NULL : &note->velocities->l;
  first_velocity->velocity = note->velocity;
  first_velocity->logtype = note->velocity_first_logtype;
  
  struct Velocities *last_velocity = (struct Velocities*)&note->last_velocity;
  last_velocity->l.p = note->end;
  last_velocity->l.next = NULL;
  last_velocity->velocity = note->velocity_end;
  last_velocity->logtype = LOGTYPE_IRRELEVANT;
  
  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  g_subtrack_x1 = GetNoteX1(wtrack,note->polyphony_num);
  g_subtrack_x2 = R_MAX(g_subtrack_x1+1, GetNoteX2(wtrack,note->polyphony_num));
  
  return create_nodelines(window,
                          wblock,
                          &first_velocity->l,
                          get_velocity_x,
                          &last_velocity->l
                          );
}
#endif

const struct NodeLine2 *GetVelocityNodeLines2(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){  
  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  double subtrack_x1 = GetNoteX1(wtrack,note->polyphony_num);
  double subtrack_x2 = R_MAX(subtrack_x1+1, GetNoteX2(wtrack,note->polyphony_num));

  const r::VelocityTimeData::Reader reader(note->_velocities);

  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size() + 2,
                                [&reader, subtrack_x1, subtrack_x2, note](int n, float &x, int &logtype, Ratio &time, int64_t &id) {

                                  int val;
                                  
                                  if (n==0){
                                    
                                    val = note->velocity;
                                    logtype = note->velocity_first_logtype;
                                    time = ratio_from_place(note->l.p);
                                    id = -1; // FIX
                                    
                                  } else if (n==reader.size()+1) {
                                    
                                    val = note->velocity_end;
                                    logtype = LOGTYPE_IRRELEVANT;
                                    time = note->end;
                                    id = -2; // FIX
                                    
                                  } else {
                                  
                                    const r::Velocity &node = reader.at_ref(n-1);
                                    val = node._val;
                                    
                                    logtype = node._logtype;
                                    time = node._time;
                                    id = node._id;
                                    
                                  }

                                  x = scale_double(val, 0, MAX_VELOCITY, subtrack_x1, subtrack_x2);
                                  
                                }
                                );
  return ret;
}

const vector_t *GetVelocityNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct Notes *note){
  return get_nodeline_nodes2(GetVelocityNodeLines2(window, wblock, wtrack, note),
                             wblock->t.y1);
}

const struct NodeLine2 *GetVelocityNodeLines3(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const r::Note *note){  
  //printf("Note: %s, pointer: %p, subtrack: %d\n",NotesTexts3[(int)note->note],note,note->subtrack);
  double subtrack_x1 = GetNoteX1(wtrack,note->d._polyphony_num);
  double subtrack_x2 = R_MAX(subtrack_x1+1, GetNoteX2(wtrack,note->d._polyphony_num));

  const r::VelocityTimeData::Reader reader(&note->_velocities);

  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size() + 2,
                                [&reader, subtrack_x1, subtrack_x2, note](int n, float &x, int &logtype, Ratio &time, int64_t &id) {

                                  int val;
                                  
                                  if (n==0){
                                    
                                    val = note->d._velocity;
                                    logtype = note->d._velocity_first_logtype;
                                    time = note->get_time();
                                    id = -1; // FIX
                                    
                                  } else if (n==reader.size()+1) {
                                    
                                    val = note->d._velocity_end;
                                    logtype = LOGTYPE_IRRELEVANT;
                                    time = note->d._end;
                                    id = -2; // FIX
                                    
                                  } else {
                                  
                                    const r::Velocity &node = reader.at_ref(n-1);
                                    val = node._val;
                                    
                                    logtype = node._logtype;
                                    time = node._time;
                                    id = node._id;
                                    
                                  }

                                  x = scale_double(val, 0, MAX_VELOCITY, subtrack_x1, subtrack_x2);
                                  
                                }
                                );
  return ret;
}

const vector_t *GetVelocityNodes3(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const r::Note *note){
  return get_nodeline_nodes2(GetVelocityNodeLines3(window, wblock, wtrack, note),
                             wblock->t.y1);
}



// fxs
///////////////////////////////////////////////////////////

//static float fx_min, fx_max, wtrackfx_x1, wtrackfx_x2;

#if 1

/*
static float get_fxs_x(const struct WBlocks *wblock, const r::FXNode &fxnode, int *logtype){
  *logtype = fxnode._logtype;
  return scale(fxnode._val, fx_min, fx_max, wtrackfx_x1, wtrackfx_x2);
}
*/

const struct NodeLine2 *GetFxNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){

  const r::FXTimeData::Reader reader(fxs->_fxnodes);

  auto *ret = create_nodelines2(window,
                                wblock,
                                reader.size(),
                                [&reader, wtrack, fxs](int n, float &x, int &logtype, Ratio &time, int64_t &id) {
                                  
                                  const r::FXNode &node = reader.at_ref(n);
                                  
                                  float fx_min = fxs->fx->min;
                                  float fx_max = fxs->fx->max;
                                  float wtrackfx_x1 = wtrack->fxarea.x;
                                  float wtrackfx_x2 = wtrack->fxarea.x2;
  
                                  x = scale(node._val, fx_min, fx_max, wtrackfx_x1, wtrackfx_x2);
                                  logtype = node._logtype;
                                  time = node._time;
                                  id = node._id;
                                }
                                );
  return ret;
}

#else
static float get_fxs_x(const struct WBlocks *wblock, const struct ListHeader3 *element, int *logtype){
  struct FXNodeLines *fxnode = (struct FXNodeLines *)element;
  *logtype = fxnode->logtype;
  return scale(fxnode->val, fx_min, fx_max, wtrackfx_x1, wtrackfx_x2);
}


const struct NodeLine *GetFxNodeLines(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){
  fx_min = fxs->fx->min;
  fx_max = fxs->fx->max;
  wtrackfx_x1 = wtrack->fxarea.x;
  wtrackfx_x2 = wtrack->fxarea.x2;

  return create_nodelines(window,
                          wblock,
                          &fxs->fxnodelines->l,
                          get_fxs_x,
                          NULL
                          );  
}
#endif

const vector_t *GetFxNodes(const struct Tracker_Windows *window, const struct WBlocks *wblock, const struct WTracks *wtrack, const struct FXs *fxs){
  const struct NodeLine2 *nodelines = GetFxNodeLines(window, wblock, wtrack, fxs);
  return get_nodeline_nodes2(nodelines, wblock->t.y1);
}
  


// OLD CODE BELOW. Will be deleted

#if !USE_OPENGL

#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "nodelines.h"
#include "tbox_proc.h"


/*
void FillInLineRealLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,

	float x1,float y1,float x2,float y2,

	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
		void *extrainfo,
		int firstlast,
		int realline,
		int u_y1,int u_y2,
		float u_x1,float u_x2
	),
	int firstlast
){
	(*ReturnNodeLineData)(
		window,
		wblock,
		extrainfo,
		firstlast,
		realline,
		y1*window->org_fontheight,y2*window->org_fontheight,
		x1,x2
	);
}
*/

#define FillInLineRealLine(a,b,t,c,d,e,f,g,h,i,j) (*i)(a,b,t,h,j,c,e,g,(float)(d),(float)(f))


/**********************************************************************
  FUNCTION
    Given the line,counter and dividor for start and end of a nodeline;
    Make nodelines that spans this area.
  INPUTS
    window             - window
    wblock             - wblock
    p1                 - Start placement
    p2                 - End placement
    x1                 - Start x value    (this is not a graphical x, but a vector)
    x2                 - End x value       -------------- "" ---------------------
    minx               - minimum x value   -------------- "" ---------------------
    maxx               - maximum x value   -------------- "" ---------------------
    extrainfo          - Information sent with the ReturnNodeLineData function
    ReturnNodeLineData - When a nodeline has been calculated, this function
                         is called. u_y1,u_y2,u_x1,u_x2 is graphical coordinates.
                         For the parameter 'firstlast', check out nodelines.h.
**********************************************************************/


void MakeNodeLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	Place *p1,
	Place *p2,
	float x1,float x2,
	float minx,float maxx,
	void *extrainfo,
	void (*ReturnNodeLineData)(
		struct Tracker_Windows *window,
		struct WBlocks *wblock,
                struct WTracks *wtrack,
		void *extrainfo,
		int firstlast,
		int realline,
		float u_y1,float u_y2,
		float u_x1,float u_x2
	)
){

  float y1=GetFloatFromPlace(p1);
  float y2=GetFloatFromPlace(p2);

  int realline1=FindRealLineFor(wblock,p1->line,p1);
  int realline2=FindRealLineFor(wblock,R_MAX(realline1,p2->line),p2);

  float ry1,ry2;

  //printf("x1: %f, x2: %f, minx: %f, maxx: %f\n", x1,x2,minx,maxx);

  /* Get rid of minx. */
  x1-=minx;
  x2-=minx;
  maxx-=minx;

  /* Get rid of maxx. */
  x1/=maxx;
  x2/=maxx;

  if(realline2==realline1){
    ry1=GetFloatFromPlace(&(wblock->reallines[realline1]->l.p));
    if(realline1==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline1+1]->l.p));
    }

    FillInLineRealLine(
		       window,
		       wblock,
                       wtrack,
		       realline1,
		       
		       x1,(float)((y1-ry1)/(ry2-ry1)),
		       x2,(float)((y2-ry1)/(ry2-ry1)),

		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_FIRSTANDLAST
		       );		

  }else{

    int lokke;
    float direction=(x2-x1)/(y2-y1);

    ry1=GetFloatFromPlace(&(wblock->reallines[realline1]->l.p));
    if(realline1==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline1+1]->l.p));
    }

    //printf("direction: %f, ry1: %f, ry2: %f\n",direction,ry1,ry2);

    FillInLineRealLine(
		       window,
		       wblock,
                       wtrack,
		       realline1,
		       
		       x1,(float)((y1-ry1)/(ry2-ry1)),
		       (float)((direction*(ry2-y1))+x1),
		       1.0f,
		       
		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_FIRST
		       );
    
    
    for(lokke=realline1+1;lokke<realline2;lokke++){
      ry1=ry2;
      if(lokke==wblock->num_reallines-1){
	ry2=wblock->block->num_lines;
      }else{
	ry2=GetFloatFromPlace(&(wblock->reallines[lokke+1]->l.p));
      }

      FillInLineRealLine(
			 window,
			 wblock,
                         wtrack,
			 lokke,
			 
			 (float)((direction*(ry1-y1))+x1),
			 0.0f,
			 (float)((direction*(ry2-y1))+x1),
			 1.0f,
			 
			 extrainfo,
			 ReturnNodeLineData,
			 NODELINE_NOTFIRSTORLAST
			 );
    }


    ry1=ry2;

    if(realline2==wblock->num_reallines-1){
      ry2=wblock->block->num_lines;
    }else{
      ry2=GetFloatFromPlace(&(wblock->reallines[realline2+1]->l.p));
    }

    FillInLineRealLine(
		       window,
		       wblock,
                       wtrack,
		       realline2,
		       
		       (float)((direction*(ry1-y1))+x1),
		       0.0f,
		       x2,
		       (float)((y2-ry1)/(ry2-ry1)),
		       
		       extrainfo,
		       ReturnNodeLineData,
		       NODELINE_LAST
		       );
  }
}


void GetNodeLine(
		 struct TrackReallineElements *tre,
		 WArea *warea,
		 TBox *within,
		 TBox *ret
		 ){

  ret->y1 = within->y1 + (tre->y1*(within->y2-within->y1));
  ret->y2 = within->y1 + (tre->y2*(within->y2-within->y1));

  ret->x1 = warea->x + (warea->width*tre->x1);
  ret->x2 = warea->x + (warea->width*tre->x2);

  TBOX_within(ret,within);
}


#endif
