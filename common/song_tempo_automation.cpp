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





#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "seqtrack_proc.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/Mixer_proc.h"

#include "song_tempo_automation_proc.h"

#include <QPainter>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


// The sequencer tempo automation. It maps between abstime and absabstime.
//

namespace{

struct TempoAutomationNode{
  double time; // abstime format
  double value;
  int logtype;
};
 
}

static double g_max_tempo = 4.0;

static radium::SeqAutomation<TempoAutomationNode> g_tempo_automation;


static TempoAutomationNode create_node(double abstime, double value, int logtype){
  TempoAutomationNode node = {.time = abstime,
                              .value = value,
                              .logtype = logtype};

  return node;
}


static float get_node_y(const TempoAutomationNode *node, float y1, float y2){
  float mid = (y1+y2)/2.0f;

  if (node->value >= 1.0)
    return scale(node->value,
                 g_max_tempo, 1.0,
                 y1, mid);
  else
    return scale(node->value,
                 1.0, 1.0/g_max_tempo,
                 mid, y2);
}

/*
faust code for "get_optimized_custom_value":
"""
g_max_tempo = hslider("max_tempo", 4.0, 0.001, 10000, 0.001);

scale(x,x1,x2,y1,y2) = y1 + ( ((x-x1)*(y2-y1)) / (x2-x1));


get_node_y(value, y1, y2) = ret with {
    mid = (y1+y2)/2.0f;

    larger = scale(value,
                   g_max_tempo, 1.0,
                   y1, mid);
    smaller = scale(value,
                    1.0, 1.0/g_max_tempo,
                    mid, y2);

    ret = select2(value>=1,smaller, larger);
};

process(abstime, abstime1, abstime2, value1, value2) = ret with{
    y1 = get_node_y(value1, 0, 2);
    y2 = get_node_y(value2, 0, 2);
    midtime = scale(1, y1, y2, abstime1, abstime2);
    smaller = scale(abstime, abstime1, midtime, value1, 1.0);
    larger = scale(abstime, midtime, abstime2, 1.0, value2);
    ret = select2(abstime<=midtime, larger, smaller);
};
"""
 */
static double get_optimized_custom_value(double input0, double input1, double input2, double input3, double input4){
  double fHslider0 = g_max_tempo;
		double fSlow0 = double(fHslider0);
		double fSlow1 = (1.0 / ((1.0 / fSlow0) - 1.0));
		double fSlow2 = (1.0 / (1.0 - fSlow0));
                //		for (int i = 0; (i < count); i = (i + 1)) {
			double fTemp0 = double(input0);
			double fTemp1 = double(input3);
			double fSel0 = 0.0;
			if ((fTemp1 >= 1.0) != 0) {
				fSel0 = (fSlow2 * (fTemp1 - fSlow0));
				
			} else {
				fSel0 = (1.0 + (fSlow1 * (fTemp1 - 1.0)));
				
			}
			double fTemp2 = double(input2);
			double fTemp3 = double(input1);
			double fTemp4 = ((1.0 - fSel0) * (fTemp2 - fTemp3));
			double fTemp5 = double(input4);
			double fTemp6 = (fTemp5 - 1.0);
			double fSel1 = 0.0;
			if ((fTemp5 >= 1.0) != 0) {
				fSel1 = (fSlow2 * (fTemp5 - fSlow0));
				
			} else {
				fSel1 = (1.0 + (fSlow1 * fTemp6));
				
			}
			double fTemp7 = (fSel1 - fSel0);
			double fTemp8 = ((fTemp4 / fTemp7) + fTemp3);
			double fSel2 = 0.0;
			if ((fTemp0 <= fTemp8) != 0) {
				fSel2 = ((((fTemp7 * (1.0 - fTemp1)) * (fTemp0 - fTemp3)) / fTemp4) + fTemp1);
				
			} else {
				fSel2 = (1.0 + (((0.0 - (fTemp8 - fTemp0)) * fTemp6) / (0.0 - (fTemp8 - fTemp2))));
				
			}
			double output0 = fSel2;
			
                        //		}

                        return output0;

}

static double custom_get_value(double abstime, const TempoAutomationNode *node1, const TempoAutomationNode *node2){
  const double abstime1 = node1->time;
  const double abstime2 = node2->time;

  const double value1 = node1->value;
  const double value2 = node2->value;

  if (value1<=1.0 && value2<=1.0)
    return scale_double(abstime, abstime1, abstime2, value1, value2);
  
  if (value1>=1.0 && value2>=1.0)
    return scale_double(abstime, abstime1, abstime2, value1, value2);

#if 1
  return get_optimized_custom_value(abstime, abstime1, abstime2, value1, value2);
#else
  const float y1 = get_node_y(*node1, 0, 2);
  const float y2 = get_node_y(*node2, 0, 2);
  const double midtime = scale_double(1, y1, y2, abstime1, abstime2);
                                      
  if (abstime <= midtime)
    return scale_double(abstime, abstime1, midtime, value1, 1.0);
  else
    return scale_double(abstime, midtime, abstime2, 1.0, value2);
#endif
}
  


// Called from MIXER.cpp in the player thread.
double RT_TEMPOAUTOMATION_get_value(double abstime){
  double ret = 1.0;
  g_tempo_automation.RT_get_value(abstime, ret, custom_get_value);
  return ret;
}

double TEMPOAUTOMATION_get_value(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 1.0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 1.0);

  return g_tempo_automation.at(nodenum).value;
}

double TEMPOAUTOMATION_get_abstime(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 0);

  return g_tempo_automation.at(nodenum).time;
}

int TEMPOAUTOMATION_get_logtype(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, LOGTYPE_LINEAR);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), LOGTYPE_LINEAR);

  return g_tempo_automation.at(nodenum).logtype;
}

int TEMPOAUTOMATION_get_num_nodes(void){
  return g_tempo_automation.size();
}
  
int TEMPOAUTOMATION_add_node(double abstime, double value, int logtype){
  if (abstime < 0)
    abstime = 0;
  
  int ret = g_tempo_automation.add_node(create_node(abstime, value, logtype));

  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
  
  return ret;
}
                              
void TEMPOAUTOMATION_delete_node(int nodenum){
  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < g_tempo_automation.size());

  if (nodenum==0 || nodenum==g_tempo_automation.size()-1) {

    TempoAutomationNode node = g_tempo_automation.at(nodenum);
    node.value = 1.0;
    g_tempo_automation.replace_node(nodenum, node);
    
  } else {
  
    g_tempo_automation.delete_node(nodenum);
    
  }

  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
}

void TEMPOAUTOMATION_set_curr_node(int nodenum){
  if (g_tempo_automation.get_curr_nodenum() != nodenum){
    g_tempo_automation.set_curr_nodenum(nodenum);
    SEQUENCER_update(SEQUPDATE_SONGTEMPO);
  }
}

void TEMPOAUTOMATION_set(int nodenum, double abstime, double value, int logtype){
  int size = g_tempo_automation.size();

  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < size);

  const TempoAutomationNode *prev = nodenum==0 ? NULL : &g_tempo_automation.at(nodenum-1);
  TempoAutomationNode node = g_tempo_automation.at(nodenum);
  const TempoAutomationNode *next = nodenum==size-1 ? NULL : &g_tempo_automation.at(nodenum+1);

  double mintime = prev==NULL ? 0 : next==NULL ? R_MAX(R_MAX(node.time, abstime), SONG_get_length()*MIXER_get_sample_rate()) : prev->time;
  double maxtime = (prev==NULL || next==NULL) ? mintime : next->time;

  abstime = R_BOUNDARIES(mintime, abstime, maxtime);

  value = R_BOUNDARIES(1.0/g_max_tempo, value, g_max_tempo);

  node.time = abstime;
  node.value = value;
  node.logtype = logtype;

  g_tempo_automation.replace_node(nodenum, node);

  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
}


// Does nothing if end_time < length and do_shrink==false.
// Don't shrinks below second last node. (i.e. we don't change a manually created node)
//
void TEMPOAUTOMATION_set_length(double end_time, bool do_shrink){

  R_ASSERT_RETURN_IF_FALSE(end_time >= 0);
  
  int size = g_tempo_automation.size();
  R_ASSERT_RETURN_IF_FALSE(size != 1);
  
  if (size==0){
    
    g_tempo_automation.add_node(create_node(0,        1, LOGTYPE_LINEAR));
    g_tempo_automation.add_node(create_node(end_time, 1, LOGTYPE_LINEAR));
    
  } else {

    const auto &second_last = g_tempo_automation.at(size-2);
    auto last = g_tempo_automation.last();

    if (end_time <= last.time && do_shrink==false)
      return;

    if (end_time <= second_last.time)
      return;
    
    if (last.value==1.0 && second_last.value==1.0){
      last.time = end_time;
      g_tempo_automation.replace_node(size-1, last);
    } else {
      g_tempo_automation.add_node(create_node(end_time,1,LOGTYPE_LINEAR));
    }

  }

  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
}

double TEMPOAUTOMATION_get_length(void){
  if (g_tempo_automation.size()==0)
    return 1.0;
  
  return g_tempo_automation.last().time;
}

void TEMPOAUTOMATION_reset(void){
  g_tempo_automation.reset();
  g_tempo_automation.set_do_paint_nodes(true);
  SEQTEMPO_set_visible(false);
  //TEMPOAUTOMATION_set_length(SONG_get_length(), true);
}


static bool absabstime_scales_linearly_with_abstime(double time){ // time can be either abstime or absabstime.
  if (time < 0.00001)
    return true;
      
  int size = g_tempo_automation.size();

  if (size < 2)
    return true;

  const TempoAutomationNode &node1 = g_tempo_automation.at(0);
  const TempoAutomationNode &node2 = g_tempo_automation.at(1);
  
  double val1 = node1.value;
  double val2 = node2.value;

  if (time <= node2.time && fabs(val1-1.0)<0.000001 && (node1.logtype==LOGTYPE_HOLD || fabs(val2-1.0)<0.000001))
    return true;

  return false;
}
                                                    
// A little bit tricky to calculate this more efficiently when we want a very accurate value. Fortunately, the song needs to be several hours long before noticing that it's slow.
static int64_t get_absabstime_from_abstime(double goal){

  int64_t absabstime = 0;
  double abstime = 0.0;

  int size = g_tempo_automation.size();

  const TempoAutomationNode *node1 = &g_tempo_automation.at(0);
  const TempoAutomationNode *node2 = &g_tempo_automation.at(1);
  int i = 1;
  //fprintf(stderr,"************** time node2: %f\n", node2->time / 44100.0);

  
  while(true){
    if (abstime >= goal)
      return absabstime;
    
    double tempo;
    
    if (i==size) {
      tempo = 1.0;
    } else {
      if (!g_tempo_automation.RT_get_value(abstime, node1, node2, tempo, custom_get_value)){
        i++;
        if (i<size){          
          node1 = node2;
          node2 = &g_tempo_automation.at(i);
        }
        continue;
      }
    }

    abstime += (double)RADIUM_BLOCKSIZE * tempo;
    absabstime += RADIUM_BLOCKSIZE;
  }
}

int64_t TEMPOAUTOMATION_get_absabstime(double goal){

#if !defined(RELEASE)
  printf(".......... Note: TEMPOAUTOMATION_get_absabstime(%f) called.\n", goal);
#endif

  if(absabstime_scales_linearly_with_abstime(goal))
    return goal;

  return get_absabstime_from_abstime(goal);
}

// Based on pseudocode for the function BinarySearch_Left found at https://rosettacode.org/wiki/Binary_search
static double BinarySearch_abstime(int64_t absabstime, double low, double high) {
  
  double mid = (low + high) / 2.0;

  if (fabs(low-high) <= 1.0)
    return mid;
  
  int64_t try_ = get_absabstime_from_abstime(mid);

  //printf("Goal: %d. Try: %d, low: %f, mid: %f, high: %f\n", (int)absabstime, (int)try_, low, mid, high);
  
  if (try_==absabstime)
    return mid;

  if (try_ > absabstime)
    return BinarySearch_abstime(absabstime, low, mid);
  else
    return BinarySearch_abstime(absabstime, mid, high);
}

// This function could spend a lot of time.
double TEMPOAUTOMATION_get_abstime_from_absabstime(int64_t absabstime){

#if !defined(RELEASE)
  printf(".......... Note: TEMPOAUTOMATION_get_abstime_from_absabstime(%d) called.\n", (int)absabstime);
#endif
  
  if(absabstime_scales_linearly_with_abstime(absabstime))
    return absabstime;
  
  double low = 0.0;
  double high = SONG_get_length() * MIXER_get_sample_rate();

  int64_t high_absabs = get_absabstime_from_abstime(high);
  if (absabstime >= high_absabs){
    //printf("  PAST end. absabs: %f, high_absabs: %f. Song len: %f\n", (double)absabstime/44100.0, (double)high_absabs/44100.0, high);
    return high + (absabstime-high_absabs);
  }
  
  return BinarySearch_abstime(absabstime, low, high);
}

static TempoAutomationNode create_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_float(state, "abstime");
  return create_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}

void TEMPOAUTOMATION_create_from_state(hash_t *state, double state_samplerate){
  g_max_tempo = HASH_get_float(state, "max_tempo");
  g_tempo_automation.create_from_state(HASH_get_dyn(state, "nodes"), create_node_from_state, state_samplerate);
  SEQTEMPO_set_visible(HASH_get_bool(state, "is_visible"));
  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
}

static hash_t *get_node_state(const TempoAutomationNode &node){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "abstime", node.time);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}


hash_t *TEMPOAUTOMATION_get_state(void){
  hash_t *state = HASH_create(2);

  HASH_put_dyn(state, "nodes", g_tempo_automation.get_state(get_node_state));
  HASH_put_bool(state, "is_visible", SEQTEMPO_is_visible());
  HASH_put_float(state, "max_tempo", g_max_tempo);
  
  return state;
}

double TEMPOAUTOMATION_get_max_tempo(void){
  return g_max_tempo;
}

void TEMPOAUTOMATION_set_max_tempo(double new_max_tempo){
  if (new_max_tempo < 1.0001)
    g_max_tempo = 1.0001;
  else
    g_max_tempo = new_max_tempo;

  SEQUENCER_update(SEQUPDATE_SONGTEMPO);
}

float TEMPOAUTOMATION_get_node_x(int nodenum){
  int64_t start_time = SEQUENCER_get_visible_start_time();
  int64_t end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQTEMPO_get_x1();
  float x2 = SEQTEMPO_get_x2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return scale(node1.time, start_time, end_time, x1, x2);
}

static float get_node_y(const TempoAutomationNode &node, float y1, float y2, void *data){
  return get_node_y(&node, y1, y2);
}

float TEMPOAUTOMATION_get_node_y(int nodenum){
  float y1 = SEQTEMPO_get_y1();
  float y2 = SEQTEMPO_get_y2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return get_node_y(node1, y1, y2, NULL);
}

void TEMPOAUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time){
  
  TEMPOAUTOMATION_set_length(end_time, false);

  g_tempo_automation.paint(p, x1, y1, x2, y2, start_time, end_time, get_qcolor(SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM), get_node_y);
}

