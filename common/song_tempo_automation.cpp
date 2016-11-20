



#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "seqtrack_proc.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"

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
  double abstime;
  double value;
  int logtype;
};
 
}

static radium::SeqAutomation<TempoAutomationNode> g_tempo_automation;


static TempoAutomationNode create_node(double abstime, double value, int logtype){
  TempoAutomationNode node = {.abstime = abstime,
                              .value = value,
                              .logtype = logtype};

  return node;
}



// Called from MIXER.cpp in the player thread.
double RT_TEMPOAUTOMATION_get_value(double abstime){
  return g_tempo_automation.RT_get_value(abstime);
}

double TEMPOAUTOMATION_get_value(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 1.0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 1.0);

  return g_tempo_automation.at(nodenum).value;
}

double TEMPOAUTOMATION_get_abstime(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 0);

  return g_tempo_automation.at(nodenum).abstime;
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
  
  return g_tempo_automation.add_node(create_node(abstime, value, logtype));
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
}

void TEMPOAUTOMATION_set_curr_node(int nodenum){
  if (g_tempo_automation.get_curr_nodenum() != nodenum){
    g_tempo_automation.set_curr_nodenum(nodenum);
    SEQUENCER_update();
  }
}

void TEMPOAUTOMATION_set(int nodenum, double abstime, double value, int logtype){
  int size = g_tempo_automation.size();

  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < size);

  const TempoAutomationNode *prev = nodenum==0 ? NULL : &g_tempo_automation.at(nodenum-1);
  TempoAutomationNode node = g_tempo_automation.at(nodenum);
  const TempoAutomationNode *next = nodenum==size-1 ? NULL : &g_tempo_automation.at(nodenum+1);

  double mintime = prev==NULL ? 0 : next==NULL ? R_MAX(R_MAX(node.abstime, abstime), SONG_get_length()) : prev->abstime;
  double maxtime = (prev==NULL || next==NULL) ? mintime : next->abstime;

  abstime = R_BOUNDARIES(mintime, abstime, maxtime);

  value = R_BOUNDARIES(0.01, value, 2);

  node.abstime = abstime;
  node.value = value;
  node.logtype = logtype;

  g_tempo_automation.replace_node(nodenum, node);
}


// Does nothing if end_time < length and do_shrink==false.
// Don't shrinks below second last node. (i.e. we don't change a manually created node)
//
void TEMPOAUTOMATION_set_length(double end_time, bool do_shrink){

  R_ASSERT_RETURN_IF_FALSE(end_time >= 0);
  
  int size = g_tempo_automation.size();
  
  if (size==0){
    
    g_tempo_automation.add_node(create_node(0,        1, LOGTYPE_LINEAR));
    g_tempo_automation.add_node(create_node(end_time, 1, LOGTYPE_LINEAR));
    
  } else {

    const auto &second_last = g_tempo_automation.at(size-2);
    auto last = g_tempo_automation.last();

    if (end_time <= last.abstime && do_shrink==false)
      return;

    if (end_time <= second_last.abstime)
      return;
    
    if (last.value==1.0 && second_last.value==1.0){
      last.abstime = end_time;
      g_tempo_automation.replace_node(size-1, last);
    } else {
      g_tempo_automation.add_node(create_node(end_time,1,LOGTYPE_LINEAR));
    }

  }
}

double TEMPOAUTOMATION_get_length(void){
  if (g_tempo_automation.size()==0)
    return 1.0;
  
  return g_tempo_automation.last().abstime;
}

void TEMPOAUTOMATION_reset(void){
  g_tempo_automation.reset();
  SEQTEMPO_set_visible(false);
  //TEMPOAUTOMATION_set_length(SONG_get_length(), true);
}


// A little bit tricky to calculate this more efficiently when we want a very accurate value. Fortunately, the song needs to be several hours long before noticing that this function is slow.
double TEMPOAUTOMATION_get_absabstime(double goal){
  int64_t absabstime = 0;
  double abstime = 0.0;

  int size = g_tempo_automation.size();

  if (size < 2)
    return goal;
  
  const TempoAutomationNode *node1 = &g_tempo_automation.at(0);
  const TempoAutomationNode *node2 = &g_tempo_automation.at(1);
  int i = 1;
  
  while(true){
    if (abstime >= goal)
      return absabstime;
    
    double tempo;
    
    if (i==size) {
      tempo = 1.0;
    } else {
      if (!g_tempo_automation.RT_get_value(abstime, node1, node2, tempo)){
        i++;
        node1 = node2;
        node2 = &g_tempo_automation.at(i);
        continue;
      }
    }

    abstime += (double)RADIUM_BLOCKSIZE * tempo;
    absabstime += RADIUM_BLOCKSIZE;
  }
}


static TempoAutomationNode create_node_from_state(hash_t *state){
  return create_node(HASH_get_float(state, "abstime"),
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}


void TEMPOAUTOMATION_create_from_state(hash_t *state){
  g_tempo_automation.create_from_state(HASH_get_hash(state, "nodes"), create_node_from_state);
  SEQTEMPO_set_visible(HASH_get_bool(state, "is_visible"));
}

static hash_t *get_node_state(const TempoAutomationNode &node){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "abstime", node.abstime);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}


hash_t *TEMPOAUTOMATION_get_state(void){
  hash_t *state = HASH_create(2);

  HASH_put_hash(state, "nodes", g_tempo_automation.get_state(get_node_state));
  HASH_put_bool(state, "is_visible", SEQTEMPO_is_visible());
  
  return state;
}

float TEMPOAUTOMATION_get_node_x(int nodenum){
  int64_t start_time = SEQUENCER_get_visible_start_time();
  int64_t end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQTEMPO_get_x1();
  float x2 = SEQTEMPO_get_x2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return scale(node1.abstime, start_time, end_time, x1, x2);
}

static float get_node_y(const TempoAutomationNode &node, float y1, float y2){
  return scale(node.value, 2, 0, y1, y2);
}

float TEMPOAUTOMATION_get_node_y(int nodenum){
  float y1 = SEQTEMPO_get_y1();
  float y2 = SEQTEMPO_get_y2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return get_node_y(node1, y1, y2);
}

void TEMPOAUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time){
  
  TEMPOAUTOMATION_set_length(end_time, false);

  g_tempo_automation.paint(p, x1, y1, x2, y2, start_time, end_time, get_qcolor(SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM), get_node_y);
  
}

