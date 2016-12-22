



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

struct AutomationNode{
  double abstime;
  double value;
  int logtype;

  AutomationNode(double abstime, double value, int logtype)
    : abstime(abstime)
    , value(value)
    , logtype(logtype)
  {
  }
};

struct Automation{
  radium::SeqAutomation<AutomationNode> automation;
  struct Patch *patch; // I'm pretty sure we can use SoundPlugin directly now... I don't think patch->patchdata changes anymore.
  int effect_num;
};

class SeqtrackAutomation{
  
private:
  
  struct SeqTrack *_seqtrack;
  Radium::Vector<Automation*> _automations;
  
public:
  
  SeqTrackAutomation(struct SeqTrack *seqtrack)
    :_seqtrack(add_gc_root(seqtrack))
  {
  }

  ~SeqTrackAutomation(){
    for(auto *automation : _automations)
      delete automation;
    
    remove_gc_root(_seqtrack);
  }

  void add_automation(struct Patch *patch, int effect_num, double abstime1, double value1, int logtype, double abstime2, double value2){
    Automation *automation = new Automation(patch, effect_num);

    automation.automation.add_node(AutomationNode(abstime1, value1, logtype));
    automation.automation.add_node(AutomationNode(abstime2, value2, logtype));

    {
      _automations.ensure_there_is_room_for_one_more_without_having_to_allocate_memory();
      {
        radium::PlayerLock lock;    
        _automations.push_back(_automation);
      }  
      _automations.post_add();
    }
    
  }

private:
  
  Automation *find_automation(struct Patch *patch, int effect_num){
    for(auto *automation : _automations)
      if (automation->patch==patch && automation->effect_num==effect_num)
        return automation;

    RError("SeqTrackAutomation::find_automation: Could not find %s / %d", patch->name, effect_num);
    return NULL;    
  }
  
public:
  
  void remove_automation(struct Patch *patch, int effect_num){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;

    {
      radium::PlayerLock lock;    
      _automations.remove(automation);
    }

    delete automation;
  }

  void add_automation_node(struct Patch *patch, int effect_num, double abstime, double value, int logtype){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;

    automation->add_node(AutomationNode(abstime, value, logtype));
  }
  
  void remove_automation_node(struct Patch *patch, int effect_num, int nodenum){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;

    automation->delete_node(nodenum);
  }

  void change_automation_node(struct Patch *patch, int effect_num, int nodenum, double abstime, double value, int logtype){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;

    automation->replace_node(nodenum, AutomationNode(abstime, value, logtype));
  }
};
 
}

struct SeqtrackAutomation *SEQTRACK_AUTOMATION_create(struct SeqTrack *seqtrack){
  return new SeqtrackAutomation(seqtrack);
}

void SEQTRACK_AUTOMATION_add_automation(struct SeqTrack *seqtrack, struct Patch *patch, int effect_num){
  
}


// Called from MIXER.cpp in the player thread.
double RT_SEQTRACK_AUTOMATION_get_value(double abstime){
  return g_tempo_automation.RT_get_value(abstime);
}

double SEQTRACK_AUTOMATION_get_value(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 1.0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 1.0);

  return g_tempo_automation.at(nodenum).value;
}

double SEQTRACK_AUTOMATION_get_abstime(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, 0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 0);

  return g_tempo_automation.at(nodenum).abstime;
}

int SEQTRACK_AUTOMATION_get_logtype(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>=0, LOGTYPE_LINEAR);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), LOGTYPE_LINEAR);

  return g_tempo_automation.at(nodenum).logtype;
}

int SEQTRACK_AUTOMATION_get_num_nodes(void){
  return g_tempo_automation.size();
}
  
int SEQTRACK_AUTOMATION_add_node(double abstime, double value, int logtype){
  if (abstime < 0)
    abstime = 0;
  
  int ret = g_tempo_automation.add_node(create_node(abstime, value, logtype));

  SEQUENCER_update();
  
  return ret;
}
                              
void SEQTRACK_AUTOMATION_delete_node(int nodenum){
  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < g_tempo_automation.size());

  if (nodenum==0 || nodenum==g_tempo_automation.size()-1) {

    AutomationNode node = g_tempo_automation.at(nodenum);
    node.value = 1.0;
    g_tempo_automation.replace_node(nodenum, node);
    
  } else {
  
    g_tempo_automation.delete_node(nodenum);
    
  }

  SEQUENCER_update();
}

void SEQTRACK_AUTOMATION_set_curr_node(int nodenum){
  if (g_tempo_automation.get_curr_nodenum() != nodenum){
    g_tempo_automation.set_curr_nodenum(nodenum);
    SEQUENCER_update();
  }
}

void SEQTRACK_AUTOMATION_set(int nodenum, double abstime, double value, int logtype){
  int size = g_tempo_automation.size();

  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < size);

  const AutomationNode *prev = nodenum==0 ? NULL : &g_tempo_automation.at(nodenum-1);
  AutomationNode node = g_tempo_automation.at(nodenum);
  const AutomationNode *next = nodenum==size-1 ? NULL : &g_tempo_automation.at(nodenum+1);

  double mintime = prev==NULL ? 0 : next==NULL ? R_MAX(R_MAX(node.abstime, abstime), SONG_get_length()) : prev->abstime;
  double maxtime = (prev==NULL || next==NULL) ? mintime : next->abstime;

  abstime = R_BOUNDARIES(mintime, abstime, maxtime);

  value = R_BOUNDARIES(1.0/g_max_tempo, value, g_max_tempo);

  node.abstime = abstime;
  node.value = value;
  node.logtype = logtype;

  g_tempo_automation.replace_node(nodenum, node);

  SEQUENCER_update();
}


// Does nothing if end_time < length and do_shrink==false.
// Don't shrinks below second last node. (i.e. we don't change a manually created node)
//
void SEQTRACK_AUTOMATION_set_length(double end_time, bool do_shrink){

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

  SEQUENCER_update();
}

double SEQTRACK_AUTOMATION_get_length(void){
  if (g_tempo_automation.size()==0)
    return 1.0;
  
  return g_tempo_automation.last().abstime;
}

void SEQTRACK_AUTOMATION_reset(void){
  g_tempo_automation.reset();
  SEQTEMPO_set_visible(false);
  //SEQTRACK_AUTOMATION_set_length(SONG_get_length(), true);
}


// A little bit tricky to calculate this more efficiently when we want a very accurate value. Fortunately, the song needs to be several hours long before noticing that this function is slow.
double SEQTRACK_AUTOMATION_get_absabstime(double goal){
  int64_t absabstime = 0;
  double abstime = 0.0;

  int size = g_tempo_automation.size();

  if (size < 2)
    return goal;
  
  const AutomationNode *node1 = &g_tempo_automation.at(0);
  const AutomationNode *node2 = &g_tempo_automation.at(1);
  int i = 1;
  
  while(true){
    if (abstime >= goal)
      return absabstime;
    
    double tempo;
    
    if (i==size) {
      tempo = 1.0;
    } else {
      if (!g_tempo_automation.RT_get_value(abstime, node1, node2, tempo, custom_get_value)){
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


static AutomationNode create_node_from_state(hash_t *state){
  return create_node(HASH_get_float(state, "abstime"),
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}


void SEQTRACK_AUTOMATION_create_from_state(hash_t *state){
  g_max_tempo = HASH_get_float(state, "max_tempo");
  g_tempo_automation.create_from_state(HASH_get_hash(state, "nodes"), create_node_from_state);
  SEQTEMPO_set_visible(HASH_get_bool(state, "is_visible"));
  SEQUENCER_update();
}

static hash_t *get_node_state(const AutomationNode &node){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "abstime", node.abstime);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}


hash_t *SEQTRACK_AUTOMATION_get_state(void){
  hash_t *state = HASH_create(2);

  HASH_put_hash(state, "nodes", g_tempo_automation.get_state(get_node_state));
  HASH_put_bool(state, "is_visible", SEQTEMPO_is_visible());
  HASH_put_float(state, "max_tempo", g_max_tempo);
  
  return state;
}

double SEQTRACK_AUTOMATION_get_max_tempo(void){
  return g_max_tempo;
}

void SEQTRACK_AUTOMATION_set_max_tempo(double new_max_tempo){
  if (new_max_tempo < 1.0001)
    g_max_tempo = 1.0001;
  else
    g_max_tempo = new_max_tempo;

  SEQUENCER_update();
}

float SEQTRACK_AUTOMATION_get_node_x(int nodenum){
  int64_t start_time = SEQUENCER_get_visible_start_time();
  int64_t end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQTEMPO_get_x1();
  float x2 = SEQTEMPO_get_x2();
  
  const AutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return scale(node1.abstime, start_time, end_time, x1, x2);
}

static float get_node_y(const AutomationNode &node, float y1, float y2){
  return get_node_y(&node, y1, y2);
}

float SEQTRACK_AUTOMATION_get_node_y(int nodenum){
  float y1 = SEQTEMPO_get_y1();
  float y2 = SEQTEMPO_get_y2();
  
  const AutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return get_node_y(node1, y1, y2);
}

void SEQTRACK_AUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time){
  
  SEQTRACK_AUTOMATION_set_length(end_time, false);

  g_tempo_automation.paint(p, x1, y1, x2, y2, start_time, end_time, get_qcolor(SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM), get_node_y);
}

