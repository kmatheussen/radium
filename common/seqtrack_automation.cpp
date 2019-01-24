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
#include "patch_proc.h"
#include "seqtrack_proc.h"
#include "instruments_proc.h"
#include "Vector.hpp"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"

#include "song_tempo_automation_proc.h"

#include <QPainter>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


#include "seqtrack_automation_proc.h"


// g_curr_seqtrack_automation_seqtrack is set to NULL by ~SeqtrackAutomation or SEQTRACK_AUTOMATION_cancel_curr_automation.
// Note that it not possible for a seqtrack to be valid, and its seqtrack->seqtrackautomation value to be invalid, or
// vice versa, so it's fine using the SeqtrackAutomation destructor for setting this value to NULL when the seqtrack is deleted.
static struct SeqTrack *g_curr_seqtrack_automation_seqtrack = NULL;
static int g_curr_seqtrack_automation_num = -1;

static void assert_no_curr_seqtrack(void){
  if (g_curr_seqtrack_automation_seqtrack != NULL){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqtrack_automation_seqtrack = NULL;
  }

  if (g_curr_seqtrack_automation_num != -1){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqtrack_automation_num = -1;
  }
}

static void set_no_curr_seqtrack(void){
  g_curr_seqtrack_automation_seqtrack = NULL;
  g_curr_seqtrack_automation_num = -1;
}

static void update_seqtrack(const struct SeqTrack *seqtrack, double start_time, double end_time){
  SEQTRACK_update_with_nodes(seqtrack, floor(start_time), ceil(end_time));
}

namespace{

struct AutomationNode{
  double time; // seqtime format
  double value;
  int logtype;
};

static AutomationNode create_node(double seqtime, double value, int logtype){
  AutomationNode node = {
    .time = seqtime,
    .value = value,
    .logtype = logtype
  };
  return node;
}

static hash_t *get_node_state(const AutomationNode &node, void*){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "seqtime", node.time);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}

static AutomationNode create_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_float(state, "seqtime");
  return create_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}


struct Automation{
  radium::SeqAutomation<AutomationNode> automation;
  struct Patch *patch; // I'm pretty sure we can use SoundPlugin directly now... I don't think patch->patchdata changes anymore.
  int effect_num = -1;
  double last_value = -1.0;
  QColor color;
  bool _is_enabled = true;
  
  bool islegalnodenum(int nodenum){
    return nodenum>=0 && (nodenum<=automation.size()-1);
  }

  hash_t *get_state(void) const {
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;

    hash_t *state = HASH_create(3);
    HASH_put_int(state, "patch", patch->id);
    HASH_put_chars(state, "effect_name", effect_num==-1 ? "<effect not found>" : PLUGIN_get_effect_name(plugin, effect_num));
    HASH_put_bool(state, "is_enabled", _is_enabled);
    HASH_put_dyn(state, "automation", automation.get_state(get_node_state, NULL));
    return state;
  }

  Automation(struct Patch *patch, int effect_num)
    : patch(patch)
    , effect_num(effect_num)
  {
    
    SEQTRACK_AUTOMATION_cancel_curr_automation();
    
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

    R_ASSERT(effect_num >= 0);
    R_ASSERT(effect_num < plugin->type->num_effects + NUM_SYSTEM_EFFECTS);

    effect_num = R_BOUNDARIES(0, effect_num, plugin->type->num_effects + NUM_SYSTEM_EFFECTS - 1);
    
    color = get_qcolor(get_effect_color(plugin, effect_num));
  }

  Automation(hash_t *state, double state_samplerate){
    if(HASH_has_key(state, "is_enabled"))
      _is_enabled = HASH_get_bool(state, "is_enabled");
    
    patch = PATCH_get_from_id(HASH_get_int(state, "patch"));
    automation.create_from_state(HASH_get_dyn(state, "automation"), create_node_from_state, state_samplerate);

    const char *effect_name = HASH_get_chars(state, "effect_name");

    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

    const SoundPluginType *type=plugin->type;
    int i;
    
    for(i=0;i<type->num_effects+NUM_SYSTEM_EFFECTS;i++){
      if (!strcmp(PLUGIN_get_effect_name(plugin, i), effect_name))
        break;
    }
    
    if (i==type->num_effects+NUM_SYSTEM_EFFECTS){
      GFX_Message(NULL, "Sequencer automation: Could not find effect named \"%s\" in %s/%s", effect_name, type->type_name, type->name);
#if !defined(RELEASE)
      R_ASSERT(false);
#endif
    }else{
      effect_num = i;//HASH_get_int32(state, "effect_num");
      color = get_qcolor(get_effect_color(plugin, effect_num));
    }
  }

  ~Automation(){
    assert_no_curr_seqtrack();
  }

  void update(const struct SeqTrack *seqtrack) const {
    update_seqtrack(seqtrack, automation.at(0).time, automation.last().time);
  }

  void update(const struct SeqTrack *seqtrack, int nodenum) const {
    update_seqtrack(seqtrack, automation.at(R_MAX(0, nodenum-1)).time, automation.at(R_MIN(automation.size()-1, nodenum+1)).time);
  }
};

}

struct SeqtrackAutomation {
  
private:
  
  //struct SeqTrack *_seqtrack; // Not used, but can be practical when debugging.

#if !defined(RELEASE)
  int magic = 918345; // Check that we are freeing the correct data.
#endif

  
public:

  struct SeqTrack *_seqtrack;
  radium::Vector<Automation*> _automations;
  
  SeqtrackAutomation(struct SeqTrack *seqtrack, double state_samplerate, const hash_t *state = NULL)
    :_seqtrack(seqtrack)
  {
    SEQTRACK_AUTOMATION_cancel_curr_automation();
          
    if (state != NULL) {
      int size = HASH_get_array_size(state, "automation");
      
      for(int i = 0 ; i < size ; i++){
        Automation *automation = new Automation(HASH_get_hash_at(state, "automation", i), state_samplerate);
        if (automation->effect_num >= 0)
          _automations.push_back(automation);
      }
    }
  }

  ~SeqtrackAutomation(){
#if !defined(RELEASE)
    if (magic != 918345)
      abort();
#endif

    if (g_curr_seqtrack_automation_seqtrack!=NULL && g_curr_seqtrack_automation_seqtrack->seqtrackautomation==this){
      set_no_curr_seqtrack(); // It's a little bit unclear what data is available and not right now. We can think through the situation, and probably conclude that it's fine to call SEQTRACK_AUTOMATION_cancel_curr_automation() no matter what, but this is also fine, probably clearer, and much simpler since we don't have to worry whether it's safe to call SEQTRACK_AUTOMATION_cancel_curr_automation.
    } else {
      SEQTRACK_AUTOMATION_cancel_curr_automation();
    }

    for(auto *automation : _automations)
      delete automation;

    assert_no_curr_seqtrack();
  }
  
  hash_t *get_state(void) const {
    hash_t *state = HASH_create(_automations.size());
    for(int i = 0 ; i < _automations.size() ; i++)
      HASH_put_hash_at(state, "automation", i, _automations.at(i)->get_state());
    return state;
  }

  /*
  //
  // Not used. Instead, we just do: auto *a = SEQTRACK_AUTOMATION_create(seqtrack, automation_state); lock() = seqtrack->seqtrackautomation = a ; unlock()
  //
  void apply_state(const hash_t *state){
    R_ASSERT_RETURN_IF_FALSE(state != NULL);
    
    int size = HASH_get_array_size(state, "automation");

    radium::Vector<Automation*> new_automations;
    
    for(int i = 0 ; i < size ; i++){
      Automation *automation = new Automation(HASH_get_hash_at(state, "automation", i));
      if (automation->effect_num >= 0)
        new_automations.push_back(automation);
    }

    _automations.ensure2(new_automations.size());
    
    {
      radium::PlayerLock lock;    
      _automations.clear();

      for(auto *automation : new_automations)
        _automations.push_back(automation);
    }
  }
  */

  int add_automation(struct Patch *patch, int effect_num, double seqtime1, double value1, int logtype, double seqtime2, double value2, int &nodenum1, int &nodenum2){
    Automation *automation = find_automation(patch, effect_num, true);

    bool already_here;
    
    if (automation != NULL){
      already_here = true;
      automation->update(_seqtrack);
    } else {
      already_here = false;
      automation = new Automation(patch, effect_num);
    }

    nodenum1 = automation->automation.add_node(create_node(seqtime1, value1, logtype));
    nodenum2 = automation->automation.add_node(create_node(seqtime2, value2, logtype));
    
    if (already_here==false){
      _automations.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);
      {
        radium::PlayerLock lock;    
        _automations.push_back(automation);
      }  
      _automations.post_add();
    }

    /*
    automation->automation.set_curr_nodenum(ret);
    
    SEQTRACK_AUTOMATION_set_curr_automation(seqtrack, );
    */
    
    automation->update(_seqtrack);
    
    return _automations.find_pos(automation);
  }

private:
  
  Automation *find_automation(struct Patch *patch, int effect_num, bool may_return_null=false) const {
    for(auto *automation : _automations)
      if (automation->patch==patch && automation->effect_num==effect_num)
        return automation;

    if (may_return_null==false)
      RError("SeqtrackAutomation::find_automation: Could not find %s / %d", patch->name, effect_num);

    return NULL;    
  }


public:

  bool is_enabled(const Automation *automation) const {
    return automation->_is_enabled;
  }
    
  bool is_enabled(struct Patch *patch, int effect_num) const {
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return false;

    return is_enabled(automation);
  }

  void set_enabled(Automation *automation, bool is_enabled) {
    if (automation->_is_enabled==is_enabled)
      return;

    {
      radium::PlayerLock lock;
      automation->_is_enabled = is_enabled;
    }

    automation->update(_seqtrack);
  }
  
  void set_enabled(struct Patch *patch, int effect_num, bool is_enabled){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;
    
    set_enabled(automation, is_enabled);
  }
  
  bool islegalautomation(int automationnum) const {
    return automationnum>=0 && (automationnum<=_automations.size()-1);
  }

  void remove_automation(Automation *automation){
    SEQTRACK_AUTOMATION_cancel_curr_automation();
    
    {
      radium::PlayerLock lock;    
      _automations.remove(automation);
    }

    delete automation;
  }

  void remove_automation(struct Patch *patch, int effect_num){
    Automation *automation = find_automation(patch, effect_num);
    if (automation==NULL)
      return;

    remove_automation(automation);
  }

  void remove_all_automations(struct Patch *patch){
    SEQTRACK_AUTOMATION_cancel_curr_automation();
    
    QVector<Automation*> to_remove;

    for(auto *automation : _automations)
      if (automation->patch==patch)
        to_remove.push_back(automation);

    if(to_remove.size()>0){
      {
        radium::PlayerLock lock;
        for(auto *automation : to_remove)
          _automations.remove(automation);
      }
      for(auto *automation : to_remove)
        delete automation;
    }
  }

  void replace_all_automations(struct Patch *old_patch, struct Patch *new_patch){
    SEQTRACK_AUTOMATION_cancel_curr_automation();
    
    R_ASSERT(old_patch!=NULL);
    R_ASSERT(new_patch!=NULL);

    QVector<Automation*> to_remove;

    SoundPlugin *old_plugin = (SoundPlugin*) old_patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE(old_plugin!=NULL);
    
    const SoundPluginType *old_type = old_plugin->type;
    int num_old_effects = old_type->num_effects;
    
    SoundPlugin *new_plugin = (SoundPlugin*) new_patch->patchdata;
    const SoundPluginType *new_type = new_plugin->type;
    int num_new_effects = new_type->num_effects;
    
    bool same_instrument_type = false;
  
    if(true
       && !strcmp(old_type->type_name, new_type->type_name)
       && !strcmp(old_type->name,      new_type->name)
       )
      same_instrument_type = true;
    
    for(auto *automation : _automations) {

      if (automation->patch==old_patch) {
        
        if (same_instrument_type){

          radium::PlayerLock lock;
          automation->patch = new_patch;

        } else if (automation->effect_num >= num_old_effects) {

          automation->color = get_qcolor(get_effect_color(new_plugin, automation->effect_num));

          {
            radium::PlayerLock lock;
            automation->effect_num = num_new_effects + (automation->effect_num - num_old_effects);
            automation->patch = new_patch;
          }

        } else {

          to_remove.push_back(automation);

        }
        
      }

    }

    if(to_remove.size()>0){
      {
        radium::PlayerLock lock;
        for(auto *automation : to_remove)
          _automations.remove(automation);
      }
      for(auto *automation : to_remove)
        delete automation;
    }

  }
};
 

struct SeqtrackAutomation *SEQTRACK_AUTOMATION_create(struct SeqTrack *seqtrack, const hash_t *automation_state, double state_samplerate){
  return new SeqtrackAutomation(seqtrack, state_samplerate, automation_state);
}

void SEQTRACK_AUTOMATION_free(struct SeqtrackAutomation *seqtrackautomation){
  delete seqtrackautomation;
}

void SEQTRACK_AUTOMATION_replace_all_automations(struct Patch *old_patch, struct Patch *new_patch){
  R_ASSERT_RETURN_IF_FALSE(old_patch!=NULL);

  if (old_patch->instrument==get_MIDI_instrument())
    return;

  if (new_patch!=NULL && new_patch->instrument==get_MIDI_instrument())
    new_patch = NULL;

  ALL_SEQTRACKS_FOR_EACH(){
    if(new_patch==NULL)
      seqtrack->seqtrackautomation->remove_all_automations(old_patch);
    else
      seqtrack->seqtrackautomation->replace_all_automations(old_patch, new_patch);
  }END_ALL_SEQTRACKS_FOR_EACH;

  SEQUENCER_update(SEQUPDATE_TIME);
}

int SEQTRACK_AUTOMATION_add_automation(struct SeqtrackAutomation *seqtrackautomation, struct Patch *patch, int effect_num, double seqtime1, double value1, int logtype, double seqtime2, double value2, int *nodenum1, int *nodenum2){
  
  int nodenum1b,nodenum2b;
  
  int automationnum = seqtrackautomation->add_automation(patch, effect_num, seqtime1, value1, logtype, seqtime2, value2, nodenum1b, nodenum2b);
  
  if (nodenum1!=NULL)
    *nodenum1 = nodenum1b;
  
  if (nodenum2!=NULL)
    *nodenum2 = nodenum2b;
  
  return automationnum;
}

int SEQTRACK_AUTOMATION_get_num_automations(struct SeqtrackAutomation *seqtrackautomation){
  return seqtrackautomation->_automations.size();
}

struct Patch *SEQTRACK_AUTOMATION_get_patch(struct SeqtrackAutomation *seqtrackautomation, int automationnum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), (struct Patch*)get_audio_instrument()->patches.elements[0]);
  return seqtrackautomation->_automations[automationnum]->patch;
}

int SEQTRACK_AUTOMATION_get_effect_num(struct SeqtrackAutomation *seqtrackautomation, int automationnum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);
  return seqtrackautomation->_automations[automationnum]->effect_num;
}

bool SEQTRACK_AUTOMATION_is_enabled(struct SeqtrackAutomation *seqtrackautomation, int automationnum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), false);
  return seqtrackautomation->is_enabled(seqtrackautomation->_automations[automationnum]);
}
// husk load/save
void SEQTRACK_AUTOMATION_set_enabled(struct SeqtrackAutomation *seqtrackautomation, int automationnum, bool is_enabled){
  R_ASSERT_RETURN_IF_FALSE(seqtrackautomation->islegalautomation(automationnum));
  seqtrackautomation->set_enabled(seqtrackautomation->_automations[automationnum], is_enabled);
  SEQUENCER_update(SEQUPDATE_TIME);
}

double SEQTRACK_AUTOMATION_get_value(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0.5);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE2(automation->islegalnodenum(nodenum), 0.5);

  return automation->automation.at(nodenum).value;
}

double SEQTRACK_AUTOMATION_get_seqtime(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0.5);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE2(automation->islegalnodenum(nodenum), 0.5);

  return automation->automation.at(nodenum).time;
}

int SEQTRACK_AUTOMATION_get_logtype(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE2(automation->islegalnodenum(nodenum), 0);

  return automation->automation.at(nodenum).logtype;
}

int SEQTRACK_AUTOMATION_get_num_nodes(struct SeqtrackAutomation *seqtrackautomation, int automationnum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  return automation->automation.size();
}
  

int SEQTRACK_AUTOMATION_add_node(struct SeqtrackAutomation *seqtrackautomation, int automationnum, double seqtime, double value, int logtype){
  if (seqtime < 0)
    seqtime = 0;

  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];

  value = R_BOUNDARIES(0.0, value, 1.0);

  int ret = automation->automation.add_node(create_node(seqtime, value, logtype));
  automation->update(seqtrackautomation->_seqtrack, ret);
    
  return ret;
}
                              
void SEQTRACK_AUTOMATION_delete_node(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int nodenum){
  R_ASSERT_RETURN_IF_FALSE(seqtrackautomation->islegalautomation(automationnum));

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE(automation->islegalnodenum(nodenum));

  automation->update(seqtrackautomation->_seqtrack, nodenum);
  
  if (automation->automation.size()<=2) {
    seqtrackautomation->remove_automation(automation);
  } else {
    automation->automation.delete_node(nodenum);
    automation->update(seqtrackautomation->_seqtrack);
  }
}

static void set_curr_node(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int new_nodenum, bool do_cancel){
  R_ASSERT_RETURN_IF_FALSE(seqtrackautomation->islegalautomation(automationnum));

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  if(do_cancel)
    R_ASSERT_RETURN_IF_FALSE(new_nodenum==-1);
  else
    R_ASSERT_RETURN_IF_FALSE(automation->islegalnodenum(new_nodenum));

  int old_nodenum = automation->automation.get_curr_nodenum();
  
  if (old_nodenum != new_nodenum){
    
    if(old_nodenum >= 0)
      automation->update(seqtrackautomation->_seqtrack, old_nodenum);
    
    automation->automation.set_curr_nodenum(new_nodenum);

    if(new_nodenum >= 0)
      automation->update(seqtrackautomation->_seqtrack, new_nodenum);
    
  }
}

void SEQTRACK_AUTOMATION_set_curr_node(struct SeqtrackAutomation *seqtrackautomation, int automationnum, int nodenum){
  set_curr_node(seqtrackautomation, automationnum, nodenum, false);
}

void SEQTRACK_AUTOMATION_cancel_curr_node(struct SeqtrackAutomation *seqtrackautomation, int automationnum){
  set_curr_node(seqtrackautomation, automationnum, -1, true);
}

// Legal to call even if there is already a current automation.
void SEQTRACK_AUTOMATION_set_curr_automation(struct SeqTrack *seqtrack, int automationnum){  
  struct SeqtrackAutomation *seqtrackautomation = seqtrack->seqtrackautomation;
  
  R_ASSERT_RETURN_IF_FALSE(seqtrackautomation->islegalautomation(automationnum));

  struct Automation *curr_automation = seqtrackautomation->_automations.at(automationnum);
  if (curr_automation->automation.do_paint_nodes()){
    
    R_ASSERT_NON_RELEASE(seqtrack==g_curr_seqtrack_automation_seqtrack);
    R_ASSERT_NON_RELEASE(automationnum==g_curr_seqtrack_automation_num);
    
  } else {

    SEQTRACK_AUTOMATION_cancel_curr_automation(); // Update old graphics.

    curr_automation->automation.set_do_paint_nodes(true);
    curr_automation->update(seqtrack);

  }
  
  g_curr_seqtrack_automation_seqtrack=seqtrack;
  g_curr_seqtrack_automation_num=automationnum;
}

// May be called if it there is no current automation.
void SEQTRACK_AUTOMATION_cancel_curr_automation(void){
  
  struct SeqTrack *seqtrack = g_curr_seqtrack_automation_seqtrack;
  int automation_num = g_curr_seqtrack_automation_num;
  
  if (seqtrack==NULL){

    R_ASSERT_NON_RELEASE(automation_num==-1);
    
  } else {

    SeqtrackAutomation *seqtrackautomation = seqtrack->seqtrackautomation;
    
    if (automation_num < 0 || automation_num >= seqtrackautomation->_automations.size()){
      
      R_ASSERT_NON_RELEASE(false);
      
    } else {
    
      Automation *automation = seqtrackautomation->_automations.at(automation_num);
      R_ASSERT_RETURN_IF_FALSE(automation!=NULL);
      
      if (automation->automation.do_paint_nodes()){
        automation->automation.set_do_paint_nodes(false);
        automation->update(seqtrackautomation->_seqtrack);
      }else{
        R_ASSERT_NON_RELEASE(false);
      }
      
    }
  }

  set_no_curr_seqtrack();
}


int SEQTRACK_AUTOMATION_get_curr_automation(struct SeqtrackAutomation *seqtrackautomation){
  int ret = 0;

  for(auto *automation : seqtrackautomation->_automations){
    if (automation->automation.do_paint_nodes())
      return ret;

    ret++;
  }

  return -1;
}

void SEQTRACK_AUTOMATION_set(struct SeqTrack *seqtrack, int automationnum, int nodenum, double seqtime, double value, int logtype){
  struct SeqtrackAutomation *seqtrackautomation = seqtrack->seqtrackautomation;

  R_ASSERT_RETURN_IF_FALSE(seqtrackautomation->islegalautomation(automationnum));

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE(automation->islegalnodenum(nodenum));

  int size = automation->automation.size();

  const AutomationNode *prev = nodenum==0 ? NULL : &automation->automation.at(nodenum-1);
  AutomationNode node = automation->automation.at(nodenum);
  const AutomationNode *next = nodenum==size-1 ? NULL : &automation->automation.at(nodenum+1);

  if (prev != NULL)
    seqtime = R_MAX(prev->time, seqtime);

  if (next != NULL)
    seqtime = R_MIN(next->time, seqtime);

  value = R_BOUNDARIES(0.0, value, 1.0);

  node.time = seqtime;
  node.value = value;
  node.logtype = logtype;

  if(prev==NULL || next==NULL)
    automation->update(seqtrackautomation->_seqtrack, nodenum); // If node is first or last, and we shrink time span of automation.
  
  automation->automation.replace_node(nodenum, node);

  automation->update(seqtrackautomation->_seqtrack, nodenum);
}


void SEQTRACK_AUTOMATION_call_me_before_starting_to_play_song_MIDDLE(struct SeqTrack *seqtrack, int64_t seqtime){

  for(auto *automation : seqtrack->seqtrackautomation->_automations){

    if (automation->_is_enabled==false)
      continue;

    /*
    printf("    Init automation patch \"%s\". Abstime: %d (%d), Start: %f, [0]: %f. [2]: %f, Cont: %d\n",           
           automation->patch->name,
           (int)abstime,(int)MIXER_get_sample_rate(),
           seqtime,
           automation->automation.at(0).time,
           automation->automation.at(1).time,
           seqtime < automation->automation.at(0).time
           );
    */
    
    if (seqtime < automation->automation.at(0).time)
      continue;
        
    struct Patch *patch = automation->patch;

    R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());
    
    SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
    if (plugin==NULL) {
      
      R_ASSERT_RETURN_IF_FALSE(false);

    } else {

      int size = automation->automation.size();

      float value = -1;
      FX_when when = FX_middle;
      int64_t seqtime_to_send_to_plugin = seqtime;
      
      for(int i = 0 ; i < size-1 ; i++){
        
        const AutomationNode &node1 = automation->automation.at(i);
        const AutomationNode &node2 = automation->automation.at(i+1);
        
        if (seqtime >= node1.time && seqtime < node2.time){
          value = automation->automation.get_value(seqtime, &node1, &node2); // RT_get_value scales properly and takes logtype into account.
          break;
        }
        
        if (i+1 == size-1){ // last node
          when = FX_end;
          value = node2.value;
          seqtime_to_send_to_plugin = node2.time;
          break;
        }
      }

      R_ASSERT(value != -1);
      
      PLUGIN_call_me_before_starting_to_play_song_MIDDLE(plugin,
                                                         seqtime_to_send_to_plugin,
                                                         automation->effect_num,
                                                         value,
                                                         when,
                                                         EFFECT_FORMAT_SCALED);
      
    }
  }
}
  

// Called from seqtrack.cpp. Returns true if there are more things to do.
bool RT_SEQTRACK_AUTOMATION_called_per_block(const struct SeqTrack *seqtrack){
  R_ASSERT_NON_RELEASE2(is_playing() && pc->playtype==PLAYSONG, false);

  bool more_things_to_do = false;

  int64_t seqtime = seqtrack->end_time; // use end_time instead of start_time to ensure automation is sent out before note start. (causes slightly inaccurate automation, but it probably doesn't matter)
    
  R_ASSERT_RETURN_IF_FALSE2(seqtrack->seqtrackautomation!=NULL, more_things_to_do);

  for(auto *automation : seqtrack->seqtrackautomation->_automations){
    
    if (automation->_is_enabled==false)
      continue;
    
    struct Patch *patch = automation->patch;
    int effect_num = automation->effect_num;

    R_ASSERT_RETURN_IF_FALSE2(patch->instrument==get_audio_instrument(), more_things_to_do);
    
    SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
    if (plugin==NULL) {
      
      R_ASSERT_RETURN_IF_FALSE2(false, more_things_to_do);

    } else {
      
      double latency = RT_SP_get_input_latency(plugin->sp);
      if (latency!=0){
        struct SeqBlock *seqblock = seqtrack->curr_seqblock;
        if (seqblock != NULL && seqblock->block!=NULL){
          if (pc->playtype==PLAYSONG)
            latency *= ATOMIC_DOUBLE_GET(seqblock->block->reltempo);
          latency = ceil(latency); // Ensure automation is sent out before note start. (probably not necessary)
        }
      }

      double value;
#if !defined(RELEASE) && defined(FOR_WINDOWS)
      value = 0.0; // Workaround for compiler bug in mingw32. (I"m more than 90% sure it"s a compiler bug. In addition, neither gcc 7.2 on linux, gcc 8, nor clang 5, shows this warning).
#endif
      switch(automation->automation.RT_get_value(seqtime+latency, value)){

        case radium::SeqAutomationReturnType::VALUE_OK:{

          if (value != automation->last_value){
            FX_when when;
            if (automation->last_value < -0.5)
              when = FX_start;
            else
              when = FX_middle;
            
            RT_PLUGIN_touch(plugin);

            PLUGIN_set_effect_value(plugin,0,effect_num,value, DONT_STORE_VALUE, when, EFFECT_FORMAT_SCALED);
            automation->last_value = value;
          }
          
          more_things_to_do = true;

          break;
          
        }

        case radium::SeqAutomationReturnType::NO_VALUES_YET:{

          R_ASSERT_NON_RELEASE(automation->last_value == -1.0);
          more_things_to_do = true;
          automation->last_value = -1.0;
          break;
          
        }
  
        case radium::SeqAutomationReturnType::NO_MORE_VALUES:{
          
          RT_PLUGIN_touch(plugin);

          const AutomationNode &last_node = automation->automation.at(automation->automation.size()-1);
          PLUGIN_set_effect_value(plugin,0,effect_num, last_node.value, DONT_STORE_VALUE, FX_end, EFFECT_FORMAT_SCALED); // Make sure last value is sent out with FX_end. This also ensures last value is sent when the second last node is LOGTYPE_HOLD.
          automation->last_value = -1.0;
          
          //more_things_to_do = false; // Already set to false.
          break;
            
        }

        case radium::SeqAutomationReturnType::NO_VALUES:{
          // I guess it could happen when loading full state.
          //RError("Internal: Seqautomation error. RT_get_value returned NO_VALUES. That is not supposed to happen.\n");
          break;
        }
        
      }
      
    }
  }

  //if(more_things_to_do)
  
  return more_things_to_do;
}

void RT_SEQTRACK_AUTOMATION_called_when_player_stopped(void){
  ALL_SEQTRACKS_FOR_EACH(){

    //fprintf(stderr, "seqiterator: %d, num_seqtracks: %d\n", seqiterator666, root->song->seqtracks.num_elements);
    
    R_ASSERT_RETURN_IF_FALSE(seqtrack->seqtrackautomation != NULL);
    
    for(auto *automation : seqtrack->seqtrackautomation->_automations){
      R_ASSERT_RETURN_IF_FALSE(automation!=NULL);
      automation->last_value = -1.0;
    }

  }END_ALL_SEQTRACKS_FOR_EACH;
}



hash_t *SEQTRACK_AUTOMATION_get_state(struct SeqtrackAutomation *seqtrackautomation){
  return seqtrackautomation->get_state();
}


static float get_node_x2(const struct SeqTrack *seqtrack, const AutomationNode &node, double start_time, double end_time, float x1, float x2){
  //printf("      GET_NODE_X2 returned %f - %f. start_time: %f, end_time: %f\n",abstime/48000.0, scale(abstime, start_time, end_time, x1, x2), start_time/48000.0, end_time/48000.0);
  return scale(node.time, start_time, end_time, x1, x2);
}

static float get_node_x(const AutomationNode &node, double start_time, double end_time, float x1, float x2, void *data){
  return get_node_x2((const struct SeqTrack*)data, node, start_time, end_time, x1, x2);
}

float SEQTRACK_AUTOMATION_get_node_x(struct SeqtrackAutomation *seqtrackautomation, const struct SeqTrack *seqtrack, int automationnum, int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE2(automation->islegalnodenum(nodenum), 0);

  double start_time = SEQUENCER_get_visible_start_time();
  double end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQTRACK_get_x1(0);
  float x2 = SEQTRACK_get_x2(0);
  
  const AutomationNode &node = automation->automation.at(nodenum);

  return get_node_x2(seqtrack, node, start_time, end_time, x1, x2);
}

static float get_node_y(const AutomationNode &node, float y1, float y2, void *data){
  return scale(node.value, 0, 1, y2, y1);
}

float SEQTRACK_AUTOMATION_get_node_y(struct SeqtrackAutomation *seqtrackautomation, int seqtracknum, int automationnum, int nodenum){
  float y1 = SEQTRACK_get_y1(seqtracknum);
  float y2 = SEQTRACK_get_y2(seqtracknum);
  
  R_ASSERT_RETURN_IF_FALSE2(seqtrackautomation->islegalautomation(automationnum), 0);

  struct Automation *automation = seqtrackautomation->_automations[automationnum];
  R_ASSERT_RETURN_IF_FALSE2(automation->islegalnodenum(nodenum), 0);

  return get_node_y(automation->automation.at(nodenum), y1, y2, NULL);
}

void SEQTRACK_AUTOMATION_paint(QPainter *p, const struct SeqTrack *seqtrack, float x1, float y1, float x2, float y2, double start_time, double end_time){

  for(auto *automation : seqtrack->seqtrackautomation->_automations){
    QColor color = automation->color;
    if(!automation->_is_enabled)
      color.setAlpha(20);
    automation->automation.paint(p, x1, y1, x2, y2, start_time, end_time,
                                 color,
                                 get_node_y, get_node_x, (void*)seqtrack);
  }
}
