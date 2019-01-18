/* Copyright 2019 Kjetil S. Matheussen

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



#include <QUuid>


#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "seqtrack_proc.h"
#include "patch_proc.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/Mixer_proc.h"


#include "sequencer_timing_proc.h"


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


namespace{

struct TempoAutomationNode{
  double time; // seqtime format
  double ideal_time;
  double value; // bpm
  int logtype;
  double num_quarters; // Total number of beats in the song at time 'time'.
  int barnum;
  int beatnum;
  QString uuid;
};

struct BarAutomationNode{
  double time; // seqtime format
  double value; // ppq
  int logtype;
  double bar_duration_in_frames;  // Only set for the last bar. (used when calculating ppq of bars after last node)
  double bar_duration_in_ppq;  // Only set for the last bar. (used when calculating ppq of bars after last node)
};

struct SignatureAutomationNode{
  double time; // seqtime format
  double ideal_time;
  int barnum;
  double value; // not really used, but radium::Seqautomation requires it.
  StaticRatio signature;
  int logtype;
  QString uuid;
};
 
}


static radium::SeqAutomation<TempoAutomationNode> g_tempo_automation;
static radium::SeqAutomation<BarAutomationNode> g_bar_automation;
static radium::SeqAutomation<SignatureAutomationNode> g_signature_automation;


static void prepare_signature_and_tempo_automation(QVector<SignatureAutomationNode> org_signatures,
                                                   QVector<TempoAutomationNode> org_tempos);




/************************************
            BPM
*************************************/


static TempoAutomationNode create_node(double seqtime, double value, int logtype, double num_quarters, QString uuid){
  TempoAutomationNode node = {
    .time = seqtime,
    .ideal_time = seqtime,
    .value = value,
    .logtype = logtype,
    .num_quarters = num_quarters,
    .barnum = 0,
    .beatnum = 0,
    .uuid = uuid
  };

  return node;
}


static void add_default_node0(radium::SeqAutomation<TempoAutomationNode> &tempo_automation, float default_bpm){
  TempoAutomationNode first = create_node(0, default_bpm, LOGTYPE_HOLD, 0, QUuid::createUuid().toString());
  first.beatnum=1;
  first.barnum=1;
  tempo_automation.add_node(first);
}

static void add_default_node0(float default_bpm){
  add_default_node0(g_tempo_automation, default_bpm);
}

double RT_SEQUENCER_TEMPO_get_value(double seqtime){
  const TempoAutomationNode *node1=NULL,*node2=NULL;

  auto res = g_tempo_automation.RT_get_nodes(seqtime, &node1, &node2);
  if (res==radium::SeqAutomationReturnType::NO_VALUES_YET || res==radium::SeqAutomationReturnType::NO_VALUES){
    R_ASSERT_NON_RELEASE(false);
    return root->tempo;
  }else
    return node1->value;
}

static double get_num_quarters(double seqtime, radium::SeqAutomationReturnType res, const TempoAutomationNode *node1, const TempoAutomationNode *node2){
  switch(res){
    
    case radium::SeqAutomationReturnType::NO_VALUES_YET:
    case radium::SeqAutomationReturnType::NO_VALUES:{

      R_ASSERT_NON_RELEASE(false);
      return 0.0;
    }

    case radium::SeqAutomationReturnType::NO_MORE_VALUES:{
    
      double duration_in_frames = seqtime - node1->time;
      double duration_in_seconds = duration_in_frames / (double)pc->pfreq;
      double quarters_per_second = node1->value / 60.0;
      
      return node1->num_quarters + duration_in_seconds*quarters_per_second;
    }


    case radium::SeqAutomationReturnType::VALUE_OK:{

      if (node1->time==node2->time)
        return node2->num_quarters;

      return scale_double(seqtime, node1->time, node2->time, node1->num_quarters, node2->num_quarters);
    }

  }

  R_ASSERT(false);
  return 0.0;
}

double RT_SEQUENCER_TEMPO_get_num_quarters(double seqtime){
  const TempoAutomationNode *node1=NULL,*node2=NULL;

  auto res = g_tempo_automation.RT_get_nodes(seqtime, &node1, &node2);

  return get_num_quarters(seqtime, res, node1, node2);
}

static TempoAutomationNode create_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_number(state, ":time");
  return create_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                     HASH_get_number(state, ":bpm"),
                     LOGTYPE_HOLD, //HASH_get_int32(state, ":logtype"),
                     0, //HASH_get_float(state, ":num_quarters")
                     HASH_get_qstring(state, ":uuid")
                     );
}

void SEQUENCER_TEMPO_create_from_state(const dyn_t &state, double state_samplerate){
  R_ASSERT_RETURN_IF_FALSE(state.type==ARRAY_TYPE);
  //R_ASSERT_RETURN_IF_FALSE(state.array->num_elements > 0);

  radium::SeqAutomation<TempoAutomationNode> tempo_automation;

  tempo_automation.create_from_state(state, create_node_from_state, state_samplerate);

  if (tempo_automation.size()==0)
    add_default_node0(tempo_automation, root->tempo);

  const TempoAutomationNode &first = tempo_automation.at(0);
  if (first.time > 0)
    add_default_node0(tempo_automation, root->tempo);

  QVector<SignatureAutomationNode> signatures = g_signature_automation.get_qvector();
  QVector<TempoAutomationNode> tempos = tempo_automation.get_qvector();
  prepare_signature_and_tempo_automation(signatures, tempos);
}

static hash_t *get_node_state(const TempoAutomationNode &node, void*){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, ":time", node.time);
  HASH_put_float(state, ":bpm", node.value);
  HASH_put_float(state, ":num_quarters", node.num_quarters);
  HASH_put_int(state, ":logtype", node.logtype);
  HASH_put_qstring(state, ":uuid", node.uuid);

  return state;
}


dyn_t SEQUENCER_TEMPO_get_state(void){
  if(g_tempo_automation.size()==0){
    R_ASSERT(false);
    add_default_node0(root->tempo);
  }

  return g_tempo_automation.get_state(get_node_state, NULL);
}





/************************************
            Signatures
*************************************/

static SignatureAutomationNode create_signature_node(double seqtime, const StaticRatio &signature, QString uuid){
  SignatureAutomationNode node = {
    .time = seqtime,
    .ideal_time = seqtime,
    .barnum = 0,
    .value = 0,
    .signature = signature,
    .logtype = LOGTYPE_HOLD,
    .uuid = uuid
  };

  return node;
}

static void add_default_node0(radium::SeqAutomation<SignatureAutomationNode> &signature_automation, const StaticRatio &signature){
  SignatureAutomationNode first = create_signature_node(0, signature, QUuid::createUuid().toString());
  first.barnum = 1;
  signature_automation.add_node(first);
}

static void add_default_node0(const StaticRatio &signature){
  add_default_node0(g_signature_automation, signature);
}

StaticRatio RT_SEQUENCER_SIGNATURE_get_value(double seqtime){
  const SignatureAutomationNode *node1=NULL,*node2=NULL;

  auto res = g_signature_automation.RT_get_nodes(seqtime, &node1, &node2);
  if (res==radium::SeqAutomationReturnType::NO_VALUES_YET || res==radium::SeqAutomationReturnType::NO_VALUES){
    R_ASSERT_NON_RELEASE(false);
    return root->signature;
  }else
    return node1->signature;
}


static SignatureAutomationNode create_signature_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_number(state, ":time");
  int numerator = HASH_get_int32(state, ":numerator");                  
  int denominator = HASH_get_int32(state, ":denominator");

  if (numerator<=0){
    R_ASSERT_NON_RELEASE(false);
    numerator = 1;
  }

  if (denominator<=0){
    R_ASSERT_NON_RELEASE(false);
    denominator = 1;
  }

  return create_signature_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                               make_static_ratio(numerator, denominator),
                               HASH_get_qstring(state, ":uuid")
                               );
}

void SEQUENCER_SIGNATURE_create_from_state(const dyn_t &state, double state_samplerate){
  R_ASSERT_RETURN_IF_FALSE(state.type==ARRAY_TYPE);
  //R_ASSERT_RETURN_IF_FALSE(state.array->num_elements > 0);

  radium::SeqAutomation<SignatureAutomationNode> signature_automation;

  signature_automation.create_from_state(state, create_signature_node_from_state, state_samplerate);

  if (signature_automation.size()==0)
    add_default_node0(signature_automation, root->signature);

  const SignatureAutomationNode &first = signature_automation.at(0);
  if (first.time > 0)
    add_default_node0(signature_automation, root->signature);

  QVector<SignatureAutomationNode> signatures = signature_automation.get_qvector();
  QVector<TempoAutomationNode> tempos = g_tempo_automation.get_qvector();

  prepare_signature_and_tempo_automation(signatures, tempos);
}

static hash_t *get_node_state(const SignatureAutomationNode &node, void*){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, ":time", node.time);
  HASH_put_int(state, ":numerator", node.signature.numerator);
  HASH_put_int(state, ":denominator", node.signature.denominator);
  HASH_put_qstring(state, ":uuid", node.uuid);

  return state;
}


dyn_t SEQUENCER_SIGNATURE_get_state(void){
  if(g_signature_automation.size()==0){
    R_ASSERT(false);
    add_default_node0(root->signature);
  }

  return g_signature_automation.get_state(get_node_state, NULL);
}



/************************************************
            Iterate time
*************************************************/

static void iterate_sequencer_time(int64_t start_seqtime, int64_t end_seqtime,
                                   GridType what_to_find,
                                   const QVector<SignatureAutomationNode> &signatures,
                                   const QVector<TempoAutomationNode> &tempos,
                                   std::function<bool(int64_t,int,int,int)> callback)
{

  R_ASSERT_RETURN_IF_FALSE(start_seqtime >= 0);

  if (end_seqtime != -1){
    R_ASSERT_RETURN_IF_FALSE(end_seqtime >= start_seqtime);
  }

  if(what_to_find==NO_GRID){
    R_ASSERT(false);
    return;
  }

  if(what_to_find==LINE_GRID)
    what_to_find = BEAT_GRID;

  R_ASSERT_RETURN_IF_FALSE(what_to_find==BEAT_GRID || what_to_find==BAR_GRID);

  R_ASSERT_RETURN_IF_FALSE(tempos.size() > 0);
  R_ASSERT_RETURN_IF_FALSE(signatures.size() > 0);

  int tempo_pos = 0; //g_tempo_automation.get_node_pos(start_seqtime);
  int signature_pos = 0; //g_signature_automation.get_node_pos(start_seqtime);

  const TempoAutomationNode *tempo_node1 = &tempos.at(tempo_pos);
  const TempoAutomationNode *tempo_node2 = (tempo_pos==tempos.size()-1) ? NULL : &tempos.at(tempo_pos+1);

  const SignatureAutomationNode *signature_node1 = &signatures.at(signature_pos);
  const SignatureAutomationNode *signature_node2 = (signature_pos==signatures.size()-1) ? NULL : &signatures.at(signature_pos+1);

  int barnum = 0;

  const double samplerate = pc->pfreq;

  double time = 0;

  for(;;){

    barnum++;

    while(signature_node2 != NULL && barnum==signature_node2->barnum){
      signature_pos++;
      signature_node1 = signature_node2;
      signature_node2 = signature_pos==signatures.size()-1 ? NULL : &signatures.at(signature_pos+1);
    }

    int numerator = signature_node1->signature.numerator;
    int denominator = signature_node1->signature.denominator;
    double signature = (double)numerator / (double)denominator;
      
    for(int beatnum = 1 ; beatnum <= numerator ; beatnum++){

      //printf("Bar/beat: %d/%d. Time: %f. (%f -> %f)\n", barnum, beatnum, time/samplerate, start_seqtime/samplerate, end_seqtime/samplerate);

      if (time >= start_seqtime)
        if (what_to_find==BEAT_GRID || (what_to_find==BAR_GRID && beatnum==1))
          if(callback(time, barnum, beatnum, 0)==false)    
            return;

      for(;;){
        if(tempo_node2==NULL)
          break;
        if(tempo_node2->barnum < barnum || (tempo_node2->barnum==barnum && tempo_node2->beatnum==beatnum)){ // Testing for tempo_node2->barnum < barnum in case the node has illegal number of beats (i.e. more beats than beats in the signature).
          tempo_pos++;
          tempo_node1 = tempo_node2;
          tempo_node2 = (tempo_pos==tempos.size()-1) ? NULL : &tempos.at(tempo_pos+1);
        }else
          break;
      }

      double bpm = tempo_node1->value; // not accurate if the tempo node is placed between two beats.

      double quarters_per_second = bpm / 60.0;
      double quarter_duration_in_seconds = 1.0 / quarters_per_second;
      double quarter_duration = quarter_duration_in_seconds * samplerate;

      double bar_duration = quarter_duration * (signature * 4.0);

      double beat_duration = bar_duration / (double)numerator;

      time += beat_duration;
      if (end_seqtime != -1 && time >= end_seqtime)
        return;
    }

  }
}

void SEQUENCER_iterate_sequencer_time(int64_t start_seqtime, int64_t end_seqtime, GridType what_to_find, std::function<bool(int64_t,int,int,int)> callback){
  iterate_sequencer_time(start_seqtime, end_seqtime, what_to_find, g_signature_automation.get_qvector(), g_tempo_automation.get_qvector(), callback);
}




/************************************************
          BAR automation
*************************************************/

static double RT_SEQUENCER_BAR_get_ppq_of_last_bar_start(double seqtime){
  const BarAutomationNode *node1=NULL,*node2=NULL;

  auto res = g_bar_automation.RT_get_nodes(seqtime, &node1, &node2);
  if (res==radium::SeqAutomationReturnType::NO_VALUES_YET || res==radium::SeqAutomationReturnType::NO_VALUES){
    R_ASSERT_NON_RELEASE(false);
    return 0.0;
  }

  if (res==radium::SeqAutomationReturnType::NO_MORE_VALUES) {
    double duration_in_frames = (seqtime-node1->time);

    double bar_duration_in_frames = node1->bar_duration_in_frames;
    int num_bars = int(duration_in_frames / bar_duration_in_frames);

    double ret = node1->value + node1->bar_duration_in_ppq*num_bars;

    //printf("   RET: %f. Num bars: %d\n", ret,num_bars);
    return ret;
  }

  return node1->value;
}

static double find_ppq(radium::SeqAutomationIterator<TempoAutomationNode> &iterator, double time){

  const TempoAutomationNode *node1=NULL,*node2=NULL;

  auto res = iterator.get_nodes(time,&node1,&node2);

  return get_num_quarters(time, res, node1, node2);
}


static QVector<BarAutomationNode> recreate_bar_automation(const QVector<SignatureAutomationNode> &signatures,
                                                          const QVector<TempoAutomationNode> &tempos)
{
  radium::SeqAutomation<TempoAutomationNode> tempo_automation;
  tempo_automation.set_qvector(tempos);

  radium::SeqAutomationIterator<TempoAutomationNode> tempo_iterator(tempo_automation);


  QVector<BarAutomationNode> ret;

  double end_time = R_MAX(signatures.last().time, tempos.last().time) + 10;

  end_time = R_MAX(end_time, SONG_get_length()); // Span whole song. We want to avoid manually calculating bar start values during playback since rounding errors could cause subtle timing inaccuracies. (Note: recreate_bar_automation is not always called when song length changes, so we are sill calculating bar start values manually sometimes.)


  int downcount = 1;

  iterate_sequencer_time(0, -1, BAR_GRID, signatures, tempos,
                         [&](int64_t seqtime, int barnum, int beatnum, int linenum)
                         {
                           BarAutomationNode node = {
                             .time = (double)seqtime,
                             .value = find_ppq(tempo_iterator, seqtime),
                             .logtype = LOGTYPE_HOLD,
                             .bar_duration_in_frames = -1,
                             .bar_duration_in_ppq = -1
                           };

                           //printf("    Create BAR PPQ. Time: %f. End time: %f. barnum: %d/%d. ppq: %f\n", (double)seqtime/(double)pc->pfreq, end_time/(double)pc->pfreq, barnum, beatnum, node.value);

                           ret.push_back(node);

                           if(seqtime > end_time){

                             if (downcount==0)
                               return false;

                             downcount--;
                           }

                           return true;
                         });

  // set ppq_duration_factor of last node
  {
    const BarAutomationNode &node1 = ret.at(ret.size()-2);
    BarAutomationNode &node2 = ret[ret.size()-1];

    double frame_duration = node2.time - node1.time;
    double ppq_duration = node2.value - node1.value;
    
    node2.bar_duration_in_frames = frame_duration;
    node2.bar_duration_in_ppq = ppq_duration;
  }

  return ret;
}


/************************************************
          Prepare signatures and tempos
*************************************************/

static void add_ppq_to_tempos(QVector<TempoAutomationNode> &tempos){

  // Note: When loading song, the time attributes in each node have already been
  // converted to the currently used samplerate by the call to 'make_qvector_from_state'.
  double samplerate = pc->pfreq;

  double last_bpm = root->tempo;
  double last_time = 0;
  double last_num_quarters = 0;

  int nodenum = 0;
  for(TempoAutomationNode &node : tempos){

    double time = node.time;
    double duration_in_frames = time - last_time;
    double duration_in_seconds = duration_in_frames / samplerate;
    double quarters_per_second = last_bpm / 60.0;
    double num_quarters = last_num_quarters + duration_in_seconds * quarters_per_second;

    node.num_quarters = num_quarters;
    //printf("   %d: PPQ=%f\n", nodenum, num_quarters);

    last_bpm = node.value;
    last_time = time;
    last_num_quarters = num_quarters;

    nodenum++;
  }
}



// Sets time values to nearest beat.
static QVector<TempoAutomationNode> prepare_tempo_automation(const QVector<TempoAutomationNode> &tempos,
                                                             const QVector<SignatureAutomationNode> &signatures,
                                                             bool &is_changed){
  QVector<TempoAutomationNode> ret;

  {
    TempoAutomationNode node0 = tempos.at(0);
    R_ASSERT(node0.time == 0);

    if (node0.time!=0 || node0.barnum!=1 || node0.beatnum!=1){
      is_changed = true;
      node0.time=0;
      node0.barnum=1;
      node0.beatnum=1;
    }

    ret.push_back(node0);
  }

  bool is_first=true;

  for(const TempoAutomationNode &tempo : tempos){

    if(is_first){
      is_first=false;
      continue;
    }

    double beat_seqtime = 0;
    double least_diff = -1;
    int beatnum,barnum;

    iterate_sequencer_time(0, -1, BEAT_GRID, signatures, ret,
                           [&](int64_t seqtime, int barnum2, int beatnum2, int linenum)
                           {
                             double diff = fabs(tempo.ideal_time - seqtime);
                             if (least_diff < -0.5 || diff < least_diff){
                               least_diff = diff;
                               beat_seqtime = seqtime;
                               beatnum=beatnum2;
                               barnum=barnum2;
                             }

                             //printf("  %d/%d. diff: %f. Beat.time: %f. tempo.time: %f\n", barnum2, beatnum2, diff/(double)pc->pfreq, (double)seqtime/(double)pc->pfreq, tempo.time/(double)pc->pfreq);
                             if (seqtime > tempo.time)
                               return false;

                             return true;
                           });
    
    R_ASSERT(least_diff >= 0);

    TempoAutomationNode tempo2 = tempo; 

    if (tempo2.time != beat_seqtime ||
        tempo2.beatnum != beatnum ||
        tempo2.barnum != barnum)
      {
        is_changed = true;
        tempo2.time = beat_seqtime;
        tempo2.beatnum = beatnum;
        tempo2.barnum = barnum;
      }

    ret.push_back(tempo2);
  }

  return ret;
}

// Adds barnums and sets time values to nearest bar.
static QVector<SignatureAutomationNode> prepare_signature_automation(const QVector<SignatureAutomationNode> &signatures,
                                                                     const QVector<TempoAutomationNode> &tempos,
                                                                     bool &is_changed){
  QVector<SignatureAutomationNode> ret;

  {
    SignatureAutomationNode node0 = signatures.at(0);
    R_ASSERT(node0.time == 0);

    if(node0.time != 0 || node0.barnum!=1){
      is_changed = true;
      node0.time=0;
      node0.barnum=1;
    }

    ret.push_back(node0);
  }

  bool is_first=true;

  for(const SignatureAutomationNode &signature : signatures){

    if(is_first){
      is_first=false;
      continue;
    }

    int barnum = 0;
    double bar_seqtime = 0;
    double least_diff = -1;

    iterate_sequencer_time(0, -1, BAR_GRID, ret, tempos,
                           [&](int64_t seqtime, int barnum2, int beatnum, int linenum)
                           {
                             R_ASSERT(beatnum==1);

                             double diff = fabs(signature.ideal_time - seqtime);
                             if (least_diff < -0.5 || diff < least_diff){
                               least_diff = diff;
                               barnum = barnum2;
                               bar_seqtime = seqtime;
                             }

                             if (seqtime > signature.time)
                               return false;

                             return true;
                           });
    
    R_ASSERT(barnum>0);

    SignatureAutomationNode signature2 = signature; 

    if(signature2.barnum != barnum || signature2.time != bar_seqtime){
      is_changed = true;      
      signature2.barnum = barnum;
      signature2.time = bar_seqtime;
    }

    ret.push_back(signature2);
  }

  return ret;
}

/*
 * Ensures signatures are put at bar grid
 * Ensures tempos are put at beat grid
 * Adds barnums, beatnums, and ppq to nodes.
 */
static void prepare_signature_and_tempo_automation(QVector<SignatureAutomationNode> signatures,
                                                   QVector<TempoAutomationNode> tempos)
{

  int max_size1 = signatures.size() * tempos.size();
  int max_size2 = 10*max_size1;

  // Changing tempos changes signature positions, and changing signature changes tempo positions, so we just change both of them again and again until they stabilize.
  // (no need for a faster algorithm, it should never iterate more than max_size1 times)
  for(int i=0;;i++){

    bool is_changed = false;

    QVector<SignatureAutomationNode> new_signatures = prepare_signature_automation(signatures, tempos, is_changed);
    QVector<TempoAutomationNode> new_tempos = prepare_tempo_automation(tempos, signatures, is_changed);

    if (is_changed==false)
      break;

#if !defined(RELEASE)
    printf("      I: %d. is_changed: %d. max1: %d. max2: %d. size1: %d. size2: %d\n", i, is_changed, max_size1, max_size2, signatures.size(), tempos.size());
#endif

    if (i > max_size1){
      printf("Warning:      I: %d. is_changed: %d. max1: %d. max2: %d. size1: %d. size2: %d\n", i, is_changed, max_size1, max_size2, signatures.size(), tempos.size());
      R_ASSERT_NON_RELEASE(false);
    }

    if (i > max_size2){
      break;
    }

    signatures = new_signatures;
    tempos = new_tempos;
  }

  add_ppq_to_tempos(tempos);

  QVector<BarAutomationNode> bars = recreate_bar_automation(signatures, tempos); // must be called after add_ppq_to_tempos.

  // Ideally, the player should be locked while executing these three lines, but I don't think there should be any problems not doing it.
  g_tempo_automation.set_qvector(tempos); // bang
  g_signature_automation.set_qvector(signatures); // bang
  g_bar_automation.set_qvector(bars); // bang

  SEQUENCER_update(SEQUPDATE_TIMING);
}



/************************************
            Markers
*************************************/

static dyn_t g_markers = g_empty_dynvec;


dyn_t SEQUENCER_MARKER_get_state(void){
  return g_markers;
}

void SEQUENCER_MARKER_create_from_state(dyn_t markers, double state_samplerate){
  if(state_samplerate > 0)
    R_ASSERT(fabs(state_samplerate-pc->pfreq) < 1);

  g_markers = markers;
}



/************************************
            Timing state
*************************************/

void SEQUENCER_TIMING_create_from_state(const hash_t *state, double state_samplerate){

  radium::SeqAutomation<TempoAutomationNode> tempo_automation;

  {
    tempo_automation.create_from_state(HASH_get_dyn(state, "tempos"), create_node_from_state, state_samplerate);

    if (tempo_automation.size()==0)
      add_default_node0(tempo_automation, root->tempo);

    const TempoAutomationNode &first = tempo_automation.at(0);
    if (first.time > 0)
      add_default_node0(tempo_automation, root->tempo);
  }


  radium::SeqAutomation<SignatureAutomationNode> signature_automation;
  {
    signature_automation.create_from_state(HASH_get_dyn(state, "signatures"), create_signature_node_from_state, state_samplerate);

    if (signature_automation.size()==0)
      add_default_node0(signature_automation, root->signature);

    const SignatureAutomationNode &first = signature_automation.at(0);
    if (first.time > 0)
      add_default_node0(signature_automation, root->signature);
  }


  QVector<SignatureAutomationNode> signatures = signature_automation.get_qvector();
  QVector<TempoAutomationNode> tempos = tempo_automation.get_qvector();
  prepare_signature_and_tempo_automation(signatures, tempos);
}


hash_t *SEQUENCER_TIMING_get_state(void){
  hash_t *state = HASH_create(2);

  HASH_put_dyn(state, "tempos", g_tempo_automation.get_state(get_node_state, NULL));
  HASH_put_dyn(state, "signatures", g_signature_automation.get_state(get_node_state, NULL));

  return state;
}




/************************************
          Realtime
 ***********************************/

StaticRatio g_rt_sequencer_signature = {4,4};
double g_rt_sequencer_bpm = 120;
double g_rt_sequencer_ppq = 0;
double g_rt_sequencer_ppq_of_last_bar_start = 0;

static void RT_play_click(struct SeqTrack *seqtrack, int beatnum){
  //printf("           beaTT: %d\n", beatnum);

  int note_num = beatnum==0 ? c_bar_note_num : c_beat_note_num;

  int64_t time = 0; // We could calculate the accurate time here (somewhere between 0 and RADIUM_BLOCKSIZE), but unless each tick is only a few ms apart (which probably wouldn't make sense for a metronome), it would probably be impossible to notice a difference.
  RT_play_click_note(seqtrack, time, note_num);
}

bool RT_SEQUENCER_TIMING_call_before_start_of_audio_block(struct SeqTrack *seqtrack, bool is_playing_song){

  // Can't use root->song->block_seqtrack here since timing doesn't follow sequencer.
  //R_ASSERT_NON_RELEASE(seqtrack==root->song->block_seqtrack);

  R_ASSERT_NON_RELEASE(seqtrack==root->song->seqtracks.elements[0]);

  static bool s_was_playing_click = false;

  const int64_t start_time = seqtrack->start_time;

  if(!is_playing_song){
    s_was_playing_click = false;
    return false;
  }

  if(root->song->use_sequencer_tempos_and_signatures){

    g_rt_sequencer_signature = RT_SEQUENCER_SIGNATURE_get_value(start_time);
    g_rt_sequencer_bpm = RT_SEQUENCER_TEMPO_get_value(start_time);
    g_rt_sequencer_ppq = RT_SEQUENCER_TEMPO_get_num_quarters(start_time);
    g_rt_sequencer_ppq_of_last_bar_start = RT_SEQUENCER_BAR_get_ppq_of_last_bar_start(start_time);

    if(ATOMIC_GET(root->clickonoff)){

      static int s_last_beatnum = -1;

      const double numerator = g_rt_sequencer_signature.numerator;
      const double denominator = g_rt_sequencer_signature.denominator;
      
      const double ppq_since_last_bar = g_rt_sequencer_ppq - g_rt_sequencer_ppq_of_last_bar_start;
      const double signature = numerator / denominator;
      
      const double ppq_duration_of_bar = 4.0 * signature;
      const double ppq_duration_of_beat = ppq_duration_of_bar / numerator;
      
      const double beatnum = ppq_since_last_bar / ppq_duration_of_beat;
      const int i_beatnum = beatnum;

      if (s_was_playing_click) {

        if (i_beatnum != s_last_beatnum && i_beatnum < numerator)
          RT_play_click(seqtrack, i_beatnum);

      } else {

        const double num_ppq_after_beat = beatnum - i_beatnum;
        const double num_seconds_after_beat = num_ppq_after_beat / g_rt_sequencer_bpm;
        const double num_ms_after_beat = num_seconds_after_beat * 1000.0;
        
        if ( num_ms_after_beat < 1.0){
          RT_play_click(seqtrack, i_beatnum);
          //printf("           beaTT: %d (diff: %f. ms: %f)\n", i_beatnum, beatnum-(double)i_beatnum, num_ms_after_beat);
        } 
      }
      
      s_last_beatnum = i_beatnum;
      
      s_was_playing_click = true;

    } else {

      s_was_playing_click = false;
    
    }

    /*
    printf("    Sequencer timing called for %f. BPM: %f. PPQ: %f. PPQ last bar start: %f\n",
           (double)start_time/(double)pc->pfreq,
           g_rt_sequencer_bpm,
           g_rt_sequencer_ppq,
           g_rt_sequencer_ppq_of_last_bar_start);
    */

  } else {

    s_was_playing_click = false;

  }

  return false; // more things to do.
}


/************************************
          Init
 ***********************************/

void SEQUENCER_TIMING_set_default_values(float default_bpm, StaticRatio default_signature){

  if(g_signature_automation.size()==0){

    R_ASSERT(g_tempo_automation.size()==0);

    SEQUENCER_TIMING_init(default_bpm, default_signature);

    return;
  }

  R_ASSERT_RETURN_IF_FALSE(g_tempo_automation.size()>0);

  QVector<SignatureAutomationNode> signatures = g_signature_automation.get_qvector();
  QVector<TempoAutomationNode> tempos = g_tempo_automation.get_qvector();

  signatures[0].signature = default_signature;
  tempos[0].value = default_bpm;

  prepare_signature_and_tempo_automation(signatures, tempos);
}

void SEQUENCER_TIMING_init(float default_bpm, StaticRatio default_signature){

  g_markers = g_empty_dynvec;

  radium::SeqAutomation<SignatureAutomationNode> signature_automation;
  radium::SeqAutomation<TempoAutomationNode> tempo_automation;

  add_default_node0(signature_automation, default_signature);
  add_default_node0(tempo_automation, default_bpm);
 
  QVector<SignatureAutomationNode> signatures = signature_automation.get_qvector();
  QVector<TempoAutomationNode> tempos = tempo_automation.get_qvector();

  prepare_signature_and_tempo_automation(signatures, tempos);
}
