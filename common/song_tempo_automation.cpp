



#include "nsmtracker.h"
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
 
struct TempoAutomation{
  int num_nodes;
  struct TempoAutomationNode nodes[];
};

}

static QVector<TempoAutomationNode> g_tempo_automation;

static const struct TempoAutomation g_rt_empty_tempo_automation = {0};

static DEFINE_ATOMIC(const struct TempoAutomation *, g_rt_tempo_automation) = &g_rt_empty_tempo_automation;


static TempoAutomationNode create_node(double abstime, double value, int logtype){
  TempoAutomationNode node = {.abstime = abstime,
                              .value = value,
                              .logtype = logtype};

  return node;
}

static int get_size(int num_nodes){
  return sizeof(struct TempoAutomation) + num_nodes*sizeof(struct TempoAutomationNode);
}

/*
static struct TempoAutomation *get_copy(void){
  const struct TempoAutomation *org = ATOMIC_GET(g_tempo_automation);

  return tcopy(org, get_size(org->num_nodes));
}

static QVector<TempoAutomationNode> to_qvector(const TempoAutomation *tempo_automation){
  QVector<TempoAutomationNode> ret;
  
  for(int i=0;i<tempo_automation->num_nodes;i++)
    ret.push_back(tempo_automation->nodes[i]);

  return ret;
}
*/

static struct TempoAutomation *from_qvector(const QVector<TempoAutomationNode> &v){
  struct TempoAutomation *tempo_automation = (struct TempoAutomation*)talloc(get_size(v.size()));
  
  tempo_automation->num_nodes=v.size();
  
  for(int i=0 ; i<v.size() ; i++)
    tempo_automation->nodes[i] = v.at(i);

  return tempo_automation;
}

static void set_rt_tempo_automation(void){
  ATOMIC_SET(g_rt_tempo_automation, (const struct TempoAutomation*)from_qvector(g_tempo_automation));
}




static const struct TempoAutomation *g_last_used_rt_tempo_automation = NULL; // For the GC.

// Called from MIXER.cpp in the player thread.
double RT_TEMPOAUTOMATION_get_value(double abstime){

  // Ensure the tempo_automation we are working on won't be gc-ed while using it.
  // It probably never happens though, there shouldn't be time for it to
  // be both found, freed, and reused, in such a short time, but now we
  // don't have to worry about the possibility.
  g_last_used_rt_tempo_automation = ATOMIC_GET(g_rt_tempo_automation);
  
  const struct TempoAutomation *rt_tempo_automation = g_last_used_rt_tempo_automation;

  for(int i = 0 ; i < rt_tempo_automation->num_nodes-1 ; i++){
    const TempoAutomationNode &node1 = rt_tempo_automation->nodes[i];
    const TempoAutomationNode &node2 = rt_tempo_automation->nodes[i+1];

    if (abstime >= node1.abstime && abstime < node2.abstime){
      if (node1.logtype==LOGTYPE_LINEAR)
        return scale(abstime, node1.abstime, node2.abstime, node1.value, node2.value);
      else
        return node1.value;
    }
  }

  return 1.0; //rt_tempo_automation->nodes[rt_tempo_automation->num-1].value;
}

double TEMPOAUTOMATION_get_value(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>0, 1.0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 1.0);

  return g_tempo_automation.at(nodenum).value;
}

double TEMPOAUTOMATION_get_abstime(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>0, 0);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), 0);

  return g_tempo_automation.at(nodenum).abstime;
}

int TEMPOAUTOMATION_get_logtype(int nodenum){
  R_ASSERT_RETURN_IF_FALSE2(nodenum>0, LOGTYPE_LINEAR);
  R_ASSERT_RETURN_IF_FALSE2(nodenum<g_tempo_automation.size(), LOGTYPE_LINEAR);

  return g_tempo_automation.at(nodenum).logtype;
}

int TEMPOAUTOMATION_get_num_nodes(void){
  return g_tempo_automation.size();
}
  

void TEMPOAUTOMATION_add_node(double abstime, double value, int logtype){
  if (abstime < 0)
    abstime = 0;
  
  int nodenum = 1;
  for(const TempoAutomationNode &node : g_tempo_automation){
    if(abstime >= node.abstime)
      break;
    nodenum++;
  }

  g_tempo_automation.insert(nodenum, create_node(abstime, value, logtype));
  
  set_rt_tempo_automation();
}
                              
void TEMPOAUTOMATION_delete_node(int nodenum){
  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);

  if (nodenum==0){
    
    g_tempo_automation[0].value = 1.0;
    
  } else {
  
    if(nodenum >= g_tempo_automation.size()){
      RError("TEMPOAUTOMATION_remove_node: nodenum larger than size. %d %d %d", nodenum, g_tempo_automation.size(), ATOMIC_GET(g_rt_tempo_automation)->num_nodes);
      nodenum = g_tempo_automation.size()-1;
    }
  
    g_tempo_automation.remove(nodenum);
    
  }
  
  set_rt_tempo_automation();
}
                              
void TEMPOAUTOMATION_set(int nodenum, double abstime, double value, int logtype){
  TEMPOAUTOMATION_delete_node(nodenum);

  TEMPOAUTOMATION_add_node(abstime, value, logtype);

#if 0
  struct TempoAutomation *tempo_automation = get_copy();
  
  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < tempo_automation->num_nodes);
  
  tempo_automation->nodes[nodenum] = create_node(abstime, value, logtype);

  ATOMIC_SET(g_tempo_automation, tempo_automation);
#endif
}


// Does nothing if end_time < length and do_shrink==false.
// Don't shrinks below second last node. (i.e. we don't change a manually created node)
//
void TEMPOAUTOMATION_set_length(double end_time, bool do_shrink){

  R_ASSERT(end_time >= 0);
  
  int size = g_tempo_automation.size();
  
  if (size==0){
    
    g_tempo_automation.push_back(create_node(0,        1, LOGTYPE_LINEAR));
    g_tempo_automation.push_back(create_node(end_time, 1, LOGTYPE_LINEAR));
    
  } else {

    auto &second_last = g_tempo_automation.at(size-2);
    auto &last = g_tempo_automation[size-1];

    if (end_time <= last.abstime && do_shrink==false)
      return;

    if (end_time <= second_last.abstime)
      return;
    
    if (last.value==1.0 && second_last.value==1.0){
      last.abstime = end_time;
    } else {
      g_tempo_automation.push_back(create_node(end_time,1,LOGTYPE_LINEAR));
    }

  }

  set_rt_tempo_automation();
}

double TEMPOAUTOMATION_get_length(void){
  if (g_tempo_automation.size()==0)
    return 1.0;
  
  return g_tempo_automation.last().abstime;
}

double TEMPOAUTOMATION_get_absabstime(double abstime){
  return 1.0;
}


static void paint_node(QPainter *p, float x, float y){ //const TempoAutomationNode &node){
  const float half_width = 8/2;
  QRectF rect(x-half_width, y-half_width, half_width*2, half_width*2);

  p->setBrush(QBrush(QColor(255,0,0)));
  
  p->drawRect(rect);
}

void TEMPOAUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time){
  
  TEMPOAUTOMATION_set_length(end_time, false);

  for(int i = 0 ; i < g_tempo_automation.size()-1 ; i++){
    const TempoAutomationNode &node1 = g_tempo_automation.at(i);

    if (node1.abstime >= end_time)
      break;

    const TempoAutomationNode &node2 = g_tempo_automation.at(i+1);

    if (node2.abstime < start_time)
      continue;

    float x_a = scale(node1.abstime, start_time, end_time, x1, x2);
    float x_b = scale(node2.abstime, start_time, end_time, x1, x2);

    float y_a = scale(node1.value, 0, 2, y1, y2);
    float y_b = scale(node2.value, 0, 2, y1, y2);

    //printf("y_a: %f, y1: %f, y2: %f\n", 
    QLineF line(x_a, y_a, x_b, y_b);
    p->setPen(QColor(200,200,200));
    p->drawLine(line);
    
    paint_node(p, x_a, y_a);
    if(i==g_tempo_automation.size()-2)
      paint_node(p, x_b, y_b);
  }
}

