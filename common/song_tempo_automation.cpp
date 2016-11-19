



#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "seqtrack_proc.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"

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

static int g_curr_nodenum = -1;

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

static const struct TempoAutomation *g_last_used_rt_tempo_automation = NULL; // For the GC. Always prevent previous generation from being garbage collected.
static void set_rt_tempo_automation(void){
  const struct TempoAutomation *new_rt_tempo_automation = (const struct TempoAutomation*)from_qvector(g_tempo_automation);

  g_last_used_rt_tempo_automation = ATOMIC_GET(g_rt_tempo_automation);
  
  ATOMIC_SET(g_rt_tempo_automation, new_rt_tempo_automation);
}




static const struct TempoAutomation *g_last_used_rt_tempo_automation2 = NULL; // For the GC.

// Called from MIXER.cpp in the player thread.
double RT_TEMPOAUTOMATION_get_value(double abstime){

  // Ensure the tempo_automation we are working on won't be gc-ed while using it.
  // It probably never happens though, there shouldn't be time for it to
  // be both found, freed, and reused, in such a short time, but now we
  // don't have to worry about the possibility.
  g_last_used_rt_tempo_automation2 = ATOMIC_GET(g_rt_tempo_automation);
  
  const struct TempoAutomation *rt_tempo_automation = g_last_used_rt_tempo_automation2;

  for(int i = 0 ; i < rt_tempo_automation->num_nodes-1 ; i++){
    const TempoAutomationNode &node1 = rt_tempo_automation->nodes[i];
    const TempoAutomationNode &node2 = rt_tempo_automation->nodes[i+1];

    if (abstime >= node1.abstime && abstime < node2.abstime){
      if (node1.logtype==LOGTYPE_LINEAR){
        if (node1.abstime==node2.abstime)
          return (node1.value+node2.value) / 2.0;
        else
          return scale_double(abstime, node1.abstime, node2.abstime, node1.value, node2.value);
      }else
        return node1.value;
    }
  }

  return 1.0; //rt_tempo_automation->nodes[rt_tempo_automation->num-1].value;
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
  
static int get_node_num(double abstime){
  double abstime1 = g_tempo_automation.at(0).abstime;
  R_ASSERT_RETURN_IF_FALSE2(abstime1==0,1);
  
  int size = g_tempo_automation.size();
  if (size==1){
    return 1;
  }

  for(int i=1;i<size;i++){
    double abstime2 = g_tempo_automation.at(i).abstime;

    if (abstime >= abstime1 && abstime < abstime2)
      return i;
    
    abstime1 = abstime2;
  }

  return size;
}

int TEMPOAUTOMATION_add_node(double abstime, double value, int logtype){
  if (abstime < 0)
    abstime = 0;
  
  int nodenum = get_node_num(abstime);

  g_tempo_automation.insert(nodenum, create_node(abstime, value, logtype));
  
  set_rt_tempo_automation();

  return nodenum;
}
                              
void TEMPOAUTOMATION_delete_node(int nodenum){
  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < g_tempo_automation.size());

  if (nodenum==0){
    
    g_tempo_automation[0].value = 1.0;

  } else if (nodenum==g_tempo_automation.size()-1) {

    g_tempo_automation[g_tempo_automation.size()-1].value = 1.0;

  } else {
  
    g_tempo_automation.remove(nodenum);
    
  }
  
  set_rt_tempo_automation();
}

void TEMPOAUTOMATION_set_curr_node(int nodenum){
  if (g_curr_nodenum != nodenum){
    g_curr_nodenum = nodenum;
    SEQUENCER_update();
  }
}

void TEMPOAUTOMATION_set(int nodenum, double abstime, double value, int logtype){
  int size = g_tempo_automation.size();

  R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
  R_ASSERT_RETURN_IF_FALSE(nodenum < size);

  const TempoAutomationNode *prev = nodenum==0 ? NULL : &g_tempo_automation.at(nodenum-1);
  TempoAutomationNode *node = &g_tempo_automation[nodenum];
  const TempoAutomationNode *next = nodenum==size-1 ? NULL : &g_tempo_automation.at(nodenum+1);

  double mintime = prev==NULL ? 0 : next==NULL ? R_MAX(R_MAX(node->abstime, abstime), SONG_get_length()) : prev->abstime;
  double maxtime = (prev==NULL || next==NULL) ? mintime : next->abstime;

  abstime = R_BOUNDARIES(mintime, abstime, maxtime);

  value = R_BOUNDARIES(0.01, value, 2);

  node->abstime = abstime;
  node->value = value;
  node->logtype = logtype;
  
  set_rt_tempo_automation();
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

void TEMPOAUTOMATION_reset(void){
  g_tempo_automation.clear();
  SEQTEMPO_set_visible(false);
  //TEMPOAUTOMATION_set_length(SONG_get_length(), true);
}


double TEMPOAUTOMATION_get_absabstime(double abstime){
  return 1.0;
}

static hash_t *get_node_state(int nodenum){
  const auto &node = g_tempo_automation.at(nodenum);
  
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "abstime", node.abstime);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}

static TempoAutomationNode create_node_from_state(hash_t *state){
  return create_node(HASH_get_float(state, "abstime"),
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}


hash_t *TEMPOAUTOMATION_get_state(void){
  int size = g_tempo_automation.size();
  
  hash_t *state = HASH_create(size);
  
  for(int i = 0 ; i < size ; i++)
    HASH_put_hash_at(state, "node", i, get_node_state(i));

  HASH_put_bool(state, "is_visible", SEQTEMPO_is_visible());
  
  return state;
}

void TEMPOAUTOMATION_create_from_state(hash_t *state){
  int size = HASH_get_array_size(state);

  g_tempo_automation.clear();

  for(int i = 0 ; i < size ; i++)
    g_tempo_automation.push_back(create_node_from_state(HASH_get_hash_at(state, "node", i)));

  if (HASH_has_key(state, "is_visible"))
    SEQTEMPO_set_visible(HASH_get_bool(state, "is_visible"));
    
  set_rt_tempo_automation();
}

static QColor get_color(QColor col1, QColor col2, int mix, float alpha){
  QColor ret = mix_colors(col1, col2, (float)mix/1000.0);
  ret.setAlphaF(alpha);
  return ret;
}

static void paint_node(QPainter *p, float x, float y, int nodenum, QColor color){ //const TempoAutomationNode &node){
  float minnodesize = root->song->tracker_windows->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in getHalfOfNodeWidth in api_mouse.c and OpenGL/Render.cpp
  float x1 = x-minnodesize;
  float x2 = x+minnodesize;
  float y1 = y-minnodesize;
  float y2 = y+minnodesize;
  const float width = 1.2;

  static QPen pen1,pen2,pen3,pen4;
  static QBrush fill_brush;
  static bool has_inited = false;
  
  if(has_inited==false){
    //QColor color = QColor(80,40,40);

    fill_brush = QBrush(get_color(color, Qt::white, 300, 0.3));
    
    pen1 = QPen(get_color(color, Qt::white, 100, 0.3));
    pen1.setWidth(width);

    pen2 = QPen(get_color(color, Qt::black, 300, 0.3));
    pen2.setWidth(width);

    pen3 = QPen(get_color(color, Qt::black, 400, 0.3));
    pen3.setWidth(width);

    pen4 = QPen(get_color(color, Qt::white, 300, 0.3));
    pen4.setWidth(width);

    has_inited=true;
  }
  
  if (nodenum == g_curr_nodenum) {
    p->setBrush(fill_brush);
    p->setPen(Qt::NoPen);
    QRectF rect(x1,y1,x2-x1-1,y2-y1);
    p->drawRect(rect);
  }

  // vertical left
  {
    p->setPen(pen1);
    QLineF line(x1+1, y1+1,
                x1+2,y2-1);
    p->drawLine(line);
  }
  
  // horizontal bottom
  {
    p->setPen(pen2);
    QLineF line(x1+2,y2-1,
                x2-1,y2-2);
    p->drawLine(line);
  }

  // vertical right
  {
    p->setPen(pen3);
    QLineF line(x2-1,y2-2,
                x2-2,y1+2);
    p->drawLine(line);
  }

  // horizontal top
  {
    p->setPen(pen4);
    QLineF line(x2-2,y1+2,
                x1+1,y1+1);
    p->drawLine(line);
  }
}

float TEMPOAUTOMATION_get_node_x(int nodenum){
  int64_t start_time = SEQUENCER_get_visible_start_time();
  int64_t end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQTEMPO_get_x1();
  float x2 = SEQTEMPO_get_x2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return scale(node1.abstime, start_time, end_time, x1, x2);
}

float TEMPOAUTOMATION_get_node_y(int nodenum){
  float y1 = SEQTEMPO_get_y1();
  float y2 = SEQTEMPO_get_y2();
  
  const TempoAutomationNode &node1 = g_tempo_automation.at(nodenum);
  
  return scale(node1.value, 2, 0, y1, y2);
}

void TEMPOAUTOMATION_paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time){
  
  TEMPOAUTOMATION_set_length(end_time, false);

  QColor color = get_qcolor(SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM); //(200,200,200);
  QPen pen(color);
  pen.setWidth(2.3);
  
  for(int i = 0 ; i < g_tempo_automation.size()-1 ; i++){
    const TempoAutomationNode &node1 = g_tempo_automation.at(i);

    if (node1.abstime >= end_time)
      break;

    const TempoAutomationNode &node2 = g_tempo_automation.at(i+1);

    if (node2.abstime < start_time)
      continue;

    float x_a = scale(node1.abstime, start_time, end_time, x1, x2);
    float x_b = scale(node2.abstime, start_time, end_time, x1, x2);

    float y_a = scale(node1.value, 2, 0, y1, y2);
    float y_b = scale(node2.value, 2, 0, y1, y2);

    //printf("y_a: %f, y1: %f, y2: %f\n", 
    
    p->setPen(pen);

    if (node1.logtype==LOGTYPE_HOLD){
      QLineF line1(x_a, y_a, x_b, y_a);
      p->drawLine(line1);
      
      QLineF line2(x_b, y_a, x_b, y_b);
      p->drawLine(line2);

    } else {
      
      QLineF line(x_a, y_a, x_b, y_b);
      p->drawLine(line);

    }
    
    paint_node(p, x_a, y_a, i, color);
    
    if(i==g_tempo_automation.size()-2)
      paint_node(p, x_b, y_b, i+1, color);
  }
}

