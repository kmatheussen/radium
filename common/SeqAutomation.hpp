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



#ifndef _RADIUM_COMMON_SEQAUTOMATION_HPP
#define _RADIUM_COMMON_SEQAUTOMATION_HPP

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


#include <QPainter>

#include "../Qt/Qt_mix_colors.h"


namespace radium{

  
template <typename T> struct NodeFromStateProvider{
  virtual T create_node_from_state(hash_t *state, double state_samplerate) const = 0;
  virtual ~NodeFromStateProvider() = default; // Crazy c++ stuff. https://www.securecoding.cert.org/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
};


template <typename T> class SeqAutomation{
  
private:
  
  QVector<T> _automation;

  struct RT{
    int num_nodes;
    T nodes[];
  };

  bool _paint_nodes = false;
  int _curr_nodenum = -1;

  AtomicPointerStorage _rt;


  // Double free / using data after free, if trying to copy data.
  SeqAutomation(const SeqAutomation&) = delete;
  SeqAutomation& operator=(const SeqAutomation&) = delete;


public:

  SeqAutomation()
    : _rt(V_free_function)
  {
  }

  void *new_rt_data_has_been_created_data = NULL;
  void (*new_rt_data_has_been_created)(void *data) = NULL;

private:
  
  int get_size(int num_nodes) const {
    return sizeof(struct RT) + num_nodes*sizeof(T);
  }

  const struct RT *create_rt(void) const {
    struct RT *rt = (struct RT*)V_malloc(get_size(_automation.size()));
  
    rt->num_nodes=_automation.size();
    
    for(int i=0 ; i<_automation.size() ; i++)
      rt->nodes[i] = _automation.at(i);
    
    return (const struct RT*)rt;
  }

  void create_new_rt_data(void){
    const struct RT *new_rt_tempo_automation = create_rt();

    _rt.set_new_pointer((void*)new_rt_tempo_automation);

    if (new_rt_data_has_been_created != NULL)
      new_rt_data_has_been_created(new_rt_data_has_been_created_data);
  }


public:

  bool do_paint_nodes(void) const {
    return _paint_nodes;
  }

  // called very often.
  void set_do_paint_nodes(bool do_paint_nodes){
    if (do_paint_nodes != _paint_nodes)
      _paint_nodes = do_paint_nodes;
  }

  int size(void) const {
    return _automation.size();
  }

  T* begin() {
    return _automation.begin();
    //return &_automation[0];
  }

  T* end() {
    return _automation.end();
  }

  const T &at(int n) const {
    return _automation.at(n);
  }

  const T &last(void) const {
    R_ASSERT(size()>0);
    return at(size()-1);
  }
  
  double get_value(double time, double time1, double time2, int logtype1, double value1, double value2) const {
    if (logtype1==LOGTYPE_LINEAR) {
      if (time1==time2)        
        return (value1 + value2) / 2.0;
      else
        return scale_double(time, time1, time2, value1, value2);
    } else
      return value1;
  }

  double get_value(double time, const T *node1, const T *node2, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL) const {
    const double time1 = node1->time;
    const double time2 = node2->time;
    
    const int logtype1 = node1->logtype;

    R_ASSERT_NON_RELEASE(time >= time1);
    R_ASSERT_NON_RELEASE(time <= time2);

    if (custom_get_value!=NULL && logtype1==LOGTYPE_LINEAR && time1!=time2)
      return custom_get_value(time, node1, node2);
    else
      return get_value(time, time1, time2, logtype1, node1->value, node2->value);
  }

  double get_value(double time, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL) const {
    int size = _automation.size();

    if (size==0)
      return 0;

    if (size==1)
      return _automation.at(0).value;

    int nodenum = get_node_num(time);
    if (nodenum==-1)
      return _automation.at(0).value;

    if (nodenum >= size-1)
      return _automation.at(nodenum-1).value;

    return get_value((double)time, &_automation.at(nodenum), &_automation.at(nodenum+1), custom_get_value);
  }

  bool get_value(double time, const T *node1, const T *node2, double &value, double (*custom_get_value)(double time, const T *node1, const T *node2)) const {
    const double time1 = node1->time;
    const double time2 = node2->time;
    
    if (time >= time1 && time < time2){
      value = get_value(time, node1, node2, custom_get_value);
      return true;
    } else {
      return false;
    }
  }


private:

  // Based on pseudocode for the function BinarySearch_Left found at https://rosettacode.org/wiki/Binary_search
  int BinarySearch_Left(const struct RT *rt, double value, int low, int high) const {   // initially called with low = 0, high = N - 1
      // invariants: value  > A[i] for all i < low
      //             value <= A[i] for all i > high
      if (high < low)
        return low;
      
      int mid = (low + high) / 2;
        
      if (rt->nodes[mid].time >= value)
        return BinarySearch_Left(rt, value, low, mid-1);
      else
        return BinarySearch_Left(rt, value, mid+1, high);
  }
  
  int _RT_last_search_pos = 1;

  /*
  bool get_value(double time, double &value, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL, bool always_set_value = false){
  }
  */

public:

  // Note: Value is not set if rt->num_nodes==0, even if always_set_value==true.  
  bool RT_get_value(double time, double &value, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL, bool always_set_value = false){

    R_ASSERT_NON_RELEASE(_RT_last_search_pos > 0);

    RT_AtomicPointerStorage_ScopedUsage rt_pointer(&_rt);
      
    const struct RT *rt = (const struct RT*)rt_pointer.get_pointer();

    if (rt!=NULL) {
      
      if (rt->num_nodes==0)
        return false;
                
      if (rt->num_nodes==1){
        
        if (always_set_value)
          value = rt->nodes[0].value;
        
        return false;
      }
      
      if (time < rt->nodes[0].time){
        
        if (always_set_value)
          value = rt->nodes[0].value;

        return false;
      }
      
      if (time == rt->nodes[0].time){
        value = rt->nodes[0].value;
        return true;
      }

      if (time > rt->nodes[rt->num_nodes-1].time){

        if (always_set_value)
          value = rt->nodes[rt->num_nodes-1].value;

        return false;
      }
      
      const T *node1;
      const T *node2;
      int i = _RT_last_search_pos;

      if (i<rt->num_nodes){
        node1 = &rt->nodes[i-1];
        node2 = &rt->nodes[i];
        if (time >= node1->time && time <= node2->time) // Same position in array as last time. No need to do binary search. This is the path we usually take.
          goto gotit;
      }

      i = BinarySearch_Left(rt, time, 0, rt->num_nodes-1);
      R_ASSERT_NON_RELEASE(i>0);
      
      _RT_last_search_pos = i;
      node1 = &rt->nodes[i-1];
      node2 = &rt->nodes[i];

    gotit:      
      value = get_value(time, node1, node2, custom_get_value);
      return true;
    }
    
    return false; //rt_tempo_automation->nodes[rt_tempo_automation->num-1].value;
  }

  /*
  double RT_get_value(double time, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL){
    double ret;
    if(RT_get_value(time, ret, custom_get_value))
      return ret;
    else
      return 1.0;
  }
  */

  int get_curr_nodenum(void) const {
    return _curr_nodenum;
  }
  
  void set_curr_nodenum(int nodenum){
    _curr_nodenum = nodenum;
  }

  int get_node_num(double time) const {
    int size = _automation.size();

    for(int i=0;i<size;i++)
      if (time < at(i).time)
        return i-1;

    return size-1;
  }

  int add_node(const T &node){
    double time = node.time;

    R_ASSERT(time >= 0);

    if (time < 0)
      time = 0;
    
    int nodenum = get_node_num(time)+1;
    
    _automation.insert(nodenum, node);

    create_new_rt_data();
    
    return nodenum;
  }


  void delete_node(int nodenum){
    R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
    R_ASSERT_RETURN_IF_FALSE(nodenum < _automation.size());
    
    _automation.remove(nodenum);

    create_new_rt_data();
  }

  void replace_node(int nodenum, const T &new_node){
    R_ASSERT_RETURN_IF_FALSE(nodenum >= 0);
    R_ASSERT_RETURN_IF_FALSE(nodenum < _automation.size());

    T *node = &_automation[nodenum];

    if (false && node->time != new_node.time){
      _automation.remove(nodenum);
      add_node(new_node);
    } else {
      *node = new_node;
      create_new_rt_data();
    }
            
  }

  void reset(void){
    _automation.clear();
    create_new_rt_data();
  }

private:

  QColor get_color(QColor col1, QColor col2, int mix, float alpha) const {
    QColor ret = mix_colors(col1, col2, (float)mix/1000.0);
    ret.setAlphaF(alpha);
    return ret;
  }

  float get_min_node_size(void) const {
    return root->song->tracker_windows->fontheight / 1.5; // if changing 1.5 here, also change 1.5 in getHalfOfNodeWidth in api/api_mouse.c and OpenGL/Render.cpp
  }

  void paint_node(QPainter *p, float x, float y, int nodenum, QColor color) const {
    float minnodesize = get_min_node_size();
    float x1 = x-minnodesize;
    float x2 = x+minnodesize;
    float y1 = y-minnodesize;
    float y2 = y+minnodesize;
    const float width = 1.2;
    
    static QPen pen1,pen2,pen3,pen4;
    static QBrush fill_brush;
    static bool has_inited = false;
    
    if(has_inited==false){
      
      fill_brush = QBrush(get_color(color, Qt::white, 300, 0.3));
      
      pen1 = QPen(get_color(color, Qt::white, 100, 0.3));
      pen1.setWidthF(width);
      
      pen2 = QPen(get_color(color, Qt::black, 300, 0.3));
      pen2.setWidthF(width);
      
      pen3 = QPen(get_color(color, Qt::black, 400, 0.3));
      pen3.setWidthF(width);
      
      pen4 = QPen(get_color(color, Qt::white, 300, 0.3));
      pen4.setWidthF(width);
      
      has_inited=true;
    }
    
    if (nodenum == _curr_nodenum) {
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

public:

  void print(void){
    for(int i = 0 ; i < _automation.size()-1 ; i++){
      const T &node1 = _automation.at(i);
      const T &node2 = _automation.at(i+1);
      printf("%d: %f -> %f. (%f -> %f)\n", i, node1.value, node2.value, node1.time, node2.time);
    }
  }

  void paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time, const QColor &color,
             float (*get_y)(const T &node, float y1, float y2, void *data),
             float (*get_x)(const T &node, double start_time, double end_time, float x1, float x2, void *data) = NULL,
             void *data = NULL,
             const QColor fill_color = QColor(),
             float fill_x1 = -1, float fill_x2 = -1
             ) const {

    if (_automation.size()==0){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    int size = 0;
    QPointF points[4 + _automation.size()*2];

    int start_i = -1;
    int num_after_end = 0;
    bool next_is_hold = false;

    // 1. find x+y points in the gfx coordinate system
    for(int i = 0; i < _automation.size() ; i++){
      const T &node1 = _automation.at(i);

      float x_a;

      if (get_x != NULL)
        x_a = get_x(node1, start_time, end_time, x1, x2, data);
      else
        x_a = scale(node1.time, start_time, end_time, x1, x2);

      if (x_a >= x2)
        num_after_end++;

      if (num_after_end == 2)
        break;
      
      if (start_i < 0)
        start_i = i;

      float y_a = get_y(node1, y1, y2, data);

      if (next_is_hold && x_a>=x1){
        points[size] = QPointF(x_a, points[size-1].y());
        size++;
      }

      //printf("   %d: %f, %f  (x1: %f)\n", size, x_a, y_a, x1);
      if (size > 0 && x_a < x1)
        points[size-1] = QPointF(x_a, y_a);
      else
        points[size++] = QPointF(x_a, y_a);


      next_is_hold = node1.logtype==LOGTYPE_HOLD;
    }

    //printf("---------------------------. x1: %f\n", x1);

    if (size==0)
      return;

    bool paint_lines = size >= 2;
    
    if (points[size-1].x() < x1){

      if (points[size-1].x() < x1-get_min_node_size())
        return;

      paint_lines = false;
    }

    if (points[0].x() >= x2){

      if (points[0].x() >= x2+get_min_node_size())
        return;

      paint_lines = false;
    }

    if (paint_lines && true) {

      float first_x = points[0].x();
      float first_y = points[0].y();
      float last_x = points[size-1].x();
      float last_y = points[size-1].y();
      
      int fill_size;
      
      if (fill_x1 == -1){
        points[size]   = QPointF(last_x, y2);
        points[size+1] = QPointF(first_x, y2);
        fill_size = size+2;
      } else {
        points[size]   = QPointF(x2, last_y);
        points[size+1] = QPointF(x2, y2);
        points[size+2] = QPointF(x1, y2);
        points[size+3] = QPointF(x1, first_y);
        fill_size = size+4;
      }
        
      //for(int i=0 ; i < size+2 ; i++)
      //  printf("  %d/%d: %d , %d  (y1: %f, y2: %f)\n", i, size, (int)points[i].x(), (int)points[i].y(), y1, y2);
      
      if (fill_color.isValid()) {

        // 2. Background fill
        
        p->setPen(Qt::NoPen);
        p->setBrush(fill_color);
        
        p->drawPolygon(points, fill_size);
        
        p->setBrush(Qt::NoBrush);

      } else {

        // 3. stipled line.
        p->setOpacity(0.35);

#if 0
        QPen pen(color);
        pen.setWidthF(_paint_nodes ? root->song->tracker_windows->systemfontheight / 3 : root->song->tracker_windows->systemfontheight / 6);
        p->setPen(pen);

        p->drawPolygon(points, fill_size);
#else
        p->setPen(Qt::NoPen);
        p->setBrush(color);
        
        p->drawPolygon(points, fill_size);
        
        p->setBrush(Qt::NoBrush);
#endif
        p->setOpacity(1.0);

      }

    }
    
    // 3. Lines
    if (paint_lines){
      QPen pen(color);
      pen.setWidthF(_paint_nodes ? root->song->tracker_windows->systemfontheight / 3 : root->song->tracker_windows->systemfontheight / 6);
      p->setPen(pen);
      p->drawPolyline(points, size);
    }
    
    // 4. Nodes
    if (_paint_nodes){
      int node_pos = start_i;
      for(int i = start_i; i < size ; i++){
        const T &node = _automation.at(node_pos);

        float x_a = points[i].x();
        float y_a = points[i].y();
        paint_node(p, x_a, y_a, node_pos, color);

        if (node.logtype==LOGTYPE_HOLD)
          i++;

        node_pos++;
      }
    }
  }


  void create_from_state(const dyn_t &dynstate, const NodeFromStateProvider<T> *nsp, double state_samplerate){
    _automation.clear();

    if (dynstate.type==HASH_TYPE) {

      // Old format. When loading old songs.
      
      R_ASSERT(g_is_loading==true);
      
      const hash_t *state = dynstate.hash;
      int size = HASH_get_array_size(state, "node");
      
      for(int i = 0 ; i < size ; i++)
        add_node(nsp->create_node_from_state(HASH_get_hash_at(state, "node", i), state_samplerate));

    } else if (dynstate.type==ARRAY_TYPE) {

      const dynvec_t *vec = dynstate.array;
      
      for(const dyn_t &dyn : vec)
        add_node(nsp->create_node_from_state(dyn.hash, state_samplerate));

    } else {
      
      R_ASSERT(false);
      
    }
  }
  
  void create_from_state(const dyn_t &dynstate, T (*create_node_from_state_func)(hash_t *,double), double state_samplerate){

    struct MyProvider : public NodeFromStateProvider<T> {
      T (*_create_node_from_state_func)(hash_t *,double);
      
      MyProvider(T (*create_node_from_state_func)(hash_t *,double))
        : _create_node_from_state_func(create_node_from_state_func)
      {}
      
      T create_node_from_state(hash_t *state, double state_samplerate) const {
        return _create_node_from_state_func(state, state_samplerate);
      }      
    };

    MyProvider myprovider(create_node_from_state_func);
    
    create_from_state(dynstate, &myprovider, state_samplerate);
  }
  
  dyn_t get_state(hash_t *(*get_node_state)(const T &node, void*), void *data) const {
    int size = _automation.size();
    
    dynvec_t ret = {};
    
    for(int i = 0 ; i < size ; i++)
      DYNVEC_push_back(ret, DYN_create_hash(get_node_state(_automation.at(i), data)));
    
    return DYN_create_array(ret);
  }

};

template <typename T> 
class SeqAutomationIterator{
  const SeqAutomation<T> &_automation;

#if !defined(RELEASE)
  double _prev_time = -1;
#endif

  int _size;
  int _n;
  const T *_node2 = NULL;
  double _time1;
  double _time2;
  double _value1;
  double _value2;
  int _logtype1;

public:
  SeqAutomationIterator(const SeqAutomation<T> &automation)
    : _automation(automation)
    , _size(automation.size())
  {
    R_ASSERT_RETURN_IF_FALSE(_size > 0);

    const T &_node1 = _automation.at(0);
    _time1 = _node1.time;
    _value1 = _node1.value;
    _logtype1 = _node1.logtype;

    _n = 1;

    if (_size >= 2){
      _node2 = &_automation.at(_n);
      _time2 = _node2->time;
      _value2 = _node2->value;
    } else {
      _value2 = _value1; // Used in case there is only one node.
    }
  }

  double get_value(double time){
#if !defined(RELEASE)
    if(time<0)
      abort();
    if(time<=_prev_time)
      abort();
    _prev_time = time;
#endif

    if (_n==_size)
      return _value2;

    if (time < _time1)
      return _value1;

    while(time > _time2){
      _n++;

      if (_n==_size)
        return _value2;

      _time1 = _time2;
      _value1 = _value2;
      _logtype1 = _node2->logtype;

      _node2 = &_automation.at(_n);
      _time2 = _node2->time;
      _value2 = _node2->value;
    }

    return _automation.get_value(time, _time1, _time2, _logtype1, _value1, _value2);
  }
};

}

#endif
