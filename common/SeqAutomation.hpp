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


// Run small test by executing "./build_linux.sh test"
// See ../test/test_seqautomation.cpp



#ifndef _RADIUM_COMMON_SEQAUTOMATION_HPP
#define _RADIUM_COMMON_SEQAUTOMATION_HPP

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


#include <QPainter>

#include "../Qt/Qt_mix_colors.h"

namespace radium{

// Default T struct for SeqAutomation
struct AutomationNode{
  double time; // seqtime format
  double value;
  int logtype;
};

static inline bool nodes_are_not_equal(const radium::AutomationNode &node1, const radium::AutomationNode &node2){
  return !equal_doubles(node1.time, node2.time) || !equal_doubles(node1.value, node2.value) || node1.logtype!=node2.logtype;
}

static inline bool nodes_are_equal(const radium::AutomationNode &node1, const radium::AutomationNode &node2){
  return equal_doubles(node1.time, node2.time) && equal_doubles(node1.value, node2.value) && node1.logtype==node2.logtype;
}

  
template <typename T> struct NodeFromStateProvider{
  virtual T create_node_from_state(hash_t *state, double state_samplerate) const = 0;
  virtual ~NodeFromStateProvider() = default; // Crazy c++ stuff. https://www.securecoding.cert.org/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
};


enum class SeqAutomationReturnType{
  VALUE_OK,
  NO_VALUES_YET,
  NO_VALUES,
  NO_MORE_VALUES,
};



template <typename T> 
struct SeqAutomationPainter : AutomationPainter{
  const QVector<T> &_automation;
  int _size = 0;

  bool _can_paint = false;
  bool _paint_lines = false;

  const QColor _color;

  float _x1, _y1, _x2, _y2;

  int _start_i;

  int _curr_nodenum;
  bool _paint_nodes;

  int _fill_x1, _fill_x2;
  const QColor _fill_color;
  
  QPointF *_points;

  SeqAutomationPainter(const SeqAutomationPainter&) = delete;
  SeqAutomationPainter& operator=(const SeqAutomationPainter&) = delete;

  SeqAutomationPainter(const QVector<T> &automation,
                       int curr_nodenum, bool paint_nodes,
                       float x1, float y1, float x2, float y2,
                       double start_time, double end_time,
                       const QColor &color,                       
                       float (*get_y)(const T &node, float y1, float y2, void *data),
                       float (*get_x)(const T &node, double start_time, double end_time, float x1, float x2, void *data) = NULL,
                       void *data = NULL,
                       const QColor fill_color = QColor(),
                       float fill_x1 = -1, float fill_x2 = -1                                       
                       )
      
    : _automation(automation)
    , _color(color)
    , _x1(x1)
    , _y1(y1)
    , _x2(x2)
    , _y2(y2)
    , _curr_nodenum(curr_nodenum)
    , _paint_nodes(paint_nodes)
    , _fill_x1(fill_x1)
    , _fill_x2(fill_x2)
    , _fill_color(fill_color)
    , _points(new QPointF[4 + _automation.size()*2])
      
  {
    if (_automation.size()==0){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
      
    _start_i = -1;
    int num_after_end = 0;
    bool next_is_hold = false;
      
    // 1. find x+y _points in the gfx coordinate system
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
        
      if (_start_i < 0)
        _start_i = i;
        
      float y_a = get_y(node1, y1, y2, data);
        
      if (next_is_hold && x_a>=x1){
        _points[_size] = QPointF(x_a, _points[_size-1].y());
        _size++;
      }
        
      //printf("   %d: %f, %f  (x1: %f)\n", _size, x_a, y_a, x1);
      if (_size > 0 && x_a < x1)
        _points[_size-1] = QPointF(x_a, y_a);
      else
        _points[_size++] = QPointF(x_a, y_a);
        
        
      next_is_hold = node1.logtype==LOGTYPE_HOLD;
    }
      
    //printf("---------------------------. x1: %f\n", x1);
      
    if (_size==0)
      return;

    _paint_lines = _size >= 2;
      
    if (_points[_size-1].x() < x1){
        
      if (_points[_size-1].x() < x1-get_min_node_size())
        return;
        
      _paint_lines = false;
    }
      
    if (_points[0].x() >= x2){
        
      if (_points[0].x() >= x2+get_min_node_size())
        return;
        
      _paint_lines = false;
    }

    _can_paint = true;
  }

  ~SeqAutomationPainter(){
    delete[] _points;
  }
                         
  QColor get_color(QColor col1, QColor col2, int mix, float alpha) const {
    QColor ret = mix_colors(col1, col2, (float)mix/1000.0);
    ret.setAlphaF(alpha);
    return ret;
  }

  void paint_node(QPainter *p, float x, float y, int nodenum, QColor color) const {
    static float penwidth_d2 = 1.2;
    
    static QPen pen1,pen2,pen3,pen4;
    static QBrush fill_brush;
    static bool has_inited = false;

    if(has_inited==false){

      const float penwidth = (float)root->song->tracker_windows->systemfontheight / 10.0f;
      penwidth_d2 = penwidth/2.01;

      fill_brush = QBrush(get_color(color, Qt::white, 300, 0.7));
      
      pen1 = QPen(get_color(color, Qt::white, 100, 0.3));
      pen1.setWidthF(penwidth);
      
      pen2 = QPen(get_color(color, Qt::black, 300, 0.3));
      pen2.setWidthF(penwidth);
      
      pen3 = QPen(get_color(color, Qt::black, 400, 0.3));
      pen3.setWidthF(penwidth);
      
      pen4 = QPen(get_color(color, Qt::white, 300, 0.3));
      pen4.setWidthF(penwidth);
      
      has_inited=true;
    }

    float minnodesize = get_min_node_size() - 1;

    //printf("penwidth: %f\n", penwidth);
    
    float x1 = x-minnodesize+penwidth_d2;
    float x2 = x+minnodesize-penwidth_d2;
    float y1 = y-minnodesize+penwidth_d2;
    float y2 = y+minnodesize-penwidth_d2;

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

  void paint_fill(QPainter *p) const override {
    if (_can_paint==false)
      return;
      
    float first_x = _points[0].x();
    float first_y = _points[0].y();
    float last_x = _points[_size-1].x();
    float last_y = _points[_size-1].y();
      
    int fill_size;
      
    if (_fill_x1 == -1){
      _points[_size]   = QPointF(last_x, _y2);
      _points[_size+1] = QPointF(first_x, _y2);
      fill_size = _size+2;
    } else {
      _points[_size]   = QPointF(_x2, last_y);
      _points[_size+1] = QPointF(_x2, _y2);
      _points[_size+2] = QPointF(_x1, _y2);
      _points[_size+3] = QPointF(_x1, first_y);
      fill_size = _size+4;
    }
        
    //for(int i=0 ; i < _size+2 ; i++)
    //  printf("  %d/%d: %d , %d  (y1: %f, y2: %f)\n", i, _size, (int)_points[i].x(), (int)_points[i].y(), y1, y2);
      
    if (_fill_color.isValid()) {

      // 2. Background fill
        
      p->setPen(Qt::NoPen);
      p->setBrush(_fill_color);
        
      p->drawPolygon(_points, fill_size);
        
      p->setBrush(Qt::NoBrush);

    } else {

      double org_opacity = p->opacity();
        
      // 3. stipled line.
      p->setOpacity(0.35*org_opacity);

#if 0
      QPen pen(_color);
      pen.setWidthF(_paint_nodes ? root->song->tracker_windows->systemfontheight / 3 : root->song->tracker_windows->systemfontheight / 6);
      p->setPen(pen);

      p->drawPolygon(_points, fill_size);
#else
      p->setPen(Qt::NoPen);
      p->setBrush(_color);
        
      p->drawPolygon(_points, fill_size);
        
      p->setBrush(Qt::NoBrush);
#endif
      p->setOpacity(org_opacity);

    }

  }

  void paint_lines(QPainter *p) const override {
    if (_can_paint==false || _paint_lines==false)
      return;
      
    QPen pen(_color);
    pen.setWidthF(_paint_nodes ? root->song->tracker_windows->systemfontheight / 3 : root->song->tracker_windows->systemfontheight / 6);
    p->setPen(pen);
    p->drawPolyline(_points, _size);
  }

  void paint_nodes(QPainter *p) const override {
    if (_can_paint==false || _paint_nodes==false)
      return;

    int node_pos = _start_i;
    for(int i = _start_i; i < _size ; i++){
      const T &node = _automation.at(node_pos);
        
      float x_a = _points[i].x();
      float y_a = _points[i].y();
      paint_node(p, x_a, y_a, node_pos, _color);
        
      if (node.logtype==LOGTYPE_HOLD)
        i++;
        
      node_pos++;
    }

  }
};
  

template <typename T> class SeqAutomation{
  
private:
  
  QVector<T> &_automation;

  QVector<T> _automation_storage; // Used if constructor didn't get a QVector<T> as argument.
  
public:
  struct RT{
    int num_nodes;
    T *nodes;

    RT(const RT&) = delete;
    RT& operator=(const RT&) = delete;

    RT(const QVector<T> &automation){

      num_nodes = automation.size();
  
      nodes = new T[num_nodes];
    
      for(int i=0 ; i < num_nodes ; i++)
        nodes[i] = automation.at(i);
    }

    ~RT(){
      delete[] nodes;
    }
  };
private:
  int _curr_nodenum = -1;
  bool _paint_nodes = false;

  mutable const SeqAutomationPainter<T> *_last_painter = NULL;
  
  AtomicPointerStorage<RT> _rt;

  // Double free / using data after free, if trying to copy data.
  SeqAutomation(const SeqAutomation&) = delete;
  SeqAutomation& operator=(const SeqAutomation&) = delete;


  static void free_rt(RT *rt){
    delete rt;
  }

public:

  SeqAutomation(QVector<T> &automation)
    : _automation(automation)
    , _rt(free_rt)
  {
  }

  SeqAutomation()
    : SeqAutomation(_automation_storage)
  {
  }

  ~SeqAutomation(){
    delete _last_painter;
  }

  void *new_rt_data_has_been_created_data = NULL;
  void (*new_rt_data_has_been_created)(void *data) = NULL;

  // Scope to protect node pointers that can be accessed in a realtime thread.
  // I.e. 'node1' and 'node2' can not be used outside the scope (the array holding the nodes could be deleted immediately after the scope runs out).
  struct ScopedRtAccess{

    const RT_AtomicPointerStorage_ScopedUsage<RT> rt_pointer;

    const T *node1=NULL;
    const T *node2=NULL;

    ScopedRtAccess(SeqAutomation<T> &instance)
      : rt_pointer(&instance._rt)
    {
    }
  };
  
private:
  
  /*
  int get_size(int num_nodes) const {
    return sizeof(struct RT) + num_nodes*sizeof(T);
  }
  */

  struct RT *create_rt(void) const {
    return new RT(_automation);
  }

  void create_new_rt_data(void){
    struct RT *new_rt = create_rt();

    _rt.set_new_pointer(new_rt);

    if (new_rt_data_has_been_created != NULL)
      new_rt_data_has_been_created(new_rt_data_has_been_created_data);
  }


public:

  bool do_paint_nodes(void) const {
    return _paint_nodes;
  }

  // called very often.
  bool set_do_paint_nodes(bool do_paint_nodes){
    if (do_paint_nodes != _paint_nodes){
      _paint_nodes = do_paint_nodes;
      return true;
    } else {
      return false;
    }
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
  
  static double get_value(double time, const double time1, const double time2, int das_logtype1_das, double value1, double value2) {
    if (das_logtype1_das==LOGTYPE_LINEAR) {
      if (equal_doubles(time1, time2))
        return value2; // (value1 + value2) / 2.0;
      else
        return scale_double(time, time1, time2, value1, value2);
    } else
      return value1;
  }

  static double get_value(double time, const T *node1, const T *node2, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL) {
    const double time1 = node1->time;
    const double time2 = node2->time;
    
    const int logtype1 = node1->logtype;

    R_ASSERT_NON_RELEASE(time >= time1);
    R_ASSERT_NON_RELEASE(time <= time2);

    if (custom_get_value!=NULL && logtype1==LOGTYPE_LINEAR && !equal_doubles(time1, time2))
      return custom_get_value(time, node1, node2);
    else
      return get_value(time, time1, time2, logtype1, node1->value, node2->value);
  }

  static double get_value(const QVector<T> &automation, double time, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL) {
    int size = automation.size();

    if (size==0)
      return 0;

    if (size==1)
      return automation.at(0).value;

    int nodenum = get_node_num(automation, time);
    if (nodenum==-1)
      return automation.at(0).value;

    if (nodenum >= size-1)
      return automation.at(nodenum-1).value;

    return get_value((double)time, &automation.at(nodenum), &automation.at(nodenum+1), custom_get_value);
  }

  double get_value(double time, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL) const {
    return get_value(_automation, time, custom_get_value);
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

  // This function is tested in ../test/test_seqautomation.cpp (run by calling "./build_linux test")
  SeqAutomationReturnType RT_get_nodes(double time, ScopedRtAccess &rt_access){
    R_ASSERT_NON_RELEASE(_RT_last_search_pos > 0);
    R_ASSERT_NON_RELEASE(rt_access.node1==NULL);
    R_ASSERT_NON_RELEASE(rt_access.node2==NULL);

    const struct RT *rt = (const struct RT*)rt_access.rt_pointer.get_pointer();

    if (rt!=NULL) {

      const int num_nodes = rt->num_nodes;

      if (num_nodes==0) {

        return SeqAutomationReturnType::NO_VALUES;

      } else if (time >= rt->nodes[num_nodes-1].time){

        rt_access.node1 = &rt->nodes[num_nodes-1];

        return SeqAutomationReturnType::NO_MORE_VALUES;
        
      } else if (time <= rt->nodes[0].time){

        if (equal_doubles(time, rt->nodes[0].time)){
          
          rt_access.node1 = &rt->nodes[0];
          rt_access.node2 = &rt->nodes[1];

          return SeqAutomationReturnType::VALUE_OK;
          
        } else {
          
          rt_access.node2 = &rt->nodes[0];

          return SeqAutomationReturnType::NO_VALUES_YET;
        }
        
        
      } else {
      
        const T *node1_;
        const T *node2_;
        int i = _RT_last_search_pos;
        
        R_ASSERT_NON_RELEASE(i >= 0);

        if (i>0 && i<num_nodes){
          
          node1_ = &rt->nodes[i-1];
          node2_ = &rt->nodes[i];
          if (time >= node1_->time && time <= node2_->time) // Same position in array as last time. No need to do binary search. This is the path we usually take.
            goto gotit;
        }
        
        i = BinarySearch_Left(rt, time, 0, num_nodes-1);

        if (i > 0){
        
          _RT_last_search_pos = i;
          node1_ = &rt->nodes[i-1];
          node2_ = &rt->nodes[i];

        } else {
          
          rt_access.node1 = &rt->nodes[0];
          rt_access.node2 = &rt->nodes[1];

          R_ASSERT(false);

          return SeqAutomationReturnType::NO_VALUES;
                  
        }
        
      gotit:

        rt_access.node1 = node1_;
        rt_access.node2 = node2_;

        return SeqAutomationReturnType::VALUE_OK;

      }
    }

    return SeqAutomationReturnType::NO_VALUES;    
  }


  SeqAutomationReturnType RT_get_value(double time, double &value, bool always_set_value = false) {
    ScopedRtAccess rt_access(*this);
      
    const SeqAutomationReturnType ret = RT_get_nodes(time, rt_access);

    if (ret==SeqAutomationReturnType::NO_VALUES){
      if(always_set_value){
        R_ASSERT(false);
        value = 0.0;
      }
      return ret;
    }

    if (always_set_value){

      if(ret==SeqAutomationReturnType::NO_VALUES_YET)
        value = rt_access.node2->value;

      else if(ret==SeqAutomationReturnType::NO_MORE_VALUES)
        value = rt_access.node1->value;
    }

    if(ret==SeqAutomationReturnType::VALUE_OK)
      value = get_value(time, rt_access.node1, rt_access.node2);
    
    return ret;
  }

#if 0
  // Note: Value is not set if rt->num_nodes==0, even if always_set_value==true.  
  SeqAutomationReturnType RT_get_value(double time, double &value, double (*custom_get_value)(double time, const T *node1, const T *node2) = NULL, bool always_set_value = false, const T **target_node = NULL, bool we_only_need_target_node = false) {

    R_ASSERT_NON_RELEASE(_RT_last_search_pos > 0);

    RT_AtomicPointerStorage_ScopedUsage rt_pointer(&_rt);
      
    const struct RT *rt = (const struct RT*)rt_pointer.get_pointer();

    if (rt!=NULL) {

      const int num_nodes = rt->num_nodes;
      
      if (num_nodes==0) {
        
        return SeqAutomationReturnType::NO_VALUES;
                
      } else if (time < rt->nodes[0].time){
        
        if (always_set_value && we_only_need_target_node==false)
          value = rt->nodes[0].value;

        if (target_node)
          *target_node = &rt->nodes[0];

        return SeqAutomationReturnType::NO_VALUES_YET;
        
      } else if (time == rt->nodes[0].time) {
        
        if (we_only_need_target_node==false)
          value = rt->nodes[0].value;

        if (target_node)
          *target_node = &rt->nodes[0];
        
        return SeqAutomationReturnType::VALUE_OK;

      } else if (num_nodes==1) {
        
        if (always_set_value && we_only_need_target_node==false)
          value = rt->nodes[0].value;

        if (target_node)
          *target_node = &rt->nodes[0];

        return SeqAutomationReturnType::NO_MORE_VALUES;
        
      } else if (time > rt->nodes[num_nodes-1].time){

        if (always_set_value && we_only_need_target_node==false)
          value = rt->nodes[num_nodes-1].value;

        if (target_node)
          *target_node = &rt->nodes[num_nodes-1];
        
        return SeqAutomationReturnType::NO_MORE_VALUES;
        
      } else {
      
        const T *node1;
        const T *node2;
        int i = _RT_last_search_pos;
        
        R_ASSERT_NON_RELEASE(i >= 0);
        
        if (i<num_nodes){
          node1 = &rt->nodes[i-1];
          node2 = &rt->nodes[i];
          if (time >= node1->time && time <= node2->time) // Same position in array as last time. No need to do binary search. This is the path we usually take.
            goto gotit;
        }
        
        i = BinarySearch_Left(rt, time, 0, num_nodes-1);
        R_ASSERT_NON_RELEASE(i>0);
        
        _RT_last_search_pos = i;
        node1 = &rt->nodes[i-1];
        node2 = &rt->nodes[i];
        
      gotit:

        if (target_node)
          *target_node = node1;
        
        if (we_only_need_target_node==false)
          value = get_value(time, node1, node2, custom_get_value);

        return SeqAutomationReturnType::VALUE_OK;

      }
    }

    return SeqAutomationReturnType::NO_VALUES;
  }
#endif

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
  
  bool set_curr_nodenum(int nodenum){
    if(nodenum != _curr_nodenum){
      _curr_nodenum = nodenum;
      return true;
    } else {
      return false;
    }
  }

  static int get_node_num(const QVector<T> &automation, double time) {
    int size = automation.size();

    for(int i=0;i<size;i++)
      if (time < automation.at(i).time)
        return i-1;

    return size-1;
  }

  int get_node_num(double time) const {
    return get_node_num(_automation, time);
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

    if (false && !equal_doubles(node->time, new_node.time)){
      _automation.remove(nodenum);
      add_node(new_node);
    } else {
      *node = new_node;
      create_new_rt_data();
    }
            
  }

  void cut_after(double time){
    R_ASSERT_RETURN_IF_FALSE(_automation.size() > 0);
    
    if (time >= _automation.last().time)
      return;
        
    QVector<T> new_automation;
    
    if (time <= 0){
      
      RError("radium::SeqAutomation::cut_after: Time<=0. Time: %f\n", time);
      T node = _automation.at(0);
      node.time = 0;      
      new_automation.push_back(node);
      node.time = 10;
      new_automation.push_back(node);
            
    } else if (_automation.at(0).time >= time || _automation.size()==1) {
      
      T node = _automation.at(0);
      
      node.time = 0;      
      new_automation.push_back(node);
      
      node.time = time;
      new_automation.push_back(node);
      
    } else {
    
      T node1;
      T node2;
      bool got_node1 = false;
      bool got_node2 = false;

      for(const auto &node : _automation){
        
        if (node.time <= time) {
          node1 = node;
          new_automation.push_back(node);
          got_node1 = true;
          
        } else {
          R_ASSERT(got_node1);
          node2 = node;
          got_node2 = true;
          goto gotit;
          
        }
        
      }

      R_ASSERT(false);
      
    gotit:
      if(got_node1==false || got_node2==false){
        R_ASSERT(false);        
      } else {

        T node = node1;
        node.time = time;
        
        if (node.logtype == LOGTYPE_LINEAR){

          if(equal_doubles(node1.time, node2.time)){

            R_ASSERT_NON_RELEASE(false);
            node.value = node2.value;

          } else {

            node.value = scale_double(time, node1.time, node2.time, node1.value, node2.value);
          }
        }

        new_automation.push_back(node);
      }
    }

    _automation = new_automation;
    
    create_new_rt_data();    
  }

  void extend_last_node(double new_duration, radium::PlayerLockOnlyIfNeeded *lock){
    R_ASSERT_RETURN_IF_FALSE(_automation.size() > 0);
    
    T &last_node = _automation.last();

    R_ASSERT_RETURN_IF_FALSE(last_node.time < new_duration);
                             
    last_node.time = new_duration;

    {
      radium::PlayerLockOnlyIfNeeded::ScopedLockPause pause(lock);
      create_new_rt_data();
    }
  }
  
  void duration_has_changed(double new_duration, radium::PlayerLockOnlyIfNeeded *lock){
    int size = _automation.size();
    
    R_ASSERT_RETURN_IF_FALSE(size > 0);
      
    T &last_node = _automation.last();
    
    if (equal_doubles(new_duration, last_node.time))
      return;
    
    for(T &node : _automation){
      if (node.time > new_duration){
        radium::PlayerLockOnlyIfNeeded::ScopedLockPause pause(lock);
        return cut_after(new_duration);
      }
    }

    if (size > 1){
      
      T second_last_node = _automation.at(size-2);
      
      if (equal_doubles(second_last_node.value, last_node.value))
        return extend_last_node(new_duration, lock);
        
    }
    
    {
      radium::PlayerLockOnlyIfNeeded::ScopedLockPause pause(lock);
      
      T new_node = last_node;
      new_node.time = new_duration;
      
      add_node(new_node);
    }
  }
  
  void reset(void){
    _automation.clear();
    create_new_rt_data();
  }

public:

  void print(void) const {
    for(int i = 0 ; i < _automation.size()-1 ; i++){
      const T &node1 = _automation.at(i);
      const T &node2 = _automation.at(i+1);
      printf("%d: Value: %f -> %f. (Time: %f -> %f)\n", i, node1.value, node2.value, node1.time, node2.time);
    }
  }

  

  const SeqAutomationPainter<T> *get_painter(float x1, float y1, float x2, float y2,
                                       double start_time, double end_time,
                                       const QColor &color,
                                       float (*get_y)(const T &node, float y1, float y2, void *data),
                                       float (*get_x)(const T &node, double start_time, double end_time, float x1, float x2, void *data) = NULL,
                                       void *data = NULL,
                                       const QColor fill_color = QColor(),
                                       float fill_x1 = -1, float fill_x2 = -1                                       
                                       ) const
  {
    const SeqAutomationPainter<T> *painter = new SeqAutomationPainter<T>(_automation,
                                                                         _curr_nodenum, _paint_nodes,
                                                                         x1, y1, x2, y2,
                                                                         start_time, end_time,
                                                                         color,
                                                                         get_y,
                                                                         get_x,
                                                                         data,
                                                                         fill_color,
                                                                         fill_x1, fill_x2);
    if (_last_painter != NULL)
      delete _last_painter;
    
    _last_painter = painter;

    return painter;
  }

  
  void paint(QPainter *p,
             float x1, float y1, float x2, float y2,
             double start_time, double end_time,
             const QColor &color,
             float (*get_y)(const T &node, float y1, float y2, void *data),
             float (*get_x)(const T &node, double start_time, double end_time, float x1, float x2, void *data) = NULL,
             void *data = NULL,
             const QColor fill_color = QColor(),
             float fill_x1 = -1, float fill_x2 = -1
             ) const
  {
    const SeqAutomationPainter<T> painter(_automation,
                                          _curr_nodenum, _paint_nodes,
                                          x1, y1, x2, y2,
                                          start_time, end_time,
                                          color,
                                          get_y,
                                          get_x,
                                          data,
                                          fill_color,
                                          fill_x1, fill_x2);
    painter.paint_fill(p);
    painter.paint_lines(p);
    painter.paint_nodes(p);
  }

  void sort_qvector(QVector<T> &ret) const {
    std::sort(ret.begin(), ret.end(), 
              [](const T &a, const T &b){
                return a.time < b.time;
              });
  }

  void sort_qvector_if_necessary(QVector<T> &ret) const {
    double last_time = 0;
    for(const T &node : ret){
      if(node.time < last_time){
        sort_qvector(ret);
        return;
      }
      last_time = node.time;
    }
  }

  // Always returns a sorted vector.
  QVector<T> make_qvector_from_state(const dyn_t &dynstate, const NodeFromStateProvider<T> *nsp, double state_samplerate) const {
    QVector<T> ret;

    const dynvec_t *vec = dynstate.array;
    
    bool needs_sorting = false;

    double last_time = 0;

    for(const dyn_t &dyn : vec){
      T node = nsp->create_node_from_state(dyn.hash, state_samplerate);
      if(node.time < 0)
        node.time = 0;

      if (node.time < last_time)
        needs_sorting = true;

      last_time = node.time;

      ret.push_back(node);
    }

    if (needs_sorting)
      sort_qvector(ret);

    return ret;
  }

  QVector<T> make_qvector_from_state(const dyn_t &dynstate, T (*create_node_from_state_func)(hash_t *,double), double state_samplerate) const {

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
    
    return make_qvector_from_state(dynstate, &myprovider, state_samplerate);
  }

  void set_qvector(const QVector<T> &automation){
    _automation_storage = automation;
    _automation = _automation_storage;

    sort_qvector_if_necessary(_automation);
      
    create_new_rt_data();    
  }

  const QVector<T> &get_qvector(void) const {
    return _automation;
  }

  // Note: Earlier, dynstate was a hash table. Should probably remove that code now, and rather give message about converting the song using a semi-old version of Radium. It's many years ago now since hash tables were used.
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

      set_qvector(make_qvector_from_state(dynstate, nsp, state_samplerate));

    } else {
      
      R_ASSERT(false);
      
    }
  }
  
  void create_from_state(const dynvec_t &dynstate, const NodeFromStateProvider<T> *nsp, double state_samplerate){
    create_from_state(DYN_create_array(dynstate), nsp, state_samplerate);
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

  void create_from_state(const dynvec_t &dynstate, T (*create_node_from_state_func)(hash_t *,double), double state_samplerate){
    create_from_state(DYN_create_array(dynstate), create_node_from_state_func, state_samplerate);
  }
  
  dynvec_t get_state(hash_t *(*get_node_state)(const T &node, void*), void *data) const {
    int size = _automation.size();
    
    dynvec_t ret = {};
    
    for(int i = 0 ; i < size ; i++)
      DYNVEC_push_back(ret, DYN_create_hash(get_node_state(_automation.at(i), data)));

    return ret;
  }

};

template <typename T> 
class SeqAutomationIterator{
  const SeqAutomation<T> _automation_storage;
  const SeqAutomation<T> &_automation;

  struct Something {
#if !defined(RELEASE)
    double prev_time = -1;
#endif
    
    int size;

    int n;
    const T *node2 = NULL;
    double time1;
    double time2;
    double value1;
    double value2;
    int logtype1;
  } _t;
  
  void init_something(void){
    _t.size = _automation.size();
    
    R_ASSERT_RETURN_IF_FALSE(_t.size > 0);

    const T &_node1 = _automation.at(0);
    _t.time1 = _node1.time;
    _t.value1 = _node1.value;
    _t.logtype1 = _node1.logtype;

    _t.n = 1;

    if (_t.size >= 2){
      _t.node2 = &_automation.at(_t.n);
      _t.time2 = _t.node2->time;
      _t.value2 = _t.node2->value;
    } else {
      _t.value2 = _t.value1; // Used in case there is only one node.
    }
  }

public:

  SeqAutomationIterator(const SeqAutomation<T> &automation)
    : _automation(automation)
  {
    init_something();
  }

  SeqAutomationIterator(QVector<T> &automation)
    : _automation_storage(automation)
    , _automation(_automation_storage)
  {
    init_something();
  }


  SeqAutomationIterator(const SeqAutomationIterator &obj)
    : _automation(obj._automation)
    , _t(obj._t)
  {
  }

  SeqAutomationReturnType return_no_more_values(const T **node1, const T **node2) const {
    *node1 = &_automation.last();
    *node2 = NULL;
    return SeqAutomationReturnType::NO_MORE_VALUES;
  }

  SeqAutomationReturnType get_nodes(double time, const T **node1, const T **node2){
#if !defined(RELEASE)
    if(time<0)
      abort();
    if(time<=_t.prev_time)
      abort();
    _t.prev_time = time;
#endif

    if (_t.n==_t.size)
      return return_no_more_values(node1,node2);

    if (time < _t.time1){
      *node1 = NULL;
      *node2 = &_automation.at(0);
      return SeqAutomationReturnType::NO_VALUES_YET;
    }

    if (time <= _t.time2){

      *node1 = &_automation.at(_t.n-1);
      *node2 = _t.node2;

    } else {

      do{
        _t.n++;
        
        if (_t.n==_t.size)
          return return_no_more_values(node1,node2);
        
        *node1 = _t.node2;
        _t.time1 = _t.time2;
        _t.value1 = _t.value2;
        _t.logtype1 = _t.node2->logtype;
        
        _t.node2 = &_automation.at(_t.n);
        *node2 = _t.node2;
        _t.time2 = _t.node2->time;
        _t.value2 = _t.node2->value;
        
      }while(time > _t.time2);

    }

    return SeqAutomationReturnType::VALUE_OK;
  }

  double get_value(double time){
#if !defined(RELEASE)
    if(time<0)
      abort();
    if(time<=_t.prev_time)
      abort();
    _t.prev_time = time;
#endif

    if (_t.n==_t.size)
      return _t.value2;

    if (time < _t.time1)
      return _t.value1;

    while(time > _t.time2){
      _t.n++;

      if (_t.n==_t.size)
        return _t.value2;

      _t.time1 = _t.time2;
      _t.value1 = _t.value2;
      _t.logtype1 = _t.node2->logtype;

      _t.node2 = &_automation.at(_t.n);
      _t.time2 = _t.node2->time;
      _t.value2 = _t.node2->value;
    }

    return _automation.get_value(time, _t.time1, _t.time2, _t.logtype1, _t.value1, _t.value2);
  }
};

}

#endif
