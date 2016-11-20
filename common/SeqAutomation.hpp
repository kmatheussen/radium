
#ifndef _RADIUM_COMMON_SEQAUTOMATION_HPP
#define _RADIUM_COMMON_SEQAUTOMATION_HPP

#include <QVector>
#include <QPainter>

namespace radium{

template <typename T> class SeqAutomation{
  
private:
  
  QVector<T> _automation;

  struct RT{
    int num_nodes;
    T nodes[];
  };

  int _curr_nodenum = -1;

  AtomicPointerStorage _rt;

public:

  SeqAutomation()
    : _rt(V_free_function)
  {
  }
  

private:
  
  int get_size(int num_nodes) const{
    return sizeof(struct RT) + num_nodes*sizeof(T);
  }

  const struct RT *create_rt(void) const{
    struct RT *rt = (struct RT*)V_malloc(get_size(_automation.size()));
  
    rt->num_nodes=_automation.size();
    
    for(int i=0 ; i<_automation.size() ; i++)
      rt->nodes[i] = _automation.at(i);
    
    return (const struct RT*)rt;
  }


  void create_new_rt_data(void){
    const struct RT *new_rt_tempo_automation = create_rt();

    _rt.set_new_pointer((void*)new_rt_tempo_automation);
  }


public:


  int size(void) const{
    return _automation.size();
  }

  const T &at(int n) const{
    return _automation.at(n);
  }

  const T &last(void) const{
    R_ASSERT(size()>0);
    return at(size()-1);
  }
  
  bool RT_get_value(double abstime, const T *node1, const T *node2, double &value, double (*custom_get_value)(double abstime, const T *node1, const T *node2)) const {
    double abstime1 = node1->abstime;
    double abstime2 = node2->abstime;
    
    if (abstime >= abstime1 && abstime < abstime2){

      int logtype1 = node1->logtype;
        
      if (logtype1==LOGTYPE_LINEAR){

        if (abstime1==abstime2) {
          
          value = (node1->value+node2->value) / 2.0;
          
        } else {

          if (custom_get_value != NULL)        
            value = custom_get_value(abstime, node1, node2);
          else
            value = scale_double(abstime, abstime1, abstime2, node1->value, node2->value);
          
        }
        
      }else {
        
        value = node1->value;
        
      }

      return true;
      
    } else {

      return false;
      
    }
  }

  double RT_get_value(double abstime, double (*custom_get_value)(double abstime, const T *node1, const T *node2)){

    RT_AtomicPointerStorage_ScopedUsage rt_pointer(&_rt);
      
    const struct RT *rt = (const struct RT*)rt_pointer.get_pointer();

    if (rt!=NULL) {
    
      for(int i = 0 ; i < rt->num_nodes-1 ; i++){
        const T *node1 = &rt->nodes[i];
        const T *node2 = &rt->nodes[i+1];
        
        double value;
        if (RT_get_value(abstime, node1, node2, value, custom_get_value))
          return value;
      }
      
    }
    
    return 1.0; //rt_tempo_automation->nodes[rt_tempo_automation->num-1].value;
  }


  int get_curr_nodenum(void){
    return _curr_nodenum;
  }
  
  void set_curr_nodenum(int nodenum){
    _curr_nodenum = nodenum;
  }

  int get_node_num(double abstime) const {
    int size = _automation.size();
    if (size==0)
      return 0;
    if (size==1)
      return 1;
    
    double abstime1 = at(0).abstime;
    //R_ASSERT_RETURN_IF_FALSE2(abstime1==0,1);

    for(int i=1;i<size;i++){
      double abstime2 = at(i).abstime;
      
      if (abstime >= abstime1 && abstime < abstime2)
        return i;
      
      abstime1 = abstime2;
    }
    
    return size;
  }

  int add_node(const T &node){
    double abstime = node.abstime;

    R_ASSERT(abstime >= 0);

    if (abstime < 0)
      abstime = 0;
    
    int nodenum = get_node_num(abstime);
    
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

    if (node->abstime != new_node.abstime){
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

  void paint_node(QPainter *p, float x, float y, int nodenum, QColor color) const {
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

  void paint(QPainter *p, float x1, float y1, float x2, float y2, double start_time, double end_time, QColor color, float (*get_y)(const T &node, float y1, float y2)) const {
  
    QPen pen(color);
    pen.setWidthF(2.3);
    
    for(int i = 0 ; i < _automation.size()-1 ; i++){
      const T &node1 = _automation.at(i);
      double abstime1 = node1.abstime;

      if (abstime1 >= end_time)
        break;
      
      const T &node2 = _automation.at(i+1);
      double abstime2 = node2.abstime;

      if (abstime2 < start_time)
        continue;
      
      float x_a = scale(abstime1, start_time, end_time, x1, x2);
      float x_b = scale(abstime2, start_time, end_time, x1, x2);
      
      float y_a = get_y(node1, y1, y2);
      float y_b = get_y(node2, y1, y2);
      
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
      
      if(i==_automation.size()-2)
        paint_node(p, x_b, y_b, i+1, color);
    }
  }


  void create_from_state(const hash_t *state, T (*create_node_from_state)(hash_t *state)){
    int size = HASH_get_array_size(state);
    
    _automation.clear();
    
    for(int i = 0 ; i < size ; i++)
      add_node(create_node_from_state(HASH_get_hash_at(state, "node", i)));
  }
  

  hash_t *get_state(hash_t *(*get_node_state)(const T &node)) const {
    int size = _automation.size();
    
    hash_t *state = HASH_create(size);
    
    for(int i = 0 ; i < size ; i++)
      HASH_put_hash_at(state, "node", i, get_node_state(_automation.at(i)));
    
    return state;
  }

};

}

#endif
