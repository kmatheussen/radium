
#pragma once


#include <QFrame>
#include <QVBoxLayout>
#include <QTimer>

#include "helpers.h"
#include "Qt_colors_proc.h"


namespace radium{

enum class KeyboardFocusFrameType
  {
   EDITOR = 0,
   MIXER = 1,
   MIXERSTRIPS = 2,
   SEQUENCER = 3,
   NUM_TYPES = 4
  };


struct KeyboardFocusFrame;

}

extern radium::KeyboardFocusFrameType g_curr_focus_type;
extern radium::KeyboardFocusFrameType g_prev_focus_type;
extern QVector<radium::KeyboardFocusFrame*> g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::NUM_TYPES];

void FOCUSFRAMES_init(void);
void FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType type, bool has_focus);
void FOCUSFRAMES_set_focus_best_guess(void);
bool FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType type);


namespace radium{

struct KeyboardFocusFrame : public QFrame{
  int _last_fontheight = -1;

  KeyboardFocusFrameType _type;

  bool _has_focus;

 public:
  KeyboardFocusFrame(QWidget *parent, KeyboardFocusFrameType type, bool create_standard_layout)
    : QFrame(parent)
    , _type(type)
    , _has_focus(type==KeyboardFocusFrameType::EDITOR)
  {
    R_ASSERT(_type != KeyboardFocusFrameType::NUM_TYPES);
    setFrameShape(QFrame::Box);
    /*
    QPalette pal = palette();
    pal.setColor(QPalette::WindowText, QColor("green"));
    setPalette(pal);
    */

    if(create_standard_layout){
      QVBoxLayout *layout = new QVBoxLayout(this);
      
      layout->setSpacing(0);
      layout->setContentsMargins(0,0,0,0);
      
      setLayout(layout);
    }
    
    //R_ASSERT(g_keyboard_focus_frames[(int)_type]==NULL);
    g_keyboard_focus_frames[(int)_type].push_back(this);

    maybe_update_width();
  }

  ~KeyboardFocusFrame(){
    R_ASSERT(g_keyboard_focus_frames[(int)_type].removeAll(this)==1);
  }
  
  /*
  void updateAll(void) {
    for(auto *focus : g_keyboard_focus_frames)
      focus->update();
  }
  */

  
  
  void set_focus(bool has_focus){
    
    if (has_focus != _has_focus){
      
      _has_focus = has_focus;
      
      if (has_focus)
        g_curr_focus_type = _type;

      /*

      // Doesn't work. Qt refuses to behave in a sane way.

      if (has_focus)
        setLineWidth(15);
      else
        setLineWidth(1);

      if (has_focus)
        setFrameShape(QFrame::Box);
      else
        setFrameShape(QFrame::NoFrame);


      for(auto *c : children()){
        QWidget *w = dynamic_cast<QWidget*>(c);      
        if (w && w->isWindow()==false)
          setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
      }
      */
      
      update();
      
      for(auto focuses : g_keyboard_focus_frames)
        for(auto *focus : focuses){
          if(focus->_type != _type){
            if (focus->_has_focus){
              g_prev_focus_type = focus->_type;
              focus->_has_focus = false;
              focus->update();
              if(focus->_type==KeyboardFocusFrameType::EDITOR)
                root->song->tracker_windows->must_redraw_editor = true;
            }
          }
        }
    }
  }
  
  bool has_focus(void){
    return _has_focus;
  }
  
  void maybe_update_width(void){
    if (_last_fontheight != root->song->tracker_windows->systemfontheight){
      // schedule to run later since we may have been called from paintEvent()
      QTimer::singleShot(1,[this]
                         {
                           _last_fontheight = root->song->tracker_windows->systemfontheight;
                           setLineWidth(R_MAX(1, _last_fontheight  / (_type==KeyboardFocusFrameType::EDITOR ? 7.8 : 4.8)));
                         }
                         );
    }
  }

  void paintEvent ( QPaintEvent * ev ) override {    
    TRACK_PAINT();

    maybe_update_width();

    QPainter p(this);

    QColor color(get_qcolor(_has_focus ? KEYBOARD_FOCUS_BORDER_COLOR_NUM : HIGH_BACKGROUND_COLOR_NUM));

    {
      QPen pen(color);
      pen.setWidth(lineWidth()*2);
      p.setPen(pen);
      
      p.drawRect(rect());
    }

    if (_has_focus && lineWidth()*2 > 2){
      QPen pen(color.darker());
      pen.setWidth(2);    
      p.setPen(pen);
      
      p.drawRect(rect());
    }
  }

  void enterEvent(QEvent * event) override {
    FOCUSFRAMES_set_focus(_type, true);
  }
};

}
