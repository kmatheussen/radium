

#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#include <QApplication>
#include <QCursor>
#include <QVector>

#include "../common/nsmtracker.h"
#include "../api/api_gui_proc.h"

#include "Qt_Bs_edit_proc.h"


#include "KeyboardFocusFrame.hpp"


namespace radium
{

void KeyboardFocusFrame::set_focus(bool has_focus)
{
	if (has_focus == _has_focus && has_focus == false)
		return;

	if (has_focus != _has_focus)
	{
		_has_focus = has_focus;

		if (has_focus)
			g_curr_focus_type = _type;

		update();
	}

	// Must also clear the other focus frames when this frame already had
	// focus. Otherwise, if two frames of different types somehow both have
	// focus (this can happen if a frame is created while another frame has
	// focus), neither of them will ever be able to take the focus back.
	if (has_focus)
	{
		for(auto focuses : g_keyboard_focus_frames)
			for(auto *focus : focuses)
			{
				if(focus->_type != _type)
				{
					if (focus->_has_focus)
					{
						g_prev_focus_type = focus->_type;
						focus->_has_focus = false;
						{
							QWidget *focused = QApplication::focusWidget();
							if (focused && focus->isAncestorOf(focused))
								focused->clearFocus();
						}
						focus->update();
						if(focus->_type==KeyboardFocusFrameType::EDITOR)
						{
							root->song->tracker_windows->must_redraw_editor = true;
						}
					}
				}
			}
	}
}

void KeyboardFocusFrame::enterEvent(QEnterEvent * event)
{
	//printf("kf::Enter. Right type? %d\n", _type==KeyboardFocusFrameType::EDITOR);
	
    FOCUSFRAMES_set_focus(_type, true);

	if(_type==KeyboardFocusFrameType::EDITOR)
		if (root && root->song && root->song->tracker_windows)
			root->song->tracker_windows->must_redraw_editor = true;
}
} // radium namespace

QVector<radium::KeyboardFocusFrame*> g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::NUM_TYPES] = {};

namespace{

  struct Obj : public QObject {

    QWidget *_edit_gui = NULL;
    
    void handle_mouse_widget(QWidget *w){
      if (w==NULL)
        return;

      if (_edit_gui==NULL)
        _edit_gui = API_get_editGui();

      radium::KeyboardFocusFrame *gakk = dynamic_cast<radium::KeyboardFocusFrame *>(w);
      
      if (g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::EDITOR].contains(gakk) || w==_edit_gui)
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::EDITOR, true);
      
      else if (g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::MIXER].contains(gakk))
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, true);
      
      else if (g_keyboard_focus_frames[(int)radium::KeyboardFocusFrameType::SEQUENCER].contains(gakk) || w==BS_get())
        FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::SEQUENCER, true);

      else
        handle_mouse_widget(w->parentWidget());
    }

    bool eventFilter(QObject *obj, QEvent *event) override {
      // Note that we set focus on mouse moves as well, and not only on mouse
      // presses. QEvent::Enter/Leave is not always delivered to the focus
      // frames (seen when running in full screen on macOS), so we can not rely
      // on KeyboardFocusFrame::enterEvent() alone.
      if (event->type()==QEvent::GraphicsSceneMousePress
          || event->type()==QEvent::MouseButtonPress
          || event->type()==QEvent::MouseMove)
      {
        QWidget *widget = qobject_cast<QWidget*>(obj);
        handle_mouse_widget(widget);
      }

      return false;
    }
  };
  
  static Obj g_obj;
}


// Sets keyboard focus to the widget under the mouse pointer. Can be called to
// sync up the focus with the mouse pointer, for instance after the window has
// been resized or switched to full screen.
void FOCUSFRAMES_update_from_mouse_pos(void)
{
  QWidget *widget = QApplication::widgetAt(QCursor::pos());
  if (widget != NULL)
    g_obj.handle_mouse_widget(widget);
}


void FOCUSFRAMES_init(void){
  static bool has_installed_event_filter=false;
  
  if (has_installed_event_filter==false){
    has_installed_event_filter=true;
    qApp->installEventFilter(&g_obj);
  }
}


radium::KeyboardFocusFrameType g_curr_focus_type = radium::KeyboardFocusFrameType::EDITOR;
radium::KeyboardFocusFrameType g_prev_focus_type = radium::KeyboardFocusFrameType::SEQUENCER;
  
void FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType type, bool has_focus){
  //R_ASSERT_RETURN_IF_FALSE(g_keyboard_focus_frames[(int)type] != NULL);

	//printf("Set focus/START. Right type? %d\n", type==radium::KeyboardFocusFrameType::EDITOR);
		
	for(auto *frame : g_keyboard_focus_frames[(int)type])
		frame->set_focus(has_focus);

	//printf("Set focus/END. Right type? %d\n", type==radium::KeyboardFocusFrameType::EDITOR);
}

static bool set_if_visible(radium::KeyboardFocusFrameType type){

  for(auto *frame : g_keyboard_focus_frames[(int)type])
    if (frame->isVisible()){
      FOCUSFRAMES_set_focus(type, true);
      return true;
    }


  return false;
}

void FOCUSFRAMES_set_focus_best_guess(void){
  if (set_if_visible(g_curr_focus_type))
    return;

  if (set_if_visible(g_curr_focus_type))
    return;

  for(int type = 0 ; type < (int)radium::KeyboardFocusFrameType::NUM_TYPES ; type++){
    if (set_if_visible((radium::KeyboardFocusFrameType)type))
      return;
  }
}
  
bool FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType type){
  for(auto *frame : g_keyboard_focus_frames[(int)type])
    if (frame->has_focus())
      return true;

  return false;
}

