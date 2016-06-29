/* Copyright 2012-2013 Kjetil S. Matheussen

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

#include <QScrollArea>
#include <QWebView>
#include <QWebFrame>

#include <Qsci/qscilexerjava.h>
#include <Qsci/qscilexercpp.h>

#include "../audio/SoundPlugin_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"


static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index));

class FaustResultScrollArea : public QScrollArea{
  Q_OBJECT
  
public:
  
 FaustResultScrollArea(QWidget *parent)
   : QScrollArea(parent)
  {
  }

  void wheelEvent(QWheelEvent *qwheelevent) {
    if (qwheelevent->modifiers() & Qt::ShiftModifier)
      horizontalScrollBar()->setValue(horizontalScrollBar()->value() - qwheelevent->delta()/5);
    else
      verticalScrollBar()->setValue(verticalScrollBar()->value() - qwheelevent->delta()/5);
  }
};


#include "Qt_faust_plugin_widget.h"

struct FaustResultWebView : public QWebView{

  int org_height, org_width;
  bool is_dragging;
  bool was_dragging;
  
  FaustResultWebView(QWidget *parent)
    : QWebView(parent)
    , org_height(-10000)
    , is_dragging(false)
  {}

  QPoint start;
  QPoint start_scrollPos;

  void setPointer(QMouseEvent * event) {
    QWebFrame *frame = page()->mainFrame();
      
    bool is_in_scrollbar = frame->scrollBarGeometry(Qt::Vertical).contains(event->pos());

    is_in_scrollbar = is_in_scrollbar || frame->scrollBarGeometry(Qt::Horizontal).contains(event->pos());
    
    if (!is_in_scrollbar)
      setCursor(Qt::OpenHandCursor);
  }
  
  void mouseMoveEvent(QMouseEvent * event) override {
    if (is_dragging){
      QPoint pos = event->pos();

      QPoint delta = start - pos;

      page()->mainFrame()->setScrollPosition(start_scrollPos + delta);

      was_dragging = true;

      event->accept();

    } else {

      QWebView::mouseMoveEvent(event);

      if (cursor().shape() == Qt::ArrowCursor)
        setPointer(event);      
    }
  }
  
  void mousePressEvent(QMouseEvent * event) override {
    QWebFrame *frame = page()->mainFrame();

    printf("mouse: %d,%d. geo: %d,%d -> %d, %d\n", start.x(), start.y(), frame->scrollBarGeometry(Qt::Vertical).x(), frame->scrollBarGeometry(Qt::Vertical).y(), frame->scrollBarGeometry(Qt::Vertical).width(), frame->scrollBarGeometry(Qt::Vertical).height());

    bool is_in_scrollbar = frame->scrollBarGeometry(Qt::Vertical).contains(event->pos());

    is_in_scrollbar = is_in_scrollbar || frame->scrollBarGeometry(Qt::Horizontal).contains(event->pos());
    
    if (!is_in_scrollbar){
      
      start = event->pos();
      start_scrollPos = frame->scrollPosition();
      is_dragging = true;
      was_dragging = false;

      setCursor(Qt::ClosedHandCursor);
              
      event->accept();

    } else {
      is_dragging = false;
      was_dragging = false;
    }
    
    QWebView::mousePressEvent(event);
  }

  void mouseReleaseEvent(QMouseEvent * event) override {
    is_dragging = false;
    
    if (was_dragging) {
      event->accept();
    } else {
      QWebView::mouseReleaseEvent(event);
    }

    setPointer(event);
  }

  void wheelEvent(QWheelEvent *qwheelevent) override {
    if (org_height==-10000){
      org_height = page()->viewportSize().height();
      org_width = page()->viewportSize().width();
    }
    
    if (qwheelevent->modifiers() & Qt::ControlModifier){
      float zoom = zoomFactor();      
      float newzoom;
      if (qwheelevent->delta() > 0)
        newzoom = zoom * 1.2;
      else
        newzoom = zoom * 0.8;

      if (newzoom > 0.85 && newzoom < 1.15)
        newzoom = 1.0;

      if (newzoom > 0.05) {
        /*
        QSize size(org_width, org_height);
        size.scale(org_width*newzoom, org_height*newzoom,  Qt::IgnoreAspectRatio);
        resize(size);
        page()->setViewportSize(size);
        scroll_area->widget()->resize(size.width(), scroll_area->widget()->height());
        */
        page()->mainFrame()->setZoomFactor(newzoom);
      }
    } else {
      Qt::Orientation orientation;
      
      if (qwheelevent->modifiers() & Qt::ShiftModifier)
        orientation = Qt::Horizontal;
      else
        orientation = Qt::Vertical;
          
      page()->mainFrame()->setScrollBarValue(orientation, page()->mainFrame()->scrollBarValue(orientation) - qwheelevent->delta()/5);
      //scroll_area->wheelEvent(qwheelevent);
    }
  }
};

namespace radium{
  
class Editor : public FocusSnifferQsciScintilla{
  public:

  QString last_search;

  Editor(QWidget *parent)  
    : FocusSnifferQsciScintilla(parent)
  {
    minimizeMargins(this);
  }


  void minimizeMargins(QWidget *widget){
#if 0 // Didn't work. I guess qscnintinittlilla doesn't set parent object for all widget children. Changed the source code of qacnintil instead.
    const QList<QObject*> list = widget->children();

    for(auto *element : list){
      widget->children();
      QWidget *widget = dynamic_cast<QWidget*>(element);
      if(widget!=NULL){
        QLayout *layout = widget->layout();
        if (layout!=NULL){
          layout->setSpacing(0);
          layout->setContentsMargins(0,0,0,0);
          for (int i = 0; i < layout->count(); ++i){
            QSpacerItem *item = layout->itemAt(i)->spacerItem();
            if (item!=NULL)
              item->changeSize(0,0, QSizePolicy::Fixed, QSizePolicy::Fixed);
            QWidget *w = layout->itemAt(i)->widget();
            if (w!=NULL)
              minimizeMargins(w);
          }
        }
        
        QFrame *frame = dynamic_cast<QFrame*>(widget);
        if (frame!=NULL){
          frame->setLineWidth(0);
          frame->setMidLineWidth(0);
          frame->setFrameShape(QFrame::NoFrame);
        }
        
        minimizeMargins(widget);
      }
    }
#endif
  }
  
  void search(QString s){
    findFirst(s,
              false,
              false,
              false,
              true
              );
    last_search = s;
  }
  
  void keyPressEvent ( QKeyEvent * event ){
    if (event->key()==Qt::Key_F3 && last_search != "")
      search(last_search);
    
    else if (event->key()==Qt::Key_F3 || (event->key()==Qt::Key_F && (event->modifiers() & Qt::ControlModifier))) {
      printf("Ctrl+F\n");
      char *s = GFX_GetString(root->song->tracker_windows, NULL, "Search for (F3 to repeat): ");
      if (s!=NULL && strlen(s)>0)
        search(s);
      setFocus(Qt::OtherFocusReason);
      
    }else if(event->key()==Qt::Key_Play) {
      playBlockFromStart(-1);

    }else if(event->key()==Qt::Key_Stop) {
      playStop();

    }else if(event->key()==Qt::Key_MediaPlay) {
      if (isPlaying())
        playStop();
      else
        playBlockFromCurrent(-1);

    }else if(event->key()==Qt::Key_MediaStop) {
      playStop();

    }else if(event->key()==Qt::Key_MediaPause) {
      if (isPlaying())
        playStop();
      else
        playBlockFromCurrent(-1);

    }else if(event->key()==Qt::Key_MediaTogglePlayPause) {
      if (isPlaying())
        playStop();
      else
        playBlockFromCurrent(-1);
      
    }else if(event->key()==Qt::Key_VolumeDown) {
      volumeDown();

    }else if(event->key()==Qt::Key_VolumeUp) {
      volumeUp();

    }else if(event->key()==Qt::Key_VolumeMute) {
      mute();

    }else
      FocusSnifferQsciScintilla::keyPressEvent(event);
  }
};
}

static radium::Editor *create_editor(QWidget *parent){
  QPalette app_pal = QApplication::palette();
  QColor org_base = app_pal.base().color();
  QColor org_text = app_pal.text().color();
  app_pal.setColor(QPalette::Base, Qt::white);
  app_pal.setColor(QPalette::Text, Qt::black);    
  QApplication::setPalette(app_pal);
    

  auto *faust_code = new radium::Editor(parent);
  faust_code->setMarginLineNumbers(1, true);
  faust_code->setMarginType(1, QsciScintilla::NumberMargin);
  
  faust_code->setLexer(new QsciLexerCPP(parent));
  
  app_pal.setColor(QPalette::Base, org_base);
  app_pal.setColor(QPalette::Text, org_text);
  QApplication::setPalette(app_pal);
  
  // set margin width
  faust_code->calculateMarginWidth();

  return faust_code;
}

class Faust_Plugin_widget : public QWidget, public Ui::Faust_Plugin_widget{
  Q_OBJECT;

public:
  QWidget *parent;
  
  PluginWidget *_plugin_widget;
  QLabel *_faust_compilation_status;
  struct Patch *_patch;
  FaustResultWebView *web;
  QWebFrame *_last_web_frame;
  QString _web_text;
  float _svg_zoom_factor;
  float _error_zoom_factor;
  
  //QLabel *_error_message;
  radium::Editor *_faust_editor;
  
  bool _initing;

  SizeType _size_type;
  SizeType _size_type_before_hidden;
  int _header_height;
  
  int _last_height;
  int _update_count_down;
  
  int _prev_cursor_line, _cursor_line;
  int _prev_cursor_index, _cursor_index;

  QString _latest_working_code;

  QDialog *_cpp_dialog;
  radium::Editor *_cpp_editor;
  
  QDialog *_options_dialog;
  radium::Editor *_options_editor;

  int _initial_height;
  int _initial_parent_height;
  int _initial_width;
  
  Faust_Plugin_widget(QWidget *parent, QLabel *faust_compilation_status, struct Patch *patch)
    : QWidget(parent)
    , parent(parent)
    , _faust_compilation_status(faust_compilation_status)
    , _patch(patch)
    , _last_web_frame(NULL)
    , _svg_zoom_factor(1.0)
    , _error_zoom_factor(1.0)
    , _size_type(SIZETYPE_NORMAL)
    , _size_type_before_hidden(SIZETYPE_NORMAL)
    , _header_height(40)
    , _last_height(10)
    , _update_count_down(0)
    , _prev_cursor_line(0) , _cursor_line(0)
    , _prev_cursor_index(0) , _cursor_index(0)
    , _cpp_dialog(NULL)
    , _cpp_editor(NULL)
    , _options_dialog(NULL)
    , _options_editor(NULL)
  {
    _initing = true;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (FAUST_is_compiling(plugin))
      _faust_compilation_status->setText("Initializing... ");
    else
      _faust_compilation_status->setText("Ready ");

    setupUi(this);

    _initial_height = height();
    _initial_width = width();
    
    printf("Initial height: %d\n", height());
    
    _faust_editor = create_editor(this);
    _faust_editor->resize(50,50);
    
    //_faust_editor->setFolding(QsciScintilla::FoldStyle::PlainFoldStyle);
    //_faust_editor->resize(width()*2/3,height());
    
    develop_layout->insertWidget(0, _faust_editor);

    connect(_faust_editor, SIGNAL(textChanged()), this, SLOT(a_on__faust_editor_textChanged()));
    connect(_faust_editor, SIGNAL(linesChanged()), this, SLOT(a_on__faust_editor_linesChanged()));
    connect(_faust_editor, SIGNAL(cursorPositionChanged(int,int)), this, SLOT(a_on__faust_editor_cursorPositionChanged(int,int)));

    web = new FaustResultWebView(this);
    web->resize(50,50);
        
    
    QSizePolicy fixedPolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    QSizePolicy expandingPolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    //web->setSizePolicy(expandingPolicy);
    faust_webview_widget->setSizePolicy(fixedPolicy);
    faust_webview_widget->resize(50,50);

    _faust_editor->setSizePolicy(fixedPolicy);

    _faust_editor->setMaximumHeight(_initial_height);
    _faust_editor->setMaximumWidth(_initial_width);
    faust_webview_widget->setMaximumHeight(_initial_height);
    faust_webview_widget->setMaximumWidth(_initial_width);


    //web->setHtml("<object id=\"svg1\" data=\"file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg\" type=\"image/svg+xml\"></object>");
    //web->setUrl(QUrl("file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg"));
    web->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(FAUST_get_svg_path(plugin))));
    printf("    URL: -%s-. native: -%s-, org: -%s-\n",web->url().toString().toUtf8().constData(), QDir::fromNativeSeparators(FAUST_get_svg_path(plugin)).toUtf8().constData(), FAUST_get_svg_path(plugin).toUtf8().constData());

    _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
    _last_web_frame->setZoomFactor(0.5);
    _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOn); // The faust editor always has a scroll bar, so it looks strange without it here as well.

    //_faust_editor->setText(FAUST_get_code(plugin));
    //_faust_editor->zoomIn(10);

    /*
    web->resize(100,100);
    web->setMaximumHeight(parent->height() - 2);
    web->setMinimumWidth(parent->width() / 2);
    web->setMaximumWidth(parent->width() / 2);
    */
    
    //faust_webview_widget->setUpdatesEnabled(false);
    //_update_count_down = 3; // Hack to avoid some flicker in qt

    faust_webview_layout->addWidget(web);

    _plugin_widget = PluginWidget_create(this, patch);
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    update_gui(); // <--- Note, update_gui sets _initing to false.

    updateGeometry();
    adjustSize();

    _initial_parent_height = parent->height();
    
    calculate_small_web_heights();
    
    calculate_widths();
    _initing = false;
  }

  ~Faust_Plugin_widget() {
    /*
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL)
      FAUST_inform_about_instrument_gui(plugin, NULL);
    */
  }

  void set_text_in__faust_editor_widget(QString new_code){
    //QTextCursor cr = _faust_editor->textCursor();
    _faust_editor->setText(new_code);
    //_faust_editor->setTextCursor(cr);
  }
  
  void update_gui(){
    for(ParamWidget *param_widget : _plugin_widget->_param_widgets)
      param_widget->update_gui_element();

    #if 1
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      _initing = true;{
        
        QString new_code = FAUST_get_code(plugin);
        if (new_code != _faust_editor->text())
          set_text_in__faust_editor_widget(new_code);
      
      }_initing = false;
    }
    #endif
  }

  bool showing_svg(void){
    return _web_text=="";
  }
  
  // todo: Move web by dragging.
  
  void calledRegularlyByParent(void){
    
    if (Undo_num_undos()==0) // I don't think this can happen, but in case it does, we return since the call to Undo_ReopenLast() below would fail (badly).
      return;

    if (radium_runs_custom_exec==true)
      return;

    //R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open()==false);
    if (Undo_Is_Open()==true) // <-- This seems to happen when we have just started to move a chip.
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {

      FAUST_calledRegularlyByParentReply status;
      
      Undo_ReopenLast();{ // Simply add any undos create here into the last undo point. Adding a new undo point here would be confusing for the user since this function is triggered by a timer and not a user interaction. (and we need to add undo to avoid inconsitencies, i.e. we can't just call Undo_StartIgnoringUndo()/Undo_StopIgnoringUndo().)
        status = FAUST_calledRegularlyByParent(plugin);
      }Undo_Close();
      
      if (status==Faust_No_New_Reply){

        //
        
      } else if (status==Faust_Failed){

        // TODO: Just print the error message into the web view. No need for a separate error label.
        
        _faust_compilation_status->setText("Failed ");
        //error_message->setText(FAUST_get_error_message(plugin));

        if (showing_svg())
          _svg_zoom_factor = web->zoomFactor();
        else
          _error_zoom_factor = web->zoomFactor();
        
        //_last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOff);
        _web_text = 
                     "<!DOCTYPE html>"
                     "<html>"
                     "<body style=\"background-color:white;\"><big>"
                     +FAUST_get_error_message(plugin)+
                     "</big></body>"
                     "</html>"
          ;
        
        web->setHtml(_web_text);
        web->setZoomFactor(_error_zoom_factor);
        
        _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
        
      } else if (status==Faust_Success){
        _faust_compilation_status->setText("Ready ");

        _latest_working_code = FAUST_get_code(plugin);

        if (showing_svg())
          _svg_zoom_factor = web->zoomFactor();
        else
          _error_zoom_factor = web->zoomFactor();

        _web_text = "";
        
        web->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(FAUST_get_svg_path(plugin))));
        web->setZoomFactor(_svg_zoom_factor);
                
        printf("    URL: -%s-. native: -%s-, org: -%s-\n",web->url().toString().toUtf8().constData(), QDir::fromNativeSeparators(FAUST_get_svg_path(plugin)).toUtf8().constData(), FAUST_get_svg_path(plugin).toUtf8().constData());
        _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
        
        PluginWidget *old = _plugin_widget;
        _plugin_widget=PluginWidget_create(this, _patch);

        if (_size_type != SIZETYPE_NORMAL){
          faust_webview_widget->setUpdatesEnabled(false);
          _update_count_down = 3; // Hack to avoid flicker in qt (not working perfectly though, this is a design issue in qt where the widget configuration updates are spread over several events)
          
          faust_webview_layout->removeWidget(old);
          faust_webview_layout->addWidget(_plugin_widget);
          
          if (old==NULL || _plugin_widget->_num_rows != old->_num_rows)
            calculate_large_web_heights();
          
        }else {
          
          faust_interface_layout_radium->insertWidget(0, _plugin_widget);
          
        }

        if (_cpp_editor != NULL && _cpp_editor->isVisible())
          _cpp_editor->setText(FAUST_get_cpp_code(plugin));

        _plugin_widget->set_automation_value_pointers(plugin);

        delete old;
      }
    }

    // Hack to avoid flicker in qt
    if (_update_count_down > 0){
      _update_count_down--;
      if (_update_count_down==0){
        
        faust_webview_widget->setUpdatesEnabled(true);
        struct Tracker_Windows *window = root->song->tracker_windows;
        window->must_redraw = true;
      }
    }
  }

  void set_max_heights(int height, bool set_web_height = true, bool set_min = true){
    _faust_editor->setMinimumHeight(set_min ? height : 0);
    _faust_editor->setMaximumHeight(height);

    faust_webview_widget->setMinimumHeight(set_min ? height : 0);
    faust_webview_widget->setMaximumHeight(height);

    if (set_web_height){
      web->setMinimumHeight(set_min ? height : 0);
      web->setMaximumHeight(height);
    }
  }
  
  void calculate_small_web_heights(){
    int the_height = _initial_parent_height;//parent->height() - 10;

    printf("    TEH ehgith: %d. initial: %d\n", the_height, _initial_height);
    
    set_max_heights(the_height);
  }
  
  void calculate_large_web_heights(void){
    int height = g_main_window->height() - 50;

    int full_height;

    if (_size_type==SIZETYPE_HALF)
      full_height = height/2 - _header_height - 8;
    else
      full_height = height - _header_height - 8;
    
    int plugin_widget_height =
      _plugin_widget->_num_rows==0
      ? 0
      : (1+_plugin_widget->_num_rows) * (root->song->tracker_windows->systemfontheight) + 20;
    
    if (plugin_widget_height > full_height/2)
      plugin_widget_height = full_height/2;

    int max_web_height = full_height - (plugin_widget_height==0 ? 2 : (plugin_widget_height+20));
    printf("max_web: %d, full: %d, plugin: %d, num_rows: %d\n",max_web_height,full_height,plugin_widget_height,_plugin_widget->_num_rows);
    
    web->setMaximumWidth(g_main_window->width()); // To ensure we don't see a small web browser in the left part of the widget with a lot of empty space to the right of it.

    set_max_heights(full_height, false, _size_type==SIZETYPE_HALF);

    if (_size_type==SIZETYPE_FULL)
      web->setMinimumHeight(max_web_height);
    else
      web->setMinimumHeight(max_web_height);
    
    web->setMaximumHeight(max_web_height);
  }
  
  void set_large(SizeType new_size_type, int header_height = 0){
    _size_type = new_size_type;

    if (header_height > 0)
      _header_height = header_height;
    
    // Change vertical scroll bar policy (not easy...)
    {
      _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOff);
      
      if (showing_svg())
        web->reload();
      else
        web->setHtml(_web_text);
      
      _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
      _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAsNeeded);
    }
    
    faust_webview_widget->setUpdatesEnabled(false);
    _update_count_down = 3; // Hack to avoid some flicker in qt

    main_layout->addWidget(code_widget);
    
    tab_widget->hide();
    faust_webview_layout->addWidget(_plugin_widget);

    calculate_large_web_heights();
    calculate_widths();
  }

  void set_small(void){
    _size_type = SIZETYPE_NORMAL;
    //_is_large = false;

    _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOn);
    
    set_max_heights(_initial_height);

    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    tab_develop_layout->addWidget(code_widget);

    tab_widget->show();

    updateGeometry();
    adjustSize();

    calculate_small_web_heights();
    calculate_widths();
  }

  void change_height(SizeType type, int header_height = 0){
    QSizePolicy fixedPolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    QSizePolicy expandingPolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    if (type==SIZETYPE_FULL){
      faust_webview_widget->setSizePolicy(expandingPolicy);
      _faust_editor->setSizePolicy(expandingPolicy);
    } else {
      faust_webview_widget->setSizePolicy(fixedPolicy);
      _faust_editor->setSizePolicy(fixedPolicy);
    }
    
    if (type==SIZETYPE_NORMAL)
      set_small();
    else
      set_large(type, header_height);
  }
  
  void start_compilation(QString code){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL){
      FAUST_set_code(plugin, code);
      if (_options_editor != NULL)
        FAUST_set_options(plugin, _options_editor->text());
      FAUST_start_compilation(plugin);
      _faust_compilation_status->setText("Compiling... ");
    }
  }

  void revert_to_latest_working_version(void){
    set_text_in__faust_editor_widget(_latest_working_code);
  }

  void load_source(QString filename){
    disk_t *disk = DISK_open_for_reading(filename);

    if (disk==NULL){
      GFX_Message(NULL, "File not found (%s)", filename.toUtf8().constData());
      return;
    }

    QString new_code = DISK_read_qstring_file(disk);
      
    if (DISK_close_and_delete(disk)==false) {
      GFX_Message(NULL, "Unable to read from %s", filename.toUtf8().constData());
      return;
    }
    
    set_text_in__faust_editor_widget(new_code);
  }
  
  void save_source(QString filename){
    disk_t *disk = DISK_open_for_writing(filename);

    //GFX_Message(NULL, "   fff filename: -%s-, %p",filename.toUtf8().constData(), disk);
    if (disk==NULL){
      GFX_Message(NULL, "Unable to open %s for writing", filename.toUtf8().constData());
      return;
    }

    QString code = _faust_editor->text();
    int n = DISK_write_qstring(disk, code);
    
    bool show_warning = false;
    if (n != code.size())
      show_warning = true;

    if (DISK_close_and_delete(disk)==false)
      GFX_Message(NULL, "Unable to write to %s", filename.toUtf8().constData());
    else if (show_warning)
      GFX_Message(NULL, "Warning: Wrote %d bytes. Expected %d", n, code.size());
  }

  void show_cpp_source(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_cpp_dialog==NULL) {
        _cpp_dialog = new RememberGeometryQDialog(this);
        QHBoxLayout *mainLayout = new QHBoxLayout;
      
        _cpp_dialog->setLayout(mainLayout);
        
        _cpp_editor = create_editor(_cpp_dialog);

        mainLayout->addWidget(_cpp_editor);
        
        _cpp_dialog->resize(600,400);
      }
      
      _cpp_editor->setText(FAUST_get_cpp_code(plugin));
      
      _cpp_dialog->show();
      _cpp_dialog->raise();
    }
  }

  void edit_options(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_options_dialog==NULL){
        _options_dialog = new RememberGeometryQDialog(this);
        QHBoxLayout *mainLayout = new QHBoxLayout;
      
        _options_dialog->setLayout(mainLayout);
        
        _options_editor = create_editor(_options_dialog);

        mainLayout->addWidget(_options_editor);
        
        _options_dialog->resize(600,400);
      }
      
      _options_editor->setText(FAUST_get_options(plugin));
      
      _options_dialog->show();
      _options_dialog->raise();
    }
  }

  void hideEvent(QHideEvent * event){
    _size_type_before_hidden = _size_type;
    
    if(_size_type!=SIZETYPE_NORMAL)
      set_small(); // If not, all instrument widgets will have large height (due to the call to web->setMaximumHeight(window_height/2) in set_large()).
  }

  void showEvent(QShowEvent * event){
    if (_size_type_before_hidden != SIZETYPE_NORMAL)
      set_large(_size_type_before_hidden);
  }

  void calculate_widths(void){
    int the_width;

    if (_size_type != SIZETYPE_NORMAL){
      the_width = width() - 1;
    } else {
      the_width = tab_code_widget->width();
    }

    int the_middle = the_width/2;
    int editor_width = (the_width / 2);
    int border = 5;
    int svg_width = (the_width / 2) - border - 10; // Subtract a little bit so that it won't trigger a new resizeEvent.
      
    //printf("          Calculating widths. height: %d\n",the_height);
    
    // code
    _faust_editor->move(0,0);
    if (editor_width > 0){
      _faust_editor->setMinimumWidth(editor_width);
      _faust_editor->setMaximumWidth(editor_width);
    }
    
    // svg / interface
    faust_webview_widget->move(the_middle+border,0);

    if (svg_width > 0){
      faust_webview_widget->setMinimumWidth(svg_width);
      faust_webview_widget->setMaximumWidth(svg_width);
    }
    
    // Ensure that web uses as much space as possible.
    if (svg_width > 0)
      web->setMaximumWidth(svg_width);
  }
  
  void resizeEvent( QResizeEvent *qresizeevent) override{
    printf("Resizeevent called\n");

    setUpdatesEnabled(false);
    calculate_widths();
    setUpdatesEnabled(true);
  }

public slots:

  #if 0
  void on_splitter_splitterMoved(int pos, int index){
    int webWidth = splitter->width() - _faust_editor->width() -  10;

    printf("Splitter moved to pos %d. Full width: %d, faust_width: %d, web: %d\n", pos, splitter->width(),_faust_editor->width(), webWidth);

    if (webWidth > 10){
      web->resize(webWidth,web->height());//setMinimumWidth(webWidth);
      web->setMaximumWidth(webWidth+10);
    }
  }
  #endif
  
  void a_on__faust_editor_cursorPositionChanged(int new_line, int new_index){
    _prev_cursor_line = _cursor_line;
    _prev_cursor_index = _cursor_index;
    
    _cursor_line = new_line;
    _cursor_index = new_index;
    
    //_faust_editor->getCursorPosition(&_cursor_line, &_cursor_index);
    //printf("Cursor pos changed to %d\n", _cursor_line);
  }

  void a_on__faust_editor_linesChanged(){
    _faust_editor->calculateMarginWidth();
  }
  
  void a_on__faust_editor_textChanged(){
    //printf("Text changed. pos: %d\n",0);//_faust_editor->textCursor().position());
    if (!_initing){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (plugin!=NULL) {
        QString new_code = _faust_editor->text();
        if (new_code != ""){ // <-- QScintilla sometimes gives us empty string in _faust_editor->text() (when there shouldn't be).
          QString old_code = FAUST_get_code(plugin);
          start_compilation(new_code);
          ADD_UNDO(FaustDev_CurrPos(_patch, old_code, _prev_cursor_line, _prev_cursor_index)); // note: _prev_cursor_pos is not correct when pasting something.
        }
      }
    }
  }
  
};

/*
// Doesn't work so well. The gui is deleted at inconvenient times.
QString FAUSTGUI_get_code(QWidget *widget){
  Faust_Plugin_widget *f = dynamic_cast<Faust_Plugin_widget*>(widget);
  R_ASSERT_RETURN_IF_FALSE2(f!=NULL, "");
  return f->_faust_editor->text();
}
*/


namespace{
  struct Undo_FaustDev{
    struct Patch *patch;
    wchar_t *code; // this is clumsy. Should investigate the time finding out if there are the quirks using bdw-gc with c++.
    int cursor_line;
    int cursor_index;
  };
}

static void *Undo_Do_FaustDev(
                              struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack,
                              int realline,
                              void *pointer
                              );

static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  
  static double last_undo_block_time = -1000;
  double time_now = TIME_get_ms();

  static Patch *last_patch = NULL;
  static WBlocks *last_wblock = NULL;
  
  if( (time_now-last_undo_block_time) > 1500 // more than 1.5 seconds.
      || wblock!=last_wblock
      || last_patch != patch
      || Undo_get_last_function()!=Undo_Do_FaustDev
      )
    {
      Undo_FaustDev *undo_fd = (Undo_FaustDev*)talloc(sizeof(Undo_FaustDev));
      undo_fd->patch = patch;
      undo_fd->code = STRING_create(code);
      undo_fd->cursor_line = cursor_line;
      undo_fd->cursor_index = cursor_index;

      Undo_Add_dont_stop_playing(
                                 window->l.num,
                                 wblock->l.num,
                                 wblock->wtrack->l.num,
                                 wblock->curr_realline,
                                 undo_fd,
                                 Undo_Do_FaustDev,
                                 "FaustDevCodeChange"
                                 );
      last_patch = patch;
      last_wblock = wblock;
      last_undo_block_time = time_now;
    }
}

static void *Undo_Do_FaustDev(
                            struct Tracker_Windows *window,
                            struct WBlocks *wblock,
                            struct WTracks *wtrack,
                            int realline,
                            void *pointer
                            )
{
  struct Undo_FaustDev *undo_fd=(Undo_FaustDev*)pointer;
  struct Patch *patch = undo_fd->patch;
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  Audio_instrument_widget *audio_instrument_widget = get_audio_instrument_widget(patch);
  R_ASSERT_RETURN_IF_FALSE2(audio_instrument_widget!=NULL, undo_fd);
    
  Faust_Plugin_widget *faust_plugin_widget = AUDIOWIDGET_get_faust_plugin_widget(audio_instrument_widget);
  R_ASSERT_RETURN_IF_FALSE2(faust_plugin_widget!=NULL, undo_fd);

  wchar_t *new_code = STRING_create(FAUST_get_code(plugin));

  int new_cursor_line, new_cursor_index;
  faust_plugin_widget->_faust_editor->getCursorPosition(&new_cursor_line, &new_cursor_index);
  
  
  faust_plugin_widget->_initing = true;{

    QString undo_code = STRING_get_qstring(undo_fd->code);
    
    faust_plugin_widget->set_text_in__faust_editor_widget(undo_code);

    //  Chaos trying to set cursor in qscisintella. :-(
    //faust_plugin_widget->_faust_editor->setCursorPosition(undo_fd->cursor_line, undo_fd->cursor_index);
    
    faust_plugin_widget->start_compilation(undo_code);
    
  }faust_plugin_widget->_initing = false;

  
  undo_fd->code = new_code;
  undo_fd->cursor_line = new_cursor_line;
  undo_fd->cursor_index = new_cursor_index;

  return undo_fd;
}

//// UNDO END
