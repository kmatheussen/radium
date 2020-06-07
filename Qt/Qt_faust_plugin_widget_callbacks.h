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

#if USE_QT5
  #define USE_QWEBENGINE 0
#else
  #define USE_QWEBENGINE 0 // Can not be 1.
#endif


#if USE_QWEBENGINE

  #include <QWebEngineView>
  #include <QWebEnginePage>

#else

  #if USE_QT5
    #include <QtWebKitWidgets/QWebView>
    #include <QtWebKitWidgets/QWebFrame>
  #else
    #include <QWebView>
    #include <QWebFrame>
  #endif

#endif

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/QScintilla_gpl-2.10.8/Qt4Qt5/Qsci/qscilexerjava.h"
#include "../bin/packages/QScintilla_gpl-2.10.8/Qt4Qt5/Qsci/qscilexercpp.h"
#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#include "../audio/SoundPlugin_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"
#include "Qt_MyQScrollBar.hpp"

static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index));

namespace{
  
class FaustResultScrollArea : public QScrollArea{
  Q_OBJECT
  
public:
  
 FaustResultScrollArea(QWidget *parent)
   : QScrollArea(parent)
  {
    setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));
  }

  void wheelEvent(QWheelEvent *qwheelevent) override {
    if (HorizontalModifierPressed(qwheelevent->modifiers()))
      horizontalScrollBar()->setValue(horizontalScrollBar()->value() + qwheelevent->delta()/5);
    else
      verticalScrollBar()->setValue(verticalScrollBar()->value() - qwheelevent->delta()/5);
  }
};
}


#include "Qt_faust_plugin_widget.h"

namespace{
  
struct FaustResultWebView
#if USE_QWEBENGINE
  : public QWebEngineView
#else
  : public QWebView
#endif
  , public radium::MouseCycleFix
{

  bool is_dragging;
  bool was_dragging;
  
  FaustResultWebView(QWidget *parent)
#if USE_QWEBENGINE
    : QWebEngineView(parent)
#else
    : QWebView(parent)
#endif
    , is_dragging(false)
  {
  }

  QPoint start;
  QPoint start_scrollPos;

  void setPointer(QPoint pos){
#if !USE_QWEBENGINE
    QWebFrame *frame = page()->mainFrame();
    
    bool is_in_scrollbar = frame->scrollBarGeometry(Qt::Vertical).contains(pos);

    is_in_scrollbar = is_in_scrollbar || frame->scrollBarGeometry(Qt::Horizontal).contains(pos);
    
    if (!is_in_scrollbar)
#endif
      setCursor(Qt::OpenHandCursor);
  }
  
  void fix_mouseMoveEvent(QMouseEvent * event) override {
    if (is_dragging){
#if !USE_QWEBENGINE
      QPoint pos = event->pos();

      QPoint delta = start - pos;
      page()->mainFrame()->setScrollPosition(start_scrollPos + delta);
#endif
      was_dragging = true;

      event->accept();

    } else {

#if USE_QWEBENGINE
      QWebEngineView::mouseMoveEvent(event);
#else
      QWebView::mouseMoveEvent(event);
#endif
      
      if (cursor().shape() == Qt::ArrowCursor)
        setPointer(event->pos());
    }
  }
  
  void fix_mousePressEvent(QMouseEvent * event) override {
#if !USE_QWEBENGINE
    QWebFrame *frame = page()->mainFrame();

    printf("mouse: %d,%d. geo: %d,%d -> %d, %d\n",
           start.x(), start.y(),
           frame->scrollBarGeometry(Qt::Vertical).x(), frame->scrollBarGeometry(Qt::Vertical).y(),
           frame->scrollBarGeometry(Qt::Vertical).width(), frame->scrollBarGeometry(Qt::Vertical).height()
           );

    bool is_in_scrollbar = frame->scrollBarGeometry(Qt::Vertical).contains(event->pos());
    
    is_in_scrollbar = is_in_scrollbar || frame->scrollBarGeometry(Qt::Horizontal).contains(event->pos());
#else
    bool is_in_scrollbar = false;
#endif
    
    if (!is_in_scrollbar){
      
      start = event->pos();
#if !USE_QWEBENGINE
      start_scrollPos = frame->scrollPosition();
#endif
      is_dragging = true;
      was_dragging = false;

      setCursor(Qt::ClosedHandCursor);
              
      event->accept();

    } else {
      is_dragging = false;
      was_dragging = false;
    }

#if USE_QWEBENGINE
    QWebEngineView::mousePressEvent(event);
#else
    QWebView::mousePressEvent(event);
#endif
  }

  void fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override {
    is_dragging = false;
    
    if (was_dragging) {
      event.accept();
    } else {
#if USE_QWEBENGINE
      if(event.is_real_event())
        QWebEngineView::mouseReleaseEvent(event.get_qtevent());
#else
      if(event.is_real_event())
        QWebView::mouseReleaseEvent(event.get_qtevent());
#endif
    }

    setPointer(event.pos());
  }

  MOUSE_CYCLE_CALLBACKS_FOR_QT;
    
  // Seems like QWebView tries to find a smart sizeHint by default. We don't want that.
  QSize sizeHint() const override {
    return QSize(-1,-1);
  }

  void wheelEvent(QWheelEvent *qwheelevent) override {
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
#if USE_QWEBENGINE
        page()->setZoomFactor(newzoom);
#else
        page()->mainFrame()->setZoomFactor(newzoom);
#endif
      }
    } else {
#if !USE_QWEBENGINE
      Qt::Orientation orientation;

      double direction;
      
      if (HorizontalModifierPressed(qwheelevent->modifiers())){
        orientation = Qt::Horizontal;
        direction = 1.0;
      } else {
        orientation = Qt::Vertical;
        direction = -1.0;
      }
    
      page()->mainFrame()->setScrollBarValue(orientation, page()->mainFrame()->scrollBarValue(orientation) + (direction*qwheelevent->delta()/5));
      //scroll_area->wheelEvent(qwheelevent);
#endif
    }
  }
};
}


namespace radium{

  
class Editor : public FocusSnifferQsciScintilla{
  public:

  QString last_search;

  Editor(QWidget *parent)  
    : FocusSnifferQsciScintilla(parent)
  {
    minimizeMargins(this);
    /*
    static QStyle *style = QStyleFactory::create("fusion"); // parent is platique
    if (style!=NULL)
      setStyle(style);
    */
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
  
  void keyPressEvent ( QKeyEvent * event ) override {
    if (event->key()==Qt::Key_F3 && last_search != "")
      search(last_search);
    
    else if (event->key()==Qt::Key_F3 || (event->key()==Qt::Key_F && (event->modifiers() & Qt::ControlModifier))) {
      printf("Ctrl+F\n");
      const char *s = GFX_GetString(root->song->tracker_windows, NULL, "Search for (F3 to repeat): ", true);
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

namespace{
  
class Faust_Plugin_widget : public QWidget, public Ui::Faust_Plugin_widget{
  Q_OBJECT;

public:
  QWidget *parent;
  
  PluginWidget *_plugin_widget;
  QLabel *_faust_compilation_status;
  radium::GcHolder<struct Patch> _patch;
  FaustResultWebView *web;
#if !USE_QWEBENGINE
  QWebFrame *_last_web_frame;
#endif
  QString _web_text;
  float _svg_zoom_factor;
  float _error_zoom_factor;
  
  //QLabel *_error_message;
  radium::Editor *_faust_editor;
  
  bool _initing;

  SizeType _size_type;
  SizeType _size_type_before_hidden;
  
  int _prev_cursor_line, _cursor_line;
  int _prev_cursor_index, _cursor_index;

  QString _latest_working_code;

  QDialog *_cpp_dialog;
  radium::Editor *_cpp_editor;
  
  QDialog *_options_dialog;
  radium::Editor *_options_editor;

  Faust_Plugin_widget(QWidget *parent, QLabel *faust_compilation_status, struct Patch *patch)
    : QWidget(parent)
    , parent(parent)
    , _faust_compilation_status(faust_compilation_status)
    , _patch(patch)
#if !USE_QWEBENGINE
    , _last_web_frame(NULL)
#endif
    , _svg_zoom_factor(1.0)
    , _error_zoom_factor(1.0)
    , _size_type(SIZETYPE_NORMAL)
    , _size_type_before_hidden(SIZETYPE_NORMAL)
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
      _faust_compilation_status->setText("&#8987;");
    //_faust_compilation_status->setText("Initializing... ");
    else
      _faust_compilation_status->setText("<font color=\"green\">&#10004;</font>");
    //_faust_compilation_status->setText("Ready ");

    setupUi(this);

    if(0){
      static QStyle *style = QStyleFactory::create("plastique");
      if (style!=NULL)
        setStyle(style);
    }
    
    _faust_editor = create_editor(this);
    
    develop_layout->insertWidget(0, _faust_editor);

    connect(_faust_editor, SIGNAL(textChanged()), this, SLOT(a_on__faust_editor_textChanged()));
    connect(_faust_editor, SIGNAL(linesChanged()), this, SLOT(a_on__faust_editor_linesChanged()));
    connect(_faust_editor, SIGNAL(cursorPositionChanged(int,int)), this, SLOT(a_on__faust_editor_cursorPositionChanged(int,int)));

    web = new FaustResultWebView(this);
    
    //web->setHtml("<object id=\"svg1\" data=\"file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg\" type=\"image/svg+xml\"></object>");
    //web->setUrl(QUrl("file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg"));
    web->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(FAUST_get_svg_path(plugin))));
    printf("    URL: -%s-. native: -%s-, org: -%s-\n",web->url().toString().toUtf8().constData(), QDir::fromNativeSeparators(FAUST_get_svg_path(plugin)).toUtf8().constData(), FAUST_get_svg_path(plugin).toUtf8().constData());

#if !USE_QWEBENGINE
    _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
    _last_web_frame->setZoomFactor(0.5);
    _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOn); // The faust editor always has a scroll bar, so it looks strange without it here as well.
#else
    web->page()->setZoomFactor(0.5);
#endif
    
    faust_webview_layout->addWidget(web, 4);

    _plugin_widget = PluginWidget_create(this, patch, SIZETYPE_NORMAL);
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    update_gui(); // <--- Note, update_gui sets _initing to false.

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

  // These two are here so that an older version is not displayed after a newer version.
  int _cpp_generation = 0;
  int _last_displayed_cpp_generation = -1;

  void update_cpp_editor(struct SoundPlugin *plugin){
    if (_cpp_editor != NULL && _cpp_editor->isVisible()) {

      _cpp_editor->setText("// Please wait, generating C++ code");


      IsAlive is_alive(this);

      FAUST_generate_cpp_code(plugin, _cpp_generation++, [is_alive, this](int generation, QString cpp_code){

          R_ASSERT(THREADING_is_main_thread());

          if (!is_alive)
            return;
          
          if (generation < _last_displayed_cpp_generation)
            return;

          _last_displayed_cpp_generation = generation;

          if (_cpp_editor != NULL)
            _cpp_editor->setText(cpp_code);
        });
    }
  }
  
  void calledRegularlyByParent(void){

    RETURN_IF_DATA_IS_INACCESSIBLE();

    /*    
    if (Undo_num_undos()==0) // I don't think this can happen, but in case it does, we return since the call to Undo_ReopenLast() below would fail (badly).
      return;

    //R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open()==false);
    if (Undo_Is_Open()==true) // <-- This seems to happen when we have just started to move a chip.
      return;
    */

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {

      const radium::FAUST_calledRegularlyByParentReply ready = FAUST_calledRegularlyByParent(plugin);      

      if (ready.has_new_data==false){
        R_ASSERT(ready.factory_is_ready==false);
        R_ASSERT(ready.svg_is_ready==false);
        return;
      }

      printf("========== %d: %d - %d: %d ===========\n", ready.factory_is_ready, ready.factory_succeeded, ready.svg_is_ready, ready.svg_succeeded);

      if (ready.factory_is_ready) {

        if (ready.factory_succeeded) {

          _latest_working_code = FAUST_get_code(plugin);

          _faust_compilation_status->setText("<font color=\"green\">&#10004;</font>");
          
          PluginWidget *old = _plugin_widget;
          _plugin_widget = PluginWidget_create(this, _patch.data(), SIZETYPE_NORMAL);
          
          if (_size_type != SIZETYPE_NORMAL){
            
            faust_webview_layout->removeWidget(old);
            faust_webview_layout->addWidget(_plugin_widget, 1);
            
          }else {
            
            faust_interface_layout_radium->insertWidget(0, _plugin_widget);
            
          }
          
          //_plugin_widget->set_automation_value_pointers(plugin);
          
          delete old;
          

        } else {

          _faust_compilation_status->setText("<font color=\"red\">&#10007;</font>");
          //_faust_compilation_status->setText("Failed ");

        }

      }


      if (ready.svg_is_ready && ready.svg_succeeded) {

        if (showing_svg())
          _svg_zoom_factor = web->zoomFactor();
        else
          _error_zoom_factor = web->zoomFactor();
        
        _web_text = "";
        
        web->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(FAUST_get_svg_path(plugin))));
        
        web->setZoomFactor(_svg_zoom_factor);
        
        printf("    URL: -%s-. native: -%s-, org: -%s-\n",web->url().toString().toUtf8().constData(), QDir::fromNativeSeparators(FAUST_get_svg_path(plugin)).toUtf8().constData(), FAUST_get_svg_path(plugin).toUtf8().constData());
#if !USE_QWEBENGINE
        _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
#endif
        
        update_cpp_editor(plugin);
      }


      bool factory_failed = ready.factory_is_ready && ready.factory_succeeded==false;
      bool svg_failed = ready.svg_is_ready && ready.svg_succeeded==false;


      if (factory_failed || svg_failed){

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
#if !USE_QWEBENGINE
        _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
#endif
        
      }
    }
  }

  void set_large(SizeType new_size_type){
    _size_type = new_size_type;

    // Change vertical scroll bar policy (not easy...)
    {
#if !USE_QWEBENGINE
      _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOff);
#endif
      if (showing_svg())
        web->reload();
      else
        web->setHtml(_web_text);
#if !USE_QWEBENGINE
      _last_web_frame = web->page()->mainFrame(); // Important that we do this after calling setUrl/setHtml
      _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAsNeeded);
#endif
    }
    
    main_layout->addWidget(code_widget);
    
    tab_widget->hide();
    faust_webview_layout->addWidget(_plugin_widget, 1);
  }

  void set_small(void){
    _size_type = SIZETYPE_NORMAL;
    //_is_large = false;
#if !USE_QWEBENGINE
    _last_web_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOn);
#endif
    
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    tab_develop_layout->addWidget(code_widget);

    tab_widget->show();
  }
  
  void change_height(SizeType type){
    if (type==SIZETYPE_NORMAL)
      set_small();
    else
      set_large(type);
  }
  
  void start_compilation(QString code){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL){
      FAUST_set_code(plugin, code);
      if (_options_editor != NULL)
        FAUST_set_options(plugin, _options_editor->text());
      _faust_compilation_status->setText("&#8987;");
      FAUST_start_compilation(plugin);
      //_faust_compilation_status->setText("Compiling... ");
    }
  }

  void revert_to_latest_working_version(void){
    set_text_in__faust_editor_widget(_latest_working_code);
  }

  void load_source(QString filename){
    disk_t *disk = DISK_open_for_reading(filename);

    if (disk==NULL){
      GFX_Message2(NULL, true, "File not found (%s)", filename.toUtf8().constData());
      return;
    }

    QString new_code = DISK_read_qstring_file(disk);
      
    if (DISK_close_and_delete(disk)==false) {
      GFX_Message2(NULL, true, "Unable to read from %s", filename.toUtf8().constData());
      return;
    }
    
    set_text_in__faust_editor_widget(new_code);
  }
  
  void save_source(QString filename){
    disk_t *disk = DISK_open_for_writing(filename);

    //GFX_Message(NULL, "   fff filename: -%s-, %p",filename.toUtf8().constData(), disk);
    if (disk==NULL){
      GFX_Message2(NULL, true, "Unable to open %s for writing", filename.toUtf8().constData());
      return;
    }

    QString code = _faust_editor->text();
    int n = DISK_write_qstring(disk, code);
    
    bool show_warning = false;
    if (n != code.size())
      show_warning = true;

    if (DISK_close_and_delete(disk)==false)
      return;

    if (show_warning)
      GFX_Message2(NULL, true, "Warning: Wrote %d bytes. Expected %d", n, code.size());
  }

  void show_cpp_source(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_cpp_dialog==NULL) {
        _cpp_dialog = new RememberGeometryQDialog(this, radium::NOT_MODAL);
        QHBoxLayout *mainLayout = new QHBoxLayout;
      
        _cpp_dialog->setLayout(mainLayout);
        
        _cpp_editor = create_editor(_cpp_dialog);

        mainLayout->addWidget(_cpp_editor);
        
        _cpp_dialog->resize(600,400);
      }
      
      _cpp_dialog->show();
      _cpp_dialog->raise();

      update_cpp_editor(plugin);
    }
  }

  void edit_options(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_options_dialog==NULL){
        _options_dialog = new RememberGeometryQDialog(this, radium::NOT_MODAL);
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

  void hideEvent(QHideEvent * event) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    _size_type_before_hidden = _size_type;
    
    if(_size_type!=SIZETYPE_NORMAL)
      set_small(); // If not, all instrument widgets will have large height, maybe
  }

  void showEvent(QShowEvent * event) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    if (_size_type_before_hidden != SIZETYPE_NORMAL)
      set_large(_size_type_before_hidden);
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
          ADD_UNDO(FaustDev_CurrPos(_patch.data(), old_code, _prev_cursor_line, _prev_cursor_index)); // note: _prev_cursor_pos is not correct when pasting something.
        }
      }
    }
  }
  
};

}


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
