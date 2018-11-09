
#ifndef _RADIUM_API_API_GUI_PROC_H
#define _RADIUM_API_API_GUI_PROC_H

extern LANGSPEC void API_gui_call_regularly(void);

extern LANGSPEC bool MIXERSTRIPS_has_mouse_pointer(void);

//extern LANGSPEC bool API_gui_is_painting(void);

extern LANGSPEC void API_add_child_plugin_gui(int64_t guinum, struct Patch *patch);
extern LANGSPEC void API_remove_child_plugin_gui(int64_t guinum, struct Patch *patch);



#ifdef __cplusplus

void *API_get_native_gui_handle(int64_t guinum);


#if USE_QT5
class QWidget;
class QPaintEvent;
class QMouseEvent;
class QEvent;
class QResizeEvent;
class QPainter;

QVector<QWidget*> MIXERSTRIPS_get_all_widgets(void);
QWidget *MIXERSTRIPS_get_curr_widget(void);
QWidget *API_gui_get_widget(int64_t guinum);
QWidget *API_gui_get_parentwidget(QWidget *child, int64_t guinum); // child can be NULL. It's only used when guinum=-2.
int64_t API_get_gui_from_widget(QWidget *widget);
int64_t API_get_gui_from_existing_widget(QWidget *widget);

#if defined(QREGION_H)
void API_run_paint_event_for_custom_widget(QWidget *widget, QPaintEvent *ev, const QRegion &already_painted_areas);
bool API_run_custom_gui_paint_function(QWidget *widget, QPainter *p, const QRegion *region, std::function<void(void)> func);
#endif

#ifdef RADIUM_QT_HELPERS
bool API_run_mouse_press_event_for_custom_widget(QWidget *widget, QMouseEvent *ev);
bool API_run_mouse_move_event_for_custom_widget(QWidget *widget, QMouseEvent *ev);
bool API_run_mouse_release_event_for_custom_widget(QWidget *widget, radium::MouseCycleEvent &event);
bool API_run_mouse_leave_event_for_custom_widget(QWidget *widget, QEvent *ev);
void API_run_resize_event_for_custom_widget(QWidget *widget, QResizeEvent *ev);
#endif

QWidget *API_get_main_ysplitter(void);
QWidget *API_get_lowertabs(void);
void API_setLowertabIncludesInstrument(bool includeit);
bool API_instrumentGuiIsVisibleInLowerTab(void);
void API_showInstrumentGui(void);
void API_hideInstrumentGui(void);
void API_showSequencerGui(void);
void API_hideSequencerGui(void);
#endif
#endif

#endif
