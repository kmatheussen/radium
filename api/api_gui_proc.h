
#ifndef _RADIUM_API_API_GUI_PROC_H
#define _RADIUM_API_API_GUI_PROC_H

extern LANGSPEC void API_gui_call_regularly(void);

extern LANGSPEC bool MIXERSTRIPS_has_mouse_pointer(void);

extern LANGSPEC bool API_gui_is_painting(void);

#ifdef __cplusplus
#if USE_QT5
class QWidget;
QVector<QWidget*> MIXERSTRIPS_get_all_widgets(void);
QWidget *MIXERSTRIPS_get_curr_widget(void);
QWidget *API_gui_get_widget(int64_t guinum);
QWidget *API_gui_get_parentwidget(int64_t guinum);
int64_t API_get_gui_from_widget(QWidget *widget);
int64_t API_get_gui_from_existing_widget(QWidget *widget);
#endif
#endif

#endif
