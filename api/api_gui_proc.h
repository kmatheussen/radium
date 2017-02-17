
#ifndef _RADIUM_API_API_GUI_PROC_H
#define _RADIUM_API_API_GUI_PROC_H

extern LANGSPEC void API_gui_call_regularly(void);

extern LANGSPEC bool MIXERSTRIPS_has_mouse_pointer(void);

extern LANGSPEC void API_gui_pause_all_paint_callbacks(void);
extern LANGSPEC void API_gui_continue_all_paint_callbacks(void);
extern LANGSPEC bool API_gui_is_painting(void);

#ifdef __cplusplus
#if USE_QT5
class QWidget;
QVector<QWidget*> MIXERSTRIPS_get_all_widgets(void);
QWidget *MIXERSTRIPS_get_curr_widget(void);
QWidget *API_gui_get_widget(int64_t guinum);
#endif
#endif

#endif
