
extern LANGSPEC void API_gui_call_regularly(void);

extern LANGSPEC bool MIXERSTRIPS_has_mouse_pointer(void);

#if USE_QT5
QWidget *MIXERSTRIPS_get_curr_widget(void);
QWidget *API_gui_get_widget(int64_t guinum);
#endif
