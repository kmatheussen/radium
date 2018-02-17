
#ifndef _RADIUM_QT_COMMEND_DIALOG_PROC_H
#define _RADIUM_QT_COMMEND_DIALOG_PROC_H 1

extern LANGSPEC void COMMENTDIALOG_open(void);
extern LANGSPEC bool COMMENT_show_after_loading(void);
extern LANGSPEC hash_t *COMMENT_get_state(void);
extern LANGSPEC void COMMENT_reset(void);
extern LANGSPEC void COMMENT_set_state(hash_t *state);

#endif
