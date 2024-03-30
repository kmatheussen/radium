
#ifndef _RADIUM_EMBEDDED_SCHEME_SCHEME_PROC_H
#define _RADIUM_EMBEDDED_SCHEME_SCHEME_PROC_H

extern bool g_scheme_has_inited1;
extern bool g_scheme_has_inited2;

extern LANGSPEC bool quantitize_note(const struct Blocks *block, struct Notes *note, bool *was_changed); // 'was_changed' must point to a bool. If note was changed, the value will be true.

extern LANGSPEC void SCHEME_throw_catch(const char *symbol, const char *message); // Call this one to display backtrace into the message log. It throws, but it also catches its own exception. In practice, it jdoes the same as GFX_addMessage(SCHEME_get_history()), but in a different way, and less efficiently.
extern LANGSPEC void SCHEME_throw(const char *symbol, const char *message);
extern LANGSPEC const char *SCHEME_get_history(void); // The returned value should be freed with call to free().
extern LANGSPEC bool SCHEME_is_currently_getting_scheme_history(void);
extern LANGSPEC bool SCHEME_mousepress(int button, float x, float y);
extern LANGSPEC bool SCHEME_mousemove_rerun(int button, float x, float y);
extern LANGSPEC bool SCHEME_mousemove(int button, float x, float y);
extern LANGSPEC bool SCHEME_mouserelease(int button, float x, float y);
extern LANGSPEC dyn_t SCHEME_eval_withreturn(const char *code);
extern LANGSPEC void SCHEME_eval(const char *code);
extern LANGSPEC int SCHEME_get_webserver_port(void);
extern LANGSPEC void SCHEME_init1(void);
extern LANGSPEC void SCHEME_init2(void);

#endif
