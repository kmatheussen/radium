extern LANGSPEC bool quantitize_note(const struct Blocks *block, struct Notes *note);


extern LANGSPEC bool SCHEME_mousepress(int button, float x, float y);
extern LANGSPEC bool SCHEME_mousemove(int button, float x, float y);
extern LANGSPEC bool SCHEME_mouserelease(int button, float x, float y);
extern LANGSPEC void SCHEME_eval(const char *code);
extern LANGSPEC int SCHEME_get_webserver_port(void);
extern LANGSPEC void SCHEME_start(void);
