#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif

extern LANGSPEC void create_jack_plugins(void);

struct SoundPlugin;
extern LANGSPEC const char *JACK_get_name(const SoundPlugin *plugin, int portnum);
extern LANGSPEC void JACK_set_name(SoundPlugin *plugin, int portnum, const char *new_name);
