#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif

<<<<<<< HEAD
extern LANGSPEC void create_jack_plugins(void);
=======
extern LANGSPEC void create_jack_plugins();
>>>>>>> d866f9643abae87c36001180994c3d5f26c4d187

struct SoundPlugin;
extern LANGSPEC const char *JACK_get_name(SoundPlugin *plugin, int portnum);
extern LANGSPEC void JACK_set_name(SoundPlugin *plugin, int portnum, const char *new_name);
