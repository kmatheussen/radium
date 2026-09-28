#ifndef __M_LIBPD_H__ 
#define __M_LIBPD_H__ 
 
#ifdef __cplusplus 
extern "C" 
{ 
#endif 
 
#include <stdbool.h> 
 
#include "m_pd.h" 
#include "z_libpd.h" 
 
typedef struct _pd pd_t; 

pd_t *libpds2_create(bool use_gui, const char* libdir); 
char *libpds2_strerror(void); 
void libpds2_delete(pd_t *pd); 
void libpds2_set_hook_data(pd_t *pd, void* data); 
void libpds2_clear_search_path(pd_t *pd); 
void libpds2_add_to_search_path(pd_t *pd, const char* sym); 
void* libpds2_openfile(pd_t *pd, const char* basename, const char* dirname); 
void libpds2_closefile(pd_t *pd, void* p); 
void libpds2_savefile(pd_t *pd, void* p, const char *filename, const char *dirname); 
int libpds2_getdollarzero(pd_t *pd, void* p); 
void libpds2_show_gui(pd_t *pd); 
void libpds2_hide_gui(pd_t *pd); 
int libpds2_poll_gui(pd_t *pd); 
int libpds2_try_poll_gui(pd_t *pd); 
int libpds2_blocksize(pd_t *pd); 
int libpds2_init_audio(pd_t *pd, int inChans, int outChans, int sampleRate); 
int libpds2_process_raw(pd_t *pd, const float* inBuffer, float* outBuffer); 
int libpds2_process_float_noninterleaved(pd_t *pd, int ticks, const float** inBuffer, float** outBuffer); 
int libpds2_process_short(pd_t *pd, const int ticks, const short* inBuffer, short* outBuffer); 
int libpds2_process_float(pd_t *pd, int ticks, const float* inBuffer, float* outBuffer); 
int libpds2_process_double(pd_t *pd, int ticks, const double* inBuffer, double* outBuffer); 
int libpds2_arraysize(pd_t *pd, const char* name); 
int libpds2_read_array(pd_t *pd, float* dest, const char* src, int offset, int n); 
int libpds2_write_array(pd_t *pd, const char* dest, int offset, float* src, int n); 
int libpds2_bang(pd_t *pd, const char* recv); 
int libpds2_float(pd_t *pd, const char* recv, float x); 
int libpds2_symbol(pd_t *pd, const char* recv, const char* sym); 
void libpds2_set_float(pd_t *pd, t_atom* v, float x); 
void libpds2_set_symbol(pd_t *pd, t_atom* v, const char* sym); 
int libpds2_list(pd_t *pd, const char* recv, int argc, t_atom* argv); 
int libpds2_message(pd_t *pd, const char* recv, const char* msg, int argc, t_atom* argv); 
int libpds2_start_message(pd_t *pd, int max_length); 
void libpds2_add_float(pd_t *pd, float x); 
void libpds2_add_symbol(pd_t *pd, const char* sym); 
int libpds2_finish_list(pd_t *pd, const char* recv); 
int libpds2_finish_message(pd_t *pd, const char* recv, const char* msg); 
int libpds2_exists(pd_t *pd, const char* sym); 
void* libpds2_bind(pd_t *pd, const char* sym, void* data); 
void libpds2_unbind(pd_t *pd, void* p); 
void libpds2_set_printhook(pd_t *pd, t_libpd_printhook hook); 
void libpds2_set_banghook(pd_t *pd, t_libpd_banghook hook); 
void libpds2_set_floathook(pd_t *pd, t_libpd_floathook hook); 
void libpds2_set_symbolhook(pd_t *pd, t_libpd_symbolhook hook); 
void libpds2_set_listhook(pd_t *pd, t_libpd_listhook hook); 
void libpds2_set_messagehook(pd_t *pd, t_libpd_messagehook hook); 
int libpds2_noteon(pd_t *pd, int channel, int pitch, int velocity); 
int libpds2_controlchange(pd_t *pd, int channel, int controller, int value); 
int libpds2_programchange(pd_t *pd, int channel, int value); 
int libpds2_pitchbend(pd_t *pd, int channel, int value); 
int libpds2_aftertouch(pd_t *pd, int channel, int value); 
int libpds2_polyaftertouch(pd_t *pd, int channel, int pitch, int value); 
int libpds2_midibyte(pd_t *pd, int port, int byte); 
int libpds2_sysex(pd_t *pd, int port, int byte); 
int libpds2_sysrealtime(pd_t *pd, int port, int byte); 
void libpds2_set_noteonhook(pd_t *pd, t_libpd_noteonhook hook); 
void libpds2_set_controlchangehook(pd_t *pd, t_libpd_controlchangehook hook); 
void libpds2_set_programchangehook(pd_t *pd, t_libpd_programchangehook hook); 
void libpds2_set_pitchbendhook(pd_t *pd, t_libpd_pitchbendhook hook); 
void libpds2_set_aftertouchhook(pd_t *pd, t_libpd_aftertouchhook hook); 
void libpds2_set_polyaftertouchhook(pd_t *pd, t_libpd_polyaftertouchhook hook); 
void libpds2_set_midibytehook(pd_t *pd, t_libpd_midibytehook hook); 

#ifdef __cplusplus 
} 
#endif 
 
#endif 
