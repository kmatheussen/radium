/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifndef CRASHREPORTER_CRASHREPORTER_PROC_H
#define CRASHREPORTER_CRASHREPORTER_PROC_H

enum Crash_Type{
  CT_CRASH,
  CT_ERROR,
  CT_WARNING
};

#define CR_STRINGIZE_DETAIL(x) #x
#define CR_STRINGIZE(x) CR_STRINGIZE_DETAIL(x)
#define CR_FORMATEVENT(msg,...) (msg ": " __FILE__ " line " CR_STRINGIZE(__LINE__) "." __VA_ARGS__)
// __FUNCTION__ is a variable. (this is a clear design flaw in gcc and should be fixed)


extern double g_rt_set_bus_descendant_types_duration;

extern double g_last_midi_receive_time;

#ifdef __cplusplus
extern "C"{
#endif

  void EVENTLOG_add_event(const char *log_entry);
  const char *EVENTLOG_get(void);
    
  void CRASHREPORTER_init(void);
  int CRASHREPORTER_set_plugin_name(const char *plugin_name);
  void CRASHREPORTER_unset_plugin_name(int pos);
  void CRASHREPORTER_send_message(const char *additional_information, const char **messages, int num_messages, enum Crash_Type crash_type, double time);
  void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, enum Crash_Type crash_type, double time);


  void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...);

  bool CRASHREPORTER_is_currently_sending(void);


  // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT.
  /*
  // got strange compilation error. Not important anyway.
#define CRASHREPORTER_send_assert_message(CrashType, FMT, ...)          \
  do{                                                                   \
    if(0)printf(FMT,  __VA_ARGS__ );                                    \
    CRASHREPORTER_send_assert_message_internal(CrashType, FMT, __VA_ARGS__); \
  }while(0)
  */
  
  void CRASHREPORTER_dont_report_more(void);
  void CRASHREPORTER_do_report(void);
  void CRASHREPORTER_dont_report(void);

  void CRASHREPORTER_close(void);

  void CRASHREPORTER_posix_init(void);
  void CRASHREPORTER_windows_init(void);
  void CRASHREPORTER_windows_close(void);

#ifdef __cplusplus
}
#endif


#endif // CRASHREPORTER_CRASHREPORTER_PROC_H

