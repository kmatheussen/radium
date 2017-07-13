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

#ifndef OS_SETTINGS_PROC_H
#define OS_SETTINGS_PROC_H

#include <wchar.h>

#define SCANNED_PLUGINS_DIRNAME QString("scanned_plugins")

extern LANGSPEC void OS_set_loading_path(const wchar_t *filename);
extern LANGSPEC void OS_unset_loading_path(void);
extern LANGSPEC const wchar_t *OS_loading_get_resolved_file_path(const wchar_t *path, bool program_state_is_valid);

extern LANGSPEC const wchar_t *OS_saving_get_relative_path_if_possible(const wchar_t *filepath);  
extern LANGSPEC void OS_set_saving_path(const wchar_t *filename);

extern LANGSPEC const char *OS_get_directory_separator(void);
extern LANGSPEC void OS_set_argv0(char *argv0);
extern LANGSPEC const char *OS_get_program_path(void);
extern LANGSPEC const wchar_t *OS_get_program_path2(void);

extern LANGSPEC bool OS_config_key_is_color(const char *key);

#ifdef USE_QT4
#include <QString>
QString OS_get_home_path(void);
QString OS_get_dot_radium_path(void);

bool OS_has_full_program_file_path(QString filename);
QString OS_get_full_program_file_path(QString filename); // Note, will exit radium if filename doesn't exist
bool OS_has_conf_filename(QString filename);
QString OS_get_config_filename(const char *key);
QString OS_get_conf_filename(QString filename);
#endif

extern LANGSPEC wchar_t *OS_get_full_program_file_path(const wchar_t *filename);
extern LANGSPEC bool OS_has_conf_filename2(const char *filename);
extern LANGSPEC char *OS_get_conf_filename2(const char *filename);

#ifdef USE_QT4
#include <QString>
QString OS_get_keybindings_conf_filename(void);
QString OS_get_menues_conf_filename(void);
extern QString OS_get_custom_keybindings_conf_filename(void);
#endif
extern LANGSPEC char *OS_get_keybindings_conf_filename2(void);
extern LANGSPEC char *OS_get_menues_conf_filename2(void);

extern LANGSPEC char *OS_get_custom_keybindings_conf_filename2(void);

extern LANGSPEC void OS_make_config_file_expired(const char *key);

// locale independent.
extern LANGSPEC double OS_get_double_from_string(const char *s);
extern LANGSPEC char *OS_get_string_from_double(double d);

#ifdef USE_QT4
QString OS_get_qstring_from_double(double d);
#endif

#endif
