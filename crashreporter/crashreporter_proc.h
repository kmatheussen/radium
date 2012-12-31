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

#ifdef __cplusplus
extern "C"{
#endif

  void CRASHREPORTER_init(void);
  void CRASHREPORTER_report_crash(const char **messages, int num_messages);
  void CRASHREPORTER_close(void);

  void CRASHREPORTER_posix_init(void);
  void CRASHREPORTER_windows_init(void);
  void CRASHREPORTER_windows_close(void);

#ifdef __cplusplus
}
#endif


#endif // CRASHREPORTER_CRASHREPORTER_PROC_H

