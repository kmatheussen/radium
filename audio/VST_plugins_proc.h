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


#if defined(__linux__)
#  define VST_SUFFIX "so"
#endif

#if defined(FOR_MACOSX)
#  define VST_SUFFIX ""
#endif

#if defined(FOR_WINDOWS)
#  define VST_SUFFIX "dll"
#endif


std::vector<QString> VST_get_vst_paths(void);
void VST_write_vst_paths(const std::vector<QString> &paths);
void VST_add_path(QString path);
