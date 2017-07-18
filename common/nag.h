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


#ifndef QT_NAG_H
#define QT_NAG_H

#include "visual_proc.h"

static void show_nag_window(const char *message){
#if FULL_VERSION==0
  GFX_Message2(NULL,
               true,
               "%sThanks for trying this DEMO version of Radium!<br>"
               "I hope you like the program."
               "<p>"
               "In this version, only two VST plugins can be used at the same time, and soundfile export is disabled.<p>"
               "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a> to get access to the full version.<br>"
               "It only costs &#8364;2 per month, and you can unsubscribe at any time."
               "<p>"
               "Thanks again for using Radium!"
               "<p>"
               "-Kjetil Matheussen",
               message
               );
#endif // FULL_VERSION==0
}


#endif
