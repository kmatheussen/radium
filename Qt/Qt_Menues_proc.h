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

#pragma once

class QMenu;
class QMenuBar;

extern QMenuBar *g_main_menu_bar;
extern QMenuBar *g_main_menu_bar_right; // Second menu bar, used to place menus to the right of the notch when the window is full screen on a mac with a notch.

void initMenues(QMenuBar *base_menu);
void init_recent_menu(void); // Call after all main menus have been set up.
void GFX_AddMenuMenu(const char *name, QMenu *mymenu);
void GFX_SetMenuFontsAgain(void);

// When window_is_fullscreen==true, and the window is on a mac display with a
// notch, menus that would be placed under the notch are moved to
// g_main_menu_bar_right, and an empty area is reserved where the notch is.
void update_main_menu_notch_gap(bool window_is_fullscreen);
