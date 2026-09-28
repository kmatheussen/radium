/* Copyright 2026 Kjetil S. Matheussen

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

/*
  Dispatches the PD_* calls used by the GUI, undo and API code to the
  implementation of the instrument type in question ("Pd" or "Pd2").
*/

#include <string.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "Pd_plugin.h"
#include "Pd_plugin_proc.h"
#include "Pd_plugin2_proc.h"

bool PD_is_pd_type(const char *type_name)
{
	return !strcmp(type_name, "Pd") || !strcmp(type_name, "Pd2");
}

const wchar_t *PD_set_controller_name(SoundPlugin *plugin, int n, const wchar_t *name)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd"))
		return PD1_set_controller_name(plugin, n, name);
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		return PD2_set_controller_name(plugin, n, name);
#endif
	return name;
}

Pd_Controller *PD_get_controller(SoundPlugin *plugin, int n)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd"))
		return PD1_get_controller(plugin, n);
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		return PD2_get_controller(plugin, n);
#endif
	return NULL;
}

void PD_set_qtgui(SoundPlugin *plugin, void *qtgui)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd")) {
		PD1_set_qtgui(plugin, qtgui);
		return;
	}
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		PD2_set_qtgui(plugin, qtgui);
#endif
}

void PD_delete_controller(SoundPlugin *plugin, int controller_num)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd")) {
		PD1_delete_controller(plugin, controller_num);
		return;
	}
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		PD2_delete_controller(plugin, controller_num);
#endif
}

void PD_put_controllers_to_state(const SoundPlugin *plugin, hash_t *state)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd")) {
		PD1_put_controllers_to_state(plugin, state);
		return;
	}
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		PD2_put_controllers_to_state(plugin, state);
#endif
}

void PD_recreate_controllers_from_state(SoundPlugin *plugin, const hash_t *state)
{
#ifdef WITH_PD
	if (!strcmp(plugin->type->type_name, "Pd")) {
		PD1_recreate_controllers_from_state(plugin, state);
		return;
	}
#endif
#ifdef WITH_PD2
	if (!strcmp(plugin->type->type_name, "Pd2"))
		PD2_recreate_controllers_from_state(plugin, state);
#endif
}
