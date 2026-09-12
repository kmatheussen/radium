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

#pragma once

#include <QString>

static const char *g_default_faust_dev2_program =
	"import(\"stdfaust.lib\");\n"
	"\n"
	"freq = hslider(\"freq\", 440, 20, 20000, 0.01);\n"
	"gain = hslider(\"gain\", 0.25, 0, 1, 0.01);\n"
	"volume = hslider(\"volume\", 0, -60, 12, 0.1) : ba.db2linear : si.smooth(ba.tau2pole(0.010));\n"
	"gate = button(\"gate\");\n"
	"\n"
	"attack = hslider(\"attack\", 0.01, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"decay = hslider(\"decay\", 0.2, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"sustain = hslider(\"sustain\", 0.7, 0, 1, 0.01) : si.smooth(ba.tau2pole(0.010));\n"
	"release = hslider(\"release\", 0.4, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"\n"
	"envelope = en.adsr(attack, decay, sustain, release, gate);\n"
	"\n"
	"process = os.osc(freq) * gain * envelope * volume <: _,_;\n";

static inline QString FAUST2_get_default_code(void)
{
	return QString(g_default_faust_dev2_program);
}
