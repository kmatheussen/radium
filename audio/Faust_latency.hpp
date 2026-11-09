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

#include <stdlib.h>
#include <string.h>

#include "../bin/packages/faust/architecture/faust/dsp/dsp.h"
#include "../bin/packages/faust/architecture/faust/gui/meta.h"

namespace radium
{

// Faust does not report latency from the 'dsp' class, but its official
// architectures (faust2juce) use global metadata to declare it:
//
//   declare latency_frames  "128";   // or latency_samples
//   declare latency_sec     "0.003";
//
// Note that the declaration does not insert the delay into the DSP - the
// program must do that itself (e.g. '... : @(128)'). The metadata is read
// with dsp->metadata(), which works for both the LLVM and the interpreter
// backend.
struct FaustLatencyMeta : public Meta
{
	double frames = -1.0;
	double seconds = -1.0;

	void declare(const char *key, const char *value) override
	{
		if (!strcmp(key, "latency_frames") || !strcmp(key, "latency_samples"))
			frames = atof(value);
		else if (!strcmp(key, "latency_sec"))
			seconds = atof(value);
	}
};

// Returns the declared latency in samples, or 0 if no (valid) latency is
// declared. 'sample_rate' must be the sample rate the dsp instance was
// initialized with, and is only used to convert a 'latency_sec' declaration.
static inline int faust_get_latency_samples(dsp *dsp_instance, double sample_rate)
{
	if (dsp_instance == NULL)
		return 0;

	FaustLatencyMeta meta;
	dsp_instance->metadata(&meta);

	double frames = meta.frames;
	if (frames <= 0)
		frames = meta.seconds > 0 ? meta.seconds * sample_rate + 0.5 : 0.0;

	if (!(frames > 0))
		return 0;

	// Sanity clamp (10 seconds) so a typo can't wreck delay compensation.
	const double max_frames = 10.0 * (sample_rate > 0 ? sample_rate : 48000.0);
	if (frames > max_frames)
		frames = max_frames;

	return (int)frames;
}

} // namespace radium
