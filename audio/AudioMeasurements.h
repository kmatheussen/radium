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

#include <stdbool.h>

#include "AudioAnalyzer.h"

struct SoundProducer;
struct SoundPlugin;

// Keeps the most recent output of ONE instrument (the one the LLM effect tuner
// is looking at) so the tuning prompt can include a few measured facts about
// what it currently sounds like.
//
// The capture happens in the audio thread and costs one check of an atomic
// pointer per block; everything else (FFT) runs on the main thread when a
// prompt is built. All functions are safe to call at any time.

// Selects which instrument's output to capture. NULL stops capturing.
// 'sound_plugin' is the instrument's plugin; it is matched as a fallback when
// the SoundProducer pointer differs from the one the mixer processes (the two
// are looked up on different threads), which would otherwise silently disable
// the measurements. Main thread.
void MEASUREMENTS_set_target(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin);
void MEASUREMENTS_clear_target(void);

// Called from the audio thread with the final output of one instrument.
// Cheap when 'sound_producer' is not the target.
void MEASUREMENTS_RT_capture(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                             float **sounds, int num_channels, int num_frames);

// Analyzes the captured block for 'sound_producer' (the one passed to
// MEASUREMENTS_set_target). Returns false when there is nothing recent to
// analyze (not playing, silent, or FFTW unavailable). Main thread.
bool MEASUREMENTS_get(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                      int sample_rate, AudioAnalysis *out);

// Waits up to 'timeout_ms' for a block of this instrument to be captured.
// Main thread. Used when a prompt is built while nothing has been captured yet
// (the target may have been selected a moment ago): if the instrument is
// audible, a block arrives within ~85 ms.
bool MEASUREMENTS_wait_for_block(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin, int timeout_ms);

// Why MEASUREMENTS_get() has no usable data. For diagnostics: the difference
// between "the audio thread never saw this instrument" and "the plugin is
// missing" matters when something does not work.
typedef enum {
  MEASUREMENTS_STATUS_OK,                 // a block is available
  MEASUREMENTS_STATUS_NO_PRODUCER,        // no SoundProducer for the instrument
  MEASUREMENTS_STATUS_NOT_TARGET,         // another instrument is selected
  MEASUREMENTS_STATUS_NEVER_CAPTURED,     // the audio thread never processed it
  MEASUREMENTS_STATUS_SILENT,             // captured, but the block is silent
  MEASUREMENTS_STATUS_NO_ANALYZER,        // libfftw3f is not available
} MeasurementsStatus;

MeasurementsStatus MEASUREMENTS_get_status(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                                           int sample_rate, AudioAnalysis *out);

// Diagnostics for the current target: how many times the audio thread called
// into the capture for it, and how many complete blocks it published. Zero
// calls means the instrument's RT_process never ran (it was idle/autosuspended)
// or the target is not the producer the mixer processes.
long long MEASUREMENTS_get_num_capture_calls(void);
long long MEASUREMENTS_get_num_captured_blocks(void);

// Milliseconds since the returned block was captured (or -1 when there is no
// block for this instrument). Measurements taken before a parameter change no
// longer describe the sound, so the age is shown to the model.
long long MEASUREMENTS_get_age_ms(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin);
