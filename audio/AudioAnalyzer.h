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

// A coarse description of a short audio block, made for the LLM effect tuner:
// the figures a mixing decision can be based on (loudness, dynamics, stereo
// width, spectral shape, noise, transients). Coarse on purpose - it is prompt
// text, and a fine analysis would cost more than it tells the model.
//
// Everything is computed once per prompt on the main thread, from ~1 s of the
// instrument's output (audio/AudioMeasurements.cpp).

#define AUDIO_ANALYZER_NUM_BANDS 10

typedef struct {
  float low_hz;
  float high_hz;
} AudioAnalyzerBand;

typedef struct {
  bool valid;

  int num_frames;
  int sample_rate;

  // ---- levels ----
  // Full-scale-relative: peak_db/rms_db are dB re full scale (0 = full scale),
  // crest_db = peak_db - rms_db.
  float peak_db;
  float rms_db;
  float crest_db;

  // BS.1770 loudness (K-weighted), in LUFS. The momentary figure uses the last
  // 400 ms, the "window" figure the whole captured block. 'loudest' is the
  // maximum over all 400 ms windows: the level of the SOUND, which is the one
  // to match levels with (the window average is depressed by the silence after
  // a decaying note).
  float loudness_momentary_lufs;
  float loudness_loudest_lufs;
  float loudness_window_lufs;
  // Inter-sample peak estimate (4x oversampled), in dB re full scale.
  float true_peak_db;

  // ---- stereo ----
  // 1 = mono/identical channels, 0 = uncorrelated, -1 = out of phase.
  float stereo_correlation;
  // Side (L-R) level relative to mid (L+R), in dB (very negative = mono).
  float stereo_side_db;
  // Right minus left level in dB (positive = louder on the right).
  float stereo_balance_db;

  // ---- spectral shape ----
  float centroid_hz;   // "brightness": energy-weighted mean frequency
  float flatness;      // 0 = tonal, 1 = noise-like (geometric/arithmetic mean)
  float dc_offset;     // mean (mid) sample value, ~0 for a healthy signal
  float hum_db;        // level of a mains-hum peak, dB re full scale
  float hum_hz;        // 0 when no hum peak was found
  float sub_bass_db;   // level below 30 Hz, dB re full scale

  // ---- transients ----
  float attack_db_per_ms;  // steepest rise of the envelope (dB per ms)
  float onsets_per_second; // detected attacks per second

  // ---- space (reverb / room) ----
  // Estimated time for the level to fall 60 dB after a peak (RT60-like), in
  // seconds. 0 when no clear decay could be measured (sustained material,
  // silence, or a decay that does not fit the window).
  float decay_time_s;
  // Level 300 ms after that peak, relative to the peak, in dB. Closer to 0
  // means a longer/heavier tail; -30 dB or lower means dry/short. 0 when
  // unknown.
  float tail_db;

  // Per-band level in dB re full scale, band-limited (so a band with no energy
  // is very negative). Index i corresponds to AUDIO_ANALYZER_band(i).
  float band_db[AUDIO_ANALYZER_NUM_BANDS];
} AudioAnalysis;

// Band i of the analysis (edges in Hz).
AudioAnalyzerBand AUDIO_ANALYZER_band(int band_num);

// Analyzes 'left' (and 'right' when not NULL, otherwise mono) -num_frames
// samples at 'sample_rate'. Returns false for too-short input.
bool AUDIO_ANALYZE(const float *left, const float *right, int num_frames, int sample_rate, AudioAnalysis *out);
