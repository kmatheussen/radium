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

// A coarse spectral/level description of a short audio block, made for the LLM
// effect tuner: ten octave-ish bands plus the usual level figures. Coarse on
// purpose - it is prompt text, and a fine spectrum would cost more than it
// tells the model.
//
// FFTW is loaded with dlopen() on first use, so Radium builds and runs exactly
// as before on systems without libfftw3 (AUDIO_ANALYZE then returns false).

#define AUDIO_ANALYZER_NUM_BANDS 10

typedef struct {
  float low_hz;
  float high_hz;
} AudioAnalyzerBand;

typedef struct {
  bool valid;

  int num_frames;
  int sample_rate;

  // Full-scale-relative levels. peak_db/rms_db are dBFS (0 = full scale),
  // crest_db = peak_db - rms_db.
  float peak_db;
  float rms_db;
  float crest_db;

  // Per-band level in dBFS, band-limited (so a band with no energy is very
  // negative). Index i corresponds to AUDIO_ANALYZER_band(i).
  float band_db[AUDIO_ANALYZER_NUM_BANDS];
} AudioAnalysis;

// Band i of the analysis (edges in Hz).
AudioAnalyzerBand AUDIO_ANALYZER_band(int band_num);

// Analyzes 'mono' (num_frames samples, mixed down to mono by the caller).
// Returns false when FFTW is unavailable or the input is too short.
bool AUDIO_ANALYZE(const float *mono, int num_frames, int sample_rate, AudioAnalysis *out);
