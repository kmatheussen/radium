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

// Coarse spectrum/level analysis for the LLM effect tuner.
//
// FFTW3 (single precision, libfftw3f) is linked normally; the build script
// passes `pkg-config --cflags fftw3f` when compiling this file and
// `pkg-config --libs fftw3f` when linking Radium.
//
// The output is deliberately coarse (10 bands, ~12 Hz resolution at 48 kHz
// with 4096 samples). It is meant to give the model a few measurable facts
// ("the 60-120 Hz band is 9 dB below 250-500 Hz") instead of none.
//
// Everything here runs on the main thread only (a plan and its buffers are
// reused between calls).

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <fftw3.h>

#include "AudioAnalyzer.h"

namespace {
  struct {
    // Scratch buffers, reused (analysis runs on the main thread only).
    int num_bins = 0;
    int num_frames = 0;

    float *windowed = NULL;
    fftwf_complex *spectrum = NULL;
    fftwf_plan plan = NULL;
  } g_fftw;

  bool ensure_fftw(int num_frames)
  {
    if (g_fftw.plan != NULL && g_fftw.num_frames == num_frames)
      return true;

    if (g_fftw.plan != NULL){
      fftwf_destroy_plan(g_fftw.plan);
      g_fftw.plan = NULL;
    }
    if (g_fftw.windowed != NULL){
      fftwf_free(g_fftw.windowed);
      g_fftw.windowed = NULL;
    }
    if (g_fftw.spectrum != NULL){
      fftwf_free(g_fftw.spectrum);
      g_fftw.spectrum = NULL;
    }

    // FFTW's own allocation returns suitably aligned memory for SIMD plans.
    g_fftw.windowed = (float*)fftwf_malloc(sizeof(float) * num_frames);
    g_fftw.num_bins = num_frames / 2 + 1;
    g_fftw.spectrum = (fftwf_complex*)fftwf_malloc(sizeof(fftwf_complex) * g_fftw.num_bins);

    if (g_fftw.windowed == NULL || g_fftw.spectrum == NULL)
      return false;

    // FFTW_ESTIMATE: no measurement pass, so asking for a plan from the GUI is
    // fast and does not touch the audio thread.
    g_fftw.plan = fftwf_plan_dft_r2c_1d(num_frames, g_fftw.windowed, g_fftw.spectrum, FFTW_ESTIMATE);
    if (g_fftw.plan == NULL)
      return false;

    g_fftw.num_frames = num_frames;
    return true;
  }
}

AudioAnalyzerBand AUDIO_ANALYZER_band(int band_num)
{
  // Octave-ish bands, with the 60-120 Hz pair kept separate because most
  // "more bass" requests land there. (Radium's source sanity check forbids
  // static arrays, so this is computed instead of looked up.)
  const float edges[AUDIO_ANALYZER_NUM_BANDS + 1] = {
    30.0f, 60.0f, 120.0f, 250.0f, 500.0f, 1000.0f, 2000.0f, 4000.0f, 8000.0f, 16000.0f, 24000.0f
  };

  AudioAnalyzerBand band = {0.0f, 0.0f};
  if (band_num < 0 || band_num >= AUDIO_ANALYZER_NUM_BANDS)
    return band;

  band.low_hz = edges[band_num];
  band.high_hz = edges[band_num + 1];
  return band;
}

static float to_db(float power)
{
  // -120 dB for silence, so the prompt never contains -inf.
  if (!(power > 0.0f))
    return -120.0f;

  return fmaxf(-120.0f, 10.0f * log10f(power));
}

bool AUDIO_ANALYZE(const float *mono, int num_frames, int sample_rate, AudioAnalysis *out)
{
  if (out == NULL)
    return false;

  memset(out, 0, sizeof(AudioAnalysis));

  if (mono == NULL || num_frames < 512 || sample_rate <= 0)
    return false;

  // ---- time-domain figures ----
  double sum_squares = 0.0;
  float peak = 0.0f;
  for (int i = 0; i < num_frames; i++){
    const float sample = mono[i];
    sum_squares += (double)sample * (double)sample;
    const float abs_sample = fabsf(sample);
    if (abs_sample > peak)
      peak = abs_sample;
  }

  const float rms = (float)sqrt(sum_squares / (double)num_frames);

  out->valid = true;
  out->num_frames = num_frames;
  out->sample_rate = sample_rate;
  out->peak_db = to_db(peak * peak);
  out->rms_db = to_db(rms * rms);
  out->crest_db = out->peak_db - out->rms_db;

  for (int band = 0; band < AUDIO_ANALYZER_NUM_BANDS; band++)
    out->band_db[band] = -120.0f;

  // ---- spectrum ----
  if (!ensure_fftw(num_frames)){
    out->valid = false; // do not report an empty spectrum as a measurement
    return false;
  }

  // Hann window (periodic), then the r2c transform.
  for (int i = 0; i < num_frames; i++){
    const float w = 0.5f - 0.5f * cosf(2.0f * (float)M_PI * (float)i / (float)num_frames);
    g_fftw.windowed[i] = mono[i] * w;
  }
  fftwf_execute(g_fftw.plan);

  // A full-scale sine (amplitude 1) puts |X| = N/4 in its bin with this
  // window, so dividing by (N/4)^2 makes a full-scale band 0 dB. The extra
  // 1/1.5 removes the Hann window's energy spread (ENBW = 1.5), so a
  // -6 dB tone measures -6 dB in its band instead of -4.
  const double normalisation = (1.0 / 1.5) / (((double)num_frames * 0.25) * ((double)num_frames * 0.25));
  const double bin_width = (double)sample_rate / (double)num_frames;

  for (int band = 0; band < AUDIO_ANALYZER_NUM_BANDS; band++){
    const AudioAnalyzerBand edges = AUDIO_ANALYZER_band(band);
    int first_bin = (int)ceil(edges.low_hz / bin_width);
    int last_bin = (int)floor(edges.high_hz / bin_width);
    if (first_bin < 0)
      first_bin = 0;
    if (last_bin > g_fftw.num_bins - 1)
      last_bin = g_fftw.num_bins - 1;

    double power = 0.0;
    for (int bin = first_bin; bin <= last_bin; bin++){
      const double re = g_fftw.spectrum[bin][0];
      const double im = g_fftw.spectrum[bin][1];
      power += re * re + im * im;
    }

    if (last_bin >= first_bin)
      out->band_db[band] = to_db((float)(power * normalisation));
  }

  return true;
}
