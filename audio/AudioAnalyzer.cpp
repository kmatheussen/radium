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

// Coarse analysis for the LLM effect tuner: levels, BS.1770 loudness,
// inter-sample peak, stereo width, spectral shape (centroid/flatness), mains
// hum/DC, and transient figures.
//
// FFTW3 (single precision, libfftw3f) is linked normally; the build script
// passes `pkg-config --cflags fftw3f` when compiling this file and
// `pkg-config --libs fftw3f` when linking Radium.
//
// The output is deliberately coarse (10 bands, ~12 Hz resolution with 4096
// samples). It is meant to give the model a few measurable facts instead of
// none. Everything here runs on the main thread only (a plan and its buffers
// are reused between calls), once per prompt.

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <fftw3.h>

#include "AudioAnalyzer.h"

namespace {
  // ---- FFT scratch (reused between calls) ----
  struct {
    int num_frames = 0;
    int num_bins = 0;
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

    g_fftw.windowed = (float*)fftwf_malloc(sizeof(float) * num_frames);
    g_fftw.num_bins = num_frames / 2 + 1;
    g_fftw.spectrum = (fftwf_complex*)fftwf_malloc(sizeof(fftwf_complex) * g_fftw.num_bins);

    if (g_fftw.windowed == NULL || g_fftw.spectrum == NULL)
      return false;

    // FFTW_ESTIMATE: no measurement pass, so planning from the GUI is fast and
    // does not disturb the audio thread.
    g_fftw.plan = fftwf_plan_dft_r2c_1d(num_frames, g_fftw.windowed, g_fftw.spectrum, FFTW_ESTIMATE);
    if (g_fftw.plan == NULL)
      return false;

    g_fftw.num_frames = num_frames;
    return true;
  }

  float to_db(float power)
  {
    // -120 dB for silence, so the prompt never contains -inf.
    if (!(power > 0.0f))
      return -120.0f;

    return fmaxf(-120.0f, 10.0f * log10f(power));
  }

  float power_to_db(double power)
  {
    if (!(power > 0.0))
      return -120.0f;

    return (float)fmax(-120.0, 10.0 * log10(power));
  }

  // ---- BS.1770 pre-filter (K-weighting), RBJ biquads at 48 kHz ----
  // Two stages: a high shelf (+4 dB above ~1.7 kHz) and a highpass (~38 Hz).
  struct Biquad { double b0, b1, b2, a1, a2; };

  Biquad make_high_shelf(double sample_rate)
  {
    // Coefficients from the BS.1770 specification, computed for the rate.
    const double f0 = 1681.974450955533;
    const double G = 3.999843853973347;
    const double Q = 0.7071752369554196;

    const double K = tan(M_PI * f0 / sample_rate);
    const double Vh = pow(10.0, G / 20.0);
    const double Vb = pow(Vh, 0.4996667741545416);
    const double a0 = 1.0 + K / Q + K * K;

    Biquad bq;
    bq.b0 = (Vh + Vb * K / Q + K * K) / a0;
    bq.b1 = 2.0 * (K * K - Vh) / a0;
    bq.b2 = (Vh - Vb * K / Q + K * K) / a0;
    bq.a1 = 2.0 * (K * K - 1.0) / a0;
    bq.a2 = (1.0 - K / Q + K * K) / a0;
    return bq;
  }

  Biquad make_highpass(double sample_rate)
  {
    const double f0 = 38.13547087602444;
    const double Q = 0.5003270373238773;
    const double K = tan(M_PI * f0 / sample_rate);

    Biquad bq;
    const double a0 = 1.0 + K / Q + K * K;
    bq.b0 = 1.0;
    bq.b1 = -2.0;
    bq.b2 = 1.0;
    bq.a1 = 2.0 * (K * K - 1.0) / a0;
    bq.a2 = (1.0 - K / Q + K * K) / a0;
    bq.b0 = bq.b1 = bq.b2 = 0.0; // replaced below
    bq.b0 = 1.0 / a0;
    bq.b1 = -2.0 / a0;
    bq.b2 = 1.0 / a0;
    return bq;
  }

  double biquad_run(const Biquad &bq, double *z1, double *z2, double x)
  {
    const double y = bq.b0 * x + *z1;
    *z1 = bq.b1 * x - bq.a1 * y + *z2;
    *z2 = bq.b2 * x - bq.a2 * y;
    return y;
  }

  // BS.1770 loudness, computed once for the whole block: the K-weighted
  // per-sample power is built first, then read out as
  //   - momentary: the last 400 ms,
  //   - loudest:   the maximum over all 400 ms windows (the level of the SOUND;
  //                a plain window average drops when the window contains the
  //                silence after a decaying note, which is exactly what made a
  //                level request overshoot by 25 dB),
  //   - short-term: the whole captured window.
  struct LoudnessResult {
    float momentary = -120.0f;
    float loudest = -120.0f;
    float short_term = -120.0f;
  };

  double loudness_from_power(double weighted_power_sum, int num_samples)
  {
    if (num_samples <= 0)
      return -120.0;

    const double mean_square = weighted_power_sum / num_samples;
    if (!(mean_square > 0.0))
      return -120.0;

    return fmax(-120.0, -0.691 + 10.0 * log10(mean_square));
  }

  LoudnessResult analyze_loudness(const float *left, const float *right, int num_frames, int sample_rate)
  {
    LoudnessResult result;
    if (num_frames <= 0 || sample_rate <= 0)
      return result;

    // K-weighted per-sample power (both channels summed, as BS.1770 does).
    double *weighted = (double*)malloc(sizeof(double) * num_frames);
    if (weighted == NULL)
      return result;

    const Biquad shelf = make_high_shelf(sample_rate);
    const Biquad highpass = make_highpass(sample_rate);
    const int num_channels = right != NULL ? 2 : 1;

    for (int ch = 0; ch < num_channels; ch++){
      const float *samples = ch == 0 ? left : right;
      double z1a = 0.0, z2a = 0.0, z1b = 0.0, z2b = 0.0;

      for (int i = 0; i < num_frames; i++){
        double x = biquad_run(shelf, &z1a, &z2a, samples[i]);
        x = biquad_run(highpass, &z1b, &z2b, x);
        const double power = x * x;
        if (ch == 0)
          weighted[i] = power;
        else
          weighted[i] += power;
      }
    }

    // Prefix sums, so every window is O(1).
    double *prefix = (double*)malloc(sizeof(double) * (num_frames + 1));
    if (prefix == NULL){
      free(weighted);
      return result;
    }
    prefix[0] = 0.0;
    for (int i = 0; i < num_frames; i++)
      prefix[i + 1] = prefix[i] + weighted[i];

    const int window = sample_rate * 400 / 1000;      // BS.1770 momentary
    const int hop = sample_rate / 10;                 // 100 ms

    result.short_term = (float)loudness_from_power(prefix[num_frames], num_frames);

    if (num_frames >= window && window > 0){
      result.momentary = (float)loudness_from_power(prefix[num_frames] - prefix[num_frames - window], window);

      double best = -1.0;
      for (int start = 0; start + window <= num_frames; start += hop){
        const double power = prefix[start + window] - prefix[start];
        if (power > best)
          best = power;
      }
      if (best >= 0.0)
        result.loudest = (float)loudness_from_power(best, window);
    }else{
      result.momentary = result.short_term;
      result.loudest = result.short_term;
    }

    free(prefix);
    free(weighted);
    return result;
  }

  // Inter-sample peak estimate. The sample peak is refined per local maximum
  // by fitting a parabola through the magnitudes of the three samples around
  // it, which recovers most of the inter-sample peak of a band-limited signal
  // (a full 4x oversampled FIR would be better for material near Nyquist, but
  // costs far more than this figure is worth).
  float true_peak_db(const float *samples, int num_frames)
  {
    float best = 0.0f;

    for (int i = 1; i < num_frames - 1; i++){
      const float prev = fabsf(samples[i - 1]);
      const float here = fabsf(samples[i]);
      const float next = fabsf(samples[i + 1]);

      if (here < prev || here < next)
        continue; // not a local maximum

      // Parabola through (-1, prev), (0, here), (1, next); its vertex may lie
      // above 'here' by up to a quarter of the curvature.
      const float denominator = prev - 2.0f * here + next;
      float peak = here;
      if (denominator < 0.0f)
        peak = here - (next - prev) * (next - prev) / (8.0f * denominator);

      if (peak > best)
        best = peak;
    }

    return to_db(best * best);
  }

  // Envelope follower + attack/onset detection, in one pass.
  struct TransientResult {
    float attack_db_per_ms = 0.0f;
    float onsets_per_second = 0.0f;
  };

  TransientResult analyze_transients(const float *left, const float *right, int num_frames, int sample_rate)
  {
    TransientResult result;
    if (num_frames <= 0 || sample_rate <= 0)
      return result;

    // Windowed RMS, in 10 ms windows with 5 ms hops. An envelope follower is
    // not used: even a fast one tracks the waveform of a low note, which reads
    // as a large fake "attack". A 10 ms window holds several cycles of
    // anything above ~200 Hz, so its level only follows real dynamics.
    const int win = sample_rate / 100;         // 10 ms
    const int hop = win / 2;                   // 5 ms
    if (win < 8 || num_frames < win + hop)
      return result;

    const double hop_ms = 1000.0 * hop / sample_rate;

    // Running sum of squares for the current window.
    double window_sum = 0.0;
    for (int i = 0; i < win; i++){
      const double l = left[i];
      const double r = right != NULL ? right[i] : l;
      const double x = 0.5 * (l + r);
      window_sum += x * x;
    }

    double previous_mean_square = window_sum / win;
    double last_onset_ms = -1.0e9;
    int num_onsets = 0;
    double steepest_db_per_ms = 0.0;

    for (int end = win + hop; end <= num_frames; end += hop){
      // Slide the window by 'hop' samples.
      for (int i = end - hop; i < end; i++){
        const double l = left[i];
        const double r = right != NULL ? right[i] : l;
        const double x = 0.5 * (l + r);
        window_sum += x * x;
      }
      for (int i = end - win - hop; i < end - win; i++){
        const double l = left[i];
        const double r = right != NULL ? right[i] : l;
        const double x = 0.5 * (l + r);
        window_sum -= x * x;
      }
      if (window_sum < 0.0)
        window_sum = 0.0;

      const double mean_square = window_sum / win;

      // Floor, so a jump out of digital silence is measurable.
      const double floor_ms_value = 1.0e-10;
      const double current = mean_square > floor_ms_value ? mean_square : floor_ms_value;
      const double previous = previous_mean_square > floor_ms_value ? previous_mean_square : floor_ms_value;
      const double rise_db = 10.0 * log10(current / previous);
      const double time_ms = 1000.0 * end / sample_rate;

      // The attack rate describes the material's own dynamics, so only rises
      // that start from an already audible level count: a fade-in from silence,
      // or a single click in silence, is reported by onsets/s instead.
      if (rise_db > 0.0 && previous > 3.0e-5){ // -45 dBFS
        const double db_per_ms = rise_db / hop_ms;
        if (db_per_ms > steepest_db_per_ms)
          steepest_db_per_ms = db_per_ms;
      }

      // An onset is a clear level jump, at most one per 50 ms.
      if (rise_db >= 6.0 && time_ms - last_onset_ms > 50.0){
        num_onsets++;
        last_onset_ms = time_ms;
      }

      previous_mean_square = mean_square;
    }

    result.attack_db_per_ms = (float)fmin(1000.0, fmax(0.0, steepest_db_per_ms));

    const double seconds = (double)num_frames / (double)sample_rate;
    result.onsets_per_second = seconds > 0.0 ? (float)(num_onsets / seconds) : 0.0f;

    return result;
  }

  // Energy decay after a peak: an RT60-like decay time and the level 300 ms
  // after the peak relative to it (a direct-to-reverberant-ish figure that is
  // easy to interpret: near 0 dB = long tail, -30 dB = dry).
  //
  // The envelope is a 10 ms RMS hop sequence over the whole captured window.
  void analyze_decay(const float *left, const float *right, int num_frames, int sample_rate,
                     AudioAnalysis *out)
  {
    const int hop = sample_rate / 100; // 10 ms
    if (hop < 8)
      return;

    const int num_hops = num_frames / hop;
    if (num_hops < 40) // need at least 400 ms
      return;

    // 10 ms RMS levels, in dB.
    float *levels = (float*)calloc(num_hops, sizeof(float));
    if (levels == NULL)
      return;

    double window_sum = 0.0;
    for (int h = 0; h < num_hops; h++){
      const int start = h * hop;
      for (int i = start; i < start + hop; i++){
        const double l = left[i];
        const double r = right != NULL ? right[i] : l;
        const double x = 0.5 * (l + r);
        window_sum += x * x;
      }
      const double mean_square = window_sum / hop;
      levels[h] = mean_square > 0.0 ? (float)(10.0 * log10(mean_square)) : -120.0f;
      window_sum = 0.0;
    }

    // The loudest hop, and the median level (as a "is there a real peak" test).
    int peak_hop = 0;
    for (int h = 1; h < num_hops; h++)
      if (levels[h] > levels[peak_hop])
        peak_hop = h;

    float *sorted = (float*)calloc(num_hops, sizeof(float));
    if (sorted == NULL){
      free(levels);
      return;
    }
    memcpy(sorted, levels, sizeof(float) * num_hops);
    // Insertion sort is fine for a few hundred values.
    for (int i = 1; i < num_hops; i++){
      const float value = sorted[i];
      int j = i - 1;
      while (j >= 0 && sorted[j] > value){
        sorted[j + 1] = sorted[j];
        j--;
      }
      sorted[j + 1] = value;
    }
    const float median = sorted[num_hops / 2];
    free(sorted);

    const float peak_level = levels[peak_hop];

    // A peak has to stand out from the material's typical level, and there has
    // to be some decay left inside the window.
    if (peak_level < -70.0f || peak_level - median < 6.0f || peak_hop >= num_hops - 10){
      free(levels);
      return;
    }

    // ---- tail: level 300 ms after the peak ----
    const int tail_hop = peak_hop + 30;
    if (tail_hop < num_hops && levels[tail_hop] > -120.0f)
      out->tail_db = levels[tail_hop] - peak_level;

    // ---- RT60 from the decay between -5 dB and -25 dB below the peak ----
    int first_hop = -1;
    int last_hop = -1;
    for (int h = peak_hop; h < num_hops; h++){
      const double relative = levels[h] - peak_level;
      if (relative <= -5.0 && first_hop < 0)
        first_hop = h;
      if (relative <= -25.0){
        last_hop = h;
        break;
      }
    }

    if (first_hop > 0 && last_hop > first_hop + 4){
      // Least-squares slope of dB over time, over the decay segment.
      const double hop_s = (double)hop / (double)sample_rate;
      double sum_x = 0.0, sum_y = 0.0, sum_xx = 0.0, sum_xy = 0.0;
      int count = 0;
      for (int h = first_hop; h <= last_hop; h++){
        const double x = (h - first_hop) * hop_s;
        const double y = levels[h];
        sum_x += x;
        sum_y += y;
        sum_xx += x * x;
        sum_xy += x * y;
        count++;
      }

      const double denominator = count * sum_xx - sum_x * sum_x;
      if (count >= 5 && denominator > 0.0){
        const double slope = (count * sum_xy - sum_x * sum_y) / denominator; // dB per second
        // -0.5 dB/s is a very long tail, -400 dB/s a very short one (RT60 of
        // 0.15 s); anything outside is a mis-detected segment.
        if (slope < -0.5 && slope > -400.0){
          const double rt60 = 60.0 / -slope;
          if (rt60 > 0.02 && rt60 < 20.0){
            // A window of 'num_frames' cannot certify a longer decay than its
            // own length: the fit then sits in the noise tail. Report the
            // window length instead, and let the prompt say "longer than the
            // window" (see FxInstrumentMeasurement::decay_time_s).
            const double window_s = (double)num_frames / (double)sample_rate;
            out->decay_time_s = (float)fmin(rt60, window_s);
          }
        }
      }
    }

    free(levels);
  }

  // Finds a mains-hum peak (50 or 60 Hz, or its second harmonic) that stands
  // out from its neighbourhood, and the energy below 30 Hz.
  void analyze_hum_and_sub(const float *spectrum_values, int num_bins, double bin_width,
                           double normalisation, AudioAnalysis *out)
  {
    const double hum_targets[4] = { 50.0, 60.0, 100.0, 120.0 };

    double best_excess = 0.0;
    double best_level = -120.0;
    double best_freq = 0.0;

    for (int t = 0; t < 4; t++){
      const int bin = (int)lround(hum_targets[t] / bin_width);
      if (bin < 2 || bin >= num_bins - 2)
        continue;

      const double peak = spectrum_values[2 * bin] * spectrum_values[2 * bin]
                        + spectrum_values[2 * bin + 1] * spectrum_values[2 * bin + 1];

      // Neighbourhood: a few bins either side, excluding the peak itself.
      double neighbours = 0.0;
      int count = 0;
      for (int d = 3; d <= 8; d++){
        for (int side = -1; side <= 1; side += 2){
          const int b = bin + side * d;
          if (b < 0 || b >= num_bins)
            continue;
          neighbours += spectrum_values[2 * b] * spectrum_values[2 * b]
                      + spectrum_values[2 * b + 1] * spectrum_values[2 * b + 1];
          count++;
        }
      }
      if (count == 0 || !(neighbours > 0.0) || !(peak > 0.0))
        continue;

      const double excess = 10.0 * log10(peak / (neighbours / count));
      if (excess > best_excess){
        best_excess = excess;
        best_level = 10.0 * log10(peak * normalisation);
        best_freq = hum_targets[t];
      }
    }

    // Only report a peak that clearly stands out (a narrow, ~20 dB spike) and
    // is loud enough to be worth mentioning. A musical tone at 50 Hz, or the
    // random peaks of white noise, are not hum.
    if (best_excess > 12.0 && best_level > -55.0){
      out->hum_db = (float)fmax(-120.0, best_level);
      out->hum_hz = (float)best_freq;
    }

    int sub_bins = (int)(30.0 / bin_width);
    if (sub_bins < 1)
      sub_bins = 1;
    double sub_power = 0.0;
    for (int bin = 1; bin < sub_bins && bin < num_bins; bin++)
      sub_power += spectrum_values[2 * bin] * spectrum_values[2 * bin]
                 + spectrum_values[2 * bin + 1] * spectrum_values[2 * bin + 1];

    out->sub_bass_db = power_to_db(sub_power * normalisation);
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

bool AUDIO_ANALYZE(const float *left, const float *right, int num_frames, int sample_rate, AudioAnalysis *out)
{
  if (out == NULL)
    return false;

  memset(out, 0, sizeof(AudioAnalysis));

  if (left == NULL || num_frames < 512 || sample_rate <= 0)
    return false;

  const bool is_stereo = right != NULL;

  out->valid = true;
  out->num_frames = num_frames;
  out->sample_rate = sample_rate;

  // ---- time-domain: levels, crest, DC, stereo, transients ----
  double sum_squares = 0.0;
  double left_squares = 0.0;
  double right_squares = 0.0;
  double cross = 0.0;
  double mid_squares = 0.0;
  double side_squares = 0.0;
  double dc_sum = 0.0;
  float peak = 0.0f;

  for (int i = 0; i < num_frames; i++){
    const float l = left[i];
    const float r = is_stereo ? right[i] : l;

    const double mono = is_stereo ? 0.5 * (l + r) : l;
    const double mid = 0.5 * ((double)l + (double)r);
    const double side = 0.5 * ((double)l - (double)r);

    sum_squares += mono * mono;
    left_squares += (double)l * (double)l;
    right_squares += (double)r * (double)r;
    cross += (double)l * (double)r;
    mid_squares += mid * mid;
    side_squares += side * side;
    dc_sum += mid;

    const float abs_sample = fabsf(l) > fabsf(r) ? fabsf(l) : fabsf(r);
    if (abs_sample > peak)
      peak = abs_sample;
  }

  const float rms = (float)sqrt(sum_squares / (double)num_frames);

  out->peak_db = to_db(peak * peak);
  out->rms_db = to_db(rms * rms);
  out->crest_db = out->peak_db - out->rms_db;
  out->true_peak_db = true_peak_db(left, num_frames);
  if (is_stereo){
    const float right_true_peak = true_peak_db(right, num_frames);
    if (right_true_peak > out->true_peak_db)
      out->true_peak_db = right_true_peak;
  }

  out->dc_offset = (float)(dc_sum / (double)num_frames);

  // Stereo figures. Both levels are needed to avoid dividing by zero.
  if (is_stereo && left_squares > 0.0 && right_squares > 0.0){
    out->stereo_correlation = (float)(cross / sqrt(left_squares * right_squares));
    out->stereo_side_db = power_to_db(side_squares) - power_to_db(mid_squares);
    out->stereo_balance_db = power_to_db(right_squares) - power_to_db(left_squares);
  }else if (!is_stereo){
    out->stereo_correlation = 1.0f; // a mono signal is perfectly correlated
    out->stereo_side_db = -120.0f;
  }

  const LoudnessResult loudness = analyze_loudness(left, is_stereo ? right : NULL, num_frames, sample_rate);
  out->loudness_momentary_lufs = loudness.momentary;
  out->loudness_loudest_lufs = loudness.loudest;
  out->loudness_window_lufs = loudness.short_term;

  const TransientResult transients = analyze_transients(left, is_stereo ? right : NULL, num_frames, sample_rate);
  analyze_decay(left, is_stereo ? right : NULL, num_frames, sample_rate, out);
  out->attack_db_per_ms = transients.attack_db_per_ms;
  out->onsets_per_second = transients.onsets_per_second;

  for (int band = 0; band < AUDIO_ANALYZER_NUM_BANDS; band++)
    out->band_db[band] = -120.0f;
  out->hum_db = -120.0f;
  out->sub_bass_db = -120.0f;

  // ---- spectrum (mono mix, 4096 samples from the last part of the block) ----
  const int fft_frames = num_frames >= 4096 ? 4096 : (num_frames & ~1);
  if (fft_frames < 512 || !ensure_fftw(fft_frames)){
    out->valid = false; // do not report an empty spectrum as a measurement
    return false;
  }

  // Pick the loudest window: the last 85 ms can be silence (a gap between
  // notes), and then every spectral figure would be a lie.
  int fft_offset = num_frames - fft_frames;
  {
    const int step = fft_frames / 2;
    double best_energy = -1.0;
    for (int start = 0; start + fft_frames <= num_frames; start += step){
      double energy = 0.0;
      for (int i = 0; i < fft_frames; i += 4){ // subsample: cheaper, same choice
        const int index = start + i;
        const double mono = is_stereo ? 0.5 * ((double)left[index] + (double)right[index]) : left[index];
        energy += mono * mono;
      }
      if (energy > best_energy){
        best_energy = energy;
        fft_offset = start;
      }
    }
  }

  for (int i = 0; i < fft_frames; i++){
    const int index = fft_offset + i;
    const double mono = is_stereo ? 0.5 * ((double)left[index] + (double)right[index]) : left[index];
    const float w = 0.5f - 0.5f * cosf(2.0f * (float)M_PI * (float)i / (float)fft_frames);
    g_fftw.windowed[i] = (float)(mono * w);
  }
  fftwf_execute(g_fftw.plan);

  // A full-scale sine (amplitude 1) puts |X| = N/4 in its bin with this
  // window, so dividing by (N/4)^2 makes a full-scale band 0 dB. The extra
  // 1/1.5 removes the Hann window's energy spread (ENBW = 1.5), so a
  // -6 dB tone measures -6 dB in its band instead of -4.
  const double normalisation = (1.0 / 1.5) / (((double)fft_frames * 0.25) * ((double)fft_frames * 0.25));
  const double bin_width = (double)sample_rate / (double)fft_frames;

  double total_power = 0.0;
  double weighted_frequency = 0.0;
  double log_power_sum = 0.0;
  int counted_bins = 0;

  const float *spectrum_values = (const float*)g_fftw.spectrum;
  for (int bin = 1; bin < g_fftw.num_bins; bin++){
    const double re = g_fftw.spectrum[bin][0];
    const double im = g_fftw.spectrum[bin][1];
    const double power = re * re + im * im;
    if (!(power > 0.0))
      continue;

    const double frequency = bin * bin_width;
    if (frequency < 25.0 || frequency > 20000.0)
      continue;

    total_power += power;
    weighted_frequency += frequency * power;
    log_power_sum += log(power);
    counted_bins++;
  }

  if (total_power > 0.0){
    out->centroid_hz = (float)(weighted_frequency / total_power);
    if (counted_bins > 0){
      const double geometric = exp(log_power_sum / counted_bins);
      const double arithmetic = total_power / counted_bins;
      out->flatness = (float)fmin(1.0, geometric / arithmetic);
    }
  }

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
      out->band_db[band] = power_to_db(power * normalisation);
  }

  analyze_hum_and_sub(spectrum_values, g_fftw.num_bins, bin_width, normalisation, out);

  return true;
}
