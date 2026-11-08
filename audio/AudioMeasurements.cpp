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

// Capture of one instrument's output for the LLM effect tuner's measurements.
//
// Audio thread: MEASUREMENTS_RT_capture() is called for every SoundProducer,
// every block, and does almost nothing:
//
//   - nothing selected (the normal state: no tuning terminal on screen):
//     one relaxed atomic load, then return.
//   - another instrument selected: two atomic loads, then return.
//   - the selected instrument: a mono mixdown into a fixed buffer (3 flops per
//     frame) and, every 4096 frames (~85 ms at 48 kHz), one 16 KB memcpy to
//     hand the block over. No allocation, no locking, no clock reads.
//
// The host selects an instrument only while that instrument's tuning terminal
// is visible (LLM_fx_tune_widget::showEvent/hideEvent), so in normal use the
// audio thread does nothing at all.
//
// Main thread: MEASUREMENTS_get() copies the most recent complete block and
// runs the FFT analysis (AudioAnalyzer.cpp). The block is tied to the
// instrument it came from, and its age is reported, because measurements taken
// before a parameter change no longer describe the sound.
//
// The selected instrument is identified by BOTH its SoundProducer and its
// SoundPlugin: the tuner looks the producer up on the main thread, while the
// mixer processes the producer object it holds itself. Matching either pointer
// means a difference between the two cannot silently disable the measurements.
//
// Run with RADIUM_DEBUG_MEASUREMENTS=1 to trace the whole path (see
// debug_measurements()).

#include <atomic>
#include <chrono>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <thread>

#include "AudioMeasurements.h"
#include "AudioAnalyzer.h"

// ~85 ms at 48 kHz: enough for ~12 Hz resolution (the 30-60 Hz band needs it),
// short enough that a change made by the user is reflected on the next prompt.
#define CAPTURE_FRAMES 4096

// Opt-in tracing (RADIUM_DEBUG_MEASUREMENTS=1): which instrument is selected,
// how much audio was captured, and why a read fails.
static bool debug_measurements(void)
{
  static int enabled = -1;
  if (enabled < 0)
    enabled = getenv("RADIUM_DEBUG_MEASUREMENTS") != NULL ? 1 : 0;
  return enabled == 1;
}

namespace {
  std::atomic<void*> g_target_sound_producer{NULL};
  std::atomic<void*> g_target_sound_plugin{NULL};

  // Fast path for the audio thread: false (the normal state) means "nobody is
  // looking at the tuner", so the per-block check is one relaxed load and the
  // capture is skipped entirely.
  std::atomic<bool> g_capture_enabled{false};

  // Written by the audio thread, read by the main thread.
  float g_write_buffer[CAPTURE_FRAMES];
  int g_write_pos = 0;

  float g_ready_buffer[CAPTURE_FRAMES];
  std::atomic<int> g_ready_version{0}; // bumped after each completed block
  int g_ready_frames = 0;
  void *g_ready_producer = NULL;
  void *g_ready_plugin = NULL;

  // Diagnostics: what the audio thread saw for the current target.
  std::atomic<long long> g_capture_calls{0};
  std::atomic<long long> g_captured_blocks{0};

  // Non-zero while the audio thread writes, so the main thread can retry
  // instead of reading a half-copied block. (A seqlock.)
  std::atomic<int> g_sequence{0};

  // Main thread only: when the block that is currently published was first
  // seen. (No clock is ever read from the audio thread.)
  int g_age_version = -1;
  std::chrono::steady_clock::time_point g_age_time;

  void publish_block(void *sound_producer, void *sound_plugin, int num_frames)
  {
    g_sequence.fetch_add(1, std::memory_order_release); // odd: writing
    memcpy(g_ready_buffer, g_write_buffer, sizeof(float) * num_frames);
    g_ready_frames = num_frames;
    g_ready_producer = sound_producer;
    g_ready_plugin = sound_plugin;
    g_sequence.fetch_add(1, std::memory_order_release); // even: done
    g_ready_version.fetch_add(1, std::memory_order_release);
  }

  // True when this is the selected instrument (producer pointer, or the plugin
  // pointer as a fallback; see the comment at the top of this file).
  bool is_target(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin)
  {
    if (sound_producer != NULL
        && sound_producer == (struct SoundProducer*)g_target_sound_producer.load(std::memory_order_acquire))
      return true;

    return sound_plugin != NULL
        && sound_plugin == (struct SoundPlugin*)g_target_sound_plugin.load(std::memory_order_acquire);
  }

  bool ready_block_belongs_to(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin)
  {
    return g_ready_version.load(std::memory_order_acquire) > 0
        && is_target((struct SoundProducer*)g_ready_producer, (struct SoundPlugin*)g_ready_plugin);
  }
}

void MEASUREMENTS_set_target(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin)
{
  static struct SoundProducer *last_sound_producer = NULL;
  static struct SoundPlugin *last_sound_plugin = NULL;

  if (sound_producer == last_sound_producer && sound_plugin == last_sound_plugin){
    if (debug_measurements())
      printf("measurements: set_target(%p/%p) unchanged\n", (void*)sound_producer, (void*)sound_plugin);
    return;
  }

  if (debug_measurements())
    printf("measurements: set_target(%p/%p), was %p/%p\n",
           (void*)sound_producer, (void*)sound_plugin,
           (void*)last_sound_producer, (void*)last_sound_plugin);

  last_sound_producer = sound_producer;
  last_sound_plugin = sound_plugin;

  g_write_pos = 0; // a different instrument: start a fresh block

  // The published block belongs to the previous instrument; discard it, so a
  // prompt for the new one cannot report the old one's sound.
  g_ready_version.store(0, std::memory_order_release);
  g_ready_producer = NULL;
  g_ready_plugin = NULL;

  g_capture_calls.store(0, std::memory_order_relaxed);
  g_captured_blocks.store(0, std::memory_order_relaxed);

  g_capture_enabled.store(sound_producer != NULL || sound_plugin != NULL, std::memory_order_release);
  g_target_sound_plugin.store(sound_plugin, std::memory_order_release);
  g_target_sound_producer.store(sound_producer, std::memory_order_release);
}

void MEASUREMENTS_clear_target(void)
{
  MEASUREMENTS_set_target(NULL, NULL);
}

void MEASUREMENTS_RT_capture(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                             float **sounds, int num_channels, int num_frames)
{
  // One relaxed load in the normal case (no tuning terminal on screen).
  if (!g_capture_enabled.load(std::memory_order_relaxed))
    return;

  if (!is_target(sound_producer, sound_plugin)){
    if (debug_measurements()){
      static int mismatches = 0;
      if (mismatches < 5){
        mismatches++;
        printf("measurements: RT_capture(%p/%p) is not the target (%p/%p), ignored\n",
               (void*)sound_producer, (void*)sound_plugin,
               g_target_sound_producer.load(std::memory_order_relaxed),
               g_target_sound_plugin.load(std::memory_order_relaxed));
      }
    }
    return;
  }

  g_capture_calls.fetch_add(1, std::memory_order_relaxed);

  if (sounds == NULL || num_channels < 1 || num_frames < 1){
    if (debug_measurements())
      printf("measurements: RT_capture(%p): nothing usable (sounds=%p ch=%d frames=%d)\n",
             (void*)sound_producer, (void*)sounds, num_channels, num_frames);
    return;
  }

  // Mix down to mono; the model reasons about the instrument as a whole.
  const float *left = sounds[0];
  const float *right = num_channels > 1 ? sounds[1] : NULL;
  if (left == NULL)
    return;

  for (int i = 0; i < num_frames; i++){
    const float sample = right != NULL ? 0.5f * (left[i] + right[i]) : left[i];
    g_write_buffer[g_write_pos++] = sample;

    if (g_write_pos == CAPTURE_FRAMES){
      publish_block(sound_producer, sound_plugin, CAPTURE_FRAMES);
      const long long num_blocks = g_captured_blocks.fetch_add(1, std::memory_order_relaxed) + 1;
      if (debug_measurements() && (num_blocks <= 3 || num_blocks % 20 == 0))
        printf("measurements: published block %lld (%p/%p)\n", num_blocks, (void*)sound_producer, (void*)sound_plugin);
      g_write_pos = 0;
    }
  }
}

static bool read_block(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                       float *block, int *num_frames_out)
{
  if (block == NULL || num_frames_out == NULL)
    return false;

  if (!is_target(sound_producer, sound_plugin))
    return false;

  int num_frames = 0;
  for (int attempt = 0; attempt < 4; attempt++){
    const int sequence_before = g_sequence.load(std::memory_order_acquire);
    if ((sequence_before & 1) != 0)
      continue; // the audio thread is copying right now

    if (!ready_block_belongs_to(sound_producer, sound_plugin))
      return false; // nothing captured yet, or captured for another instrument

    num_frames = g_ready_frames;
    if (num_frames <= 0 || num_frames > CAPTURE_FRAMES)
      return false;
    memcpy(block, g_ready_buffer, sizeof(float) * num_frames);

    if (g_sequence.load(std::memory_order_acquire) == sequence_before)
      break; // consistent

    num_frames = 0;
  }

  if (num_frames <= 0)
    return false;

  *num_frames_out = num_frames;
  return true;
}

long long MEASUREMENTS_get_num_capture_calls(void)
{
  return g_capture_calls.load(std::memory_order_relaxed);
}

long long MEASUREMENTS_get_num_captured_blocks(void)
{
  return g_captured_blocks.load(std::memory_order_relaxed);
}

long long MEASUREMENTS_get_age_ms(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin)
{
  if (!ready_block_belongs_to(sound_producer, sound_plugin))
    return -1;

  const int version = g_ready_version.load(std::memory_order_acquire);
  if (version != g_age_version){
    g_age_version = version;
    g_age_time = std::chrono::steady_clock::now();
    return 0;
  }

  const auto age = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - g_age_time);
  return (long long)age.count();
}

bool MEASUREMENTS_wait_for_block(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin, int timeout_ms)
{
  const auto start = std::chrono::steady_clock::now();
  while (true){
    if (ready_block_belongs_to(sound_producer, sound_plugin))
      return true;

    const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start).count();
    if (elapsed >= timeout_ms)
      return false;

    std::this_thread::sleep_for(std::chrono::milliseconds(5));
  }
}

MeasurementsStatus MEASUREMENTS_get_status(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                                           int sample_rate, AudioAnalysis *out)
{
  const bool debug = debug_measurements();

  if (sound_producer == NULL && sound_plugin == NULL){
    if (debug) printf("measurements: get_status: no instrument given\n");
    return MEASUREMENTS_STATUS_NO_PRODUCER;
  }

  if (!is_target(sound_producer, sound_plugin)){
    if (debug) printf("measurements: get_status(%p/%p): not the target (%p/%p)\n",
                      (void*)sound_producer, (void*)sound_plugin,
                      g_target_sound_producer.load(std::memory_order_relaxed),
                      g_target_sound_plugin.load(std::memory_order_relaxed));
    return MEASUREMENTS_STATUS_NOT_TARGET;
  }

  float block[CAPTURE_FRAMES];
  int num_frames = 0;
  const bool have_block = read_block(sound_producer, sound_plugin, block, &num_frames);

  if (debug)
    printf("measurements: get_status(%p/%p): calls=%lld blocks=%lld ready=%p/%p version=%d have_block=%d frames=%d\n",
           (void*)sound_producer, (void*)sound_plugin,
           (long long)g_capture_calls.load(std::memory_order_relaxed),
           (long long)g_captured_blocks.load(std::memory_order_relaxed),
           g_ready_producer, g_ready_plugin,
           (int)g_ready_version.load(std::memory_order_relaxed),
           (int)have_block, num_frames);

  if (!have_block)
    return g_capture_calls.load(std::memory_order_relaxed) == 0
      ? MEASUREMENTS_STATUS_NEVER_CAPTURED
      : MEASUREMENTS_STATUS_SILENT; // captured, but no complete block yet

  // Silence means the instrument is not audible right now (stopped, muted, or
  // no notes): reporting a flat -120 dB spectrum would only mislead the model.
  double sum = 0.0;
  for (int i = 0; i < num_frames; i++)
    sum += fabs((double)block[i]);
  if (sum / (double)num_frames < 1e-5){
    if (debug) printf("measurements: get_status: block is silent (mean %.9f)\n", sum / (double)num_frames);
    return MEASUREMENTS_STATUS_SILENT;
  }

  if (!AUDIO_ANALYZE(block, num_frames, sample_rate, out)){
    if (debug) printf("measurements: get_status: AUDIO_ANALYZE failed (libfftw3f missing?)\n");
    return MEASUREMENTS_STATUS_NO_ANALYZER;
  }

  if (debug) printf("measurements: get_status: OK (peak %.1f dBFS, %d frames)\n", out->peak_db, num_frames);
  return MEASUREMENTS_STATUS_OK;
}

bool MEASUREMENTS_get(struct SoundProducer *sound_producer, struct SoundPlugin *sound_plugin,
                      int sample_rate, AudioAnalysis *out)
{
  AudioAnalysis analysis;
  if (MEASUREMENTS_get_status(sound_producer, sound_plugin, sample_rate, &analysis) != MEASUREMENTS_STATUS_OK)
    return false;

  if (out != NULL)
    *out = analysis;

  return true;
}
