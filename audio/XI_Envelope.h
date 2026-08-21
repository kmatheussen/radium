// Shared XI envelope structures and processing.
// Included by both XI_Plugin.cpp and Sampler_plugin.cpp.
//
// Code is made by looking at the Soundtracker source code, made by Michael Krause (mainly), Yury Aliaev, and others,
// which again uses code from several others. (see AUTHORS file in the Soundtracker source code).
//

#pragma once

#ifndef XI_MAX_ENVELOPE_POINTS
#define XI_MAX_ENVELOPE_POINTS 12
#endif

// Envelope flags (from Soundtracker)
#ifndef XI_EF_ON
#define XI_EF_ON      1
#endif
#ifndef XI_EF_SUSTAIN
#define XI_EF_SUSTAIN 2
#endif
#ifndef XI_EF_LOOP
#define XI_EF_LOOP    4
#endif

// Bit 5 in panning flags: stereo sample (not supported by sample player)
#define XI_STEREO_SAMPLE 32

#ifdef __cplusplus
extern "C" {
#endif

struct XIEnvelopePoint
{
	uint16_t pos;  // time in ticks
	uint16_t val;  // value 0..64
};

struct XIEnvelope
{
	XIEnvelopePoint points[XI_MAX_ENVELOPE_POINTS];
	uint8_t num_points;
	uint8_t sustain_point;
	uint8_t loop_start;
	uint8_t loop_end;
	uint8_t flags; // XI_EF_ON | XI_EF_SUSTAIN | XI_EF_LOOP
	int16_t volfade;          // volume fadeout rate (0 = disabled, max 0xfff)
};

// Validate and clamp envelope data
static void xi_envelope_check(struct XIEnvelope *e)
{
	if (e->num_points == 0 || e->num_points > XI_MAX_ENVELOPE_POINTS)
		e->num_points = 1;

	for (int i = 0; i < e->num_points; i++)
	{
		if (e->points[i].val > 64)
			e->points[i].val = 32;
	}

	e->points[0].pos = 0;
}

static int xi_envelope_length(const struct XIEnvelope *env)
{
	return env->points[env->num_points - 1].pos;
}

static int xi_envelope_interpolate(int v1, int v2, int p1, int p, int p2)
{
	// v1, v2: 0..64.  p1, p, p2: tick positions.
	if (p2 <= p1)
		return v2;
	return (p - p1) * (v2 - v1) / (p2 - p1);
}

// Evaluate envelope gain at a given tick position without advancing *p.
// Returns gain value (0..256), which is 4 * envelope volume (0..64).
static int xi_envelope_gain_at(const struct XIEnvelope *env, int32_t tick)
{
	int i, v;
	int env_len = xi_envelope_length(env);

	if (tick > env_len)
		tick = env_len;

	for (i = env->num_points - 1; i >= 1; i--)
	{
		if (env->points[i].pos <= tick)
			break;
	}

	v = env->points[i].val;
	if (tick != env->points[i].pos)
		v += xi_envelope_interpolate(env->points[i].val, env->points[i + 1].val,
		                             env->points[i].pos, tick, env->points[i + 1].pos);

	return 4 * v; // 0..256
}

// Apply a per-tick volume envelope to a block of float samples.
// env:         the volume envelope
// env_pos:     pointer to current envelope tick position (advanced in place)
// env_sub:     pointer to sub-tick counter (0 .. tick_rate-1). One envelope tick
//              advances every tick_rate audio frames (FT2: tick_rate = samplerate * 2.5 / bpm).
// sustain:     true while the note is held (respects sustain point)
// samples:     interleaved float sample buffer (num_frames * num_channels)
// num_frames:  number of frames (mono: frames=samples; stereo: frames=samples/2)
// num_channels: 1 for mono, 2 for stereo
// gain_base:   base gain to apply before envelope (0..1, default 1.0)
// tick_rate:   number of audio frames per envelope tick (samplerate * 2.5 / bpm)
//
// Returns the number of audible frames (may be less than num_frames if the
// envelope reached permanent silence mid-buffer). The caller should treat this
// as the effective frame count for downstream processing and voice termination.
static int RT_apply_xi_envelope(const struct XIEnvelope *env,
								 int32_t *env_pos,
								 int32_t *env_sub,
								 int32_t *env_gain,
								 bool sustain,
                                 float *samples,
								 int num_frames, int num_channels,
                                 float gain_base, int tick_rate)
{
	if (!(env->flags & XI_EF_ON))
		return num_frames;

	if (tick_rate <= 0)
		tick_rate = 882; // fallback: samplerate * 2.5 / 125 bpm at 44.1 kHz

	int32_t sub = *env_sub;
	int32_t pos = *env_pos;

	for (int i = 0; i < num_frames; i++)
	{
		// Interpolate gain between current and next envelope tick
		int gain_curr = xi_envelope_gain_at(env, pos);
		int gain_next = xi_envelope_gain_at(env, pos + 1);

		// When both current and next tick gain are zero, all subsequent
		// frames will be silent — the envelope has finished and won't
		// advance further (or every future tick also evaluates to zero).
		if (gain_curr == 0 && gain_next == 0)
		{
			*env_pos = pos;
			*env_sub = sub;
			*env_gain = 0;
			return i;
		}

		int gain = gain_curr + (gain_next - gain_curr) * sub / tick_rate;

		float amp = gain_base * gain / 256.0f;

		for (int ch = 0; ch < num_channels; ch++)
			samples[i * num_channels + ch] *= amp;

		sub++;
		if (sub >= tick_rate)
		{
			sub = 0;
			// Advance envelope (respecting sustain and loop)
			int env_len = xi_envelope_length(env);
			if (pos < env_len &&
			    !(sustain && (env->flags & XI_EF_SUSTAIN) &&
			      pos == env->points[env->sustain_point].pos))
			{
				pos++;
				if ((env->flags & XI_EF_LOOP)
				    && pos == env->points[env->loop_end].pos
				    && (sustain || !(env->flags & XI_EF_SUSTAIN)))
				{
					pos = env->points[env->loop_start].pos;
				}
			}
		}
	}

	*env_pos = pos;
	*env_sub = sub;
	*env_gain = xi_envelope_gain_at(env, pos); // cached gain for non-interpolating consumers
	return num_frames;
}

// Apply pan envelope value (0..64) as per-channel gain to stereo outputs.
// pan=0 → hard left (left=1.0, right=0.0)
// pan=32 → center (left=0.5, right=0.5)
// pan=64 → hard right (left=0.0, right=1.0)
static void RT_apply_xi_pan_envelope(const struct XIEnvelope *env,
                                     int32_t *env_pos, int32_t *env_sub, int32_t *env_gain,
                                     bool sustain,
                                     float *left, float *right,
                                     int num_frames, int tick_rate)
{
	if (!(env->flags & XI_EF_ON))
		return;

	if (tick_rate <= 0)
		tick_rate = 882;

	int32_t sub = *env_sub;
	int32_t pos = *env_pos;

	for (int i = 0; i < num_frames; i++)
	{
		// Interpolate pan value between current and next envelope tick
		int pan_curr = xi_envelope_gain_at(env, pos);     // 0..256 = 4 * (0..64)
		int pan_next = xi_envelope_gain_at(env, pos + 1);
		int pan_val = pan_curr + (pan_next - pan_curr) * sub / tick_rate; // 0..256

		// Convert to per-channel gains (linear pan law)
		float right_gain = pan_val / 256.0f;       // 0..1
		float left_gain  = 1.0f - right_gain;      // 1..0

		left[i]  *= left_gain;
		right[i] *= right_gain;

		sub++;
		if (sub >= tick_rate)
		{
			sub = 0;
			int env_len = xi_envelope_length(env);
			if (pos < env_len &&
			    !(sustain && (env->flags & XI_EF_SUSTAIN) &&
			      pos == env->points[env->sustain_point].pos))
			{
				pos++;
				if ((env->flags & XI_EF_LOOP)
				    && pos == env->points[env->loop_end].pos
				    && (sustain || !(env->flags & XI_EF_SUSTAIN)))
				{
					pos = env->points[env->loop_start].pos;
				}
			}
		}
	}

	*env_pos = pos;
	*env_sub = sub;
	*env_gain = xi_envelope_gain_at(env, pos);
}

// Apply XI volume fadeout to a block of float samples. Advances the fade accumulator
// and volume, applying per-frame gain. Returns the number of audible frames (may be
// less than num_frames if the fade reached zero mid-buffer).
static int XI_volume_fadeout_apply(int volfade,
                                   int32_t *fade_vol,
                                   int32_t *fade_accum,
                                   int tick_rate,
                                   float *data,
                                   int num_frames)
{
	int audible = 0;

	for (int i = 0; i < num_frames; i++)
	{
		*fade_accum += 1;
		if (*fade_accum >= tick_rate)
		{
			*fade_accum = 0;
			*fade_vol -= volfade;
			if (*fade_vol <= 0)
			{
				*fade_vol = 0;
				data[i] *= 0.0f;
				audible = i + 1;
				break;
			}
		}
		data[i] *= (float)*fade_vol / 65536.0f;
		audible = i + 1;
	}

	return audible;
}

// Read both volume and pan envelopes from XI file. Assumes file position is at the
// 48-byte volume envelope point data (offset 0xA2 in XI file). Returns true on success.
// After this call, the file position is past the 23-byte envelope parameters block.
static bool xi_envelope_read_both(struct XIEnvelope *vol_env, struct XIEnvelope *pan_env, disk_t *file)
{
	uint8_t b[23];

	// Read 48 bytes of volume envelope points (12 points * 4 bytes each)
	if (DISK_read_binary(file, vol_env->points, 48) != 48)
	{
		fprintf(stderr, "XI: Volume envelope points reading error\n");
		return false;
	}

	// Convert from little-endian
	{
		uint8_t *vp = (uint8_t*)vol_env->points;
		for (int i = 0; i < 24; i++)
		{
			int16_t val = (int16_t)vp[i * 2] | ((int16_t)vp[i * 2 + 1] << 8);
			*(int16_t*)(vp + i * 2) = val;
		}
	}

	// Read 48 bytes of panning envelope points (offset 0xD2)
	if (DISK_read_binary(file, pan_env->points, 48) != 48)
	{
		fprintf(stderr, "XI: Pan envelope points reading error\n");
		return false;
	}

	// Convert from little-endian
	{
		uint8_t *vp = (uint8_t*)pan_env->points;
		for (int i = 0; i < 24; i++)
		{
			int16_t val = (int16_t)vp[i * 2] | ((int16_t)vp[i * 2 + 1] << 8);
			*(int16_t*)(vp + i * 2) = val;
		}
	}

	// Read 23 bytes of envelope parameters at offset 0x102 (shared between vol + pan envs)
	if (DISK_read_binary(file, b, 23) != 23)
	{
		fprintf(stderr, "XI: Envelope parameters reading error\n");
		return false;
	}

	vol_env->num_points    = b[0];
	vol_env->sustain_point = b[2];
	vol_env->loop_start     = b[3];
	vol_env->loop_end       = b[4];
	vol_env->flags          = b[8];

	pan_env->num_points    = b[1];  // b[1] = number of panning points (0x103)
	pan_env->sustain_point = b[5];  // b[5] = panning sustain point (0x107)
	pan_env->loop_start     = b[6]; // b[6] = panning loop start point (0x108)
	pan_env->loop_end       = b[7]; // b[7] = panning loop end point (0x109)
	pan_env->flags          = b[9]; // pan envelope flags at offset 9

	xi_envelope_check(vol_env);
	xi_envelope_check(pan_env);

	// Read volume fadeout (16-bit at offset 14 within the param block, file offset 0x110)
	pan_env->volfade = (int16_t)b[14] | ((int16_t)b[15] << 8);
	vol_env->volfade = pan_env->volfade; // shared, stored in both for convenience

	return true;
}

#ifdef __cplusplus
}
#endif
