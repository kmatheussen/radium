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
	MCU (Mackie Control Universal) support.

	The MCU protocol is used by many controllers, not only the original Mackie
	Control. Examples are Behringer X-Touch, Icon QCon, and various other
	devices. This file only implements the parts of the protocol Radium uses.

	Currently the eight channel faders, their V-Pots, mute, solo, record, and
	select buttons, the channel VU meters, and the track panels are supported.
	Fader movements are sent as pitch bend messages on MIDI channels 1-8 (i.e.
	channel numbers 0-7), and motor positions are set by sending pitch bend back
	to the device. Button LEDs, V-Pot LED rings, VU meters, and track panels are
	kept in sync with the corresponding seqtrack state. X-Touch track panel
	colors are also supported.
*/


#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include <boost/lockfree/queue.hpp>

#include <QByteArray>
#include <QColor>
#include <QString>

#include "../common/nsmtracker.h"
#include "../common/sequencer_proc.h"
#include "../common/settings_proc.h"
#include "../common/OS_visual_input.h"
#include "../api/api_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/AudioMeterPeaks_proc.h"

#include "../Qt/Qt_colors_proc.h"

#include "OS_midi_proc.h"
#include "midi_ports_proc.h"

#include "mcu_proc.h"


static bool is_xtouch_port_name(const symbol_t *port_name)
{
	const char *name = port_name->name;

	return strstr(name, "X-Touch") != NULL || strstr(name, "X-TOUCH") != NULL || strstr(name, "x-touch") != NULL;
}


namespace
{

struct McuMessage
{
	const char *port_name;
	int byte1;
	int byte2;
	int byte3;
};

struct FaderTarget
{
	int seqtracknum = -1;
	int tracknum = -1;
	bool for_audiofiles = false;
	bool is_master = false;
	bool editor_mode = false;
	instrument_t instrument_id;
};

struct McuPort
{
	const symbol_t *port_name;

	bool is_xtouch;

	radium::MidiOutputPort *output_port = NULL;

	int64_t last_output_attempt = -100000;

	bool has_reported_output_failure = false;

	// Last pitch bend value sent to the device. -2 means that the fader has not
	// been synchronized yet. Index 8 is the master fader.
	int last_sent[9];

	// The seqtrack each fader was mapped to when it was last synchronized.
	// Used to detect when the visible seqtracks change.
	int last_target_seqtrack[9];

	// Last sent button LED states. -1 means that the LED has not been
	// synchronized yet.
	int last_sent_mute[8];
	int last_sent_solo[8];
	int last_sent_rec[8];
	int last_sent_select[8];
	int last_sent_vpot[8];

	// Fader bank / channel buttons.
	int bank_offset = 0;
	int last_sent_bank_left = -1;
	int last_sent_bank_right = -1;
	int last_sent_channel_left = -1;
	int last_sent_channel_right = -1;

	// Main transport button LEDs: rewind, forward, stop, play, record.
	int last_sent_transport[5];
	int64_t last_transport_led_update = -100000;
	bool last_playing_state = false;
	bool last_punching_state = false;

	// Main display.
	int last_sent_namevalue_button = -1;
	int last_sent_timecode_button = -1;
	int last_sent_smpte_led = -1;
	int last_sent_beats_led = -1;
	int64_t last_timecode_update = -100000;
	char last_timecode[11];
	bool timecode_needs_full_update = true;
	bool last_timecode_bars_and_beats = false;

	// Used to avoid adding a new undo entry for every single fader message.
	int64_t last_fader_input_time[9];

	// Same, but for knob movements.
	int64_t last_vpot_input_time[8];

	// While this is in the future, show the pan value on the track panel
	// instead of the volume.
	int64_t pan_display_until[8];

	// True when the fader has been moved by the user and the device still needs
	// the new motor position. The new position is sent after a short quiet
	// period, both to avoid fighting the user while dragging and to update the
	// device's internal motor target before it times out and moves the fader
	// back to its previous position.
	bool fader_needs_feedback[9];

	bool meter_overload[8];

	float pending_peak_db[8];

	int64_t last_meter_update = -100000;

	int64_t last_panel_update = -100000;

	bool panels_need_full_update = true;

	char last_line1[55];
	char last_line2[55];
	int last_colors[8];

	bool meters_enabled = false;

	bool was_sequencer_selected = false;
	bool was_editor_selected = false;

	McuPort(const symbol_t *port_name)
		: port_name(port_name)
		, is_xtouch(is_xtouch_port_name(port_name))
	{
		memset(last_line1, 0, sizeof(last_line1));
		memset(last_line2, 0, sizeof(last_line2));
		memset(last_timecode, 0, sizeof(last_timecode));

		for(int i=0;i<8;i++)
		{
			last_sent[i] = -2;
			last_target_seqtrack[i] = -2;
			last_sent_mute[i] = -1;
			last_sent_solo[i] = -1;
			last_sent_rec[i] = -1;
			last_sent_select[i] = -1;
			last_sent_vpot[i] = -1;
			last_fader_input_time[i] = -100000;
			last_vpot_input_time[i] = -100000;
			pan_display_until[i] = -100000;
			fader_needs_feedback[i] = false;
			meter_overload[i] = false;
			pending_peak_db[i] = MIN_DB;
			last_colors[i] = -1;
		}

		last_sent[8] = -2;
		last_target_seqtrack[8] = -2;
		last_fader_input_time[8] = -100000;
		fader_needs_feedback[8] = false;

		for(int i=0 ; i<5 ; i++)
			last_sent_transport[i] = -1;
	}
};

} // anonymous namespace


static auto *g_messages = new boost::lockfree::queue<McuMessage, boost::lockfree::capacity<4096> >;

static std::vector<McuPort*> g_ports;

static bool g_sequencer_has_focus = false;
static bool g_editor_has_focus = false;


/*********************************************************/
/**************** Fader value conversion *****************/
/*********************************************************/

// Calibration of the fader positions against the printed dB scale on the
// X-Touch. The scale is a standard Mackie Control scale; the values below were
// measured on an X-Touch.
//
// Radium's actual volume value can go above the slider maximum of +6 dB, so
// use the whole printed +10 dB range. Below -35 dB the printed scale is ignored
// and the fader instead goes linearly down to -inf at the bottom.
static float db_to_fader(float db)
{
	const float points[][2] = {
		{-35.0f, 0.201990f},
		{-30.0f, 0.236915f},
		{-20.0f, 0.367430f},
		{-10.0f, 0.497945f},
		{-5.0f,  0.627044f},
		{0.0f,   0.756144f},
		{5.0f,   0.878072f},
		{6.0f,   0.902457f},
		{10.0f,  1.0f}
	};

	if (db <= MIN_DB)
		return 0.0f;

	// Below -35 dB, ignore the printed scale and go linearly down to -inf.
	if (db < -35.0f)
	{
		const float t = (db - MIN_DB) / (-35.0f - MIN_DB);
		return t * 0.201990f;
	}

	if (db >= 10.0f)
		return points[8][1];

	for(int i=0 ; i<8 ; i++)
	{
		if (db <= points[i+1][0])
		{
			const float t = (db - points[i][0]) / (points[i+1][0] - points[i][0]);
			return points[i][1] + t * (points[i+1][1] - points[i][1]);
		}
	}

	return points[8][1];
}

static float fader_to_db(float fader)
{
	const float points[][2] = {
		{0.201990f, -35.0f},
		{0.236915f, -30.0f},
		{0.367430f, -20.0f},
		{0.497945f, -10.0f},
		{0.627044f, -5.0f},
		{0.756144f, 0.0f},
		{0.878072f, 5.0f},
		{0.902457f, 6.0f},
		{1.0f, 10.0f}
	};

	// Below -35 dB, ignore the printed scale and go linearly down to -inf.
	if (fader <= 0.001f)
		return MIN_DB;

	if (fader < 0.201990f)
	{
		const float t = fader / 0.201990f;
		return MIN_DB + t * 5.0f;
	}

	// Some X-Touch units report slightly less than 16383 at the physical top
	// stop. Snap the last tiny part of the travel to +10 so the status bar
	// shows 10.00 dB instead of 9.99 dB.
	if (fader >= 0.999f)
		return 10.0f;

	for(int i=0 ; i<8 ; i++)
	{
		if (fader <= points[i+1][0])
		{
			const float t = (fader - points[i][0]) / (points[i+1][0] - points[i][0]);
			return points[i][1] + t * (points[i+1][1] - points[i][1]);
		}
	}

	return 10.0f;
}

static float fader_to_radium_normalized(float fader)
{
	float db = fader_to_db(fader);
	float ret = (db - MIN_DB) / (MAX_DB - MIN_DB);

	if (ret < 0.0f)
		return 0.0f;
	else if (ret > 1.0f)
		return 1.0f;
	else
		return ret;
}

static int fader_to_int(float fader)
{
	if (fader <= 0.0f)
		return 0;

	if (fader >= 1.0f)
		return 16383;

	return (int)(fader * 16383.0f + 0.5f);
}

static float int_to_fader(int value)
{
	return (float)value / 16383.0f;
}


/*********************************************************/
/**************** Seqtrack/fader mapping *****************/
/*********************************************************/

static int get_target_id(const FaderTarget &target)
{
	if (target.is_master)
		return -2;

	if (target.editor_mode)
		return -1000 - target.tracknum;

	return target.seqtracknum;
}

static int get_num_bank_items(bool editor_mode)
{
	return editor_mode ? getNumTracks(-1) : getNumSeqtracks();
}

static void change_bank(McuPort *port, int delta, bool whole_bank, bool editor_mode)
{
	const int num_seqtracks = get_num_bank_items(editor_mode);

	if (num_seqtracks <= 0)
	{
		port->bank_offset = 0;
		return;
	}

	if (whole_bank)
	{
		if (delta > 0)
		{
			const int max_bank_offset = ((num_seqtracks - 1) / 8) * 8;
			int next_offset = port->bank_offset + 8;

			if (next_offset > max_bank_offset)
				next_offset = port->bank_offset < max_bank_offset ? max_bank_offset : port->bank_offset;

			port->bank_offset = next_offset;
		}
		else
		{
			port->bank_offset = R_MAX(0, port->bank_offset - 8);
		}
	}
	else
	{
		port->bank_offset = R_BOUNDARIES(0, port->bank_offset + delta, num_seqtracks - 1);
	}

	port->panels_need_full_update = true;
	port->last_panel_update = -100000;
}

static FaderTarget get_fader_target(int fader, int bank_offset, bool editor_mode)
{
	FaderTarget target;

	if (fader == 8)
	{
		struct Patch *system_out = GFX_OS_get_system_out();

		if (system_out == NULL)
			return target;

		target.is_master = true;
		target.for_audiofiles = true;
		target.instrument_id = system_out->id;

		if (!instrumentEffectExists(target.instrument_id, "System In"))
			target.is_master = false;

		return target;
	}

	if (editor_mode)
	{
		const int num_tracks = getNumTracks(-1);
		const int tracknum = bank_offset + fader;

		if (tracknum < 0 || tracknum >= num_tracks)
			return target;

		target.tracknum = tracknum;
		target.editor_mode = true;

		return target;
	}

	const int num_seqtracks = getNumSeqtracks();
	const int seqtracknum = bank_offset + fader;

	if (seqtracknum < 0 || seqtracknum >= num_seqtracks)
		return target;

	target.seqtracknum = seqtracknum;
	target.for_audiofiles = seqtrackForAudiofiles(seqtracknum);

	if (target.for_audiofiles)
	{
		target.instrument_id = getSeqtrackInstrument(seqtracknum);

		if (!instrumentEffectExists(target.instrument_id, "System Volume"))
			target.seqtracknum = -1; // Not a seqtrack with a volume slider.
	}

	return target;
}

static const char *get_volume_effect_name(const FaderTarget &target)
{
	return target.is_master ? "System In" : "System Volume";
}

static float get_fader_value(const FaderTarget &target)
{
	if (target.editor_mode)
		return getTrackVolume(target.tracknum, -1, -1);

	if (target.seqtracknum < 0 && !target.is_master)
		return -1.0f;

	float db;

	if (target.for_audiofiles)
		db = MIN_DB + getStoredInstrumentEffect(target.instrument_id, get_volume_effect_name(target)) * (MAX_DB - MIN_DB);
	else
		db = gainToDb(getSeqtrackNoteGain(target.seqtracknum));

	return db_to_fader(db);
}

static void maybe_make_undo(const FaderTarget &target)
{
	if (target.editor_mode)
	{
		undoTrackVolume(target.tracknum, -1, -1);
		return;
	}

	if (target.seqtracknum < 0 && !target.is_master)
		return;

	if (target.for_audiofiles)
		undoInstrumentEffect(target.instrument_id, get_volume_effect_name(target));
	else
		undoSeqtrackNoteGain(target.seqtracknum);
}

static void set_fader_value(const FaderTarget &target, float fader)
{
	if (target.editor_mode)
	{
		setTrackVolume(fader, target.tracknum, -1, -1);
		return;
	}

	if (target.seqtracknum < 0 && !target.is_master)
		return;

	if (target.for_audiofiles)
	{
		setInstrumentEffect(target.instrument_id, get_volume_effect_name(target), fader_to_radium_normalized(fader));
	}
	else
	{
		setSeqtrackNoteGain(db2gain(fader_to_db(fader)), target.seqtracknum);
	}
}


/*********************************************************/
/**************** Mute / solo / record *******************/
/*********************************************************/

enum
{
	MCU_NOTE_REC = 0x00,
	MCU_NOTE_SOLO = 0x08,
	MCU_NOTE_MUTE = 0x10,
	MCU_NOTE_SELECT = 0x18,

	MCU_NOTE_BANK_LEFT = 0x2e,
	MCU_NOTE_BANK_RIGHT = 0x2f,
	MCU_NOTE_CHANNEL_LEFT = 0x30,
	MCU_NOTE_CHANNEL_RIGHT = 0x31,

	MCU_NOTE_REWIND = 0x5b,
	MCU_NOTE_FORWARD = 0x5c,
	MCU_NOTE_STOP = 0x5d,
	MCU_NOTE_PLAY = 0x5e,
	MCU_NOTE_RECORD = 0x5f
};

static bool get_target_mute(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return false;

	return getSeqtrackMute(target.seqtracknum);
}

static bool get_target_solo(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return false;

	return getSeqtrackSolo(target.seqtracknum);
}

static bool get_target_recording(const FaderTarget &target)
{
	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return false;

	if (seqtrackIsBus(target.seqtracknum))
		return false;

	return seqtrackIsRecording(target.seqtracknum, false);
}

static bool get_target_selected(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return false;

	return getCurrSeqtrack() == target.seqtracknum;
}

static void toggle_target_mute(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return;

	switchSeqtrackMute(target.seqtracknum);
}

static void toggle_target_solo(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return;

	switchSeqtrackSolo(target.seqtracknum);
}

static void toggle_target_recording(const FaderTarget &target)
{
	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return;

	if (seqtrackIsBus(target.seqtracknum))
		return;

	const bool is_recording = seqtrackIsRecording(target.seqtracknum, false);
	setSeqtrackIsRecording(target.seqtracknum, !is_recording);
}

static void select_target(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return;

	setCurrSeqtrack(target.seqtracknum, false, true);
}

static void send_button_led_value(McuPort *port, int note, int value, int *last_sent)
{
	if (port->output_port == NULL)
		return;

	if (*last_sent == value)
		return;

	MIDIPORT_send_message(port->output_port, 0x90, note, value);
	*last_sent = value;
}

static void send_button_led(McuPort *port, int note, bool is_on, int *last_sent)
{
	send_button_led_value(port, note, is_on ? 127 : 0, last_sent);
}

static void sync_button_leds(McuPort *port, int fader, const FaderTarget &target)
{
	send_button_led(port, MCU_NOTE_MUTE + fader, get_target_mute(target), &port->last_sent_mute[fader]);
	send_button_led(port, MCU_NOTE_SOLO + fader, get_target_solo(target), &port->last_sent_solo[fader]);
	send_button_led(port, MCU_NOTE_REC + fader, get_target_recording(target), &port->last_sent_rec[fader]);
	send_button_led(port, MCU_NOTE_SELECT + fader, get_target_selected(target), &port->last_sent_select[fader]);
}

static void format_bars_and_beats(int64_t pos, char timecode[11])
{
	int64_t last_beat_time = -1;
	int64_t previous_beat_time = -1;
	int64_t next_beat_time = -1;
	int bar = 1;
	int beat = 1;

	SEQUENCER_iterate_time(0, pos + (int64_t)getSampleRate() * 60, GridType::BEAT_GRID,
		[&](int64_t seqtime, int barnum, int beatnum, int linenum)
		{
			if (seqtime <= pos)
			{
				previous_beat_time = last_beat_time;
				last_beat_time = seqtime;
				bar = barnum;
				beat = beatnum;
				return true;
			}

			if (next_beat_time < 0)
				next_beat_time = seqtime;

			return false;
		});

	int64_t beat_length = 0;

	if (last_beat_time >= 0 && next_beat_time > last_beat_time)
		beat_length = next_beat_time - last_beat_time;
	else if (last_beat_time >= 0 && previous_beat_time >= 0)
		beat_length = last_beat_time - previous_beat_time;

	int ticks = 0;

	if (beat_length > 0 && last_beat_time >= 0)
	{
		ticks = (int)((pos - last_beat_time) * 1000 / beat_length);
		ticks = R_BOUNDARIES(0, ticks, 999);
	}

	const int display_bar = R_BOUNDARIES(0, bar, 999);
	const int display_beat = R_BOUNDARIES(1, beat, 99);

	char temp[32];
	snprintf(temp, sizeof(temp), "%03d %02d %03d", display_bar, display_beat, ticks);
	memcpy(timecode, temp, 10);
	timecode[10] = '\0';
}

static void update_timecode_display(McuPort *port)
{
	const int64_t now = (int64_t)TIME_get_ms();

	if (now - port->last_timecode_update < 100)
		return;

	port->last_timecode_update = now;

	const bool show_bars_and_beats = MCU_display_bars_and_beats();

	if (show_bars_and_beats != port->last_timecode_bars_and_beats)
	{
		port->last_timecode_bars_and_beats = show_bars_and_beats;
		port->timecode_needs_full_update = true;
	}

	send_button_led(port, 0x34, !show_bars_and_beats, &port->last_sent_namevalue_button);
	send_button_led(port, 0x35, show_bars_and_beats, &port->last_sent_timecode_button);
	send_button_led(port, 0x71, !show_bars_and_beats, &port->last_sent_smpte_led);
	send_button_led(port, 0x72, show_bars_and_beats, &port->last_sent_beats_led);

	char timecode[11];

	if (show_bars_and_beats)
	{
		format_bars_and_beats(getSongPos(), timecode);
	}
	else
	{
		const int fps = 25;
		int64_t total_frames = getSongPos() * fps / getSampleRate();

		if (total_frames < 0)
			total_frames = 0;

		const int frames = total_frames % fps;
		const int64_t total_seconds = total_frames / fps;
		const int seconds = total_seconds % 60;
		const int64_t total_minutes = total_seconds / 60;
		const int minutes = total_minutes % 60;
		const int hours = (total_minutes / 60) % 24;

		snprintf(timecode, sizeof(timecode), "%02d %02d%02d %02d", hours, minutes, seconds, frames);
	}

	for(int i=0 ; i<10 ; i++)
	{
		if (port->timecode_needs_full_update == false && port->last_timecode[i] == timecode[i])
			continue;

		int value = timecode[i] >= '0' && timecode[i] <= '9'
			? 0x30 + (timecode[i] - '0')
			: 0x20;

		// Mackie Control timecode digits: CC 0x40 is the rightmost digit,
		// CC 0x49 is the leftmost digit.
		MIDIPORT_send_message(port->output_port, 0xb0, 0x40 + (9 - i), value);
	}

	memcpy(port->last_timecode, timecode, sizeof(timecode));
	port->timecode_needs_full_update = false;
}

static void sync_bank_leds(McuPort *port, bool editor_mode)
{
	const int num_seqtracks = get_num_bank_items(editor_mode);
	const int max_channel_offset = R_MAX(0, num_seqtracks - 1);
	const int max_bank_offset = num_seqtracks > 0 ? ((num_seqtracks - 1) / 8) * 8 : 0;

	port->bank_offset = R_BOUNDARIES(0, port->bank_offset, max_channel_offset);

	send_button_led(port, MCU_NOTE_BANK_LEFT, port->bank_offset > 0, &port->last_sent_bank_left);
	send_button_led(port, MCU_NOTE_BANK_RIGHT, port->bank_offset < max_bank_offset, &port->last_sent_bank_right);
	send_button_led(port, MCU_NOTE_CHANNEL_LEFT, port->bank_offset > 0, &port->last_sent_channel_left);
	send_button_led(port, MCU_NOTE_CHANNEL_RIGHT, port->bank_offset < max_channel_offset, &port->last_sent_channel_right);
}

static void sync_transport_leds(McuPort *port)
{
	const int64_t now = (int64_t)TIME_get_ms();
	const bool playing = isPlaying();
	const bool punching = isSeqpunching();
	const bool transport_state_changed = playing != port->last_playing_state || punching != port->last_punching_state;

	port->last_playing_state = playing;
	port->last_punching_state = punching;

	if (now - port->last_transport_led_update < 1000 && !transport_state_changed)
		return;

	port->last_transport_led_update = now;

	// Resend periodically since some devices clear their button LEDs when they
	// receive other setup messages.
	for(int i=0 ; i<5 ; i++)
		port->last_sent_transport[i] = -1;

	send_button_led(port, MCU_NOTE_REWIND,  true, &port->last_sent_transport[0]);
	send_button_led(port, MCU_NOTE_FORWARD, true, &port->last_sent_transport[1]);
	send_button_led(port, MCU_NOTE_STOP,    true, &port->last_sent_transport[2]);
	send_button_led_value(port, MCU_NOTE_PLAY, playing ? 1 : 127, &port->last_sent_transport[3]);
	send_button_led(port, MCU_NOTE_RECORD,  punching, &port->last_sent_transport[4]);
}


/*********************************************************/
/************************ V-Pots *************************/
/*********************************************************/

static float get_target_pan(const FaderTarget &target)
{
	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return -1.0f;

	if (!instrumentEffectExists(target.instrument_id, "System Pan"))
		return -1.0f;

	return getStoredInstrumentEffect(target.instrument_id, "System Pan");
}

static float get_editor_target_pan(const FaderTarget &target)
{
	if (!target.editor_mode)
		return -1.0f;

	return getTrackPan(target.tracknum, -1, -1);
}

static float editor_pan_to_normalized(float pan)
{
	return R_BOUNDARIES(0.0f, (pan + 1.0f) * 0.5f, 1.0f);
}

static float get_editor_target_pan_normalized(const FaderTarget &target)
{
	if (!target.editor_mode || !getTrackPanOnOff(target.tracknum, -1, -1))
		return -1.0f;

	const float pan = get_editor_target_pan(target);
	return pan < -1.0f ? -1.0f : editor_pan_to_normalized(pan);
}

static void send_vpot_led(McuPort *port, int fader, float pan)
{
	int value;

	if (pan < 0.0f)
	{
		value = 0;
	}
	else
	{
		int position = (int)lrintf(pan * 10.0f) + 1;
		position = R_BOUNDARIES(1, position, 11);

		// Dot mode. Don't use the center-indicator bit: on the X-Touch it
		// lights two extra LEDs around 5 and 7 o'clock, which looks cluttered.
		value = position;
	}

	if (port->last_sent_vpot[fader] != value)
	{
		if (getenv("RADIUM_MCU_DEBUG_VPOT") != NULL)
			printf("MCU V-Pot ring: fader %d, CC 0x%02x value %d\n", fader, 0x30 + fader, value);

		// Mackie Control V-Pot LED rings use CC 0x30..0x37, not 0x20..0x27.
		MIDIPORT_send_message(port->output_port, 0xb0, 0x30 + fader, value);
		port->last_sent_vpot[fader] = value;
	}
}

static void undo_pan_start(const FaderTarget &target)
{
	openUndo();
	undoInstrumentEffect(target.instrument_id, "System Pan On/Off");
	undoInstrumentEffect(target.instrument_id, "System Pan");
	closeUndo();
}

static void undo_editor_pan_start(const FaderTarget &target)
{
	openUndo();
	undoTrackPan(target.tracknum, -1, -1);
	closeUndo();
}


static void set_target_pan(const FaderTarget &target, float pan)
{
	pan = R_BOUNDARIES(0.0f, pan, 1.0f);

	setInstrumentEffect(target.instrument_id, "System Pan On/Off", 1.0f);
	setInstrumentEffect(target.instrument_id, "System Pan", pan);
}

static void reset_target_pan(const FaderTarget &target)
{
	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return;

	if (!instrumentEffectExists(target.instrument_id, "System Pan"))
		return;

	openUndo();
	resetInstrumentEffect(target.instrument_id, "System Pan On/Off");
	resetInstrumentEffect(target.instrument_id, "System Pan");
	closeUndo();

	SEQUENCER_update(SEQUPDATE_HEADERS);
}

static int g_jog_moves_by_bars = -1;
static int g_display_bars_and_beats = -1;

bool MCU_jog_moves_by_bars(void)
{
	if (g_jog_moves_by_bars < 0)
		g_jog_moves_by_bars = strcmp(SETTINGS_read_string("mcu_jog_grid", "beat"), "beat") == 0 ? 0 : 1;

	return g_jog_moves_by_bars != 0;
}

void MCU_set_jog_moves_by_bars(bool by_bars)
{
	g_jog_moves_by_bars = by_bars ? 1 : 0;
	SETTINGS_write_string("mcu_jog_grid", by_bars ? "bar" : "beat");
}

bool MCU_display_bars_and_beats(void)
{
	if (g_display_bars_and_beats < 0)
		g_display_bars_and_beats = strcmp(SETTINGS_read_string("mcu_display_mode", "smpte"), "bbt") == 0 ? 1 : 0;

	return g_display_bars_and_beats != 0;
}

void MCU_set_display_bars_and_beats(bool show_bars_and_beats)
{
	g_display_bars_and_beats = show_bars_and_beats ? 1 : 0;
	SETTINGS_write_string("mcu_display_mode", show_bars_and_beats ? "bbt" : "smpte");
}

static int64_t find_grid_start_at_or_before(int64_t pos, GridType grid_type)
{
	int64_t result = -1;

	SEQUENCER_iterate_time(0, pos + 1, grid_type,
		[&](int64_t seqtime, int barnum, int beatnum, int linenum)
		{
			if (seqtime <= pos)
			{
				result = seqtime;
				return true;
			}

			return false;
		});

	return result;
}

static int64_t get_adjacent_grid_position(int64_t pos, int direction, GridType grid_type)
{
	const int64_t song_length = getSongLengthInFrames();
	const int64_t max_pos = R_MAX(song_length, pos + (int64_t)getSampleRate() * 60 * 60 * 24);

	if (direction > 0)
	{
		int64_t result = -1;

		SEQUENCER_iterate_time(pos + 1, max_pos, grid_type,
			[&](int64_t seqtime, int barnum, int beatnum, int linenum)
			{
				if (seqtime > pos)
				{
					if (result < 0)
						result = seqtime;
					return false;
				}

				return true;
			});

		return result >= 0 ? result : pos;
	}

	const int64_t current_grid_start = find_grid_start_at_or_before(pos, grid_type);

	if (current_grid_start <= 0)
		return 0;

	const int64_t previous_grid_start = find_grid_start_at_or_before(current_grid_start - 1, grid_type);

	return previous_grid_start >= 0 ? previous_grid_start : 0;
}

static void process_jog_message(int value)
{
	int ticks = value & 0x3f;

	if (ticks == 0)
		return;

	const int direction = (value & 0x40) == 0 ? 1 : -1;
	const GridType grid_type = MCU_jog_moves_by_bars() ? GridType::BAR_GRID : GridType::BEAT_GRID;
	int64_t pos = getSongPos();

	for(int i=0 ; i<ticks ; i++)
		pos = get_adjacent_grid_position(pos, direction, grid_type);

	pos = R_MIN(pos, getSongLengthInFrames());

	setSongPos(pos);
}

static void process_vpot_message(McuPort *port, int controller, int value)
{
	if (controller == 0x3c)
	{
		process_jog_message(value);
		return;
	}

	const bool editor_mode = g_editor_has_focus && !g_sequencer_has_focus;

	if (!g_sequencer_has_focus && !editor_mode)
		return;

	if (controller < 0x10 || controller >= 0x18)
		return;

	const int fader = controller - 0x10;

	FaderTarget target = get_fader_target(fader, port->bank_offset, editor_mode);

	const float sign = (value & 0x40) == 0 ? 1.0f : -1.0f;
	int ticks = value & 0x3f;

	if (ticks == 0)
		ticks = 1;

	const int64_t now = (int64_t)TIME_get_ms();

	if (editor_mode)
	{
		if (!target.editor_mode)
			return;

		const float delta = sign * ticks / 63.0f * 2.0f;

		if (now - port->last_vpot_input_time[fader] > 200)
			undo_editor_pan_start(target);

		port->last_vpot_input_time[fader] = now;
		port->pan_display_until[fader] = now + 1000;

		const float pan = get_editor_target_pan(target);
		if (pan < -1.0f)
			return;

		const float new_pan = R_BOUNDARIES(-1.0f, pan + delta, 1.0f);
		setTrackPan(new_pan, target.tracknum, -1, -1);

		return;
	}

	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return;

	if (!instrumentEffectExists(target.instrument_id, "System Pan"))
		return;

	const float delta = sign * ticks / 63.0f;

	if (now - port->last_vpot_input_time[fader] > 200)
		undo_pan_start(target);

	port->last_vpot_input_time[fader] = now;
	port->pan_display_until[fader] = now + 1000;

	float pan = get_target_pan(target);
	if (pan < 0.0f)
		return;

	set_target_pan(target, pan + delta);

	SEQUENCER_update(SEQUPDATE_HEADERS);
}


/*********************************************************/
/********************** VU meters ************************/
/*********************************************************/

static SoundPlugin *get_target_plugin(const FaderTarget &target)
{
	if (target.seqtracknum < 0 || !target.for_audiofiles)
		return NULL;

	if (target.seqtracknum >= root->song->seqtracks.num_elements)
		return NULL;

	struct SeqTrack *seqtrack = (struct SeqTrack*)root->song->seqtracks.elements[target.seqtracknum];
	if (seqtrack == NULL || seqtrack->patch == NULL)
		return NULL;

	return (SoundPlugin*)seqtrack->patch->patchdata;
}

static float get_target_meter_input_db(const FaderTarget &target)
{
	SoundPlugin *plugin = get_target_plugin(target);
	if (plugin == NULL)
		return MIN_DB;

	float max_db = MIN_DB;

	// Use the peak from the latest audio block as input. The MCU device has
	// its own meter falloff, so we must not use Radium's slow meter
	// falloff/peak hold values here.
	for(int ch=0 ; ch<plugin->volume_peaks.num_channels ; ch++)
		max_db = R_MAX(max_db, plugin->volume_peaks.max_dbs[ch]);

	return max_db;
}

static int db_to_meter_segment(float db)
{
	if (db <= MIN_DB)
		return 0;

	// Use the same scaling as Radium's own vertical audio meters.
	const float top_to_bottom = db2linear(db, 0.0f, 1.0f);

	return R_BOUNDARIES(0, (int)lrintf((1.0f - top_to_bottom) * 13.0f), 13);
}

static void send_meter(McuPort *port, int fader, int segment)
{
	if (port->output_port == NULL)
		return;

	MIDIPORT_send_message(port->output_port,
	                      0xd0,
	                      (fader << 4) | R_BOUNDARIES(0, segment, 13),
	                      0);
}

static void send_meter_overload(McuPort *port, int fader, bool overload)
{
	if (port->output_port == NULL)
		return;

	// 0xE enables the overload/clip indication, 0xF clears it.
	MIDIPORT_send_message(port->output_port,
	                      0xd0,
	                      (fader << 4) | (overload ? 0x0e : 0x0f),
	                      0);
}

static void clear_meters(McuPort *port)
{
	for(int fader=0 ; fader<8 ; fader++)
	{
		send_meter(port, fader, 0);

		if (port->meter_overload[fader])
			send_meter_overload(port, fader, false);

		port->meter_overload[fader] = false;
		port->pending_peak_db[fader] = MIN_DB;
	}
}

static void send_meter_enable_message(McuPort *port, int fader, bool enable)
{
	if (port->output_port == NULL)
		return;

	uint8_t data[] = {0xf0, 0x00, 0x00, 0x66, 0x14, 0x20,
	                  (uint8_t)fader, (uint8_t)(enable ? 0x07 : 0x00), 0xf7};

	MIDIPORT_send_sysex(port->output_port, sizeof(data), data);
}

static void enable_meters(McuPort *port)
{
	if (port->meters_enabled)
		return;

	for(int fader=0 ; fader<8 ; fader++)
		send_meter_enable_message(port, fader, true);

	port->meters_enabled = true;
}

static void disable_meters(McuPort *port)
{
	if (!port->meters_enabled)
		return;

	for(int fader=0 ; fader<8 ; fader++)
		send_meter_enable_message(port, fader, false);

	port->meters_enabled = false;
}


/*********************************************************/
/******************* Track panels ************************/
/*********************************************************/

static void copy_number_and_name_6(int number, const char *name, char *dest)
{
	QByteArray latin = QString::fromUtf8(name == NULL ? "" : name).toLatin1();
	char number_string[32];
	snprintf(number_string, sizeof(number_string), "%d", number);

	int pos = 0;
	for(int i=0 ; number_string[i] != '\0' && pos<6 ; i++)
		dest[pos++] = number_string[i];

	if (pos < 6)
		dest[pos++] = ' ';

	for(int i=0 ; pos<6 ; i++)
	{
		if (i < latin.size() && latin.at(i) >= 0x20 && latin.at(i) <= 0x7e)
			dest[pos++] = latin.at(i);
		else
			dest[pos++] = ' ';
	}
}

static void format_volume_6(float db, char *dest)
{
	char temp[32];

	const bool is_zero = fabsf(db) < 0.05f;

	if (is_zero)
		db = 0.0f;

	if (db <= MIN_DB)
	{
		snprintf(temp, sizeof(temp), "  -inf");
	}
	else if (is_zero)
	{
		snprintf(temp, sizeof(temp), " 0.0dB");
	}
	else
	{
		snprintf(temp, sizeof(temp), "%+.1fdB", db);

		if (strlen(temp) > 6)
			snprintf(temp, sizeof(temp), "%+.0fdB", db);
	}

	for(int i=0 ; i<6 ; i++)
		dest[i] = i < (int)strlen(temp) ? temp[i] : ' ';
}

static void format_normalized_6(float value, char *dest)
{
	char temp[32];

	snprintf(temp, sizeof(temp), "%6.2f", value);

	for(int i=0 ; i<6 ; i++)
		dest[i] = i < (int)strlen(temp) ? temp[i] : ' ';
}

static void format_pan_6(float pan, char *dest)
{
	char temp[32];

	if (pan < 0.0f)
	{
		snprintf(temp, sizeof(temp), "      ");
	}
	else
	{
		// Display the same -90..+90 range that Radium's pan slider uses.
		int amount = (int)lrintf((pan - 0.5f) * 180.0f);
		amount = R_BOUNDARIES(-90, amount, 90);

		if (amount == 0)
			snprintf(temp, sizeof(temp), "%6d", 0);
		else
			snprintf(temp, sizeof(temp), "%+6d", amount);
	}

	for(int i=0 ; i<6 ; i++)
		dest[i] = i < (int)strlen(temp) ? temp[i] : ' ';
}

static QColor get_target_color(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return QColor();

	if (target.for_audiofiles)
	{
		QColor color(getInstrumentColor(target.instrument_id, true));

		if (color.isValid())
			return color;

		return QColor(Qt::white);
	}

	return get_qcolor(SEQUENCER_EDITOR_SEQTRACKS_BACKGROUND_COLOR_NUM);
}

static QColor get_editor_target_color(const FaderTarget &target)
{
	if (!target.editor_mode)
		return QColor();

	instrument_t instrument_id = getInstrumentForTrack(target.tracknum, -1, -1);

	if (!isLegalInstrument(instrument_id))
		return QColor(Qt::white);

	QColor color(getInstrumentColor(instrument_id, true));

	return color.isValid() ? color : QColor(Qt::white);
}

static int color_to_closest_xtouch_color(const QColor &color)
{
	if (!color.isValid())
		return 7; // White

	const int red = color.red();
	const int green = color.green();
	const int blue = color.blue();

	if (R_MAX(red, R_MAX(green, blue)) == 0)
		return 7; // White; black means off on the X-Touch.

	const int colors[8][3] = {
		{0, 0, 0},       // Off
		{255, 0, 0},     // Red
		{0, 255, 0},     // Green
		{255, 255, 0},   // Yellow
		{0, 0, 255},     // Blue
		{255, 0, 255},   // Purple
		{0, 255, 255},   // Cyan
		{255, 255, 255}  // White
	};

	int best = 7;
	int best_distance = INT_MAX;

	// Skip Off. An instrument color should result in a visible background.
	for(int i=1 ; i<8 ; i++)
	{
		const int dr = red - colors[i][0];
		const int dg = green - colors[i][1];
		const int db = blue - colors[i][2];
		const int distance = dr * dr + dg * dg + db * db;

		if (distance < best_distance)
		{
			best_distance = distance;
			best = i;
		}
	}

	return best;
}

static int color_to_xtouch_color(const QColor &color)
{
	if (!color.isValid())
		return 7; // White

	int red = color.red();
	int green = color.green();
	int blue = color.blue();

	int max = R_MAX(red, R_MAX(green, blue));

	if (max == 0)
		return 7; // White (black means off on the X-Touch)

	const float norm = 255.0f / max;

	red = (int)(red * norm);
	green = (int)(green * norm);
	blue = (int)(blue * norm);

	int ret = 0;

	if (red > 0x7f)
		ret |= 1; // Red

	if (green > 0x7f)
		ret |= 2; // Green

	if (blue > 0x7f)
		ret |= 4; // Blue

	return ret == 0 ? 7 : ret;
}

static void send_display_line(McuPort *port, int line_number, const char line[55])
{
	if (port->output_port == NULL)
		return;

	uint8_t data[63];
	int pos = 0;

	data[pos++] = 0xf0;
	data[pos++] = 0x00;
	data[pos++] = 0x00;
	data[pos++] = 0x66;
	data[pos++] = 0x14;
	data[pos++] = 0x12;
	data[pos++] = line_number == 0 ? 0x00 : 0x38;

	for(int i=0 ; i<55 ; i++)
		data[pos++] = (uint8_t)line[i];

	data[pos++] = 0xf7;

	MIDIPORT_send_sysex(port->output_port, pos, data);
}

static void send_xtouch_colors(McuPort *port, const int colors[8])
{
	if (!port->is_xtouch || port->output_port == NULL)
		return;

	uint8_t data[15] = {0xf0, 0x00, 0x00, 0x66, 0x14, 0x72};

	for(int i=0 ; i<8 ; i++)
		data[6+i] = (uint8_t)colors[i];

	data[14] = 0xf7;

	MIDIPORT_send_sysex(port->output_port, sizeof(data), data);
}

static void update_track_panels(McuPort *port, bool force, bool editor_mode)
{
	char line1[56];
	char line2[56];
	int colors[8];
	const int64_t now = (int64_t)TIME_get_ms();

	memset(line1, ' ', 55);
	memset(line2, ' ', 55);
	line1[55] = '\0';
	line2[55] = '\0';

	for(int fader=0 ; fader<8 ; fader++)
	{
		FaderTarget target = get_fader_target(fader, port->bank_offset, editor_mode);
		const int pos = fader * 7;

		colors[fader] = 0; // Off

		if (target.editor_mode)
		{
			instrument_t instrument_id = getInstrumentForTrack(target.tracknum, -1, -1);
			const char *name = isLegalInstrument(instrument_id) ? getInstrumentName(instrument_id) : "none";

			copy_number_and_name_6(target.tracknum, name, &line1[pos]);

			const float pan = get_editor_target_pan(target);
			const bool show_pan = getTrackPanOnOff(target.tracknum, -1, -1);

			if (show_pan && now < port->pan_display_until[fader] && pan >= -1.0f)
				format_pan_6(editor_pan_to_normalized(pan), &line2[pos]);
			else
				format_normalized_6(getTrackVolume(target.tracknum, -1, -1), &line2[pos]);

			colors[fader] = color_to_closest_xtouch_color(get_editor_target_color(target));
			continue;
		}

		if (target.seqtracknum < 0)
			continue;

		copy_number_and_name_6(target.seqtracknum, getSeqtrackName(target.seqtracknum), &line1[pos]);

		if (now < port->pan_display_until[fader])
		{
			format_pan_6(get_target_pan(target), &line2[pos]);
		}
		else
		{
			const float fader_value = get_fader_value(target);

			if (fader_value >= 0.0f)
				format_volume_6(fader_to_db(fader_value), &line2[pos]);
		}

		colors[fader] = color_to_xtouch_color(get_target_color(target));
	}

	if (force || memcmp(port->last_line1, line1, 55) != 0)
	{
		send_display_line(port, 0, line1);
		memcpy(port->last_line1, line1, 55);
	}

	if (force || memcmp(port->last_line2, line2, 55) != 0)
	{
		send_display_line(port, 1, line2);
		memcpy(port->last_line2, line2, 55);
	}

	if (port->is_xtouch && (force || memcmp(port->last_colors, colors, sizeof(colors)) != 0))
	{
		send_xtouch_colors(port, colors);
		memcpy(port->last_colors, colors, sizeof(colors));
	}
}


/*********************************************************/
/**************** Output / device handling ***************/
/*********************************************************/

static void send_mcu_wakeup(McuPort *port)
{
	if (port->output_port == NULL)
		return;

	// Standard Mackie Control wakeup message. Ardour sends this when the
	// surface connects, and some devices need it before they accept all
	// control messages.
	uint8_t data[] = {0xf0, 0x00, 0x00, 0x66, 0x14, 0x00, 0xf7};

	MIDIPORT_send_sysex(port->output_port, sizeof(data), data);
}

static radium::MidiOutputPort *open_output_port(McuPort *port)
{
	port->last_output_attempt = (int64_t)TIME_get_ms();

	// Most MCU devices use the same name for their input and output port.
	radium::MidiOutputPort *output_port = MIDIPORT_open_output(port->port_name->name, false, false);

	if (output_port == NULL)
	{
		if (!port->has_reported_output_failure)
		{
			printf("MCU: Could not find a MIDI output port named \"%s\". Motorized faders will not work.\n", port->port_name->name);
			port->has_reported_output_failure = true;
		}
	}
	else
	{
		printf("MCU: Using MIDI output port \"%s\".\n", port->port_name->name);
		port->has_reported_output_failure = false;

		port->last_sent_bank_left = -1;
		port->last_sent_bank_right = -1;
		port->last_sent_channel_left = -1;
		port->last_sent_channel_right = -1;
		port->last_sent_namevalue_button = -1;
		port->last_sent_timecode_button = -1;
		port->last_sent_smpte_led = -1;
		port->last_sent_beats_led = -1;
		port->last_timecode_update = -100000;
		port->timecode_needs_full_update = true;
		port->last_timecode_bars_and_beats = false;
		memset(port->last_timecode, 0, sizeof(port->last_timecode));

		send_mcu_wakeup(port);
	}

	return output_port;
}

static void send_fader(McuPort *port, int fader, int value)
{
	if (port->output_port == NULL)
		return;

	if (value < 0)
		value = 0;
	else if (value > 16383)
		value = 16383;

	MIDIPORT_send_message(
		port->output_port,
		0xe0 | fader,
		value & 0x7f,
		value >> 7
	);

	port->last_sent[fader] = value;
}

static void force_sync_fader(McuPort *port, int fader, const FaderTarget &target)
{
	float fader_value = get_fader_value(target);

	int value = (target.seqtracknum >= 0 || target.is_master || target.editor_mode)
		? fader_to_int(fader_value)
		: 0;

	send_fader(port, fader, value);
	port->last_target_seqtrack[fader] = get_target_id(target);

	if (fader < 8)
	{
		sync_button_leds(port, fader, target);
		send_vpot_led(port, fader, get_target_pan(target));
	}
}

static void update_fader_motor(McuPort *port, int fader, const FaderTarget &target, int64_t now)
{
	float fader_value = get_fader_value(target);

	int value = (target.seqtracknum >= 0 || target.is_master || target.editor_mode)
		? fader_to_int(fader_value)
		: 0;

	// After the user has stopped moving the fader, send the new position back
	// to the device. Otherwise the device's motor target remains at the old
	// position, and the X-Touch moves the fader back there after a short
	// timeout.
	const bool send_deferred_feedback = port->fader_needs_feedback[fader] && now - port->last_fader_input_time[fader] > 150;

	const int target_id = get_target_id(target);

	if (target_id != port->last_target_seqtrack[fader])
	{
		send_fader(port, fader, value);
		port->last_target_seqtrack[fader] = target_id;
	}
	else if (send_deferred_feedback)
	{
		send_fader(port, fader, value);
	}
	else if (value > port->last_sent[fader] + 2 || value < port->last_sent[fader] - 2)
	{
		send_fader(port, fader, value);
	}

	if (send_deferred_feedback)
		port->fader_needs_feedback[fader] = false;
}

static McuPort *find_port(const char *port_name)
{
	for(McuPort *port : g_ports)
	{
		if (!strcmp(port->port_name->name, port_name))
			return port;
	}

	return NULL;
}

static void process_fader_message(McuPort *port, int byte1, int byte2, int byte3)
{
	const int fader = byte1 & 0x0f;
	if (fader > 8)
		return;

	const bool editor_mode = g_editor_has_focus && !g_sequencer_has_focus;

	// The master fader is global. Channel faders require editor or sequencer focus.
	if (!g_sequencer_has_focus && !editor_mode && fader != 8)
		return;

	const int value = (byte2 & 0x7f) | ((byte3 & 0x7f) << 7);
	const float fader_value = int_to_fader(value);

	FaderTarget target = get_fader_target(fader, port->bank_offset, editor_mode);
	if (target.seqtracknum < 0 && !target.is_master && !target.editor_mode)
		return;

	const int64_t now = (int64_t)TIME_get_ms();

	// Treat a fader movement as a new gesture if there has been a short pause
	// since the previous message. This makes it possible to use the normal undo
	// system without creating an undo entry for every single pitch bend message.
	if (now - port->last_fader_input_time[fader] > 200)
		maybe_make_undo(target);

	port->last_fader_input_time[fader] = now;

	set_fader_value(target, fader_value);

	// Make the seqtrack header volume slider reflect the new value immediately.
	SEQUENCER_update(SEQUPDATE_HEADERS);

	// Avoid sending the same value back to the motor while the user is moving
	// the fader. The new position is sent after the gesture instead; see
	// process_port().
	float actual_value = get_fader_value(target);
	if (actual_value >= 0.0f)
		port->last_sent[fader] = fader_to_int(actual_value);

	port->last_target_seqtrack[fader] = get_target_id(target);
	port->fader_needs_feedback[fader] = true;
}

static void process_button_message(McuPort *port, int byte1, int note, int velocity)
{
	// MCU buttons send note-on with velocity 127 on press and either note-off
	// or note-on with velocity 0 on release.
	if ((byte1 & 0xf0) != 0x90 || velocity == 0)
		return;

	// Fader bank/channel controls.
	if (note == MCU_NOTE_BANK_LEFT)
	{
		change_bank(port, -1, true, g_editor_has_focus && !g_sequencer_has_focus);
		return;
	}

	if (note == MCU_NOTE_BANK_RIGHT)
	{
		change_bank(port, 1, true, g_editor_has_focus && !g_sequencer_has_focus);
		return;
	}

	if (note == MCU_NOTE_CHANNEL_LEFT)
	{
		change_bank(port, -1, false, g_editor_has_focus && !g_sequencer_has_focus);
		return;
	}

	if (note == MCU_NOTE_CHANNEL_RIGHT)
	{
		change_bank(port, 1, false, g_editor_has_focus && !g_sequencer_has_focus);
		return;
	}

	const bool editor_mode = g_editor_has_focus && !g_sequencer_has_focus;

	if (editor_mode)
	{
		if (note >= MCU_NOTE_MUTE && note < MCU_NOTE_MUTE + 8)
		{
			const int fader = note - MCU_NOTE_MUTE;
			FaderTarget target = get_fader_target(fader, port->bank_offset, true);

			if (target.editor_mode)
				switchTrackOn(target.tracknum, -1);

			return;
		}

		// Solo is not used in editor mode.
		if (note >= MCU_NOTE_SOLO && note < MCU_NOTE_SOLO + 8)
			return;

		if (note >= MCU_NOTE_SELECT && note < MCU_NOTE_SELECT + 8)
		{
			const int fader = note - MCU_NOTE_SELECT;
			FaderTarget target = get_fader_target(fader, port->bank_offset, true);

			if (target.editor_mode)
			{
				if (noteTrackVisible(target.tracknum, -1, -1))
					setCurrentTrack(target.tracknum, -1, -1, true);
				else
					selectTrack(target.tracknum, -1);
			}

			return;
		}

		if (note >= 0x20 && note < 0x28)
		{
			const int fader = note - 0x20;
			FaderTarget target = get_fader_target(fader, port->bank_offset, true);

			if (target.editor_mode)
			{
				port->pan_display_until[fader] = (int64_t)TIME_get_ms() + 1000;
				switchTrackPanOnOff(target.tracknum, -1, -1);
			}

			return;
		}
	}

	// Transport controls are global and work regardless of sequencer focus.
	if (note == MCU_NOTE_REWIND)
	{
		jumpPrevSequencerMark();
		return;
	}

	if (note == MCU_NOTE_FORWARD)
	{
		jumpNextSequencerMark();
		return;
	}

	if (note == MCU_NOTE_STOP)
	{
		playStop();
		setSongPos(getLastSongPosStart());
		return;
	}

	if (note == MCU_NOTE_RECORD)
	{
		setSeqpunching(!isSeqpunching());
		return;
	}

	// Both buttons beside the main display toggle between SMPTE timecode
	// and bars and beats.
	if (note == 0x34 || note == 0x35)
	{
		MCU_set_display_bars_and_beats(!MCU_display_bars_and_beats());
		return;
	}

	if (note == MCU_NOTE_PLAY)
	{
		if (isPlaying())
			pausePlaying();
		else
			playSongFromCurrentSequencerPosition();

		return;
	}

	if (!g_sequencer_has_focus)
		return;

	int fader;
	if (note >= MCU_NOTE_REC && note < MCU_NOTE_REC + 8)
		fader = note - MCU_NOTE_REC;
	else if (note >= MCU_NOTE_SOLO && note < MCU_NOTE_SOLO + 8)
		fader = note - MCU_NOTE_SOLO;
	else if (note >= MCU_NOTE_MUTE && note < MCU_NOTE_MUTE + 8)
		fader = note - MCU_NOTE_MUTE;
	else if (note >= MCU_NOTE_SELECT && note < MCU_NOTE_SELECT + 8)
		fader = note - MCU_NOTE_SELECT;
	else if (note >= 0x20 && note < 0x28)
		fader = note - 0x20;
	else
		return;

	FaderTarget target = get_fader_target(fader, port->bank_offset, false);
	if (target.seqtracknum < 0)
		return;

	if (note >= 0x20 && note < 0x28)
	{
		port->pan_display_until[fader] = (int64_t)TIME_get_ms() + 1000;
		reset_target_pan(target);
	}
	else if (note >= MCU_NOTE_REC && note < MCU_NOTE_REC + 8)
		toggle_target_recording(target);
	else if (note >= MCU_NOTE_SOLO && note < MCU_NOTE_SOLO + 8)
		toggle_target_solo(target);
	else if (note >= MCU_NOTE_MUTE && note < MCU_NOTE_MUTE + 8)
		toggle_target_mute(target);
	else
		select_target(target);

	SEQUENCER_update(SEQUPDATE_HEADERS);
}

static void drain_messages(void)
{
	McuMessage message;

	while(g_messages->pop(message))
	{
		McuPort *port = find_port(message.port_name);
		if (port == NULL)
			continue;

		// Faders are pitch bend messages on MIDI channels 0-7.
		if (message.byte1 >= 0xe0 && message.byte1 <= 0xe8)
			process_fader_message(port, message.byte1, message.byte2, message.byte3);
		else if ((message.byte1 & 0xf0) == 0x90)
			process_button_message(port, message.byte1, message.byte2, message.byte3);
		else if ((message.byte1 & 0xf0) == 0xb0)
			process_vpot_message(port, message.byte2, message.byte3);
	}
}

static void process_port(McuPort *port, bool sequencer_has_focus)
{
	const int64_t now = (int64_t)TIME_get_ms();

	if (port->output_port == NULL && now - port->last_output_attempt > 5000)
		port->output_port = open_output_port(port);

	if (port->output_port == NULL)
	{
		port->was_sequencer_selected = false;
		return;
	}

	// Global controls, such as the transport buttons, should light up as soon
	// as the MCU device is detected, independently of sequencer keyboard focus.
	sync_transport_leds(port);
	sync_bank_leds(port, g_editor_has_focus && !g_sequencer_has_focus);
	update_timecode_display(port);

	{
		FaderTarget master_target = get_fader_target(8, 0, false);
		update_fader_motor(port, 8, master_target, now);
	}

	if (!sequencer_has_focus)
	{
		const bool editor_has_focus = g_editor_has_focus;

		if (port->was_sequencer_selected)
		{
			clear_meters(port);
			disable_meters(port);
			port->panels_need_full_update = true;
		}

		port->was_sequencer_selected = false;

		if (editor_has_focus)
		{
			if (!port->was_editor_selected)
			{
				for(int fader=0 ; fader<8 ; fader++)
					force_sync_fader(port, fader, get_fader_target(fader, port->bank_offset, true));

				port->panels_need_full_update = true;
				port->last_panel_update = -100000;
				port->was_editor_selected = true;
			}
			else
			{
				for(int fader=0 ; fader<8 ; fader++)
				{
					FaderTarget target = get_fader_target(fader, port->bank_offset, true);
					update_fader_motor(port, fader, target, now);
				}
			}

			for(int fader=0 ; fader<8 ; fader++)
			{
				FaderTarget target = get_fader_target(fader, port->bank_offset, true);
				const bool has_track = target.editor_mode;
				const bool is_disabled = has_track && !trackOn(target.tracknum, -1, -1);
				const bool is_selected = has_track && getCurrentTrack(-1) == target.tracknum;

				send_button_led(port, MCU_NOTE_MUTE + fader, is_disabled, &port->last_sent_mute[fader]);
				send_button_led(port, MCU_NOTE_SOLO + fader, false, &port->last_sent_solo[fader]);
				send_button_led(port, MCU_NOTE_REC + fader, false, &port->last_sent_rec[fader]);
				send_button_led(port, MCU_NOTE_SELECT + fader, is_selected, &port->last_sent_select[fader]);

				send_vpot_led(port, fader, get_editor_target_pan_normalized(target));
			}

			if (now - port->last_panel_update >= 200)
			{
				port->last_panel_update = now;
				update_track_panels(port, port->panels_need_full_update, true);
				port->panels_need_full_update = false;
			}
		}
		else
		{
			port->was_editor_selected = false;
		}

		return;
	}

	port->was_editor_selected = false;
	enable_meters(port);

	if (!port->was_sequencer_selected)
	{
		for(int fader=0 ; fader<8 ; fader++)
			port->last_sent_vpot[fader] = -1;

		for(int fader=0 ; fader<8 ; fader++)
			force_sync_fader(port, fader, get_fader_target(fader, port->bank_offset, false));

		force_sync_fader(port, 8, get_fader_target(8, 0, false));

		port->panels_need_full_update = true;
		port->last_panel_update = -100000;
		port->was_sequencer_selected = true;
		return;
	}

	const bool update_meters = now - port->last_meter_update >= 50;
	if (update_meters)
		port->last_meter_update = now;

	for(int fader=0 ; fader<8 ; fader++)
	{
		FaderTarget target = get_fader_target(fader, port->bank_offset, false);

		update_fader_motor(port, fader, target, now);

		sync_button_leds(port, fader, target);
		send_vpot_led(port, fader, get_target_pan(target));

		// Accumulate peaks between each meter update so a short peak is not
		// lost just because it happened between two 50 ms updates.
		port->pending_peak_db[fader] = R_MAX(port->pending_peak_db[fader], get_target_meter_input_db(target));

		if (update_meters)
		{
			const float db = port->pending_peak_db[fader];
			port->pending_peak_db[fader] = MIN_DB;

			const int segment = db_to_meter_segment(db);
			// Match Radium's own meter: it turns red above +4 dB. Use the
			// overload/clip segment only at that point, so the yellow/orange
			// range below +4 dB is still shown by the normal meter segments.
			const bool overload = db > 4.0f;

			if (segment > 0)
			{
				// Send the current peak value. The X-Touch has its own falloff,
				// so we do not send decaying values ourselves.
				send_meter(port, fader, segment);
			}

			if (overload)
			{
				// Send the overload segment last. Some devices treat 0xE as a
				// normal meter segment rather than a latching clip indication.
				send_meter_overload(port, fader, true);
				port->meter_overload[fader] = true;
			}
			else if (port->meter_overload[fader])
			{
				send_meter_overload(port, fader, false);
				port->meter_overload[fader] = false;
			}
		}
	}

	if (now - port->last_panel_update >= 200)
	{
		port->last_panel_update = now;
		update_track_panels(port, port->panels_need_full_update, false);
		port->panels_need_full_update = false;
	}

	sync_transport_leds(port);
}


/*********************************************************/
/********************** Public API ***********************/
/*********************************************************/

void MCU_InputMessageHasBeenReceived(const char *port_name, int byte1, int byte2, int byte3)
{
	// 'port_name' belongs to the symbol table and remains valid for the
	// lifetime of the program, so it's safe to use it as the queue identity.
	McuMessage message{port_name, byte1, byte2, byte3};

	if (!g_messages->bounded_push(message))
		RT_message("MCU MIDI message queue full.");
}

void MCU_add_port(const char *port_name_c)
{
	const symbol_t *port_name = MIDI_OS_get_port_name(port_name_c);

	if (find_port(port_name->name) != NULL)
		return;

	g_ports.push_back(new McuPort(port_name));
}

void MCU_remove_port(const char *port_name_c)
{
	const symbol_t *port_name = MIDI_OS_get_port_name(port_name_c);

	for(size_t i=0 ; i<g_ports.size() ; i++)
	{
		McuPort *port = g_ports[i];

		if (port->port_name == port_name)
		{
			if (port->output_port != NULL)
				MIDIPORT_close_output(port->output_port);

			g_ports.erase(g_ports.begin() + i);
			delete port;

			return;
		}
	}
}

void MCU_call_very_often(void)
{
	if (root == NULL || root->song == NULL)
		return;

	drain_messages();

	const bool sequencer_has_focus = g_sequencer_has_focus;

	for(McuPort *port : g_ports)
		process_port(port, sequencer_has_focus);
}

void MCU_set_sequencer_has_focus(bool has_focus)
{
	g_sequencer_has_focus = has_focus;
}

void MCU_set_editor_has_focus(bool has_focus)
{
	g_editor_has_focus = has_focus;
}
