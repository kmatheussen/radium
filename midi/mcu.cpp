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

	Currently the eight channel faders, their mute, solo, record, and select
	buttons, and the channel VU meters are supported. Fader movements are sent
	as pitch bend messages on MIDI channels 1-8 (i.e. channel numbers 0-7), and
	motor positions are set by sending pitch bend back to the device. Button LEDs
	and VU meters are kept in sync with the corresponding seqtrack state.
*/


#include <math.h>
#include <string.h>
#include <vector>

#include <boost/lockfree/queue.hpp>

#include "../common/nsmtracker.h"
#include "../common/sequencer_proc.h"
#include "../api/api_proc.h"

#include "../audio/SoundPlugin.h"
#include "../audio/AudioMeterPeaks_proc.h"

#include "OS_midi_proc.h"
#include "midi_ports_proc.h"

#include "mcu_proc.h"


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
	bool for_audiofiles = false;
	instrument_t instrument_id;
};

struct McuPort
{
	const symbol_t *port_name;

	radium::MidiOutputPort *output_port = NULL;

	int64_t last_output_attempt = -100000;

	bool has_reported_output_failure = false;

	// Last pitch bend value sent to the device. -2 means that the fader has not
	// been synchronized yet.
	int last_sent[8];

	// The seqtrack each fader was mapped to when it was last synchronized.
	// Used to detect when the visible seqtracks change.
	int last_target_seqtrack[8];

	// Last sent button LED states. -1 means that the LED has not been
	// synchronized yet.
	int last_sent_mute[8];
	int last_sent_solo[8];
	int last_sent_rec[8];
	int last_sent_select[8];

	// Used to avoid adding a new undo entry for every single fader message.
	int64_t last_fader_input_time[8];

	// True when the fader has been moved by the user and the device still needs
	// the new motor position. The new position is sent after a short quiet
	// period, both to avoid fighting the user while dragging and to update the
	// device's internal motor target before it times out and moves the fader
	// back to its previous position.
	bool fader_needs_feedback[8];

	bool meter_overload[8];

	float pending_peak_db[8];

	int64_t last_meter_update = -100000;

	bool meters_enabled = false;

	bool was_sequencer_selected = false;

	McuPort(const symbol_t *port_name)
		: port_name(port_name)
	{
		for(int i=0;i<8;i++)
		{
			last_sent[i] = -2;
			last_target_seqtrack[i] = -2;
			last_sent_mute[i] = -1;
			last_sent_solo[i] = -1;
			last_sent_rec[i] = -1;
			last_sent_select[i] = -1;
			last_fader_input_time[i] = -100000;
			fader_needs_feedback[i] = false;
			meter_overload[i] = false;
			pending_peak_db[i] = MIN_DB;
		}
	}
};

} // anonymous namespace


static auto *g_messages = new boost::lockfree::queue<McuMessage, boost::lockfree::capacity<4096> >;

static std::vector<McuPort*> g_ports;

static bool g_sequencer_has_focus = false;


/*********************************************************/
/**************** Fader value conversion *****************/
/*********************************************************/

// Same conversion as the volume sliders in bin/scheme/instruments.scm and
// bin/scheme/seqtrack-headers.scm.
static float db_to_fader(float db)
{
	if (db <= MIN_DB)
		return 0.0f;

	float mixer_normalized = (db - MIN_DB) / (MAX_VOLUME_SLIDER_DB - MIN_DB);

	if (mixer_normalized < 0.0f)
		mixer_normalized = 0.0f;
	else if (mixer_normalized > 1.0f)
		mixer_normalized = 1.0f;

	return mixer_normalized * mixer_normalized;
}

static float fader_to_db(float fader)
{
	if (fader <= 0.0f)
		return MIN_DB;

	if (fader > 1.0f)
		fader = 1.0f;

	return MIN_DB + sqrtf(fader) * (MAX_VOLUME_SLIDER_DB - MIN_DB);
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

static FaderTarget get_fader_target(int fader)
{
	FaderTarget target;

	int num_seqtracks = getNumSeqtracks();
	if (num_seqtracks <= 0)
		return target;

	// Map the faders to the seqtracks currently visible in the sequencer,
	// starting with the topmost visible seqtrack.
	const int topmost = getTopmostVisibleSeqtrack();
	int num_found = 0;

	for(int seqtracknum = topmost ; seqtracknum < num_seqtracks ; seqtracknum++)
	{
		if (!getSeqtrackVisible(seqtracknum))
			continue;

		if (num_found == fader)
		{
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

		num_found++;
	}

	return target;
}

static float get_fader_value(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return -1.0f;

	float db;

	if (target.for_audiofiles)
		db = MIN_DB + getStoredInstrumentEffect(target.instrument_id, "System Volume") * (MAX_DB - MIN_DB);
	else
		db = gainToDb(getSeqtrackNoteGain(target.seqtracknum));

	return db_to_fader(db);
}

static void maybe_make_undo(const FaderTarget &target)
{
	if (target.seqtracknum < 0)
		return;

	if (target.for_audiofiles)
		undoInstrumentEffect(target.instrument_id, "System Volume");
	else
		undoSeqtrackNoteGain(target.seqtracknum);
}

static void set_fader_value(const FaderTarget &target, float fader)
{
	if (target.seqtracknum < 0)
		return;

	if (target.for_audiofiles)
	{
		setInstrumentEffect(target.instrument_id, "System Volume", fader_to_radium_normalized(fader));
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
	MCU_NOTE_SELECT = 0x18
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

static void send_button_led(McuPort *port, int note, bool is_on, int *last_sent)
{
	if (port->output_port == NULL)
		return;

	const int value = is_on ? 127 : 0;

	if (*last_sent == value)
		return;

	MIDIPORT_send_message(port->output_port, 0x90, note, value);
	*last_sent = value;
}

static void sync_button_leds(McuPort *port, int fader, const FaderTarget &target)
{
	send_button_led(port, MCU_NOTE_MUTE + fader, get_target_mute(target), &port->last_sent_mute[fader]);
	send_button_led(port, MCU_NOTE_SOLO + fader, get_target_solo(target), &port->last_sent_solo[fader]);
	send_button_led(port, MCU_NOTE_REC + fader, get_target_recording(target), &port->last_sent_rec[fader]);
	send_button_led(port, MCU_NOTE_SELECT + fader, get_target_selected(target), &port->last_sent_select[fader]);
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
/**************** Output / device handling ***************/
/*********************************************************/

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

	int value = target.seqtracknum >= 0
		? fader_to_int(fader_value)
		: 0;

	send_fader(port, fader, value);
	port->last_target_seqtrack[fader] = target.seqtracknum;

	sync_button_leds(port, fader, target);
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
	if (!g_sequencer_has_focus)
		return;

	const int fader = byte1 & 0x0f;
	if (fader >= 8)
		return;

	const int value = (byte2 & 0x7f) | ((byte3 & 0x7f) << 7);
	const float fader_value = int_to_fader(value);

	FaderTarget target = get_fader_target(fader);
	if (target.seqtracknum < 0)
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

	port->last_target_seqtrack[fader] = target.seqtracknum;
	port->fader_needs_feedback[fader] = true;
}

static void process_button_message(McuPort *port, int byte1, int note, int velocity)
{
	if (!g_sequencer_has_focus)
		return;

	// MCU buttons send note-on with velocity 127 on press and either note-off
	// or note-on with velocity 0 on release.
	if ((byte1 & 0xf0) != 0x90 || velocity == 0)
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
	else
		return;

	FaderTarget target = get_fader_target(fader);
	if (target.seqtracknum < 0)
		return;

	if (note >= MCU_NOTE_REC && note < MCU_NOTE_REC + 8)
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
		if (message.byte1 >= 0xe0 && message.byte1 <= 0xe7)
			process_fader_message(port, message.byte1, message.byte2, message.byte3);
		else if ((message.byte1 & 0xf0) == 0x90)
			process_button_message(port, message.byte1, message.byte2, message.byte3);
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

	if (!sequencer_has_focus)
	{
		if (port->was_sequencer_selected)
		{
			clear_meters(port);
			disable_meters(port);
		}

		port->was_sequencer_selected = false;
		return;
	}

	enable_meters(port);

	if (!port->was_sequencer_selected)
	{
		for(int fader=0 ; fader<8 ; fader++)
			force_sync_fader(port, fader, get_fader_target(fader));

		port->was_sequencer_selected = true;
		return;
	}

	const bool update_meters = now - port->last_meter_update >= 50;
	if (update_meters)
		port->last_meter_update = now;

	for(int fader=0 ; fader<8 ; fader++)
	{
		FaderTarget target = get_fader_target(fader);
		float fader_value = get_fader_value(target);

		int value = target.seqtracknum >= 0
			? fader_to_int(fader_value)
			: 0;

		// After the user has stopped moving the fader, send the new position back
		// to the device. Otherwise the device's motor target remains at the old
		// position, and the X-Touch moves the fader back there after a short
		// timeout.
		const bool send_deferred_feedback = port->fader_needs_feedback[fader] && now - port->last_fader_input_time[fader] > 150;

		if (target.seqtracknum != port->last_target_seqtrack[fader])
		{
			send_fader(port, fader, value);
			port->last_target_seqtrack[fader] = target.seqtracknum;
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

		sync_button_leds(port, fader, target);

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
