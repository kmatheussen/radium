#pragma once
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

#include <QHash>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QJsonValue>
#include <QRegularExpression>
#include <QSet>
#include <QString>
#include <QStringList>
#include <QVector>

#include <atomic>
#include <cmath>
#include <functional>
#include <memory>
#include <utility>

#include "LLM_client.hpp"


namespace radium
{
namespace llm
{

// One tuneable system effect parameter. System effects are shared by every
// audio instrument; the names must match audio/SoundPlugin.cpp
// (system_effect_names) exactly, since changes are applied by name.
struct FxParam
{
	QString name;             // exact effect name, e.g. "System Eq1 F."
	QString unit;             // "Hz", "dB", "ms", "semitones", ":1", or ""
	float min_native;
	float max_native;
	bool is_bool;
	bool is_int;
	bool is_gain_db;          // true when the native value is a linear gain but the unit is dB (bus sends)
	QString band_onoff_name;  // the on/off effect of the same band, or empty
	float band_onoff_value = 1.0f; // the value to set the on/off effect to when auto-enabling (e.g. 3 for "System Pitch type")
	QString description;
};

// A catalog entry with the current value, as read from a live instrument.
struct FxParamState
{
	FxParam param;
	float current_native;
	bool has_modulator = false;
};

enum FxInstrumentParamFormat
{
	FX_INSTRUMENT_PARAM_FLOAT,
	FX_INSTRUMENT_PARAM_INT,
	FX_INSTRUMENT_PARAM_BOOL,
	FX_INSTRUMENT_PARAM_RADIO,
	FX_INSTRUMENT_PARAM_UNKNOWN,
};

// A visible control belonging to the instrument plugin itself. Controls with
// a native range/unit exposed by the plugin use native values; controls
// without that optional metadata fall back to normalized 0..1 values.
struct FxInstrumentParamState
{
	int effect_num = -1;
	QString name; // Internal effect name used to validate the control.
	QString display_name; // User-facing name shown by the instrument widget.
	// What the plugin says this control does (its parameter label / description
	// when the plugin provides one). Empty when the plugin exposes none.
	QString description;
	QString current_display;
	float current_scaled = 0.0f;
	bool has_native_range = false;
	float min_native = 0.0f;
	float max_native = 0.0f;
	float current_native = 0.0f;
	QString unit;
	FxInstrumentParamFormat format = FX_INSTRUMENT_PARAM_UNKNOWN;
	// True when the host falls back to the normalized 0..1 range only because
	// the plugin did not report a native range. The real musical range is
	// unknown then ("Bass: 200" may be the loudest setting of a -60..+12 dB
	// gain), so the control's display text must not be scaled linearly.
	bool is_clamped = false;
};

// What the instrument currently sounds like, measured from its own output.
// Coarse and cheap: ~10 bands plus peak/RMS/crest. Only present when the
// instrument was audible recently (see audio/AudioMeasurements.cpp).
struct FxInstrumentMeasurement
{
	bool valid = false;
	int sample_rate = 0;
	// Milliseconds since the block was captured; measurements older than the
	// last parameter change no longer describe the sound.
	long long age_ms = 0;
	int num_frames = 0; // length of the analysed window

	// Levels and loudness (BS.1770): momentary (last 400 ms) and short-term
	// (the whole captured window, ~3 s).
	float peak_db = 0.0f;
	float rms_db = 0.0f;
	float crest_db = 0.0f;
	float loudness_momentary_lufs = -120.0f;
	// Max over all 400 ms windows: the level of the sound itself, which is the
	// figure to match levels with.
	float loudness_loudest_lufs = -120.0f;
	float loudness_window_lufs = -120.0f;
	float true_peak_db = -120.0f;

	// Space: RT60-like decay time and the level 300 ms after the peak.
	float decay_time_s = 0.0f;
	float tail_db = 0.0f;

	// Stereo.
	float stereo_correlation = 0.0f;
	float stereo_side_db = -120.0f;
	float stereo_balance_db = 0.0f;

	// Timbre / noise.
	float centroid_hz = 0.0f;
	float flatness = 0.0f;
	float hum_db = -120.0f;
	float hum_hz = 0.0f;
	float dc_offset = 0.0f;
	float sub_bass_db = -120.0f;

	// Transients.
	float attack_db_per_ms = 0.0f;
	float onsets_per_second = 0.0f;

	// (low_hz, high_hz, level_db) per band.
	QVector<QString> bands;
};

// "peak -12.0, RMS -18.1, crest 6.2 dB" - the figures a follow-up request like
// "keep the same volume as before" needs as a reference.
static inline QString fx_measurement_summary(const FxInstrumentMeasurement &measurement)
{
	if (!measurement.valid)
		return QString();

	return QString("loudness %1 LUFS (at its loudest), true peak %2 dB, crest %3 dB")
	  .arg(measurement.loudness_loudest_lufs, 0, 'f', 1)
	  .arg(measurement.true_peak_db, 0, 'f', 1)
	  .arg(measurement.crest_db, 0, 'f', 1);
}

struct FxInstrumentContext
{
	QString type_name;
	QString plugin_name;
	int plugin_version = -1;
	QString description; // what the plugin is, in one short line (vendor, category, ...)
	// Song timing at the current position. Only used to express durations in
	// musical terms (an eighth note, an LFO of 4 cycles per beat, ...).
	double bpm = 0.0;
	int signature_numerator = 0;
	int signature_denominator = 0;

	QVector<FxInstrumentParamState> params;
	FxInstrumentMeasurement measurement;
	int omitted_params = 0;
	int hidden_midi_controls = 0; // MIDI controller controls the host left out of 'params'
};

// A proposed change, in native units.
struct FxChange
{
	QString name;
	float value;
};

struct FxInstrumentChange
{
	QString type_name;
	QString plugin_name;
	int plugin_version = -1;
	int effect_num = -1;
	QString name;
	QString display_name;
	float old_value_scaled = 0.0f;
	float value_scaled = 0.0f;
	bool value_is_native = false;
	float old_value_native = 0.0f;
	float value_native = 0.0f;
	QString unit;
	// The plugin's own value texts ("170", "35.00", "On"), when the host read
	// them; used for the summaries so they read like the plugin's UI.
	QString old_display;
	QString new_display;
};

// An effect plugin that can be inserted into the instrument's signal path.
// The names are the exact type/name pair from the plugin registry (for
// example "Ladspa" / "Auto Wah"), so the host can create the plugin without
// having to parse a menu entry.
struct FxPlugin
{
	QString type_name;      // "Ladspa", "VST", "VST3", "AU", "LV2", "Faust", ...
	QString name;           // plugin name shown in the plugin menu
	QString container_name; // container/file name for VST/AU/LV2 plugins, empty otherwise
};

// One modulation action proposed by the LLM. A modulator is an LFO-like
// controller that continuously moves one numeric effect parameter. Fields
// that are "not provided" (-1 / empty) keep their defaults for 'add' and
// stay unchanged for 'replace'.
struct FxModulation
{
	enum Action {
		ADD,
		REMOVE,
		REPLACE,
	};

	Action action = ADD;
	QString effect;             // exact catalog effect name
	QString type;               // "sine", "triangle", "square", "saw", "inverted saw", "random"; empty = not provided
	int tempo_numerator = -1;   // LFO cycles per beat = numerator/denominator; -1 = not provided
	int tempo_denominator = -1;
	float min_scaled = -1.0f;   // lower bound, scaled 0..1; -1 = not provided
	float max_scaled = -1.0f;   // upper bound, scaled 0..1; -1 = not provided
};

// Modulator type name -> Modulator plugin type index (see
// audio/Modulator_plugin.cpp). Returns -1 for unknown names.
static inline int fx_modulator_type_index(const QString &type)
{
	const QString name = type.trimmed().toLower();

	if (name == "sine" || name == "sine (lfo)" || name == "sinewave")
		return 0;
	if (name == "triangle" || name == "triangle (lfo)")
		return 1;
	if (name == "square" || name == "square (lfo)")
		return 2;
	if (name == "saw" || name == "saw (lfo)" || name == "sawtooth")
		return 3;
	if (name == "inverted saw" || name == "inverted saw (lfo)")
		return 4;
	if (name == "random")
		return 8;

	return -1;
}

static inline QString fx_modulator_type_name(int type_index)
{
	switch (type_index) {
	case 0: return "sine";
	case 1: return "triangle";
	case 2: return "square";
	case 3: return "saw";
	case 4: return "inverted saw";
	case 5: return "manual input";
	case 6: return "audio input";
	case 7: return "envelope follower";
	case 8: return "random";
	}
	return "sine";
}

// A short description of a modulation action for the status line.
static inline QString fx_summarize_modulations(const QVector<FxModulation> &modulations, int max_modulations = 2)
{
	QStringList parts;
	int count = 0;

	for (const FxModulation &modulation : modulations)
	{
		if (count == max_modulations)
		{
			parts.append(QString("and %1 more").arg(modulations.size() - count));
			break;
		}

		if (modulation.action == FxModulation::REMOVE)
		{
			parts.append(QString("Removed the modulator from %1").arg(modulation.effect));
			count++;
			continue;
		}

		QStringList settings;
		if (!modulation.type.isEmpty())
			settings.append(fx_modulator_type_name(fx_modulator_type_index(modulation.type) >= 0 ? fx_modulator_type_index(modulation.type) : 0));
		if (modulation.tempo_numerator > 0 || modulation.tempo_denominator > 0)
			settings.append(QString("%1/%2 speed").arg(modulation.tempo_numerator > 0 ? modulation.tempo_numerator : 1).arg(modulation.tempo_denominator > 0 ? modulation.tempo_denominator : 2));
		if (modulation.min_scaled >= 0.0f || modulation.max_scaled >= 0.0f)
			settings.append(QString("%1-%2%").arg((int)qRound((modulation.min_scaled >= 0.0f ? modulation.min_scaled : 0.0f) * 100)).arg((int)qRound((modulation.max_scaled >= 0.0f ? modulation.max_scaled : 1.0f) * 100)));

		const QString what = (modulation.action == FxModulation::ADD)
		  ? QString("Added a modulator to %1").arg(modulation.effect)
		  : QString("Changed the modulator on %1").arg(modulation.effect);

		parts.append(settings.isEmpty() ? what : what + " (" + settings.join(", ") + ")");
		count++;
	}

	return parts.join(". ");
}


// The curated list of system effects offered to the LLM. Excludes the mix
// controls it should not touch (input/volume/out, dry/wet and the auxiliary
// buses 3-5).
static inline QVector<FxParam> fx_catalog(void)
{
	QVector<FxParam> ret;

	auto add_param = [&ret](const QString &name, const QString &unit, float min_native, float max_native, bool is_bool, bool is_int, const QString &band_onoff_name, const QString &description, bool is_gain_db = false, float band_onoff_value = 1.0f)
	{
		FxParam param;
		param.name = name;
		param.unit = unit;
		param.min_native = min_native;
		param.max_native = max_native;
		param.is_bool = is_bool;
		param.is_int = is_int;
		param.is_gain_db = is_gain_db;
		param.band_onoff_name = band_onoff_name;
		param.band_onoff_value = band_onoff_value;
		param.description = description;
		ret.append(param);
	};

	auto add = [&add_param](const char *name, const char *unit, float min_native, float max_native, bool is_bool, bool is_int, const char *band_onoff_name, const char *description, bool is_gain_db = false, float band_onoff_value = 1.0f)
	{
		add_param(QString::fromUtf8(name), QString::fromUtf8(unit), min_native, max_native, is_bool, is_int, QString::fromUtf8(band_onoff_name), QString::fromUtf8(description), is_gain_db, band_onoff_value);
	};

	add("System Eq1 F.", "Hz", 50, 20000, false, false, "System Eq1 On/Off", "Peaking EQ band 1 center frequency (fixed bandwidth, Q about 3).");
	add("System Eq1 L.", "dB", -35, 35, false, false, "System Eq1 On/Off", "Peaking EQ band 1 gain. Positive values boost, negative values cut around the Eq1 frequency.");
	add("System Eq1 On/Off", "", 0, 1, true, false, "", "Enables peaking EQ band 1. Set to 1 when changing Eq1.");

	add("System Eq2 F.", "Hz", 50, 20000, false, false, "System Eq2 On/Off", "Peaking EQ band 2 center frequency (fixed bandwidth, Q about 3).");
	add("System Eq2 L.", "dB", -35, 35, false, false, "System Eq2 On/Off", "Peaking EQ band 2 gain. Positive values boost, negative values cut around the Eq2 frequency.");
	add("System Eq2 On/Off", "", 0, 1, true, false, "", "Enables peaking EQ band 2. Set to 1 when changing Eq2.");

	add("System Ls F.", "Hz", 50, 20000, false, false, "System Ls On/Off", "Low shelf corner frequency. The shelf boosts or cuts everything below this frequency.");
	add("System Ls L.", "dB", -35, 35, false, false, "System Ls On/Off", "Low shelf gain (broad low-end boost or cut).");
	add("System Ls On/Off", "", 0, 1, true, false, "", "Enables the low shelf. Set to 1 when changing the low shelf.");

	add("System Hs F.", "Hz", 50, 20000, false, false, "System Hs On/Off", "High shelf corner frequency. The shelf boosts or cuts everything above this frequency.");
	add("System Hs L.", "dB", -35, 35, false, false, "System Hs On/Off", "High shelf gain (broad top-end boost or cut).");
	add("System Hs On/Off", "", 0, 1, true, false, "", "Enables the high shelf. Set to 1 when changing the high shelf.");

	add("System Lp F.", "Hz", 50, 20000, false, false, "System Lowpass On/Off", "2nd order lowpass cutoff frequency. Removes frequencies above the cutoff.");
	add("System Lowpass On/Off", "", 0, 1, true, false, "", "Enables the lowpass filter. Set to 1 when changing the lowpass.");

	add("System Hp F.", "Hz", 50, 20000, false, false, "System Highpass On/Off", "2nd order highpass cutoff frequency. Removes frequencies below the cutoff.");
	add("System Highpass On/Off", "", 0, 1, true, false, "", "Enables the highpass filter. Set to 1 when changing the highpass.");

	add("System Compression Ratio", ":1", 1, 20, false, false, "System Compression On/Off", "Compression ratio. Higher values compress more.");
	add("System Compression Threshold", "dB", -20, 20, false, false, "System Compression On/Off", "Compression threshold in dB. Compression starts when the signal level exceeds this.");
	add("System Compression Attack", "ms", 0, 500, false, false, "System Compression On/Off", "Compression attack time (how fast compression kicks in).");
	add("System Compression Release", "ms", 0, 1000, false, false, "System Compression On/Off", "Compression release time (how fast compression lets go).");
	add("System Compression Makeup Gain", "dB", -40, 40, false, false, "System Compression On/Off", "Gain applied after compression, to make up for the level lost.");
	add("System Compression On/Off", "", 0, 1, true, false, "", "Enables the compressor. Set to 1 when changing compressor parameters.");

	add("System Pitch", "semitones", -12, 12, false, true, "System Pitch type", "Pitch shift of the instrument output, in semitones. Only works when 'System Pitch type' is not Disabled.", false, 3.0f);
	add("System Pitch type", "mode", 0, 4, false, true, "", "Pitch shifter mode: 0 = Disabled (no pitch shifting), 1 = Only left channel, 2 = Only right channel, 3 = All channels, 4 = Invert channels. Pattern/note pitch changes only work when this is not 0; use 3 for a normal pitch shift.");

	add("System Pan", "pan", 0, 1, false, false, "System Pan On/Off", "Stereo pan: 0 = hard left, 0.5 = center, 1 = hard right.");
	add("System Pan On/Off", "", 0, 1, true, false, "", "Enables the pan setting. Set to 1 when changing the pan.");

	add("System Width", "ms", 0, 50, false, false, "System Width On/Off", "Right-channel delay used as a Haas stereo widener: 0 ms = off, 2-10 ms widens, large values sound like an echo.");
	add("System Width On/Off", "", 0, 1, true, false, "", "Enables the width (right-channel delay). Set to 1 when changing the width.");

	add("System Reverb", "dB", -40, 35, false, false, "", "Send level to the Reverb mixer bus, in dB. -40 dB = no send; -20 to -6 dB is typical. The send carries the combined output of all note duplicator voices.", true);
	add("System Chorus", "dB", -40, 35, false, false, "", "Send level to the Chorus mixer bus, in dB. -40 dB = no send; -20 to -6 dB is typical. The send carries the combined output of all note duplicator voices.", true);

	// Note duplicator: seven voices. Every played note is played by each
	// active voice; the voices are then mixed into ONE instrument signal, and
	// all system effects (and the Reverb/Chorus sends) process that mixed
	// signal. Voice 1 is the original note (on by default); voices 2-7 add
	// extra copies. Turning off voice 1 (or all voices) silences the
	// instrument completely. Changing any voice parameter auto-enables its
	// On/Off.
	//
	// The shared part of the voice explanation is emitted once by
	// fx_system_effect_reference(), which collapses the seven voices to one
	// "Voice N" entry per parameter, so the descriptions here only say what
	// the parameter itself does.
	for (int voicenum = 1; voicenum <= 7; voicenum++)
	{
		const QString v = QString::number(voicenum);
		const QString onoff_name = "System On/Off Voice " + v;

		// The same description for every voice: fx_system_effect_reference()
		// lists the seven voices as one "Voice N" entry.
		add_param(onoff_name, "", 0, 1, true, false, "",
		          "1 = this voice is heard. Voice 1 is the original note: never turn it off to achieve something, that silences the instrument completely.");
		add_param("System Transpose Voice " + v, "semitones", -99, 99, false, false, onoff_name, "Transpose in semitones (12 = one octave up).");
		add_param("System Volume Voice " + v, "dB", -35, 35, false, true, onoff_name, "Volume in dB.");
		add_param("System Start Voice " + v, "ms", 0, 1000, false, false, onoff_name, "Delay before this voice starts playing (0 = immediate).");
		add_param("System Length Voice " + v, "ms", 0, 1000, false, false, onoff_name, "Duration override (0 = use the note's own duration).");
		add_param("System Pan Voice " + v, "degrees", -90, 90, false, true, onoff_name, "Pan in degrees (-90 = left, 0 = center, +90 = right).");
		add_param("System Chance Voice " + v, "chance", 0, 256, false, true, onoff_name, "Chance that this voice plays: 256 = always, 128 = 50%.");
	}

	// Instrument-level output and mixer buttons. Use the buttons only when
	// the user explicitly asks about the direct output, muting, bypassing or
	// soloing the instrument.
	add("System Out On/Off", "", 0, 1, true, false, "", "Direct output on/off: 1 = the instrument's direct (dry) signal is sent to the main mix, 0 = the direct signal is muted. This is NOT the mute: the Reverb/Chorus sends keep working when this is 0, so use it for 'only hear reverb/chorus' (raise the send, set this to 0). Set it back to 1 to hear the instrument itself again.");
	add("System Volume On/Off", "", 0, 1, true, false, "", "Mute button: 1 = audible, 0 = muted. Muting silences everything, including the Reverb/Chorus sends (unlike 'System Out On/Off'). Use this to silence the instrument - never the note duplicator voices.");
	add("System Effects On/Off", "", 0, 1, true, false, "", "Bypass button: 1 = effects active, 0 = all effects bypassed. Only change when explicitly asked to bypass or un-bypass, and never together with other effect changes.");
	add("System Solo On/Off", "", 0, 1, true, false, "", "Solo button: 1 = only this instrument is heard (all other instruments are muted), 0 = normal. Only change when explicitly asked to solo or unsolo.");

	return ret;
}

static inline QString fx_quote(const QString &text)
{
	return QString("\"") + text + QString("\"");
}

// Human-readable representation of a native effect value.
static inline QString fx_format_value(const FxParam &param, float value)
{
	if (param.is_bool)
		return value >= 0.5f ? QString("on") : QString("off");

	// Bus sends are stored as linear gains but exposed in dB; the minimum
	// means "no send" (Radium displays it as -inf dB).
	if (param.is_gain_db && value <= param.min_native + 0.001f)
		return "-inf dB";

	if (param.unit == "Hz")
		return QString("%1 Hz").arg(value, 0, 'f', value < 100 ? 1 : 0);
	if (param.unit == "dB")
		return QString("%1%2 dB").arg(value >= 0 ? "+" : "").arg(value, 0, 'f', 2);
	if (param.unit == "ms")
		return QString("%1 ms").arg(value, 0, 'f', value < 10 ? 1 : 0);
	if (param.unit == "semitones")
		return QString("%1 semitones").arg(value, 0, 'f', 1);
	if (param.unit == ":1")
		return QString("%1:1").arg(value, 0, 'f', 2);
	if (param.unit == "degrees")
		return QString("%1%2\u00B0").arg(value >= 0 ? "+" : "").arg((int)qRound(value));
	if (param.unit == "pan")
		return QString("%1%2\u00B0").arg(value * 180 - 90 >= 0 ? "+" : "").arg((int)qRound(value * 180 - 90));
	if (param.unit == "chance")
		return value >= 255.5f ? QString("always") : QString("%1/256").arg((int)qRound(value));
	if (param.unit == "mode")
		return QString::number((int)qRound(value));

	return QString::number(value, 'f', 2);
}

static inline QString fx_format_range(const FxParam &param)
{
	if (param.is_bool)
		return "0 or 1";
	if (param.is_int)
		return QString("%1 to %2").arg(qRound64(param.min_native)).arg(qRound64(param.max_native));
	return QString("%1 to %2").arg(param.min_native, 0, 'f', 2).arg(param.max_native, 0, 'f', 2);
}

// The unit to show for a catalog entry. Boolean effects have no unit of their
// own, and a mode/choice effect (e.g. 'System Pitch type', 0-4) is not an
// on/off switch, so it must not be labelled as one.
static inline QString fx_param_unit(const FxParam &param)
{
	if (!param.unit.isEmpty())
		return param.unit;
	if (param.is_bool)
		return "on/off";
	return "number";
}

static inline QString fx_instrument_param_format_name(FxInstrumentParamFormat format)
{
	switch (format) {
	case FX_INSTRUMENT_PARAM_FLOAT: return "continuous";
	case FX_INSTRUMENT_PARAM_INT: return "integer";
	case FX_INSTRUMENT_PARAM_BOOL: return "on/off";
	case FX_INSTRUMENT_PARAM_RADIO: return "choice";
	case FX_INSTRUMENT_PARAM_UNKNOWN: break;
	}
	return "unknown";
}

// Case/punctuation/spacing-insensitive key for matching plugin names the LLM
// copied slightly differently (for example "Auto-Wah" vs "Auto Wah").
static inline QString fx_normalize_plugin_key(const QString &text)
{
	QString ret;
	for (const QChar c : text)
		if (c.isLetterOrNumber())
			ret.append(c.toLower());
	return ret;
}

// Case/punctuation/spacing-insensitive key for matching control names the LLM
// copied slightly differently. Radium's plugin wrappers prefix plugin
// parameter names with their index ("2: Size", "0: Active #Amb1"), while the
// user - and therefore the model - thinks of them as "Size" and "Active
// #Amb1", so a leading index prefix is dropped before comparing.
static inline QString fx_normalize_control_key(const QString &text)
{
	QString name = text.trimmed();

	// Drop a leading "12: " / "3. " / "4) " index prefix. A separator is
	// required, so names that really start with a number ("808 Kick") keep it.
	int digits = 0;
	while (digits < name.size() && name.at(digits).isDigit())
		digits++;
	if (digits > 0 && digits < name.size() && (name.at(digits) == ':' || name.at(digits) == '.' || name.at(digits) == ')'))
		name = name.mid(digits + 1).trimmed();

	QString ret;
	for (const QChar c : name)
		if (c.isLetterOrNumber())
			ret.append(c.toLower());

	if (ret.isEmpty()) // the name was only an index prefix; fall back to the full text
		for (const QChar c : text)
			if (c.isLetterOrNumber())
				ret.append(c.toLower());

	return ret;
}

// Like fx_normalize_control_key(), but also drops an index prefix that is only
// separated by whitespace ("2 Size"), which is how the model tends to write it.
// Only used as an extra candidate: a name that really starts with a number
// ("808 Kick") is normalized with the number by fx_normalize_control_key().
static inline QString fx_normalize_control_key_loose(const QString &text)
{
	QString name = text.trimmed();

	int digits = 0;
	while (digits < name.size() && name.at(digits).isDigit())
		digits++;
	if (digits == 0 || digits >= name.size())
		return fx_normalize_control_key(text);

	QString rest = name.mid(digits);
	if (rest.at(0) == ':' || rest.at(0) == '.' || rest.at(0) == ')')
		rest = rest.mid(1);
	rest = rest.trimmed();
	if (rest.isEmpty() || !rest.at(0).isLetter())
		return fx_normalize_control_key(text);

	QString ret;
	for (const QChar c : rest)
		if (c.isLetterOrNumber())
			ret.append(c.toLower());

	return ret.isEmpty() ? fx_normalize_control_key(text) : ret;
}

// A control name for the user: without the plugin wrapper's index prefix
// ("2: Size" -> "Size") and without a layer prefix ("Rev1: Size" -> "Size"),
// so the status line reads like the plugin's own UI ("ZRev: Size").
// Both prefixes are dropped because a status line with two or three prefixes
// ("ZRev: Rev1: Size 100% -> 35%") is unreadable; the instrument name already
// scopes the control, so the bare control name is what the user recognizes.
static inline QString fx_control_display_name(const QString &name)
{
	const QRegularExpression index_prefix(QStringLiteral("^[0-9]+:\\s*"));
	const QRegularExpression layer_prefix(QStringLiteral("^[A-Za-z][A-Za-z0-9_ ]*:\\s*"));

	QString stripped = name.trimmed().remove(index_prefix);

	const QString without_layer = stripped.remove(layer_prefix);
	if (!without_layer.isEmpty() && without_layer != stripped)
		stripped = without_layer;

	return stripped.isEmpty() ? name.trimmed() : stripped;
}

// All keys a control name can be looked up by: the strict one and the loose one.
static inline QStringList fx_control_keys(const QString &text)
{
	QStringList ret;
	for (const QString &key : {fx_normalize_control_key(text), fx_normalize_control_key_loose(text)})
		if (!key.isEmpty() && !ret.contains(key))
			ret.append(key);
	return ret;
}

static inline bool fx_plugin_matches(const FxPlugin &a, const FxPlugin &b)
{
	if (fx_normalize_plugin_key(a.type_name) != fx_normalize_plugin_key(b.type_name)
	    || fx_normalize_plugin_key(a.name) != fx_normalize_plugin_key(b.name))
		return false;

	// Only compare the container when both sides have one. Built-in plugins
	// have no container, and the LLM is not required to repeat it.
	if (!a.container_name.isEmpty() && !b.container_name.isEmpty())
		return fx_normalize_plugin_key(a.container_name) == fx_normalize_plugin_key(b.container_name);

	return true;
}

// The schema and rules of the reply. This part is completely static (no
// per-request data), so the LLM provider's prompt cache can reuse it: the
// per-request parts (the effect reference, and the effect plugins whose
// names match the request) are appended by build_fx_system_prompt() after
// this, and the current values go into the user message.
// The behavioural rules. They change more often than the schema/reference
// (each edit invalidates the provider's prompt cache for the whole prefix), so
// they are kept in one place and appended *after* the reference.
static inline QString build_fx_system_prompt_intro(void)
{
	return QString(
	  "You are an expert mixing engineer assistant inside Radium, a music tracker/DAW. "
	  "The user tunes one audio instrument's own plugin controls and Radium's built-in system effects with short subjective "
	  "requests such as \"make it sound harsher\", \"warmer\", \"less muddy\". "
	  "You translate the request into the concrete parameter changes described in the reference below; their current "
	  "values are listed in the user message.\n"
	  "\n"
	  "Respond with ONE JSON object and nothing else - no markdown, no code fences, no text around it.\n"
	  "Schema:\n"
	  "{\"changes\":[{\"name\":\"<exact system effect name>\",\"value\":<number>}, ...], "
	  "\"instrument_changes\":[{\"name\":\"<exact current instrument control name>\",\"value\":<number>}, ...], "
	  "\"modulators\":[{\"action\":\"add|replace|remove\",\"effect\":\"<exact effect name>\",\"type\":\"sine|triangle|square|saw|inverted saw|random\","
	  "\"tempo_numerator\":1,\"tempo_denominator\":2,\"min\":0.0,\"max\":1.0}, ...], "
	  "\"add_plugins\":[{\"name\":\"<exact plugin name>\",\"type\":\"<exact plugin type>\",\"container\":\"<container name, only if needed>\"}, ...], "
	  "\"remove_plugins\":[{\"name\":\"<exact plugin name>\",\"type\":\"<exact plugin type>\",\"container\":\"<container name, only if needed>\"}, ...], "
	  "\"create_faust\":\"<short description of the effect to create, or empty>\", "
	  "\"create_faust_name\":\"<short instrument name, e.g. Autotune, or empty>\", "
	  "\"ask\":\"<one short question to the user, only when you cannot tell what to change, or empty>\", "
	  "\"answer\":\"<an answer of at most 200 characters to a question the user asked, or empty>\", "
	  "\"explanation\":\"<one or two sentences>\"}\n");
}

// The behavioural rules. They change more often than the intro and the effect
// reference (each edit invalidates the provider's prompt cache from the edit
// onwards), so they are appended last.
static inline QString build_fx_system_prompt_rules(void)
{
	return QString(
	  "\n\nRules:\n"
	  "1) Use only names from the provided lists, copied exactly. System-effect names go in \"changes\"; current instrument "
	  "visible instrument-control names (or their exact control name in brackets) go in \"instrument_changes\".\n"
	  "2) Values in \"changes\" are absolute in the native unit shown. For \"instrument_changes\", use native values and "
	  "the unit shown whenever the control lists a native range; otherwise use normalized positions from 0 to 1. Respect "
	  "the listed range. On/off controls use 0 or 1. For integer controls, return an integer native value when available. "
	  "If only normalized values are available, preserve enough decimals to produce the requested integer; never round "
	  "normalized values to three decimals (for example, 922 on a 0..1200 range is 0.768333, not 0.768).\n"
	  "3) For system On/Off effects use 0 or 1. Set the matching system On/Off effect to 1 when you change a system effect that has one "
	  "(EQ bands, filters, compressor, pan, width, and each note duplicator voice). Pitch changes need "
	  "'System Pitch type' set to 3 (All channels); it is 0 (Disabled) by default.\n"
	  "4) List only the controls you want to change (at most 8 total across both change arrays; the matching On/Off switch that "
	  "rule 3 tells you to set for a changed effect does not count toward that limit). Prefer musical, moderate moves; avoid extreme "
	  "boosts or cuts unless the request explicitly asks for something drastic. Leave a control alone when it is already right: "
	  "never add a change just to fill the budget, and never change the same control twice in one reply.\n"
	  "Which controls to change (most important rule): the current instrument's own controls come first. If any listed "
	  "instrument control can serve the request, use \"instrument_changes\" - and never answer a request that names the "
	  "plugin with system effects. 'the plugin', 'the instrument', 'the synth', 'the device', 'its <parameter>' (for example "
	  "'its size', 'the plugin mix') always mean the current instrument's own controls, even when a system effect could also "
	  "do the job: in that case the user does not want the sound processed further. Use the system effects (\"changes\") "
	  "for EQ, dynamics, filtering, panning, width and the bus sends - what the instrument cannot do - or when no listed "
	  "instrument control matches. If you choose system effects although the instrument has a plausible matching control, "
	  "state the reason in one clause of the explanation (for example 'I used the system EQ because the plugin has no tone "
	  "controls'). If you genuinely cannot tell whether the user means the instrument's own sound or the system effects "
	  "around it, ask instead of guessing: set \"ask\" to one short question, make no changes, and leave \"explanation\" "
	  "empty.\n"
	  "When two instrument controls have similar names and the plugin gives no description (for example 'Treble' and "
	  "'TrebleFreq'), change the one the user's words fit best, not both, and say which one you changed. When the names are "
	  "opaque abbreviations ('T0'..'T7', 'AP0'..'AP3') with no description, treat them as fine-tuning internals and leave "
	  "them alone unless the user asks for them by name: change the controls whose names describe the sound (size, decay, "
	  "wet, dry, damp, filter, ...). "
	  "When a request refers to a named control on the current instrument, change that control instead of inserting a different plugin. "
	  "If the instrument has a matching control for only part of the request, change the available control(s) and explain briefly "
	  "which requested control is missing. A missing control is not by itself a reason to add a different plugin.\n"
	  "5) The built-in system effects can shape EQ/filtering, compressor dynamics, pitch, panning and Haas width, "
	  "note duplication (the 'voice' effects) and the send levels to the Reverb and Chorus mixer buses. They cannot "
	  "create distortion, saturation, wah, phaser/flanger, delay/echo, amp simulation or other effects that need a "
	  "dedicated plugin. A request for a distorted, grungy, fuzzy or amped sound ('make it grungy', 'sound like "
	  "Nirvana') is that case: EQ and compression can shape such a tone but cannot create the distortion itself. "
	  "First check whether the current instrument has a control for the requested effect; insert an "
	  "effect plugin only if it does not. Use one of the plugins from the 'Available effect plugins' "
	  "list at the end of the user message (it only lists the installed plugins whose names match this request) with the "
	  "\"add_plugins\" array. If no suitable plugin is listed, do not invent one: "
	  "set \"create_faust\" to a short description of the requested effect (for example \"an autotune effect\") "
	  "and explain briefly that the effect is not currently available. The host will ask the user whether to "
	  "create it with the 'Faust Dev 2' instrument by generating Faust code. Do not claim it has already been "
	  "created. The Reverb/Chorus buses only have an audible effect if those buses are set up in the mixer.\n"
	  "6) A question is answered, not tuned. When the user asks what something is or does, why it sounds as it does, or for an "
	  "explanation ('what does this plugin do?', 'please explain Density and Size', 'what does Mix do'), put a short answer in "
	  "\"answer\" - one sentence, at most 200 characters, in plain language - make no changes, and leave \"ask\" empty. Never put the answer in "
	  "\"ask\" (that field is only for a question back to the user), and never change parameters to answer a question. "
	  "Use the plugin description and the control names for the answer; if the plugin does not reveal what a control does, say "
	  "so instead of inventing it. "
	  "An explicit value ('set modulation to 4 Hz', 'make it 300 ms', 'pan it 30% left') must be honored when the control or "
	  "effect uses that unit. If it does not (the control's unit is something else, for example a 0-100 amount instead of a "
	  "frequency), do not silently substitute a guessed value: set \"ask\" to one short question that offers the choices (for "
	  "example 'Modulation is a 0-100 amount here; should I set it to 50, or add a modulator for 4 Hz?'), make no changes, and "
	  "leave \"explanation\" empty. A modulator ('modulators'; tempo_numerator/denominator is cycles per beat) is the right way "
	  "to get a rate in Hz, and a system effect may also have the parameter the user named. The 'explanation' itself is one or "
	  "two short sentences, in plain language, no lists.\n"
	  "7) Signal flow: the note duplicator voices are mixed into ONE instrument signal that every effect - including "
	  "the Reverb/Chorus sends - processes; effects are never per voice. Never set 'System On/Off Voice 1' (or all "
	  "voices) to 0 to achieve something: it silences the instrument completely, and its bus sends too.\n"
	  "8) The output and mixer buttons ('System Out', 'System Volume', 'System Effects', 'System Solo' On/Off) are "
	  "described in the effect reference. Use the Mute/Bypass/Solo buttons only when the user explicitly asks to "
	  "mute/unmute, bypass/un-bypass or solo/unsolo, and never combine a bypass change with effect changes.\n"
	  "9) 'Only hear reverb/chorus': raise the send level and set 'System Out On/Off' to 0, which removes the "
	  "instrument's direct signal from the main mix while the bus send keeps working; set it back to 1 when the "
	  "instrument itself should be heard again. Never do this by turning note duplicator voices off. The effect's "
	  "own wet/dry balance is set on the bus in the mixer.\n"
	  "10) Modulation: a modulator is an LFO-like controller that continuously moves one numeric system-effect parameter. "
	  "Use the separate \"modulators\" array for requests like 'modulate the high pass', 'wobble', 'slow sweep', "
	  "'pulse' or 'stop modulating X' (never put these in either change array). Use only system-effect names here. action: \"add\" (default; only when the effect "
	  "has no modulator - effects that already have one are marked [modulated]), \"replace\" (change the settings of "
	  "an existing modulator) or \"remove\". type (default sine): sine, triangle, square, saw, inverted saw or "
	  "random. tempo_numerator/tempo_denominator is the LFO rate in cycles per beat (default 1/2; slow = 1/4 to 1/8, "
	  "fast = 1/1 to 4/1). min and max are the lower/upper scaled bounds 0..1 (default 0..1; narrow them, e.g. "
	  "0.4..0.6, for a subtle effect; min > max inverts the LFO). Only numeric effects can be modulated, not "
	  "On/Off effects. Use at most 2 modulator actions per reply. (A modulator added to 'System Pitch' also "
	  "switches on the pitch shifter automatically.)\n"
	  "11) Effect plugins: \"add_plugins\" inserts an external effect plugin directly after this instrument in the "
	  "signal graph (the instrument, and everything already inserted after it, now feeds the new plugin, which "
	  "feeds the instrument's old destinations). Use it only when the user explicitly asks to add a plugin, or when neither "
	  "the current instrument's listed controls nor the built-in system effects can provide the requested effect. Use it when the user asks for an effect the built-in system "
	  "effects cannot provide, such as 'add a wah wah effect', 'make it distorted', 'add a phaser' or 'add a "
	  "delay'. Use the exact name and type from the 'Available effect plugins' list in the user message; never "
	  "invent a plugin. If the list shows a container for the plugin, include it as \"container\" only when "
	  "needed to distinguish plugins with the same name and type. Do not add a plugin when the request can be "
	  "handled by the listed system effects (for "
	  "example 'more reverb' -> 'System Reverb', 'wider' -> 'System Width'). Add at most one plugin per reply "
	  "unless the user explicitly asks for several. Parameters of a newly inserted downstream effect plugin are not "
	  "changed by this reply; the user can adjust those in the plugin's mixer strip.\n"
	  "12) Removing effect plugins: use \"remove_plugins\" with the exact name and type from the 'Effect plugins "
	  "currently inserted after this instrument' list in the user message; include \"container\" when the list "
	  "shows one and it is needed to distinguish same-named plugins. Use it for requests like 'remove the "
	  "wah wah' or 'take off the distortion'. Only plugins in that list can be removed; never remove instruments, "
	  "system effects or plugins that are not listed. Removal bypasses the plugin (reconnecting its input to its "
	  "output) and deletes it. If the same plugin is inserted several times, all its instances are removed. Do not "
	  "list the same plugin in both \"add_plugins\" and \"remove_plugins\" unless the user asks to replace it; "
	  "removals are applied before additions.\n"
	  "13) \"create_faust\" is only for effects that cannot be provided by the current instrument's controls, the built-in system effects or any "
	  "listed effect plugin. Give a short, concrete description of the effect (including important character, "
	  "e.g. \"a smooth autotune effect with medium retune speed\", not just \"an effect\"). Also set "
	  "\"create_faust_name\" to a very short instrument name (one to three words, e.g. \"Autotune\") that will "
	  "be used as the new instrument's name. Leave both empty in all other replies. The host asks the user to "
	  "confirm in the LLM terminal; you never create the instrument yourself.\n"
	  "\n"
	  "Frequency guide for subjective terms (use the peaking EQ bands and the shelves):\n"
	  "- rumble/sub: below 60 Hz; warmth/weight: 100-400 Hz; mud/boxiness: 200-500 Hz; honk/nasal: 500-1000 Hz.\n"
	  "- presence/bite: 1-4 kHz; harshness/hard/edgy: 2-6 kHz; sibilance/ess: 5-9 kHz; air/brilliance: 10-16 kHz.\n"
	  "- 'brighter': boost the high shelf or Eq around 3-6 kHz. 'warmer': boost the low shelf, gently cut the top.\n"
	  "- 'harsher': boost 2-6 kHz and/or the high shelf, possibly cut some low mid. 'softer/smoother': cut 2-6 kHz and the high shelf a little.\n"
	  "- 'muddy/boomy': cut 200-500 Hz. 'thin': boost 100-300 Hz. 'boxy': cut around 300-500 Hz.\n"
	  "- 'tighter' bass: cut 200-400 Hz, optionally use the compressor with a moderate ratio. 'punchier': a little low shelf boost and a compressor with a slower attack.\n"
	  "\n"
	  "Stereo, space and layering guide:\n"
	  "- 'wider'/'stereo': use a small amount of 'System Width' (about 2-10 ms) and enable it, and/or add a note "
	  "duplicator voice panned to one side. Width values above about 20 ms sound like an echo, not width.\n"
	  "- 'pan left/right': set 'System Pan' (0 = hard left, 0.5 = center, 1 = hard right).\n"
	  "- 'more reverb'/'more space': raise the 'System Reverb' send level (-20 to -6 dB is typical). 'more chorus': "
	  "raise 'System Chorus'. For 'only hear reverb/chorus', see rule 9.\n"
	  "- 'thicker'/'layered'/'add an octave': use a note duplicator voice (2-7): set its On/Off to 1 and set "
	  "'System Transpose Voice N' (semitones), 'System Volume Voice N' (dB) and optionally 'System Pan Voice N'. "
	  "Never turn voice 1 off - it is the original sound. Do not use more than one or two extra voices "
	  "unless asked.\n"
	  "\n"
	  "Measurements: when the user message contains a 'What it currently measures' paragraph, those numbers were taken from this "
	  "instrument's own output and describe what it sounds like right now. Use them to choose the direction and the amount "
	  "instead of guessing: the band that sits well below its neighbours (or well above them) is the one that needs the EQ, "
	  "the peak and RMS levels give the headroom for 'louder'/'quieter', and the crest factor is the reference for dynamics - "
	  "a higher crest means punchier, a lower crest means more compressed/squashed. When a request is about level or "
	  "'punch', say what the numbers were before and what you expect them to become. If there are no measurements, the "
	  "instrument was not audible when the prompt was built: say so briefly instead of inventing numbers, and still make "
	  "the change. The numbers describe the sound at the moment they were taken: if the block says they were measured "
	  "some seconds ago, they may predate the user's own adjustments - treat them as indicative and do not contradict "
	  "them. Never boost a band whose measured level is already the highest one. "
	  "The other figures say which control is the right one: a correlation near 1 with side far below mid means the "
	  "material is (nearly) mono, so width can only be created (Width, chorus, a panned voice) - do not suggest making a "
	  "mono signal narrower; a high flatness or a hum peak means the request may be about noise, not tone (highpass, EQ cut); "
	  "a steep attack (above ~2 dB/ms) with many onsets means punchy/percussive material, a flat one means sustained. "
	  "For space ('more/less reverb', 'drier', 'bigger/smaller room', 'more air around it'): the decay figure is what "
	  "the space currently does - a long decay (over ~1.5 s) or a tail level close to 0 dB means it is already wet, so "
	  "the request may be about character (size, damping) rather than more of it; a short decay (under ~0.4 s) or a tail "
	  "around -25 dB or lower means it is dry. When the plugin is a reverb, prefer its own Size/Decay/Damping/Mix "
	  "controls over raising the Reverb send, and use the decay/tail figures to say what you changed and why. "
	  "The song tempo (in the user message, with the beat/bar length in ms) is what you convert musical terms with: an "
	  "eighth note is half a beat, a sixteenth a quarter beat, and the modulator rate is in cycles per beat (so at 120 BPM "
	  "a rate of 2/1 is 4 Hz). When the user asks for a note-length value (delay, pre-delay, attack, width, voice start) or "
	  "a rate in Hz, compute it from the tempo instead of guessing. "
	  "If the peak is above 0 dB the output is clipping: lowering the level comes first, before any tone change. "
	  "Before a change that raises the level, keep the true peak below 0 dB afterwards: a boost of N dB needs N dB of "
	  "headroom, and it is more than that in practice when a shelf or low boost meets the band where the material has most "
	  "of its energy and the compressor pushes the result up again. For a vague 'more'/'louder' request, prefer a tone or "
	  "dynamics change over raising the output level at all. "
	  "For volume requests ('louder', 'quieter', 'keep the same volume as before'): use the 'at its loudest, 400 ms' loudness "
	  "figure (LUFS). It describes the sound itself; the window average is pulled down when the window happens to contain "
	  "silence (a decaying note, a gap), and the 'right now' figure can be inside a decay - never match a level with either "
	  "of those. The change you "
	  "make, in dB, should equal (target loudness - current loudness), applied with ONE control (makeup gain, or the instrument's "
	  "output gain if it has one) - do not stack several small corrections, and do not guess the amount: the measurement "
	  "of the previous turn, listed in the history, is the reference for 'before'. Do not change the compressor's ratio, "
	  "threshold, attack or release in the same reply as a volume target: the extra gain reduction they cause is not in that "
	  "arithmetic, so the level will miss (lowering the threshold by 6 dB at 6:1 adds about 5 dB of gain reduction). When a "
	  "reply has to change both, raise the makeup gain by the expected extra gain reduction as well. If that difference is more than about "
	  "12 dB, the two measurements almost certainly describe different material (a different note, or a decay versus a "
	  "sustained sound): make a moderate change (or leave the level alone) and say in the explanation why the level cannot "
	  "be matched exactly. Also check the true peak: a level change must not push it above 0 dB.  If the request is relative and no "
	  "earlier measurement exists, say so in one clause of the explanation instead of pretending to match a level you "
	  "cannot see.\n"
	  "\n"
	  "The current values are listed in the user message, and they are the truth. If this conversation says you changed "
	  "a value but that list still shows the old value, the user undid the change: never assume a previous change is "
	  "still active, and always give an absolute value when you change something.\n"
	  "Corrections: a user message that says the last reply was wrong or was not what they meant ('I meant ...', 'no, "
	  "not that', 'that's not what I asked', 'undo that') is a correction. For a correction, first revert the changes "
	  "listed under 'Applied in that turn' in the previous user message by setting those controls back to their old "
	  "values (a reverted On/Off switch goes back to its old value too), then apply what the user actually asked for. "
	  "Leave out the reverts for controls the new request changes anyway. Do not stack the new request on top of a "
	  "reply the user rejected.\n"
	  "Repeated complaints: a user message that reports the same problem again ('still there', 'it's still there', 'didn't "
	  "help', 'not sure') means your last change did not address the cause; do not keep guessing one new control per turn. "
	  "Read what the earlier attempts already changed (the 'Applied in that turn' lines), rule those causes out, and change "
	  "the one plausible cause that is left. If you cannot name the cause, set \"ask\" to one question that narrows it down "
	  "(when it happens, on which notes, before or after the attack) instead of changing something unrelated. When the way "
	  "to remove a symptom is to switch off a feature the user asked for earlier (a granulator, a loop, a voice), say so in "
	  "the explanation and switch it back on - or ask - once the symptom is gone.");
}

// Reference for the built-in system effects: exact name, unit, range and what
// each effect does. The descriptions never change, so this is part of the
// cached prompt prefix; only the current values go into the user message.
static inline QString fx_system_effect_reference(void)
{
	QString ret = "\n\nEffect reference (exact name [unit; allowed range]; what it does). "
	              "The current value of every effect is listed in the user message:\n";

	// The seven note duplicator voices have identical parameters, so they are
	// listed once, as "Voice N", with the shared explanation above them.
	QSet<QString> shown_voice_params;
	bool voice_header_shown = false;

	for (const FxParam &param : fx_catalog())
	{
		QString name = param.name;

		const int voice_pos = name.indexOf(" Voice ");
		if (voice_pos > 0)
		{
			const QString suffix = name.mid(voice_pos + 7);
			bool is_number = !suffix.isEmpty();
			for (const QChar c : suffix)
				if (!c.isDigit())
					is_number = false;
			if (is_number)
			{
				const QString template_name = name.left(voice_pos) + " Voice N";
				if (shown_voice_params.contains(template_name))
					continue;
				shown_voice_params.insert(template_name);

				if (!voice_header_shown)
				{
					ret += "Note duplicator voices (N = 1 to 7): voice 1 is the instrument's original note, voices 2-7 play extra "
					       "copies of every note. All voices are mixed into ONE signal and every system effect and bus send processes "
					       "that combined signal (never per voice); changing any voice parameter switches that voice on. Turning "
					       "voice 1 (or all voices) off silences the instrument completely.\n";
					voice_header_shown = true;
				}

				name = template_name;
			}
		}

		ret += "- " + name + " [" + fx_param_unit(param) + "; range " + fx_format_range(param) + "] " + param.description + "\n";
	}

	return ret;
}

// The words in the request that could name an effect plugin ("wah" in "add a
// wah effect"). Very short and very common words are dropped, since they
// would match almost every plugin name.
static inline QStringList fx_request_keywords(const QString &request)
{
	const QSet<QString> stopwords{
	  "the", "and", "for", "with", "this", "that", "from", "into", "like", "make", "made",
	  "sound", "sounds", "sounding", "more", "less", "some", "very", "much", "little", "bit",
	  "add", "adds", "remove", "take", "put", "please", "give", "use", "insert", "effect",
	  "effects", "plugin", "plugins", "instrument", "now", "not", "want", "would", "could",
	  "should", "have", "has", "its", "his", "her", "our", "your", "about", "than", "then",
	  "there", "here", "what", "when", "which", "while", "will", "can", "get", "got", "just",
	  "really", "still", "track", "song", "mix", "way", "one", "two", "out", "off", "over",
	  "under", "again", "back", "also", "all", "any", "but", "how", "let", "need", "only",
	  "other", "same", "them", "they", "too", "was", "were", "you", "after", "before",
	  // Imperative verbs of a request. "Keep same volume" would otherwise
	  // match the plugins named "Keep Left channel only"/"Keep Right channel
	  // only" (~/.radium/llm.log 2026-11-14T12:01:24).
	  "keep", "keeps", "keeping", "stay", "stays", "maintain", "leave", "turn", "turns",
	  "change", "changes", "changing", "try", "trying", "fix", "fixing", "help", "helps",
	};

	// Words of a plugin name that are too generic to identify one. Without
	// this, "hz" in "set modulation to 4 hz" matches "DC Offset Remover" (no,
	// it does not - but "de" or "hz" appear inside plenty of plugin names), and
	// a two-letter unit basically matches by accident.
	const QSet<QString> weak_words{
	  "hz", "khz", "db", "eq", "lp", "hp", "ms", "fx", "vst", "vst3", "ladspa", "lv2",
	  "mono", "stereo", "left", "right", "mid", "side", "in", "out", "up", "down",
	  "lfo", "amt", "vol", "gain", "mix", "wet", "dry", "rate", "sync", "mode", "type",
	};

	QStringList ret;
	QString word;

	// Generic audio words and units never identify a plugin, however long:
	// "set modulation to 4 hz" must not suggest "DC Offset Remover" (the "hz"
	// in ... nothing) or "TAP Tremolo" (the "mod" in "Modulator").
	const QSet<QString> generic_words{
	  "modulat", "modulate", "modulation", "frequency", "freq", "speed", "depth",
	  "amount", "volume", "panning", "pan", "filtering", "filter", "distorted",
	  "distortion", "reverbed", "chorused", "delayed", "echoes", "wobble", "sweep",
	  "sounds", "sound", "loud", "quiet", "volume", "level", "setting", "value",
	};

	auto flush_word = [&]()
	{
		if (word.size() >= 3 && !stopwords.contains(word) && !weak_words.contains(word)
		    && !generic_words.contains(word) && !ret.contains(word))
			ret.append(word);
		word.clear();
	};

	for (const QChar c : request.toLower())
	{
		if (c.isLetterOrNumber())
			word.append(c);
		else
			flush_word();
	}
	flush_word();

	return ret;
}

// True when the request could be asking for this plugin by name. Plugin names
// are matched ignoring case and punctuation, and a keyword may match only a
// part of the name ("wah" matches "Auto Wah", "reverb" matches "ZRev" only if
// the words overlap, so keep the keywords concrete).
static inline bool fx_plugin_name_matches_keywords(const FxPlugin &plugin, const QStringList &keywords)
{
	// Compare whole words ("reverb" matches "Reverb" and "Reverb 2", but not
	// the tail of an unrelated name), which is what a user means when the
	// request and the plugin share a word.
	const QStringList words = plugin.name.toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts);
	if (words.isEmpty())
		return false;

	for (const QString &keyword : keywords)
	{
		if (keyword.size() < 3)
			continue;

		// A long keyword ("autotune") matches one whole word of the name
		// ("Auto-Tune" -> "auto", "tune"): it is the joined words.
		QString joined;
		for (const QString &word : words)
			joined += word;
		if (keyword.size() >= 5 && joined.startsWith(keyword))
			return true;

		// A short keyword ("reverb", "wah") must be a word of the name, or a
		// prefix of one ("reverb" -> "reverb", "dist" -> "distortion"). Never
		// a substring: "Tune" is not "Delay".
		for (const QString &word : words)
			if (word.startsWith(keyword))
				return true;
	}

	return false;
}

// The installed effect plugins whose names match the request, as a list. The
// full catalog is several hundred entries (17 KB of prompt text, sent with
// every request) and is almost never needed: a subjective mixing request
// rarely asks for a plugin by name. The host still validates "add_plugins"
// against the complete catalog, so nothing is lost if a plugin is added whose
// name is not listed.
static inline QVector<FxPlugin> fx_matching_plugins(const QVector<FxPlugin> &plugins, const QString &request)
{
	QVector<FxPlugin> matches;

	const QStringList keywords = fx_request_keywords(request);
	if (keywords.isEmpty())
		return matches;

	for (const FxPlugin &plugin : plugins)
		if (fx_plugin_name_matches_keywords(plugin, keywords))
			matches.append(plugin);

	return matches;
}

// True when a request that mentions an effect is already served by the current
// instrument or by the built-in system effects: "compress like X" on a
// compressor (or with 'System Compression' available), "add some reverb" when
// the Reverb bus exists. The effect-plugin catalogue is then not needed, and it
// is the largest per-request part of the prompt (a common word like "compress"
// matches two dozen installed plugins).
//
// A request that explicitly asks for a plugin is never treated as covered: the
// user wants a plugin then, and the model needs the names.
static inline bool fx_request_is_covered_by_instrument(const QString &request,
                                                       const FxInstrumentContext &instrument,
                                                       const QVector<FxParamState> &states)
{
	const QString lower = request.toLower();

	// Explicit plugin requests must see the catalogue.
	const QStringList plugin_words{"plugin", "vst", "vst3", "ladspa", "faust effect", "insert a", "add a plugin"};
	for (const QString &word : plugin_words)
		if (lower.contains(word))
			return false;

	const QStringList keywords = fx_request_keywords(request);
	if (keywords.isEmpty())
		return false;

	// Words of the current instrument's control names (and descriptions).
	QStringList instrument_words;
	for (const FxInstrumentParamState &param : instrument.params)
		for (const QString &word : (param.name + " " + param.display_name).toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts))
			instrument_words.append(word);
	for (const QString &word : instrument.description.toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts))
		instrument_words.append(word);

	// Words of the built-in system effects.
	QStringList system_words;
	for (const FxParam &param : fx_catalog())
		for (const QString &word : param.name.toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts))
			system_words.append(word);
	for (const FxParamState &state : states)
		for (const QString &word : state.param.name.toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts))
			system_words.append(word);

	for (const QString &keyword : keywords)
	{
		// "compress" should also match "compression" / "compressor".
		const int keep = keyword.size() >= 6 ? keyword.size() - 2 : keyword.size();
		const QString stem = keyword.left(keep);
		if (stem.size() < 4)
			continue;

		for (const QString &word : instrument_words)
			if (word.size() >= 4 && word.startsWith(stem))
				return true;

		for (const QString &word : system_words)
			if (word.size() >= 4 && word.startsWith(stem))
				return true;
	}

	return false;
}

// The family key of a plugin name: "compressor-lr" -> "compressor-",
// "mb-compressor-mono" -> "mb-compressor-", "Auto Wah" -> "" (no family).
// Only a short trailing token counts as a variant suffix, so ordinary names
// with a space are not grouped.
static inline QString fx_plugin_family_key(const FxPlugin &plugin, QString *suffix)
{
	const QString &name = plugin.name;

	// The last '-' or '_' separated token, when it is short and alphanumeric.
	int separator = -1;
	for (int i = name.size() - 1; i >= 0; i--)
		if (name.at(i) == '-' || name.at(i) == '_'){
			separator = i;
			break;
		}

	if (separator <= 0 || separator >= name.size() - 1)
		return QString();

	const QString tail = name.mid(separator + 1);
	if (tail.size() > 10)
		return QString();
	for (const QChar c : tail)
		if (!c.isLetterOrNumber())
			return QString();

	if (suffix != NULL)
		*suffix = tail;

	return name.left(separator + 1);
}

// The per-request part of the prompt: the installed effect plugins whose names
// match the request. It goes into the *user* message (at the very end), not
// into the system prompt: it is the only part that depends on the request, and
// keeping the system prompt completely static means a prefix cache can reuse
// it - including everything after it, which is the whole conversation.
static inline QString fx_plugin_catalog_section(const QVector<FxPlugin> &plugins, const QString &request)
{
	QString ret;

	if (plugins.isEmpty())
		return "\nAvailable effect plugins (exact name; type) that can be inserted after this instrument:\n"
		       "(no effect plugins are currently available)\n";

	const QVector<FxPlugin> matches = fx_matching_plugins(plugins, request);

	if (matches.isEmpty())
	{
		// Nothing matched: do not dump "N of the N installed effect plugins
		// are hidden" - the request does not name a plugin, so there is
		// nothing to look up. Keep it to one short line.
		return "\nAvailable effect plugins (exact name; type) that can be inserted after this instrument:\n"
		       "(none whose name matches this request; the installed plugins are not listed)\n";
	}
	else
	{
		ret += "\nAvailable effect plugins (exact name; type) that can be inserted after this instrument, "
		       "whose names match this request:\n";

		// Group the variants of one plugin set ("compressor-lr", "-mono",
		// "-ms", "-stereo") on a single line. Every exact name is kept - the
		// model has to copy one - only the repeated type text is shared.
		QHash<QString, QVector<const FxPlugin*>> families;
		QStringList family_order;
		for (const FxPlugin &plugin : matches)
		{
			QString suffix;
			const QString key = fx_plugin_family_key(plugin, &suffix);
			const QString full_key = key.isEmpty() ? QString() : plugin.type_name + "\n" + key;
			if (full_key.isEmpty())
				continue;

			if (!families.contains(full_key))
				family_order.append(full_key);
			families[full_key].append(&plugin);
		}

		QSet<QString> grouped_names; // handled by a family line
		QHash<QString, QString> family_lines;
		for (const QString &key : family_order)
		{
			const QVector<const FxPlugin*> members = families.value(key);
			if (members.size() < 2)
				continue;

			QStringList names;
			for (const FxPlugin *plugin : members){
				names.append(plugin->name);
				grouped_names.insert(plugin->name + "\n" + plugin->type_name);
			}

			// The container is only useful when it differs from the name.
			const FxPlugin *first = members.at(0);
			QString line = "- " + names.join(", ") + " [" + first->type_name + "]";
			if (!first->container_name.isEmpty() && first->container_name != first->name)
				line += " (container: " + first->container_name + ")";

			family_lines.insert(key, line);
		}

		const int max_listed = 60;
		int listed = 0;
		QSet<QString> emitted_families;

		for (int i = 0; i < matches.size(); i++)
		{
			if (listed >= max_listed)
			{
				ret += QString("- (and %1 more)\n").arg(matches.size() - listed);
				break;
			}

			const FxPlugin &plugin = matches.at(i);

			if (grouped_names.contains(plugin.name + "\n" + plugin.type_name))
			{
				QString suffix;
				const QString key = fx_plugin_family_key(plugin, &suffix);
				const QString full_key = plugin.type_name + "\n" + key;
				if (emitted_families.contains(full_key))
					continue; // already printed with its family
				emitted_families.insert(full_key);
				ret += family_lines.value(full_key) + "\n";
				listed += families.value(full_key).size();
				continue;
			}

			ret += "- " + plugin.name + " [" + plugin.type_name + "]";
			// A container that equals the name is noise: it can never be what
			// distinguishes this plugin from another one.
			if (!plugin.container_name.isEmpty() && plugin.container_name != plugin.name)
				ret += " (container: " + plugin.container_name + ")";
			ret += "\n";
			listed++;
		}
	}

	if (matches.size() < plugins.size())
		ret += QString("%1 of the %2 installed effect plugins are hidden because their names do not match this request. "
		               "Never guess a hidden plugin name: if the request needs an effect plugin that is not listed, set "
		               "\"create_faust\" instead.\n").arg(plugins.size() - matches.size()).arg(plugins.size());

	return ret;
}

// The system prompt: the static rules and the static effect reference. Both
// are identical from request to request (no per-request data at all), so a
// prefix cache can reuse them. Everything that varies - the current values,
// the matching effect plugins, the history - is in the user message and after
// it.
static inline QString build_fx_system_prompt(void)
{
	// Order matters for prefix caching: the schema line and the effect
	// reference rarely change, while the rules are edited regularly. Putting
	// the rules last means such an edit only invalidates the tail of the
	// cached prefix.
	return build_fx_system_prompt_intro()
	     + fx_system_effect_reference()
	     + build_fx_system_prompt_rules();
}

// True for the generic MIDI controller controls that plugin wrappers expose
// ("Breath (CH 3)", "ModWheel (CH 12)", ...). They cannot shape the sound of a
// mixing change, and for some plugins they are almost the whole control list
// (112 of the 127 listed Uhbik-A controls), so they are left out of the prompt.
static inline bool fx_is_midi_controller_param(const FxInstrumentParamState &param)
{
	QString name = param.name.toLower().trimmed();

	// Drop a leading "12: " index prefix, as added by the plugin wrappers.
	int digits = 0;
	while (digits < name.size() && name.at(digits).isDigit())
		digits++;
	if (digits > 0 && digits < name.size() && (name.at(digits) == ':' || name.at(digits) == '.' || name.at(digits) == ')'))
		name = name.mid(digits + 1).trimmed();

	// "Breath (CH 3)" / "ModWheel (CH 12)" (u-he VST2).
	if (name.contains("(ch "))
		return true;

	// "CC 5, CH 8" (u-he VST3 exposes one such control for every MIDI CC on
	// every channel: ~1900 for Presswerk, which crowded the real ~64 controls
	// out of the prompt). Also "CC 5 CH 8" and "CC5, CH8".
	{
		int p = 0;
		auto skip_spaces = [&name, &p]()
		{
			while (p < name.size() && name.at(p) == ' ')
				p++;
		};

		if (name.startsWith("cc")){
			p = 2;
			skip_spaces();
			const int number_start = p;
			while (p < name.size() && name.at(p).isDigit())
				p++;
			const bool has_cc_number = p > number_start;
			skip_spaces();
			const bool has_separator = p < name.size() && (name.at(p) == ',' || name.at(p) == ';' || name.at(p) == ':');
			if (has_separator)
				p++;
			skip_spaces();

			if (has_cc_number && name.mid(p).startsWith("ch"))
				return true;
		}
	}

	const QStringList midi_controls{"breath", "pitchbend", "modwheel", "aftertouch", "expression", "sustainpedal", "bankselect", "program"};
	return midi_controls.contains(name);
}

// How much of a plugin's own control description is sent. Plugin labels are
// short ("Room size"), descriptions can be a paragraph; the per-control text is
// multiplied by up to 256 controls, so it is capped hard (this is prompt text
// sent with every request, i.e. real money).
static const int FX_CONTROL_DESCRIPTION_MAX = 90;

// Byte budget for the instrument-control section of the user message (~5 KB,
// about 1.2k tokens). A typical plugin needs 1-2 KB, so this only kicks in for
// plugins with very many controls, where it keeps every request bounded.
static const int FX_CONTROL_SECTION_BUDGET = 5000;

// True when every letter of 'a' also occurs in 'b' (as a multiset), i.e. 'a'
// could be 'b' with letters dropped or reordered. This is how plugin UIs
// abbreviate parameter names, and how u-he's VST3 shortTitle is built: "Densty"
// for "Density" (dropped vowels), "Trebel" for "Treble" (swapped), "TrFrq" for
// "TrebleFreq", "OutDB" for "OutputDB".
static inline bool fx_letters_are_contained(const QString &a, const QString &b)
{
	if (a.isEmpty())
		return true;

	QHash<QChar, int> available;
	for (const QChar c : b)
		available[c] = available.value(c) + 1;

	for (const QChar c : a)
		if (available.value(c) <= 0)
			return false;
		else
			available[c] = available.value(c) - 1;

	return true;
}

static inline QString fx_compact_letters(const QString &text)
{
	QString ret;
	for (const QChar c : text)
		if (c.isLetterOrNumber())
			ret.append(c.toLower());
	return ret;
}

// True when the plugin's description text actually tells the model something
// the control name does not. Abbreviations of the name ("Densty" for
// "Density") are not descriptions: they are noise, they cost prompt space, and
// they hide the fact that the plugin provides nothing (which then suppresses
// the "based on the control's name" hedge).
static inline bool fx_description_is_useful(const FxInstrumentParamState &param)
{
	if (param.description.isEmpty())
		return false;

	const QString description = fx_compact_letters(param.description);
	const QString name = fx_compact_letters(param.display_name.isEmpty() ? param.name : param.display_name);

	if (description.size() < 4 || description == name)
		return false;

	// A shortened (or letter-swapped) form of the control name: nothing the
	// model can not already read from the name.
	if (fx_letters_are_contained(description, name))
		return false;

	return true;
}

static inline QString fx_control_description(const FxInstrumentParamState &param)
{
	if (!fx_description_is_useful(param))
		return QString();

	QString description = param.description.simplified();
	if (description.size() > FX_CONTROL_DESCRIPTION_MAX){
		description = description.left(FX_CONTROL_DESCRIPTION_MAX - 1).trimmed() + QChar(0x2026); // ellipsis
	}
	return description;
}

// True when every control has the same format and the same scale (clamped
// 0..1 or a native range with the same unit), i.e. the per-line
// "[continuous; position ...; displayed ...]" boilerplate can be stated once
// instead of 64 times. Mixed groups keep the verbose per-line form.
static inline bool fx_controls_are_homogeneous(const QVector<const FxInstrumentParamState*> &params)
{
	if (params.size() < 2)
		return false;

	const FxInstrumentParamState *first = params.at(0);
	if (first->format != FX_INSTRUMENT_PARAM_FLOAT)
		return false; // on/off, choice and integer controls need their own line

	for (const FxInstrumentParamState *param : params)
	{
		if (param->format != first->format)
			return false;
		if (param->is_clamped != first->is_clamped)
			return false;
		if (param->has_native_range != first->has_native_range)
			return false;
		if (param->unit != first->unit)
			return false;
	}

	return true;
}

// The value part of a compact control line: "- 2: Size = 1.000 (100.00)".
// Only used when the group is homogeneous (see above), so the reader already
// knows the type and the scale from the section header.
static inline QString fx_compact_control_line(const FxInstrumentParamState &param)
{
	QString ret = "- " + (param.display_name.isEmpty() ? param.name : param.display_name);
	if (!param.display_name.isEmpty() && param.display_name != param.name)
		ret += " [control name: " + param.name + "]";

	if (param.has_native_range){
		ret += " = " + QString::number(param.current_native, 'g', 8);
		if (!param.unit.isEmpty())
			ret += " " + param.unit;
		if (!param.current_display.isEmpty() && param.current_display != QString::number(param.current_native, 'g', 8))
			ret += " (displayed " + param.current_display + ")";
	}else{
		ret += " = " + QString::number(param.current_scaled, 'f', 3);
		if (!param.current_display.isEmpty())
			ret += " (" + param.current_display + ")";
		if (!param.unit.isEmpty() && !param.current_display.contains(param.unit, Qt::CaseInsensitive))
			ret += " [" + param.unit + "]";
	}

	if (!fx_control_description(param).isEmpty())
		ret += " " + fx_control_description(param);

	return ret;
}

// The default value of a numeric catalog effect (the middle of its range).
static inline float fx_param_default_value(const FxParam &param)
{
	return (param.min_native + param.max_native) / 2.0f;
}

static inline bool fx_is_default_value(const FxParam &param, float value)
{
	const float tolerance = std::max(0.0005f, std::fabs(param.max_native - param.min_native) * 0.0005f);
	return std::fabs(value - fx_param_default_value(param)) <= tolerance;
}

// A short form for effect values the user does not need to see because they
// are at their defaults, e.g. "volume 0.00 dB, start 0 ms". Returns an empty
// string when the value differs from the default.
static inline QString fx_default_value_hint(const FxParam &param, float value)
{
	const QStringList units{"Hz", "dB", "ms", "semitones", "degrees", "pan", "chance", "mode", "number", ":1"};
	if (!units.contains(fx_param_unit(param)))
		return QString();

	return fx_format_value(param, value);
}

// "Name of the effect without the voice number" ("System Volume Voice 7" ->
// "System Volume Voice") plus the voice number (7), or -1 for a control that
// does not belong to a voice.
static inline int fx_voice_number(const QString &name, QString *template_name)
{
	const int voice_pos = name.indexOf(" Voice ");
	if (voice_pos <= 0)
		return -1;

	const QString suffix = name.mid(voice_pos + 7);
	if (suffix.isEmpty())
		return -1;
	for (const QChar c : suffix)
		if (!c.isDigit())
			return -1;

	if (template_name != NULL)
		*template_name = name.left(voice_pos) + " Voice N";

	return suffix.toInt();
}

// The note duplicator voices 2-7 have seven values each (42 lines of prompt
// text) and are usually all off with the same settings, which is pure noise.
// They are condensed to one line per parameter that keeps every value that is
// not the default (transpose is often not), so nothing is lost and an "add an
// octave with voice 2" request still sees the transpose it will get.
static inline QString fx_format_voice_values(const QVector<FxParamState> &states)
{
	QString ret;

	// Template name ("System Volume Voice N") -> the entries for voices 2-7,
	// in catalog order.
	QStringList template_order;
	QHash<QString, QVector<const FxParamState*>> by_template;
	for (const FxParamState &state : states)
	{
		QString template_name;
		const int voice = fx_voice_number(state.param.name, &template_name);
		if (voice <= 1)
			continue;

		if (!by_template.contains(template_name))
			template_order.append(template_name);
		by_template[template_name].append(&state);
	}

	QStringList lines;
	for (const QString &template_name : template_order)
	{
		const QVector<const FxParamState*> &entries = by_template.value(template_name);

		// A modulated voice is never "just a default": list its full rows.
		bool has_modulator = false;
		for (const FxParamState *entry : entries)
			if (entry->has_modulator)
				has_modulator = true;
		if (has_modulator)
		{
			for (const FxParamState *entry : entries)
			{
				QString row = "- " + entry->param.name + " = " + fx_format_value(entry->param, entry->current_native);
				if (entry->has_modulator)
					row += " [modulated]";
				lines.append(row);
			}
			continue;
		}

		// The first voice is the reference: the row lists only the voices
		// whose value differs from it, so six identical defaults cost one
		// short line instead of six.
		const QString common_value = fx_format_value(entries.at(0)->param, entries.at(0)->current_native);

		QStringList pairs;
		for (const FxParamState *entry : entries)
		{
			QString unused;
			const int voice = fx_voice_number(entry->param.name, &unused);
			const QString value = fx_format_value(entry->param, entry->current_native);
			if (value == common_value)
				continue;
			pairs.append(QString("%1 = %2").arg(voice).arg(value));
		}

		if (pairs.isEmpty())
			lines.append(QString("- %1 2-7 = all %2").arg(template_name, common_value));
		else
			lines.append(QString("- %1 = %2 on 2-7, except: %3").arg(template_name, common_value, pairs.join(", ")));
	}

	if (lines.isEmpty())
		return ret;

	ret += "\nNote duplicator voices 2-7, condensed (voice 1 is listed above and is the original note; voices 2-7 are extra copies; "
	       "a row shows the setting shared by all six, then only the voices whose value differs):\n";
	ret += lines.join("\n");
	ret += "\n(Change one setting on one voice by its full name, e.g. 'System Volume Voice 3', and always set that voice's 'System On/Off Voice N' to 1.)\n";
	return ret;
}

// True when the request asks for information ("what does this plugin do?",
// "please explain Density and Size", "why is it muddy?") rather than for a
// change. Those must be answered, never tuned - and the last instruction of
// the user message (the instrument-first reminder) must not contradict that.
static inline bool fx_request_is_a_question(const QString &request)
{
	const QString lower = request.toLower().trimmed();

	// A trailing question mark is the clearest signal.
	if (lower.endsWith('?') || lower.endsWith('?'))
		return true;

	const QStringList question_starts{
	  "what", "whats", "what's", "which", "who", "how", "why", "when", "where",
	  "explain", "describe", "tell me", "is ", "are ", "does ", "do ", "can you explain",
	  "please explain", "please describe", "help me understand",
	};
	for (const QString &start : question_starts)
		if (lower.startsWith(start))
			return true;

	// A question can also be phrased without a leading interrogative
	// ("curious what the mix does", "I'd like to know whether ...").
	const QStringList question_markers{"explain", "what does", "what is", "what are", "what mix", "what size",
	                                   "means", "meaning of", "know what", "know how", "difference between"};
	for (const QString &marker : question_markers)
		if (lower.contains(marker))
			return true;

	return false;
}

// True when the request shares no word with any control name or system effect,
// i.e. it describes a sound or a place ("make it sound like a small radio")
// rather than naming a parameter.
static inline bool fx_request_names_no_control(const QString &request, const FxInstrumentContext &instrument)
{
	const QStringList words = fx_request_keywords(request);
	if (words.isEmpty())
		return true;

	QStringList control_names;
	for (const FxInstrumentParamState &param : instrument.params)
	{
		control_names.append(param.name);
		control_names.append(param.display_name);
	}
	for (const FxParam &param : fx_catalog())
		control_names.append(param.name);

	for (const QString &name : control_names)
		for (const QString &word : name.toLower().split(QRegularExpression("[^a-z0-9]+"), Qt::SkipEmptyParts))
			if (word.size() >= 3 && words.contains(word))
				return false;

	return true;
}

// The user message: the request, the instrument's own controls (first, since
// they take precedence), the system effect values, the inserted effect
// plugins, and finally the effect plugins whose names match the request.
// Deliberately free of descriptions (they are in the system prompt), so it
// stays small and the cached prefix is not invalidated.
static inline QString build_fx_user_message(const QVector<FxParamState> &states, const FxInstrumentContext &instrument, const QVector<FxPlugin> &plugins, const QVector<FxPlugin> &inserted_plugins, const QString &prompt)
{
	QString ret = "User request: " + fx_quote(prompt) + "\n\n";

	ret += "Current instrument plugin: ";
	if (instrument.plugin_name.isEmpty())
		ret += "(unknown)\n";
	else
	{
		ret += instrument.plugin_name + " [" + instrument.type_name + "]";
		if (!instrument.description.isEmpty())
			ret += " (" + instrument.description.left(160) + ")";
		ret += "\n";
	}

	// Song timing: lets the model convert notes <-> milliseconds and the
	// modulator's cycles-per-beat <-> Hz without guessing the tempo.
	if (instrument.bpm > 0.0){
		ret += QString("\nSong tempo: %1 BPM%2; one beat = %3 ms, one bar = %4 ms.\n")
		  // Radium builds with -Werror=float-equal: compare the rounded value.
		  .arg(instrument.bpm, 0, 'f', qAbs(instrument.bpm - qRound(instrument.bpm)) < 0.005 ? 0 : 2)
		  .arg(instrument.signature_numerator > 0
		         ? QString(", %1/%2 signature").arg(instrument.signature_numerator).arg(instrument.signature_denominator)
		         : QString())
		  .arg((int)qRound(60000.0 / instrument.bpm))
		  .arg((int)qRound(60000.0 / instrument.bpm * instrument.signature_numerator * 4.0 / std::max(1, instrument.signature_denominator)));
	}

	// Measured facts, when the instrument has been audible recently. This is the
	// only part of the prompt that is not a guess about the sound.
	if (instrument.measurement.valid && !instrument.measurement.bands.isEmpty()){
		const FxInstrumentMeasurement &m = instrument.measurement;
		const bool stale = m.age_ms > 5000;
		const bool clipping = m.true_peak_db > 0.0f;

		ret += QString("\nWhat it currently measures (the instrument's own output, %1 Hz%2):\n")
		  .arg(m.sample_rate)
		  .arg(stale ? QString("; measured %1 s ago").arg(m.age_ms / 1000) : QString());

		// Loudness is the figure to match levels with; the true peak says
		// whether there is headroom.
		{
			// 'loudest' is the level of the sound; the window average drops when
			// the window contains silence, and 'now' can be inside a decay.
			const bool decaying = m.loudness_momentary_lufs < m.loudness_loudest_lufs - 6.0f;

			ret += QString("- loudness %1 LUFS (at its loudest, 400 ms), %2 LUFS (window average), %3 LUFS (right now)%4; "
			               "true peak %5 dB%6; crest %7 dB\n")
			  .arg(m.loudness_loudest_lufs, 0, 'f', 1)
			  .arg(m.loudness_window_lufs, 0, 'f', 1)
			  .arg(m.loudness_momentary_lufs, 0, 'f', 1)
			  .arg(decaying ? QString(" - the instrument is decaying or quiet right now") : QString())
			  .arg(m.true_peak_db, 0, 'f', 1)
			  .arg(clipping ? QString(", ABOVE full scale: the output is clipping") : QString())
			  .arg(m.crest_db, 0, 'f', 1);
		}

		// Stereo: always, because "no width at all" is exactly what a
		// "wider"/"narrower" request has to know about.
		{
			ret += QString("- stereo: correlation %1, %2, balance %3 dB (right minus left)\n")
			  .arg(m.stereo_correlation, 0, 'f', 2)
			  .arg(m.stereo_side_db <= -60.0f
			       ? QString("no side signal (the material is mono)")
			       : QString("side %1 dB below mid").arg(-m.stereo_side_db, 0, 'f', 1))
			  .arg(m.stereo_balance_db, 0, 'f', 1);
		}

		// Timbre and noise.
		ret += QString("- shape: centroid %1 Hz, flatness %2 (0 = tonal, 1 = noise-like)")
		  .arg((int)qRound(m.centroid_hz))
		  .arg(m.flatness, 0, 'f', 2);
		if (m.hum_hz > 0.0f)
			ret += QString("; hum at %1 Hz, %2 dB").arg((int)qRound(m.hum_hz)).arg(m.hum_db, 0, 'f', 1);
		if (m.dc_offset > 0.002f || m.dc_offset < -0.002f)
			ret += QString("; DC offset %1").arg(m.dc_offset, 0, 'f', 3);
		if (m.sub_bass_db > -60.0f)
			ret += QString("; energy below 30 Hz %1 dB").arg(m.sub_bass_db, 0, 'f', 1);
		ret += "\n";

		// Space: how long the sound takes to die away. This is the figure for
		// "more/less reverb", "drier", "bigger room".
		if (m.decay_time_s > 0.0f || m.tail_db < 0.0f){
			const double window_s = m.sample_rate > 0 ? (double)m.num_frames / (double)m.sample_rate : 0.0;
			QString decay_text;
			if (m.decay_time_s <= 0.0f)
				decay_text = "not measurable";
			else if (window_s > 0.1 && m.decay_time_s >= window_s * 0.9)
				decay_text = QString("longer than the %1 s window").arg(window_s, 0, 'f', 1);
			else
				decay_text = QString("decay %1 s (60 dB)").arg(m.decay_time_s, 0, 'f', 2);

			ret += QString("- space: %1, level 300 ms after the peak %2 dB\n")
			  .arg(decay_text)
			  .arg(m.tail_db, 0, 'f', 1);
		}

		// Transients.
		if (m.onsets_per_second > 0.0f || m.attack_db_per_ms > 0.0f)
			ret += QString("- transients: attack %1 dB/ms, %2 onsets/s\n")
			  .arg(m.attack_db_per_ms, 0, 'f', 1)
			  .arg(m.onsets_per_second, 0, 'f', 1);

		ret += QString("- band levels (dB): %1.\n").arg(m.bands.join(", "));
	}

	ret += "\nNote: names with a number suffix are separate instances of one parameter (\"T0\"..\"T7\" = the "
	       "individual delay/filter taps or reverb tank stages, \"AP0\"..\"AP3\" = individual allpass stages, "
	       "\"Feedback1\"/\"Feedback2\" = separate feedback paths). Prefer the named controls (size, decay, wet, dry, "
	       "damp, diffusion, filter, speed, depth) unless the user asks for a specific instance.\n";

	ret += "\nCurrent instrument controls. Change these first when they fit the request. A name of the form 'Layer: Control' "
	       "(for example 'Rev1: Size') belongs to a section of the plugin; always use the full name, exactly as listed. "
	       "Names ending in ' 1'/' 2' (or '1'/'2') are two instances of the same control, typically two compressor stages or "
	       "two filter banks: change one and say which, unless the user asks for both.\n";

	QVector<const FxInstrumentParamState*> visible_params;
	int hidden_midi_controls = instrument.hidden_midi_controls;
	for (const FxInstrumentParamState &param : instrument.params)
	{
		if (fx_is_midi_controller_param(param))
			hidden_midi_controls++;
		else
			visible_params.append(&param);
	}

	// Compact mode: every control shares its type and scale, so that is stated
	// once instead of on every line (see the header below).
	const bool compact_values = fx_controls_are_homogeneous(visible_params);

	if (visible_params.isEmpty())
	{
		ret += "(no visible plugin-specific controls)\n";
	}
	else
	{
		// Prompt budget for the whole control section. The per-control text is
		// what makes a plugin with many controls expensive, and this prompt is
		// sent on every turn, so it must stay bounded whatever the plugin looks
		// like. Once the budget is used up, the remaining controls are listed
		// by name only (they can still be changed, just not with a full line).
		const int section_budget = FX_CONTROL_SECTION_BUDGET;
		const int section_start_size = ret.size();
		QStringList overflow_names;

		// State the type and the scale once when every control shares it: the
		// per-line "[continuous; position ...; displayed ...]" boilerplate is
		// ~40 of the ~80 characters per line, and it repeats for every control.
		if (compact_values){
			const FxInstrumentParamState *first = visible_params.at(0);
			ret += "Every control below is continuous; ";
			if (first->has_native_range)
				ret += QString("value = native value, unit %1").arg(first->unit.isEmpty() ? QString("none") : first->unit);
			else
				ret += "value = position 0 to 1 (the plugin reports no range)";
			ret += ", then the plugin's own display in parentheses.\n";
		}

		for (const FxInstrumentParamState *param : visible_params)
		{
			if (ret.size() - section_start_size > section_budget){
				overflow_names.append(param->display_name.isEmpty() ? param->name : param->display_name);
				continue;
			}

			if (compact_values){
				ret += fx_compact_control_line(*param) + "\n";
				continue;
			}

			ret += "- " + (param->display_name.isEmpty() ? param->name : param->display_name);
			if (!param->display_name.isEmpty() && param->display_name != param->name)
				ret += " [control name: " + param->name + "]";
			ret += " [" + fx_instrument_param_format_name(param->format);
			if (param->is_clamped && param->format != FX_INSTRUMENT_PARAM_BOOL && param->format != FX_INSTRUMENT_PARAM_RADIO){
				// No range read from the plugin: the normalized 0..1 range
				// would just be a scale, and must not be read as a unit. The
				// explanation is a single footnote at the end of the section,
				// not repeated on every line (31 controls x 100 chars).
				ret += "; position " + QString::number(param->current_scaled, 'f', 3);
				if (!param->current_display.isEmpty())
					ret += "; displayed " + param->current_display;
				// The unit, when the plugin reports one that the display text
				// does not already show ("position 0.5" + "%" vs "displayed 50 %").
				if (!param->unit.isEmpty() && !param->current_display.contains(param->unit, Qt::CaseInsensitive))
					ret += "; unit " + param->unit;
				ret += "]";
				if (!fx_control_description(*param).isEmpty())
					ret += " " + fx_control_description(*param);
				ret += "\n";
				continue;
			}
			if (param->has_native_range){
				ret += "; native range " + QString::number(param->min_native, 'g', 8) + " to " + QString::number(param->max_native, 'g', 8);
				if (!param->unit.isEmpty())
					ret += " " + param->unit;
				ret += "; current native " + QString::number(param->current_native, 'g', 8);
				if (!param->unit.isEmpty())
					ret += " " + param->unit;
				ret += "; normalized " + QString::number(param->current_scaled, 'f', 6);
			}else{
				ret += "; normalized range 0 to 1; current " + QString::number(param->current_scaled, 'f', 6);
			}
			if (!param->current_display.isEmpty())
				ret += "; displayed " + param->current_display;
			ret += "]";
			if (!fx_control_description(*param).isEmpty())
				ret += " " + fx_control_description(*param);
			ret += "\n";
		}

		if (!overflow_names.isEmpty())
		{
			// The overflow list is capped too: 400 names are as expensive
			// as 400 full lines would have been.
			const int max_overflow_names = 24;
			const QStringList shown = overflow_names.mid(0, max_overflow_names);
			const QString more = overflow_names.size() > max_overflow_names
			                   ? QString(" (+%1 more)").arg(overflow_names.size() - max_overflow_names)
			                   : QString();
			ret += QString("(%1 controls are listed by name only because the list is capped to keep the request small: %2%3. "
			               "Ask for one if you need its value.)\n").arg(overflow_names.size()).arg(shown.join(", "), more);
		}
	}

	if (instrument.omitted_params > 0)
		ret += QString("(%1 additional controls omitted because the list is limited)\n").arg(instrument.omitted_params);
	if (hidden_midi_controls > 0)
		ret += QString("(%1 MIDI controller controls (Breath, PitchBend, ModWheel, AfterTouch, Expression, SustainPedal, BankSelect) "
		               "are not listed: they do not shape the sound of a mixing change.)\n").arg(hidden_midi_controls);

	// Whether the descriptions below come from the plugin or are missing
	// entirely decides how much the model may assert about the controls.
	{
		int described = 0;
		for (const FxInstrumentParamState *param : visible_params)
			if (fx_description_is_useful(*param))
				described++;

		if (!visible_params.isEmpty() && described == 0)
			ret += "(This plugin provides no descriptions, units or ranges for its controls; only their names are known. "
			       "When answering a question about a control, say that the explanation is based on the control's name.)\n";
	}

	if (!compact_values){
		// (The hoisted header already says this when the compact form is used.)
		int clamped_count = 0;
		for (const FxInstrumentParamState *param : visible_params)
			if (param->is_clamped)
				clamped_count++;
		if (clamped_count > 0)
			ret += QString("(%1 of these controls report no unit or range from the plugin: their value is a relative position 0 to 1, "
			               "the 'displayed' text is the plugin's own scale; use values as relative amounts.)\n").arg(clamped_count);
	}

	ret += "\nCurrent system effect values (exact name = current value; the effect reference in the system prompt lists unit, range and what each effect does):\n";
	for (const FxParamState &state : states)
	{
		QString template_name;
		if (fx_voice_number(state.param.name, &template_name) > 1)
			continue; // condensed by fx_format_voice_values below

		ret += "- " + state.param.name + " = " + fx_format_value(state.param, state.current_native);
		if (state.has_modulator)
			ret += " [modulated]";
		ret += "\n";
	}
	ret += fx_format_voice_values(states);

	ret += "\nEffect plugins currently inserted after this instrument (exact name; type):\n";
	if (inserted_plugins.isEmpty())
	{
		ret += "(none)\n";
	}
	else
	{
		for (const FxPlugin &plugin : inserted_plugins)
		{
			ret += "- " + plugin.name + " [" + plugin.type_name + "]";
			if (!plugin.container_name.isEmpty())
				ret += " (container: " + plugin.container_name + ")";
			ret += "\n";
		}
	}

	// The request-dependent part goes last: everything before it is either
	// static (the system prompt) or small, so a prefix cache can still reuse
	// the system prompt and the history. When the request is already served by
	// the instrument or the built-in effects, the catalogue is skipped: it is
	// the largest per-request part, and a common word like "compress" matches
	// two dozen installed plugins.
	if (fx_request_is_covered_by_instrument(prompt, instrument, states))
		ret += "\n(No effect-plugin list: the current instrument or the built-in system effects already "
		       "cover this request. Do not insert a plugin for it.)\n";
	else
		ret += fx_plugin_catalog_section(plugins, prompt);

	// A "vibe only" request ("make it sound like we're in Abbey Road") has no
	// obvious parameter, and a model forced to emit JSON can stall on it.
	// Rather than leaving it to guess, name the way out.
	if (fx_request_names_no_control(prompt, instrument)){
		ret += "\nThis request names no control. If you can map it to the listed controls with confidence, do so; "
		       "otherwise reply with an empty action set and set \"ask\" to one short question that offers the most "
		       "likely choices. Never return an empty reply.";
	}

	ret += "\nThe container is only for disambiguation; include it in your reply only when needed for same-named plugins.";
	ret += "\nRemember: reply with only the JSON object. Values are absolute.";

	// The final instruction is the one the model reads last, so it must agree
	// with the rules above. Telling it "a request that names the plugin is
	// answered with instrument_changes" made explanation questions ("what
	// does this plugin do?") into tuning requests, which the JSON output gate
	// of the free relay then rejects.
	if (fx_request_is_a_question(prompt)){
		ret += " This is a question: answer it in \"answer\" (one sentence, at most 200 characters) and change nothing.";
	}else{
		ret += " A request that names the plugin ('the plugin', 'the instrument') is answered with "
		       "\"instrument_changes\"; system effects are the exception, not the default.";
	}
	return ret;
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const FxInstrumentContext &instrument, const QVector<FxPlugin> &inserted_plugins, const QString &prompt)
{
	return build_fx_user_message(states, instrument, QVector<FxPlugin>(), inserted_plugins, prompt);
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const FxInstrumentContext &instrument, const QString &prompt)
{
	return build_fx_user_message(states, instrument, QVector<FxPlugin>(), QVector<FxPlugin>(), prompt);
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QVector<FxPlugin> &plugins, const QVector<FxPlugin> &inserted_plugins, const QString &prompt)
{
	return build_fx_user_message(states, FxInstrumentContext(), plugins, inserted_plugins, prompt);
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QVector<FxPlugin> &inserted_plugins, const QString &prompt)
{
	return build_fx_user_message(states, FxInstrumentContext(), QVector<FxPlugin>(), inserted_plugins, prompt);
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QString &prompt)
{
	return build_fx_user_message(states, FxInstrumentContext(), QVector<FxPlugin>(), QVector<FxPlugin>(), prompt);
}

struct FxParseResult
{
	bool ok = false;
	QString error;
	QString explanation;
	QVector<FxChange> changes;
	QVector<FxInstrumentChange> instrument_changes;
	QVector<FxModulation> modulations;
	QVector<FxPlugin> plugins;
	QVector<FxPlugin> removed_plugins;
	QString create_faust;
	QString create_faust_name;
	QString ask; // a clarifying question for the user (only set when nothing is changed)
	QString answer; // an answer to a question the user asked (no changes are made)
	QStringList warnings;
};

// Parses one "add_plugins"/"remove_plugins" array and validates the entries
// against 'catalog'. Type can be omitted when the name is unambiguous.
// Matching ignores case, spaces and punctuation, so "Auto-Wah" finds
// "Auto Wah".
static inline void fx_parse_plugin_array(const QJsonValue &value,
                                         const QVector<FxPlugin> &catalog,
                                         const QString &array_name,
                                         bool is_remove,
                                         QVector<FxPlugin> &out,
                                         QStringList &warnings)
{
	QHash<QString, const FxPlugin*> by_full_key;
	QHash<QString, QVector<const FxPlugin*>> by_type_and_name;
	QHash<QString, QVector<const FxPlugin*>> by_name;

	auto get_type_and_name_key = [](const QString &type_name, const QString &name)
	{
		return fx_normalize_plugin_key(type_name) + "\n" + fx_normalize_plugin_key(name);
	};

	auto get_full_key = [&get_type_and_name_key](const FxPlugin &plugin)
	{
		return fx_normalize_plugin_key(plugin.container_name) + "\n" + get_type_and_name_key(plugin.type_name, plugin.name);
	};

	for (const FxPlugin &plugin : catalog)
	{
		by_full_key.insert(get_full_key(plugin), &plugin);
		by_type_and_name[get_type_and_name_key(plugin.type_name, plugin.name)].append(&plugin);
		by_name[fx_normalize_plugin_key(plugin.name)].append(&plugin);
	}

	QSet<QString> added;

	for (const QJsonValue &plugin_value : value.toArray())
	{
		if (!plugin_value.isObject())
		{
			warnings.append(QString("Ignored a %1 entry that is not an object.").arg(array_name));
			continue;
		}

		const QJsonObject plugin_object = plugin_value.toObject();
		const QString name = plugin_object.value("name").toString().trimmed();
		const QString type_name = plugin_object.value("type").toString().trimmed();
		const QString container_name = plugin_object.value("container").toString().trimmed();
		if (name.isEmpty())
		{
			warnings.append(QString("Ignored a %1 entry without a plugin name.").arg(array_name));
			continue;
		}

		const FxPlugin *plugin = NULL;

		if (!container_name.isEmpty())
		{
			FxPlugin requested;
			requested.type_name = type_name;
			requested.name = name;
			requested.container_name = container_name;
			plugin = by_full_key.value(get_full_key(requested), NULL);
			if (plugin == NULL)
			{
				warnings.append(is_remove
				                ? QString("Ignored '%1' (type '%2', container '%3'): no such effect plugin is inserted after this instrument.").arg(name, type_name, container_name)
				                : QString("Ignored unknown effect plugin '%1' (type '%2', container '%3').").arg(name, type_name, container_name));
				continue;
			}
		}
		else if (!type_name.isEmpty())
		{
			const QVector<const FxPlugin*> matches = by_type_and_name.value(get_type_and_name_key(type_name, name));
			if (matches.size() == 1)
			{
				plugin = matches.at(0);
			}
			else if (matches.isEmpty())
			{
				warnings.append(is_remove
				                ? QString("Ignored '%1' (type '%2'): no such effect plugin is inserted after this instrument.").arg(name, type_name)
				                : QString("Ignored unknown effect plugin '%1' (type '%2').").arg(name, type_name));
				continue;
			}
			else
			{
				warnings.append(QString("Ignored effect plugin '%1' (type '%2'): several containers provide it; the container must be provided.").arg(name, type_name));
				continue;
			}
		}
		else
		{
			const QVector<const FxPlugin*> matches = by_name.value(fx_normalize_plugin_key(name));

			if (matches.size() == 1)
			{
				plugin = matches.at(0);
			}
			else if (matches.isEmpty())
			{
				warnings.append(is_remove
				                ? QString("Ignored '%1': no such effect plugin is inserted after this instrument.").arg(name)
				                : QString("Ignored unknown effect plugin '%1'.").arg(name));
				continue;
			}
			else
			{
				warnings.append(QString("Ignored effect plugin '%1': several plugins have that name; the type/container must be provided.").arg(name));
				continue;
			}
		}

		const QString key = get_full_key(*plugin);
		if (added.contains(key))
			continue;

		added.insert(key);
		out.append(*plugin);
	}
}

// The value part of a plugin's display text. Plugin wrappers often prefix it
// with the port name ("Bass: 200.00", "pitch_glide_length: 420"), so a leading
// "<name>: " is dropped. Returns an empty string when the text is empty.
static inline QString fx_display_text_value(const QString &display_text)
{
	QString display = display_text.trimmed();
	if (display.isEmpty())
		return QString();

	const QRegularExpression prefix(QStringLiteral("^.*:\\s*"));
	return display.remove(prefix);
}

// The value text of a plugin control. Returns the current display text, or an
// empty string when the host did not read one.
static inline QString fx_parameter_display(const FxInstrumentParamState *param)
{
	return fx_display_text_value(param->current_display);
}

// A plugin's display texts are opaque ("200", "35.00", "On"), but they are
// almost always a number with some prefix or suffix ("Mix: 40"). For a
// normalized control the new value is interpolated into the last number of the
// text, proportionally to the normalized change. Returns an empty string when
// the display is not numeric (booleans, choices, texts) or when the current
// value is at a range endpoint (nothing to scale from): those are summarized as
// percentages instead.
static inline QString fx_interpolate_display(const QString &display, float old_scaled, float new_scaled)
{
	if (!(old_scaled > 0.000001f && old_scaled < 0.999999f))
		return QString(); // cannot scale from an endpoint

	// Find the last number in the text ("200.00", "Mix: 40", "-12.5 dB").
	int end = -1;
	for (int i = display.size() - 1; i >= 0; i--)
	{
		if (display.at(i).isDigit()){
			end = i + 1;
			break;
		}
	}
	if (end < 0)
		return QString();

	int start = end;
	while (start > 0)
	{
		const QChar c = display.at(start - 1);
		const bool previous_is_digit = start - 1 > 0 && display.at(start - 2).isDigit();
		if (c.isDigit() || c == '.' || (c == '-' && previous_is_digit))
			start--;
		else
			break;
	}

	QString number_text = display.mid(start, end - start);
	bool ok = false;
	double old_display_value = number_text.toDouble(&ok);
	if (!ok){
		// A trailing dot was probably punctuation; retry without it.
		number_text.chop(1);
		old_display_value = number_text.toDouble(&ok);
		end--;
	}
	if (!ok || old_display_value < 0.0)
		return QString(); // negative: a proportional scale would be invented

	int decimals = 0;
	const int dot = number_text.indexOf('.');
	if (dot >= 0)
		decimals = number_text.size() - dot - 1;

	double new_display_value = old_display_value * ((double)new_scaled / (double)old_scaled);
	// Avoid "-0.00" and "168.0000000001".
	if (std::fabs(new_display_value) < 0.5 * std::pow(10.0, -decimals))
		new_display_value = 0.0;
	const QString new_number = QString::number(new_display_value, 'f', std::min(6, decimals));
	return display.left(start) + new_number + display.mid(end);
}

// Parses and validates an LLM reply. Unknown names and non-numeric values are
// dropped; out-of-range values are clamped and reported in 'warnings'. System
// effect values are native, while instrument plugin controls are normalized.
// Plugins must be in the supplied catalogs; unknown plugins are dropped.
static inline FxParseResult parse_fx_response(const QString &raw,
                                              const QVector<FxParamState> &states,
                                              const QVector<FxPlugin> &plugins,
                                              const QVector<FxPlugin> &inserted_plugins,
                                              const FxInstrumentContext &instrument)
{
	FxParseResult result;

	QString text = raw.trimmed();
	const int start = text.indexOf('{');
	const int end = text.lastIndexOf('}');
	if (start < 0 || end <= start)
	{
		result.error = "The reply does not contain a JSON object.";
		return result;
	}

	QJsonParseError parse_error;
	const QJsonDocument doc = QJsonDocument::fromJson(text.mid(start, end - start + 1).toUtf8(), &parse_error);
	if (doc.isNull() || !doc.isObject())
	{
		result.error = "Invalid JSON: " + parse_error.errorString();
		return result;
	}

	const QJsonObject root = doc.object();
	const QJsonValue changes_value = root.value("changes");
	const QJsonValue instrument_changes_value = root.value("instrument_changes");
	const QJsonValue modulators_value = root.value("modulators");
	const QJsonValue plugins_value = root.value("add_plugins");
	const QJsonValue remove_plugins_value = root.value("remove_plugins");
	const QJsonValue create_faust_value = root.value("create_faust");
	const QJsonValue create_faust_name_value = root.value("create_faust_name");
	const QJsonValue ask_value = root.value("ask");
	const QJsonValue answer_value = root.value("answer");
	if (changes_value.isUndefined() && instrument_changes_value.isUndefined() && modulators_value.isUndefined()
	    && plugins_value.isUndefined() && remove_plugins_value.isUndefined()
	    && create_faust_value.isUndefined() && ask_value.isUndefined() && answer_value.isUndefined())
	{
		result.error = "The JSON object has no action ('changes', 'instrument_changes', 'modulators', 'add_plugins', 'remove_plugins', 'create_faust', 'ask' or 'answer').";
		return result;
	}
	if (!changes_value.isUndefined() && !changes_value.isArray())
	{
		result.error = "'changes' is not an array.";
		return result;
	}
	if (!instrument_changes_value.isUndefined() && !instrument_changes_value.isArray())
	{
		result.error = "'instrument_changes' is not an array.";
		return result;
	}
	if (!modulators_value.isUndefined() && !modulators_value.isArray())
	{
		result.error = "'modulators' is not an array.";
		return result;
	}
	if (!plugins_value.isUndefined() && !plugins_value.isArray())
	{
		result.error = "'add_plugins' is not an array.";
		return result;
	}
	if (!remove_plugins_value.isUndefined() && !remove_plugins_value.isArray())
	{
		result.error = "'remove_plugins' is not an array.";
		return result;
	}
	if (!create_faust_value.isUndefined() && !create_faust_value.isNull() && !create_faust_value.isString())
	{
		result.error = "'create_faust' is not a string.";
		return result;
	}
	if (!create_faust_name_value.isUndefined() && !create_faust_name_value.isNull() && !create_faust_name_value.isString())
	{
		result.error = "'create_faust_name' is not a string.";
		return result;
	}

	QHash<QString, const FxParamState*> by_name;
	QHash<QString, const FxParamState*> by_normalized_name;
	QSet<QString> ambiguous_normalized_names;
	for (const FxParamState &state : states)
	{
		by_name.insert(state.param.name.toLower(), &state);

		for (const QString &key : fx_control_keys(state.param.name))
		{
			if (by_normalized_name.contains(key))
			{
				by_normalized_name.remove(key);
				ambiguous_normalized_names.insert(key);
			}
			else if (!ambiguous_normalized_names.contains(key))
				by_normalized_name.insert(key, &state);
		}
	}

	// Exact (case-insensitive) name first, then the name with the plugin
	// wrapper's index prefix and the punctuation removed.
	auto find_state = [&by_name, &by_normalized_name](const QString &name) -> const FxParamState*
	{
		const FxParamState *state = by_name.value(name.toLower(), NULL);
		if (state != NULL)
			return state;

		for (const QString &key : fx_control_keys(name))
		{
			state = by_normalized_name.value(key, NULL);
			if (state != NULL)
				return state;
		}

		return NULL;
	};

	QHash<QString, FxChange> resolved;
	QStringList order;

	for (const QJsonValue &value : changes_value.toArray())
	{
		if (!value.isObject())
		{
			result.warnings.append("Ignored a change that is not an object.");
			continue;
		}

		const QJsonObject change_object = value.toObject();
		const QString name = change_object.value("name").toString().trimmed();
		if (name.isEmpty())
		{
			result.warnings.append("Ignored a change without a name.");
			continue;
		}

		const QJsonValue value_json = change_object.value("value");
		if (!value_json.isDouble() && !value_json.isBool())
		{
			result.warnings.append(QString("Ignored '%1': the value is missing or is not a number.").arg(name));
			continue;
		}

		double new_value = value_json.isBool()
		                   ? (value_json.toBool() ? 1.0 : 0.0)
		                   : value_json.toDouble();

		const FxParamState *state = find_state(name);
		if (state == NULL)
		{
			result.warnings.append(QString("Ignored unknown effect '%1'.").arg(name));
			continue;
		}

		const FxParam &param = state->param;

		if (param.is_bool)
		  new_value = new_value >= 0.5 ? 1.0 : 0.0;
		else if (param.is_int)
		  new_value = qRound(new_value);

		if (new_value < param.min_native)
		{
			result.warnings.append(QString("Clamped '%1' from %2 to the minimum %3.").arg(param.name).arg(new_value).arg(param.min_native));
			new_value = param.min_native;
		}
		if (new_value > param.max_native)
		{
			result.warnings.append(QString("Clamped '%1' from %2 to the maximum %3.").arg(param.name).arg(new_value).arg(param.max_native));
			new_value = param.max_native;
		}

		if (std::fabs(new_value - state->current_native) < 0.0001)
		  continue; // Not a change.

		FxChange change;
		change.name = param.name;
		change.value = (float)new_value;

		const QString key = param.name.toLower();
		if (!resolved.contains(key))
		  order.append(key);
		resolved.insert(key, change);
	}

	// Effect plugins to insert/remove ("add_plugins"/"remove_plugins").
	// Matching ignores case, spacing and punctuation, so "Auto-Wah" works.
	if (!plugins_value.isUndefined())
		fx_parse_plugin_array(plugins_value, plugins, "add_plugins", false, result.plugins, result.warnings);

	if (!remove_plugins_value.isUndefined())
		fx_parse_plugin_array(remove_plugins_value, inserted_plugins, "remove_plugins", true, result.removed_plugins, result.warnings);

	// Modulation actions ("modulators" array).
	auto read_optional_double = [&result](const QJsonObject &object, const char *key, double *out) -> bool
	{
		const QJsonValue value = object.value(QString::fromUtf8(key));
		if (value.isUndefined())
		  return false;
		if (!value.isDouble())
		{
			result.warnings.append(QString("Ignored '%1': not a number.").arg(QString::fromUtf8(key)));
			return false;
		}
		*out = value.toDouble();
		return true;
	};

	for (const QJsonValue &value : modulators_value.toArray())
	{
		if (!value.isObject())
		{
			result.warnings.append("Ignored a modulator entry that is not an object.");
			continue;
		}

		const QJsonObject modulator_object = value.toObject();
		const QString effect_name = modulator_object.value("effect").toString().trimmed();
		if (effect_name.isEmpty())
		{
			result.warnings.append("Ignored a modulator entry without an effect name.");
			continue;
		}

		const FxParamState *state = find_state(effect_name);
		if (state == NULL)
		{
			result.warnings.append(QString("Ignored modulation of unknown effect '%1'.").arg(effect_name));
			continue;
		}
		if (state->param.is_bool)
		{
			result.warnings.append(QString("Ignored modulation of the On/Off effect '%1' (only numeric effects can be modulated).").arg(state->param.name));
			continue;
		}

		const QString action_string = modulator_object.value("action").toString().trimmed().toLower();
		FxModulation::Action action = FxModulation::ADD;
		if (action_string == "remove")
		  action = FxModulation::REMOVE;
		else if (action_string == "replace")
		  action = FxModulation::REPLACE;
		else if (!action_string.isEmpty() && action_string != "add")
		{
			result.warnings.append(QString("Ignored modulation of '%1': unknown action '%2'.").arg(state->param.name, action_string));
			continue;
		}

		if (action == FxModulation::ADD && state->has_modulator)
		{
			result.warnings.append(QString("Ignored 'add' for '%1': it already has a modulator (use 'replace' or 'remove').").arg(state->param.name));
			continue;
		}
		if (action == FxModulation::REMOVE && !state->has_modulator)
		{
			result.warnings.append(QString("Ignored 'remove' for '%1': it has no modulator.").arg(state->param.name));
			continue;
		}
		if (action == FxModulation::REPLACE && !state->has_modulator)
		  action = FxModulation::ADD; // Forgiving: replace on an unmodulated effect becomes add.

		FxModulation modulation;
		modulation.action = action;
		modulation.effect = state->param.name;

		if (action != FxModulation::REMOVE)
		{
			const QString type = modulator_object.value("type").toString().trimmed().toLower();
			if (!type.isEmpty())
			{
				if (fx_modulator_type_index(type) < 0)
					result.warnings.append(QString("Unknown modulator type '%1' for '%2'; using the default sine.").arg(type, state->param.name));
				else
					modulation.type = type;
			}

			double number = 0.0;
			if (read_optional_double(modulator_object, "tempo_numerator", &number))
				modulation.tempo_numerator = std::min(32, std::max(1, qRound(number)));
			if (read_optional_double(modulator_object, "tempo_denominator", &number))
				modulation.tempo_denominator = std::min(32, std::max(1, qRound(number)));
			if (read_optional_double(modulator_object, "min", &number))
				modulation.min_scaled = std::min(1.0f, std::max(0.0f, (float)number));
			if (read_optional_double(modulator_object, "max", &number))
				modulation.max_scaled = std::min(1.0f, std::max(0.0f, (float)number));
		}

		result.modulations.append(modulation);
	}

	result.explanation = root.value("explanation").toString().trimmed();
	result.ask = root.value("ask").toString().trimmed();
	result.answer = root.value("answer").toString().trimmed();
	result.create_faust = create_faust_value.toString().trimmed();
	result.create_faust_name = create_faust_name_value.toString().trimmed();

	for (const QString &key : order)
	  result.changes.append(resolved.value(key));

	QHash<QString, const FxInstrumentParamState*> instrument_params_by_name;
	QHash<QString, const FxInstrumentParamState*> instrument_params_by_display_name;
	QSet<QString> ambiguous_instrument_display_names;
	// Normalized keys, so the model's readable name ("Size") also finds the
	// wrapper's exact name ("2: Size"). A key shared by two controls is
	// dropped instead of resolving to an arbitrary one.
	QHash<QString, const FxInstrumentParamState*> instrument_params_by_normalized_key;
	QSet<QString> ambiguous_normalized_keys;

	auto add_normalized_key = [&instrument_params_by_normalized_key, &ambiguous_normalized_keys](const QString &text, const FxInstrumentParamState *param)
	{
		for (const QString &key : fx_control_keys(text))
		{
			const FxInstrumentParamState *existing = instrument_params_by_normalized_key.value(key, NULL);
			if (existing != NULL && existing != param)
			{
				instrument_params_by_normalized_key.remove(key);
				ambiguous_normalized_keys.insert(key);
			}
			else if (!ambiguous_normalized_keys.contains(key))
			{
				instrument_params_by_normalized_key.insert(key, param);
			}
		}
	};

	for (const FxInstrumentParamState &param : instrument.params)
	{
		instrument_params_by_name.insert(param.name.toLower(), &param);
		add_normalized_key(param.name, &param);

		if (!param.display_name.isEmpty() && param.display_name.toLower() != param.name.toLower())
		{
			const QString key = param.display_name.toLower();
			if (instrument_params_by_display_name.contains(key))
			{
				instrument_params_by_display_name.remove(key);
				ambiguous_instrument_display_names.insert(key);
			}
			else if (!ambiguous_instrument_display_names.contains(key))
				instrument_params_by_display_name.insert(key, &param);
		}
		add_normalized_key(param.display_name, &param);
	}

	QHash<QString, FxInstrumentChange> resolved_instrument_changes;
	QStringList instrument_change_order;
	for (const QJsonValue &value : instrument_changes_value.toArray())
	{
		if (!value.isObject())
		{
			result.warnings.append("Ignored an instrument change that is not an object.");
			continue;
		}

		const QJsonObject change_object = value.toObject();
		const QString name = change_object.value("name").toString().trimmed();
		if (name.isEmpty())
		{
			result.warnings.append("Ignored an instrument change without a name.");
			continue;
		}

		const QJsonValue value_json = change_object.value("value");
		if (!value_json.isDouble() && !value_json.isBool())
		{
			result.warnings.append(QString("Ignored instrument control '%1': the value is missing or is not a number.").arg(name));
			continue;
		}

		const QString key = name.toLower();
		const FxInstrumentParamState *param = instrument_params_by_name.value(key, NULL);
		if (param == NULL && ambiguous_instrument_display_names.contains(key))
		{
			result.warnings.append(QString("Ignored current-instrument control '%1': its displayed name is ambiguous; use the exact control name shown in brackets.").arg(name));
			continue;
		}
		if (param == NULL)
			param = instrument_params_by_display_name.value(key, NULL);
		if (param == NULL)
			for (const QString &candidate : fx_control_keys(name))
			{
				param = instrument_params_by_normalized_key.value(candidate, NULL);
				if (param != NULL)
					break;
			}
		if (param == NULL)
		{
			result.warnings.append(QString("Ignored unknown current-instrument control '%1'.").arg(name));
			continue;
		}

		double new_value = value_json.isBool()
		                   ? (value_json.toBool() ? 1.0 : 0.0)
		                   : value_json.toDouble();

		if (param->has_native_range)
		{
			if (param->format == FX_INSTRUMENT_PARAM_BOOL)
				new_value = new_value >= 0.5 ? param->max_native : param->min_native;
			else if (param->format == FX_INSTRUMENT_PARAM_INT)
				new_value = qRound(new_value);

			if (new_value < param->min_native)
			{
				result.warnings.append(QString("Clamped instrument control '%1' from %2 to its minimum %3.").arg(param->name).arg(new_value).arg(param->min_native));
				new_value = param->min_native;
			}
			if (new_value > param->max_native)
			{
				result.warnings.append(QString("Clamped instrument control '%1' from %2 to its maximum %3.").arg(param->name).arg(new_value).arg(param->max_native));
				new_value = param->max_native;
			}
			if (std::fabs(new_value - param->current_native) < 0.000001)
				continue;
		}
		else
		{
			if (param->format == FX_INSTRUMENT_PARAM_BOOL)
				new_value = new_value >= 0.5 ? 1.0 : 0.0;

			if (new_value < 0.0)
			{
				result.warnings.append(QString("Clamped instrument control '%1' from %2 to 0.").arg(param->name).arg(new_value));
				new_value = 0.0;
			}
			if (new_value > 1.0)
			{
				result.warnings.append(QString("Clamped instrument control '%1' from %2 to 1.").arg(param->name).arg(new_value));
				new_value = 1.0;
			}
			if (std::fabs(new_value - param->current_scaled) < 0.0001)
				continue;
		}

		FxInstrumentChange change;
		change.type_name = instrument.type_name;
		change.plugin_name = instrument.plugin_name;
		change.plugin_version = instrument.plugin_version;
		change.effect_num = param->effect_num;
		change.name = param->name;
		change.display_name = param->display_name;
		change.old_value_scaled = param->current_scaled;
		change.value_scaled = param->has_native_range
		                    ? (float)((new_value - param->min_native) / (param->max_native - param->min_native))
		                    : (float)new_value;
		change.value_is_native = param->has_native_range;
		change.old_value_native = param->current_native;
		change.value_native = (float)new_value;
		change.unit = param->unit;

		// The plugin's own value text before the change. It is used by the
		// summary, and it is what the host reads the new text back for after
		// applying: a clamped control's display scale is unknown, so its new
		// text cannot be interpolated, only read. Whatever stays unfilled here
		// is summarized as a normalized position.
		change.old_display = fx_parameter_display(param);
		if (!param->has_native_range && !param->is_clamped && !param->current_display.isEmpty())
		{
			// new_value is native for a native-range control and the scaled
			// value otherwise; only the latter is evaluated here.
			const QString new_display = fx_interpolate_display(change.old_display, param->current_scaled, (float)new_value);
			if (!new_display.isEmpty() && new_display != change.old_display)
				change.new_display = new_display;
		}

		const QString param_key = param->name.toLower();
		if (!resolved_instrument_changes.contains(param_key))
			instrument_change_order.append(param_key);
		resolved_instrument_changes.insert(param_key, change);
	}

	for (const QString &key : instrument_change_order)
		result.instrument_changes.append(resolved_instrument_changes.value(key));

	result.ok = true;
	return result;
}

static inline FxParseResult parse_fx_response(const QString &raw,
                                              const QVector<FxParamState> &states,
                                              const QVector<FxPlugin> &plugins)
{
	return parse_fx_response(raw, states, plugins, QVector<FxPlugin>(), FxInstrumentContext());
}

static inline FxParseResult parse_fx_response(const QString &raw,
	                                          const QVector<FxParamState> &states,
	                                          const QVector<FxPlugin> &plugins,
	                                          const QVector<FxPlugin> &inserted_plugins)
{
	return parse_fx_response(raw, states, plugins, inserted_plugins, FxInstrumentContext());
}

static inline FxParseResult parse_fx_response(const QString &raw, const QVector<FxParamState> &states)
{
	return parse_fx_response(raw, states, QVector<FxPlugin>(), QVector<FxPlugin>(), FxInstrumentContext());
}

// If a band parameter is changed while its band is off, the change would be
// inaudible. Returns the extra changes needed to switch those bands on
// (unless the model already set the on/off effect itself in the same reply).
static inline QVector<FxChange> fx_band_enable_changes(const QVector<FxParamState> &states, const QVector<FxChange> &changes)
{
	QVector<FxChange> ret;

	QHash<QString, const FxParamState*> by_name;
	for (const FxParamState &state : states)
		by_name.insert(state.param.name.toLower(), &state);

	QSet<QString> changed_names;
	for (const FxChange &change : changes)
		changed_names.insert(change.name.toLower());

	QSet<QString> added_names;
	for (const FxChange &change : changes)
	{
		const FxParamState *state = by_name.value(change.name.toLower(), NULL);
		if (state == NULL || state->param.band_onoff_name.isEmpty())
		  continue;

		const QString onoff_name = state->param.band_onoff_name;
		const QString key = onoff_name.toLower();
		if (changed_names.contains(key) || added_names.contains(key))
		  continue; // The model already set it, or we added it.

		const FxParamState *onoff_state = by_name.value(key, NULL);
		if (onoff_state == NULL || onoff_state->current_native >= 0.5f)
		  continue; // Already on (or not in the catalog).

		added_names.insert(key);
		ret.append(FxChange{onoff_name, state->param.band_onoff_value});
	}

	return ret;
}

// A short "old -> new" summary of the applied changes.
static inline QString fx_summarize_changes(const QVector<FxParamState> &states, const QVector<FxChange> &changes, int max_changes = 4)
{
	QHash<QString, const FxParamState*> by_name;
	for (const FxParamState &state : states)
		by_name.insert(state.param.name.toLower(), &state);

	QStringList parts;
	int count = 0;
	for (const FxChange &change : changes)
	{
		const FxParamState *state = by_name.value(change.name.toLower(), NULL);

		if (count == max_changes)
		{
			parts.append(QString("and %1 more").arg(changes.size() - count));
			break;
		}

		// Entries added by the apply step alongside the parameter changes
		// (the global effects switch, bus send connections) are not part of
		// the catalog and thus have no state.
		if (state == NULL && change.name.endsWith(" On/Off"))
		{
			parts.append(QString("%1 off -> %2").arg(change.name, change.value >= 0.5f ? "on" : "off"));
			count++;
			continue;
		}

		const QString old_value = (state != NULL)
		                          ? fx_format_value(state->param, state->current_native)
		                          : QString("?");
		const QString new_value = (state != NULL)
		                          ? fx_format_value(state->param, change.value)
		                          : QString::number(change.value, 'f', 2);

		parts.append(QString("%1 %2 -> %3").arg(fx_control_display_name(change.name), old_value, new_value));
		count++;
	}

	return parts.join(", ");
}

// A normalized 0..1 position as a percentage, with only the decimals the value
// needs: "0.2%" instead of a rounded "0%", "66%" instead of "66.00%".
// Rounding to whole percent made real changes read as no-ops in the applied
// summary ("Start Position 0% -> 0%" for 0.002, ~/.radium/llm.log
// 2026-11-14T12:00:35).
static inline QString fx_format_position_percent(float scaled)
{
	QString text = QString::number(scaled * 100.0, 'f', 2);
	if (text.contains('.')) {
		while (text.endsWith('0'))
			text.chop(1);
		if (text.endsWith('.'))
			text.chop(1);
	}
	return text + "%";
}

static inline QString fx_summarize_instrument_changes(const QVector<FxInstrumentChange> &changes, int max_changes = 4)
{
	QStringList parts;
	for (int i = 0; i < changes.size(); i++)
	{
		if (i == max_changes)
		{
			parts.append(QString("and %1 more").arg(changes.size() - i));
			break;
		}

		const FxInstrumentChange &change = changes.at(i);
		const QString instrument = change.plugin_name.isEmpty()
		                          ? QString("current instrument")
		                          : change.plugin_name;
		const QString param_name = fx_control_display_name(change.display_name.isEmpty() ? change.name : change.display_name);
		if (change.value_is_native){
			const QString old_value = QString::number(change.old_value_native, 'g', 8);
			const QString new_value = QString::number(change.value_native, 'g', 8);
			const QString unit = change.unit.isEmpty() ? QString() : " " + change.unit;
			parts.append(QString("%1: %2 %3 -> %4")
			             .arg(instrument, param_name)
			             .arg(old_value + unit, new_value + unit));
		}else if (!change.old_display.isEmpty() && !change.new_display.isEmpty()){
			// The plugin's own numbers ("200" -> "170", "0 ms" -> "40 ms") read
			// better than the normalized positions ("100%" -> "85%").
			parts.append(QString("%1: %2 %3 -> %4")
			             .arg(instrument, param_name, change.old_display, change.new_display));
		}else{
			parts.append(QString("%1: %2 %3 -> %4")
			             .arg(instrument, param_name,
			                  fx_format_position_percent(change.old_value_scaled),
			                  fx_format_position_percent(change.value_scaled)));
		}
	}
	return parts.join(", ");
}

// The GUI/instrument operations the session needs. Implemented by the
// instrument widget.
struct FxTuneSessionHost
{
	std::function<QVector<FxParamState>()> read_catalog;
	std::function<FxInstrumentContext()> read_instrument_context;
	// Effect plugins that can be inserted into the signal path. The host
	// reads this from the plugin registry.
	std::function<QVector<FxPlugin>()> read_plugin_catalog;
	// Effect plugins currently inserted after this instrument, in chain
	// order. The host reads this from the mixer graph.
	std::function<QVector<FxPlugin>()> read_inserted_plugins;
	// Applies the whole reply (effect value changes, modulation actions and
	// plugin insertions/removals) as ONE undo step. May extend 'changes' with
	// the extra changes it makes itself (e.g. enabling a band that was off),
	// so the final status summary includes them. It fills in the
	// 'new_display' text of an instrument change whose new value text could
	// not be computed, so the summary can use the plugin's own units.
	std::function<bool(QVector<FxChange> &changes,
	                   QVector<FxInstrumentChange> &instrument_changes,
	                   const QVector<FxModulation> &modulations,
	                   const QVector<FxPlugin> &add_plugins,
	                   const QVector<FxPlugin> &remove_plugins)> apply_reply;
	// Asks the user in the LLM terminal whether to create a Faust Dev 2
	// instrument for an effect that no plugin can provide, and starts the
	// creation if the answer is yes. Returns true when the question was shown
	// (the user answers asynchronously), false on failure.
	std::function<bool(const QString &description, const QString &name)> create_faust_effect;
	std::function<void(const QString &text)> status;
	std::function<void(const QString &message)> show_error;
	std::function<void(bool busy)> set_busy;
};

static inline QString fx_plugin_display_name(const FxPlugin &plugin)
{
	QString ret = plugin.name + " (" + plugin.type_name;
	if (!plugin.container_name.isEmpty())
		ret += ", " + plugin.container_name;
	ret += ")";
	return ret;
}

// Turns a Faust effect description ("a smooth autotune effect with medium
// retune speed") into a short instrument name ("Smooth autotune").
static inline QString fx_make_faust_instrument_name(const QString &description)
{
	QString name = description.simplified();
	if (name.isEmpty())
		return "Faust Effect";

	const QStringList articles = {"a ", "an ", "the "};
	for (const QString &article : articles)
	{
		if (name.startsWith(article, Qt::CaseInsensitive))
		{
			name = name.mid(article.size());
			break;
		}
	}

	const QStringList cut_markers = {" with ", " using ", " for ", " that "};
	for (const QString &marker : cut_markers)
	{
		const int pos = name.indexOf(marker, 0, Qt::CaseInsensitive);
		if (pos > 0)
		{
			name = name.left(pos);
			break;
		}
	}

	const QStringList suffixes = {" effect", " plugin", " processor", " fx"};
	for (const QString &suffix : suffixes)
	{
		if (name.endsWith(suffix, Qt::CaseInsensitive))
		{
			name.chop(suffix.size());
			break;
		}
	}

	name = name.trimmed();
	while (!name.isEmpty() && (name.endsWith('.') || name.endsWith(',') || name.endsWith(';') || name.endsWith(':')))
		name.chop(1);

	name = name.trimmed();
	if (name.isEmpty() || name.compare("effect", Qt::CaseInsensitive) == 0)
		return "Faust Effect";

	if (name.size() > 48)
		name = name.left(48).trimmed();

	name[0] = name[0].toUpper();
	return name;
}

// A short summary of the effect plugins inserted by the reply.
static inline QString fx_summarize_plugins(const QVector<FxPlugin> &plugins)
{
	QStringList names;
	for (const FxPlugin &plugin : plugins)
		names.append(fx_plugin_display_name(plugin));

	if (names.isEmpty())
		return QString();

	return "Inserted " + names.join(", ");
}

// A short summary of the effect plugins removed by the reply.
static inline QString fx_summarize_removed_plugins(const QVector<FxPlugin> &plugins)
{
	QStringList names;
	for (const FxPlugin &plugin : plugins)
		names.append(fx_plugin_display_name(plugin));

	if (names.isEmpty())
		return QString();

	return "Removed " + names.join(", ");
}

// The complete list of the deltas a reply applied ("old -> new" per control),
// in the form stored in the history and written to the log. Unlike the status
// summaries it is never truncated, so a correction can revert all of it.
// Modulator actions and plugin insertions/removals are not parameter values
// and are summarized in words.
static inline QString fx_applied_summary(const FxParseResult &parsed, const QVector<FxParamState> &states)
{
	QString ret;

	auto append = [&ret](const QString &line)
	{
		if (line.isEmpty())
			return;
		if (!ret.isEmpty())
			ret += "\n";
		ret += line;
	};

	if (!parsed.changes.isEmpty())
		append("System effects: " + fx_summarize_changes(states, parsed.changes, parsed.changes.size()));

	if (!parsed.instrument_changes.isEmpty())
		append(fx_summarize_instrument_changes(parsed.instrument_changes, parsed.instrument_changes.size()));

	if (!parsed.modulations.isEmpty())
		append(fx_summarize_modulations(parsed.modulations, parsed.modulations.size()));

	if (!parsed.removed_plugins.isEmpty())
		append(fx_summarize_removed_plugins(parsed.removed_plugins));

	if (!parsed.plugins.isEmpty())
		append(fx_summarize_plugins(parsed.plugins));

	return ret;
}

// The effect-tuning conversation state machine: send the system prompt (rules
// and effect reference) plus the request with the current values and the
// matching effect plugins, parse the JSON reply, apply the changes (one undo
// step), and keep the conversation for follow-up requests ("less of that").
// Open-loop: the human ear closes the loop via undo and follow-ups.
class FxTuneSession
{
public:
	explicit FxTuneSession(FxTuneSessionHost host)
		: _host(std::move(host))
		, _alive(std::make_shared<std::atomic_bool>(true))
	{
	}

	// Stops all pending callbacks from touching this session. Must be called
	// before the session is destroyed.
	void shutdown(void)
	{
		*_alive = false;
		if (_cancel)
		  *_cancel = true;
	}

	bool is_busy(void) const
	{
		return _busy;
	}

	void cancel(void)
	{
		if (_cancel)
		  *_cancel = true;
	}

	void clear_history(void)
	{
		_history = QJsonArray();
	}

	void tune(const QString &prompt)
	{
		if (_busy)
		  return;

		const QString trimmed = prompt.trimmed();
		if (trimmed.isEmpty())
		  return;

		_states = _host.read_catalog();
		_instrument = _host.read_instrument_context ? _host.read_instrument_context() : FxInstrumentContext();
		if (_states.isEmpty() && _instrument.params.isEmpty())
		{
			_host.show_error("This instrument has no tuneable effects or plugin controls.");
			return;
		}

		_plugins = _host.read_plugin_catalog ? _host.read_plugin_catalog() : QVector<FxPlugin>();
		_inserted_plugins = _host.read_inserted_plugins ? _host.read_inserted_plugins() : QVector<FxPlugin>();

		_request = trimmed;
		_applied_summary.clear();
		// The system prompt is static (cacheable), so the request-dependent
		// plugin list goes into the user message instead.
		_system_message = build_fx_system_prompt();
		_user_message = build_fx_user_message(_states, _instrument, _plugins, _inserted_plugins, trimmed);
		_repair_attempted = false;
		request(_user_message, false);
	}

private:
	void request(const QString &user_message, bool is_repair)
	{
		// Free mode points at the dedicated effect-tuning relay (see
		// get_fx_config); custom mode uses the user's own provider.
		const LLMConfig config = get_fx_config();

		_busy = true;
		if (_host.set_busy)
		  _host.set_busy(true);
		if (_host.status)
		  _host.status(is_repair ? "Fixing the reply..." : "Tuning effects...");

		const std::shared_ptr<std::atomic_bool> alive = _alive;
		_cancel = std::make_shared<std::atomic_bool>(false);
		const std::shared_ptr<std::atomic_bool> cancel = _cancel;

		send_chat(config, _system_message, user_message,
		          [this, alive, is_repair](bool ok, QString result_or_error)
		          {
			          if (!*alive)
				          return;

			          _busy = false;
			          if (_host.set_busy)
				          _host.set_busy(false);

			          if (!ok)
			          {
				          if (result_or_error == "Request cancelled.")
				          {
					          if (_host.status)
						          _host.status("Tuning cancelled.");
				          }
				          else
				          {
					          // A failed turn (empty stream, server error) must not
					          // vanish from the conversation: the user retries the
					          // request next, and the reply must not appear to
					          // have succeeded.
					          llm_log_note("request failed: " + result_or_error);
					          remember_turn(_request, "(no reply: the request failed)", QString(), QString());
					          if (_host.status)
						          _host.status("No reply - the request failed.");
					          if (_host.show_error)
						          _host.show_error(result_or_error);
				          }
				          return;
			          }

			FxParseResult parsed = parse_fx_response(result_or_error, _states, _plugins, _inserted_plugins, _instrument);

			// Everything the parser had to drop or clamp is invisible
			// otherwise (a silently ignored control name looks exactly
			// like a successful change), so log it.
			if (!parsed.warnings.isEmpty())
				llm_log_note("parse warnings:\n- " + parsed.warnings.join("\n- "));

			          if (!parsed.ok)
			          {
				          if (!is_repair && !_repair_attempted)
				          {
					          // One repair round: ask for a single valid JSON object.
					          _repair_attempted = true;
					          request(QString("Your previous reply could not be parsed: %1\n\n"
					                          "Reply with ONLY a single valid JSON object using the schema "
					                          "{\"changes\":[{\"name\":\"...\",\"value\":0}],\"instrument_changes\":[{\"name\":\"...\",\"value\":0.5}],\"add_plugins\":[{\"name\":\"...\",\"type\":\"...\"}],"
					                          "\"remove_plugins\":[{\"name\":\"...\",\"type\":\"...\"}],\"create_faust\":\"...\",\"explanation\":\"...\"} and nothing else.").arg(parsed.error),
					                  true);
					          return;
				          }

				          if (_host.show_error)
					          _host.show_error("Could not read the LLM reply:\n" + parsed.error);
				          return;
			          }

			// The deltas of this reply (complete, no "... and N more"). They go
			// into the history so a correction can revert them, and into the log
			// so the reply can be audited. They are built after the reply has been
			// applied, so the summary can use the value texts the plugin reports
			// for the new values (the host reads them back in its apply_changes).
			const bool nothing_to_apply = parsed.changes.isEmpty() && parsed.instrument_changes.isEmpty()
			                              && parsed.modulations.isEmpty() && parsed.plugins.isEmpty()
			                              && parsed.removed_plugins.isEmpty() && parsed.create_faust.isEmpty();

			if (nothing_to_apply)
			{
				// Remember the turn (with an empty summary) even though nothing
				// was applied, so a correction never references a turn that is
				// missing from the history.
				_applied_summary = fx_applied_summary(parsed, _states);
				remember_turn(_request, result_or_error, _applied_summary,
				              fx_measurement_summary(_instrument.measurement));

				if (!parsed.answer.isEmpty())
				{
					// An answer to a question is a complete reply: show it
					// (plus the explanation, if the model added one).
					if (_host.status)
						_host.status(parsed.explanation.isEmpty() ? parsed.answer : parsed.answer + " " + parsed.explanation);
					return;
				}

				if (!parsed.ask.isEmpty())
				{
					// A clarifying question is a valid reply: show it
					// instead of pretending something was changed.
					if (_host.status)
						_host.status(parsed.ask);
					return;
				}

				// The reply proposed something, but the parser could not use
				// any of it (wrong control names, out of range, ...). Say
				// so: otherwise the explanation below reads as if the
				// changes had been made.
				if (!parsed.warnings.isEmpty())
				{
					if (_host.show_error)
						_host.show_error("Could not apply the reply:\n- " + parsed.warnings.join("\n- "));
					return;
				}

				if (_host.status)
					_host.status(parsed.explanation.isEmpty() ? QString("No changes made.") : parsed.explanation);
				return;
			}

			          QStringList summary_parts;

			if (_host.apply_reply && !_host.apply_reply(parsed.changes, parsed.instrument_changes, parsed.modulations, parsed.plugins, parsed.removed_plugins))
			          {
				          // The reply was accepted but not applied (the state changed
				          // between reading and applying it). Keep the turn in the
				          // history without claiming changes, so a correction can still
				          // refer to it.
				          remember_turn(_request, result_or_error, QString(), fx_measurement_summary(_instrument.measurement));
				          if (_host.show_error)
					          _host.show_error("Failed to apply the changes.");
				          return;
			          }

			// Now that the reply is applied, the plugin reports the new value
			// texts, so the summary can show them (e.g. "Release 0 ms -> 40 ms")
			// instead of a normalized position.
			_applied_summary = fx_applied_summary(parsed, _states);
			if (!_applied_summary.isEmpty())
				llm_log_note("applied:\n" + _applied_summary);
			remember_turn(_request, result_or_error, _applied_summary,
			              fx_measurement_summary(_instrument.measurement));

			          if (!parsed.create_faust.isEmpty()){
				          if (_host.create_faust_effect && !_host.create_faust_effect(parsed.create_faust, parsed.create_faust_name)){
					          if (_host.show_error)
						          _host.show_error("Failed to create the Faust Dev 2 effect.");
					          return;
				          }
			          }

			if (!parsed.changes.isEmpty())
				summary_parts.append("Applied: " + fx_summarize_changes(_states, parsed.changes));

			if (!parsed.instrument_changes.isEmpty())
				summary_parts.append("Adjusted: " + fx_summarize_instrument_changes(parsed.instrument_changes));

			          if (!parsed.modulations.isEmpty())
				          summary_parts.append(fx_summarize_modulations(parsed.modulations));

			          if (!parsed.removed_plugins.isEmpty())
				          summary_parts.append(fx_summarize_removed_plugins(parsed.removed_plugins));

			          if (!parsed.plugins.isEmpty())
				          summary_parts.append(fx_summarize_plugins(parsed.plugins));

			          QString summary = summary_parts.join(". ");
			          if (!parsed.explanation.isEmpty())
			          {
				          if (!summary.isEmpty())
					          summary += " - ";
				          summary += parsed.explanation;
			          }
			          if (!parsed.answer.isEmpty())
			          {
				          if (!summary.isEmpty())
					          summary += " - ";
				          summary += parsed.answer;
			          }
			          if (!parsed.ask.isEmpty())
			          {
				          if (!summary.isEmpty())
					          summary += " Question: ";
				          summary += parsed.ask;
			          }
			          if (_host.status)
				          _host.status(summary);
		          },
		          _history, cancel, 0.2, std::function<void(int, int)>(), true /* json_mode */);
	}

	void remember_turn(const QString &request, const QString &assistant_message,
	                   const QString &applied_summary, const QString &measurement_summary)
	{
		// Only the request and the deltas go into the history: the full
		// per-turn parameter snapshot is stale the moment the reply is
		// applied, and repeating it costs another full prompt. The current
		// values in the newest user message are the ones that count.
		//
		// The deltas are what makes a correction ("I meant the plugin") able
		// to revert the reply it refers to without sending the old snapshot.
		QString content = "User request: " + fx_quote(request);
		if (!measurement_summary.isEmpty())
		  content += "\nMeasured before that turn (instrument output): " + measurement_summary
		           + ". Use it as the reference for 'before'/'the same volume as before'.";
		content += applied_summary.isEmpty()
		  ? QString("\n(No changes were applied in that turn.)")
		  : QString("\nApplied in that turn (old -> new; the newest user message lists the current values):\n") + applied_summary;
		_history.append(QJsonObject{{QStringLiteral("role"), QStringLiteral("user")}, {QStringLiteral("content"), content}});
		_history.append(QJsonObject{{QStringLiteral("role"), QStringLiteral("assistant")}, {QStringLiteral("content"), assistant_message}});

		// Keep the last three turns, so follow-up requests stay small.
		while (_history.size() > 6)
		  _history.removeFirst();
	}

	FxTuneSessionHost _host;
	std::shared_ptr<std::atomic_bool> _alive;
	std::shared_ptr<std::atomic_bool> _cancel;
	QJsonArray _history;
	QVector<FxParamState> _states;
	FxInstrumentContext _instrument;
	QVector<FxPlugin> _plugins;
	QVector<FxPlugin> _inserted_plugins;
	QString _request;
	// What the current turn's reply changed (complete, "old -> new" per
	// control). Goes into the history so a correction can revert it.
	QString _applied_summary;
	QString _system_message;
	QString _user_message;
	bool _busy = false;
	bool _repair_attempted = false;
};

} // namespace llm
} // namespace radium
