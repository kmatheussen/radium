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

// A proposed change, in native units.
struct FxChange
{
	QString name;
	float value;
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
	add("System Pitch type", "", 0, 4, false, true, "", "Pitch shifter mode: 0 = Disabled (no pitch shifting), 1 = Only left channel, 2 = Only right channel, 3 = All channels, 4 = Invert channels. Pattern/note pitch changes only work when this is not 0; use 3 for a normal pitch shift.");

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
	for (int voicenum = 1; voicenum <= 7; voicenum++)
	{
		const QString v = QString::number(voicenum);
		const QString onoff_name = "System On/Off Voice " + v;
		const QString voice_desc = (voicenum == 1)
		  ? QString("Note duplicator voice 1 is the instrument's ORIGINAL note.")
		  : QString("Note duplicator voice %1 is an extra copy of every note, mixed together with the other voices.").arg(v);
		const QString onoff_desc = (voicenum == 1)
		  ? QString(" All voices share the same system effects and bus sends; turning voice 1 (or all voices) off silences the instrument completely.")
		  : QString(" Set On/Off to 1 to hear this layer. There are no separate effects per voice.");

		add_param(onoff_name, "", 0, 1, true, false, "", voice_desc + onoff_desc);
		add_param("System Transpose Voice " + v, "semitones", -99, 99, false, false, onoff_name, voice_desc + " Transpose in semitones (12 = one octave up).");
		add_param("System Volume Voice " + v, "dB", -35, 35, false, true, onoff_name, voice_desc + " Volume in dB.");
		add_param("System Start Voice " + v, "ms", 0, 1000, false, false, onoff_name, voice_desc + " Delay before this voice starts playing, in milliseconds (0 = immediate).");
		add_param("System Length Voice " + v, "ms", 0, 1000, false, false, onoff_name, voice_desc + " Duration override in milliseconds (0 = use the note's own duration).");
		add_param("System Pan Voice " + v, "degrees", -90, 90, false, true, onoff_name, voice_desc + " Pan in degrees (-90 = left, 0 = center, +90 = right).");
		add_param("System Chance Voice " + v, "chance", 0, 256, false, true, onoff_name, voice_desc + " Chance that this voice plays: 256 = always, 128 = 50%.");
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

	return QString::number(value, 'f', 2);
}

static inline QString fx_format_range(const FxParam &param)
{
	if (param.is_bool)
		return "0 or 1";
	return QString("%1 to %2").arg(param.min_native, 0, 'f', 2).arg(param.max_native, 0, 'f', 2);
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

// The system prompt is stable (no per-request data), so the LLM provider's
// prompt cache can reuse it.
static inline QString build_fx_system_prompt(void)
{
	return QString(
	  "You are an expert mixing engineer assistant inside Radium, a music tracker/DAW. "
	  "The user tunes the built-in system effects of one audio instrument with short subjective "
	  "requests such as \"make it sound harsher\", \"warmer\", \"less muddy\". "
	  "You translate the request into concrete parameter changes for the effects listed in the user message.\n"
	  "\n"
	  "Respond with ONE JSON object and nothing else - no markdown, no code fences, no text around it.\n"
	  "Schema:\n"
	  "{\"changes\":[{\"name\":\"<exact effect name>\",\"value\":<number>}, ...], "
	  "\"modulators\":[{\"action\":\"add|replace|remove\",\"effect\":\"<exact effect name>\",\"type\":\"sine|triangle|square|saw|inverted saw|random\","
	  "\"tempo_numerator\":1,\"tempo_denominator\":2,\"min\":0.0,\"max\":1.0}, ...], "
	  "\"add_plugins\":[{\"name\":\"<exact plugin name>\",\"type\":\"<exact plugin type>\",\"container\":\"<container name, only if needed>\"}, ...], "
	  "\"remove_plugins\":[{\"name\":\"<exact plugin name>\",\"type\":\"<exact plugin type>\",\"container\":\"<container name, only if needed>\"}, ...], "
	  "\"create_faust\":\"<short description of the effect to create, or empty>\", "
	  "\"create_faust_name\":\"<short instrument name, e.g. Autotune, or empty>\", "
	  "\"explanation\":\"<one or two sentences>\"}\n"
	  "\n"
	  "Rules:\n"
	  "1) Use only effect names from the provided list, copied exactly.\n"
	  "2) Values are absolute (the new value), in the native unit shown in the list. Respect the given min..max range.\n"
	  "3) For On/Off effects use 0 or 1. Set the matching On/Off effect to 1 when you change an effect that has one "
	  "(EQ bands, filters, compressor, pan, width, and each note duplicator voice). Pitch changes need "
	  "'System Pitch type' set to 3 (All channels); it is 0 (Disabled) by default.\n"
	  "4) List only the effects you want to change (at most 8). Prefer musical, moderate moves; avoid extreme "
	  "boosts or cuts unless the request explicitly asks for something drastic.\n"
	  "5) The built-in system effects can shape EQ/filtering, compressor dynamics, pitch, panning and Haas width, "
	  "note duplication (the 'voice' effects) and the send levels to the Reverb and Chorus mixer buses. They cannot "
	  "create distortion, saturation, wah, phaser/flanger, delay/echo, amp simulation or other effects that need a "
	  "dedicated plugin. For those requests, insert one of the effect plugins from the 'Available effect plugins' "
	  "list in the user message with the \"add_plugins\" array. If no suitable plugin is listed, do not invent one: "
	  "set \"create_faust\" to a short description of the requested effect (for example \"an autotune effect\") "
	  "and explain briefly that the effect is not currently available. The host will ask the user whether to "
	  "create it with the 'Faust Dev 2' instrument by generating Faust code. Do not claim it has already been "
	  "created. The Reverb/Chorus buses only have an audible effect if those buses are set up in the mixer.\n"
	  "6) The 'explanation' is one or two short sentences, in plain language, no lists.\n"
	  "7) Signal flow: the note duplicator voices (voice 1 = the original note, voices 2-7 = extra copies) are mixed "
	  "into ONE instrument signal. Every effect in the list - including the Reverb/Chorus sends - processes that "
	  "mixed signal; effects are never per voice. Never set 'System On/Off Voice 1' (or all voices) to 0 to achieve "
	  "something: it silences the instrument completely, and its bus sends too.\n"
	  "8) Instrument output and mixer buttons: 'System Out On/Off' turns the instrument's direct (dry) output to the "
	  "main mix on/off - the Reverb/Chorus sends keep working when it is 0; 'System Volume On/Off' is the Mute "
	  "(1 = audible, 0 = muted, silences everything including the sends); 'System Effects On/Off' is the Bypass "
	  "(1 = effects active, 0 = bypassed); 'System Solo On/Off' is Solo (1 = only this instrument is heard, all "
	  "other instruments are muted). Use the Mute/Bypass/Solo buttons only when the user explicitly asks to mute/"
	  "unmute, bypass/un-bypass or solo/unsolo, and never combine a bypass change with effect changes. To silence "
	  "the instrument completely, set 'System Volume On/Off' to 0.\n"
	  "9) 'Only hear reverb/chorus': raise the send level and set 'System Out On/Off' to 0. That removes the "
	  "instrument's direct signal from the main mix while the bus send keeps working, so only the bus effect is "
	  "heard. Set 'System Out On/Off' back to 1 when the instrument itself should be heard again. (Never try to do "
	  "this by turning off note duplicator voices.) The effect's own wet/dry balance is set on the bus in the mixer.\n"
	  "10) Modulation: a modulator is an LFO-like controller that continuously moves one numeric effect parameter. "
	  "Use the separate \"modulators\" array for requests like 'modulate the high pass', 'wobble', 'slow sweep', "
	  "'pulse' or 'stop modulating X' (never put these in \"changes\"). action: \"add\" (default; only when the effect "
	  "has no modulator - effects that already have one are marked [modulated]), \"replace\" (change the settings of "
	  "an existing modulator) or \"remove\". type (default sine): sine, triangle, square, saw, inverted saw or "
	  "random. tempo_numerator/tempo_denominator is the LFO rate in cycles per beat (default 1/2; slow = 1/4 to 1/8, "
	  "fast = 1/1 to 4/1). min and max are the lower/upper scaled bounds 0..1 (default 0..1; narrow them, e.g. "
	  "0.4..0.6, for a subtle effect; min > max inverts the LFO). Only numeric effects can be modulated, not "
	  "On/Off effects. Use at most 2 modulator actions per reply. (A modulator added to 'System Pitch' also "
	  "switches on the pitch shifter automatically.)\n"
	  "11) Effect plugins: \"add_plugins\" inserts an external effect plugin directly after this instrument in the "
	  "signal graph (the instrument, and everything already inserted after it, now feeds the new plugin, which "
	  "feeds the instrument's old destinations). Use it when the user asks for an effect the built-in system "
	  "effects cannot provide, such as 'add a wah wah effect', 'make it distorted', 'add a phaser' or 'add a "
	  "delay'. Use the exact name and type from the 'Available effect plugins' list in the user message; never "
	  "invent a plugin. If the list shows a container for the plugin, include it as \"container\" only when "
	  "needed to distinguish plugins with the same name and type. Do not add a plugin when the request can be "
	  "handled by the listed system effects (for "
	  "example 'more reverb' -> 'System Reverb', 'wider' -> 'System Width'). Add at most one plugin per reply "
	  "unless the user explicitly asks for several. Plugin parameters are not changed by this reply; the user "
	  "can adjust them in the plugin's mixer strip.\n"
	  "12) Removing effect plugins: use \"remove_plugins\" with the exact name and type from the 'Effect plugins "
	  "currently inserted after this instrument' list in the user message; include \"container\" when the list "
	  "shows one and it is needed to distinguish same-named plugins. Use it for requests like 'remove the "
	  "wah wah' or 'take off the distortion'. Only plugins in that list can be removed; never remove instruments, "
	  "system effects or plugins that are not listed. Removal bypasses the plugin (reconnecting its input to its "
	  "output) and deletes it. If the same plugin is inserted several times, all its instances are removed. Do not "
	  "list the same plugin in both \"add_plugins\" and \"remove_plugins\" unless the user asks to replace it; "
	  "removals are applied before additions.\n"
	  "13) \"create_faust\" is only for effects that cannot be provided by the built-in system effects or any "
	  "listed effect plugin. Give a short, concrete description of the effect (including important character, "
	  "e.g. \"a smooth autotune effect with medium retune speed\", not just \"an effect\"). Also set "
	  "\"create_faust_name\" to a very short instrument name (one to three words, e.g. \"Autotune\") that will "
	  "be used as the new instrument's name. Leave both empty in all other replies. The host shows a confirmation "
	  "prompt; you never create the instrument yourself.\n"
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
	  "- 'more reverb'/'more space': raise the 'System Reverb' send level (dB; -40 = none, -20 to -6 is typical). "
	  "'more chorus': raise 'System Chorus'.\n"
	  "- 'only hear reverb'/'only hear chorus': raise the send level and set 'System Out On/Off' to 0 (this removes "
	  "the instrument's direct signal from the main mix but keeps the send working). Set 'System Out On/Off' back "
	  "to 1 when the instrument itself should be heard again.\n"
	  "- 'thicker'/'layered'/'add an octave': use a note duplicator voice (2-7): set its On/Off to 1 and set "
	  "'System Transpose Voice N' (semitones), 'System Volume Voice N' (dB) and optionally 'System Pan Voice N'. "
	  "Never turn voice 1 off - it is the original sound. Do not use more than one or two extra voices "
	  "unless asked.\n"
	  "\n"
	  "The current values in the list are context only; you do not need to repeat effects that should stay unchanged.");
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QVector<FxPlugin> &plugins, const QVector<FxPlugin> &inserted_plugins, const QString &prompt)
{
	QString ret = "Tune the system effects of the current instrument, or add/remove an effect plugin when the request needs it.\n\n";
	ret += "User request: " + fx_quote(prompt) + "\n\n";
	ret += "Effect parameters (exact name; unit; allowed range; current value):\n";

	bool buses_explained = false;
	bool voices_explained = false;
	bool mixer_buttons_explained = false;

	for (const FxParamState &state : states)
	{
		const FxParam &param = state.param;

		if (!buses_explained && param.name == "System Reverb")
		{
			ret += "\nThe 'System Reverb'/'System Chorus' effects are send levels to the mixer buses: they add the "
			       "effect to the instrument's output. To hear only the bus effect, raise the send and switch "
			       "'System Out On/Off' off (see below).\n";
			buses_explained = true;
		}

		if (!voices_explained && param.name == "System On/Off Voice 1")
		{
			ret += "\nThe 'Voice' effects are the note duplicator. Voice 1 is the original note; voices 2-7 play "
			       "extra copies of every note. All voices are mixed into ONE signal, and every effect and bus send "
			       "processes that combined signal - no effect is per voice. Turning voice 1 (or all voices) off "
			       "silences the instrument completely.\n";
			voices_explained = true;
		}

		if (!mixer_buttons_explained && param.name == "System Out On/Off")
		{
			ret += "\nThe last four effects are the instrument's output and mixer buttons: 'System Out On/Off' turns "
			       "the instrument's direct (dry) output to the main mix on/off (the Reverb/Chorus sends keep "
			       "working when it is 0), 'System Volume On/Off' is Mute (1 = audible, 0 = muted, silences "
			       "everything including the sends), 'System Effects On/Off' is Bypass (1 = effects active), "
			       "'System Solo On/Off' is Solo (1 = only this instrument is heard, all others muted). Use the "
			       "buttons only when explicitly asked; to silence the instrument, mute it.\n";
			mixer_buttons_explained = true;
		}

		ret += "- " + param.name
		    + " [" + (param.unit.isEmpty() ? QString("on/off") : param.unit)
		    + "; range " + fx_format_range(param)
		    + "; current " + fx_format_value(param, state.current_native) + "]"
		    + (state.has_modulator ? QString(" [modulated]") : QString()) + " "
		    + param.description + "\n";
	}

	ret += "\nAvailable effect plugins (exact name; type) that can be inserted after this instrument:\n";
	if (plugins.isEmpty())
	{
		ret += "(no effect plugins are currently available)\n";
	}
	else
	{
		for (const FxPlugin &plugin : plugins)
		{
			ret += "- " + plugin.name + " [" + plugin.type_name + "]";
			if (!plugin.container_name.isEmpty())
				ret += " (container: " + plugin.container_name + ")";
			ret += "\n";
		}
	}

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

	ret += "\nThe container is only for disambiguation; include it in your reply only when needed for same-named plugins.";
	ret += "\nIf no listed plugin can provide the requested effect, do not invent one. Leave \"add_plugins\" empty, set "
	       "\"create_faust\" to a short description of the effect and \"create_faust_name\" to a short instrument name "
	       "(e.g. \"Autotune\"), and explain that it is not currently available; the host will ask whether to create it "
	       "with the 'Faust Dev 2' instrument.";
	ret += "\nRemember: reply with only the JSON object. Values are absolute.";
	return ret;
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QVector<FxPlugin> &plugins, const QString &prompt)
{
	return build_fx_user_message(states, plugins, QVector<FxPlugin>(), prompt);
}

static inline QString build_fx_user_message(const QVector<FxParamState> &states, const QString &prompt)
{
	return build_fx_user_message(states, QVector<FxPlugin>(), QVector<FxPlugin>(), prompt);
}

struct FxParseResult
{
	bool ok = false;
	QString error;
	QString explanation;
	QVector<FxChange> changes;
	QVector<FxModulation> modulations;
	QVector<FxPlugin> plugins;
	QVector<FxPlugin> removed_plugins;
	QString create_faust;
	QString create_faust_name;
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

// Parses and validates an LLM reply. Unknown effect names, non-numeric
// values and out-of-range values are dropped or clamped (reported in
// 'warnings'); all changes are converted to native units. Plugins must be
// in the supplied catalogs; unknown plugins are dropped with a warning.
static inline FxParseResult parse_fx_response(const QString &raw,
                                              const QVector<FxParamState> &states,
                                              const QVector<FxPlugin> &plugins,
                                              const QVector<FxPlugin> &inserted_plugins)
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
	const QJsonValue modulators_value = root.value("modulators");
	const QJsonValue plugins_value = root.value("add_plugins");
	const QJsonValue remove_plugins_value = root.value("remove_plugins");
	const QJsonValue create_faust_value = root.value("create_faust");
	const QJsonValue create_faust_name_value = root.value("create_faust_name");
	if (changes_value.isUndefined() && modulators_value.isUndefined()
	    && plugins_value.isUndefined() && remove_plugins_value.isUndefined()
	    && create_faust_value.isUndefined())
	{
		result.error = "The JSON object has no action ('changes', 'modulators', 'add_plugins', 'remove_plugins' or 'create_faust').";
		return result;
	}
	if (!changes_value.isUndefined() && !changes_value.isArray())
	{
		result.error = "'changes' is not an array.";
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
	for (const FxParamState &state : states)
		by_name.insert(state.param.name.toLower(), &state);

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

		const FxParamState *state = by_name.value(name.toLower(), NULL);
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

		const FxParamState *state = by_name.value(effect_name.toLower(), NULL);
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
	result.create_faust = create_faust_value.toString().trimmed();
	result.create_faust_name = create_faust_name_value.toString().trimmed();

	for (const QString &key : order)
	  result.changes.append(resolved.value(key));

	result.ok = true;
	return result;
}

static inline FxParseResult parse_fx_response(const QString &raw,
                                              const QVector<FxParamState> &states,
                                              const QVector<FxPlugin> &plugins)
{
	return parse_fx_response(raw, states, plugins, QVector<FxPlugin>());
}

static inline FxParseResult parse_fx_response(const QString &raw, const QVector<FxParamState> &states)
{
	return parse_fx_response(raw, states, QVector<FxPlugin>(), QVector<FxPlugin>());
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

		parts.append(QString("%1 %2 -> %3").arg(change.name, old_value, new_value));
		count++;
	}

	return parts.join(", ");
}

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

// The GUI/instrument operations the session needs. Implemented by the
// instrument widget.
struct FxTuneSessionHost
{
	std::function<QVector<FxParamState>()> read_catalog;
	// Effect plugins that can be inserted into the signal path. The host
	// reads this from the plugin registry.
	std::function<QVector<FxPlugin>()> read_plugin_catalog;
	// Effect plugins currently inserted after this instrument, in chain
	// order. The host reads this from the mixer graph.
	std::function<QVector<FxPlugin>()> read_inserted_plugins;
	// Applies the whole reply (effect value changes, modulation actions and
	// plugin insertions/removals) as ONE undo step. May extend 'changes' with
	// the extra changes it makes itself (e.g. enabling a band that was off),
	// so the final status summary includes them.
	std::function<bool(QVector<FxChange> &changes,
	                   const QVector<FxModulation> &modulations,
	                   const QVector<FxPlugin> &add_plugins,
	                   const QVector<FxPlugin> &remove_plugins)> apply_reply;
	// Asks the user whether to create a Faust Dev 2 instrument for an effect
	// that no plugin can provide. Returns true if the user was asked (and the
	// creation was started or declined), false on failure.
	std::function<bool(const QString &description, const QString &name)> create_faust_effect;
	std::function<void(const QString &text)> status;
	std::function<void(const QString &message)> show_error;
	std::function<void(bool busy)> set_busy;
};

// The effect-tuning conversation state machine: send the catalog and the
// user's request, parse the JSON reply, apply the changes (one undo step),
// and keep the conversation for follow-up requests ("less of that").
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
		if (_states.isEmpty())
		{
			_host.show_error("This instrument has no tuneable system effects.");
			return;
		}

		_plugins = _host.read_plugin_catalog ? _host.read_plugin_catalog() : QVector<FxPlugin>();
		_inserted_plugins = _host.read_inserted_plugins ? _host.read_inserted_plugins() : QVector<FxPlugin>();

		_user_message = build_fx_user_message(_states, _plugins, _inserted_plugins, trimmed);
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

		send_chat(config, build_fx_system_prompt(), user_message,
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
					          if (_host.show_error)
						          _host.show_error(result_or_error);
				          }
				          return;
			          }

			          FxParseResult parsed = parse_fx_response(result_or_error, _states, _plugins, _inserted_plugins);

			          if (!parsed.ok)
			          {
				          if (!is_repair && !_repair_attempted)
				          {
					          // One repair round: ask for a single valid JSON object.
					          _repair_attempted = true;
					          request(QString("Your previous reply could not be parsed: %1\n\n"
					                          "Reply with ONLY a single valid JSON object using the schema "
					                          "{\"changes\":[{\"name\":\"...\",\"value\":0}],\"add_plugins\":[{\"name\":\"...\",\"type\":\"...\"}],"
					                          "\"remove_plugins\":[{\"name\":\"...\",\"type\":\"...\"}],\"create_faust\":\"...\",\"explanation\":\"...\"} and nothing else.").arg(parsed.error),
					                  true);
					          return;
				          }

				          if (_host.show_error)
					          _host.show_error("Could not read the LLM reply:\n" + parsed.error);
				          return;
			          }

			          // Remember the original tuning request (not a possible
			          // JSON-repair message) and the final reply, so follow-up
			          // requests have the full context.
			          remember_turn(_user_message, result_or_error);

			          if (parsed.changes.isEmpty() && parsed.modulations.isEmpty()
			              && parsed.plugins.isEmpty() && parsed.removed_plugins.isEmpty()
			              && parsed.create_faust.isEmpty())
			          {
				          if (_host.status)
					          _host.status(parsed.explanation.isEmpty() ? QString("No changes made.") : parsed.explanation);
				          return;
			          }

			          QStringList summary_parts;

			          if (_host.apply_reply && !_host.apply_reply(parsed.changes, parsed.modulations, parsed.plugins, parsed.removed_plugins))
			          {
				          if (_host.show_error)
					          _host.show_error("Failed to apply the changes.");
				          return;
			          }

			          if (!parsed.create_faust.isEmpty()){
				          if (_host.create_faust_effect && !_host.create_faust_effect(parsed.create_faust, parsed.create_faust_name)){
					          if (_host.show_error)
						          _host.show_error("Failed to create the Faust Dev 2 effect.");
					          return;
				          }
			          }

			          if (!parsed.changes.isEmpty())
				          summary_parts.append("Applied: " + fx_summarize_changes(_states, parsed.changes));

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
			          if (_host.status)
				          _host.status(summary);
		          },
		          _history, cancel, 0.2);
	}

	void remember_turn(const QString &user_message, const QString &assistant_message)
	{
		_history.append(QJsonObject{{QStringLiteral("role"), QStringLiteral("user")}, {QStringLiteral("content"), user_message}});
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
	QVector<FxPlugin> _plugins;
	QVector<FxPlugin> _inserted_plugins;
	QString _user_message;
	bool _busy = false;
	bool _repair_attempted = false;
};

} // namespace llm
} // namespace radium
