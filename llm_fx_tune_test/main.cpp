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

// Unit tests for the pure parts of the LLM effect tuning code: the catalog,
// the JSON response parser and the band auto-enable logic. No Radium
// application, no network.

#include <QString>
#include <QVector>

#include <cmath>
#include <cstdio>

// Environment stubs for the Radium headers (shared with faust_llm_test).
#include "../faust_llm_test/radium_env.hpp"

#include "../Qt/LLM_fx_tune.hpp"
#include "../audio/AudioAnalyzer.h"

using namespace radium::llm;

static int g_failures = 0;

static void check(bool ok, const char *what)
{
	if (ok)
	  printf("  ok: %s\n", what);
	else
	{
		printf("FAIL: %s\n", what);
		g_failures++;
	}
}

static const FxParamState *find_state(const QVector<FxParamState> &states, const char *name)
{
	for (const FxParamState &state : states)
		if (state.param.name == QString::fromUtf8(name))
		  return &state;
	return NULL;
}

static const FxChange *find_change(const QVector<FxChange> &changes, const char *name)
{
	for (const FxChange &change : changes)
		if (change.name == QString::fromUtf8(name))
		  return &change;
	return NULL;
}

static const FxParam *find_param(const QVector<FxParamState> &states, const char *name)
{
	const FxParamState *state = find_state(states, name);
	return state != NULL ? &state->param : NULL;
}

static QVector<FxParamState> make_states(void)
{
	QVector<FxParamState> states;

	for (const FxParam &param : fx_catalog())
	{
		FxParamState state;
		state.param = param;
		// Something in the middle (and 'off' for on/off effects), so both
		// increasing and decreasing values count as changes.
		state.current_native = param.is_bool
		  ? 0.0f
		  : (param.min_native + param.max_native) / 2.0f;
		states.append(state);
	}

	return states;
}

static QVector<FxParamState> make_states_with_modulator(const char *name)
{
	QVector<FxParamState> states = make_states();

	for (FxParamState &state : states)
		if (state.param.name == QString::fromUtf8(name))
			state.has_modulator = true;

	return states;
}

static FxInstrumentContext make_instrument_context(void)
{
	FxInstrumentContext instrument;
	instrument.type_name = "Ladspa";
	instrument.plugin_name = "Organ";
	instrument.plugin_version = 1;
	instrument.description = "A drawbar organ instrument.";

	FxInstrumentParamState second_harmonic;
	second_harmonic.effect_num = 7;
	second_harmonic.name = "Harmonic 2";
	second_harmonic.display_name = "2nd harmonic";
	second_harmonic.description = "Level of the second harmonic.";
	second_harmonic.current_display = "0.4";
	second_harmonic.current_scaled = 0.4f;
	second_harmonic.format = FX_INSTRUMENT_PARAM_FLOAT;
	instrument.params.append(second_harmonic);

	FxInstrumentParamState percussion;
	percussion.effect_num = 8;
	percussion.name = "Percussion";
	percussion.current_display = "Off";
	percussion.current_scaled = 0.0f;
	percussion.format = FX_INSTRUMENT_PARAM_BOOL;
	instrument.params.append(percussion);

	FxInstrumentParamState pitch_glide;
	pitch_glide.effect_num = 9;
	pitch_glide.name = "pitch_glide_length";
	pitch_glide.display_name = "pitch_glide_length";
	pitch_glide.description = "Pitch glide time.";
	pitch_glide.current_display = "pitch_glide_length: 420";
	pitch_glide.current_scaled = 0.35f;
	pitch_glide.has_native_range = true;
	pitch_glide.min_native = 0.0f;
	pitch_glide.max_native = 1200.0f;
	pitch_glide.current_native = 420.0f;
	pitch_glide.unit = "ms";
	pitch_glide.format = FX_INSTRUMENT_PARAM_INT;
	instrument.params.append(pitch_glide);

	return instrument;
}

static void test_catalog(void)
{
	printf("catalog:\n");

	const QVector<FxParam> catalog = fx_catalog();
	check(catalog.size() >= 75, "catalog has all effect entries");

	bool all_have_descriptions = true;
	bool all_ranges_valid = true;
	for (const FxParam &param : catalog)
	{
		if (param.description.isEmpty())
		  all_have_descriptions = false;
		if (!(param.min_native < param.max_native))
		  all_ranges_valid = false;
	}
	check(all_have_descriptions, "every entry has a description");
	check(all_ranges_valid, "every entry has a valid range");

	const QVector<FxParamState> states = make_states();
	check(find_state(states, "System Eq1 F.") != NULL, "has System Eq1 F.");
	check(find_state(states, "System Compression Makeup Gain") != NULL, "has compressor makeup gain");
	check(find_state(states, "System Pitch") != NULL, "has pitch");
	check(find_state(states, "System Pan") != NULL, "has pan");
	check(find_state(states, "System Width") != NULL, "has width");
	check(find_state(states, "System Reverb") != NULL, "has reverb bus");
	check(find_state(states, "System Chorus") != NULL, "has chorus bus");
	check(find_state(states, "System On/Off Voice 2") != NULL, "has voice 2 on/off");
	check(find_state(states, "System Transpose Voice 2") != NULL, "has voice 2 transpose");
	check(find_state(states, "System Pan Voice 7") != NULL, "has voice 7 pan");
	check(find_state(states, "System Chance Voice 1") != NULL, "has voice 1 chance");
	check(find_state(states, "System Volume On/Off") != NULL, "has mute");
	check(find_state(states, "System Effects On/Off") != NULL, "has bypass");
	check(find_state(states, "System Solo On/Off") != NULL, "has solo");
	check(find_state(states, "System Out On/Off") != NULL, "has direct output on/off");
	check(find_state(states, "System Pitch type") != NULL, "has pitch type");

	// The bus sends are stored as linear gains natively, so they must be
	// flagged for gain<->dB conversion in the instrument widget.
	const FxParam *reverb = find_param(states, "System Reverb");
	const FxParam *chorus = find_param(states, "System Chorus");
	const FxParam *eq_gain = find_param(states, "System Eq1 L.");
	check(reverb != NULL && reverb->is_gain_db, "reverb bus is marked as gain-in-dB");
	check(chorus != NULL && chorus->is_gain_db, "chorus bus is marked as gain-in-dB");
	check(eq_gain != NULL && !eq_gain->is_gain_db, "EQ gain is not marked as gain-in-dB");

	// "System Pitch" is gated by "System Pitch type"; enabling it means
	// setting the type to 3 (All channels).
	const FxParam *pitch = find_param(states, "System Pitch");
	check(pitch != NULL && pitch->band_onoff_name == "System Pitch type", "pitch is gated by the pitch type");
	check(pitch != NULL && std::fabs(pitch->band_onoff_value - 3.0f) < 0.001, "enabling pitch uses type 3");
}

static void test_parse_valid(void)
{
	printf("parse valid:\n");

	const QVector<FxParamState> states = make_states();

	const FxParseResult result = parse_fx_response(
	  R"({"changes":[{"name":"System Eq1 F.","value":2500},{"name":"System Eq1 L.","value":4.5},{"name":"System Eq1 On/Off","value":1}],"explanation":"Added upper-mid bite."})",
	  states);

	check(result.ok, "ok");
	check(result.error.isEmpty(), "no error");
	check(result.changes.size() == 3, "three changes");
	check(result.explanation == "Added upper-mid bite.", "explanation kept");

	const FxChange *freq = find_change(result.changes, "System Eq1 F.");
	const FxChange *gain = find_change(result.changes, "System Eq1 L.");
	check(freq != NULL && std::fabs(freq->value - 2500) < 0.01, "Eq1 frequency value");
	check(gain != NULL && std::fabs(gain->value - 4.5) < 0.01, "Eq1 gain value");
	check(find_change(result.changes, "System Eq1 On/Off") != NULL, "Eq1 on/off value");
}

static void test_parse_instrument_changes(void)
{
	printf("parse current-instrument changes:\n");

	const QVector<FxParamState> states = make_states();
	const FxInstrumentContext instrument = make_instrument_context();
	const FxParseResult result = parse_fx_response(
	  R"({"instrument_changes":[{"name":"2nd harmonic","value":0.75},{"name":"Percussion","value":true},{"name":"pitch_glide_length","value":922}],"explanation":"Raised the second harmonic."})",
	  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);

	check(result.ok, "instrument-only reply is valid");
	check(result.changes.isEmpty(), "instrument controls are not parsed as system-effect changes");
	check(result.plugins.isEmpty(), "matching instrument control does not require plugin insertion");
	check(result.instrument_changes.size() == 3, "all instrument controls parsed");
	if (result.instrument_changes.size() == 3){
		const FxInstrumentChange &harmonic = result.instrument_changes[0];
		check(harmonic.name == "Harmonic 2" && harmonic.display_name == "2nd harmonic" && harmonic.effect_num == 7, "display label resolves to exact control and index");
		check(harmonic.type_name == "Ladspa" && harmonic.plugin_name == "Organ", "change is bound to the instrument identity");
		check(harmonic.plugin_version == 1, "change is bound to the instrument parameter version");
		check(std::fabs(harmonic.value_scaled - 0.75f) < 0.001f, "normalized instrument value parsed");
		check(std::fabs(harmonic.old_value_scaled - 0.4f) < 0.001f, "old normalized value retained for summary");
		check(std::fabs(result.instrument_changes[1].value_scaled - 1.0f) < 0.001f, "boolean instrument control parsed");
		const FxInstrumentChange &pitch_glide = result.instrument_changes[2];
		check(pitch_glide.value_is_native && pitch_glide.value_native == 922.0f && pitch_glide.unit == "ms", "native range control accepts exact value and unit");
	}

	const FxParseResult clamped = parse_fx_response(
	  R"({"instrument_changes":[{"name":"2nd harmonic","value":2},{"name":"missing control","value":0.5}]})",
	  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);
	check(clamped.ok && clamped.instrument_changes.size() == 1, "unknown instrument control is rejected");
	check(std::fabs(clamped.instrument_changes[0].value_scaled - 1.0f) < 0.001f, "instrument value is clamped to normalized range");
	check(clamped.warnings.size() == 2, "clamp and unknown-control warnings reported");

	const FxParseResult wrong_type = parse_fx_response(
	  R"({"instrument_changes":"not an array"})", states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);
	check(!wrong_type.ok, "non-array instrument_changes rejected");

	const FxParseResult native_clamped = parse_fx_response(
	  R"({"instrument_changes":[{"name":"pitch_glide_length","value":5000}]})",
	  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);
	check(native_clamped.ok && native_clamped.instrument_changes.size() == 1, "native instrument value parsed");
	check(native_clamped.instrument_changes.size() == 1 && native_clamped.instrument_changes[0].value_native == 1200.0f, "native instrument value clamped to its native maximum");
	check(native_clamped.instrument_changes.size() == 1 && native_clamped.instrument_changes[0].value_is_native, "native range keeps native units");
}

static void test_parse_fenced_and_prose(void)
{
	printf("parse fenced/prose:\n");

	const QVector<FxParamState> states = make_states();

	const FxParseResult result = parse_fx_response(
	  "Sure, here you go:\n```json\n{\"changes\":[{\"name\":\"System Hs L.\",\"value\":6}],\"explanation\":\"More air.\"}\n```\n",
	  states);

	check(result.ok, "ok");
	check(result.changes.size() == 1, "one change");
	const FxChange *change = find_change(result.changes, "System Hs L.");
	check(change != NULL && std::fabs(change->value - 6) < 0.01, "high shelf gain value");
}

static void test_parse_case_insensitive(void)
{
	printf("parse case-insensitive names:\n");

	const QVector<FxParamState> states = make_states();

	const FxParseResult result = parse_fx_response(
	  R"({"changes":[{"name":"system eq2 l.","value":3}],"explanation":"x"})",
	  states);

	check(result.ok, "ok");
	check(result.changes.size() == 1, "one change");
	check(find_change(result.changes, "System Eq2 L.") != NULL, "name normalized to catalog spelling");
}

static void test_parse_warnings_and_clamping(void)
{
	printf("parse warnings/clamping:\n");

	const QVector<FxParamState> states = make_states();

	const FxParseResult result = parse_fx_response(
	  R"({"changes":[{"name":"No Such Effect","value":1},{"name":"System Eq1 L.","value":999},{"name":"System Eq1 F.","value":"loud"}],"explanation":"x"})",
	  states);

	check(result.ok, "ok");
	check(result.changes.size() == 1, "only the valid change survives");
	const FxChange *gain = find_change(result.changes, "System Eq1 L.");
	check(gain != NULL && std::fabs(gain->value - 35) < 0.01, "value clamped to 35 dB");
	check(result.warnings.size() == 3, "three warnings (unknown, clamped, non-numeric)");
}

static void test_parse_bools_and_duplicates(void)
{
	printf("parse bools/duplicates:\n");

	const QVector<FxParamState> states = make_states();

	{
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[{"name":"System Eq1 On/Off","value":true},{"name":"System Eq2 On/Off","value":false}],"explanation":"x"})",
		  states);
		check(result.ok, "ok");
		check(result.changes.size() == 1, "true is a change, false is a no-op");
		const FxChange *onoff = find_change(result.changes, "System Eq1 On/Off");
		check(onoff != NULL && std::fabs(onoff->value - 1) < 0.01, "true becomes 1");
	}

	{
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[{"name":"System Pitch","value":3},{"name":"System Pitch","value":-5}],"explanation":"x"})",
		  states);
		check(result.ok, "ok");
		check(result.changes.size() == 1, "duplicates collapsed");
		const FxChange *pitch = find_change(result.changes, "System Pitch");
		check(pitch != NULL && std::fabs(pitch->value + 5) < 0.01, "last duplicate wins");
	}
}

static void test_parse_voice(void)
{
	printf("parse voice:\n");

	const QVector<FxParamState> states = make_states();

	const FxParseResult result = parse_fx_response(
	  R"({"changes":[{"name":"System Transpose Voice 2","value":12},{"name":"System Volume Voice 2","value":-6},{"name":"System On/Off Voice 2","value":1}],"explanation":"Added an octave layer."})",
	  states);

	check(result.ok, "ok");
	check(result.changes.size() == 3, "three voice changes");
	const FxChange *transpose = find_change(result.changes, "System Transpose Voice 2");
	const FxChange *volume = find_change(result.changes, "System Volume Voice 2");
	check(transpose != NULL && std::fabs(transpose->value - 12) < 0.01, "voice transpose value");
	check(volume != NULL && std::fabs(volume->value + 6) < 0.01, "voice volume value");
}

static void test_format_values(void)
{
	printf("format values:\n");

	const QVector<FxParamState> states = make_states();

	{
		const FxParam *pan = find_param(states, "System Pan");
		check(pan != NULL && fx_format_value(*pan, 0.75f).contains("+45"), "pan 0.75 -> +45 degrees");
		check(pan != NULL && fx_format_value(*pan, 0.0f).contains("-90"), "pan 0 -> -90 degrees");
	}

	{
		const FxParam *voice_pan = find_param(states, "System Pan Voice 2");
		check(voice_pan != NULL && fx_format_value(*voice_pan, -45.0f).contains("-45"), "voice pan -45 -> -45 degrees");
	}

	{
		const FxParam *chance = find_param(states, "System Chance Voice 2");
		check(chance != NULL && fx_format_value(*chance, 256.0f) == QStringLiteral("always"), "chance 256 -> always");
		check(chance != NULL && fx_format_value(*chance, 128.0f) == QStringLiteral("128/256"), "chance 128 -> 128/256");
	}

	{
		const FxParam *width = find_param(states, "System Width");
		check(width != NULL && fx_format_value(*width, 5.0f).contains("5.0 ms"), "width formats as ms");
	}

	{
		const FxParam *chorus = find_param(states, "System Chorus");
		check(chorus != NULL && fx_format_value(*chorus, -40.0f) == QStringLiteral("-inf dB"), "silent bus send shows -inf dB");
		check(chorus != NULL && fx_format_value(*chorus, -6.0f) == QStringLiteral("-6.00 dB"), "bus send shows dB");
		check(chorus != NULL && fx_format_value(*chorus, 0.0f) == QStringLiteral("+0.00 dB"), "0-dB bus send shows +0.00 dB");
	}
}

static void test_prompts(void)
{
	printf("prompts:\n");

	const QVector<FxParamState> states = make_states();

	const QString system_prompt = build_fx_system_prompt();
	check(system_prompt.contains("note duplicator"), "system prompt explains the note duplicator");
	check(system_prompt.contains("'System Out On/Off' to 0"), "system prompt explains how to hear only the bus effect");
	check(system_prompt.contains("Muting silences everything, including the Reverb/Chorus sends"), "effect reference distinguishes mute from output off");
	check(system_prompt.contains("Use the Mute/Bypass/Solo buttons only when the user explicitly asks"), "system prompt keeps the mixer-button policy");
	check(system_prompt.contains("Never set 'System On/Off Voice 1'"), "system prompt forbids muting voice 1");
	check(system_prompt.contains("Mute"), "system prompt mentions mute");
	check(system_prompt.contains("Bypass"), "system prompt mentions bypass");
	check(system_prompt.contains("Solo"), "system prompt mentions solo");
	check(system_prompt.contains("modulators"), "system prompt explains modulation");
	check(system_prompt.contains("cycles per beat"), "system prompt explains the LFO rate");
	check(system_prompt.contains("add_plugins"), "system prompt explains adding effect plugins");
	check(system_prompt.contains("remove_plugins"), "system prompt explains removing effect plugins");
	check(system_prompt.contains("instrument_changes"), "system prompt explains current-instrument changes");
	check(system_prompt.contains("change that control instead of inserting a different plugin"), "system prompt prefers a matching instrument control");
	check(system_prompt.contains("the current instrument's own controls come first"), "system prompt prefers the instrument's own controls");
	check(system_prompt.contains("never round normalized values to three decimals"), "system prompt preserves precision for integer controls");
	check(system_prompt.contains("native values and the unit shown"), "system prompt prefers native values when available");
	check(system_prompt.contains("0.768333, not 0.768"), "system prompt demonstrates precise scaled fallback values");
	check(system_prompt.contains("Available effect plugins"), "system prompt points at the plugin list");
	check(system_prompt.contains("Faust Dev 2"), "system prompt suggests Faust Dev 2 when no effect is available");
	check(system_prompt.contains("create_faust"), "system prompt explains Faust Dev 2 creation requests");
	check(system_prompt.contains("create_faust_name"), "system prompt asks for a Faust instrument name");
	check(system_prompt.contains("\"ask\""), "system prompt offers a clarifying question");
	check(system_prompt.contains("does not count toward that limit"), "the companion On/Off switch does not count toward the change limit");
	check(system_prompt.contains("undid the change"), "system prompt says the current values are the truth after an undo");
	check(system_prompt.contains("'System Pitch type' set to 3"), "system prompt explains the pitch mode");

	// The per-request prose moved into the (cached) system prompt, so the
	// effect reference must carry the descriptions.
	check(system_prompt.contains("Effect reference"), "system prompt has the effect reference");
	check(system_prompt.contains("- System Eq1 F. [Hz; range 50.00 to 20000.00] Peaking EQ band 1 center frequency"), "effect reference keeps name, unit, range and description");
	check(system_prompt.contains("System Pitch type [mode; range 0 to 4]"), "a mode effect is not labelled as an on/off switch");
	check(system_prompt.contains("- System Eq1 On/Off [on/off; range 0 or 1]"), "a real on/off effect is labelled as one");

	const QString user_message = build_fx_user_message(states, "test");
	check(user_message.contains("User request: \"test\""), "user message starts with the request");
	check(user_message.contains("- System Eq1 F. = "), "user message lists the current system effect values");
	check(user_message.contains("(exact name = current value"), "user message says the values are current");
	check(!user_message.contains("Peaking EQ band 1"), "user message does not repeat the effect descriptions");
	check(!user_message.contains("note duplicator"), "user message does not repeat the signal-flow prose");
	check(user_message.contains("(no effect plugins are currently available)"), "the (empty) plugin catalog is in the user message");
	check(!user_message.contains("[modulated]"), "no [modulated] marker for unmodulated effects");

	const FxInstrumentContext instrument = make_instrument_context();
	const QString instrument_message = build_fx_user_message(states, instrument, "boost the second harmonic");
	check(instrument_message.contains("Organ [Ladspa]"), "user message identifies the current plugin instrument");
	check(instrument_message.contains("2nd harmonic [control name: Harmonic 2] [continuous; normalized range 0 to 1; current 0.400000; displayed 0.4]"), "user message includes visible and exact instrument control names, format and value");
	check(instrument_message.contains("Level of the second harmonic."), "user message includes instrument control description");
	check(instrument_message.contains("pitch_glide_length [integer; native range 0 to 1200 ms; current native 420 ms; normalized 0.350000; displayed pitch_glide_length: 420]"), "user message includes native range, unit and value when available");

	// The generic MIDI controller controls (Breath/PitchBend/... on channels
	// 1-16) cannot shape a mixing change and used to be most of the list.
	{
		FxInstrumentContext with_midi = make_instrument_context();

		FxInstrumentParamState breath;
		breath.effect_num = 20;
		breath.name = "Breath (CH 3)";
		breath.display_name = breath.name;
		breath.format = FX_INSTRUMENT_PARAM_FLOAT;
		with_midi.params.append(breath);

		FxInstrumentParamState program;
		program.effect_num = 126;
		program.name = "126: Program";
		program.display_name = program.name;
		program.format = FX_INSTRUMENT_PARAM_FLOAT;
		with_midi.params.append(program);

		// The host already drops the MIDI controls and counts them there; the
		// prompt builder counts the ones it still receives itself.
		with_midi.hidden_midi_controls = 1;

		const QString midi_message = build_fx_user_message(states, with_midi, "warmer");
		check(!midi_message.contains("Breath (CH 3)"), "per-channel MIDI controls are not listed");
		check(!midi_message.contains("126: Program"), "the program-change control is not listed");
		check(midi_message.contains("3 MIDI controller controls"), "the omitted MIDI controls are counted");
		check(midi_message.contains("2nd harmonic"), "the real instrument controls are still listed");

		check(fx_is_midi_controller_param(breath), "per-channel MIDI control is detected");

		// The u-he VST3 naming, one control per MIDI CC per channel (Presswerk
		// exposes ~1900 of these next to its ~64 real controls).
		{
			const char *cc_names[] = {"5: CC 5, CH 8", "CC 1, CH 1", "CC5,CH16", "CC 64, CH 2", "CC 5 ; CH 3"};
			for (const char *name : cc_names) {
				FxInstrumentParamState param;
				param.name = name;
				check(fx_is_midi_controller_param(param), "a 'CC n, CH n' control is detected");
			}

			FxInstrumentParamState real;
			real.name = "2: Ratio 1";
			check(!fx_is_midi_controller_param(real), "a real compressor control is not mistaken for a CC");
			real.name = "CC Mode";
			check(!fx_is_midi_controller_param(real), "a control that merely mentions CC is not dropped");
			real.name = "Continuous Control";
			check(!fx_is_midi_controller_param(real), "a word starting with 'cc' is not dropped");
		}
		check(!fx_is_midi_controller_param(make_instrument_context().params[0]), "a normal instrument control is not detected as MIDI");
	}

	// The plugin catalog is per-request data, so it lives at the *end* of the
	// user message: the system prompt must stay byte-identical so a prefix
	// cache can reuse it (and the history after it).
	const QVector<FxPlugin> plugins{
		FxPlugin{QString("Ladspa"), QString("Auto Wah")},
		FxPlugin{QString("VST"), QString("Auto-Tune"), QString("Antares")},
		FxPlugin{QString("Ladspa"), QString("TAP Tremolo")},
	};

	check(build_fx_system_prompt() == build_fx_system_prompt(), "the system prompt is static");
	check(!build_fx_system_prompt().contains("Available effect plugins (exact name; type) that can be inserted"),
	      "the system prompt does not contain the per-request plugin list");

	{
		const QString wah_message = build_fx_user_message(states, plugins, QVector<FxPlugin>(), "add a wah effect");
		check(wah_message.contains("Auto Wah [Ladspa]"), "plugin matching the request is listed");
		check(!wah_message.contains("TAP Tremolo"), "unrelated plugin is not listed");
		check(wah_message.contains("2 of the 3 installed effect plugins are hidden"), "hidden plugin count is reported");
		check(wah_message.contains("create_faust"), "the user message says what to do when a plugin is hidden");
		check(wah_message.lastIndexOf("Available effect plugins") > wah_message.indexOf("Current instrument controls"),
		      "the plugin list comes after the instrument controls");
		check(wah_message.lastIndexOf("Available effect plugins") > wah_message.indexOf("Current system effect values"),
		      "the plugin list comes after the system effect values");
	}

	{
		const QString tune_message = build_fx_user_message(states, plugins, QVector<FxPlugin>(), "autotune it please");
		check(tune_message.contains("Auto-Tune [VST] (container: Antares)"), "plugin entry keeps name, type and container");
	}

	{
		const QString none_message = build_fx_user_message(states, plugins, QVector<FxPlugin>(), "make it sound like a boom box");
		check(none_message.contains("none whose name matches this request"), "no match is stated explicitly");
		check(!none_message.contains("installed effect plugins are hidden"), "no hidden-plugin noise when the request names no plugin");
	}

	{
		const QString empty_message = build_fx_user_message(states, QVector<FxPlugin>(), QVector<FxPlugin>(), "add a wah");
		check(empty_message.contains("(no effect plugins are currently available)"), "no plugins is stated explicitly");
	}

	// The instrument's own controls come first: they take precedence.
	{
		const FxInstrumentContext instrument = make_instrument_context();
		const QString message = build_fx_user_message(states, instrument, plugins, QVector<FxPlugin>(), "make it warmer");
		check(message.indexOf("Current instrument controls") < message.indexOf("Current system effect values"),
		      "the instrument controls are listed before the system effect values");
		check(message.contains("Change these first when they fit the request"), "the instrument controls are announced as the first choice");
		check(message.contains("system effects are the exception, not the default"), "the final reminder repeats the precedence");
	}

	// Plugins already inserted after the instrument are always listed in
	// full, since they can be removed by name.
	{
		const QVector<FxPlugin> inserted{FxPlugin{QString("Ladspa"), QString("Auto Wah")}};
		const QString inserted_message = build_fx_user_message(states, inserted, "make it sound like a boom box");
		check(inserted_message.contains("Effect plugins currently inserted after this instrument"), "user message lists inserted plugins");
		check(inserted_message.contains("Auto Wah [Ladspa]"), "inserted plugin is listed even when the request does not match it");
	}

	{
		const QVector<FxParamState> no_inserted = make_states();
		check(build_fx_user_message(no_inserted, "x").contains("(none)"), "an empty inserted-plugin list says (none)");
	}

	check(build_fx_user_message(states, plugins, plugins, "make it sound warmer").contains("none whose name matches this request"), "no plugin match is stated");
	check(system_prompt.contains("Faust Dev 2"), "system prompt explains that Faust Dev 2 creates the effect");

	const QVector<FxParamState> modulated_states = make_states_with_modulator("System Hp F.");
	check(build_fx_user_message(modulated_states, "test").contains("[modulated]"), "user message marks modulated effects");
}

static void test_voice_condensing(void)
{
	printf("voice condensing:\n");

	QVector<FxParamState> states = make_states();

	const QString message = build_fx_user_message(states, "add an octave");

	// Voice 1 stays a normal row.
	check(message.contains("- System Volume Voice 1 = "), "voice 1 is listed as a normal row");
	check(message.contains("- System Transpose Voice 1 = "), "voice 1 transpose is listed as a normal row");

	// Voices 2-7 are condensed into one line per parameter.
	check(!message.contains("- System Volume Voice 2 = "), "voice 2 volume is not a separate row");
	check(message.contains("Note duplicator voices 2-7"), "the condensed voice section is there");
	check(message.contains("- System Volume Voice N 2-7 = all +0.00 dB"), "voice volume is condensed");
	check(message.contains("- System Start Voice N 2-7 = all 500 ms"), "voice start is condensed");

	// Non-default values are kept, per voice.
	QVector<FxParamState> transposed = make_states();
	for (FxParamState &state : transposed)
	{
		if (state.param.name == "System Transpose Voice 2")
			state.current_native = 12.0f;
		if (state.param.name == "System Transpose Voice 3")
			state.current_native = 19.0f;
	}
	const QString transposed_message = build_fx_user_message(transposed, "x");
	check(transposed_message.contains("- System Transpose Voice N = 12.0 semitones on 2-7, except: 3 = 19.0 semitones"),
	      "the shared value is stated once and only the differing voices are listed");
	check(transposed_message.contains("only the voices whose value differs"), "the condensed format is explained");
	check(!transposed_message.contains("2 = 12.0 semitones"), "a voice that has the shared value is not repeated");
	check(!transposed_message.contains("- System Volume Voice 2 = "), "the condensed form also covers the default parameters");

	// Fewer lines than the seven full rows.
	const int full_rows = 6 * 7;
	const QStringList lines = message.split('\n');
	int voice_lines = 0;
	for (const QString &line : lines)
		if (line.contains(" Voice ") && line.startsWith("- "))
			voice_lines++;
	check(voice_lines < full_rows, "the condensed section is shorter than one row per voice setting");
	check(voice_lines >= 7 + 7, "every voice parameter type is still represented");

	check(fx_voice_number("System Volume Voice 7", NULL) == 7, "voice number parsed");
	check(fx_voice_number("System Volume", NULL) == -1, "a normal effect has no voice number");
	check(fx_voice_number("System Volume Voice 1", NULL) == 1, "voice 1 number parsed");

	QString template_name;
	fx_voice_number("System Pan Voice 4", &template_name);
	check(template_name == "System Pan Voice N", "template name built");
}

static void test_corrections(void)
{
	printf("corrections and the applied deltas:\n");

	const QString system_prompt = build_fx_system_prompt();
	check(system_prompt.contains("Corrections:"), "system prompt has a correction rule");
	check(system_prompt.contains("I meant"), "system prompt recognizes a correction");
	check(system_prompt.contains("Applied in that turn"), "system prompt points at the deltas in the history");
	check(system_prompt.contains("Do not stack the new request on top of a reply the user rejected"), "system prompt forbids stacking");
	check(system_prompt.contains("its <parameter>"), "system prompt maps 'its <parameter>' to the instrument controls");
	check(system_prompt.contains("always mean the current instrument's own controls"), "system prompt maps 'the plugin' to the instrument controls");
	check(system_prompt.contains("never answer a request that names the plugin with system effects"), "system prompt forbids answering a plugin request with system effects");
	check(system_prompt.contains("state the reason in one clause of the explanation"), "system prompt asks for a reason when system effects are used anyway");

	// Status lines use the plugin's own control names, without the wrapper's
	// index prefix ("2: Size" -> "Size").
	check(fx_control_display_name("2: Size") == "Size", "index prefix is stripped for display");
	check(fx_control_display_name("Rev1: Size") == "Size", "layer prefix is stripped for display");
	check(fx_control_display_name("1: Rev1: Dry") == "Dry", "index and layer prefixes are both stripped");
	check(fx_control_display_name("12:  Mix") == "Mix", "prefix and spacing are stripped for display");
	check(fx_control_display_name("System Eq1 F.") == "System Eq1 F.", "a normal effect name is unchanged");
	check(fx_control_display_name("808 Kick") == "808 Kick", "a numeric name is not stripped");

	{
		FxInstrumentChange change;
		change.plugin_name = "Uhbik-A";
		change.name = "2: Size";
		change.display_name = "2: Size";
		change.old_value_scaled = 0.0f;
		change.value_scaled = 0.35f;
		check(fx_summarize_instrument_changes(QVector<FxInstrumentChange>{change}) == "Uhbik-A: Size 0% -> 35%",
		      "the instrument summary shows the readable control name");

		change.name = "1: Rev1: Dry";
		change.display_name = "1: Rev1: Dry";
		change.old_value_scaled = 0.82f;
		change.value_scaled = 0.75f;
		check(fx_summarize_instrument_changes(QVector<FxInstrumentChange>{change}) == "Uhbik-A: Dry 82% -> 75%",
		      "a layered (ZRev-style) control is summarized without its prefixes");
	}

	// The summary helpers the session stores in the history keep every change.
	const QVector<FxParamState> states = make_states();
	QVector<FxChange> changes;
	for (int i = 0; i < 6; i++)
		changes.append(FxChange{QString("System Eq1 F."), 100.0f + i});
	const QString short_summary = fx_summarize_changes(states, changes);
	const QString full_summary = fx_summarize_changes(states, changes, INT_MAX);
	check(short_summary.contains("and 2 more"), "the status summary is truncated");
	check(!full_summary.contains("more"), "the history summary is complete");
	check(full_summary.contains("->"), "the history summary keeps old -> new");
}

static void test_prefixed_control_names(void)
{
	printf("prefixed plugin control names:\n");

	const QVector<FxParamState> states = make_states();

	// Plugin wrappers prefix the parameter names with their index ("2: Size"),
	// but the model - like the user - calls them "Size". That must resolve.
	FxInstrumentContext instrument;
	instrument.type_name = "VST3";
	instrument.plugin_name = "Uhbik-A";
	instrument.plugin_version = 1;

	auto add_control = [&instrument](int index, const char *name)
	{
		FxInstrumentParamState param;
		param.effect_num = index;
		param.name = QString("%1: %2").arg(index).arg(QString::fromUtf8(name));
		param.display_name = param.name;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		instrument.params.append(param);
	};
	add_control(2, "Size");
	add_control(3, "Spread");
	add_control(0, "Active #Amb1");

	{
		const FxParseResult result = parse_fx_response(
		  R"({"instrument_changes":[{"name":"Size","value":0.15},{"name":"Spread","value":0.35},{"name":"Active #Amb1","value":1}]})",
		  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);

		check(result.ok, "ok");
		check(result.warnings.isEmpty(), "the readable names resolve without warnings");
		check(result.instrument_changes.size() == 3, "all readable names resolve");
		check(result.instrument_changes.size() == 3 && result.instrument_changes[0].name == "2: Size", "the change is bound to the exact wrapper name");
		check(result.instrument_changes.size() == 3 && result.instrument_changes[0].effect_num == 2, "the change is bound to the right control index");
		check(result.instrument_changes.size() == 3 && std::fabs(result.instrument_changes[0].value_scaled - 0.15f) < 0.001f, "the value is parsed");
	}

	{
		const FxParseResult result = parse_fx_response(
		  R"({"instrument_changes":[{"name":"3: Spread","value":0.5},{"name":"2 SIZE","value":0.4}]})",
		  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);
		check(result.ok && result.instrument_changes.size() == 2, "the prefixed and the upper-case names still resolve");
	}

	{
		// Case and the trailing period must not matter for system effects either.
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[{"name":"system EQ1 F","value":400},{"name":"SYSTEM EQ1 L.","value":2}]})", states);
		check(result.ok && result.changes.size() == 2, "system effect names are matched ignoring case and punctuation");
		check(result.changes.size() == 2 && result.changes[0].name == "System Eq1 F.", "the change is bound to the catalog spelling");
	}

	check(fx_normalize_control_key("2: Size") == fx_normalize_control_key("Size"), "index prefix is ignored");
	check(fx_normalize_control_key("808 Kick") == fx_normalize_control_key("808 Kick"), "a name that starts with a number keeps it");
	check(fx_normalize_control_key("808 Kick") != fx_normalize_control_key("Kick"), "a numeric name is not truncated");
}

static void test_correction_round_trip(void)
{
	printf("correction round trip (parse -> applied summary -> history):\n");

	const QVector<FxParamState> states = make_states();
	FxInstrumentContext instrument = make_instrument_context();
	instrument.plugin_name = "Uhbik-A";
	instrument.type_name = "VST3";
	instrument.plugin_version = 1;

	// The reply from ~/.radium/llm.log (2026-11-07T17:14): an instrument-only
	// reply whose deltas were lost from the history by an earlier version.
	FxInstrumentParamState size;
	size.effect_num = 2;
	size.name = "2: Size";
	size.display_name = "2: Size";
	size.current_scaled = 0.0f;
	size.current_display = "0.00";
	size.format = FX_INSTRUMENT_PARAM_FLOAT;
	instrument.params.append(size);

	const FxParseResult parsed = parse_fx_response(
	  R"({"changes":[],"instrument_changes":[{"name":"Size","value":0.35}],"modulators":[],"add_plugins":[],"remove_plugins":[],"create_faust":"","create_faust_name":"","ask":"","explanation":"I set the plugin's own reverb controls."})",
	  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);

	const QString applied = fx_applied_summary(parsed, states);
	check(parsed.ok && !parsed.instrument_changes.isEmpty(), "the logged reply parses into instrument changes");
	check(!applied.isEmpty(), "the logged reply produces history deltas");

	// The history entry is built from the same deltas, so a correction can
	// revert them ("old -> new" is present).
	check(applied.contains("Uhbik-A: Size 0% -> 35%"), "the deltas name the control and the old -> new values");
	check(applied.contains("->"), "the deltas are in old -> new form");

	// And the untruncated form keeps every change (a correction needs all).
	QVector<FxInstrumentChange> many = parsed.instrument_changes;
	for (int i = 0; i < 8; i++)
		many.append(parsed.instrument_changes[0]);
	const QString all = fx_summarize_instrument_changes(many, many.size());
	check(!all.contains("more"), "the history deltas are never truncated");
	check(fx_summarize_instrument_changes(many).contains("and 5 more"), "the status summary still truncates");
}

static void test_description_budget(void)
{
	printf("control descriptions and the prompt budget:\n");

	const QVector<FxParamState> states = make_states();

	// A plugin description is capped hard: it is per-control text.
	{
		FxInstrumentParamState param;
		param.description = QString(200, QChar('x'));
		check(fx_control_description(param).size() == FX_CONTROL_DESCRIPTION_MAX,
		      "a long control description is cut to the cap");
		check(fx_control_description(param).endsWith(QChar(0x2026)), "the cut is marked with an ellipsis");

		param.description = "Room size";
		check(fx_control_description(param) == "Room size", "a short description is sent as-is");

		param.description = "  Room   size  ";
		check(fx_control_description(param) == "Room size", "whitespace is normalized");

		param.description = "";
		check(fx_control_description(param).isEmpty(), "a missing description stays empty");
	}

	// u-he's VST3 shortTitle values, as they arrive from the plugin. They are
	// abbreviations of the control name, not descriptions: sending them is
	// noise, and counting them as descriptions suppresses the honest hint that
	// the plugin provides none.
	{
		const struct { const char *name; const char *description; bool useful; } cases[] = {
		  {"Density", "Densty", false},
		  {"Treble", "Trebel", false},
		  {"TrebleFreq", "TrFrq", false},
		  {"Pre-Delay", "Pre", false},
		  {"HF-Range", "HFRng", false},
		  {"Reverb", "Rev", false},
		  {"Modulation", "Mod", false},
		  {"OutputDB", "OutDB", false},
		  {"Active #Amb1", "#Amb1", false},
		  {"Size", "Room size", true},
		  {"Decay", "Reverb tail length", true},
		  {"Mix", "Dry/wet balance", true},
		  {"Size", "Size", false},
		  {"Size", "Hi", false},
		};

		for (const auto &c : cases) {
			FxInstrumentParamState param;
			param.name = c.name;
			param.display_name = c.name;
			param.description = c.description;
			const QString sent = fx_control_description(param);
			const bool ok = c.useful ? (sent == QString::fromUtf8(c.description)) : sent.isEmpty();
			if (!ok)
				printf("    case %s / %s -> sent=[%s]\n", c.name, c.description, sent.toUtf8().constData());
			check(ok, c.useful ? "a real description is sent" : "a name abbreviation is not sent as a description");
		}
	}

	// With only abbreviations, the honest hint is still shown.
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Uhbik-A";
		instrument.type_name = "VST3";
		const char *names[] = {"2: Size", "5: Decay", "6: Density", "9: Treble"};
		const char *descriptions[] = {"", "", "Densty", "Trebel"};
		for (int i = 0; i < 4; i++){
			FxInstrumentParamState param;
			param.effect_num = i;
			param.name = names[i];
			param.display_name = names[i];
			param.description = descriptions[i];
			param.current_scaled = 0.5f;
			param.current_display = "50.00";
			param.format = FX_INSTRUMENT_PARAM_FLOAT;
			param.is_clamped = true;
			instrument.params.append(param);
		}
		const QString message = build_fx_user_message(states, instrument, "What does Size do?");
		check(message.contains("This plugin provides no descriptions"), "abbreviations do not hide the no-description hint");
		check(!message.contains("Densty"), "abbreviations are not in the prompt");
	}

	// Missing descriptions are stated once, so the model can hedge.
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Uhbik-A";
		instrument.type_name = "VST3";
		FxInstrumentParamState param;
		param.effect_num = 2;
		param.name = "2: Size";
		param.display_name = param.name;
		param.current_scaled = 0.5f;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = true;
		instrument.params.append(param);

		const QString message = build_fx_user_message(states, instrument, "What does this plugin do?");
		check(message.contains("This plugin provides no descriptions"), "the absence of descriptions is stated");
		check(message.contains("based on the control's name"), "the model is told to say what its explanation is based on");

		// With a description present, that footnote is not needed.
		instrument.params[0].description = "Room size";
		const QString described_message = build_fx_user_message(states, instrument, "What does this plugin do?");
		check(!described_message.contains("provides no descriptions"), "the footnote disappears when descriptions exist");
		check(described_message.contains("Room size"), "the plugin's description is included in the control list");
	}

	// The control section is bounded: a plugin with very many controls cannot
	// make the prompt arbitrarily expensive.
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Big";
		instrument.type_name = "VST3";
		for (int i = 0; i < 400; i++){
			FxInstrumentParamState param;
			param.effect_num = i;
			param.name = QString("Ch%1: Some Fairly Long Control Name Number %1").arg(i);
			param.display_name = param.name;
			param.description = QString("description for control number %1 which is fairly long too").arg(i);
			param.current_scaled = 0.5f;
			param.current_display = "50.00 %";
			param.format = FX_INSTRUMENT_PARAM_FLOAT;
			param.is_clamped = true;
			instrument.params.append(param);
		}

		const QString message = build_fx_user_message(states, instrument, "make it warmer");

		// The section = from its header to the system-effect values, including
		// the footnotes that belong to it.
		const int section_start = message.indexOf("Current instrument controls");
		const int section_end = message.indexOf("Current system effect values");
		const int section = section_end - section_start;

		check(section < FX_CONTROL_SECTION_BUDGET + 2000, "the control section stays near its budget");
		check(message.contains("because the list is capped to keep the request small"), "the cap is reported to the model");
		check(message.contains("Ch0: Some Fairly Long Control Name Number 0"), "the first controls are listed");
		check(message.contains("more). Ask for one if you need its value."), "the overflow is summarized, not spelled out");
		check(message.count("Some Fairly Long Control Name Number") < 200, "the number of full lines is bounded");
	}
}

static void test_plugin_catalogue_economy(void)
{
	printf("effect-plugin catalogue economy:\n");

	const QVector<FxParamState> states = make_states();

	FxInstrumentContext presswerk;
	presswerk.plugin_name = "Presswerk";
	presswerk.type_name = "VST3";
	presswerk.description = "u-he";
	const char *controls[] = {"Threshold 1", "Ratio 1", "Attack 1", "Soft Knee 1", "Compression 1", "Enhancer 1"};
	for (int i = 0; i < 6; i++){
		FxInstrumentParamState param;
		param.effect_num = i;
		param.name = controls[i];
		param.display_name = param.name;
		param.current_scaled = 0.5f;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = true;
		presswerk.params.append(param);
	}

	// ---- (1) the catalogue is skipped when the request is already covered ----
	check(fx_request_is_covered_by_instrument("Compress like Daft Punk", presswerk, states),
	      "a compressor request is covered by a compressor instrument");
	check(fx_request_is_covered_by_instrument("Add some reverb", presswerk, states),
	      "a reverb request is covered by the built-in Reverb bus");
	check(fx_request_is_covered_by_instrument("Decrease attack by 50%", presswerk, states),
	      "a named control is covered");
	check(!fx_request_is_covered_by_instrument("add a wah effect", presswerk, states),
	      "an effect the built-in effects cannot do is not covered");
	check(!fx_request_is_covered_by_instrument("add a compressor plugin", presswerk, states),
	      "an explicit plugin request is never covered");
	check(!fx_request_is_covered_by_instrument("insert a phaser please", presswerk, states),
	      "an explicit insert request is not covered");

	{
		const QVector<FxPlugin> plugins{
			FxPlugin{QString("Ladspa"), QString("Calf Compressor LADSPA")},
			FxPlugin{QString("VST"), QString("compressor-lr"), QString("compressor-lr")},
			FxPlugin{QString("VST"), QString("compressor-mono"), QString("compressor-mono")},
		};
		const QString covered_message = build_fx_user_message(states, presswerk, plugins, QVector<FxPlugin>(), "Compress like Daft Punk");
		check(covered_message.contains("No effect-plugin list"), "the catalogue is replaced by one short line");
		check(!covered_message.contains("Calf Compressor"), "no plugin names are sent when covered");

		const QVector<FxPlugin> delay_plugins{
			FxPlugin{QString("Ladspa"), QString("Tape Delay Simulation")},
		};
		const QString open_message = build_fx_user_message(states, presswerk, delay_plugins, QVector<FxPlugin>(), "add a tape delay");
		check(open_message.contains("Tape Delay Simulation [Ladspa]"), "the catalogue is sent when not covered");
		check(!open_message.contains("No effect-plugin list"), "no skip line when the catalogue is needed");
	}

	// ---- (2) variants share a line, every exact name is kept ----
	{
		const QVector<FxPlugin> plugins{
			FxPlugin{QString("VST"), QString("compressor-lr"), QString("compressor-lr")},
			FxPlugin{QString("VST"), QString("compressor-mono"), QString("compressor-mono")},
			FxPlugin{QString("VST"), QString("compressor-ms"), QString("compressor-ms")},
			FxPlugin{QString("VST"), QString("compressor-stereo"), QString("compressor-stereo")},
			FxPlugin{QString("VST"), QString("Uhbik-F"), QString("Uhbik.64")},
			FxPlugin{QString("Ladspa"), QString("Auto Wah")},
		};
		// A request whose words match all of them (the section only lists
		// matches; the grouping is what this checks).
		const QString section = fx_plugin_catalog_section(plugins, "add compressor uhbik auto wah");

		check(section.contains("compressor-lr, compressor-mono, compressor-ms, compressor-stereo [VST]"),
		      "the variants are listed on one line with their type");
		for (const char *name : {"compressor-lr", "compressor-mono", "compressor-ms", "compressor-stereo"})
			check(section.contains(name), "every variant keeps its exact name");
		check(!section.contains("(container: compressor-lr)"), "a container equal to the name is not repeated");
		check(section.contains("Uhbik-F [VST] (container: Uhbik.64)"), "a container that differs from the name is kept");
		check(section.contains("Auto Wah [Ladspa]"), "a plugin without variants is listed normally");

		// Grouping must not swallow a plugin that is not part of a family.
		check(section.count("Auto Wah") == 1, "a non-family plugin appears exactly once");
	}
}

static void test_compact_controls(void)
{
	printf("compact control lines (hoisted metadata):\n");

	const QVector<FxParamState> states = make_states();

	auto make_instrument = [](int count, FxInstrumentParamFormat format, bool clamped, const char *unit)
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Big";
		instrument.type_name = "VST";
		for (int i = 0; i < count; i++){
			FxInstrumentParamState param;
			param.effect_num = i;
			param.name = QString("Layer: Knob %1").arg(i);
			param.display_name = param.name;
			param.current_scaled = 0.25f + i * 0.01f;
			param.current_display = QString("%1.00").arg(i);
			param.unit = QString::fromUtf8(unit);
			param.format = format;
			param.is_clamped = clamped;
			param.has_native_range = !clamped;
			param.min_native = 0;
			param.max_native = 1000;
			param.current_native = 100.0f + i;
			instrument.params.append(param);
		}
		return instrument;
	};

	// Homogeneous group -> compact form, type/scale stated once.
	{
		const QString message = build_fx_user_message(states, make_instrument(6, FX_INSTRUMENT_PARAM_FLOAT, true, ""), "x");
		check(message.contains("Every control below is continuous; value = position 0 to 1"), "the shared scale is hoisted");
		check(message.contains("- Layer: Knob 0 = 0.250 (0.00)"), "each line is just name + value");
		check(!message.contains("[continuous;"), "no per-line type boilerplate");

		const int start = message.indexOf("Current instrument controls");
		const int end = message.indexOf("Current system effect values");
		const int section = end - start;
		check(section < 1000, "a 6-control section stays around 1 KB, mostly the fixed header lines");
		check(section - message.mid(0, message.indexOf("Current instrument controls")).size() < 1000,
		      "the control lines themselves are a fraction of the section");
	}

	// Native-range groups hoist the unit instead.
	{
		const QString message = build_fx_user_message(states, make_instrument(4, FX_INSTRUMENT_PARAM_FLOAT, false, "ms"), "x");
		check(message.contains("value = native value, unit ms"), "the unit is hoisted for native-range controls");
		check(message.contains("- Layer: Knob 0 = 100 ms"), "the native value is shown");
	}

	// Mixed formats (on/off next to continuous) keep the verbose form, so no
	// control loses its type.
	{
		FxInstrumentContext instrument = make_instrument(4, FX_INSTRUMENT_PARAM_FLOAT, true, "");
		FxInstrumentParamState onoff;
		onoff.effect_num = 9;
		onoff.name = "Layer: Bypass";
		onoff.display_name = onoff.name;
		onoff.format = FX_INSTRUMENT_PARAM_BOOL;
		instrument.params.append(onoff);

		const QString message = build_fx_user_message(states, instrument, "x");
		check(message.contains("[continuous; position"), "a mixed group keeps per-line metadata");
		check(message.contains("[on/off"), "an on/off control keeps its own type");
		check(!message.contains("Every control below is continuous"), "no incorrect hoisted claim for a mixed group");
	}

	// A single control is not "homogeneous": one line is already minimal.
	{
		const QString message = build_fx_user_message(states, make_instrument(1, FX_INSTRUMENT_PARAM_FLOAT, true, ""), "x");
		check(message.contains("[continuous; position 0.250"), "a single control keeps the verbose form");
	}
}

static std::vector<float> make_tone(double freq, double amplitude, int num_frames, int sample_rate)
{
	std::vector<float> samples(num_frames);
	for (int i = 0; i < num_frames; i++)
		samples[i] = (float)(amplitude * sin(2.0 * M_PI * freq * i / sample_rate));
	return samples;
}

static void test_audio_analysis(void)
{
	printf("audio analysis (measurements for the prompt):\n");

	const int sample_rate = 48000;
	const int num_frames = 4096;

	check(AUDIO_ANALYZER_NUM_BANDS > 0, "the band table exists");
	{
		bool ordered = true;
		for (int band = 0; band < AUDIO_ANALYZER_NUM_BANDS; band++){
			const AudioAnalyzerBand e = AUDIO_ANALYZER_band(band);
			if (!(e.low_hz < e.high_hz))
				ordered = false;
		}
		check(ordered, "every band has a valid range");
		check(AUDIO_ANALYZER_band(0).low_hz <= 30.0f, "the lowest band starts at or below 30 Hz");
	}

	// Level figures.
	{
		std::vector<float> tone = make_tone(1000.0, 1.0, num_frames, sample_rate);
		AudioAnalysis analysis;
		const bool ok = AUDIO_ANALYZE(tone.data(), tone.data(), num_frames, sample_rate, &analysis);
		check(ok && analysis.valid, "a tone is analyzed");
		check(fabsf(analysis.peak_db - 0.0f) < 0.5f, "a full-scale tone peaks at 0 dBFS");
		check(fabsf(analysis.rms_db + 3.0f) < 0.5f, "a full-scale sine has -3 dBFS RMS");
		check(fabsf(analysis.crest_db - 3.0f) < 0.5f, "a sine has a 3 dB crest factor");
	}

	// The bands find a tone where it belongs, at the right level.
	{
		std::vector<float> bass = make_tone(80.0, 0.5, num_frames, sample_rate);
		AudioAnalysis analysis;
		AUDIO_ANALYZE(bass.data(), bass.data(), num_frames, sample_rate, &analysis);

		int loudest = 0;
		for (int band = 1; band < AUDIO_ANALYZER_NUM_BANDS; band++)
			if (analysis.band_db[band] > analysis.band_db[loudest])
				loudest = band;

		const AudioAnalyzerBand edges = AUDIO_ANALYZER_band(loudest);
		check(edges.low_hz <= 80.0f && edges.high_hz >= 80.0f, "an 80 Hz tone lands in the band containing 80 Hz");
		check(fabsf(analysis.band_db[loudest] + 6.0f) < 1.5f, "a -6 dBFS tone measures about -6 dB in its band");
	}

	{
		std::vector<float> treble = make_tone(10000.0, 0.25, num_frames, sample_rate);
		AudioAnalysis analysis;
		AUDIO_ANALYZE(treble.data(), treble.data(), num_frames, sample_rate, &analysis);

		int loudest = 0;
		for (int band = 1; band < AUDIO_ANALYZER_NUM_BANDS; band++)
			if (analysis.band_db[band] > analysis.band_db[loudest])
				loudest = band;

		const AudioAnalyzerBand edges = AUDIO_ANALYZER_band(loudest);
		check(edges.low_hz <= 10000.0f && edges.high_hz >= 10000.0f, "a 10 kHz tone lands in the band containing 10 kHz");
	}

	// A broadband signal reports both ends; a crest-heavy one reports a big crest.
	{
		std::vector<float> both = make_tone(80.0, 0.5, num_frames, sample_rate);
		std::vector<float> high = make_tone(10000.0, 0.25, num_frames, sample_rate);
		for (int i = 0; i < num_frames; i++)
			both[i] += high[i];

		AudioAnalysis analysis;
		AUDIO_ANALYZE(both.data(), both.data(), num_frames, sample_rate, &analysis);
		check(analysis.band_db[1] > -12.0f, "the bass tone shows in the low band");
		check(analysis.band_db[AUDIO_ANALYZER_NUM_BANDS - 2] > -20.0f, "the treble tone shows in the high band");
		check(analysis.band_db[5] < -30.0f, "an empty band is far below the tones");
	}

	{
		std::vector<float> spike(num_frames, 0.0f);
		spike[num_frames / 2] = 0.9f;
		AudioAnalysis analysis;
		AUDIO_ANALYZE(spike.data(), spike.data(), num_frames, sample_rate, &analysis);
		check(analysis.crest_db > 20.0f, "a single spike has a large crest factor");
	}

	// Nothing to measure must be reported as nothing, never as a flat spectrum.
	{
		AudioAnalysis analysis;
		std::vector<float> silence(num_frames, 0.0f);
		AUDIO_ANALYZE(silence.data(), silence.data(), num_frames, sample_rate, &analysis);
		check(analysis.peak_db < -100.0f, "silence measures as silence");

		check(!AUDIO_ANALYZE(silence.data(), silence.data(), 10, sample_rate, &analysis), "too few samples is refused");
	}
}

static void test_measurement_rule(void)
{
	printf("measurement guidance in the prompt:\n");

	const QString rules = build_fx_system_prompt();

	check(rules.contains("What it currently measures"), "the rules point at the measurement block by name");
	check(rules.contains("contains a 'What it currently measures' paragraph"),
	      "the rules say where the block is (not 'starts with': it follows the plugin line)");
	check(rules.contains("describe what it sounds like right now"), "the rules say what the numbers are");
	check(rules.contains("the band that sits well below its neighbours"), "the rules say how to pick an EQ band from the bands");
	check(rules.contains("crest factor is the reference for dynamics"), "the rules explain the crest factor");
	check(rules.contains("a higher crest means punchier"), "the direction of the crest factor is stated");
	check(rules.contains("no measurements, the instrument was not audible"), "the missing-measurement case is covered");
	check(rules.contains("instead of inventing numbers"), "the model is told not to invent numbers");

	// The guidance must not claim that measurements always exist: they only do
	// when the instrument was playing.
	check(!rules.contains("the numbers are always available"), "no false promise about availability");

	check(rules.contains("Never boost a band whose measured level is already the highest one"), "the rules forbid boosting the loudest band");
	// No request-specific recipes ('flat response', ...): the model works those
	// out from the general guidance above and the band levels.
	check(!rules.contains("For 'flat response'"), "no request-specific recipe for a flat response");
	check(rules.contains("measured some seconds ago"), "the rules mention the age of the numbers");

	// From the session where "keep the same volume" overshot twice.
	check(rules.contains("use the 'at its loudest, 400 ms' loudness"), "volume requests are told which loudness figure to use");
	check(rules.contains("never match a level with either of those"), "the window average and 'now' are ruled out for level matching");
	check(rules.contains("more than about 12 dB"), "an implausible level difference is called out");
	check(rules.contains("target loudness - current loudness"), "the rules give the dB arithmetic for a volume change");
	check(rules.contains("applied with ONE control"), "the rules forbid stacking small corrections");
	check(rules.contains("is the reference for 'before'"), "the earlier measurement is the reference for 'before'");
	check(rules.contains("no earlier measurement exists, say so"), "the model must not pretend to match an unseen level");
	check(rules.contains("If the peak is above 0 dB the output is clipping"), "clipping is called out");
}

static void test_song_timing(void)
{
	printf("song tempo and signature in the prompt:\n");

	const QVector<FxParamState> states = make_states();

	FxInstrumentContext instrument;
	instrument.plugin_name = "Presswerk";
	instrument.type_name = "VST3";
	FxInstrumentParamState param;
	param.effect_num = 0;
	param.name = "Ratio 1";
	param.display_name = param.name;
	param.current_scaled = 0.3f;
	param.format = FX_INSTRUMENT_PARAM_FLOAT;
	param.is_clamped = true;
	instrument.params.append(param);

	// No timing known: no line, no claim.
	{
		const QString message = build_fx_user_message(states, instrument, "x");
		check(!message.contains("Song tempo"), "no tempo line without a tempo");
	}

	// 120 BPM, 4/4: a beat is 500 ms and a bar 2000 ms.
	{
		FxInstrumentContext timed = instrument;
		timed.bpm = 120.0;
		timed.signature_numerator = 4;
		timed.signature_denominator = 4;

		const QString message = build_fx_user_message(states, timed, "x");
		check(message.contains("Song tempo: 120 BPM, 4/4 signature"), "the tempo and signature are sent");
		check(message.contains("one beat = 500 ms"), "a beat length is given in ms");
		check(message.contains("one bar = 2000 ms"), "a bar length is given in ms");
	}

	// 128 BPM, 3/4: beat 469 ms, bar 1406 ms.
	{
		FxInstrumentContext timed = instrument;
		timed.bpm = 128.0;
		timed.signature_numerator = 3;
		timed.signature_denominator = 4;

		const QString message = build_fx_user_message(states, timed, "x");
		check(message.contains("Song tempo: 128 BPM, 3/4 signature"), "128 BPM is shown as an integer");
		check(message.contains("one beat = 469 ms"), "the beat length is computed from the tempo");
		check(message.contains("one bar = 1406 ms"), "the bar length follows the signature");
	}

	// The rules must point at it.
	const QString rules = build_fx_system_prompt();
	check(rules.contains("The song tempo"), "the rules mention the song tempo");
	check(rules.contains("cycles per beat"), "the rules explain the modulator rate unit");
	check(rules.contains("compute it from the tempo instead of guessing"), "the rules forbid guessing durations");
}

static void test_measurement_history(void)
{
	printf("measurement reference in the history:\n");

	FxInstrumentMeasurement measurement;
	check(fx_measurement_summary(measurement).isEmpty(), "no summary for an absent measurement");

	measurement.valid = true;
	measurement.peak_db = -12.0f;
	measurement.rms_db = -18.1f;
	measurement.crest_db = 6.2f;
	const QString summary = fx_measurement_summary(measurement);
	check(summary.contains("loudness") && summary.contains("crest 6.2 dB"), "the summary has the level figures");

	// It must be small: it is repeated for every turn in the history.
	check(summary.size() < 90, "the summary is compact");
}

static void test_audio_analysis_tier1(void)
{
	printf("audio analysis, Tier 1 figures:\n");

	const int sample_rate = 48000;
	const int num_frames = sample_rate; // 1 s

	auto tone = [&](double freq, double amplitude)
	{
		std::vector<float> v(num_frames);
		for (int i = 0; i < num_frames; i++)
			v[i] = (float)(amplitude * sin(2.0 * M_PI * freq * i / sample_rate));
		return v;
	};

	// ---- loudness: BS.1770 reference behaviour ----
	// A -6 dBFS 1 kHz sine, both channels: about -6 LUFS.
	{
		std::vector<float> v = tone(1000.0, 0.5);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(fabsf(a.loudness_window_lufs + 6.0f) < 1.0f, "a -6 dBFS stereo 1 kHz sine reads about -6 LUFS");
		check(fabsf(a.loudness_momentary_lufs - a.loudness_window_lufs) < 0.5f,
		      "a steady tone has the same momentary and window loudness");
	}

	// -12 dB of gain should move loudness by -12 LU, i.e. loudness is a level.
	{
		std::vector<float> loud = tone(1000.0, 0.5);
		std::vector<float> quiet = tone(1000.0, 0.125); // -12 dB
		AudioAnalysis a, b;
		AUDIO_ANALYZE(loud.data(), loud.data(), num_frames, sample_rate, &a);
		AUDIO_ANALYZE(quiet.data(), quiet.data(), num_frames, sample_rate, &b);
		check(fabsf((a.loudness_window_lufs - b.loudness_window_lufs) - 12.0f) < 0.5f,
		      "12 dB of gain reads as 12 LU");
	}

	// ---- loudness: which figure describes the sound ----
	// A steady tone: all three figures agree.
	// A short note followed by silence (the log's decaying sample): the window
	// average and "now" collapse, the loudest figure does not. Matching levels
	// with the collapsed figure is what asked for +25 dB.
	{
		std::vector<float> steady = tone(500.0, 0.5);
		AudioAnalysis a;
		AUDIO_ANALYZE(steady.data(), steady.data(), num_frames, sample_rate, &a);
		check(fabsf(a.loudness_loudest_lufs - a.loudness_window_lufs) < 0.5f,
		      "a steady sound has the same loudest and average loudness");

		// The real capture window is 3 s, which is where a single short note
		// leaves mostly silence behind (the log's case).
		const int long_frames = 3 * sample_rate;
		std::vector<float> shortNote(long_frames, 0.0f);
		for (int i = 0; i < sample_rate * 3 / 10; i++) // 300 ms of tone
			shortNote[i] = (float)(0.5 * sin(2.0 * M_PI * 500.0 * i / sample_rate));
		AudioAnalysis b;
		AUDIO_ANALYZE(shortNote.data(), shortNote.data(), long_frames, sample_rate, &b);

		check(b.loudness_loudest_lufs > a.loudness_window_lufs - 3.0f,
		      "the loudest figure stays near the note's own level despite the silence");
		check(b.loudness_window_lufs < a.loudness_window_lufs - 6.0f,
		      "the window average collapses when the window is mostly silence");
		check(b.loudness_momentary_lufs < b.loudness_loudest_lufs - 6.0f,
		      "the momentary figure shows that the instrument is decaying right now");
	}

	// ---- true peak ----
	{
		std::vector<float> v = tone(1000.0, 0.5);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(a.true_peak_db >= a.peak_db - 0.2f, "the true peak is never below the sample peak");
		check(a.true_peak_db < 1.0f, "a -6 dBFS sine does not read as clipping");
	}

	// ---- stereo ----
	{
		std::vector<float> l = tone(1000.0, 0.5);
		std::vector<float> r = l; // identical channels
		AudioAnalysis a;
		AUDIO_ANALYZE(l.data(), r.data(), num_frames, sample_rate, &a);
		check(a.stereo_correlation > 0.99f, "identical channels are fully correlated");
		check(a.stereo_side_db < -60.0f, "identical channels have no side signal");

		for (int i = 0; i < num_frames; i++)
			r[i] = -l[i]; // out of phase
		AUDIO_ANALYZE(l.data(), r.data(), num_frames, sample_rate, &a);
		check(a.stereo_correlation < -0.99f, "out-of-phase channels correlate at -1");

		for (int i = 0; i < num_frames; i++)
			r[i] = 0.5f * l[i]; // right channel 6 dB down
		AUDIO_ANALYZE(l.data(), r.data(), num_frames, sample_rate, &a);
		check(fabsf(a.stereo_balance_db + 6.0f) < 0.5f, "a 6 dB difference reads as -6 dB (right minus left)");
	}

	// ---- spectral shape ----
	{
		std::vector<float> low = tone(200.0, 0.5);
		std::vector<float> high = tone(5000.0, 0.5);
		AudioAnalysis a, b;
		AUDIO_ANALYZE(low.data(), low.data(), num_frames, sample_rate, &a);
		AUDIO_ANALYZE(high.data(), high.data(), num_frames, sample_rate, &b);
		check(fabsf(a.centroid_hz - 200.0f) < 30.0f, "a 200 Hz tone has its centroid at 200 Hz");
		check(fabsf(b.centroid_hz - 5000.0f) < 200.0f, "a 5 kHz tone has its centroid at 5 kHz");
		check(a.flatness < 0.2f, "a tone is not noise-like");

		std::vector<float> noise(num_frames);
		unsigned int seed = 1;
		for (int i = 0; i < num_frames; i++){
			seed = seed * 1103515245u + 12345u;
			noise[i] = 0.1f * (((seed >> 16) & 0x7fff) / 16384.0f - 1.0f);
		}
		AudioAnalysis c;
		AUDIO_ANALYZE(noise.data(), noise.data(), num_frames, sample_rate, &c);
		check(c.flatness > 0.35f, "white noise is noise-like");
	}

	// ---- hum and DC ----
	{
		std::vector<float> hum(num_frames);
		for (int i = 0; i < num_frames; i++)
			hum[i] = (float)(0.05 * sin(2.0 * M_PI * 60.0 * i / sample_rate) + 0.2 * sin(2.0 * M_PI * 1000.0 * i / sample_rate));
		AudioAnalysis a;
		AUDIO_ANALYZE(hum.data(), hum.data(), num_frames, sample_rate, &a);
		check(a.hum_hz > 0.0f, "a 60 Hz hum peak is found");
		check(fabsf(a.hum_hz - 60.0f) < 1.0f, "the hum is reported at 60 Hz");

		std::vector<float> clean = tone(1000.0, 0.5);
		AudioAnalysis b;
		AUDIO_ANALYZE(clean.data(), clean.data(), num_frames, sample_rate, &b);
		check(b.hum_hz == 0.0f, "a clean tone has no hum");
	}

	{
		std::vector<float> v = tone(1000.0, 0.2);
		for (float &sample : v)
			sample += 0.1f; // DC offset
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(fabsf(a.dc_offset - 0.1f) < 0.01f, "a DC offset is measured");
	}

	// ---- transients ----
	{
		// Four clicks per second: onsets, no sustained attack measured.
		std::vector<float> clicks(num_frames, 0.0f);
		for (int c = 0; c < 4; c++)
			for (int i = 0; i < 200; i++)
				clicks[6000 + 12000 * c + i] = 0.8f * expf(-i / 40.0f);
		AudioAnalysis a;
		AUDIO_ANALYZE(clicks.data(), clicks.data(), num_frames, sample_rate, &a);
		check(a.onsets_per_second > 3.0f && a.onsets_per_second < 5.0f, "four clicks per second are counted");
		check(a.crest_db > 20.0f, "clicks have a high crest factor");

		// A steady tone has neither.
		std::vector<float> steady = tone(1000.0, 0.5);
		AudioAnalysis b;
		AUDIO_ANALYZE(steady.data(), steady.data(), num_frames, sample_rate, &b);
		check(b.onsets_per_second == 0.0f, "a steady tone has no onsets");
		check(b.attack_db_per_ms < 0.2f, "a steady tone has no attack rate");
	}

	// Mono input (right == NULL) works and reports no stereo.
	{
		std::vector<float> v = tone(440.0, 0.5);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), NULL, num_frames, sample_rate, &a);
		check(a.valid, "a mono block is analyzed");
		check(a.stereo_side_db < -60.0f, "a mono block has no side signal");
	}
}

static void test_audio_decay(void)
{
	printf("space (reverb decay) analysis:\n");

	const int sample_rate = 48000;
	const int num_frames = 3 * sample_rate;

	// A 50 ms burst followed by an exponential decay with a known RT60.
	auto burst = [&](double rt60_s)
	{
		std::vector<float> v(num_frames, 0.0f);
		const double decay_db_per_s = 60.0 / rt60_s;
		unsigned int seed = 7;
		for (int i = 0; i < num_frames; i++){
			const double t = (double)i / sample_rate;
			double amplitude = t < 0.05 ? 1.0 : pow(10.0, -decay_db_per_s * (t - 0.05) / 20.0);
			if (amplitude < 1e-4)
				amplitude = 0.0;
			seed = seed * 1103515245u + 12345u;
			const double noise = ((seed >> 16) & 0x7fff) / 16384.0 - 1.0;
			v[i] = (float)(0.5 * amplitude * noise);
		}
		return v;
	};

	{
		std::vector<float> v = burst(0.3);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(fabsf(a.decay_time_s - 0.3f) < 0.1f, "a 0.3 s decay is estimated as ~0.3 s (short/dry)");
		check(a.tail_db < -35.0f, "a 0.3 s tail is far below the peak 300 ms later");
	}

	{
		std::vector<float> v = burst(1.0);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(fabsf(a.decay_time_s - 1.0f) < 0.25f, "a 1 s decay is estimated as ~1 s");
		check(a.tail_db > -25.0f && a.tail_db < -8.0f, "a 1 s tail is a moderate distance below the peak");
	}

	{
		std::vector<float> v = burst(2.0);
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(fabsf(a.decay_time_s - 2.0f) < 0.5f, "a 2 s decay is estimated as ~2 s (long/wet)");
		check(a.tail_db > -12.0f, "a long tail stays close to the peak");
	}

	// Sustained material has no decay to measure: the model must not be fed a
	// made-up reverb time.
	{
		std::vector<float> v(num_frames);
		for (int i = 0; i < num_frames; i++)
			v[i] = (float)(0.5 * sin(2.0 * M_PI * 500.0 * i / sample_rate));
		AudioAnalysis a;
		AUDIO_ANALYZE(v.data(), v.data(), num_frames, sample_rate, &a);
		check(a.decay_time_s == 0.0f, "a sustained tone has no decay estimate");
		check(a.tail_db == 0.0f, "a sustained tone has no tail figure");
	}

	// And the prompt must show the space line, and hide it when unknown.
	{
		const QVector<FxParamState> states = make_states();
		FxInstrumentContext instrument;
		instrument.plugin_name = "Uhbik-A";
		instrument.type_name = "VST3";
		FxInstrumentParamState param;
		param.effect_num = 2;
		param.name = "2: Size";
		param.display_name = param.name;
		param.current_scaled = 0.5f;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = true;
		instrument.params.append(param);

		FxInstrumentContext measured = instrument;
		measured.measurement.valid = true;
		measured.measurement.sample_rate = 48000;
		measured.measurement.decay_time_s = 1.35f;
		measured.measurement.tail_db = -14.2f;
		measured.measurement.bands << "60-120 Hz -9";

		const QString message = build_fx_user_message(states, measured, "more reverb");
		check(message.contains("- space: decay 1.35 s (60 dB)"), "the decay time is in the prompt");
		check(message.contains("level 300 ms after the peak -14.2 dB"), "the tail level is in the prompt");

		// A decay longer than the captured window cannot be measured; it must
		// not be printed as a number (the log had a bogus 7.90 s).
		measured.measurement.sample_rate = 48000;
		measured.measurement.num_frames = 144000; // 3 s
		measured.measurement.decay_time_s = 3.0f;
		const QString long_decay = build_fx_user_message(states, measured, "more reverb");
		check(long_decay.contains("space: longer than the 3.0 s window"), "a decay at the window length is reported as longer than the window");

		measured.measurement.decay_time_s = 0.0f;
		const QString no_decay = build_fx_user_message(states, measured, "more reverb");
		check(no_decay.contains("space: not measurable"), "an unmeasurable decay says so (no 'unknown s')");
		check(!no_decay.contains("unknown"), "no malformed 'unknown' text");

		// The stereo line is always present: mono is the case a width request
		// has to know about (it was hidden in the log).
		{
			FxInstrumentContext mono = measured;
			mono.measurement.stereo_correlation = 1.0f;
			mono.measurement.stereo_side_db = -120.0f;
			mono.measurement.stereo_balance_db = 0.0f;
			const QString mono_message = build_fx_user_message(states, mono, "narrow lows, wide highs");
			check(mono_message.contains("- stereo: correlation 1.00"), "the stereo line is always shown");
			check(mono_message.contains("no side signal (the material is mono)"), "mono material is stated as mono");
		}

		const QString plain = build_fx_user_message(states, instrument, "more reverb");
		check(!plain.contains("- space:"), "no space line without a decay measurement");
	}

	const QString rules = build_fx_system_prompt();
	check(rules.contains("the decay figure is what the space currently does"), "the rules explain the decay figure");
	check(rules.contains("prefer its own Size/Decay/Damping/Mix"), "a reverb's own controls are preferred over the send");
}

static void test_measurement_prompt(void)
{
	printf("measurements in the prompt:\n");

	const QVector<FxParamState> states = make_states();

	FxInstrumentContext instrument;
	instrument.plugin_name = "Presswerk";
	instrument.type_name = "VST3";
	FxInstrumentParamState param;
	param.effect_num = 0;
	param.name = "Ratio 1";
	param.display_name = param.name;
	param.current_scaled = 0.3f;
	param.format = FX_INSTRUMENT_PARAM_FLOAT;
	param.is_clamped = true;
	instrument.params.append(param);

	// No measurement: no block, and no claim about the sound.
	{
		const QString message = build_fx_user_message(states, instrument, "x");
		check(!message.contains("measure"), "no measurement block without a measurement");
	}

	// With a measurement: one compact line.
	{
		FxInstrumentContext measured = instrument;
		measured.measurement.valid = true;
		measured.measurement.sample_rate = 48000;
		measured.measurement.peak_db = -3.2f;
		measured.measurement.rms_db = -14.0f;
		measured.measurement.crest_db = 10.8f;
		measured.measurement.bands << "60-120 Hz -9" << "250-500 Hz -4";

		const QString message = build_fx_user_message(states, measured, "more bass");
		check(message.contains("What it currently measures"), "the measurement block is present");
		check(message.contains("crest 10.8 dB"), "the crest factor is reported");
		check(!message.contains("dBFS"), "the levels are not called dBFS (they can be above full scale)");
		check(message.contains("true peak"), "the true peak is reported");
		check(message.contains("loudness"), "the loudness is reported");
		check(message.contains("60-120 Hz -9, 250-500 Hz -4"), "the band levels are reported");

		// It must stay small: this is sent on every turn.
		const int start = message.indexOf("What it currently measures");
		const int end = message.indexOf("Note: names with a number suffix");
		check(end - start < 900, "the measurement block is bounded");
	}
}

static void test_instrument_summary(void)
{
	printf("instrument summary line:\n");

	const QVector<FxParamState> states = make_states();

	auto make_instrument = [](const char *description)
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Uhbik-A";
		instrument.type_name = "VST3";
		instrument.description = QString::fromUtf8(description);
		FxInstrumentParamState param;
		param.effect_num = 0;
		param.name = "2: Size";
		param.display_name = param.name;
		param.current_scaled = 0.5f;
		param.display_name = param.name;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = true;
		instrument.params.append(param);
		return instrument;
	};

	// A short summary replaces the metadata dump.
	{
		const QString message = build_fx_user_message(states, make_instrument("u-he, Fx|u-he"), "What does this plugin do?");
		check(message.contains("Uhbik-A [VST3] (u-he, Fx|u-he)"), "the summary is shown next to the plugin name");
		check(!message.contains("Version:"), "the metadata dump is not sent when a summary exists");
	}

	// A long summary is bounded: this line is sent on every turn.
	{
		const QString message = build_fx_user_message(states, make_instrument(QString(400, QChar('x')).toUtf8().constData()), "x");
		const int start = message.indexOf("Uhbik-A [VST3] (");
		const int end = message.indexOf(")\n", start);
		check(end - start < 200, "the summary line is bounded");
	}

	// No summary at all: the line still works.
	{
		const QString message = build_fx_user_message(states, make_instrument(""), "x");
		check(message.contains("Uhbik-A [VST3]\n"), "the plugin line works without a summary");
	}
}

static void test_unit_reporting(void)
{
	printf("control units in the prompt:\n");

	const QVector<FxParamState> states = make_states();

	auto make_instrument = [](const char *display, const char *unit)
	{
		FxInstrumentContext instrument;
		instrument.plugin_name = "Uhbik-A";
		instrument.type_name = "VST3";
		FxInstrumentParamState param;
		param.effect_num = 2;
		param.name = "2: Size";
		param.display_name = param.name;
		param.current_scaled = 0.5f;
		param.current_display = QString::fromUtf8(display);
		param.unit = QString::fromUtf8(unit);
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = true; // JUCE-hosted plugins report no native range
		instrument.params.append(param);
		return instrument;
	};

	// A unit the display text does not show is reported, so the model does not
	// have to treat the value as a bare 0..1 position.
	{
		const QString message = build_fx_user_message(states, make_instrument("50.00", "%"), "x");
		check(message.contains("; unit %]"), "a unit missing from the display text is included");
	}

	// When the display text already shows it, it is not repeated.
	{
		const QString message = build_fx_user_message(states, make_instrument("50.00 %", "%"), "x");
		check(!message.contains("; unit %]"), "a unit already visible in the display text is not repeated");
	}

	// No unit: nothing is claimed.
	{
		const QString message = build_fx_user_message(states, make_instrument("50.00", ""), "x");
		check(!message.contains("; unit "), "no unit is reported when the plugin has none");
	}
}

static void test_question_detection(void)
{
	printf("question detection (answer, don't tune):\n");

	// From ~/.radium/llm.log: both of these were treated as tuning requests
	// (and produced empty replies / an answer crammed into 'ask').
	check(fx_request_is_a_question("What does this plugin do?"), "a question mark means a question");
	check(fx_request_is_a_question("Please explain Density and Size."), "an explanation request is a question");
	check(fx_request_is_a_question("explain the mix"), "explain without punctuation is a question");
	check(fx_request_is_a_question("why does it sound muddy?"), "why is a question");
	check(fx_request_is_a_question("What's the difference between Size and Decay"), "what's ... is a question");
	check(fx_request_is_a_question("I'd like to know what Mix does"), "know what ... is a question");

	// Tuning requests must never be mistaken for questions.
	check(!fx_request_is_a_question("Make it sound like we're in the Abbey Road studio when the Beatles recorded their albums."), "a vibe request is not a question");
	check(!fx_request_is_a_question("Make it sound more like the 60-ties."), "a subjective request is not a question");
	check(!fx_request_is_a_question("Increase bass"), "a plain instruction is not a question");
	check(!fx_request_is_a_question("Set modulation to 4 hz."), "a value instruction is not a question");
	check(!fx_request_is_a_question("make it warmer"), "a one-word request is not a question");
	check(!fx_request_is_a_question("Add a wah effect"), "a plugin request is not a question");

	// The last instruction of the user message must agree with the rules.
	{
		const QVector<FxParamState> states = make_states();
		const QString question_message = build_fx_user_message(states, "What does this plugin do?");
		check(question_message.contains("This is a question: answer it in \"answer\""), "a question request says: answer it");
		check(!question_message.contains("is answered with \"instrument_changes\""), "a question request does not say: change instrument controls");

		const QString tuning_message = build_fx_user_message(states, "Make it sound like a boom box");
		check(tuning_message.contains("is answered with \"instrument_changes\""), "a tuning request keeps the instrument-first reminder");
		check(!tuning_message.contains("This is a question"), "a tuning request is not called a question");
	}

	// Both remain a valid JSON object request.
	check(build_fx_system_prompt().contains("A question is answered, not tuned"), "the system prompt has the answering rule");
}

static void test_answer_field(void)
{
	printf("answer field (questions are answered, not tuned):\n");

	const QVector<FxParamState> states = make_states();

	// The reply from the log for "What does this plugin do?" used 'ask' to
	// explain the plugin back. Now the schema has a dedicated field.
	{
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[],"instrument_changes":[],"ask":"","answer":"Uhbik-A is a reverb: Size and Decay shape the room, Mix sets the wet level.","explanation":""})",
		  states);
		check(result.ok, "an answer-only reply is valid");
		check(result.answer.startsWith("Uhbik-A is a reverb"), "the answer is parsed");
		check(result.ask.isEmpty(), "the answer is not mixed into 'ask'");
		check(result.changes.isEmpty() && result.instrument_changes.isEmpty(), "an answer changes nothing");
	}

	{
		const FxParseResult result = parse_fx_response(
		  R"({"answer":"It is a reverb.","explanation":"No parameters were changed."})", states);
		check(result.ok && result.answer == "It is a reverb." && result.explanation == "No parameters were changed.",
		      "answer and explanation are separate fields");
	}

	{
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[{"name":"System Eq1 L.","value":3}],"answer":"I also raised the EQ."})", states);
		check(result.ok && result.changes.size() == 1 && result.answer == "I also raised the EQ.",
		      "an answer can accompany a change");
	}

	check(parse_fx_response(R"({"answer":""})", states).ok, "an empty answer is still a valid reply");
	check(!parse_fx_response(R"({"explanation":"no action"})", states).ok, "explanation alone is still not a reply");

	// The schema and the rules must teach it.
	const QString system_prompt = build_fx_system_prompt();
	check(system_prompt.contains("\"answer\":\"<an answer of at most 200 characters"), "the schema has the answer field");
	check(system_prompt.contains("A question is answered, not tuned"), "there is a rule for answering questions");
	check(system_prompt.contains("Never put the answer in"), "the rule says not to put the answer in 'ask'");
	check(system_prompt.contains("at most 200 characters"), "the answer length limit matches the relay's string cap");
	check(build_fx_user_message(make_states(), "What does this plugin do?").contains("at most 200 characters"),
	      "the question-typed reminder repeats the length limit");
	check(system_prompt.contains("never change parameters to answer a question"), "the rule forbids tuning as an answer");
	check(system_prompt.contains("instead of inventing it"), "the model must not invent what a control does");

	// The no-budget-padding guidance from the log ("60-ties" filled the cap).
	check(system_prompt.contains("never add a change just to fill"), "unchanged controls are not added to fill the budget");
}

static void test_prompt_layout(void)
{
	printf("prompt layout (prefix caching):\n");

	const QString intro = build_fx_system_prompt_intro();
	const QString reference = fx_system_effect_reference();
	const QString rules = build_fx_system_prompt_rules();
	const QString prompt = build_fx_system_prompt();

	check(!intro.isEmpty() && !reference.isEmpty() && !rules.isEmpty(), "the system prompt is built from three parts");
	check(prompt.size() == intro.size() + reference.size() + rules.size(), "the parts are concatenated without loss");
	check(prompt.indexOf("Effect reference") < prompt.indexOf("Rules:"),
	      "the stable reference comes before the frequently edited rules");
	check(intro.indexOf("Schema:") >= 0 && intro.indexOf("Rules:") < 0, "the intro holds the schema, not the rules");
	check(reference.indexOf("- System Eq1 F.") >= 0, "the reference lists the system effects");
	check(rules.indexOf("1) Use only names from the provided lists") >= 0, "the rules part starts at rule 1");

	// No per-request data may leak into the system prompt: it must not depend
	// on the instrument, the values or the request.
	const QString again = build_fx_system_prompt();
	check(prompt == again, "the system prompt is static");

	// The clamped-control caveat is a single footnote, not repeated per line.
	{
		const QVector<FxParamState> states = make_states();
		FxInstrumentContext instrument;
		instrument.plugin_name = "ZRev";
		instrument.type_name = "VST";
		for (int i = 0; i < 12; i++){
			FxInstrumentParamState param;
			param.effect_num = i;
			param.name = QString("Rev1: Control %1").arg(i);
			param.display_name = param.name;
			param.current_scaled = 0.5f;
			param.current_display = "50.00%";
			param.format = FX_INSTRUMENT_PARAM_FLOAT;
			param.is_clamped = true;
			instrument.params.append(param);
		}

		const QString message = build_fx_user_message(states, instrument, "x");
		check(message.count("(the plugin reports no range)") == 1, "the caveat appears exactly once, in the header");
		check(message.contains("Every control below is continuous; value = position 0 to 1 (the plugin reports no range)"),
		      "the shared type and scale are stated once");
		check(message.contains("- Rev1: Control 0 = 0.500 (50.00%)"), "a control line only carries its value");
		check(!message.contains("[continuous;"), "the per-line type boilerplate is gone");
		check(message.contains("Names ending in ' 1'/' 2'"), "the instance-name hint is present");
		check(message.contains("'Layer: Control' (for example 'Rev1: Size')"), "the layer/control name form is explained");
	}

	// Opaque instance names get one legend line.
	{
		const QVector<FxParamState> states = make_states();
		FxInstrumentContext instrument;
		instrument.plugin_name = "ZRev";
		instrument.type_name = "VST";
		FxInstrumentParamState param;
		param.effect_num = 6;
		param.name = "Rev1: T0";
		param.display_name = param.name;
		param.current_scaled = 0.2f;
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		instrument.params.append(param);

		const QString message = build_fx_user_message(states, instrument, "x");
		check(message.contains("separate instances of one parameter"), "opaque instance names are explained once");
		check(message.contains("Prefer the named controls"), "the model is told to prefer the described controls");
	}
}

static void test_prompt_fingerprint(void)
{
	printf("prompt fingerprint (cache diagnosis):\n");

	const QByteArray a = QJsonDocument(QJsonObject{{QStringLiteral("model"), QStringLiteral("deepseek-v4-flash")},
	                                               {QStringLiteral("messages"), QJsonArray{QJsonObject{{QStringLiteral("content"), build_fx_system_prompt()}}}}})
	                      .toJson(QJsonDocument::Compact);
	QByteArray b = a;

	check(!llm_body_fingerprint(a).isEmpty(), "a fingerprint is produced");
	check(llm_body_fingerprint(a).size() == 12, "the fingerprint is short enough to read in the log");
	check(llm_body_fingerprint(a) == llm_body_fingerprint(b), "an unchanged prompt keeps its fingerprint (a cache hit is possible)");

	b.replace("Rules:", "Rules: (edited)");
	check(llm_body_fingerprint(a) != llm_body_fingerprint(b), "an edited prompt changes the fingerprint (explains a cache miss)");
}

// The client-level policy for an empty response: retry a transient one, but
// never retry the identical request that already came back empty (that cost
// the user three ~7 s attempts in ~/.radium/llm.log before it failed).
static inline bool fx_should_retry_empty(bool is_retry, const QString &retried_fingerprint, const QString &fingerprint, int retries_left, bool has_stream_error)
{
	if (has_stream_error)
		return false; // an explicit error is not transient
	if (is_retry && retried_fingerprint == fingerprint)
		return false; // the identical request already failed: retrying repeats it
	return retries_left > 0;
}

static void test_empty_retry_policy(void)
{
	printf("empty-response retry policy:\n");

	const QString fp = "0574bbea5bcf";

	check(fx_should_retry_empty(false, QString(), fp, 2, false), "a first empty response is retried");
	check(!fx_should_retry_empty(true, fp, fp, 2, false), "the identical request that already failed is not retried again");
	check(fx_should_retry_empty(true, "other", fp, 2, false), "a retry whose prompt changed is retried");
	check(!fx_should_retry_empty(false, QString(), fp, 0, false), "no retries left means no retry");
	check(!fx_should_retry_empty(false, QString(), fp, 2, true), "an explicit stream error is not retried");

	// The state the policy reads: an empty 200 and a stream error must be
	// distinguishable from a normal reply.
	{
		LLMStreamAccumulator empty;
		sse_feed(&empty, QByteArray(), true);
		check(empty.content.isEmpty() && empty.finish_reason.isEmpty() && empty.prompt_tokens == 0,
		      "an empty 200 is detected (not a stream error)");

		LLMStreamAccumulator failed;
		sse_feed(&failed, QByteArray("data: {\"error\":{\"message\":\"boom\"}}\n"), true);
		check(!failed.stream_error.isEmpty(), "a stream error is distinguished from an empty 200");
	}
}

static void test_stream_accumulator(void)
{
	printf("stream accumulator (empty response / stream errors):\n");

	// A normal stream still accumulates content and usage.
	{
		LLMStreamAccumulator acc;
		sse_feed(&acc, QByteArray("data: {\"choices\":[{\"delta\":{\"content\":\"{\\\"ask\\\":\\\"\\\"}\"},\"finish_reason\":null}]}\n"), true);
		sse_feed(&acc, QByteArray("data: {\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":100,\"prompt_cache_hit_tokens\":60,\"prompt_cache_miss_tokens\":40,\"completion_tokens\":20}}\n"), true);
		sse_feed(&acc, QByteArray("data: [DONE]\n"), true);
		check(acc.content.contains("ask"), "content is accumulated");
		check(acc.finish_reason == "stop", "finish_reason is kept");
		check(acc.prompt_tokens == 100 && acc.cache_hit_tokens == 60 && acc.completion_tokens == 20, "usage is kept");
		check(acc.stream_error.isEmpty() && acc.unparsed_lines == 0, "a healthy stream reports no error");
	}

	// An error object inside the stream is kept, not swallowed (it used to
	// only set done=true, so the log never showed the message).
	{
		LLMStreamAccumulator acc;
		sse_feed(&acc, QByteArray("data: {\"error\":{\"message\":\"context length exceeded\"}}\n"), true);
		check(acc.stream_error == "context length exceeded", "an in-stream error message is preserved");
		check(acc.done, "an in-stream error ends the stream");
	}

	// A string error form is kept too.
	{
		LLMStreamAccumulator acc;
		sse_feed(&acc, QByteArray("data: {\"error\":\"rate limited\"}\n"), true);
		check(acc.stream_error == "rate limited", "a string error is preserved");
	}

	// The exact failure from the log: HTTP 200 with an empty body.
	{
		LLMStreamAccumulator acc;
		sse_feed(&acc, QByteArray(), true);
		check(acc.content.isEmpty() && acc.reasoning_content.isEmpty(), "nothing accumulated");
		check(acc.finish_reason.isEmpty() && acc.prompt_tokens == 0 && acc.completion_tokens == 0,
		      "an empty 200 has no finish_reason and no usage (the case that was logged as ok)");
	}

	// A non-SSE body (HTML error page, truncated JSON) is counted instead of
	// being silently ignored.
	{
		LLMStreamAccumulator acc;
		sse_feed(&acc, QByteArray("data: <html>gateway timeout</html>\n"), true);
		check(acc.unparsed_lines == 1, "an unparseable stream line is counted");
	}
}

static void test_plugin_keywords(void)
{
	printf("plugin keyword matching:\n");

	const QVector<FxPlugin> plugins{
		FxPlugin{QString("Ladspa"), QString("Auto Wah")},
		FxPlugin{QString("VST"), QString("Auto-Tune"), QString("Antares")},
		FxPlugin{QString("Ladspa"), QString("TAP Tremolo")},
		FxPlugin{QString("Ladspa"), QString("DC Offset Remover")},
	};

	auto names_for = [&plugins](const QString &request)
	{
		QStringList names;
		for (const FxPlugin &plugin : plugins)
			if (fx_plugin_name_matches_keywords(plugin, fx_request_keywords(request)))
				names.append(plugin.name);
		return names.join(",");
	};

	check(names_for("add a wah effect") == "Auto Wah", "a distinctive word matches its plugin");
	check(names_for("autotune it please") == "Auto-Tune", "a keyword matches a hyphenated plugin name");
	check(names_for("Set modulation to 4 hz.").isEmpty(), "units and generic words match nothing");
	check(names_for("make it sound like a boom box").isEmpty(), "a subjective request does not match plugins by accident");
	check(names_for("add a delay").isEmpty(), "a plugin name is not matched by an unrelated word");
	check(!fx_request_keywords("Set modulation to 4 hz.").contains("hz"), "units are not keywords");
	check(!fx_request_keywords("add a delay").contains("add"), "stopwords are not keywords");
	check(fx_request_keywords("add a wah effect").contains("wah"), "a distinctive word is a keyword");
}

static void test_applied_summary(void)
{
	printf("applied summary (history/log deltas):\n");

	const QVector<FxParamState> states = make_states();

	// A plugin context with the same shape as the real one in the log:
	// "N: Name" control names and an opaque display text per control.
	FxInstrumentContext instrument;
	instrument.type_name = "VST3";
	instrument.plugin_name = "Uhbik-A";
	instrument.plugin_version = 1;

	auto add_control = [&instrument](int index, const char *name, float current, const char *display, bool is_clamped)
	{
		FxInstrumentParamState param;
		param.effect_num = index;
		param.name = QString("%1: %2").arg(index).arg(QString::fromUtf8(name));
		param.display_name = param.name;
		param.current_scaled = current;
		param.current_display = QString::fromUtf8(display);
		param.format = FX_INSTRUMENT_PARAM_FLOAT;
		param.is_clamped = is_clamped; // no native range read from the plugin
		instrument.params.append(param);
	};
	add_control(2, "Size", 0.0f, "Size: 0.00", true);
	add_control(7, "Bass", 1.0f, "Bass: 200.00", true);
	add_control(9, "Treble", 0.4f, "40.00", false);

	// The case from the log: an instrument-only reply must produce deltas.
	{
		const FxParseResult parsed = parse_fx_response(
		  R"({"changes":[],"instrument_changes":[{"name":"Size","value":0.35},{"name":"Bass","value":0.85},{"name":"Treble","value":0.5}],"explanation":"x"})",
		  states, QVector<FxPlugin>(), QVector<FxPlugin>(), instrument);

		check(parsed.ok && parsed.instrument_changes.size() == 3, "instrument-only reply parsed");
		const QString summary = fx_applied_summary(parsed, states);
		check(!summary.isEmpty(), "an instrument-only reply has an applied summary");
		check(summary.contains("Uhbik-A: Size 0% -> 35%"), "a clamped control is summarized as a percentage");
		check(summary.contains("Treble 40.00 -> 50.00"), "an unclamped control is summarized in its display units");
		check(!summary.contains("more"), "the summary is complete");
	}

	// System effects, modulations and plugins are all represented.
	{
		const QVector<FxPlugin> inserted{FxPlugin{QString("Ladspa"), QString("Auto Wah")}};
		const FxParseResult parsed = parse_fx_response(
		  R"({"changes":[{"name":"System Eq1 F.","value":300},{"name":"System Eq1 L.","value":3}],"modulators":[{"effect":"System Hp F."}],"remove_plugins":[{"name":"Auto Wah"}]})",
		  states, QVector<FxPlugin>(), inserted, instrument);
		const QString summary = fx_applied_summary(parsed, states);
		check(summary.contains("System effects:"), "system effects are summarized");
		check(summary.contains("System Eq1 F.") && summary.contains("-> 300 Hz"), "the system effect old -> new is kept");
		check(summary.contains("Added a modulator to System Hp F."), "modulations are summarized");
		check(summary.contains("Removed Auto Wah"), "plugin removals are summarized");
	}

	// A reply with nothing applied has an empty summary (the history then says
	// explicitly that nothing was applied).
	{
		const FxParseResult parsed = parse_fx_response(R"({"ask":"Which one?","explanation":""})", states);
		check(parsed.ok && fx_applied_summary(parsed, states).isEmpty(), "a question has no applied summary");
	}

	// The display value interpolation itself.
	check(fx_interpolate_display("200.00", 0.4f, 0.5f) == "250.00", "display value scaled");
	check(fx_interpolate_display("Treble: 40.00", 0.4f, 0.5f) == "Treble: 50.00", "display value scaled after a port-name prefix");
	check(fx_interpolate_display("Mix: 40", 0.4f, 0.55f) == "Mix: 55", "display value with a prefix");
	check(fx_interpolate_display("0.00", 0.0f, 0.5f).isEmpty(), "cannot scale from an endpoint");
	check(fx_interpolate_display("On", 0.5f, 1.0f).isEmpty(), "a non-numeric display is not interpolated");

	{
		FxInstrumentParamState param;
		param.current_display = "pitch_glide_length: 420";
		check(fx_parameter_display(&param) == "420", "the port-name prefix is stripped from the display");
		param.current_display = "35.00";
		check(fx_parameter_display(&param) == "35.00", "a plain display is kept");
		param.current_display = "";
		check(fx_parameter_display(&param).isEmpty(), "a missing display is empty");
	}
}

static void test_ask(void)
{
	printf("clarifying question:\n");

	const QVector<FxParamState> states = make_states();

	{
		const FxParseResult result = parse_fx_response(
		  R"({"ask":"Do you mean the instrument's own room size, or the system reverb send?","explanation":""})", states);
		check(result.ok, "an ask-only reply is valid");
		check(result.ask == "Do you mean the instrument's own room size, or the system reverb send?", "the question is parsed");
		check(result.changes.isEmpty() && result.instrument_changes.isEmpty(), "no changes are proposed");
	}

	{
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[{"name":"System Eq1 L.","value":3}],"ask":"More?", "explanation":"x"})", states);
		check(result.ok && result.ask == "More?" && result.changes.size() == 1, "a question can accompany changes");
	}

	{
		const FxParseResult result = parse_fx_response(R"({"ask":""})", states);
		check(result.ok && result.ask.isEmpty(), "an empty question is parsed");
	}

	check(!parse_fx_response(R"({"explanation":"no action at all"})", states).ok, "explanation alone is still not an action");
}
static void test_modulations(void)
{
	printf("modulations:\n");

	{
		const QVector<FxParamState> states = make_states();
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[],"modulators":[{"effect":"System Hp F."}],"explanation":"x"})", states);

		check(result.ok, "ok");
		check(result.modulations.size() == 1, "one modulation");
		const FxModulation &modulation = result.modulations[0];
		check(modulation.action == FxModulation::ADD, "default action is add");
		check(modulation.effect == "System Hp F.", "effect name normalized");
		check(modulation.type.isEmpty(), "type not provided stays empty (apply uses sine)");
		check(modulation.tempo_numerator == -1 && modulation.tempo_denominator == -1, "rate not provided stays -1");
		check(modulation.min_scaled < 0.0f && modulation.max_scaled < 0.0f, "bounds not provided stay -1");
	}

	{
		const QVector<FxParamState> states = make_states();
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[],"modulators":[{"effect":"System Hp F.","type":"triangle","tempo_numerator":1,"tempo_denominator":8,"min":0.3,"max":0.7}]})", states);

		check(result.ok && result.modulations.size() == 1, "explicit modulation parsed");
		const FxModulation &modulation = result.modulations[0];
		check(modulation.type == "triangle", "type parsed");
		check(modulation.tempo_numerator == 1 && modulation.tempo_denominator == 8, "rate parsed");
		check(std::fabs(modulation.min_scaled - 0.3f) < 0.001 && std::fabs(modulation.max_scaled - 0.7f) < 0.001, "bounds parsed");
	}

	{
		const QVector<FxParamState> states = make_states_with_modulator("System Hp F.");

		const FxParseResult add = parse_fx_response(
		  R"({"changes":[],"modulators":[{"effect":"System Hp F."}]})", states);
		check(add.ok && add.modulations.isEmpty() && add.warnings.size() == 1, "adding to a modulated effect is ignored with a warning");

		const FxParseResult replace = parse_fx_response(
		  R"({"changes":[],"modulators":[{"action":"replace","effect":"System Hp F.","tempo_denominator":4}]})", states);
		check(replace.ok && replace.modulations.size() == 1 && replace.modulations[0].action == FxModulation::REPLACE, "replace parsed");
		check(replace.modulations.size() == 1 && replace.modulations[0].tempo_denominator == 4, "replace field parsed");

		const FxParseResult remove = parse_fx_response(
		  R"({"changes":[],"modulators":[{"action":"remove","effect":"System Hp F."}]})", states);
		check(remove.ok && remove.modulations.size() == 1 && remove.modulations[0].action == FxModulation::REMOVE, "remove parsed");
	}

	{
		const QVector<FxParamState> states = make_states();

		const FxParseResult replace = parse_fx_response(
		  R"({"changes":[],"modulators":[{"action":"replace","effect":"System Hp F."}]})", states);
		check(replace.ok && replace.modulations.size() == 1 && replace.modulations[0].action == FxModulation::ADD, "replace without modulator becomes add");

		const FxParseResult remove = parse_fx_response(
		  R"({"changes":[],"modulators":[{"action":"remove","effect":"System Hp F."}]})", states);
		check(remove.ok && remove.modulations.isEmpty() && remove.warnings.size() == 1, "remove without modulator is ignored with a warning");
	}

	{
		const QVector<FxParamState> states = make_states();
		const FxParseResult result = parse_fx_response(
		  R"({"changes":[],"modulators":[{"effect":"No Such Effect"},{"effect":"System Eq1 On/Off"},{"effect":"System Eq1 L.","type":"bogus","tempo_numerator":99}]})", states);

		check(result.ok, "ok");
		check(result.modulations.size() == 1, "only the valid modulation survives");
		check(result.warnings.size() == 3, "three warnings (unknown, on/off, bad type)");
		check(result.modulations.size() == 1 && result.modulations[0].type.isEmpty(), "bad type falls back to the default");
		check(result.modulations.size() == 1 && result.modulations[0].tempo_numerator == 32, "rate is clamped");
	}

	check(fx_modulator_type_index("sine") == 0, "type index sine");
	check(fx_modulator_type_index("Inverted Saw") == 4, "type index inverted saw");
	check(fx_modulator_type_index("bogus") == -1, "unknown type index");

	{
		FxModulation add;
		add.action = FxModulation::ADD;
		add.effect = "System Hp F.";
		add.type = "sine";
		add.tempo_numerator = 1;
		add.tempo_denominator = 8;
		add.min_scaled = 0.3f;
		add.max_scaled = 0.7f;

		const QString summary = fx_summarize_modulations(QVector<FxModulation>{add});
		check(summary.contains("Added a modulator to System Hp F."), "add summary");
		check(summary.contains("1/8 speed"), "rate summary");
		check(summary.contains("30-70%"), "bounds summary");

		FxModulation remove;
		remove.action = FxModulation::REMOVE;
		remove.effect = "System Hp F.";
		check(fx_summarize_modulations(QVector<FxModulation>{remove}).contains("Removed the modulator from System Hp F."), "remove summary");
	}
}

static void test_plugins(void)
{
	printf("plugins:\n");

	const QVector<FxParamState> states = make_states();
	const QVector<FxPlugin> plugins{
		FxPlugin{QString("Ladspa"), QString("Auto Wah")},
		FxPlugin{QString("Ladspa"), QString("TAP Tremolo")},
		FxPlugin{QString("VST"), QString("Auto Wah")}, // Same name, different type.
	};

	const FxParseResult result = parse_fx_response(
	  R"({"changes":[],"add_plugins":[{"name":"Auto Wah","type":"Ladspa"},{"name":"TAP Tremolo"},{"name":"No Such Plugin","type":"Ladspa"}],"explanation":"Added a wah."})",
	  states, plugins);

	check(result.ok, "ok");
	check(result.plugins.size() == 2, "two valid plugins, unknown dropped");
	check(result.warnings.size() == 1, "one warning for the unknown plugin");
	check(result.plugins.size() == 2 && result.plugins[0].type_name == "Ladspa" && result.plugins[0].name == "Auto Wah", "exact name/type match");
	check(result.plugins.size() == 2 && result.plugins[1].name == "TAP Tremolo", "type can be omitted when the name is unique");

	{
		// Without a type, an ambiguous name is rejected.
		const FxParseResult ambiguous = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"Auto Wah"}]})", states, plugins);
		check(ambiguous.ok && ambiguous.plugins.isEmpty() && ambiguous.warnings.size() == 1, "ambiguous name is rejected without a type");
	}

	{
		// Duplicate entries are collapsed.
		const FxParseResult duplicate = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"Auto Wah","type":"Ladspa"},{"name":"auto wah","type":"ladspa"}]})", states, plugins);
		check(duplicate.ok && duplicate.plugins.size() == 1, "duplicate plugin entries are collapsed");
	}

	{
		// Spacing/hyphen differences are forgiven.
		const FxParseResult normalized = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"Auto-Wah","type":"LADSPA"}]})", states, plugins);
		check(normalized.ok && normalized.plugins.size() == 1 && normalized.plugins[0].name == "Auto Wah", "plugin names are matched ignoring case and punctuation");
	}

	{
		// A plugin-only reply is valid.
		const FxParseResult only_plugins = parse_fx_response(
		  R"({"add_plugins":[{"name":"Auto Wah","type":"Ladspa"}]})", states, plugins);
		check(only_plugins.ok && only_plugins.plugins.size() == 1, "a reply with only add_plugins is valid");
	}

	{
		// Without a catalog, no plugin can be added.
		const FxParseResult no_catalog = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"Auto Wah","type":"Ladspa"}]})", states);
		check(no_catalog.ok && no_catalog.plugins.isEmpty() && no_catalog.warnings.size() == 1, "unknown plugin without a catalog is ignored");
	}

	const QString summary = fx_summarize_plugins(result.plugins);
	check(summary.contains("Inserted Auto Wah (Ladspa)"), "plugin summary");
	check(fx_summarize_plugins(QVector<FxPlugin>()).isEmpty(), "empty plugin summary");

	{
		// If no plugin is available, the LLM can ask the host to offer
		// creating a Faust Dev 2 effect.
		const FxParseResult result = parse_fx_response(
		  R"({"create_faust":"a smooth autotune effect with medium retune speed","create_faust_name":"Autotune","explanation":"No autotune plugin is available. Should I create one with Faust Dev 2?"})",
		  states);
		check(result.ok, "create_faust ok");
		check(result.create_faust == "a smooth autotune effect with medium retune speed", "create_faust description parsed");
		check(result.create_faust_name == "Autotune", "create_faust instrument name parsed");
		check(result.changes.isEmpty() && result.plugins.isEmpty() && result.removed_plugins.isEmpty(), "create_faust needs no other actions");

		const FxParseResult empty = parse_fx_response(R"({"create_faust":""})", states);
		check(empty.ok && empty.create_faust.isEmpty(), "empty create_faust parsed");

		const FxParseResult invalid = parse_fx_response(R"({"create_faust":5})", states);
		check(!invalid.ok, "non-string create_faust rejected");

		const FxParseResult invalid_name = parse_fx_response(R"({"create_faust":"an autotune effect","create_faust_name":5})", states);
		check(!invalid_name.ok, "non-string create_faust_name rejected");
	}

	check(fx_make_faust_instrument_name("an autotune effect") == "Autotune", "instrument name derived from a simple description");
	check(fx_make_faust_instrument_name("a smooth autotune effect with medium retune speed") == "Smooth autotune", "instrument name derived from a detailed description");
	check(fx_make_faust_instrument_name("") == "Faust Effect", "empty description gets a fallback instrument name");

	{
		// Removal is validated against the plugins currently inserted after
		// the instrument, not the full available catalog.
		const QVector<FxPlugin> inserted{
			FxPlugin{QString("Ladspa"), QString("Auto Wah")},
		};

		const FxParseResult remove = parse_fx_response(
		  R"({"changes":[],"remove_plugins":[{"name":"auto-wah","type":"LADSPA"},{"name":"TAP Tremolo"}],"explanation":"Removed the wah."})",
		  states, plugins, inserted);

		check(remove.ok, "remove ok");
		check(remove.removed_plugins.size() == 1, "only a currently inserted plugin is removed");
		check(remove.warnings.size() == 1, "warning for the plugin that is not inserted");
		check(remove.removed_plugins.size() == 1 && remove.removed_plugins[0].name == "Auto Wah", "removed plugin normalized");

		const FxParseResult only_remove = parse_fx_response(
		  R"({"remove_plugins":[{"name":"Auto Wah","type":"Ladspa"}]})", states, plugins, inserted);
		check(only_remove.ok && only_remove.removed_plugins.size() == 1, "a reply with only remove_plugins is valid");

		const FxParseResult remove_unknown = parse_fx_response(
		  R"({"changes":[],"remove_plugins":[{"name":"No Such Plugin","type":"Ladspa"}]})", states, plugins, inserted);
		check(remove_unknown.ok && remove_unknown.removed_plugins.isEmpty() && remove_unknown.warnings.size() == 1, "unknown removal is ignored");

		const QString remove_summary = fx_summarize_removed_plugins(remove.removed_plugins);
		check(remove_summary.contains("Removed Auto Wah (Ladspa)"), "removed plugin summary");
		check(fx_summarize_removed_plugins(QVector<FxPlugin>()).isEmpty(), "empty removed plugin summary");
	}

	{
		// Container plugins with the same type/name in different containers
		// need the container in the reply.
		const QVector<FxPlugin> container_plugins{
			FxPlugin{QString("VST"), QString("Auto-Tune"), QString("Antares")},
			FxPlugin{QString("VST"), QString("Auto-Tune"), QString("Other Vendor")},
		};

		const FxParseResult ambiguous = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"Auto-Tune","type":"VST"}]})", states, container_plugins);
		check(ambiguous.ok && ambiguous.plugins.isEmpty() && ambiguous.warnings.size() == 1, "same name/type in several containers requires the container");

		const FxParseResult exact = parse_fx_response(
		  R"({"changes":[],"add_plugins":[{"name":"auto-tune","type":"vst","container":"antares"}]})", states, container_plugins);
		check(exact.ok && exact.plugins.size() == 1, "container plugin parsed");
		check(exact.plugins.size() == 1 && exact.plugins[0].container_name == "Antares", "container name normalized from the catalog");

		const QString summary_with_container = fx_summarize_plugins(exact.plugins);
		check(summary_with_container.contains("Auto-Tune (VST, Antares)"), "summary includes the container");
	}
}

static void test_parse_errors(void)
{
	printf("parse errors:\n");

	const QVector<FxParamState> states = make_states();

	check(!parse_fx_response("no json here", states).ok, "no JSON object");
	check(!parse_fx_response("{\"changes\":[}", states).ok, "broken JSON");
	check(!parse_fx_response("{\"explanation\":\"no changes key\"}", states).ok, "missing changes array");
	check(!parse_fx_response("{\"changes\":\"nope\"}", states).ok, "changes is not an array");
	check(!parse_fx_response("{\"changes\":[],\"add_plugins\":\"nope\"}", states, QVector<FxPlugin>()).ok, "add_plugins is not an array");
	check(!parse_fx_response("{\"changes\":[],\"remove_plugins\":\"nope\"}", states, QVector<FxPlugin>(), QVector<FxPlugin>()).ok, "remove_plugins is not an array");
}

static void test_band_enable(void)
{
	printf("band auto-enable:\n");

	QVector<FxParamState> states = make_states();

	{
		const QVector<FxChange> changes{FxChange{QString("System Eq1 L."), 5.0f}};
		const QVector<FxChange> extra = fx_band_enable_changes(states, changes);
		check(extra.size() == 1, "one band is enabled");
		check(extra.size() == 1 && extra[0].name == "System Eq1 On/Off" && extra[0].value == 1.0f, "the right band is enabled");
	}

	{
		const QVector<FxChange> changes{FxChange{QString("System Eq1 L."), 5.0f}, FxChange{QString("System Eq1 On/Off"), 1.0f}};
		const QVector<FxChange> extra = fx_band_enable_changes(states, changes);
		check(extra.isEmpty(), "no extra change when the model already enabled the band");
	}

	{
		// Already on: no need to add anything.
		for (FxParamState &state : states)
			if (state.param.name == "System Hs On/Off")
				state.current_native = 1.0f;
		const QVector<FxChange> changes{FxChange{QString("System Hs L."), -4.0f}};
		check(fx_band_enable_changes(states, changes).isEmpty(), "no extra change when the band is already on");
	}

	{
		const QVector<FxChange> changes{FxChange{QString("System Volume Voice 2"), 8.0f}};
		const QVector<FxChange> extra = fx_band_enable_changes(states, changes);
		check(extra.size() == 1 && extra[0].name == "System On/Off Voice 2", "changing a voice enables that voice");
	}

	{
		const QVector<FxChange> changes{FxChange{QString("System Pan"), 0.8f}};
		const QVector<FxChange> extra = fx_band_enable_changes(states, changes);
		check(extra.size() == 1 && extra[0].name == "System Pan On/Off", "changing pan enables pan");
	}

	{
		const QVector<FxChange> changes{FxChange{QString("System Width"), 5.0f}};
		const QVector<FxChange> extra = fx_band_enable_changes(states, changes);
		check(extra.size() == 1 && extra[0].name == "System Width On/Off", "changing width enables width");
	}

	{
		const QVector<FxChange> changes{FxChange{QString("System Reverb"), -8.0f}};
		check(fx_band_enable_changes(states, changes).isEmpty(), "bus sends need no enable");
	}

	{
		// "System Pitch" needs "System Pitch type" set to 3 (All channels).
		QVector<FxParamState> pitch_off_states = make_states();
		for (FxParamState &state : pitch_off_states)
			if (state.param.name == "System Pitch type")
				state.current_native = 0.0f; // Disabled

		const QVector<FxChange> changes{FxChange{QString("System Pitch"), 0.5f}};
		const QVector<FxChange> extra = fx_band_enable_changes(pitch_off_states, changes);
		check(extra.size() == 1, "pitch enable is added");
		check(extra.size() == 1 && extra[0].name == "System Pitch type" && std::fabs(extra[0].value - 3.0f) < 0.001, "pitch type is set to 3 (All channels)");

		// Modulation targets use the same helper: a pseudo change with just
		// the effect name enables the gating effect.
		const QVector<FxChange> modulation_target{FxChange{QString("System Pitch"), 0.0f}};
		check(fx_band_enable_changes(pitch_off_states, modulation_target).size() == 1, "modulating pitch enables the pitch shifter");

		for (FxParamState &state : pitch_off_states)
			if (state.param.name == "System Pitch type")
				state.current_native = 3.0f;
		check(fx_band_enable_changes(pitch_off_states, changes).isEmpty(), "no enable when the pitch shifter is already on");
	}
}

static void test_summarize(void)
{
	printf("summarize:\n");

	const QVector<FxParamState> states = make_states();
	const QVector<FxChange> changes{
	  FxChange{QString("System Eq1 F."), 2500.0f},
	  FxChange{QString("System Eq1 L."), 4.5f},
	};

	const QString summary = fx_summarize_changes(states, changes);
	check(summary.contains("System Eq1 F.") && summary.contains("->"), "names and arrows");
	check(summary.contains("2500 Hz"), "formatted Hz value");
	check(summary.contains("+4.50 dB"), "formatted dB value");

	const QVector<FxChange> bypass_changes{FxChange{QString("System Effects On/Off"), 1.0f}};
	check(fx_summarize_changes(states, bypass_changes).contains("off -> on"), "global bypass is summarized as off -> on");

	const QVector<FxChange> bus_changes{FxChange{QString("System Chorus On/Off"), 1.0f}};
	check(fx_summarize_changes(states, bus_changes).contains("System Chorus On/Off off -> on"), "bus connection is summarized as off -> on");

	{
		QVector<FxParamState> mute_states = make_states();
		for (FxParamState &state : mute_states)
			if (state.param.name == "System Volume On/Off")
				state.current_native = 1.0f;
		const QVector<FxChange> mute_changes{FxChange{QString("System Volume On/Off"), 0.0f}};
		check(fx_summarize_changes(mute_states, mute_changes).contains("System Volume On/Off on -> off"), "mute is summarized as on -> off");
	}

	{
		FxInstrumentChange change;
		change.plugin_name = "Organ";
		change.name = "Harmonic 2";
		change.display_name = "2nd harmonic";
		change.old_value_scaled = 0.4f;
		change.value_scaled = 0.75f;
		const QString instrument_summary = fx_summarize_instrument_changes(QVector<FxInstrumentChange>{change});
		check(instrument_summary.contains("Organ: 2nd harmonic 40% -> 75%"), "instrument control change summary");

		change.name = "pitch_glide_length";
		change.display_name = "pitch_glide_length";
		change.value_is_native = true;
		change.old_value_native = 420.0f;
		change.value_native = 922.0f;
		change.unit = "ms";
		check(fx_summarize_instrument_changes(QVector<FxInstrumentChange>{change}).contains("Organ: pitch_glide_length 420 ms -> 922 ms"), "native control change summary includes units");
	}
}

int main(int argc, char **argv)
{
	printf("llm_fx_tune_test\n");

	test_catalog();
	test_parse_valid();
	test_parse_instrument_changes();
	test_parse_fenced_and_prose();
	test_parse_case_insensitive();
	test_parse_warnings_and_clamping();
	test_parse_bools_and_duplicates();
	test_parse_voice();
	test_format_values();
	test_prompts();
	test_prefixed_control_names();
	test_voice_condensing();
	test_corrections();
	test_applied_summary();
	test_correction_round_trip();
	test_plugin_keywords();
	test_prompt_layout();
	test_answer_field();
	test_question_detection();
	test_unit_reporting();
	test_instrument_summary();
	test_audio_analysis();
	test_audio_analysis_tier1();
	test_audio_decay();
	test_measurement_prompt();
	test_measurement_rule();
	test_measurement_history();
	test_song_timing();
	test_compact_controls();
	test_plugin_catalogue_economy();
	test_description_budget();
	test_prompt_fingerprint();
	test_stream_accumulator();
	test_empty_retry_policy();
	test_ask();
	test_modulations();
	test_plugins();
	test_parse_errors();
	test_band_enable();
	test_summarize();

	if (g_failures == 0)
	{
		printf("All tests passed.\n");
		return 0;
	}

	printf("%d test(s) failed.\n", g_failures);
	return 1;
}
