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


#include <math.h>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#if defined(__clang__)
#  pragma GCC diagnostic ignored "-Wcast-function-type-mismatch"
#else
#  pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"

#include "../bin/packages/faust/architecture/faust/dsp/dsp.h"
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
#include "../bin/packages/faust/architecture/faust/dsp/llvm-dsp.h"
#endif
#include "../bin/packages/faust/architecture/faust/dsp/interpreter-dsp.h"
#include "Faust_dev2_poly.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust/compiler/generator/libfaust.h"
#undef uchar
#undef uint

#include "../bin/packages/faust/architecture/faust/gui/UI.h"
#include "../bin/packages/faust/architecture/faust/gui/APIUI.h"
#include "../bin/packages/faust/architecture/faust/gui/Soundfile.h"
#include "../bin/packages/faust/architecture/faust/gui/LibsndfileReader.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#pragma GCC diagnostic pop

#include <QThread>
#include <QString>
#include <QStringList>
#include <QCoreApplication>
#include <QDialog>
#include <QPointer>
#include <QGridLayout>
#include <QFile>
#include <QSet>
#include <QRegularExpression>
#include <QtConcurrent>
#include <QTimer>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <faust/gui/QTUI.h>
#pragma GCC diagnostic pop

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS // for the radium_sf_open declarations in nsmtracker.h
#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/patch_proc.h"
#include "../common/disk.h"
#include "../common/OS_rosetta.h"

#include <sndfile.h>

#include "../common/ArgsCreator.hpp"

#include "../api/api_proc.h"
#include "../api/api_gui_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Juce_plugins_proc.h"
#include "Mixer_proc.h"

#include "Faust_plugins_proc.h"
#include "Fade.hpp"

#include "../Qt/MyQTemporaryDir.hpp"
#include "../Qt/helpers.h"

#include "SubBlockNoteCollector.h"


#define MAX_CHANNELS 16
#define MAX_EFFECTS 1024
#define MAX_POLYPHONY 128

#define MIN_LINEAR_VELOCITY 0.1f
static constexpr float g_min_linear_gain_fd2 = 0.001995f; // = powf(10, R_SCALE(MIN_LINEAR_VELOCITY, 0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;

// input is between 0 and 1. output is between 0 and 1.
static inline float velocity2gain(float val)
{
	if (val <= 0.0f)
		return 0.0f;
	else if (val >= 1.0f)
		return 1.0f;
	else if (val < MIN_LINEAR_VELOCITY)
		return scale(val, 0, MIN_LINEAR_VELOCITY, 0, g_min_linear_gain_fd2);
	else
		return powf(10, scale(val, 0.0f, 1.0f, -40, 20) / 20.0f) / 10.0f;
}

static const char *g_default_faust_dev2_program =
	"import(\"stdfaust.lib\");\n"
	"\n"
	"freq = hslider(\"freq\", 440, 20, 20000, 0.01);\n"
	"gain = hslider(\"gain\", 0.25, 0, 1, 0.01);\n"
	"volume = hslider(\"volume\", 0, -60, 12, 0.1) : ba.db2linear : si.smooth(ba.tau2pole(0.010));\n"
	"gate = button(\"gate\");\n"
	"\n"
	"attack = hslider(\"attack\", 0.01, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"decay = hslider(\"decay\", 0.2, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"sustain = hslider(\"sustain\", 0.7, 0, 1, 0.01) : si.smooth(ba.tau2pole(0.010));\n"
	"release = hslider(\"release\", 0.4, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"\n"
	"envelope = en.adsr(attack, decay, sustain, release, gate);\n"
	"\n"
	"process = os.osc(freq) * gain * envelope * volume <: _,_;\n";

// The code a newly created Faust Dev 2 instrument starts with.
QString FAUST2_get_default_code(void)
{
	return QString(g_default_faust_dev2_program);
}


namespace{

struct NoteVoice
{
	int64_t note_id;
	const struct SeqBlock *seqblock;
	float pitch;
	FaustDev2PolyVoice *voice;
};


//===========================================
// Soundfile support
//===========================================

// All audio data used by one compiled program. Created on the compile thread
// (file decoding is slow), then assigned to the dsp zones in create_dsp_data
// and owned by FaustDev2Dsp from then on.
struct FaustDev2SoundfileData
{
	std::map<std::string, Soundfile*> url2soundfile; // url string (as passed to addSoundfile) -> Soundfile
	std::vector<Soundfile*> owned;                   // all created Soundfiles

	~FaustDev2SoundfileData()
	{
		for (Soundfile *sf : owned)
			delete sf;
	}
};

// Collects the list of soundfile urls used by a dsp. Derives from GUI (which
// provides no-op defaults for all widget methods) and only handles
// addSoundfile.
class Faust2SoundfileCollectUI : public GUI
{
public:
	std::vector<std::string> url_list;

	void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) override
	{
		if (filename == NULL || *filename == 0)
			return;
		if (std::find(url_list.begin(), url_list.end(), filename) == url_list.end())
			url_list.push_back(filename);
	}
};

// Assigns already-loaded Soundfile pointers to the zones of a dsp instance.
// Run once per dsp instance (the mono dsp and each polyphonic voice); every
// voice then reads from the same shared Soundfile data.
class Faust2SoundfileAssignUI : public GUI
{
public:
	const std::map<std::string, Soundfile*> &url2soundfile;

	Faust2SoundfileAssignUI(const std::map<std::string, Soundfile*> &map)
		: url2soundfile(map)
	{
	}

	void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) override
	{
		if (filename == NULL || sf_zone == NULL)
			return;
		auto it = url2soundfile.find(filename);
		if (it != url2soundfile.end())
			*sf_zone = it->second;
	}
};

struct FaustDev2Dsp
{
	nonstealing_microtonal_poly_dsp *poly_dsp;  // owns the voice DSPs; NULL for effects
	dsp *final_dsp;           // points to poly_dsp for instruments, or mono_dsp for effects
	APIUI api_ui;
	NoteEventCollector collector;
	NoteVoice note_voices[MAX_POLYPHONY]; // RT-safe: only touched from the player thread / RT_process.
	int num_note_voices;
	dsp_factory *factory;     // base factory pointer
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	llvm_dsp_factory *llvm_factory;       // NULL if interpreter was used
#endif
	interpreter_dsp_factory *interp_factory; // NULL if LLVM was used
	FaustDev2SoundfileData *soundfile_data; // owned; soundfiles loaded on the compile thread
	bool is_instrument;
	int num_params;
	float *param_values;      // current values, indexed by APIUI id
	int num_inputs;
	int num_outputs;

	FaustDev2Dsp()
		: poly_dsp(NULL)
		, final_dsp(NULL)
		, num_note_voices(0)
		, factory(NULL)
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		, llvm_factory(NULL)
#endif
		, interp_factory(NULL)
		, soundfile_data(NULL)
		, is_instrument(false)
		, num_params(0)
		, param_values(NULL)
		, num_inputs(0)
		, num_outputs(0)
	{
	}

	~FaustDev2Dsp()
	{
		// final_dsp owns poly_dsp and the mono DSP, so just delete final_dsp
		delete final_dsp;
		V_free(param_values);
		delete soundfile_data;

		// Delete factory
		if (interp_factory != NULL)
			deleteInterpreterDSPFactory(interp_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm_factory != NULL)
			deleteDSPFactory(llvm_factory);
#endif
	}
};


// Identifies one control in the QTGUI dialog, so a change made there can be
// routed back through set_effect_value() (keeping param_values/stored values in
// sync, and making the change survive a recompile).
struct Faust2GuiControlRef
{
	instrument_t patch_id;
	int effect_num;
};


struct FaustDev2Data
{
	QString code;
	QString options;

#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
	bool use_interpreter_backend = true;
#else
	bool use_interpreter_backend = OS_running_under_rosetta();
#endif

	FaustDev2Dsp *dsp_data;   // NULL until compiled
	bool is_compiling;
	QString error_message;

	// When true (default), effect values are reset to the program's default
	// values after each successful compilation. When false, existing effects
	// keep their current values. New effects always get their default values.
	// Set from the GUI (the "R_FX" checkbox), read on the main thread.
	bool reset_effect_values_on_compile = true;

	// Fade-out state. When a compile finishes, the current dsp fades out over
	// about FADE_LENGTH_MS (see perform_compile_completion), and the new dsp
	// is only swapped in after the fade is finished, so the old dsp is never
	// cut off abruptly. fade_out_is_active and the fade counters are written
	// by the main thread (under the player lock) and read by RT_process.
	bool fade_out_is_active;
	int fade_frames_total;
	int fade_frames_left;

	QPointer<QDialog> qtgui_parent;
	QTGUI *qtgui;
	std::vector<Faust2GuiControlRef*> qtgui_control_refs; // owned; one per visible control
	radium::FAUST_calledRegularlyByParentReply ready;
	MyQTemporaryDir *svg_dir; // owns SVG output directory

	FaustDev2Data()
		: options("-I\n%radium_path%/packages/faust/libraries")
		, dsp_data(NULL)
		, is_compiling(false)
		, fade_out_is_active(false)
		, fade_frames_total(0)
		, fade_frames_left(0)
		, qtgui(NULL)
		, svg_dir(NULL)
	{
	}

	~FaustDev2Data()
	{
		for (Faust2GuiControlRef *ref : qtgui_control_refs)
			delete ref;
		qtgui_control_refs.clear();
		delete qtgui;
		delete svg_dir;
		delete dsp_data;
	}
};


// Called by the QTGUI dialog whenever a control zone is reflected and its value
// differs from what Radium thinks the value is. Routes the change through
// PLUGIN_set_effect_value -> set_effect_value so it updates param_values, the
// stored effect values, and the polyphonic voices. (Same pattern as Faust Dev 1's
// faust_gui_zone_callback.)
static void faust2_gui_zone_callback(float val, void *data)
{
	Faust2GuiControlRef *ref = (Faust2GuiControlRef*)data;
	if (ref == NULL)
		return;

	struct Patch *patch = PATCH_get_from_id(ref->patch_id);
	if (patch == NULL || patch->patchdata == NULL)
		return;

	SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	if (devdata == NULL)
		return;

	FaustDev2Dsp *dsp_data = devdata->dsp_data;
	if (dsp_data == NULL || ref->effect_num >= dsp_data->num_params)
		return;

	if (equal_floats(val, dsp_data->param_values[ref->effect_num]))
		return; // round-trip guard; the change already came from Radium.

	PLUGIN_set_effect_value(plugin, -1, ref->effect_num, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
}


// The player lock must be held when calling this function.
static FaustDev2Dsp *create_dsp_data(dsp_factory *factory,
									 dsp_factory *base_factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
									 llvm_dsp_factory *llvm_factory,
#endif
									 interpreter_dsp_factory *interp_factory,
									 FaustDev2SoundfileData *soundfile_data,
									 float sample_rate)
{

	dsp *mono_dsp = factory->createDSPInstance();
	if (mono_dsp == NULL){
		RWarning("createDSPInstance returned NULL in FaustDev2");
		delete soundfile_data;
		return NULL;
	}

	// Assign the pre-loaded soundfiles to the mono dsp's zones before it is
	// cloned into voices and before init: the interpreter backend needs the
	// soundfile zones to be valid when init runs.
	if (soundfile_data != NULL && !soundfile_data->url2soundfile.empty())
	{
		Faust2SoundfileAssignUI assign_ui(soundfile_data->url2soundfile);
		mono_dsp->buildUserInterface(&assign_ui);
	}

	dsp *final_dsp;
	nonstealing_microtonal_poly_dsp *poly_dsp = NULL;
	bool is_instrument;

	// Polyphony is always 128 voices: any 'declare options "[nvoices:N]"'
	// in the Faust code is ignored. An instrument is detected by the
	// presence of the freq/key, gate, and gain/velocity controls.
	if (MidiMeta::checkPolyphony(mono_dsp)){
		poly_dsp = new nonstealing_microtonal_poly_dsp(mono_dsp, MAX_POLYPHONY);
		final_dsp = poly_dsp;
		is_instrument = true;
	}else{
		poly_dsp = NULL;
		final_dsp = mono_dsp;
		is_instrument = false;
	}

	// Every voice has its own soundfile zones (the interpreter backend keeps
	// them in a per-voice table), so assign the shared Soundfiles to each
	// voice too.
	if (poly_dsp != NULL && soundfile_data != NULL && !soundfile_data->url2soundfile.empty())
	{
		Faust2SoundfileAssignUI assign_ui(soundfile_data->url2soundfile);
		for (FaustDev2PolyVoice *voice : poly_dsp->fVoiceTable)
			voice->buildUserInterface(&assign_ui);
	}

	FaustDev2Dsp *dsp_data = new FaustDev2Dsp;
	dsp_data->poly_dsp = poly_dsp;
	dsp_data->final_dsp = final_dsp;
	dsp_data->is_instrument = is_instrument;
	dsp_data->factory = base_factory;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	dsp_data->llvm_factory = llvm_factory;
#endif
	dsp_data->interp_factory = interp_factory;
	dsp_data->soundfile_data = soundfile_data;

	final_dsp->init(sample_rate);
	final_dsp->buildUserInterface(&dsp_data->api_ui);
	dsp_data->num_params = dsp_data->api_ui.getParamsCount();
	dsp_data->num_inputs = final_dsp->getNumInputs();
	dsp_data->num_outputs = final_dsp->getNumOutputs();

	dsp_data->param_values = (float*)V_malloc(sizeof(float) * dsp_data->num_params);
	for (int i = 0; i < dsp_data->num_params; i++)
		dsp_data->param_values[i] = dsp_data->api_ui.getParamValue(i);

	return dsp_data;
}


// The player lock must be held when calling this function.
static void delete_dsp_data(FaustDev2Dsp *dsp_data)
{
	if (dsp_data == NULL)
		return;

	// Don't let ~FaustDev2Dsp delete the factory if we're transferring ownership
	dsp_data->interp_factory = NULL;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	dsp_data->llvm_factory = NULL;
#endif

	delete dsp_data;
}


static void hotswap_dsp_data(FaustDev2Data *devdata, FaustDev2Dsp *new_dsp, bool reset_effect_values)
{
	FaustDev2Dsp *old_dsp = devdata->dsp_data;

	// Preserve parameter values from old DSP to new DSP by matching names.
	// Read from the live control zones (not the cached param_values), so values
	// changed directly in the QTGUI dialog are also carried over.
	// Skipped when the values should be reset to the program defaults.
	if (old_dsp != NULL && new_dsp != NULL && !reset_effect_values){
		int n_new = new_dsp->api_ui.getParamsCount();
		for (int i = 0; i < n_new; i++){
			const char *addr = new_dsp->api_ui.getParamAddress(i);
			int old_idx = old_dsp->api_ui.getParamIndex(addr);
			if (old_idx >= 0){
				float val = old_dsp->api_ui.getParamValue(old_idx);
				new_dsp->api_ui.setParamValue(i, val);
				new_dsp->param_values[i] = val;
			}
		}
	}

	{
		radium::PlayerLock lock;

		// The api_ui only writes the grouped (voice 0) control zones; the poly
		// voices only pick up values via GroupUI::updateAllZones (see
		// uiGroupItem::reflectZone in GUI.h), so fan the preserved values out
		// to all new voices now, while the new dsp is not yet live. Pure data
		// write, no callbacks into Radium, safe under the player lock.
		if (new_dsp->is_instrument && new_dsp->poly_dsp != NULL)
			new_dsp->poly_dsp->fGroups.updateAllZones();

		devdata->dsp_data = new_dsp;
		devdata->fade_out_is_active = false; // the new dsp plays at full volume
		devdata->fade_frames_left = 0;
		devdata->fade_frames_total = 0;
	}

	// Delete old implementation
	delete old_dsp;
}


//===========================================
// Compilation
//===========================================


static void delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
						   llvm_dsp_factory *llvm_factory,
#endif
						   interpreter_dsp_factory *interp_factory)
{
	if (interp_factory != NULL)
		deleteInterpreterDSPFactory(interp_factory);
	
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	if (llvm_factory != NULL)
		deleteDSPFactory(llvm_factory);
#endif
}


// Parses the url argument of a Faust 'soundfile' call, e.g.
// "{'bd_808.flac';'sn_dub.flac'}", into a list of file names.
static std::vector<std::string> parse_soundfile_url(const char *url)
{
	std::vector<std::string> ret;

	if (url == NULL || *url == 0)
		return ret;

	std::string s = url;

	// Remove surrounding braces and single quotes.
	while (!s.empty() && (s.front() == '{' || s.front() == '\''))
		s.erase(s.begin());
	while (!s.empty() && (s.back() == '}' || s.back() == '\''))
		s.pop_back();

	size_t pos = 0;
	while (true)
	{
		size_t end = s.find(';', pos);
		std::string item = s.substr(pos, (end == std::string::npos) ? std::string::npos : end - pos);

		// Remove single quotes from this item.
		std::string stripped;
		stripped.reserve(item.size());
		for (char c : item)
		{
			if (c != '\'')
				stripped.push_back(c);
		}
		if (!stripped.empty())
			ret.push_back(stripped);

		if (end == std::string::npos)
			break;
		pos = end + 1;
	}

	return ret;
}


// LibsndfileReader that opens files through Radium's own libsndfile wrapper
// (radium_sf_open), which handles non-ASCII paths on Windows (sf_wchar_open),
// where libsndfile's plain sf_open(const char*) fails. All decoding logic is
// delegated to the upstream *Aux helpers, so this reader keeps
// LibsndfileReader's features: chunked reads, 'is_double' support, and the
// '_SAMPLERATE' resampling if that build flag is ever enabled (it is not:
// the so. module already handles sample-rate differences via its read step
// srate(sf,part)/ma.SR, and resampling will be investigated separately).
class Faust2LibsndfileReader : public LibsndfileReader
{
public:

	Faust2LibsndfileReader()
		: LibsndfileReader(true)
	{}
	
	// Check that the file exists and is readable.
	bool checkFile(const std::string &path_name) override
	{
		SF_INFO snd_info;
		memset(&snd_info, 0, sizeof(snd_info));
		SNDFILE *snd_file = radium_sf_open(QString::fromStdString(path_name), SFM_READ, &snd_info);
		return checkFileAux(snd_file, path_name);
	}

	// Get the number of channels and the length in frames.
	void getParamsFile(const std::string &path_name, int &channels, int &length) override
	{
		SF_INFO snd_info;
		memset(&snd_info, 0, sizeof(snd_info));
		SNDFILE *snd_file = radium_sf_open(QString::fromStdString(path_name), SFM_READ, &snd_info);
		if (snd_file == NULL)
		{
			// checkFiles verified the file, so this should not happen; keep
			// getParamsFileAux's assert from firing.
			channels = 1;
			length = BUFFER_SIZE;
			return;
		}
		getParamsFileAux(snd_file, snd_info, channels, length);
	}

	// Read one file into part 'part' of 'soundfile', starting at frame
	// 'offset' (which is incremented by the number of frames read).
	void readFile(Soundfile *soundfile, const std::string &path_name, int part, int &offset, int max_chan) override
	{
		SF_INFO snd_info;
		memset(&snd_info, 0, sizeof(snd_info));
		SNDFILE *snd_file = radium_sf_open(QString::fromStdString(path_name), SFM_READ, &snd_info);
		if (snd_file == NULL)
		{
			// checkFiles verified the file, so this should not happen; keep
			// readFileAux's assert from firing and the part silent.
			soundfile->emptyFile(part, offset);
			return;
		}
		readFileAux(soundfile, snd_file, snd_info, part, offset, max_chan);
	}
};


// Creates a silent one-part Soundfile used when nothing could be loaded.
static Soundfile *create_empty_soundfile(void)
{
	Soundfile *sf = new Soundfile(1, BUFFER_SIZE * MAX_SOUNDFILE_PARTS, MAX_CHAN, 1, false);
	int offset = 0;
	for (int i = 0; i < MAX_SOUNDFILE_PARTS; i++)
		sf->emptyFile(i, offset);
	sf->shareBuffers(1, MAX_CHAN);
	return sf;
}

// Loads all files of one 'soundfile("label[url:{...}]")' call into a
// Soundfile, using the standard SoundfileReader::createSoundfile layout.
// Only absolute paths are used (no sample pool is searched); files that
// cannot be found become silent BUFFER_SIZE parts (the reader's
// "__empty_sound__" mechanism). Never returns NULL.
static Soundfile *load_soundfile(const std::vector<std::string> &filenames)
{
	Faust2LibsndfileReader reader;
	Soundfile::Directories dirs;

	std::vector<std::string> path_list = reader.checkFiles(dirs, filenames);
	for (size_t i = 0; i < path_list.size(); i++)
		if (path_list[i] == "__empty_sound__")
		{
			const std::string filename = filenames[i];
			THREADING_run_on_main_thread_async([filename]()
				{
					showAsyncMessage(QString("Faust Dev 2: Could not load soundfile '%1'. Replaced with silence.")
									 .arg(QString::fromStdString(filename)).toUtf8().constData());
				});
		}

	try
	{
		Soundfile *sf = reader.createSoundfile(path_list, MAX_CHAN, false);
		if (sf != NULL)
			return sf;
	}
	catch (...)
	{
	}
	return create_empty_soundfile();
}

// Loads all soundfiles used by the program in 'test_dsp'. Runs on the
// compile thread, so the (potentially slow) file decoding never blocks the
// audio thread. The Soundfile pointers are assigned to the dsp zones later,
// in create_dsp_data. Only absolute paths are supported: relative names are
// not searched in any sample pool and load as silence (with a warning).
static FaustDev2SoundfileData *collect_and_load_soundfiles(dsp *test_dsp)
{
	Faust2SoundfileCollectUI collect_ui;
	test_dsp->buildUserInterface(&collect_ui);

	if (collect_ui.url_list.empty())
		return NULL;

	FaustDev2SoundfileData *soundfile_data = new FaustDev2SoundfileData;

	for (const std::string &url : collect_ui.url_list)
	{
		if (soundfile_data->url2soundfile.find(url) != soundfile_data->url2soundfile.end())
			continue; // same url used twice; share the same data

		std::vector<std::string> filenames = parse_soundfile_url(url.c_str());
		Soundfile *sf = load_soundfile(filenames);
		soundfile_data->url2soundfile[url] = sf;
		soundfile_data->owned.push_back(sf);
	}

	return soundfile_data;
}


static dsp_factory *create_factory(const FaustDev2Data *devdata,
									int optlevel,
									QString &error_message,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
									llvm_dsp_factory **out_llvm_factory,
#endif
									interpreter_dsp_factory **out_interp_factory,
									MyQTemporaryDir **out_svg_dir,
									FaustDev2SoundfileData **out_soundfile_data)
{
	*out_soundfile_data = NULL;
	QStringList args_list = devdata->options.split("\n", Qt::SkipEmptyParts);

	// Create temp directory for SVG output
	MyQTemporaryDir *svg_dir = NULL;
	{
		MyQTemporaryDir *dir = new MyQTemporaryDir(QDir::tempPath() + QDir::separator() + "radium_faust2_svg_XXXXXX");
		if (dir->isValid()){
			args_list.push_back("-svg");
			args_list.push_back("-O");
			args_list.push_back(dir->path());
			svg_dir = dir;
		}else{
			delete dir;
		}
	}
	*out_svg_dir = svg_dir;

	QString radium_path = QCoreApplication::applicationDirPath();

	int argc = args_list.size();
	const char **argv = (const char**)calloc(argc, sizeof(char*));
	for (int i = 0; i < argc; i++)
		argv[i] = strdup(args_list[i].replace("%radium_path%", radium_path).toUtf8().constData());

	std::string error_msg;

	dsp_factory *factory = NULL;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	llvm_dsp_factory *llvm_factory = NULL;
#endif
	interpreter_dsp_factory *interp_factory = NULL;

	if (devdata->use_interpreter_backend){
		interp_factory = createInterpreterDSPFactoryFromString("FaustDev2", devdata->code.toUtf8().constData(), argc, argv, error_msg);
		factory = interp_factory;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	}else{
		llvm_factory = createDSPFactoryFromString("FaustDev2", devdata->code.toUtf8().constData(), argc, argv,
#if FOR_LINUX
												  "x86_64-pc-linux-gnu",
#elif FOR_MACOSX
												  MACOS_LLVM_TARGET,
#else
												  "",
#endif
												  error_msg, optlevel);
		factory = llvm_factory;
#endif
	}

	for (int i = 0; i < argc; i++)
		free((void*)argv[i]);
	free((void*)argv);

	if (factory == NULL){
		error_message = QString::fromStdString(error_msg);
		return NULL;
	}

	// Verify the factory works
	dsp *test_dsp = factory->createDSPInstance();
	if (test_dsp == NULL){
		if (interp_factory)
			deleteInterpreterDSPFactory(interp_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm_factory)
			deleteDSPFactory(llvm_factory);
#endif
		error_message = "createDSPInstance returned NULL";
		return NULL;
	}

	int num_inputs = test_dsp->getNumInputs();
	int num_outputs = test_dsp->getNumOutputs();

	FaustDev2SoundfileData *soundfile_data = collect_and_load_soundfiles(test_dsp);

	delete test_dsp;

	if (num_inputs > MAX_CHANNELS){
		if (interp_factory)
			deleteInterpreterDSPFactory(interp_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm_factory)
			deleteDSPFactory(llvm_factory);
#endif
		delete soundfile_data;
		error_message = QString("Maximum %1 input channels supported (%2)").arg(MAX_CHANNELS).arg(num_inputs);
		return NULL;
	}

	if (num_outputs > MAX_CHANNELS){
		if (interp_factory)
			deleteInterpreterDSPFactory(interp_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm_factory)
			deleteDSPFactory(llvm_factory);
#endif
		delete soundfile_data;
		error_message = QString("Maximum %1 output channels supported (%2)").arg(MAX_CHANNELS).arg(num_outputs);
		return NULL;
	}

#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	*out_llvm_factory = llvm_factory;
#endif
	*out_interp_factory = interp_factory;
	*out_soundfile_data = soundfile_data;

	return factory;
}


} // end anonymous namespace (split for Dev2CompileThread)


static void start_compilation(SoundPlugin *plugin);
static bool effect_is_visible(SoundPlugin *plugin, int effect_num);


// How long the current dsp fades out when a recompile finishes, before the
// new dsp is swapped in. Short enough to be nearly inaudible, long enough to
// avoid a click.
static constexpr int FADE_LENGTH_MS = 50;

// How many times to poll (with a 4 ms interval) for the fade to finish
// before swapping in the new dsp anyway. The fade progresses in RT_process,
// so if the audio device is closed it never finishes; swapping anyway is
// then harmless since nothing is audible.
static constexpr int FADE_POLL_RETRIES = 15;


static void perform_compile_completion(instrument_t patch_id,
									   dsp_factory *factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
									   llvm_dsp_factory *llvm_factory,
#endif
									   interpreter_dsp_factory *interp_factory,
									   FaustDev2SoundfileData *soundfile_data,
									   MyQTemporaryDir *svg_dir,
									   const QString &compile_code,
									   int retries_left)
{
	struct Patch *patch = PATCH_get_from_id(patch_id);
	if (patch == NULL || patch->patchdata == NULL){
		// The instrument was deleted while we were compiling.
		// Must delete the factory, or it stays in libfaust's global
		// factory table and crashes during shutdown (libfaust's static
		// destructor then destroys a JIT registration mutex that has
		// already been torn down).
		delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
					   llvm_factory,
#endif
					   interp_factory);
		delete svg_dir;
		delete soundfile_data;
		return;
	}

	SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	devdata->is_compiling = false;

	// Discard if code changed while this compilation was running
	if (devdata->code != compile_code){
		delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
					   llvm_factory,
#endif
					   interp_factory);
		delete svg_dir; // this compile result is outdated
		delete soundfile_data;
		start_compilation(plugin); // Recompile the latest code.
		return;
	}

	// Start fading out the current dsp (unless it is already fading out from
	// an earlier compile), and wait for the fade to finish before swapping in
	// the new dsp, so the old dsp is not cut off abruptly. The fade
	// progresses in RT_process, so we poll with a timer instead of holding
	// the player lock. The old and new dsp never compute at the same time, so
	// there is no CPU spike during the transition.
	//
	// Note: check fade_out_is_active, not fade_frames_left, when deciding
	// whether to start the fade. fade_frames_left reaches 0 when the fade is
	// finished, and this function is re-entered by the poll timer below, so
	// checking fade_frames_left would restart the finished fade. That re-arms
	// the normal compute path in RT_process: the old dsp, which had been
	// frozen in the silent wait window, starts playing its voices again at
	// full volume (the fade multiplier restarts at 1.0), and then the swap is
	// forced by the retry budget while the restarted fade is barely started,
	// cutting the burst abruptly. Net effect: ugly scratchy sounds for some
	// 60-70ms after a recompile. fade_out_is_active, on the other hand, is
	// only set when the fade starts and only cleared at the swap.
	bool must_wait_for_fade;
	{
		radium::PlayerLock lock; // makes the fade state reads consistent

		if (devdata->dsp_data != NULL && devdata->fade_out_is_active == false){
			devdata->fade_out_is_active = true;
			devdata->fade_frames_total = (int)(FADE_LENGTH_MS * MIXER_get_sample_rate() / 1000.0);
			devdata->fade_frames_left = devdata->fade_frames_total;
		}

		must_wait_for_fade = devdata->fade_out_is_active && devdata->fade_frames_left > 0;
	}

	if (must_wait_for_fade && retries_left > 0){
		QTimer::singleShot(4, [patch_id, factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
							   llvm_factory,
#endif
							   interp_factory,
							   soundfile_data,
							   svg_dir,
							   compile_code,
							   retries_left_m1 = retries_left-1]()
			{
				perform_compile_completion(patch_id,
										   factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
										   llvm_factory,
#endif
										   interp_factory,
										   soundfile_data,
										   svg_dir,
										   compile_code,
										   retries_left_m1);
		});
		return;
	}

	// Create the DSP on the main thread. The poly dsp's GroupUI registers
	// itself in Faust's process-global GUI list (GUI.h), which the main
	// thread iterates in GUI::updateAllGuis (from QTGUI timers), so
	// constructing it on the compile thread raced with that iteration.
	FaustDev2Dsp *dsp_data = create_dsp_data(factory,
											 factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
											 llvm_factory,
#endif
											 interp_factory,
											 soundfile_data,
											 MIXER_get_sample_rate());

	if (dsp_data == NULL){
		delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
					   llvm_factory,
#endif
					   interp_factory);
		delete svg_dir;
		return; // soundfile_data was deleted inside create_dsp_data
	}

	devdata->error_message = "";

	// Refresh the plugin's reset-to defaults. The parameter layout and
	// defaults may have changed since the plugin was created (e.g. an LLM
	// recompile). At creation, PLUGIN_init captures the initial values
	// before this asynchronous first compile has produced a dsp, so every
	// slot holds get_effect_value's 0.5 sentinel, and the Reset button
	// restores 0.5 instead of the code defaults. Capture the new program's
	// defaults NOW - hotswap_dsp_data below overwrites the new dsp's
	// param_values with values preserved from the old dsp.
	const int num_params = dsp_data->num_params < plugin->type->num_effects ? dsp_data->num_params : plugin->type->num_effects;
	for (int i = 0; i < num_params; i++)
	{
		plugin->initial_effect_values_native[i] = dsp_data->param_values[i];
		const float min_val = dsp_data->api_ui.getParamMin(i);
		const float max_val = dsp_data->api_ui.getParamMax(i);
		plugin->initial_effect_values_scaled[i] = scale(dsp_data->param_values[i], min_val, max_val, 0.0f, 1.0f);
	}

	// Store SVG dir
	if (devdata->svg_dir != NULL)
		delete devdata->svg_dir;

	devdata->svg_dir = svg_dir;

	// Recreate the QTGUI
	if (devdata->qtgui != NULL){
		devdata->qtgui->stop();
		if (devdata->qtgui_parent.data() != NULL
		    && devdata->qtgui_parent->layout() != NULL)
		{
			devdata->qtgui_parent->layout()->removeWidget(devdata->qtgui);
		}
		delete devdata->qtgui;
	}

	for (Faust2GuiControlRef *ref : devdata->qtgui_control_refs)
		delete ref;

	devdata->qtgui_control_refs.clear();

	// Create dialog parent if needed
	if (devdata->qtgui_parent.data() == NULL){
		devdata->qtgui_parent = FAUST_create_qdialog(plugin);
		devdata->qtgui_parent->setLayout(new QGridLayout(devdata->qtgui_parent.data()));
	}

	devdata->qtgui = new QTGUI(devdata->qtgui_parent.data());
	dsp_data->final_dsp->buildUserInterface(devdata->qtgui);
	devdata->qtgui_parent->layout()->addWidget(devdata->qtgui);

	// buildUserInterface resets each control zone to its default value
	// (the QTGUI widget constructors write fCur back into the zone), so the
	// preserved values must be copied in afterwards. This happens in
	// hotswap_dsp_data below, which (1) copies the old values into the new
	// api_ui / param_values, (2) fans them out to all poly voices via
	// fGroups.updateAllZones(), and only then (3) swaps the new dsp in under
	// the player lock. The qtgui->update() call further below
	// (GUI::updateAllGuis) then just refreshes the recreated widgets.
	hotswap_dsp_data(devdata, dsp_data, devdata->reset_effect_values_on_compile);

	// Also refresh the stored values read by the GUI sliders (via
	// PLUGIN_get_effect_value(..., VALUE_FROM_STORAGE)). They were captured
	// at plugin creation, before this asynchronous first compile produced a
	// dsp, so every slot holds get_effect_value's 0.5 sentinel and the
	// sliders all show the middle position. After the hotswap, param_values
	// hold the real values (the program defaults, or the values preserved
	// from the previous dsp).
	for (int i = 0; i < num_params; i++)
	{
		const float native = dsp_data->param_values[i];
		const float min_val = dsp_data->api_ui.getParamMin(i);
		const float max_val = dsp_data->api_ui.getParamMax(i);
		safe_float_write(&plugin->stored_effect_values_native[i], native);
		safe_float_write(&plugin->stored_effect_values_scaled[i],
						 scale(native, min_val, max_val, 0.0f, 1.0f));
	}

	if (dsp_data->is_instrument)
		plugin->type->is_instrument = true;
	else
		plugin->type->is_instrument = false;

	// Route GUI-dialog control changes through set_effect_value, so they
	// update param_values / stored values and survive recompiles.
	for (int i = 0; i < dsp_data->num_params; i++)
	{
		if (effect_is_visible(plugin, i) == false)
			continue;

		Faust2GuiControlRef *ref = new Faust2GuiControlRef;

		ref->patch_id = plugin->patch->id;
		ref->effect_num = i;

		devdata->qtgui_control_refs.push_back(ref);
		devdata->qtgui->addCallback(dsp_data->api_ui.getParamZone(i),
									faust2_gui_zone_callback,
									ref);
	}

	// Restart the interface if it is visible. The old QTGUI was stopped
	// (and deleted) above, so without this call the rebuilt interface
	// freezes (its refresh timer never starts).
	devdata->qtgui->update();

	if (devdata->qtgui_parent.data() != NULL
	    && devdata->qtgui_parent->isVisible())
	{
		devdata->qtgui->run();
	}

	devdata->ready.has_new_data = true;
	devdata->ready.factory_is_ready = true;
	devdata->ready.factory_succeeded = true;
	devdata->ready.svg_is_ready = true;
	devdata->ready.svg_succeeded = (svg_dir != NULL);
}


class Dev2CompileThread : public QThread
{
	instrument_t _patch_id;
	QString _code;
	QString _options;
	bool _use_interpreter;
	int _optlevel;

public:
	Dev2CompileThread(SoundPlugin *plugin,
					  const QString &code,
					  const QString &options,
					  bool use_interpreter)
		: _patch_id(plugin->patch->id)
		, _code(code)
		, _options(options)
		, _use_interpreter(use_interpreter)
		, _optlevel(getFaustOptimizationLevel()) // Must be read on the main thread (settings are main-thread only). This constructor runs on the main thread.
	{
	}

	void run() override
	{
		FaustDev2Data tmp_data;
		tmp_data.code = _code;
		tmp_data.options = _options;
		tmp_data.use_interpreter_backend = _use_interpreter;

		QString error_message;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		llvm_dsp_factory *llvm_factory = NULL;
#endif
		interpreter_dsp_factory *interp_factory = NULL;
		MyQTemporaryDir *svg_dir = NULL;
		FaustDev2SoundfileData *soundfile_data = NULL;

		//
		// Note: THIS CALL IS THE BIG EXPENSIVE THING.
		//
		dsp_factory *factory = create_factory(&tmp_data,
											  _optlevel,
											  error_message,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
											  &llvm_factory,
#endif
											  &interp_factory,
											  &svg_dir,
											  &soundfile_data);

		if (factory == NULL)
		{
			
			delete svg_dir;
			delete soundfile_data;

			THREADING_run_on_main_thread_async([patch_id = _patch_id,
												error_message,
												compile_code = _code]()
				{
					struct Patch *patch = PATCH_get_from_id(patch_id);
					
					if (patch == NULL || patch->patchdata == NULL)
						return;
					
					SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
					
					FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
					
					devdata->is_compiling = false;
					devdata->error_message = error_message;
					devdata->ready.has_new_data = true;
					devdata->ready.factory_is_ready = true;
					devdata->ready.factory_succeeded = false;
					devdata->ready.svg_is_ready = true;
					devdata->ready.svg_succeeded = false;

					// Recompile if code changed while this compilation was running.
					if (devdata->code != compile_code)
						start_compilation(plugin);
				});
			
			return;
		}

		THREADING_run_on_main_thread_async([patch_id = _patch_id,
											factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
											llvm_factory,
#endif
											interp_factory,
											soundfile_data,
											svg_dir,
											compile_code = _code](){
			perform_compile_completion(patch_id, factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
									   llvm_factory,
#endif
									   interp_factory,
									   soundfile_data,
									   svg_dir,
									   compile_code,
									   FADE_POLL_RETRIES);
		});
	}
};


// All running Dev2CompileThread instances. Tracked so FAUST2_shut_down can wait
// for them before libfaust tears down its global DSP factory table (otherwise a
// still-running compile could recreate a factory during/after teardown, and any
// leftover factory makes libfaust's static destructor crash at exit).
static QList<QPointer<Dev2CompileThread>> g_compile_threads;

static void start_dev2_compile_thread(Dev2CompileThread *thread){
	g_compile_threads.push_back(thread);

	// Use 'thread' as the context object: QThread::finished is emitted from the
	// worker thread, and without a context object the functor would run there
	// too, racing with the main thread's push_back/iteration of
	// g_compile_threads. With 'thread' as context (the QThread object lives on
	// the main thread), the functor is queued to the main thread.
	QObject::connect(thread, &QThread::finished, thread, [thread](){
		g_compile_threads.removeAll(thread);
		thread->deleteLater();
	});

	thread->start();
}


static void start_compilation(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	if (devdata->is_compiling)
		return;

	if (devdata->code == "")
		return;

	devdata->is_compiling = true;

	Dev2CompileThread *thread = new Dev2CompileThread(plugin,
													  devdata->code,
													  devdata->options,
													  devdata->use_interpreter_backend);
	start_dev2_compile_thread(thread);
}


//===========================================
// SoundPluginType Callbacks
//===========================================

// The pitch comparisons in this section compare float pitches that were
// stored from the same note_t value they are later matched against, so the
// exact == comparisons are correct (the values are bit-identical).
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"


// The player lock must be held when calling this function.
static void register_note_voice(FaustDev2Dsp *dsp_data, const note_t &note, float pitch, FaustDev2PolyVoice *voice)
{
	// The voice may have been stolen from a previous note; drop that note's entry.
	for (int i = 0; i < dsp_data->num_note_voices; )
	{
		if (dsp_data->note_voices[i].voice == voice)
		{
			dsp_data->note_voices[i] = dsp_data->note_voices[dsp_data->num_note_voices-1];
			dsp_data->num_note_voices--;
		}
		else
		{
			i++;
		}
	}

	if (dsp_data->num_note_voices < MAX_POLYPHONY)
	{
		dsp_data->note_voices[dsp_data->num_note_voices].note_id = note.id;
		dsp_data->note_voices[dsp_data->num_note_voices].seqblock = note.seqblock;
		dsp_data->note_voices[dsp_data->num_note_voices].pitch = pitch;
		dsp_data->note_voices[dsp_data->num_note_voices].voice = voice;
		dsp_data->num_note_voices++;
	}
}


// The player lock must be held when calling this function.
static bool release_note_voice(FaustDev2Dsp *dsp_data, const note_t &note, float pitch)
{
	for (int i = 0; i < dsp_data->num_note_voices; i++)
	{
		NoteVoice &nv = dsp_data->note_voices[i];
		if (nv.pitch == pitch && is_note(note, nv.note_id, nv.seqblock))
		{
			FaustDev2PolyVoice *voice = nv.voice;

			dsp_data->note_voices[i] = dsp_data->note_voices[dsp_data->num_note_voices-1];
			dsp_data->num_note_voices--;

			// Sanity check that the voice is still playing this pitch. If it
			// was released or restarted, the note is already gone, and
			// releasing the voice would kill the note that took it over.
			if (voice->fCurNote == pitch)
				voice->keyOff();

			return true;
		}
	}

	return false;
}


// The poly dsp's voice mixer uses fixed internal buffers of MIX_BUFFER_SIZE
// frames (poly-dsp.h), so a compute longer than that overflows the heap
// buffers. Split the compute into chunks that fit. (Block sizes up to 8192
// are selectable in the preferences.)
static constexpr int MAX_POLY_COMPUTE_FRAMES = MIX_BUFFER_SIZE;

static void compute_poly_chunked(nonstealing_microtonal_poly_dsp *poly_dsp,
								 int num_inputs,
								 int num_outputs,
								 int num_frames,
								 float **inputs,
								 float **outputs,
								 int offset)
{
	int done = 0;
	
	while (done < num_frames)
	{
		int chunk = MAX_POLY_COMPUTE_FRAMES;

		if (num_frames - done < MAX_POLY_COMPUTE_FRAMES)
			chunk = num_frames - done;
		
		float **in_slice = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, num_inputs));
		
		float **out_slice = RT_ALLOC_ARRAY_STACK(float*, R_MAX(1, num_outputs));
		
		for (int ch = 0; ch < num_inputs; ch++)
			in_slice[ch] = &inputs[ch][offset + done];
		
		for (int ch = 0; ch < num_outputs; ch++)
			out_slice[ch] = &outputs[ch][offset + done];
		
		poly_dsp->compute(chunk, in_slice, out_slice);
		
		done += chunk;
	}
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL)
	{
		for (int ch = 0; ch < MAX_CHANNELS; ch++)
			memset(outputs[ch], 0, num_frames * sizeof(float));
		
		return;
	}

	// When a recompile finishes, the current dsp fades out and the new dsp is
	// only swapped in after the fade is finished (see
	// perform_compile_completion), so the old dsp is not cut off abruptly.
	// fade_out_is_active is set by the main thread (under the player lock)
	// and only cleared at the swap.
	const bool fading = devdata->fade_out_is_active;

	if (fading && devdata->fade_frames_left == 0)
	{
		// Fade finished: stay silent until the new dsp is swapped in. Clear
		// the collector, or a note-on arriving in the wait window would be
		// processed with the wrong timing if a later compile restarts the
		// fade (playing a short, out-of-time blip before the swap).
		dsp_data->collector.clear();
		for (int ch = 0; ch < MAX_CHANNELS; ch++)
			memset(outputs[ch], 0, num_frames * sizeof(float));
		
		return;
	}

	int num_inputs = dsp_data->num_inputs;
	int num_outputs = dsp_data->num_outputs;

	if (dsp_data->is_instrument && dsp_data->poly_dsp != NULL)
	{
		dsp_data->collector.sort();

		int pos = 0;
		
		for (int i = 0; i < dsp_data->collector.num_events; i++)
		{
			const NoteEventCollector::Event &ev = dsp_data->collector.events[i];
			
			int seg_len = ev.sample_offset - pos;
			
			if (seg_len < 0)
				seg_len = 0;

			if (seg_len > 0)
				compute_poly_chunked(dsp_data->poly_dsp, num_inputs, num_outputs, seg_len, inputs, outputs, pos);

			if (ev.type == NoteEventCollector::NOTE_ON)
			{
				float pitch = ev.note.pitch;
				float gain = velocity2gain(ev.note.velocity);
				int vel = R_BOUNDARIES(0, (int)(gain * 127), 127);
				MapUI *voice = dsp_data->poly_dsp->keyOn(0, pitch, vel);
				if (voice != NULL)
					register_note_voice(dsp_data, ev.note, pitch, static_cast<FaustDev2PolyVoice*>(voice));
				else
					RT_message("FaustDev2 instrument: no more free voices. (max polyphony is %d)", (int)dsp_data->poly_dsp->fVoiceTable.size());
			}
			else if (ev.type == NoteEventCollector::NOTE_OFF)
			{
				float pitch = ev.note.pitch;
				
				if (release_note_voice(dsp_data, ev.note, pitch) == false)
				{
					// No registered voice for this note (for instance because its
					// keyOn event was dropped). Only fall back to the
					// pitch-based keyOff if no other registered note uses the
					// same pitch, since keyOff releases the *oldest* voice with
					// that pitch, which would release the wrong note when notes
					// overlap on one pitch.
					bool other_note_with_same_pitch = false;
					
					for (int i = 0; i < dsp_data->num_note_voices; i++)
						if (dsp_data->note_voices[i].pitch == pitch)
						{
							other_note_with_same_pitch = true;
							break;
						}
					
					if (other_note_with_same_pitch == false)
						dsp_data->poly_dsp->keyOff(0, pitch);
				}
			}
			else // NOTE_PITCH
			{
				// Glide / pitch-line change. Only the freq/key zones are
				// updated (setPitch keeps fCurNote at the note-on pitch, since
				// note-off events carry the original pitch). If the note is not
				// registered (keyOn was dropped, or the pitch event is ordered
				// before its note-on), the event is ignored.
				for (int n = 0; n < dsp_data->num_note_voices; n++)
				{
					NoteVoice &nv = dsp_data->note_voices[n];
					if (is_note(ev.note, nv.note_id, nv.seqblock))
					{
						nv.voice->setPitch(ev.note.pitch);
						break;
					}
				}
			}

			pos = ev.sample_offset;
		}

		if (num_frames > pos)
			compute_poly_chunked(dsp_data->poly_dsp, num_inputs, num_outputs, num_frames - pos, inputs, outputs, pos);

		dsp_data->collector.clear();
	}
	else
	{
		dsp_data->final_dsp->compute(num_frames, inputs, outputs);
	}

	if (fading)
	{
		// Fade out in place. RT_fade_out ramps from the current position
		// (done) down to the end of the fade (done+how_many), and the rest of
		// the block is silenced below.
		const int done = devdata->fade_frames_total - devdata->fade_frames_left;
		const int how_many = R_MIN(num_frames, devdata->fade_frames_left);

		for (int ch = 0; ch < num_outputs; ch++)
			RT_fade_out(outputs[ch], devdata->fade_frames_total, done, done + how_many);

		for (int ch = 0; ch < num_outputs; ch++)
			memset(outputs[ch] + how_many, 0, (num_frames - how_many) * sizeof(float));

		devdata->fade_frames_left -= how_many;
	}

	// Clean unused channels.
	for (int ch = num_outputs; ch < MAX_CHANNELS; ch++)
		memset(outputs[ch], 0, num_frames * sizeof(float));
}

#pragma GCC diagnostic pop


static void play_note(SoundPlugin *plugin, int block_delta_time, note_t note)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data != NULL && dsp_data->is_instrument)
		dsp_data->collector.noteOn(block_delta_time, note);
}


static void stop_note(SoundPlugin *plugin, int block_delta_time, note_t note)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data != NULL && dsp_data->is_instrument)
		dsp_data->collector.noteOff(block_delta_time, note);
}


static void set_note_volume(SoundPlugin *plugin, int block_delta_time, note_t note)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || !dsp_data->is_instrument)
		return;

	// For now, just ignore sub-block precision for volume changes (matching existing Faust-Dev behavior).
	// Volume is already applied when the note starts via keyOn's velocity parameter.
	// TODO: Support per-voice volume updates via MapUI.
	(void)block_delta_time;
	(void)note;
}


static void set_note_pitch(SoundPlugin *plugin, int block_delta_time, note_t note)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	// The pitch change is queued and applied in RT_process at the exact
	// sample offset, in the same way as note-on/note-off events. Note that
	// the engine only calls set_note_pitch while the note is still playing,
	// and that note-off events carry the original note-on pitch, so the
	// voice keeps its original fCurNote (see FaustDev2PolyVoice::setPitch).
	if (dsp_data != NULL && dsp_data->is_instrument)
		dsp_data->collector.notePitch(block_delta_time, note);
}


static void set_effect_value(SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return;

	float native_value;

	if (value_format == EFFECT_FORMAT_SCALED){
		float min_val = dsp_data->api_ui.getParamMin(effect_num);
		float max_val = dsp_data->api_ui.getParamMax(effect_num);
		native_value = scale(value, 0, 1, min_val, max_val);
	}else{
		native_value = value;
	}

	dsp_data->param_values[effect_num] = native_value;
	dsp_data->api_ui.setParamValue(effect_num, native_value);

	// For polyphonic instruments the control zones are "grouped": writing to a
	// control only reaches the actual voice DSPs when the voice group UI is
	// refreshed (see GroupUI in poly-dsp.h). Without this the
	// controls are dead unless the QTGUI dialog happens to be open.
	//
	// We must not call the global GUI::updateAllGuis() here: it runs while the
	// player lock is held and also updates other instruments' Qt GUIs, whose
	// callbacks call back into Radium (e.g. faust_gui_zone_callback), which
	// asserts when the player lock is held. Refreshing only this instrument's
	// voice group is a pure data update and is safe from any thread.
	if (dsp_data->is_instrument && dsp_data->poly_dsp != NULL)
		dsp_data->poly_dsp->fGroups.updateAllZones();
}


static float get_effect_value(SoundPlugin *plugin, int effect_num, enum ValueFormat value_format)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return 0.5f;

	float native_value = dsp_data->param_values[effect_num];

	if (value_format == EFFECT_FORMAT_SCALED){
		float min_val = dsp_data->api_ui.getParamMin(effect_num);
		float max_val = dsp_data->api_ui.getParamMax(effect_num);
		return scale(native_value, min_val, max_val, 0.0f, 1.0f);
	}else{
		return native_value;
	}
}


static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params){
		snprintf(buffer, buffersize, " ");
		return;
	}

	int item_type = dsp_data->api_ui.getParamItemType(effect_num);

	if (item_type == APIUI::kButton || item_type == APIUI::kCheckButton)
		snprintf(buffer, buffersize, "%s", dsp_data->param_values[effect_num] > 0.5f ? "On" : "Off");
	else{
		const char *unit = dsp_data->api_ui.getMetadata(effect_num, "unit");
		float step = dsp_data->api_ui.getParamStep(effect_num);

		if (equal_floats(step, 1.0f))
			snprintf(buffer, buffersize, "%d %s", (int)dsp_data->param_values[effect_num], unit ? unit : "");
		else
			snprintf(buffer, buffersize, "%.2f %s", dsp_data->param_values[effect_num], unit ? unit : "");
	}
}


static int get_effect_format(SoundPlugin *plugin, int effect_num)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return EFFECT_FORMAT_FLOAT;

	int item_type = dsp_data->api_ui.getParamItemType(effect_num);

	return (item_type == APIUI::kButton || item_type == APIUI::kCheckButton)
		? EFFECT_FORMAT_BOOL : EFFECT_FORMAT_FLOAT;
}


static const char *get_effect_name(const SoundPlugin *plugin, int effect_num)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return NOTUSED_EFFECT_NAME;

	return dsp_data->api_ui.getParamLabel(effect_num);
}


static const char *get_effect_description(SoundPlugin *plugin, int effect_num)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return "";

	return dsp_data->api_ui.getMetadata(effect_num, "tooltip");
}


static bool effect_is_visible(SoundPlugin *plugin, int effect_num)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data == NULL || effect_num >= dsp_data->num_params)
		return false;

	// The note-control parameters (freq/key, gain/vel/velocity, gate) are
	// managed by note events and hidden from the GUI - but ONLY when the
	// program defines the full note-control set, i.e. all three of freq,
	// gain, and gate (the same condition MidiMeta::checkPolyphony uses to
	// detect an instrument). A program defining just one or two of them,
	// e.g. a filter effect with a 'freq' slider, keeps its sliders in the
	// GUI. The Panic button added by the poly dsp is always hidden.
	const char *addr = dsp_data->api_ui.getParamAddress(effect_num);
	std::string path(addr);

	if (MapUI::endsWith(path, "/Panic"))
		return false;

	bool has_freq = false;
	bool has_gain = false;
	bool has_gate = false;
	for (int i = 0; i < dsp_data->num_params; i++)
	{
		const char *param_addr = dsp_data->api_ui.getParamAddress(i);
		if (param_addr == NULL)
			continue;
		std::string param_path(param_addr);
		if (MapUI::endsWith(param_path, "/freq")
		    || MapUI::endsWith(param_path, "/key"))
			has_freq = true;
		else if (MapUI::endsWith(param_path, "/gain")
		         || MapUI::endsWith(param_path, "/vel")
		         || MapUI::endsWith(param_path, "/velocity"))
			has_gain = true;
		else if (MapUI::endsWith(param_path, "/gate"))
			has_gate = true;
	}

	if (has_freq && has_gain && has_gate
	    && (MapUI::endsWith(path, "/freq")
	        || MapUI::endsWith(path, "/key")
	        || MapUI::endsWith(path, "/gain")
	        || MapUI::endsWith(path, "/vel")
	        || MapUI::endsWith(path, "/velocity")
	        || MapUI::endsWith(path, "/gate")))
	{
		return false;
	}

	return true;
}


//===========================================
// State serialization
//===========================================

static void create_state(const SoundPlugin *plugin, hash_t *state)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	HASH_put_string(state, "code", STRING_toBase64(STRING_create(devdata->code)));
	HASH_put_string(state, "options", STRING_toBase64(STRING_create(devdata->options)));
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	HASH_put_bool(state, "use_interpreter_backend", devdata->use_interpreter_backend);
#endif
}


static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading)
{
	FaustDev2Data *devdata = new FaustDev2Data;

	plugin->data = devdata;

	if (state != NULL){
		devdata->code = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "code")));
		devdata->options = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "options")));
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (HASH_has_key(state, "use_interpreter_backend"))
			devdata->use_interpreter_backend = HASH_get_bool(state, "use_interpreter_backend") || OS_running_under_rosetta();
#endif
	}else{
		devdata->code = g_default_faust_dev2_program;
	}

	if (is_loading == false){
		start_compilation(plugin);
	}else{
		// Synchronous compilation for loading
		QString error_message;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		llvm_dsp_factory *llvm_factory = NULL;
#endif
		interpreter_dsp_factory *interp_factory = NULL;
		MyQTemporaryDir *svg_dir = NULL;
		FaustDev2SoundfileData *soundfile_data = NULL;

		dsp_factory *factory = create_factory(devdata, getFaustOptimizationLevel(), error_message, // main thread, safe to read settings
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
											   &llvm_factory,
#endif
											   &interp_factory,
											   &svg_dir,
											   &soundfile_data);

		if (factory != NULL){
			// Store SVG dir
			if (devdata->svg_dir != NULL)
				delete devdata->svg_dir;
			devdata->svg_dir = svg_dir;

			FaustDev2Dsp *dsp_data = create_dsp_data(factory, factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
													  llvm_factory,
#endif
													  interp_factory,
													  soundfile_data,
													  sample_rate);
			if (dsp_data != NULL){
				// No old dsp here, so the reset flag has no effect. The
				// effect values saved in the song state are applied after
				// create_plugin_data returns, so they must not be lost.
				hotswap_dsp_data(devdata, dsp_data, false);
			}
		} else {
			// Loading a song with code that does not compile: store the
			// error and signal the failed compile exactly like the async
			// compile thread does (Dev2CompileThread::run), so the editor
			// widget shows the error pane when it is opened. Without this,
			// the failure is silent and the instrument (which has no DSP)
			// looks like it never tried to compile.
			delete svg_dir;
			delete soundfile_data;

			devdata->error_message = error_message;
			devdata->ready.has_new_data = true;
			devdata->ready.factory_is_ready = true;
			devdata->ready.factory_succeeded = false;
			devdata->ready.svg_is_ready = true;
			devdata->ready.svg_succeeded = false;
		}
	}

	return devdata;
}


static void cleanup_plugin_data(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	delete devdata->qtgui;
	devdata->qtgui = NULL;

	if (devdata->qtgui_parent.data() != NULL){
		// Safe: qtgui_parent is the FaustQDialog, never g_main_window.
		// During app shutdown, Qt destroys g_main_window first, so
		// QPointer auto-nulls and we skip the delete, avoiding double-free.
		devdata->qtgui_parent->close();
		delete devdata->qtgui_parent.data();
	}

	delete devdata;
}


//===========================================
// FAUST_ API functions (used by editor widget)
//===========================================

void FAUST2_set_code(SoundPlugin *plugin, QString code)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	devdata->code = code;
}

void FAUST2_set_reset_effect_values_on_compile(SoundPlugin *plugin, bool reset)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	devdata->reset_effect_values_on_compile = reset;
}

void FAUST2_set_options(SoundPlugin *plugin, QString options)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	devdata->options = options;
}

bool FAUST2_is_compiling(const SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->is_compiling;
}

QString FAUST2_get_code(const SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->code;
}

QString FAUST2_get_options(const SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->options;
}

void FAUST2_generate_cpp_code(const SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	QString code = devdata->code;
	QString options = devdata->options;

	// Must do this in main thread since DISK_create_non_existant_filename allocates gc-memory.
	filepath_t template_ = appendFilePaths(DISK_get_temp_dir(), make_filepath(L"radium_faust2_cppsource.cpp"));
	filepath_t temp_file = DISK_create_non_existant_filename(template_);

	QString filename = STRING_get_qstring(temp_file.id);

	auto ret = QtConcurrent::run([code, options, generation, filename, callback]{

		radium::ArgsCreator args;
		args.push_back("-o");
		args.push_back(filename);
		args.push_back(options.split("\n", Qt::SkipEmptyParts));

		std::string error_message2;

		QString message;

		if (generateAuxFilesFromString(
		                             "FaustDev",
		                             code.toUtf8().constData(),
		                             args.get_argc(),
		                             args.get_argv(),
		                             error_message2
		                             )
		    == false)
		{

			message = QString("// Unable to create cpp source: %1").arg(error_message2.c_str());

		} else {

			disk_t *disk = DISK_open_for_reading(filename);

			if (disk==NULL){

				message = QString("// Error! File not found: \"") + filename.toUtf8().constData() + "\"";

			} else {

				QString cpp_code = DISK_read_qstring_file(disk);

				if (DISK_close_and_delete(disk)==false) {

					message = QString("// Error! Unable to read from \"") + filename.toUtf8().constData() + "\"";

				} else {

					message = cpp_code;

				}

			}

			QFile::remove(filename);

		}

		THREADING_run_on_main_thread_async([callback, generation, message]{
			callback(generation, message);
		});

	});
	(void)ret;
}

QString FAUST2_get_error_message(const SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->error_message;
}

QString FAUST2_get_svg_path(const SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	if (devdata->svg_dir == NULL || devdata->svg_dir->isValid() == false)
		return "";
	return devdata->svg_dir->path() + QDir::separator() + "FaustDev2-svg" + QDir::separator() + "process.svg";
}

radium::FAUST_calledRegularlyByParentReply FAUST2_calledRegularlyByParent(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	auto ret = devdata->ready;
	devdata->ready = radium::FAUST_calledRegularlyByParentReply();
	return ret;
}

void FAUST2_start_compilation(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	if (devdata->code == "")
		return;

	if (devdata->is_compiling)
		return; // A compile is already running. When it finishes, it will recompile the latest code if it has changed.

	devdata->is_compiling = true;

	Dev2CompileThread *thread = new Dev2CompileThread(plugin,
	                                                   devdata->code,
	                                                   devdata->options,
	                                                   devdata->use_interpreter_backend);
	start_dev2_compile_thread(thread);
}

void FAUST2_shut_down(void)
{
	// Wait for any in-flight compiles so no factory is created while (or
	// after) we empty libfaust's global factory table.
	for (const QPointer<Dev2CompileThread> &thread : g_compile_threads)
		if (thread)
			thread->wait(10000);
	g_compile_threads.clear();

	// Empty the global factory tables now, while libfaust's static objects are
	// still alive. If factories are left in the table, libfaust destroys them
	// in its static destructor at exit(), which crashes because the LLVM JIT
	// registration mutex has already been torn down
	// ("recursive_mutex lock failed: Invalid argument").
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	deleteAllDSPFactories();
#endif
	deleteAllInterpreterDSPFactories();
}

bool FAUST2_set_use_interpreter_backend(SoundPlugin *plugin, bool use_interpreter)
{
#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
	(void)plugin;
	(void)use_interpreter;
	return false;
#else
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	if (!use_interpreter && OS_running_under_rosetta())
		return false;
	devdata->use_interpreter_backend = use_interpreter;
	return true;
#endif
}

bool FAUST2_get_use_interpreter_backend(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->use_interpreter_backend;
}


//=====================================================
// Static analysis used by the LLM auto-fix loop.
//
// When a generated program fails to compile, the widget asks the LLM to fix
// it. Faust's compiler error for arity/composition mistakes is a multi-KB
// dump of the inlined signal graph without source locations, so the model
// cannot localize the bug (it has failed whole sessions rewriting the wrong
// code). To give it exact lines instead, each top-level definition of the
// failing program is compiled in isolation here: the expression is wrapped
// in its own tiny program, compiled with the fast interpreter backend, and
// the real compiler's type system then tells us whether the expression
// itself is well-formed and whether it has unbound audio inputs (the
// signature of "a filter/smoother was used as a plain value instead of
// being applied to a signal with ':'").
//=====================================================

namespace
{

struct Faust2LintDef
{
	QString name;
	QString rhs;
	int line;
};

// Masks strings and comments in one line so delimiter counting below never
// sees them. 'in_block_comment' carries a /* */ comment across lines.
QString faust2_lint_mask_line(const QString &line, bool &in_block_comment)
{
	QString masked = line;
	const int len = masked.size();

	for (int i = 0; i < len; i++)
	{
		const QChar c = masked.at(i);

		if (in_block_comment)
		{
			if (c == '*' && i + 1 < len && masked.at(i + 1) == '/')
			{
				masked[i] = masked[i + 1] = ' ';
				i++;
				in_block_comment = false;
			}
			else
				masked[i] = ' ';
		}
		else if (c == '/' && i + 1 < len && masked.at(i + 1) == '/')
		{
			while (i < len)
			{
				masked[i] = ' ';
				i++;
			}
		}
		else if (c == '/' && i + 1 < len && masked.at(i + 1) == '*')
		{
			masked[i] = masked[i + 1] = ' ';
			i++;
			in_block_comment = true;
		}
		else if (c == '"')
		{
			masked[i] = ' ';
			i++;
			while (i < len && masked.at(i) != '"')
			{
				if (masked.at(i) == '\\')
				{
					masked[i] = ' ';
					i++;
				}
				masked[i] = ' ';
				i++;
			}
		}
	}

	return masked;
}

// Collects the top-level "name = ...;" definitions of 'code'. The RHS may
// span several lines ('with { }' blocks included) and ends at the first ';'
// outside of any bracket. Assumes at most one definition per line.
QList<Faust2LintDef> faust2_lint_collect_defs(const QString &code)
{
	QList<Faust2LintDef> defs;
	const QStringList lines = code.split('\n');
	const QRegularExpression def_re(QStringLiteral("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*=(.*)$"));

	bool in_block_comment = false;
	bool collecting = false;
	QString def_name;
	QString def_rhs;
	int def_line = 0;
	int depth = 0; // delimiters inside the statement being collected

	for (int i = 0; i < lines.size(); i++)
	{
		const QString line_text = lines.at(i);
		const QString masked = faust2_lint_mask_line(line_text, in_block_comment);

		if (!collecting)
		{
			// Match on the ORIGINAL line (the RHS must keep its string
			// literals and comments intact), but require that the matched
			// name is real code: the masked line has everything inside
			// strings/comments blanked, so if the name position was blanked
			// the "definition" is only text inside a string or comment.
			const QRegularExpressionMatch m = def_re.match(line_text);
			if (m.hasMatch()
			    && m.captured(1) != "declare"
			    && m.captured(1) != "import"
			    && m.capturedStart(1) < masked.size()
			    && masked.at(m.capturedStart(1)) != ' ')
			{
				def_name = m.captured(1);
				def_rhs = m.captured(2);
				def_line = i + 1;
				depth = 0;
				collecting = true;
			}
		}
		else
			def_rhs += "\n" + line_text;

		if (collecting)
		{
			bool terminated = false;
			int terminator_pos = -1; // index of the terminating ';' in the current (masked) line
			for (int pos = 0; pos < masked.size(); pos++)
			{
				const QChar ch = masked.at(pos);
				if (ch == '(' || ch == '{' || ch == '[')
				  depth++;
				else if (ch == ')' || ch == '}' || ch == ']')
				  depth--;
				else if (ch == ';' && depth <= 0)
				{
					terminated = true;
					terminator_pos = pos;
					break;
				}
			}

			if (terminated)
			{
				// Cut the RHS at the terminating ';' (the same position in
				// the original text; anything after it on that line belongs
				// to the next statement and is ignored, since one definition
				// per line is assumed). The synthetic program adds its own
				// ';' after the RHS.
				const int cut = def_rhs.size() - (line_text.size() - terminator_pos);
				if (cut > 0 && cut < def_rhs.size())
				  def_rhs = def_rhs.left(cut);

				Faust2LintDef d;
				d.name = def_name;
				d.rhs = def_rhs.trimmed();
				d.line = def_line;
				defs.append(d);

				def_name.clear();
				def_rhs.clear();
				collecting = false;
				depth = 0;
			}
		}
	}

	return defs;
}

// Replaces the content of every string literal with "x", so the name
// substitution below can never match (and thereby corrupt) text inside
// strings. String contents are irrelevant for the arity check.
QString faust2_lint_sanitize_strings(const QString &rhs)
{
	QString out;
	out.reserve(rhs.size());
	const int len = rhs.size();

	for (int i = 0; i < len; i++)
	{
		const QChar c = rhs.at(i);
		if (c != '"')
		{
			out.append(c);
			continue;
		}

		i++;
		while (i < len && rhs.at(i) != '"')
		{
			if (rhs.at(i) == '\\')
			  i++;
			i++;
		}

		out.append("\"x\"");
	}

	return out;
}

// Returns the output channel count if 'rhs' is a 'soundfile("...", N)'
// declaration (N is parsed from the trailing numeric argument of the
// sanitized rhs), else -1. Used to substitute references to soundfile
// definitions with type-correct placeholders.
int faust2_lint_soundfile_channels(const QString &rhs)
{
	const QString sanitized = faust2_lint_sanitize_strings(rhs);
	const QRegularExpression re(QStringLiteral("\\bsoundfile\\s*\\([^()]*,\\s*(\\d+)\\s*\\)"));
	const QRegularExpressionMatch m = re.match(sanitized);
	if (!m.hasMatch())
	  return -1;
	return m.captured(1).toInt();
}

// True if the (sanitized) rhs is a soundfile declaration.
bool faust2_lint_is_soundfile_decl(const QString &rhs)
{
	return faust2_lint_sanitize_strings(rhs).contains(QStringLiteral("soundfile"));
}

// Replaces every reference to another top-level definition in 'rhs' with a
// 0-input hslider placeholder. All replaced names refer to 0-input
// definitions (in a valid instrument every definition is 0-input, and a
// definition with an unbound input is flagged when its own line is
// checked), so the substitution preserves the input arity of the
// expression. Names in 'soundfile_channels' are replaced by a real
// soundfile placeholder with the same channel count instead: so.sound()
// and friends require an actual soundfile signal, and the channel count
// matters for arity (a stereo file produces 2 outputs, a mono hslider
// only 1).
QString faust2_lint_substitute(const QString &rhs,
                               const QString &self_name,
                               const QSet<QString> &all_names,
                               const QHash<QString, int> &soundfile_channels)
{
	QString out = faust2_lint_sanitize_strings(rhs);
	int placeholder = 0;

	for (const QString &name : all_names)
	{
		if (name == self_name)
		  continue; // recursive references are left as-is

		// Word-bounded, not followed by '.', so module-qualified names like
		// 'ma.SR' and partial identifiers like 'freq' inside 'vib_freq' are
		// never touched.
		const QRegularExpression re(QStringLiteral("\\b%1\\b(?!\\.)").arg(name));

		if (soundfile_channels.contains(name))
		{
			// A soundfile-derived name is either passed to so.sound()/so.loop()
			// (which expect the raw soundfile signal with its auxiliary
			// length/rate outputs), or used as a plain N-channel audio signal
			// (e.g. 'wet = dry : pf.flanger_stereo(...)'). In the first case
			// the raw placeholder is correct; in the second the auxiliary
			// outputs must be stripped ((0, 0) binds the implicit part/index
			// inputs so the compiler's interval analysis accepts it).
			const QRegularExpression so_arg_re(QStringLiteral("\\bso\\.[a-zA-Z_]*\\s*\\(\\s*%1\\s*,").arg(name));
			if (so_arg_re.match(out).hasMatch())
			  out.replace(re, QString("soundfile(\"_lint[url:{'_lint.wav'}]\", %1)").arg(soundfile_channels.value(name)));
			else
			  // Parenthesized: the trailing 'si.block(2), si.bus(N)' contains a
			  // top-level comma, which would leak into an enclosing tuple.
			  out.replace(re, QString("((0, 0) : soundfile(\"_lint[url:{'_lint.wav'}]\", %1) : si.block(2), si.bus(%1))").arg(soundfile_channels.value(name)));
		}
		else
		  out.replace(re, QString("hslider(\"_l%1\", 0, 0, 1, 0.01)").arg(placeholder++));
	}

	return out;
}

}

// The LLM auto-fix lint: returns "Line N: ..." findings for the definitions
// of 'code' that are themselves ill-formed or carry unbound audio inputs.
// Called synchronously on the GUI thread, only after a compile has failed
// (no plugin compile is in flight, and libfaust serializes its factory
// creation through its own global lock anyway). Each check is a small
// interpreter-backend compile, ~10-50 ms.
QStringList FAUST2_lint_faust_code(const SoundPlugin *plugin, const QString &code)
{
	(void)plugin;

	const QList<Faust2LintDef> defs = faust2_lint_collect_defs(code);
	if (defs.isEmpty())
	  return QStringList();

	QSet<QString> all_names;
	for (const Faust2LintDef &def : defs)
	  all_names.insert(def.name);

	// Soundfile definitions and the definitions derived from them (e.g.
	// 'dry = so.sound(mysf, 0).play_interp(...)') produce N-channel signals.
	// Track them so the substitution uses a type-correct, N-channel
	// placeholder instead of a mono hslider.
	QHash<QString, int> soundfile_channels;
	for (const Faust2LintDef &def : defs)
	{
		const int channels = faust2_lint_soundfile_channels(def.rhs);
		if (channels > 0)
		  soundfile_channels.insert(def.name, channels);
	}
	// Propagate to definitions that reference a soundfile-derived name.
	for (bool changed = true; changed; )
	{
		changed = false;
		for (const Faust2LintDef &def : defs)
		{
			if (soundfile_channels.contains(def.name))
			  continue;
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
			{
				const QRegularExpression re(QStringLiteral("\\b%1\\b(?!\\.)").arg(it.key()));
				if (re.match(sanitized).hasMatch())
				{
					soundfile_channels.insert(def.name, it.value());
					changed = true;
					break;
				}
			}
		}
	}

	const QString radium_path = QCoreApplication::applicationDirPath();
	radium::ArgsCreator args;
	args.push_back("-I");
	args.push_back(radium_path + "/packages/faust/libraries");

	QStringList findings;

	// Mono effects applied directly to stereo (soundfile-derived) signals:
	// e.g. 'delay = dry : ef.echo(2.0, 0.25, 0.5)' or
	// 'left = dry : de.delay(0.1, mod1)'. The compiler error says
	// "outputs [2] ... must be equal to the number of inputs [3]" and the
	// per-line check flags the line, but nothing says WHY - so name the
	// pattern and give the recipe here. Note: no map-membership skip - a
	// def that references a stereo signal can still apply a mono effect to
	// it wrongly (that is exactly how 'left'/'right' above are broken).
	{
		static const QStringList mono_effects =
		{
			QStringLiteral("ef\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("de\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("fi\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("en\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("co\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("pf\\.flanger_mono"),
			QStringLiteral("pf\\.vibrato2_mono"),
			QStringLiteral("pf\\.phaser2_mono"),
		};

		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
			{
				const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*:\\s*(%2)\\s*\\(").arg(it.key()).arg(mono_effects.join("|")));
				const QRegularExpressionMatch m = re.match(sanitized);
				if (m.hasMatch())
				{
					// The de. module takes its signal as the LAST argument
					// (de.delay(n, d, x)); the other mono effects are
					// 1-input filters applied with par.
					if (m.captured(1).startsWith("de."))
					  findings.append(QString("Line %1: '%2' takes its signal as its last argument - write %2(..., %3) instead of '%3 : %2(...)'. Example: de.delay(0.1, mod1, dry).").arg(def.line).arg(m.captured(1)).arg(it.key()));
					else
					  findings.append(QString("Line %1: '%2' is a mono effect applied to a stereo signal - this gives an arity error. Apply it per channel: sig : par(i, 2, %2).").arg(def.line).arg(m.captured(1)));
					break;
				}
			}
		}
	}

	// A parallel composition mixing mono signals with stereo signals before
	// ro.interleave(2, 2): e.g. 'mix1 = (piano, chorus) : ro.interleave(2, 2)'
	// where piano is mono (1 channel) and chorus stereo (2 channels): 3
	// channels into a 4-input interleave is an arity error. The compiler
	// error and the per-line check flag the line, but nothing says WHY -
	// name the pattern and give the recipe. Also covers tuples whose
	// members are all mono with fewer than 4 members (2 channels into the
	// 4-input interleave), and mono signals applied to stereo effects.
	{
		// Which names are stereo: soundfile-derived 2-channel signals (the
		// propagation above), outputs of known stereo effects, and the
		// results of a pairwise (2,2) interleave mix.
		QSet<QString> stereo_names;
		for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
		  if (it.value() == 2)
		    stereo_names.insert(it.key());
		static const QStringList stereo_effects =
		{
			QStringLiteral("pf\\.flanger_stereo"),
			QStringLiteral("re\\.stereo_freeverb"),
			QStringLiteral("re\\.dattorro_rev_default"),
			QStringLiteral("re\\.satrev"),
			QStringLiteral("co\\.compressor_stereo"),
		};
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			if (sanitized.contains(QStringLiteral("ro.interleave(2, 2)"))
			    || QRegularExpression(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|"))).match(sanitized).hasMatch())
			  stereo_names.insert(def.name);
		}
		// A name is (heuristically) mono when its definition uses only mono
		// primitives (oscillators, envelopes, filters, noises, UI controls)
		// and no stereo construct, propagated through definitions that only
		// reference known-mono names ('dry = piano_sound * gain' is mono
		// because piano_sound and the gain slider are). A 'name, name'
		// tuple makes a definition non-mono (parallel composition), and so
		// does any stereo construct.
		static const QStringList mono_primitives =
		{
			QStringLiteral("\\bos\\."),
			QStringLiteral("\\ben\\."),
			QStringLiteral("\\bfi\\."),
			QStringLiteral("\\bno\\."),
			QStringLiteral("\\bhslider\\s*\\("),
			QStringLiteral("\\bvslider\\s*\\("),
			QStringLiteral("\\bnentry\\s*\\("),
			QStringLiteral("\\bbutton\\s*\\("),
			QStringLiteral("\\bcheckbox\\s*\\("),
			QStringLiteral("\\bhbargraph\\s*\\("),
			QStringLiteral("\\bvbargraph\\s*\\("),
		};
		const QRegularExpression mono_primitive_re(mono_primitives.join("|"));
		const QRegularExpression name_pair_re(QStringLiteral("\\b[a-zA-Z_][a-zA-Z0-9_]*\\s*,\\s*[a-zA-Z_][a-zA-Z0-9_]*\\b"));
		const auto has_stereo_construct = [&](const QString &sanitized) -> bool
		{
			return sanitized.contains(QStringLiteral("ro.interleave"))
			    || sanitized.contains(QStringLiteral("soundfile"))
			    || sanitized.contains(QStringLiteral("so.sound"))
			    || QRegularExpression(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|"))).match(sanitized).hasMatch();
		};
		QSet<QString> mono_names;
		for (bool changed = true; changed; )
		{
			changed = false;
			for (const Faust2LintDef &def : defs)
			{
				if (mono_names.contains(def.name) || stereo_names.contains(def.name))
				  continue;
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			if (name_pair_re.match(sanitized).hasMatch())
			  continue;
			if (has_stereo_construct(sanitized))
			  continue;
			// 'x = _;' binds the input as a plain identity: 1 channel.
			bool mono = mono_primitive_re.match(sanitized).hasMatch()
			         || sanitized.trimmed() == QStringLiteral("_");
				if (!mono)
				{
					// No mono primitive of its own: mono only if it
					// references at least one known-mono name and nothing
					// else (all references are known-mono names).
					mono = false;
					bool refs_any = false;
					for (const Faust2LintDef &other : defs)
					{
						if (other.name == def.name)
						  continue;
						const QRegularExpression ref(QStringLiteral("\\b%1\\b(?!\\.)").arg(other.name));
						if (ref.match(sanitized).hasMatch())
						{
							refs_any = true;
							if (!mono_names.contains(other.name))
							{
								mono = false;
								break;
							}
							mono = true;
						}
					}
					mono = mono && refs_any;
				}
				if (mono)
				{
					mono_names.insert(def.name);
					changed = true;
				}
			}
		}
		// A mono signal applied to a stereo effect: e.g. the synthesized
		// piano's 'chorus = dry : pf.flanger_stereo(...)' (1 channel into a
		// 2-input effect). The reverse direction (stereo into a mono
		// effect) has its own finding above.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*:\\s*(%1)\\s*\\(").arg(stereo_effects.join("|")));
			const QRegularExpressionMatch m = re.match(sanitized);
			if (m.hasMatch() && mono_names.contains(m.captured(1)))
			  findings.append(QString("Line %1: '%2' is a mono signal applied to the stereo effect %3 - this gives an arity error. Duplicate it first: (%2, %2) : %3(...).").arg(def.line).arg(m.captured(1)).arg(m.captured(2)));
		}
		// A mono effect called with a stereo argument: e.g. the chorus
		// effect's 'process = x : ef.dryWetMixer(wet, chorus)' where chorus
		// is a stereo signal. (The de. module is excluded: it takes its
		// signal as the last argument and has its own finding.)
		// ef.dryWetMixer is special-cased below: it is an N-in/N-out bus
		// mixer whose second argument must be an EFFECT FUNCTION, not a
		// signal, so the generic per-channel recipe does not apply. (The
		// model repeatedly passes the named 'wet' signal there, and then
		// "fixes" it with par(i, 2, ...), which still does not compile -
		// observed.)
		{
			static const QStringList mono_arg_effects =
			{
				QStringLiteral("ef\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("fi\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("en\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("co\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("pf\\.flanger_mono"),
				QStringLiteral("pf\\.vibrato2_mono"),
				QStringLiteral("pf\\.phaser2_mono"),
			};
			for (const Faust2LintDef &def : defs)
			{
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				{
					const QRegularExpression re(QStringLiteral("\\b(%1)\\s*\\([^()]*\\b%2\\b[^()]*\\)").arg(mono_arg_effects.join("|")).arg(*it));
					const QRegularExpressionMatch m = re.match(sanitized);
					if (m.hasMatch())
					{
						if (m.captured(1) == QStringLiteral("ef.dryWetMixer")
						    || m.captured(1) == QStringLiteral("ef.dryWetMixerConstantPower"))
						{
							// Only a problem when the argument is a SIGNAL
							// (a definition with no audio inputs). A named
							// effect (its definition contains a bare '_'
							// input binding, e.g. 'chorus = _,_ : ...') is
							// exactly what FX must be: then the call is
							// legal and nothing is reported.
							QString arg_rhs;
							for (const Faust2LintDef &other : defs)
							  if (other.name == *it)
							  {
							    arg_rhs = faust2_lint_sanitize_strings(other.rhs);
							    break;
							  }
							const QRegularExpression input_re(QStringLiteral("(^|[^a-zA-Z0-9_])_([^a-zA-Z0-9_]|$)"));
							if (!arg_rhs.isEmpty() && !input_re.match(arg_rhs).hasMatch())
							  findings.append(QString("Line %1: %2(wetAmount, FX) expects an EFFECT (a function) as its second argument, not a signal like '%3' (a signal has 0 inputs, so the compiler reports the arity mismatch). Pass the effect itself instead: %2(wetAmount, re.stereo_freeverb(0.8, 0.8, 0.3, 0.5)) - and DELETE the separate '%3' definition: %2 already passes the input through as the dry signal (do not add a dry path). Do NOT wrap it in par(i, 2, ...): that does not help here.").arg(def.line).arg(m.captured(1)).arg(*it));
						}
						else
						{
							findings.append(QString("Line %1: '%2' is a mono effect, but its argument '%3' is a stereo signal - this gives an arity error. Apply the effect per channel: sig : par(i, 2, %2(...)).").arg(def.line).arg(m.captured(1)).arg(*it));
						}
						break;
					}
				}
			}
		}
		// Tuples before ro.interleave(2, 2): the interleave needs 4 input
		// channels. Flag tuples that mix stereo and mono members (3 or
		// fewer channels) and all-mono tuples with fewer than 4 members.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("\\(([^()]*)\\)\\s*:\\s*ro\\.interleave\\(2\\s*,\\s*2\\)"));
			const QRegularExpressionMatch m = re.match(sanitized);
			if (!m.hasMatch())
			  continue;
			const QStringList members = m.captured(1).split(",", Qt::SkipEmptyParts);
			QStringList mono_members;
			bool has_stereo_member = false;
			for (const QString &member : members)
			{
				const QString name = member.trimmed();
				if (stereo_names.contains(name))
				  has_stereo_member = true;
				else if (mono_names.contains(name))
				  mono_members << name;
			}
			if (has_stereo_member && !mono_members.isEmpty())
			{
				QString dup = "(";
				for (int i = 0; i < members.size(); i++)
				{
					const QString name = members[i].trimmed();
					if (i > 0)
					  dup += ", ";
					dup += mono_members.contains(name) ? QString("(%1, %1)").arg(name) : name;
				}
				dup += ")";
				findings.append(QString("Line %1: the parallel composition (%2) mixes mono signal(s) %3 with stereo signals before ro.interleave(2, 2) - this gives an arity error. Duplicate the mono signal(s): %4 : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(m.captured(1)).arg(mono_members.join(", ")).arg(dup));
			}
			else if (!has_stereo_member && members.size() < 4 && mono_members.size() == members.size())
			{
				QString dup = "(";
				for (int i = 0; i < members.size(); i++)
				{
					const QString name = members[i].trimmed();
					if (i > 0)
					  dup += ", ";
					dup += QString("(%1, %1)").arg(name);
				}
				dup += ")";
				findings.append(QString("Line %1: every signal in (%2) is mono, so the tuple has only %3 channels - ro.interleave(2, 2) needs 4. Duplicate each one: %4 : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(m.captured(1)).arg(mono_members.size()).arg(dup));
			}
		}
		// A tuple containing a stereo signal applied to a stereo effect:
		// '(dry, dry) : pf.flanger_stereo(...)' with stereo dry is 4 channels
		// into a 2-input effect (the model over-applies the duplicate-mono
		// recipe). Name it and undo the duplication.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("\\(([^()]*)\\)\\s*:\\s*(%1)\\s*\\(").arg(stereo_effects.join("|")));
			const QRegularExpressionMatch m = re.match(sanitized);
			if (!m.hasMatch())
			  continue;
			const QStringList members = m.captured(1).split(",", Qt::SkipEmptyParts);
			for (const QString &member : members)
			{
				const QString name = member.trimmed();
				if (stereo_names.contains(name))
				{
					findings.append(QString("Line %1: (%2) has at least 4 channels but %3 takes only 2 inputs - '%4' is already stereo. Write '%4 : %3(...)' without duplicating.").arg(def.line).arg(m.captured(1)).arg(m.captured(2)).arg(name));
					break;
				}
			}
		}
		// Operators do not distribute over multi-channel signals. For every
		// operator touching a stereo-tracked name, name the line and give a
		// recipe: '*'/'/' need the per-channel scaling, '+' between two
		// stereo signals needs the pairwise mix, and '+'/'-' against a
		// constant needs the per-channel operator. The '+' inside the
		// pairwise-mix idiom itself (par(i, 2, +)) is masked out first, and
		// the two-stereo case does not require the operator to be adjacent
		// to a name: '(a : par(...)) + (b : par(...))' has the stereo names
		// behind parentheses and is just as broken.
		for (const Faust2LintDef &def : defs)
		{
			QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			sanitized.replace(QRegularExpression(QStringLiteral("par\\(i\\s*,\\s*\\d+\\s*,\\s*[+\\-]\\)")), QStringLiteral("PARSUM"));

			QString stereo_name;
			QString op;
			bool found = false;
			for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd() && !found; ++it)
			{
				const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*([+\\-*/])|([+\\-*/])\\s*\\b%1\\b").arg(*it));
				const QRegularExpressionMatch m = re.match(sanitized);
				if (m.hasMatch())
				{
					stereo_name = *it;
					op = !m.captured(1).isEmpty() ? m.captured(1) : m.captured(2);
					found = true;
				}
			}
			if (!found)
			{
				// Operator not adjacent to a stereo name: still the two-stereo
				// case when a remaining +/- operator exists anywhere and at
				// least two stereo names appear (e.g. parenthesized sums).
				if (!sanitized.contains("+") && !sanitized.contains("-"))
				  continue;
				int stereo_count = 0;
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*it)).match(sanitized).hasMatch())
				    stereo_count++;
				if (stereo_count < 2)
				  continue;
				findings.append(QString("Line %1: '+'/'-' does not distribute over stereo signals - this gives an arity error. Mix stereo signals pairwise: (a, b) : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line));
				continue;
			}
			if (op == "*" || op == "/")
			  findings.append(QString("Line %1: '%2' is a stereo signal %3 by a mono coefficient - this gives an arity error. Scale each channel instead: %2 : par(i, 2, %4).").arg(def.line).arg(stereo_name).arg(op == "*" ? "multiplied" : "divided").arg(op == "*" ? "*(x)" : "/(x)"));
			else
			{
				int stereo_count = 0;
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*it)).match(sanitized).hasMatch())
				    stereo_count++;
				if (stereo_count >= 2)
				  findings.append(QString("Line %1: '%2' does not distribute over stereo signals - this gives an arity error. Mix stereo signals pairwise: (a, b) : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(op));
				else
				  findings.append(QString("Line %1: applying '%2' to the stereo signal '%3' gives an arity error. Apply it per channel: %3 : par(i, 2, %2(x)).").arg(def.line).arg(op).arg(stereo_name));
			}
		}
	}

	// A sum of applied filter terms, e.g.
	// 'process = _,_ : par(i, 2, (_ : lp) + (_ : bp1) + ...)'. Each
	// '(_ : band)' term consumes its OWN input channel (the '+' operator
	// distributes the composition's inputs across its branches), so the
	// sum has one input per term and the arity is wrong. Fan the input to
	// all bands with the split instead: '_ <: lp, bp1, ... :> _'.
	{
		const QRegularExpression re(QStringLiteral("\\(_\\s*:\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\)\\s*\\+[^;\\n]*\\(_\\s*:\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\)"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpressionMatch m = re.match(sanitized);
			if (m.hasMatch())
			  findings.append(QString("Line %1: each '(_ : band)' term in the '+' sum consumes its own input channel - this gives an arity error. Fan one signal to all bands with the split instead: _ <: %2, %3, ... :> _.").arg(def.line).arg(m.captured(1)).arg(m.captured(2)));
		}
	}

	// re.mono_freeverb / re.stereo_freeverb derive their internal delay
	// lengths from the 'spread' argument (reverbs.lib:
	// lbcf(combtuningL(i) + spread, ...)). Passing a SMOOTHED slider there
	// hides its range from the compiler (the smoothing recursion breaks
	// range analysis) and gives an 'invalid delay parameter range' error
	// with no line number - name the line and give the recipe.
	{
		const QRegularExpression freeverb_re(QStringLiteral("re\\.(mono_freeverb|stereo_freeverb)\\s*\\(([^()]*)\\)"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpressionMatch m = freeverb_re.match(sanitized);
			if (!m.hasMatch())
			  continue;
			const QStringList args = m.captured(2).split(",", Qt::SkipEmptyParts);
			if (args.size() != 4)
			  continue;
			const QString spread_name = args[3].trimmed();
			if (!QRegularExpression(QStringLiteral("^[a-zA-Z_][a-zA-Z0-9_]*$")).match(spread_name).hasMatch())
			  continue;
			for (const Faust2LintDef &other : defs)
			{
				if (other.name != spread_name)
				  continue;
				const QString rhs = faust2_lint_sanitize_strings(other.rhs);
				if (rhs.contains(QStringLiteral("si.smooth")) || rhs.contains(QStringLiteral("ba.tau2pole")) || rhs.contains(QStringLiteral("si.smoo")))
				  findings.append(QString("Line %1: '%2' is a smoothed slider passed as the 'spread' argument of %3, and its delay lengths depend on spread - the smoothing hides the range from the compiler and gives an 'invalid delay parameter range' error. Leave that slider unsmoothed ('%2 = hslider(...);' without si.smooth).").arg(def.line).arg(spread_name).arg(m.captured(1)));
				break;
			}
		}
	}

	for (const Faust2LintDef &def : defs)
	{
		// A soundfile declaration cannot be checked standalone: a bare
		// 'process = soundfile(...)' is not a valid program (the compiler
		// rejects the unbound part number), so it would always be flagged
		// even though the line is fine in context.
		if (faust2_lint_is_soundfile_decl(def.rhs))
		  continue;

		const QString substituted = faust2_lint_substitute(def.rhs, def.name, all_names, soundfile_channels);

		const QString synthetic =
		  "import(\"stdfaust.lib\");\n"
		  "process = " + substituted + ";\n";

		std::string error_msg;
		interpreter_dsp_factory *factory =
		  createInterpreterDSPFactoryFromString("FaustDev2Lint",
		                                        synthetic.toUtf8().constData(),
		                                        args.get_argc(),
		                                        args.get_argv(),
		                                        error_msg);

		if (factory == NULL)
		{
			printf("LLM lint: line %d (%s): expression does not compile on its own: %s\n",
			       def.line, def.name.toUtf8().constData(), error_msg.c_str());
			findings.append(QString("Line %1: the definition '%2' does not compile on its own, so it is part of the compile error.")
			                .arg(def.line).arg(def.name));
		}
		else
		{
			dsp *dsp = factory->createDSPInstance();
			const int num_inputs = (dsp != NULL) ? dsp->getNumInputs() : 0;
			delete dsp;
			deleteInterpreterDSPFactory(factory);

			if (num_inputs > 0)
			{
				printf("LLM lint: line %d (%s): expression has %d unbound audio input(s)\n",
				       def.line, def.name.toUtf8().constData(), num_inputs);
				findings.append(QString("Line %1: the definition '%2' has %3 unbound audio input channel(s): a filter/smoother is probably used as a plain value instead of being applied to a signal with ':'.")
				                .arg(def.line).arg(def.name).arg(num_inputs));

				// If another definition references this unbound-input
				// definition BARE (not applied with ':'), name that line
				// too and give the recipe: the model writes
				// 'lp = fi.lowpass(2, fc) : *(...)' and then sums it bare
				// ('process = _,_ : par(i, 2, lp + bp1 + ...)'), which
				// reproduces exactly this unbound input.
				for (const Faust2LintDef &user : defs)
				{
					if (user.name == def.name)
					  continue;
					const QString sanitized = faust2_lint_sanitize_strings(user.rhs);
					const QRegularExpression re(QStringLiteral("(?<!:\\s)\\b%1\\b").arg(def.name));
					if (re.match(sanitized).hasMatch())
					{
						if (num_inputs == 1)
						  findings.append(QString("Line %1: '%2' has an audio input but is referenced as a plain value - apply it to the signal with ':': (_ : %2), or, when summing several filters, fan the input to them with the split: _ <: f1, f2, ... :> _.").arg(user.line).arg(def.name));
						else
						  findings.append(QString("Line %1: '%2' has %3 unbound audio input channels and is referenced as a plain value - a bare reference consumes %3 of the process's input channels; the total over ALL bare references must equal the process input count.").arg(user.line).arg(def.name).arg(num_inputs));
						break;
					}
				}
			}
		}

		if (findings.size() >= 8)
		  break;
	}

	return findings;
}


static bool show_gui(SoundPlugin *plugin, int64_t parentgui)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	if (devdata->qtgui_parent.data() == NULL || devdata->qtgui == NULL)
		return false;

	QWidget *parent = API_gui_get_parentwidget(NULL, parentgui);
	if (parent != NULL)
		set_window_parent(devdata->qtgui_parent.data(), parent, radium::NOT_MODAL, ShowAssertionOrThrowAPIException::SHOW_ASSERTION);

	devdata->qtgui->update();
	safeShow(devdata->qtgui_parent.data());
	devdata->qtgui->run();

	return true;
}

static void hide_gui(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	if (devdata->qtgui_parent.data() != NULL)
		devdata->qtgui_parent->hide();

	if (devdata->qtgui != NULL)
		devdata->qtgui->stop();
}

static bool gui_is_visible(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;

	if (devdata->qtgui_parent.data() == NULL)
		return false;

	return devdata->qtgui_parent->isVisible();
}


static void RT_player_is_stopped(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	FaustDev2Dsp *dsp_data = devdata->dsp_data;

	if (dsp_data != NULL && dsp_data->is_instrument && dsp_data->poly_dsp != NULL)
	{
		dsp_data->poly_dsp->allNotesOff(false);
		dsp_data->collector.clear();
		dsp_data->num_note_voices = 0;
	}
}


//===========================================
// Registration
//===========================================

void create_faust_dev2_plugin(void)
{
	// Make the Faust compiler thread safe by activating its internal global lock.
	// Without this, concurrent compilations (multiple Dev2CompileThreads) race on
	// Faust's single, non-thread-local 'gGlobal' pointer and crash.
	// Idempotent: only creates the lock the first time it is called.
	startMTDSPFactories();

	SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1, sizeof(SoundPluginType));

	plugin_type->type_name                = "Faust Dev 2";
	plugin_type->name                     = "Faust Dev 2";
	plugin_type->num_inputs               = MAX_CHANNELS;
	plugin_type->num_outputs              = MAX_CHANNELS;
	plugin_type->is_instrument            = true;
	plugin_type->note_handling_is_RT      = false;
	plugin_type->num_effects              = MAX_EFFECTS;
	plugin_type->get_effect_format        = get_effect_format;
	plugin_type->get_effect_name          = get_effect_name;
	plugin_type->effect_is_RT             = NULL;
	plugin_type->create_state             = create_state;
	plugin_type->create_plugin_data       = create_plugin_data;
	plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
	plugin_type->show_gui                 = show_gui;
	plugin_type->hide_gui                 = hide_gui;
	plugin_type->gui_is_visible           = gui_is_visible;
	plugin_type->RT_player_is_stopped     = RT_player_is_stopped;

	plugin_type->RT_process       = RT_process;
	plugin_type->play_note        = play_note;
	plugin_type->set_note_volume  = set_note_volume;
	plugin_type->set_note_pitch   = set_note_pitch;
	plugin_type->stop_note        = stop_note;
	plugin_type->set_effect_value = set_effect_value;
	plugin_type->get_effect_value = get_effect_value;
	plugin_type->get_display_value_string = get_display_value_string;
	plugin_type->get_effect_description   = get_effect_description;
	plugin_type->effect_is_visible = effect_is_visible;

	plugin_type->info =
		"HTML: FAUST (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards. More info <A href=\"http://faust.grame.fr\">here</a>."
		"<p>"
		"Faust Dev 2 is a development instrument for writing and testing Faust programs in real time."
		"<UL>"
		"<LI> It uses a built-in polyphonic voice manager for automatic voice management, so instruments are polyphonic without any special coding. Microtonal notes (cents) are supported."
		"<LI> Polyphony is automatic: instruments get 128 voices."
		"<LI> Notes are triggered with sub-block accuracy for precise timing."
		"<LI> When the program defines all three note controls <code>freq</code>, <code>gain</code> and <code>gate</code>, they are handled automatically by note events and hidden from the GUI (the same applies to <code>velocity</code> and the built-in Panic button). A program defining just one or two of them, e.g. a filter effect with a <code>freq</code> slider, keeps its sliders in the GUI."
		"</UL>"
		"<p>"
		"Hints:\n"
		"<UL>"
		"<LI> To zoom, either the editor or a diagram, press CTRL while scrolling the mouse wheel."
		"<LI> To search for a string in the source code, press Ctrl + F."
		"<LI> Running full size window (by pressing the \"Full\" button) can be very convenient when developing."
		"</UL"
		;

	PR_add_plugin_type(plugin_type);

	PR_add_menu_entry(PluginMenuEntry::level_up("FaustDev examples"));
	{
		PR_add_load_preset_menu_entries_in_directory(OS_get_full_program_file_path("faustdev_examples"));
	}
	PR_add_menu_entry(PluginMenuEntry::level_down());
}
