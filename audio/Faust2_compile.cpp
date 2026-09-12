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
#include <string.h>
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
#include "../bin/packages/faust/architecture/faust/dsp/poly-dsp.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust/compiler/generator/libfaust.h"
#undef uchar
#undef uint

#include "../bin/packages/faust/architecture/faust/gui/UI.h"
#include "../bin/packages/faust/architecture/faust/gui/MidiUI.h"
#include "../bin/packages/faust/architecture/faust/gui/Soundfile.h"
#include "../bin/packages/faust/architecture/faust/gui/LibsndfileReader.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#pragma GCC diagnostic pop

#include <sndfile.h>

#include <QCoreApplication>
#include <QDir>
#include <QString>
#include <QStringList>

#include "Faust2_compile.hpp"

FaustDev2SoundfileData::~FaustDev2SoundfileData()
{
	for (Soundfile *sf : owned)
		delete sf;
}

namespace{

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

static Faust2SndfileOpenFunc g_sndfile_open_func = NULL;
static Faust2SoundfileWarningFunc g_soundfile_warning_func = NULL;

static SNDFILE *open_sndfile(const std::string &path_name, SF_INFO *snd_info)
{
	if (g_sndfile_open_func != NULL)
		return g_sndfile_open_func(path_name.c_str(), SFM_READ, snd_info);
	return sf_open(path_name.c_str(), SFM_READ, snd_info);
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


// LibsndfileReader that opens files through an injected opener (Radium's
// radium_sf_open, which handles non-ASCII paths on Windows), falling back to
// libsndfile's sf_open. All decoding logic is delegated to the upstream *Aux
// helpers, so this reader keeps LibsndfileReader's features: chunked reads,
// 'is_double' support, and the '_SAMPLERATE' resampling if that build flag is
// ever enabled (it is not: the so. module already handles sample-rate
// differences via its read step srate(sf,part)/ma.SR, and resampling will be
// investigated separately).
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
		SNDFILE *snd_file = open_sndfile(path_name, &snd_info);
		return checkFileAux(snd_file, path_name);
	}

	// Get the number of channels and the length in frames.
	void getParamsFile(const std::string &path_name, int &channels, int &length) override
	{
		SF_INFO snd_info;
		memset(&snd_info, 0, sizeof(snd_info));
		SNDFILE *snd_file = open_sndfile(path_name, &snd_info);
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
		SNDFILE *snd_file = open_sndfile(path_name, &snd_info);
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
			const QString filename = QString::fromStdString(filenames[i]);
			if (g_soundfile_warning_func != NULL)
				g_soundfile_warning_func(filename);
			else
				fprintf(stderr, "Faust Dev 2: Could not load soundfile '%s'. Replaced with silence.\n", filenames[i].c_str());
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

} // anonymous namespace


void FAUST2_set_sndfile_open_func(Faust2SndfileOpenFunc func)
{
	g_sndfile_open_func = func;
}

void FAUST2_set_soundfile_warning_func(Faust2SoundfileWarningFunc func)
{
	g_soundfile_warning_func = func;
}


void delete_factory(
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


dsp_factory *create_factory(const FaustDev2CompileConfig &config,
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
	QStringList args_list = config.options.split("\n", Qt::SkipEmptyParts);

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

	QString radium_path = config.radium_path.isEmpty() ? QCoreApplication::applicationDirPath() : config.radium_path;

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

	if (config.use_interpreter_backend){
		interp_factory = createInterpreterDSPFactoryFromString("FaustDev2", config.code.toUtf8().constData(), argc, argv, error_msg);
		factory = interp_factory;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	}else{
		llvm_factory = createDSPFactoryFromString("FaustDev2", config.code.toUtf8().constData(), argc, argv,
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


bool FAUST2_compile_check(const FaustDev2CompileConfig &config,
                          int optlevel,
                          Faust2CompileCheckResult *result)
{
	*result = Faust2CompileCheckResult();

	QString error_message;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	llvm_dsp_factory *llvm_factory = NULL;
#endif
	interpreter_dsp_factory *interp_factory = NULL;
	MyQTemporaryDir *svg_dir = NULL;
	FaustDev2SoundfileData *soundfile_data = NULL;

	dsp_factory *factory = create_factory(config, optlevel, error_message,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
										  &llvm_factory,
#endif
										  &interp_factory,
										  &svg_dir,
										  &soundfile_data);

	if (factory == NULL)
	{
		result->ok = false;
		result->error_message = error_message;
		delete svg_dir;
		delete soundfile_data;
		return false;
	}

	dsp *test_dsp = factory->createDSPInstance();
	if (test_dsp != NULL)
	{
		result->num_inputs = test_dsp->getNumInputs();
		result->num_outputs = test_dsp->getNumOutputs();
		result->is_instrument = MidiMeta::checkPolyphony(test_dsp);
		delete test_dsp;
	}

	result->ok = true;

	delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
				   llvm_factory,
#endif
				   interp_factory);
	delete svg_dir;
	delete soundfile_data;

	return true;
}
