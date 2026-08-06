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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#pragma GCC diagnostic ignored "-Wcast-function-type-mismatch"
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
#include "../bin/packages/faust/architecture/faust/gui/APIUI.h"
#include "../bin/packages/faust/architecture/faust/gui/MidiUI.h"

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
#include <QtConcurrent>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <faust/gui/QTUI.h>
#pragma GCC diagnostic pop

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/patch_proc.h"
#include "../common/disk.h"

#include "../common/ArgsCreator.hpp"

#include "../api/api_proc.h"
#include "../api/api_gui_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Juce_plugins_proc.h"
#include "Mixer_proc.h"

#include "Faust_plugins_proc.h"

#include "../Qt/MyQTemporaryDir.hpp"
#include "../Qt/helpers.h"

#include "SubBlockNoteCollector.h"


#define MAX_CHANNELS 16
#define MAX_EFFECTS 1024
#define MAX_POLYPHONY 32

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
	"declare options \"[nvoices:8]\";\n"
	"freq = hslider(\"freq\", 440, 20, 20000, 0.01);\n"
	"gain = hslider(\"gain\", 0.5, 0, 1, 0.01);\n"
	"gate = button(\"gate\");\n"
	"process = os.sawtooth(freq) * gain * gate <: _,_;\n";


namespace{

struct NoteVoice
{
	int64_t note_id;
	const struct SeqBlock *seqblock;
	int pitch;
	dsp_voice *voice;
};

struct FaustDev2Dsp
{
	mydsp_poly *poly_dsp;     // owns the voice DSPs; NULL for effects
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
	SoundPlugin *plugin;
	int effect_num;
};


struct FaustDev2Data
{
	QString code;
	QString options;

#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
	bool use_interpreter_backend = true;
#else
	bool use_interpreter_backend = false;
#endif

	FaustDev2Dsp *dsp_data;   // NULL until compiled
	bool is_compiling;
	QString error_message;
	QPointer<QDialog> qtgui_parent;
	QTGUI *qtgui;
	std::vector<Faust2GuiControlRef*> qtgui_control_refs; // owned; one per visible control
	radium::FAUST_calledRegularlyByParentReply ready;
	MyQTemporaryDir *svg_dir; // owns SVG output directory

	FaustDev2Data()
		: options("-I\n%radium_path%/packages/faust/libraries")
		, dsp_data(NULL)
		, is_compiling(false)
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
	if (ref == NULL || ref->plugin == NULL || PLUGIN_exists(ref->plugin) == false)
		return;

	SoundPlugin *plugin = ref->plugin;
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
									  float sample_rate)
{

	dsp *mono_dsp = factory->createDSPInstance();
	if (mono_dsp == NULL){
		RWarning("createDSPInstance returned NULL in FaustDev2");
		return NULL;
	}

	dsp *final_dsp;
	mydsp_poly *poly_dsp = NULL;
	bool is_instrument;

	// Detect polyphony from metadata or naming convention
	bool midi = false, midi_sync = false;
	int nvoices = 0;
	MidiMeta::analyse(mono_dsp, midi, midi_sync, nvoices);

	if (nvoices <= 0 && MidiMeta::checkPolyphony(mono_dsp))
		nvoices = MAX_POLYPHONY;

	if (nvoices > 0){
		poly_dsp = new mydsp_poly(mono_dsp, nvoices, true, true);
		final_dsp = poly_dsp;
		is_instrument = true;
	}else{
		poly_dsp = NULL;
		final_dsp = mono_dsp;
		is_instrument = false;
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


static void hotswap_dsp_data(FaustDev2Data *devdata, FaustDev2Dsp *new_dsp)
{
	FaustDev2Dsp *old_dsp = devdata->dsp_data;

	// Preserve parameter values from old DSP to new DSP by matching names.
	// Read from the live control zones (not the cached param_values), so values
	// changed directly in the QTGUI dialog are also carried over.
	if (old_dsp != NULL && new_dsp != NULL){
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
		devdata->dsp_data = new_dsp;
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


static dsp_factory *create_factory(const FaustDev2Data *devdata,
									int optlevel,
									QString &error_message,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
									llvm_dsp_factory **out_llvm_factory,
#endif
									interpreter_dsp_factory **out_interp_factory,
									MyQTemporaryDir **out_svg_dir)
{
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
	delete test_dsp;

	if (num_inputs > MAX_CHANNELS){
		if (interp_factory)
			deleteInterpreterDSPFactory(interp_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm_factory)
			deleteDSPFactory(llvm_factory);
#endif
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
		error_message = QString("Maximum %1 output channels supported (%2)").arg(MAX_CHANNELS).arg(num_outputs);
		return NULL;
	}

#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	*out_llvm_factory = llvm_factory;
#endif
	*out_interp_factory = interp_factory;

	return factory;
}


} // end anonymous namespace (split for Dev2CompileThread)


static void start_compilation(SoundPlugin *plugin);
static bool effect_is_visible(SoundPlugin *plugin, int effect_num);


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
											  &svg_dir);

		if (factory == NULL)
		{
			
			delete svg_dir;

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
											svg_dir,
											compile_code = _code]()
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
				start_compilation(plugin); // Recompile the latest code.
				return;
			}

			// Create the DSP on the main thread. mydsp_poly's GroupUI registers
			// itself in Faust's process-global GUI list (GUI.h), which the main
			// thread iterates in GUI::updateAllGuis (from QTGUI timers), so
			// constructing it on the compile thread raced with that iteration.
			FaustDev2Dsp *dsp_data = create_dsp_data(factory,
													 factory,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
													 llvm_factory,
#endif
													 interp_factory,
													 MIXER_get_sample_rate());

			if (dsp_data == NULL){
				delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
							   llvm_factory,
#endif
							   interp_factory);
				
				delete svg_dir;
				
				return;
			}

			devdata->error_message = "";

			// Store SVG dir
			if (devdata->svg_dir != NULL)
				delete devdata->svg_dir;
			
			devdata->svg_dir = svg_dir;

			hotswap_dsp_data(devdata, dsp_data);

			if (dsp_data->is_instrument)
				plugin->type->is_instrument = true;
			else
				plugin->type->is_instrument = false;

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
			// (the QTGUI widget constructors write fCur back into the zone), so
			// re-apply the preserved parameter values. The qtgui->update() call
			// below (GUI::updateAllGuis) then dispatches them to the polyphonic
			// voices and to the recreated interface.
			for (int i = 0; i < dsp_data->num_params; i++)
				dsp_data->api_ui.setParamValue(i, dsp_data->param_values[i]);

			// Route GUI-dialog control changes through set_effect_value, so they
			// update param_values / stored values and survive recompiles.
			for (int i = 0; i < dsp_data->num_params; i++)
			{
				if (effect_is_visible(plugin, i) == false)
					continue;
				
				Faust2GuiControlRef *ref = new Faust2GuiControlRef;
				
				ref->plugin = plugin;
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


// The player lock must be held when calling this function.
static void register_note_voice(FaustDev2Dsp *dsp_data, const note_t &note, int pitch, dsp_voice *voice)
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
static bool release_note_voice(FaustDev2Dsp *dsp_data, const note_t &note, int pitch)
{
	for (int i = 0; i < dsp_data->num_note_voices; i++)
	{
		NoteVoice &nv = dsp_data->note_voices[i];
		if (nv.pitch == pitch && is_note(note, nv.note_id, nv.seqblock))
		{
			dsp_voice *voice = nv.voice;

			dsp_data->note_voices[i] = dsp_data->note_voices[dsp_data->num_note_voices-1];
			dsp_data->num_note_voices--;

			// Sanity check that the voice is still playing (or is about to
			// play, in legato mode) this pitch. If it was stolen, the note is
			// already gone, and releasing the voice would kill the note that
			// stole it.
			if (voice->fCurNote == pitch || (voice->fCurNote == kLegatoVoice && voice->fNextNote == pitch))
				voice->keyOff();

			return true;
		}
	}

	return false;
}


// mydsp_poly's voice mixer uses fixed internal buffers of MIX_BUFFER_SIZE
// frames (poly-dsp.h), so a compute longer than that overflows the heap
// buffers. Split the compute into chunks that fit. (Block sizes up to 8192
// are selectable in the preferences.)
static constexpr int MAX_POLY_COMPUTE_FRAMES = MIX_BUFFER_SIZE;

static void compute_poly_chunked(mydsp_poly *poly_dsp,
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

			if (ev.is_note_on)
			{
				int pitch = (int)(ev.note.pitch + 0.5f);
				float gain = velocity2gain(ev.note.velocity);
				int vel = R_BOUNDARIES(0, (int)(gain * 127), 127);
				MapUI *voice = dsp_data->poly_dsp->keyOn(0, pitch, vel);
				if (voice != NULL)
					register_note_voice(dsp_data, ev.note, pitch, static_cast<dsp_voice*>(voice));
			}
			else
			{
				int pitch = (int)(ev.note.pitch + 0.5f);
				
				if (release_note_voice(dsp_data, ev.note, pitch) == false)
				{
					// No registered voice for this note (for instance because its
					// keyOn event was dropped, or the voice was stolen and reused).
					// Only fall back to the pitch-based keyOff if no other
					// registered note uses the same pitch, since mydsp_poly's
					// keyOff releases the *oldest* voice with that pitch, which
					// would release the wrong note when notes overlap on one pitch.
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

	// Clean unused channels.
	for (int ch = num_outputs; ch < MAX_CHANNELS; ch++)
		memset(outputs[ch], 0, num_frames * sizeof(float));
}


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
	// Not currently implemented for mydsp_poly.
	(void)plugin;
	(void)block_delta_time;
	(void)note;
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
	// refreshed (see mydsp_poly/GroupUI in poly-dsp.h). Without this the
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

	// Hide note-control parameters (freq/key, gain/vel/velocity, gate) since they are managed by note events.
	// Also hide the Panic button added by mydsp_poly.
	const char *addr = dsp_data->api_ui.getParamAddress(effect_num);
	std::string path(addr);

	if (MapUI::endsWith(path, "/freq")
	    || MapUI::endsWith(path, "/key")
	    || MapUI::endsWith(path, "/gain")
	    || MapUI::endsWith(path, "/vel")
	    || MapUI::endsWith(path, "/velocity")
	    || MapUI::endsWith(path, "/gate")
	    || MapUI::endsWith(path, "/Panic"))
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
			devdata->use_interpreter_backend = HASH_get_bool(state, "use_interpreter_backend");
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

		dsp_factory *factory = create_factory(devdata, getFaustOptimizationLevel(), error_message, // main thread, safe to read settings
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
											   &llvm_factory,
#endif
											   &interp_factory,
											   &svg_dir);

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
													  sample_rate);
			if (dsp_data != NULL){
				hotswap_dsp_data(devdata, dsp_data);
			}
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
	devdata->use_interpreter_backend = use_interpreter;
	return true;
#endif
}

bool FAUST2_get_use_interpreter_backend(SoundPlugin *plugin)
{
	FaustDev2Data *devdata = (FaustDev2Data*)plugin->data;
	return devdata->use_interpreter_backend;
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
		"<LI> It uses Faust's built-in polyphonic architecture (mydsp_poly) for automatic voice management, so instruments are polyphonic without any special coding."
		"<LI> Set the number of voices with <code>declare options \"[nvoices:N]\"</code> in the Faust code (up to 32 voices)."
		"<LI> Notes are triggered with sub-block accuracy for precise timing."
		"<LI> The note controls <code>freq</code>, <code>gain</code>, <code>gate</code> and <code>velocity</code>, as well as the built-in Panic button, are handled automatically and hidden from the GUI."
		"</UL>"
		"<p>"
		"Hints:\n"
		"<UL>"
		"<LI> An LLM prompt (beta feature) can generate or fix the Faust code for you (e.g. \"Create a low-shelf filter\"). Enable it via <b>Help -> Beta features -> \"Show/hide Faust Dev 2 LLM prompt\"</b>, then configure your LLM provider (API key, model) with the \"LLM settings\" button in the prompt bar. "
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
