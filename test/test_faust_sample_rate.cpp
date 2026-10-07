#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

#if defined(__SSE__)
#  include <pmmintrin.h>
#  include <xmmintrin.h>
#endif

#if defined(__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 33))
#  define FAUST_BENCH_HAS_MALLINFO2 1
#endif

#if defined(FAUST_BENCH_HAS_MALLINFO2)
#  include <malloc.h>
#endif

#include "../bin/packages/faust/architecture/faust/dsp/dsp.h"
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
#  include "../bin/packages/faust/architecture/faust/dsp/llvm-dsp.h"
#endif
#include "../bin/packages/faust/architecture/faust/dsp/interpreter-dsp.h"

namespace
{

struct BenchSettings
{
	int sample_rate = 48000;
	int frames = 256;
	int repetitions = 2;
	double seconds = 1.0;
	std::string libraries = "bin/packages/faust/libraries";
	std::string only_case;
	std::string only_backend;
	std::string only_mode;
	std::string code_file;
	bool svg = false;
	std::vector<int> voices = {1, 16, 128};
};

struct BenchCase
{
	std::string name;
	std::string code;
};

struct BenchResult
{
	bool ok = false;
	double ns_per_frame_per_voice = 0;
	double realtime = 0;
	long long heap_bytes = 0;
	double checksum = 0;
};

struct Factory
{
	dsp_factory *f = NULL;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	llvm_dsp_factory *llvm = NULL;
#endif
	interpreter_dsp_factory *interp = NULL;

	void destroy(void)
	{
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
		if (llvm != NULL)
		{
			deleteDSPFactory(llvm);
			llvm = NULL;
		}
#endif
		if (interp != NULL)
		{
			deleteInterpreterDSPFactory(interp);
			interp = NULL;
		}
		f = NULL;
	}
};

static const char *g_default_synth_code =
	"import(\"stdfaust.lib\");\n"
	"freq = hslider(\"freq\", 440, 20, 20000, 0.01);\n"
	"gain = hslider(\"gain\", 0.25, 0, 1, 0.01);\n"
	"volume = hslider(\"volume\", 0, -60, 12, 0.1) : ba.db2linear : si.smooth(ba.tau2pole(0.010));\n"
	"gate = button(\"gate\");\n"
	"attack = hslider(\"attack\", 0.01, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"decay = hslider(\"decay\", 0.2, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"sustain = hslider(\"sustain\", 0.7, 0, 1, 0.01) : si.smooth(ba.tau2pole(0.010));\n"
	"release = hslider(\"release\", 0.4, 0.001, 5, 0.001) : si.smooth(ba.tau2pole(0.010));\n"
	"envelope = en.adsr(attack, decay, sustain, release, gate);\n"
	"process = os.osc(freq) * gain * envelope * volume <: _,_;\n";

static const BenchCase g_cases[] =
{
	{ "delay",
	  "import(\"stdfaust.lib\");\n"
	  "process = de.delay(int(ma.SR), int(ma.SR)/2, _);\n" },
	{ "onepoles",
	  "import(\"stdfaust.lib\");\n"
	  "a1 = exp(-2.0*ma.PI*1000.0/ma.SR);\n"
	  "a2 = exp(-2.0*ma.PI*5000.0/ma.SR);\n"
	  "a3 = exp(-2.0*ma.PI*200.0/ma.SR);\n"
	  "process = _ : (+~*(a1)) : (+~*(a2)) : (+~*(a3));\n" },
	{ "freeverb", "import(\"stdfaust.lib\");\nprocess = re.mono_freeverb(0.5, 0.5, 0.5, 0.5);\n" },
	{ "default_synth", g_default_synth_code },
};

static double now_seconds(void)
{
	return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

static size_t heap_used(void)
{
#if defined(FAUST_BENCH_HAS_MALLINFO2)
	struct mallinfo2 info = mallinfo2();
	return info.uordblks + info.hblkhd;
#else
	return 0;
#endif
}

#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
static bool host_is_llvm_target(void)
{
#if defined(__linux__) && defined(__x86_64__)
	return true;
#else
	return false;
#endif
}
#endif

static std::string create_sr_overlay(const std::string &libraries_dir, int sample_rate, std::string &error)
{
	const std::string source = libraries_dir + "/platform.lib";
	std::ifstream in(source.c_str());
	if (!in.is_open())
	{
		error = "could not open " + source;
		return "";
	}

	std::vector<std::string> lines;
	std::string line;
	bool replaced = false;
	while (std::getline(in, line))
	{
		if (!replaced && line.rfind("SR = ", 0) == 0)
		{
			lines.push_back("SR = " + std::to_string(sample_rate) + ";");
			replaced = true;
		}
		else
		{
			lines.push_back(line);
		}
	}

	if (!replaced)
	{
		error = "could not find an 'SR = ' line in " + source;
		return "";
	}

	std::filesystem::path dir = std::filesystem::temp_directory_path() / ("radium_faust_sr_bench_" + std::to_string(sample_rate));
	std::error_code ec;
	std::filesystem::create_directories(dir, ec);

	std::ofstream out((dir / "platform.lib").string().c_str(), std::ios::trunc);
	if (!out.is_open())
	{
		error = "could not write " + (dir / "platform.lib").string();
		return "";
	}
	for (const std::string &l : lines)
		out << l << "\n";

	return dir.string();
}

static Factory compile_program(const std::string &code, const std::vector<std::string> &options, bool use_llvm, int opt_level, std::string &error)
{
	Factory result;

	std::vector<const char*> argv;
	argv.reserve(options.size());
	for (const std::string &option : options)
		argv.push_back(option.c_str());

#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	if (use_llvm)
	{
		const char *target = host_is_llvm_target() ? "x86_64-pc-linux-gnu" : "";
		result.llvm = createDSPFactoryFromString("bench", code, (int)argv.size(), argv.data(), target, error, opt_level);
		result.f = result.llvm;
		return result;
	}
#endif

	result.interp = createInterpreterDSPFactoryFromString("bench", code, (int)argv.size(), argv.data(), error);
	result.f = result.interp;
	return result;
}

static void fill_noise(std::vector<FAUSTFLOAT> &buffer)
{
	uint32_t state = 12345;
	for (FAUSTFLOAT &value : buffer)
	{
		state = state * 1103515245u + 12345u;
		value = (FAUSTFLOAT)(((double)((state >> 8) & 0xffff) / 32768.0 - 1.0) * 0.5);
	}
}

static long long run_for(dsp **dsps, int num_voices, int frames, FAUSTFLOAT **inputs, FAUSTFLOAT **outputs, double seconds, long long min_frames)
{
	long long frames_done = 0;
	double start = now_seconds();
	do
	{
		for (int v = 0 ; v < num_voices ; v++)
			dsps[v]->compute(frames, inputs, outputs);
		frames_done += frames;
	}
	while (frames_done < min_frames || now_seconds() - start < seconds);
	return frames_done;
}

static BenchResult measure_factory(dsp_factory *factory, const BenchSettings &settings, int num_voices)
{
	BenchResult result;

	size_t heap_before = heap_used();

	std::vector<dsp*> dsps;
	dsps.reserve(num_voices);
	for (int i = 0 ; i < num_voices ; i++)
	{
		dsp *d = factory->createDSPInstance();
		if (d == NULL)
		{
			fprintf(stderr, "ERROR: createDSPInstance returned NULL\n");
			for (dsp *created : dsps)
				delete created;
			return result;
		}
		d->init(settings.sample_rate);
		dsps.push_back(d);
	}

	size_t heap_after = heap_used();
	result.heap_bytes = (long long)(heap_after > heap_before ? heap_after - heap_before : 0);

	const int num_inputs = dsps[0]->getNumInputs();
	const int num_outputs = dsps[0]->getNumOutputs();
	const int frames = settings.frames;

	std::vector<FAUSTFLOAT> input((size_t)num_inputs * frames);
	std::vector<FAUSTFLOAT> output((size_t)num_outputs * frames);
	fill_noise(input);

	std::vector<FAUSTFLOAT*> ins(num_inputs);
	std::vector<FAUSTFLOAT*> outs(num_outputs);
	for (int i = 0 ; i < num_inputs ; i++)
		ins[i] = input.data() + (size_t)i * frames;
	for (int i = 0 ; i < num_outputs ; i++)
		outs[i] = output.data() + (size_t)i * frames;

	for (int i = 0 ; i < 200 ; i++)
		dsps[0]->compute(frames, ins.data(), outs.data());
	dsps[0]->compute(frames, ins.data(), outs.data());
	result.checksum = 0.0;
	for (FAUSTFLOAT value : output)
		result.checksum += value;

	run_for(dsps.data(), num_voices, frames, ins.data(), outs.data(), 0.1, 0);

	double best_ns = 0.0;
	for (int rep = 0 ; rep < settings.repetitions ; rep++)
	{
		double start = now_seconds();
		long long frames_done = run_for(dsps.data(), num_voices, frames, ins.data(), outs.data(), settings.seconds, (long long)frames * 32);
		double elapsed = now_seconds() - start;
		double ns = elapsed / ((double)frames_done * num_voices) * 1e9;
		if (rep == 0 || ns < best_ns)
			best_ns = ns;
	}

	double ns_total = best_ns * num_voices;
	result.ns_per_frame_per_voice = best_ns;
	result.realtime = 1.0 / (ns_total * 1e-9 * settings.sample_rate);
	result.ok = true;

	for (dsp *d : dsps)
		delete d;

	return result;
}

static void print_header(void)
{
	printf("%-16s %-7s %-8s %6s %12s %10s %10s %14s %9s\n",
	       "case", "backend", "sr-mode", "voices", "ns/fr/voice", "realtime", "heap-MB", "checksum", "compile-ms");
}

static void print_result(const BenchCase &bench_case, const char *backend, const char *mode, int num_voices, const BenchResult &result, double compile_ms)
{
	printf("%-16s %-7s %-8s %6d %12.1f %10.4f %10.2f %14.6f %9.1f\n",
	       bench_case.name.c_str(), backend, mode, num_voices,
	       result.ns_per_frame_per_voice, result.realtime,
	       (double)result.heap_bytes / (1024.0 * 1024.0), result.checksum, compile_ms);
}

static void usage(const char *program)
{
	printf("Usage: %s [options]\n", program);
	printf("  --sr <n>               sample rate (default 48000)\n");
	printf("  --seconds <f>          timed seconds per repetition (default 1.0)\n");
	printf("  --reps <n>             timed repetitions, best is reported (default 2)\n");
	printf("  --frames <n>           frames per compute call (default 256)\n");
	printf("  --voices <n[,n...]>    voice counts (default 1,16,128)\n");
	printf("  --libraries <dir>      Faust libraries dir (default bin/packages/faust/libraries)\n");
	printf("  --case <substr>        only run cases whose name contains <substr>\n");
	printf("  --code-file <file>     run a single DSP read from <file> instead of the built-in cases\n");
	printf("  --backend <llvm|interp>  only run one backend\n");
	printf("  --mode <runtime|constant>  only run one sample-rate mode\n");
	printf("  --svg                  also generate SVG block diagrams while compiling (like FaustDev2 does)\n");
}

static bool parse_voices(const char *text, std::vector<int> &voices)
{
	voices.clear();
	std::string s(text);
	size_t pos = 0;
	while (pos <= s.size())
	{
		size_t comma = s.find(',', pos);
		if (comma == std::string::npos)
			comma = s.size();
		std::string part = s.substr(pos, comma - pos);
		if (!part.empty())
			voices.push_back(atoi(part.c_str()));
		pos = comma + 1;
	}
	return !voices.empty();
}

} // namespace

int main(int argc, char **argv)
{
	BenchSettings settings;

	for (int i = 1 ; i < argc ; i++)
	{
		std::string arg = argv[i];
		if (arg == "--sr" && i + 1 < argc)
			settings.sample_rate = atoi(argv[++i]);
		else if (arg == "--seconds" && i + 1 < argc)
			settings.seconds = atof(argv[++i]);
		else if (arg == "--reps" && i + 1 < argc)
			settings.repetitions = atoi(argv[++i]);
		else if (arg == "--frames" && i + 1 < argc)
			settings.frames = atoi(argv[++i]);
		else if (arg == "--libraries" && i + 1 < argc)
			settings.libraries = argv[++i];
		else if (arg == "--case" && i + 1 < argc)
			settings.only_case = argv[++i];
		else if (arg == "--code-file" && i + 1 < argc)
			settings.code_file = argv[++i];
		else if (arg == "--svg")
			settings.svg = true;
		else if (arg == "--backend" && i + 1 < argc)
			settings.only_backend = argv[++i];
		else if (arg == "--mode" && i + 1 < argc)
			settings.only_mode = argv[++i];
		else if (arg == "--voices" && i + 1 < argc)
		{
			if (!parse_voices(argv[++i], settings.voices))
			{
				fprintf(stderr, "Invalid --voices\n");
				return 1;
			}
		}
		else
		{
			usage(argv[0]);
			return arg == "--help" || arg == "-h" ? 0 : 1;
		}
	}

	if (settings.seconds <= 0.0 || settings.repetitions < 1 || settings.frames < 1)
	{
		fprintf(stderr, "Invalid timing settings\n");
		return 1;
	}

#if defined(__SSE__)
	_MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
#  if defined(_MM_DENORMALS_ZERO_ON)
	_MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
#  endif
#endif

	startMTDSPFactories();

	printf("sample rate: %d, frames: %d, seconds/rep: %.2f, reps: %d\n",
	       settings.sample_rate, settings.frames, settings.seconds, settings.repetitions);

	std::string overlay_dir;
	std::string overlay_error;
	bool run_constant = settings.only_mode != "runtime";
	bool run_runtime = settings.only_mode != "constant";
	if (run_constant)
	{
		overlay_dir = create_sr_overlay(settings.libraries, settings.sample_rate, overlay_error);
		if (overlay_dir.empty())
		{
			fprintf(stderr, "ERROR: %s\n", overlay_error.c_str());
			return 1;
		}
		printf("constant-SR overlay: %s\n", overlay_dir.c_str());
	}

#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
	printf("LLVM backend: not available (compiled with WITHOUT_LLVM_IN_FAUST_DEV)\n");
#else
	printf("LLVM backend: available\n");
#endif
	printf("\n");
	print_header();

	std::vector<BenchCase> cases;
	if (!settings.code_file.empty())
	{
		std::ifstream in(settings.code_file.c_str());
		if (!in.is_open())
		{
			fprintf(stderr, "ERROR: could not open %s\n", settings.code_file.c_str());
			return 1;
		}
		BenchCase bench_case;
		bench_case.name = settings.code_file;
		bench_case.code = std::string((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
		cases.push_back(bench_case);
	}
	else
	{
		for (const BenchCase &bench_case : g_cases)
			cases.push_back(bench_case);
	}

	for (const BenchCase &bench_case : cases)
	{
		if (!settings.only_case.empty() && bench_case.name.find(settings.only_case) == std::string::npos)
			continue;

		for (int backend = 0 ; backend < 2 ; backend++)
		{
			const bool use_llvm = backend == 1;
			const char *backend_name = use_llvm ? "llvm" : "interp";
			if (settings.only_backend == "llvm" && !use_llvm)
				continue;
			if (settings.only_backend == "interp" && use_llvm)
				continue;
#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
			if (use_llvm)
				continue;
#endif

			for (int mode = 0 ; mode < 2 ; mode++)
			{
				const bool constant = mode == 1;
				const char *mode_name = constant ? "constant" : "runtime";
				if (constant && !run_constant)
					continue;
				if (!constant && !run_runtime)
					continue;

				std::vector<std::string> options;
				options.push_back("-I");
				options.push_back(settings.libraries);
				if (constant)
				{
					options.push_back("-I");
					options.push_back(overlay_dir);
				}
				if (settings.svg)
				{
					std::filesystem::path svg_dir = std::filesystem::temp_directory_path() / "radium_faust_bench_svg";
					std::error_code ec;
					std::filesystem::create_directories(svg_dir, ec);
					options.push_back("-svg");
					options.push_back("-O");
					options.push_back(svg_dir.string());
				}

				std::string error;
				double compile_start = now_seconds();
				Factory factory = compile_program(bench_case.code, options, use_llvm, 4, error);
				double compile_ms = (now_seconds() - compile_start) * 1000.0;

				if (factory.f == NULL)
				{
					fprintf(stderr, "ERROR: %s / %s / %s: %s\n", bench_case.name.c_str(), backend_name, mode_name, error.c_str());
					continue;
				}

				for (size_t i = 0 ; i < settings.voices.size() ; i++)
				{
					int num_voices = settings.voices[i];
					BenchResult result = measure_factory(factory.f, settings, num_voices);
					if (result.ok)
						print_result(bench_case, backend_name, mode_name, num_voices, result, i == 0 ? compile_ms : 0.0);
				}

				factory.destroy();
			}
		}
	}

	stopMTDSPFactories();
	return 0;
}
