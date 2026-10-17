/*
	Exhaustive load test for the dynamically built Pd externals used by the
	Pd2 instrument.

	This complements test_pd_externals.cpp, which tests the lazy-loading
	behavior. This test eagerly loads every external and class binary into a
	process-wide host instance, dlopen()s every class binary with RTLD_NOW,
	and creates a few objects per library in three successive libpd instances:

	1. A process-wide "library host" instance is created and every external
	   library is loaded into it (monolithic blobs by name, class binaries by
	   their namespaced name). Pd only runs a library's setup() the first time
	   it is loaded in a process, so the instance running setup must never be
	   deleted.
	2. Every class binary is dlopen()ed with RTLD_NOW, which catches missing
	   symbols (the failure mode of externals built without PDINSTANCE).
	3. A few objects per library are created in a normal libpd instance. The
	   instance is deleted and recreated twice, to catch libraries that keep
	   per-instance state in file-scope statics.

	Build and run from the Radium root directory (requires the libpd package
	and the externals to be built first, i.e. bin/packages/libpd/libs/libpd.so
	and bin/pd/externals must exist):

		make -C libpds/tests
		make -C libpds/tests run-externals
*/

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "../libpds.h"

namespace fs = std::filesystem;

static const char *g_default_externals_dir = "../../bin/pd/externals";
static std::string g_print_output;

// Paths are kept as std::filesystem::path, which uses wide characters on
// Windows. They are converted to UTF-8 at the libpd boundary, which knows how
// to handle that on all platforms.
static std::string to_utf8(const fs::path &path)
{
#if defined(__cpp_char8_t)
	std::u8string u8 = path.u8string();
	return std::string(u8.begin(), u8.end());
#else
	return path.u8string();
#endif
}

static fs::path from_utf8(const std::string &str)
{
#if defined(__cpp_char8_t)
	std::u8string u8(reinterpret_cast<const char8_t *>(str.data()), str.size());
	return fs::path(u8);
#else
	return fs::u8path(str);
#endif
}

static void print_hook(const char *s)
{
	g_print_output += s;
}

static bool ends_with(const std::string &str, const std::string &suffix)
{
	return str.size() >= suffix.size() &&
		str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

static bool starts_with(const std::string &str, const std::string &prefix)
{
	return str.size() >= prefix.size() &&
		str.compare(0, prefix.size(), prefix) == 0;
}

// Returns the class name for a Pd binary file name, or "" if the file is not
// a class binary. Shared support libraries (lib*.so / lib*.dylib / lib*.dll)
// are not classes and are skipped.
static std::string class_name_of(const std::string &filename)
{
	static const char *extensions[] = {
		".pd_linux", ".pd_darwin", ".pd_freebsd",
	};
	for (const char *ext : extensions)
		if (ends_with(filename, ext))
			return filename.substr(0, filename.size() - strlen(ext));

	// Deken-style architecture-tagged names, e.g. above~.linux-amd64-32.so.
	for (const char *marker : { ".linux-", ".darwin-", ".windows-" }) {
		size_t pos = filename.find(marker);
		if (pos != std::string::npos && ends_with(filename, ".so"))
			return filename.substr(0, pos);
	}

	return "";
}

static bool is_shared_library(const std::string &filename)
{
	return starts_with(filename, "lib") &&
		(ends_with(filename, ".so") || ends_with(filename, ".dylib") ||
		 ends_with(filename, ".dll"));
}

static std::vector<fs::path> list_dir(const fs::path &path, bool want_dirs)
{
	std::vector<fs::path> ret;
	std::error_code ec;
	for (const fs::directory_entry &entry : fs::directory_iterator(path, ec)) {
		std::error_code entry_ec;
		if (entry.is_directory(entry_ec) == want_dirs)
			ret.push_back(entry.path());
	}
	std::sort(ret.begin(), ret.end());
	return ret;
}

static bool file_exists(const fs::path &path)
{
	std::error_code ec;
	return fs::is_regular_file(path, ec);
}

static bool has_load_error(const std::string &output, std::string *found)
{
	static const char *patterns[] = {
		"no such object",
		"couldn't create",
		"can't load library",
		"undefined symbol",
		"load_object: Symbol",
		"could not load",
		"dlopen",
	};
	for (const char *pattern : patterns) {
		if (output.find(pattern) != std::string::npos) {
			if (found)
				*found = pattern;
			return true;
		}
	}
	return false;
}

static pd_t *g_library_host = NULL;

// Eagerly loads every external library and class binary into a process-wide
// instance. Unlike the lazy-loading host in test_pd_externals.cpp, this
// deliberately loads everything up front to catch broken libraries and
// setup() functions.
static pd_t *ensure_library_host(const char *libdir, const fs::path &externals_dir)
{
	if (g_library_host != NULL)
		return g_library_host;

	pd_t *host = libpds2_create(false, libdir);
	if (host == NULL) {
		fprintf(stderr, "Could not create the library host instance: %s\n",
			libpds2_strerror());
		return NULL;
	}

	libpds2_set_printhook(host, print_hook);
	libpds2_add_to_search_path(host, to_utf8(externals_dir).c_str());

	for (const fs::path &libpath : list_dir(externals_dir, true)) {
		std::string lib = to_utf8(libpath.filename());
		libpds2_add_to_search_path(host, to_utf8(libpath).c_str());

		// Monolithic libraries are loaded by name. Some libraries (cyclone)
		// also have a library binary next to per-class binaries; load both.
		std::string blob_linux = lib + ".pd_linux";
		std::string blob_darwin = lib + ".pd_darwin";
		bool has_blob = file_exists(libpath / blob_linux) ||
			file_exists(libpath / blob_darwin);
		if (has_blob) {
			g_print_output.clear();
			if (!libpds2_load_lib(host, lib.c_str())) {
				fprintf(stderr, "Could not load library %s\n", lib.c_str());
				libpds2_delete(host);
				return NULL;
			}
			std::string found;
			if (has_load_error(g_print_output, &found)) {
				fprintf(stderr, "lib %s: %s\n%s", lib.c_str(), found.c_str(),
					g_print_output.c_str());
				libpds2_delete(host);
				return NULL;
			}
		}

		for (const fs::path &file : list_dir(libpath, false)) {
			std::string filename = to_utf8(file.filename());
			if (filename == blob_linux || filename == blob_darwin)
				continue;
			if (is_shared_library(filename))
				continue;
			// Skip alias symlinks (e.g. cyclone's Append.pd_linux):
			// running setup() twice would register the class twice.
			std::error_code ec;
			if (fs::is_symlink(file, ec))
				continue;
			std::string cname = class_name_of(filename);
			if (cname.empty())
				continue;
			std::string loadname = lib + "/" + cname;
			if (getenv("PD2_TEST_VERBOSE") != NULL)
				fprintf(stderr, "host: loading %s\n", loadname.c_str());
			g_print_output.clear();
			if (!libpds2_load_lib(host, loadname.c_str())) {
				fprintf(stderr, "Could not load class %s\n", loadname.c_str());
				libpds2_delete(host);
				return NULL;
			}
		}
	}

	g_library_host = host;
	return host;
}

// dlopen() every class binary with RTLD_NOW, which resolves all symbols and
// catches e.g. missing s_float/pd_this references.
static bool test_binary_links(const fs::path &externals_dir)
{
	bool ret = true;

	for (const fs::path &libpath : list_dir(externals_dir, true)) {
		std::string lib = to_utf8(libpath.filename());
		bool checked = false;
		for (const fs::path &file : list_dir(libpath, false)) {
			std::string filename = to_utf8(file.filename());
			bool is_shared = is_shared_library(filename);
			if (!is_shared && class_name_of(filename).empty())
				continue;
			// Monolithic libraries are dlopen()ed as well; loading them into
			// the host above also runs their setup().
			checked = true;
			void *handle = dlopen(to_utf8(file).c_str(), RTLD_NOW | RTLD_GLOBAL);
			if (handle == NULL) {
				fprintf(stderr, "%s: dlopen failed: %s\n", to_utf8(file).c_str(), dlerror());
				ret = false;
			} else {
				dlclose(handle);
			}
		}
		if (!checked)
			printf("%s (no binaries) ", lib.c_str());
	}

	return ret;
}

struct object_test {
	const char *lib;
	const char *object;
};

// One or more objects per library, verified by actually creating them. Some
// classes need arguments, others fail without them, so this list is explicit
// rather than derived from the file names.
static const object_test g_objects[] = {
	{ "cyclone", "append 1" },
	{ "cyclone", "zl.change" },
	{ "cyclone", "clip 0 1" },
	{ "ekext", "bmt~" },
	{ "ekext", "markovChains" },
	{ "else", "adsr~" },
	{ "else", "osc.format" },
	{ "else", "sfont~" },
	{ "else", "plaits~" },
	{ "else", "lua" },
	{ "ggee", "constant" },
	{ "ggee", "getdir" },
	{ "ggee", "sinh" },
	{ "hcs", "folder_list" },
	{ "hcs", "uname" },
	{ "iemlib", "fade~" },
	{ "iemlib", "iem_append" },
	{ "iemlib", "biquad_freq_resp" },
	{ "iemmatrix", "mtx_mul" },
	{ "iemmatrix", "mtx_zeros 2 2" },
	{ "iemnet", "udpsend" },
	{ "iemnet", "udpreceive 0" },
	{ "list-abs", "last-x" },
	{ "list-abs", "list-sort" },
	{ "mrpeach", "slipenc 65536" },
	{ "mrpeach", "slipdec 65536" },
	{ "osc", "packOSC" },
	{ "osc", "routeOSC /foo" },
	// Abstractions from the osc library that use mrpeach's slip objects.
	{ "osc", "packOSCstream" },
	{ "osc", "unpackOSCstream" },
	{ "unauthorized", "compressor~" },
	{ "unauthorized", "wahwah~" },
	{ "windowing", "hanning~" },
	{ "windowing", "kaiser~" },
	{ "zexy", "demux 2" },
	{ "zexy", "l2s" },
	{ "zexy", "matchbox" },
};

static bool test_objects(pd_t *pd, const std::string &lib)
{
	std::string patch_name = "radium_pd_objects_" + lib + ".pd";
	fs::path patch_path = fs::temp_directory_path() / patch_name;
	{
		std::ofstream patch(patch_path);
		if (!patch.is_open()) {
			fprintf(stderr, "could not create %s\n", to_utf8(patch_path).c_str());
			return false;
		}
		patch << "#N canvas 0 0 450 300 12;\n";
		int x = 20, y = 20;
		for (const object_test &test : g_objects) {
			if (lib != test.lib)
				continue;
			patch << "#X obj " << x << " " << y << " " << test.object << ";\n";
			x += 160;
			if (x > 700) {
				x = 20;
				y += 30;
			}
		}
	}

	g_print_output.clear();
	void *file = libpds2_openfile(pd, patch_name.c_str(),
		to_utf8(patch_path.parent_path()).c_str());
	bool ok = file != NULL;
	if (ok) {
		std::string found;
		if (has_load_error(g_print_output, &found)) {
			fprintf(stderr, "%s objects: load error (%s)\n%s", lib.c_str(),
				found.c_str(), g_print_output.c_str());
			ok = false;
		}
		libpds2_closefile(pd, file);
	} else {
		fprintf(stderr, "%s: could not open object test patch\n", lib.c_str());
	}

	std::error_code ec;
	fs::remove(patch_path, ec);
	return ok;
}

static bool run_all(const char *libdir, const fs::path &externals_dir)
{
	pd_t *pd = libpds2_create(false, libdir);
	if (pd == NULL) {
		fprintf(stderr, "libpds2_create failed: %s\n", libpds2_strerror());
		return false;
	}

	libpds2_set_printhook(pd, print_hook);
	libpds2_add_to_search_path(pd, to_utf8(externals_dir).c_str());

	bool ret = true;
	for (const fs::path &libpath : list_dir(externals_dir, true)) {
		std::string lib = to_utf8(libpath.filename());
		libpds2_add_to_search_path(pd, to_utf8(libpath).c_str());

		printf("Testing %s ... ", lib.c_str());
		fflush(stdout);
		fflush(stderr);
		bool ok = test_objects(pd, lib);
		printf("%s\n", ok ? "ok" : "FAILED");
		if (!ok)
			ret = false;
	}

	libpds2_delete(pd);
	return ret;
}

int main(int argc, char **argv)
{
	fs::path externals_dir = argc > 1 ? from_utf8(argv[1]) : fs::u8path(g_default_externals_dir);
	const char *libdir = argc > 2 ? argv[2] : "../../bin/packages/libpd/pure-data";

	if (list_dir(externals_dir, true).empty()) {
		fprintf(stderr, "No external libraries found in %s\n", to_utf8(externals_dir).c_str());
		fprintf(stderr, "Build them first, e.g.: cd bin/packages && ./build.sh\n");
		return 1;
	}

	printf("Loading externals into the library host instance.\n");
	if (ensure_library_host(libdir, externals_dir) == NULL)
		return 1;

	printf("Checking that every class binary links.\n");
	if (!test_binary_links(externals_dir)) {
		fprintf(stderr, "Externals load test FAILED (dlopen)\n");
		return 1;
	}
	printf("\n");

	bool ret = true;
	for (int i = 0; i < 3; i++) {
		printf("Creating objects, instance %d.\n", i + 1);
		ret = run_all(libdir, externals_dir) && ret;
	}

	if (!ret) {
		fprintf(stderr, "Externals load test FAILED\n");
		return 1;
	}

	printf("Externals load test passed.\n");
	return 0;
}
