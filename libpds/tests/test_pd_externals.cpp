/*
	Tests loading of the third-party Pd externals the way the Pd2 instrument
	does: lazily, into a process-wide "externals host" instance, through the
	sys_set_libloadhook() hook in libpd and the libpds2 _nolock functions.

	The test scans the externals directory, builds the same kind of
	name -> library tables as Radium's ensure_externals_host(), installs the
	hook, and loads patches in instrument instances that use external classes
	and abstractions. Among the things checked are that

	  - nothing is loaded before a patch needs it (no eager loading),
	  - class binaries, monolithic-library classes and [declare -lib] work,
	  - abstractions don't cause the monolithic libraries to be loaded,
	  - classes stay usable after the instance that first needed them is
	    deleted (the reason for the external host in the first place).

	Build and run from the Radium root directory (requires bin/pd/externals
	and bin/packages/libpd to be built first):

		make -C libpds/tests
		make -C libpds/tests run-externals
*/

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "../libpds.h"

namespace fs = std::filesystem;

static int g_num_failed = 0;

static void check(bool ok, const char *what)
{
	if (ok)
	{
		printf("  ok   %s\n", what);
	}
	else
	{
		printf("  FAIL %s\n", what);
		g_num_failed++;
	}
}

// ---------------------------------------------------------------- name tables

// The file extensions Pd can load external libraries from (".pd_darwin" on
// Mac, ".pd_linux" on Linux, ...), longest first. Same as
// get_library_extensions() in audio/Pd_plugin2.cpp.
static std::vector<std::string> get_library_extensions()
{
	std::vector<std::string> ret;

	for (const char **ext = libpds2_get_library_extensions(); *ext != NULL; ext++)
		ret.push_back(*ext);

	std::sort(ret.begin(), ret.end(), [](const std::string &a, const std::string &b){
		return a.size() > b.size();
	});

	return ret;
}

static bool ends_with(const std::string &s, const std::string &suffix)
{
	return s.size() >= suffix.size() && s.compare(s.size() - suffix.size(), suffix.size(), suffix) == 0;
}

static std::string external_class_name(const std::string &filename)
{
	for (const std::string &extension : get_library_extensions())
		if (!extension.empty() && ends_with(filename, extension))
			return filename.substr(0, filename.size() - extension.size());

	return "";
}

// "foo-help.pd" -> "foo", "foo.pd_linux" -> "foo". Same as
// file_claimed_class_name() in audio/Pd_plugin2.cpp.
static std::string file_claimed_class_name(const std::string &filename)
{
	if (ends_with(filename, "-meta.pd"))
		return "";

	std::string name;

	if (ends_with(filename, "-help.pd"))
	{
		name = filename.substr(0, filename.size() - strlen("-help.pd"));
	}
	else
	{
		name = external_class_name(filename);
		if (name.empty() && ends_with(filename, ".pd"))
			name = filename.substr(0, filename.size() - strlen(".pd"));
	}

	if (name.empty() || name.find('/') != std::string::npos)
		return "";

	return name;
}

static bool is_shared_library_file(const std::string &filename)
{
	return filename.compare(0, 3, "lib") == 0
		&& (ends_with(filename, ".so") || ends_with(filename, ".dylib") || ends_with(filename, ".dll"));
}

struct Externals
{
	pd_t *host = NULL;

	std::map<std::string, std::string> class_loadnames; // name, "<libdir>/<class>" or "<libdir>"
	std::map<std::string, std::string> blob_claims;     // name -> monolithic library
	std::map<std::string, std::string> library_names;   // monolithic library name -> itself
	std::vector<std::string> blob_libraries;            // in search path order

	std::set<std::string> loaded;
	std::set<std::string> failed;
	bool all_blobs_tried = false;
};

static Externals g_externals;

static std::vector<std::string> sorted_directory_entries(const fs::path &dir, bool want_directories)
{
	std::vector<std::string> ret;

	for (const fs::directory_entry &entry : fs::directory_iterator(dir))
	{
		bool is_dir = entry.is_directory();
		if (is_dir != want_directories)
			continue;
		ret.push_back(entry.path().filename().string());
	}

	std::sort(ret.begin(), ret.end());
	return ret;
}

// Same as ensure_externals_host() in audio/Pd_plugin2.cpp: creates the host,
// adds the search paths, and builds the name tables, but loads nothing.
static bool create_externals_host(const std::string &puredatapath, const std::string &externals_path)
{
	g_externals.host = libpds2_create(false, puredatapath.c_str());
	if (g_externals.host == NULL)
	{
		fprintf(stderr, "libpds2_create (externals host) failed: %s\n", libpds2_strerror());
		return false;
	}

	libpds2_add_to_search_path(g_externals.host, externals_path.c_str());

	for (const std::string &libdir : sorted_directory_entries(externals_path, true))
	{
		const std::string libpath = externals_path + "/" + libdir;
		libpds2_add_to_search_path(g_externals.host, libpath.c_str());

		std::vector<std::string> files = sorted_directory_entries(libpath, false);

		bool has_blob = false;
		for (const std::string &file : files)
		{
			if (is_shared_library_file(file))
				continue;
			if (fs::is_symlink(fs::path(libpath) / file))
				continue;
			if (getenv("PD2_TEST_VERBOSE") != NULL && external_class_name(file) == libdir)
				fprintf(stderr, "[scan] blob %s/%s -> %s\n", libdir.c_str(), file.c_str(), external_class_name(file).c_str());
			if (external_class_name(file) == libdir)
				has_blob = true;
		}
		if (getenv("PD2_TEST_VERBOSE") != NULL)
			fprintf(stderr, "[scan] %s: %d files, has_blob=%d\n", libdir.c_str(), (int)files.size(), has_blob);

		if (has_blob)
		{
			g_externals.blob_libraries.push_back(libdir);
			g_externals.library_names[libdir] = libdir;
			g_externals.class_loadnames[libdir] = libdir;
		}

		for (const std::string &file : files)
		{
			if (is_shared_library_file(file))
				continue;
			if (fs::is_symlink(fs::path(libpath) / file))
				continue;

			std::string classname = external_class_name(file);
			if (!classname.empty() && !(has_blob && classname == libdir))
			{
				std::string loadname = libdir + "/" + classname;
				if (g_externals.class_loadnames.find(classname) == g_externals.class_loadnames.end())
					g_externals.class_loadnames[classname] = loadname;
				if (g_externals.class_loadnames.find(loadname) == g_externals.class_loadnames.end())
					g_externals.class_loadnames[loadname] = loadname;
			}

			if (has_blob)
			{
				std::string claimed = file_claimed_class_name(file);
				if (!claimed.empty() && g_externals.blob_claims.find(claimed) == g_externals.blob_claims.end())
					g_externals.blob_claims[claimed] = libdir;
			}
		}
	}

	return true;
}

// --------------------------------------------------------------------- hook

// Same as ensure_library_loaded() in audio/Pd_plugin2.cpp.
static bool ensure_library_loaded(const std::string &loadname)
{
	if (g_externals.loaded.find(loadname) != g_externals.loaded.end())
		return true;
	if (g_externals.failed.find(loadname) != g_externals.failed.end())
		return false;

	if (libpds2_load_lib_nolock(g_externals.host, loadname.c_str()))
	{
		g_externals.loaded.insert(loadname);
		return true;
	}

	g_externals.failed.insert(loadname);
	return false;
}

// Same as load_external_library() in audio/Pd_plugin2.cpp.
static bool load_external_library(const std::string &classname, const std::string &loadname)
{
	if (!ensure_library_loaded(loadname))
		return false;

	return libpds2_class_is_loaded_nolock(g_externals.host, classname.c_str()) != 0;
}

// Same as externals_libloadhook() in audio/Pd_plugin2.cpp.
static int externals_libloadhook(const char *classname)
{
	if (g_externals.host == NULL || classname == NULL)
		return 0;

	const std::string name = classname;

	if (getenv("PD2_TEST_VERBOSE") != NULL)
		fprintf(stderr, "[libloadhook] %s\n", classname);

	std::map<std::string, std::string>::const_iterator it = g_externals.library_names.find(name);
	if (it != g_externals.library_names.end())
		return ensure_library_loaded(name) ? 1 : 0;

	it = g_externals.class_loadnames.find(name);
	if (it != g_externals.class_loadnames.end())
	{
		if (load_external_library(name, it->second))
			return 1;
	}

	it = g_externals.blob_claims.find(name);
	if (it != g_externals.blob_claims.end())
	{
		if (load_external_library(name, it->second))
			return 1;
	}

	if (libpds2_abstraction_exists_nolock(classname))
		return 0;

	if (!g_externals.all_blobs_tried)
	{
		for (const std::string &libdir : g_externals.blob_libraries)
		{
			if (load_external_library(name, libdir))
				return 1;
		}
		g_externals.all_blobs_tried = true;
	}

	return 0;
}

// ---------------------------------------------------------------- instances

// Same search paths as create_data() in audio/Pd_plugin2.cpp.
static pd_t *create_instrument(const std::string &puredatapath, const std::string &externals_path)
{
	pd_t *pd = libpds2_create(true, puredatapath.c_str());
	if (pd == NULL)
	{
		fprintf(stderr, "libpds2_create failed: %s\n", libpds2_strerror());
		return NULL;
	}

	libpds2_add_to_search_path(pd, externals_path.c_str());

	for (const std::string &libdir : sorted_directory_entries(externals_path, true))
		libpds2_add_to_search_path(pd, (externals_path + "/" + libdir).c_str());

	return pd;
}

static bool write_patch(const std::string &filename, const std::vector<std::string> &objects)
{
	FILE *f = fopen(filename.c_str(), "w");
	if (f == NULL)
	{
		perror("fopen");
		return false;
	}

	fprintf(f, "#N canvas 0 0 450 300 12;\n");
	for (size_t i = 0; i < objects.size(); i++)
		fprintf(f, "#X obj 20 %d %s;\n", 30 + 30 * (int)i, objects[i].c_str());

	fclose(f);
	return true;
}

static bool class_is_loaded_in_host(const std::string &classname)
{
	// The test is single-threaded, so the _nolock functions can be called
	// without taking Pd's lock.
	return libpds2_class_is_loaded_nolock(g_externals.host, classname.c_str()) != 0;
}

// ------------------------------------------------------------------- tests

static bool test_lazy_loading(const std::string &puredatapath, const std::string &externals_path)
{
	printf("Test: nothing is loaded until a patch needs it.\n");

	check(!class_is_loaded_in_host("zl"), "cyclone/zl is not loaded before use");
	check(!class_is_loaded_in_host("multiplex~"), "zexy's multiplex~ is not loaded before use");
	check(!class_is_loaded_in_host("filter~"), "iemlib's filter~ is not loaded before use");

	pd_t *pd = create_instrument(puredatapath, externals_path);
	if (pd == NULL)
		return false;

	// Class binaries, requested both by [zl] and [cyclone/zl], plus
	// monolithic-library classes ([multiplex~] and [filter~]) and a class
	// from ELSE (which uses .pd_darwin files since the per-platform
	// extension fix in pd_externals.list).
	write_patch("/tmp/libpds_externals_test_a.pd",
		    { "cyclone/zl 10", "zl 20", "multiplex~", "filter~", "else/square~", "osc~ 440" });

	void *file = libpds2_openfile(pd, "libpds_externals_test_a.pd", "/tmp/");
	check(file != NULL, "patch with externals is opened");

	check(class_is_loaded_in_host("zl"), "cyclone/zl was loaded into the host");
	check(class_is_loaded_in_host("multiplex~"), "multiplex~ was loaded into the host");
	check(class_is_loaded_in_host("filter~"), "filter~ was loaded into the host");
	check(class_is_loaded_in_host("square~"), "ELSE's square~ was loaded into the host");
	check(class_is_loaded_in_host("osc~"), "built-in osc~ is available");

	check(libpds2_load_lib(g_externals.host, "cyclone/zl") != 0,
	      "reloading an already loaded library succeeds");

	if (file != NULL)
		libpds2_closefile(pd, file);
	libpds2_delete(pd);

	return true;
}

static bool test_abstractions_dont_load_libraries(const std::string &puredatapath, const std::string &externals_path)
{
	printf("Test: abstractions do not load the monolithic libraries.\n");

	// Make sure the class of a library that has not been used yet is not
	// loaded. (iemmatrix is used by this test only through its abstractions,
	// if any; pick a class from a library that is not otherwise loaded.)
	bool iemmatrix_loaded_before = class_is_loaded_in_host("matrix");

	pd_t *pd = create_instrument(puredatapath, externals_path);
	if (pd == NULL)
		return false;

	write_patch("/tmp/libpds_externals_test_b.pd", { "list-accum" });

	void *file = libpds2_openfile(pd, "libpds_externals_test_b.pd", "/tmp/");
	check(file != NULL, "patch with an abstraction is opened");
	check(class_is_loaded_in_host("list-accum"), "list-accum was loaded as an abstraction");
	check(class_is_loaded_in_host("matrix") == iemmatrix_loaded_before,
	      "no unrelated monolithic library was loaded");

	if (file != NULL)
		libpds2_closefile(pd, file);
	libpds2_delete(pd);

	return true;
}

static bool test_classes_survive_instance_deletion(const std::string &puredatapath, const std::string &externals_path)
{
	printf("Test: classes stay usable after the instance that needed them is deleted.\n");

	pd_t *pd = create_instrument(puredatapath, externals_path);
	if (pd == NULL)
		return false;

	write_patch("/tmp/libpds_externals_test_c.pd", { "zl 30" });
	void *file = libpds2_openfile(pd, "libpds_externals_test_c.pd", "/tmp/");
	check(file != NULL, "second instance can use zl");
	if (file != NULL)
		libpds2_closefile(pd, file);
	libpds2_delete(pd);

	// The first instance (and every other so far) is gone. A new one must
	// still work, and the global classes must not point into freed state.
	pd = create_instrument(puredatapath, externals_path);
	if (pd == NULL)
		return false;

	write_patch("/tmp/libpds_externals_test_d.pd", { "zl 40", "multiplex~" });
	file = libpds2_openfile(pd, "libpds_externals_test_d.pd", "/tmp/");
	check(file != NULL, "new instance can use zl and multiplex~");
	check(class_is_loaded_in_host("zl"), "zl is still registered");

	if (file != NULL)
		libpds2_closefile(pd, file);
	libpds2_delete(pd);

	return true;
}

static bool test_declare_lib(const std::string &puredatapath, const std::string &externals_path)
{
	printf("Test: [declare -lib] loads a monolithic library into the host.\n");

	pd_t *pd = create_instrument(puredatapath, externals_path);
	if (pd == NULL)
		return false;

	// iemmatrix has not been used by the other tests yet.
	check(!class_is_loaded_in_host("mtx_abs"), "iemmatrix's mtx_abs is not loaded before use");

	write_patch("/tmp/libpds_externals_test_e.pd", { "declare -lib iemmatrix", "mtx_abs" });

	void *file = libpds2_openfile(pd, "libpds_externals_test_e.pd", "/tmp/");
	check(file != NULL, "patch with [declare -lib iemmatrix] is opened");
	check(class_is_loaded_in_host("mtx_abs"), "iemmatrix was loaded into the host");

	if (file != NULL)
		libpds2_closefile(pd, file);
	libpds2_delete(pd);

	return true;
}

int main(int argc, char **argv)
{
	const std::string externals_path = argc > 1 ? argv[1] : "../../bin/pd/externals";
	const std::string puredatapath = argc > 2 ? argv[2] : "../../bin/packages/libpd/pure-data";

	if (!fs::is_directory(externals_path))
	{
		fprintf(stderr, "externals directory not found: %s (run from libpds/tests)\n", externals_path.c_str());
		return 1;
	}

	printf("Testing lazy loading of Pd externals.\n");
	printf("  externals: %s\n", externals_path.c_str());
	printf("  puredata:  %s\n", puredatapath.c_str());

	{
		std::vector<std::string> extensions = get_library_extensions();
		printf("  platform extensions:");
		for (const std::string &extension : extensions)
			printf(" %s", extension.c_str());
		printf("\n");
		check(!extensions.empty(), "Pd reports at least one loadable extension");
	}

	if (!create_externals_host(puredatapath, externals_path))
		return 1;

	printf("  %d class names, %d blob claims, %d monolithic libraries\n",
	       (int)g_externals.class_loadnames.size(),
	       (int)g_externals.blob_claims.size(),
	       (int)g_externals.library_names.size());

	libpds2_set_libloadhook(externals_libloadhook);

	if (!test_lazy_loading(puredatapath, externals_path)
	    || !test_abstractions_dont_load_libraries(puredatapath, externals_path)
	    || !test_classes_survive_instance_deletion(puredatapath, externals_path)
	    || !test_declare_lib(puredatapath, externals_path))
		g_num_failed++;

	remove("/tmp/libpds_externals_test_a.pd");
	remove("/tmp/libpds_externals_test_b.pd");
	remove("/tmp/libpds_externals_test_c.pd");
	remove("/tmp/libpds_externals_test_d.pd");
	remove("/tmp/libpds_externals_test_e.pd");

	if (g_num_failed > 0)
	{
		printf("%d test(s) failed.\n", g_num_failed);
		return 1;
	}

	printf("All tests passed.\n");
	return 0;
}
