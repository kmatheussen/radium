/*
	libpds GUI test program: creates two independent libpd instances, opens a
	small patch in each one, starts a Pd Tcl/Tk GUI process for both (so two
	patch windows become visible), runs audio for 15 seconds, and shuts down
	cleanly. The GUI is skipped when it can't be started (no display etc).

	Build and run from the Radium root directory (requires the libpd package
	to be built first, i.e. bin/packages/libpd/libs/libpd.a must exist):

		make -C libpds/tests
		make -C libpds/tests run-gui
*/

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <thread>

#include "../libpds.h"

static const int g_sample_rate = 44100;
static const int g_block_size = 512;
static const double g_run_seconds = 15.0;

// Returns $TMPDIR (with a trailing directory separator), or "/tmp/".
static const char *get_temp_dir(void)
{
	static char temp_dir[1024];

	const char *dir = getenv("TMPDIR");
	if (dir == NULL || dir[0] == 0)
		dir = "/tmp";

	const size_t len = strlen(dir);
	if (len > 0 && dir[len-1] == '/')
		snprintf(temp_dir, sizeof(temp_dir), "%s", dir);
	else
		snprintf(temp_dir, sizeof(temp_dir), "%s/", dir);

	return temp_dir;
}

static bool write_patch_file(const char *dirname, const char *filename, int num)
{
	char path[2048];
	snprintf(path, sizeof(path), "%s%s", dirname, filename);

	FILE *f = fopen(path, "w");
	if (f == NULL)
	{
		perror(path);
		return false;
	}

	fprintf(f, "#N canvas 0 0 450 300 12;\n"
		"#X text 20 20 libpds GUI test %d;\n"
		"#X obj 50 60 osc~ 440;\n"
		"#X obj 50 110 dac~;\n"
		"#X connect 1 0 2 0;\n"
		"#X connect 1 0 2 1;\n",
		num);

	fclose(f);
	return true;
}

static void remove_patch_file(const char *dirname, const char *filename)
{
	char path[2048];
	snprintf(path, sizeof(path), "%s%s", dirname, filename);
	remove(path);
}

struct Instance
{
	pd_t *pd;
	void *file;
};

static Instance start_instance(const char *libdir, const char *dir, const char *filename)
{
	Instance instance = {};

	instance.pd = libpds2_create(true, libdir);
	if (instance.pd == NULL)
	{
		fprintf(stderr, "libpds2_create failed: %s\n", libpds2_strerror());
		return instance;
	}

	libpds2_init_audio(instance.pd, 1, 2, g_sample_rate);

	// compute audio    [; pd dsp 1(
	libpds2_start_message(instance.pd, 1);
	libpds2_add_float(instance.pd, 1.0f);
	libpds2_finish_message(instance.pd, "pd", "dsp");

	instance.file = libpds2_openfile(instance.pd, filename, dir);
	if (instance.file == NULL)
	{
		fprintf(stderr, "Could not open %s%s: %s\n", dir, filename, libpds2_strerror());
		libpds2_delete(instance.pd);
		instance.pd = NULL;
		return instance;
	}

	libpds2_show_gui(instance.pd);

	return instance;
}

static void stop_instance(Instance instance)
{
	if (instance.pd == NULL)
		return;

	libpds2_hide_gui(instance.pd);
	libpds2_closefile(instance.pd, instance.file);
	libpds2_delete(instance.pd);
}

static void run_audio(pd_t *pd1, pd_t *pd2)
{
	const int num_ticks = g_block_size / libpds2_blocksize(pd1);
	float in_buffer[g_block_size] = {};       // one input channel (silence)
	float out_buffer[g_block_size * 2] = {};  // two output channels (interleaved)

	const auto start_time = std::chrono::steady_clock::now();
	const auto run_time = std::chrono::duration<double>(g_run_seconds);
	const auto sleep_time = std::chrono::microseconds(g_block_size * 1000000 / g_sample_rate);

	while (std::chrono::steady_clock::now() - start_time < run_time)
	{
		libpds2_process_float(pd1, num_ticks, in_buffer, out_buffer);
		libpds2_process_float(pd2, num_ticks, in_buffer, out_buffer);

		// The process functions only flush already-buffered GUI output (they
		// are realtime safe). Incoming GUI/network messages are handled from a
		// non-realtime thread, so poll here to emulate the host's GUI thread.
		for (int i = 0; i < 8 && libpds2_poll_gui(pd1); i++)
			;
		for (int i = 0; i < 8 && libpds2_poll_gui(pd2); i++)
			;

		std::this_thread::sleep_for(sleep_time);
	}
}

int main(int argc, char **argv)
{
	const char *libdir = argc > 1 ? argv[1] : "bin/packages/libpd/pure-data";
	const char *dir = get_temp_dir();
	const char *filename1 = "libpds_gui_test_1.pd";
	const char *filename2 = "libpds_gui_test_2.pd";

	if (!write_patch_file(dir, filename1, 1) || !write_patch_file(dir, filename2, 2))
		return 1;

	printf("Creating two libpd instances with GUI windows.\n");
	Instance instance1 = start_instance(libdir, dir, filename1);
	Instance instance2 = start_instance(libdir, dir, filename2);
	if (instance1.pd == NULL || instance2.pd == NULL)
	{
		stop_instance(instance1);
		stop_instance(instance2);
		remove_patch_file(dir, filename1);
		remove_patch_file(dir, filename2);
		return 1;
	}

	printf("Running audio and GUI for %g seconds.\n", g_run_seconds);
	run_audio(instance1.pd, instance2.pd);

	printf("Closing down.\n");
	stop_instance(instance1);
	stop_instance(instance2);

	remove_patch_file(dir, filename1);
	remove_patch_file(dir, filename2);

	printf("Exited cleanly.\n");
	return 0;
}
