/*
	libpds test program: creates two independent libpd instances, starts and
	stops a Pd Tcl/Tk GUI process for both, runs audio for 10 seconds, and shuts
	down cleanly. The GUI is skipped when it can't be started (no display etc).

	Build and run from the Radium root directory (requires the libpd package
	to be built first, i.e. bin/packages/libpd/libs/libpd.a must exist):

		make -C libpds/tests
		make -C libpds/tests run
*/

#include <chrono>
#include <cstdio>
#include <cstring>
#include <thread>

#include "../libpds.h"

extern void sys_lock(void);
extern void sys_unlock(void);

static const int g_sample_rate = 44100;
static const int g_block_size = 512;
static const double g_run_seconds = 10.0;

static pd_t *start_instance(const char *libdir)
{
	pd_t *pd = libpds2_create(true, libdir);
	if (pd == NULL)
	{
		fprintf(stderr, "libpds2_create failed: %s\n", libpds2_strerror());
		return NULL;
	}

	libpds2_init_audio(pd, 1, 2, g_sample_rate);

	// compute audio    [; pd dsp 1(
	libpds2_start_message(pd, 1);
	libpds2_add_float(pd, 1.0f);
	libpds2_finish_message(pd, "pd", "dsp");

	// Dummy function at the moment.
	libpds2_show_gui(pd);

	return pd;
}

// Opens a small patch, adds an object to the canvas (the same way the GUI
// does when an object is created), saves the patch, and checks that the saved
// file contains the added object.
static bool test_savefile(const char *libdir)
{
	const char *dir = "/tmp/";
	const char *inname = "libpds_save_test_in.pd";
	const char *outname = "libpds_save_test_out.pd";

	{
		FILE *f = fopen("/tmp/libpds_save_test_in.pd", "w");
		if (f == NULL)
		{
			perror("fopen");
			return false;
		}
		fprintf(f, "#N canvas 0 0 450 300 12;\n"
			"#X obj 100 100 osc~ 440;\n"
			"#X obj 100 150 dac~;\n"
			"#X connect 0 0 1 0;\n"
			"#X connect 0 0 1 1;\n");
		fclose(f);
	}
	remove("/tmp/libpds_save_test_out.pd");

	pd_t *pd = libpds2_create(true, libdir);
	if (pd == NULL)
	{
		fprintf(stderr, "libpds2_create failed: %s\n", libpds2_strerror());
		return false;
	}

	void *file = libpds2_openfile(pd, inname, dir);
	if (file == NULL)
	{
		fprintf(stderr, "Could not open test patch\n");
		libpds2_delete(pd);
		return false;
	}

	{
		t_atom argv[4];
		SETFLOAT(argv+0, 50);
		SETFLOAT(argv+1, 50);
		SETSYMBOL(argv+2, gensym("osc~"));
		SETFLOAT(argv+3, 330);
		sys_lock();
		pd_typedmess((t_pd*)file, gensym("obj"), 4, argv);
		sys_unlock();
	}

	libpds2_savefile(pd, file, outname, dir);

	bool found = false;
	FILE *f = fopen("/tmp/libpds_save_test_out.pd", "r");
	if (f == NULL)
		fprintf(stderr, "Could not read the saved patch\n");
	else
	{
		char line[1024];
		while (fgets(line, sizeof(line), f) != NULL)
			if (strstr(line, "osc~ 330") != NULL)
				found = true;
		fclose(f);
	}

	libpds2_closefile(pd, file);
	libpds2_delete(pd);

	remove("/tmp/libpds_save_test_in.pd");
	remove("/tmp/libpds_save_test_out.pd");

	if (!found)
		fprintf(stderr, "The saved patch does not contain the added object\n");

	return found;
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

	printf("Testing patch saving.\n");
	if (!test_savefile(libdir))
		return 1;

	printf("Creating two libpd instances.\n");
	pd_t *pd1 = start_instance(libdir);
	pd_t *pd2 = start_instance(libdir);
	if (pd1 == NULL || pd2 == NULL)
	{
		return 1;
	}

	printf("Running audio for %g seconds.\n", g_run_seconds);

	// The non-blocking poll should succeed (i.e. not return -1) when the
	// instance is not locked.
	if (libpds2_try_poll_gui(pd1) < 0 || libpds2_try_poll_gui(pd2) < 0)
	{
		fprintf(stderr, "libpds2_try_poll_gui returned -1 for an unlocked instance\n");
		return 1;
	}

	run_audio(pd1, pd2);

	printf("Closing down.\n");
	libpds2_hide_gui(pd1);
	libpds2_hide_gui(pd2);
	libpds2_delete(pd1);
	libpds2_delete(pd2);

	printf("Exited cleanly.\n");
	return 0;
}
