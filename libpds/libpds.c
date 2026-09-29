/* Copyright 2013 Kjetil S. Matheussen

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

/*
	libpds: multiple instance wrapper for libpd.

	This is a reimplementation of the old libpds API (from
	bin/packages/libpd-master/libpds) on top of the official libpd
	multiple-instance API:

			libpd_init()
			libpd_new_instance() / libpd_set_instance() / libpd_free_instance()

	Functions that don't have a trivially available libpd equivalent yet
	(saving patches, ...) are currently dummies. See the individual function
	comments.
*/

#include <stdbool.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32)
#include <unistd.h>
#endif

#include "libpds.h"

// Declared here instead of including Pd's internal headers. Only available
// when libpd is compiled with PDTHREADS (which Radium does).
extern void sys_lock(void);
extern void sys_unlock(void);
extern int sys_pollgui(void);

// Tell the Pd GUI (see the Radium patch in pdwindow.tcl) to not open its
// console window (.pdwindow). Radium only wants the patch editor window. This
// is done once before main() since the environment must be modified before the
// GUI process is started, and GUI startup can happen while Radium's player
// lock is held (in which case allocating memory is not allowed). The console
// can still be reopened from the GUI's Window menu.
static void libpds2_init_environment(void) __attribute__((constructor));
static void libpds2_init_environment(void)
{
#if defined(_WIN32)
	_putenv("RADIUM_PD_GUI_NO_CONSOLE=1");
#else
	setenv("RADIUM_PD_GUI_NO_CONSOLE", "1", 1);
#endif
}

struct _pd
{
	t_pdinstance *instance;

	// Stored for API compatibility. In the old libpds, this pointer was passed
	// as the first argument to all hooks. Official libpd hooks don't take a data
	// argument, so hook implementations must instead use libpd_this_instance()
	// to find their plugin, and this field is currently unused.
	void *hook_data;

	bool use_gui;
	char *libdir;
	bool gui_started;

	int in_chans;
	int out_chans;

	// Scratch buffers used by libpds2_process_float_noninterleaved.
	float *in_scratch;
	float *out_scratch;

	// Serializes process calls (and scratch buffer (re)allocation) for this
	// instance. libpd's own per-instance lock only protects each individual
	// tick, while the scratch buffers are also accessed outside of it.
	atomic_flag process_lock;
};

// lock_process/unlock_process must not be called recursively.
static void lock_process(pd_t *pd)
{
	while (atomic_flag_test_and_set_explicit(&pd->process_lock, memory_order_acquire))
		;
}

static void unlock_process(pd_t *pd)
{
	atomic_flag_clear_explicit(&pd->process_lock, memory_order_release);
}

static char error_string[1024] = {0};

static void set_error(const char *message)
{
	snprintf(error_string, sizeof(error_string), "%s", message);
}

static void set_instance(pd_t *pd)
{
	libpd_set_instance(pd->instance);
}

pd_t *libpds2_create(bool use_gui, const char* libdir)
{
	// libpd_init is safe to call more than once. Returns -1 if libpd was
	// already initialized.
	int ret = libpd_init();
	if (ret != 0 && ret != -1)
	{
		set_error("libpd_init failed.");
		return NULL;
	}

	t_pdinstance *instance = libpd_new_instance();
	if (instance == NULL)
	{
		set_error("Unable to create a new libpd instance. (Was libpd compiled with MULTI=true?)");
		return NULL;
	}

	pd_t *pd = (pd_t*)calloc(1, sizeof(pd_t));
	if (pd == NULL)
	{
		set_error("Out of memory.");
		libpd_free_instance(instance);
		return NULL;
	}

	atomic_flag_clear(&pd->process_lock);

	pd->instance = instance;
	pd->use_gui = use_gui;
	pd->libdir = libdir != NULL ? strdup(libdir) : NULL;

	set_instance(pd);

	if (pd->libdir != NULL)
	{
		char extra_path[1040];
		snprintf(extra_path, sizeof(extra_path), "%s/extra", pd->libdir);
		libpd_add_to_search_path(extra_path);
	}

	// Note: use_gui and libdir are stored, but the GUI is not started here.
	// It is started lazily by the first call to libpds2_show_gui(), and stopped
	// again by libpds2_hide_gui(). This way instances that are never edited
	// don't use an extra Tcl/Tk process.

	return pd;
}

// Not threadsafe. If having several errors at once, the function could return strange result.
char *libpds2_strerror(void)
{
	return error_string;
}

void libpds2_delete(pd_t *pd)
{
	if (pd == NULL)
		return;

	lock_process(pd);

	if (pd->gui_started)
	{
		set_instance(pd);
		libpd_stop_gui();
		pd->gui_started = false;
	}

	libpd_free_instance(pd->instance);

	free(pd->in_scratch);
	free(pd->out_scratch);
	free(pd->libdir);

	// Note: intentionally not unlocking before freeing pd. It's the host's
	// responsibility to not call any libpds2 function for this instance after
	// libpds2_delete returns.
	free(pd);
}

void libpds2_set_hook_data(pd_t *pd, void* data)
{
  pd->hook_data = data;
  // Official libpd hooks don't take a data argument, so store the pointer as
  // per-instance data instead. Hook implementations retrieve it with
  // libpd_get_instancedata().
  set_instance(pd);
  libpd_set_instancedata(data, NULL);
}

void libpds2_clear_search_path(pd_t *pd)
{
	set_instance(pd);
	libpd_clear_search_path();
}

void libpds2_add_to_search_path(pd_t *pd, const char* sym)
{
	set_instance(pd);
	libpd_add_to_search_path(sym);
}

void* libpds2_openfile(pd_t *pd, const char* basename, const char* dirname)
{
	set_instance(pd);
	return libpd_openfile(basename, dirname);
}

void libpds2_closefile(pd_t *pd, void* p)
{
	set_instance(pd);
	libpd_closefile(p);
}

// Saves the patch to the given file. Sends the same "savetofile" message to
// the canvas that the GUI's Save menu item sends. Must be called from a
// non-realtime thread since it allocates memory and does file I/O.
void libpds2_savefile(pd_t *pd, void* p, const char *filename, const char *dirname)
{
	if (p == NULL || filename == NULL || dirname == NULL)
		return;

	set_instance(pd);

	t_atom argv[2];
	SETSYMBOL(argv+0, gensym(filename));
	SETSYMBOL(argv+1, gensym(dirname));

	sys_lock();
	pd_typedmess((t_pd*)p, gensym("savetofile"), 2, argv);
	sys_unlock();
}

int libpds2_getdollarzero(pd_t *pd, void* p)
{
	set_instance(pd);
	return libpd_getdollarzero(p);
}

#if defined(__linux__)
// Returns true if "name" is an executable file found in PATH.
// (The Pd GUI on Linux runs the "wish" command found in PATH.)
static bool executable_in_path(const char *name)
{
	const char *path = getenv("PATH");
	if (path == NULL)
		return false;

	const char *p = path;
	while (true)
	{
		const char *end = strchr(p, ':');
		size_t len = end != NULL ? (size_t)(end - p) : strlen(p);

		char filename[4096];
		if (len < sizeof(filename))
		{
			if (len == 0)
				snprintf(filename, sizeof(filename), "%s", name);
			else
				snprintf(filename, sizeof(filename), "%.*s/%s", (int)len, p, name);

			if (access(filename, X_OK) == 0)
				return true;
		}

		if (end == NULL)
			break;
		p = end + 1;
	}

	return false;
}
#endif

// Starts the Pd Tcl/Tk GUI for this instance if it's not already running.
// Starting the GUI makes all the instance's canvas windows visible (the GUI
// handshake ends up in sys_doneglobinit(), which calls canvas_vis(x, 1) for
// all canvases).
//
// Note: libpd_start_gui() blocks forever inside accept() if the GUI process
// never manages to connect, so make sure that a GUI actually can be started
// before calling it.
void libpds2_show_gui(pd_t *pd)
{
	set_instance(pd);

	if (pd->gui_started || !pd->use_gui || pd->libdir == NULL)
		return;

	{
		char gui_script[1040];
		snprintf(gui_script, sizeof(gui_script), "%s/tcl/pd-gui.tcl", pd->libdir);
		if (access(gui_script, R_OK) != 0)
		{
			set_error("Unable to start the Pd GUI: can't find the tcl/pd-gui.tcl file.");
			return;
		}
	}

#if defined(__linux__)
	{
		const char *display = getenv("DISPLAY");
		if (display == NULL || display[0] == 0)
		{
			set_error("Unable to start the Pd GUI: DISPLAY is not set.");
			return;
		}

		if (!executable_in_path("wish"))
		{
			set_error("Unable to start the Pd GUI: the \"wish\" program (Tcl/Tk) was not found in PATH.");
			return;
		}
	}
#endif

	if (libpd_start_gui(pd->libdir) == 0)
		pd->gui_started = true;
	else
		set_error("Unable to start the Pd GUI.");
}

// Stops the GUI process for this instance. Editing state is kept in the Pd
// canvas, so nothing is lost, but the Tcl/Tk process has to be started again
// by the next libpds2_show_gui() call.
void libpds2_hide_gui(pd_t *pd)
{
	set_instance(pd);

	if (pd->gui_started)
	{
		libpd_stop_gui();
		pd->gui_started = false;
	}
}

// Processes incoming GUI/network messages and sends queued GUI updates for
// this instance. Processing incoming messages can allocate memory and run
// arbitrary patch code, so this must be called regularly from a non-realtime
// thread (e.g. the host's GUI thread). The process functions only flush
// already-buffered GUI output, see the Radium patch in z_libpd.c.
int libpds2_poll_gui(pd_t *pd)
{
	set_instance(pd);
	return libpd_poll_gui();
}

// Like libpds2_poll_gui(), but never blocks: returns -1 if the instance is
// currently locked (for example by a realtime thread processing audio).
// This lets a host's GUI thread poll Pd without risk of blocking behind a
// realtime thread that is stuck. Note: sys_trylock() is declared in Pd's
// m_pd.h.
int libpds2_try_poll_gui(pd_t *pd)
{
	set_instance(pd);

	if (sys_trylock() != 0)
		return -1;

	int retval = sys_pollgui();
	sys_unlock();
	return retval;
}

int libpds2_blocksize(pd_t *pd)
{
	(void)pd;
	return libpd_blocksize();
}

int libpds2_init_audio(pd_t *pd, int inChans, int outChans, int sampleRate)
{
	lock_process(pd);

	pd->in_chans = inChans;
	pd->out_chans = outChans;

	free(pd->in_scratch);
	free(pd->out_scratch);
	pd->in_scratch = NULL;
	pd->out_scratch = NULL;

	set_instance(pd);
	int ret = libpd_init_audio(inChans, outChans, sampleRate);

	if (inChans > 0)
		pd->in_scratch = (float*)calloc((size_t)inChans * libpd_blocksize(), sizeof(float));
	if (outChans > 0)
		pd->out_scratch = (float*)calloc((size_t)outChans * libpd_blocksize(), sizeof(float));

	unlock_process(pd);

	return ret;
}

int libpds2_process_raw(pd_t *pd, const float* inBuffer, float* outBuffer)
{
	set_instance(pd);
	return libpd_process_raw(inBuffer, outBuffer);
}

int libpds2_process_float_noninterleaved(pd_t *pd, int ticks, const float** inBuffer, float** outBuffer)
{
	const int block = libpd_blocksize();

	// Serialize with other process calls for this instance. libpd's own lock
	// only protects each tick, while the scratch buffers below are accessed
	// outside of it. Different instances can be processed in parallel.
	lock_process(pd);

	// Lazy allocation in case libpds2_init_audio wasn't called. (or if it was called with other channel counts)
	if (pd->in_scratch == NULL && pd->in_chans > 0)
		pd->in_scratch = (float*)calloc((size_t)pd->in_chans * block, sizeof(float));
	if (pd->out_scratch == NULL && pd->out_chans > 0)
		pd->out_scratch = (float*)calloc((size_t)pd->out_chans * block, sizeof(float));
	if ((pd->in_chans > 0 && pd->in_scratch == NULL) ||
			(pd->out_chans > 0 && pd->out_scratch == NULL))
	{
		unlock_process(pd);
		return -1;
	}

	set_instance(pd);

	// Note: libpd_process_raw handles one tick. Convert to/from its
	// channel-major, single-tick buffer layout for each tick.
	for (int i = 0; i < ticks; i++)
	{
		for (int ch = 0; ch < pd->in_chans; ch++)
			memcpy(pd->in_scratch + ch * block, inBuffer[ch] + i * block, block * sizeof(float));

		int ret = libpd_process_raw(pd->in_scratch, pd->out_scratch);
		if (ret != 0)
		{
			unlock_process(pd);
			return ret;
		}

		for (int ch = 0; ch < pd->out_chans; ch++)
			memcpy(outBuffer[ch] + i * block, pd->out_scratch + ch * block, block * sizeof(float));
	}

	unlock_process(pd);

	return 0;
}

int libpds2_process_short(pd_t *pd, const int ticks, const short* inBuffer, short* outBuffer)
{
	set_instance(pd);
	return libpd_process_short(ticks, inBuffer, outBuffer);
}

int libpds2_process_float(pd_t *pd, int ticks, const float* inBuffer, float* outBuffer)
{
	set_instance(pd);
	return libpd_process_float(ticks, inBuffer, outBuffer);
}

int libpds2_process_double(pd_t *pd, int ticks, const double* inBuffer, double* outBuffer)
{
	set_instance(pd);
	return libpd_process_double(ticks, inBuffer, outBuffer);
}

int libpds2_arraysize(pd_t *pd, const char* name)
{
	set_instance(pd);
	return libpd_arraysize(name);
}

int libpds2_read_array(pd_t *pd, float* dest, const char* src, int offset, int n)
{
	set_instance(pd);
	return libpd_read_array(dest, src, offset, n);
}

int libpds2_write_array(pd_t *pd, const char* dest, int offset, float* src, int n)
{
	set_instance(pd);
	return libpd_write_array(dest, offset, src, n);
}

int libpds2_bang(pd_t *pd, const char* recv)
{
	set_instance(pd);
	return libpd_bang(recv);
}

int libpds2_float(pd_t *pd, const char* recv, float x)
{
	set_instance(pd);
	return libpd_float(recv, x);
}

int libpds2_symbol(pd_t *pd, const char* recv, const char* sym)
{
	set_instance(pd);
	return libpd_symbol(recv, sym);
}

void libpds2_set_float(pd_t *pd, t_atom* v, float x)
{
	(void)pd;
	libpd_set_float(v, x);
}

void libpds2_set_symbol(pd_t *pd, t_atom* v, const char* sym)
{
	(void)pd;
	libpd_set_symbol(v, sym);
}

int libpds2_list(pd_t *pd, const char* recv, int argc, t_atom* argv)
{
	set_instance(pd);
	return libpd_list(recv, argc, argv);
}

int libpds2_message(pd_t *pd, const char* recv, const char* msg, int argc, t_atom* argv)
{
	set_instance(pd);
	return libpd_message(recv, msg, argc, argv);
}

int libpds2_start_message(pd_t *pd, int max_length)
{
	set_instance(pd);
	return libpd_start_message(max_length);
}

void libpds2_add_float(pd_t *pd, float x)
{
	set_instance(pd);
	libpd_add_float(x);
}

void libpds2_add_symbol(pd_t *pd, const char* sym)
{
	set_instance(pd);
	libpd_add_symbol(sym);
}

int libpds2_finish_list(pd_t *pd, const char* recv)
{
	set_instance(pd);
	return libpd_finish_list(recv);
}

int libpds2_finish_message(pd_t *pd, const char* recv, const char* msg)
{
	set_instance(pd);
	return libpd_finish_message(recv, msg);
}

int libpds2_exists(pd_t *pd, const char* sym)
{
	set_instance(pd);
	return libpd_exists(sym);
}

void* libpds2_bind(pd_t *pd, const char* sym, void* data)
{
	set_instance(pd);
	// Note: official libpd_bind() doesn't take a data pointer. Hook
	// implementations must use libpd_this_instance() to find their plugin.
	(void)data;
	return libpd_bind(sym);
}

void libpds2_unbind(pd_t *pd, void* p)
{
	set_instance(pd);
	libpd_unbind(p);
}

void libpds2_set_printhook(pd_t *pd, t_libpd_printhook hook)
{
	set_instance(pd);
	libpd_set_printhook(hook);
}

void libpds2_set_banghook(pd_t *pd, t_libpd_banghook hook)
{
	set_instance(pd);
	libpd_set_banghook(hook);
}

void libpds2_set_floathook(pd_t *pd, t_libpd_floathook hook)
{
	set_instance(pd);
	libpd_set_floathook(hook);
}

void libpds2_set_symbolhook(pd_t *pd, t_libpd_symbolhook hook)
{
	set_instance(pd);
	libpd_set_symbolhook(hook);
}

void libpds2_set_listhook(pd_t *pd, t_libpd_listhook hook)
{
	set_instance(pd);
	libpd_set_listhook(hook);
}

void libpds2_set_messagehook(pd_t *pd, t_libpd_messagehook hook)
{
	set_instance(pd);
	libpd_set_messagehook(hook);
}

int libpds2_noteon(pd_t *pd, int channel, int pitch, int velocity)
{
	set_instance(pd);
	return libpd_noteon(channel, pitch, velocity);
}

int libpds2_controlchange(pd_t *pd, int channel, int controller, int value)
{
	set_instance(pd);
	return libpd_controlchange(channel, controller, value);
}

int libpds2_programchange(pd_t *pd, int channel, int value)
{
	set_instance(pd);
	return libpd_programchange(channel, value);
}

int libpds2_pitchbend(pd_t *pd, int channel, int value)
{
	set_instance(pd);
	return libpd_pitchbend(channel, value);
}

int libpds2_aftertouch(pd_t *pd, int channel, int value)
{
	set_instance(pd);
	return libpd_aftertouch(channel, value);
}

int libpds2_polyaftertouch(pd_t *pd, int channel, int pitch, int value)
{
	set_instance(pd);
	return libpd_polyaftertouch(channel, pitch, value);
}

int libpds2_midibyte(pd_t *pd, int port, int byte)
{
	set_instance(pd);
	return libpd_midibyte(port, byte);
}

int libpds2_sysex(pd_t *pd, int port, int byte)
{
	set_instance(pd);
	return libpd_sysex(port, byte);
}

int libpds2_sysrealtime(pd_t *pd, int port, int byte)
{
	set_instance(pd);
	return libpd_sysrealtime(port, byte);
}

void libpds2_set_noteonhook(pd_t *pd, t_libpd_noteonhook hook)
{
	set_instance(pd);
	libpd_set_noteonhook(hook);
}

void libpds2_set_controlchangehook(pd_t *pd, t_libpd_controlchangehook hook)
{
	set_instance(pd);
	libpd_set_controlchangehook(hook);
}

void libpds2_set_programchangehook(pd_t *pd, t_libpd_programchangehook hook)
{
	set_instance(pd);
	libpd_set_programchangehook(hook);
}

void libpds2_set_pitchbendhook(pd_t *pd, t_libpd_pitchbendhook hook)
{
	set_instance(pd);
	libpd_set_pitchbendhook(hook);
}

void libpds2_set_aftertouchhook(pd_t *pd, t_libpd_aftertouchhook hook)
{
	set_instance(pd);
	libpd_set_aftertouchhook(hook);
}

void libpds2_set_polyaftertouchhook(pd_t *pd, t_libpd_polyaftertouchhook hook)
{
	set_instance(pd);
	libpd_set_polyaftertouchhook(hook);
}

void libpds2_set_midibytehook(pd_t *pd, t_libpd_midibytehook hook)
{
	set_instance(pd);
	libpd_set_midibytehook(hook);
}
