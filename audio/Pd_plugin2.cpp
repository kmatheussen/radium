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
#ifdef WITH_PD2

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>

#include "../libpds/libpds.h"

// The old libpd API passed atoms by value, while the new official libpd API
// takes pointers to atoms. Keep the old call syntax.
#define libpd_is_float(a)   libpd_is_float(&(a))
#define libpd_is_symbol(a)  libpd_is_symbol(&(a))
#define libpd_get_float(a)  libpd_get_float(&(a))
#define libpd_get_symbol(a) libpd_get_symbol(&(a))

#if defined(FOR_LINUX)
#  include <glib.h>
#  define strlcpy g_strlcpy
#endif

#include <QTemporaryFile>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QTextStream>
#include <QVector>
#include <QCoreApplication>
#include <QTimer>
#include <QTimerEvent>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "undo_pd_controllers_proc.h"

#include "../common/OS_Player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_settings_proc.h"
#include "../common/patch_proc.h"
#include "../common/threading.h"
#include "../common/spinlock.h"
//#include "../common/PEQcommon_proc.h"
#include "../common/playerclass.h"
extern PlayerClass *pc;

#include "../Qt/Qt_pd_plugin_widget_callbacks_proc.h"

#include "../Qt/helpers.h"

#include "../midi/midi_proc.h"

#include "SoundPluginRegistry_proc.h"
#include "Mixer_proc.h"

#include "Qt_instruments_proc.h"

#include "Pd_plugin.h"
#include "Pd_plugin2_proc.h"

#include "Pd_plugin_proc.h"

#define NUM_NOTE_IDS (8192*4)

namespace
{

enum { NUM_PENDING_CONTROLLER_DEFINITIONS = 64 };

struct PendingControllerDefinition
{
	char name[PD_NAME_LENGTH];
	int type;
	float min_value;
	float value;
	float max_value;
};

struct Data
{
	pd_t *pd;

	Pd_Controller controllers[NUM_PD_CONTROLLERS];
	void *file;

	QTemporaryFile *pdfile;

	DEFINE_ATOMIC(void *, qtgui);

	struct Data *next;

	int largest_used_ids_pos;
	int ids_pos;
	int64_t note_ids[NUM_NOTE_IDS]; 

	// Controller values received from Pd while processing incoming GUI/network
	// messages on the GUI thread. They are applied by PD2_poll_all_guis after
	// the libpd lock has been released, since PLUGIN_set_effect_value takes
	// the player lock, which must not be acquired while holding the libpd lock.
	bool controller_has_pending_value[NUM_PD_CONTROLLERS];
	float controller_pending_value[NUM_PD_CONTROLLERS];

	// Controller definitions (radium_controller messages) are queued and
	// applied on the GUI thread (see PD2_apply_pending_controller_definitions),
	// so that all writes to those controller fields that the controller widgets
	// read directly (name, type, min_value, max_value, has_gui,
	// config_dialog_visible) happen on the GUI thread.
	PendingControllerDefinition pending_controller_definitions[NUM_PENDING_CONTROLLER_DEFINITIONS];
	int pending_controller_definitions_head;
	int pending_controller_definitions_tail;

	// Protects all reads and writes of the controllers[] table. The table is
	// read/written from the player thread (automation), runner threads (Pd
	// callbacks) and the GUI thread (polling and the controller widgets).
	// This is the innermost lock: never call into libpd (sys_lock) or take
	// the PLAYER lock while holding it.
	SPINLOCK_TYPE controllers_lock;
};


struct ScopedControllersLock
{
	Data *data;

	ScopedControllersLock(Data *data)
		: data(data)
	{
		SPINLOCK_OBTAIN(data->controllers_lock);
	}

	~ScopedControllersLock()
	{
		SPINLOCK_RELEASE(data->controllers_lock);
	}

	ScopedControllersLock(const ScopedControllersLock&) = delete;
	ScopedControllersLock& operator=(const ScopedControllersLock&) = delete;
};

}


static Data *g_instances = NULL; // protected by the player lock

// Interval, in milliseconds, between each time the GUI thread processes
// incoming GUI/network messages for all Pd2 instances. The player/runner
// threads only flush already-buffered GUI output (see the Radium patch in
// z_libpd.c), since processing incoming messages can allocate memory and run
// arbitrary patch code.
static const int g_poll_gui_interval = 15;

static void PD2_poll_all_guis(void);
static void PD2_apply_pending_controller_definitions(Data *data);
static void RT_bind_pending_receivers(Data *data);
static void apply_controller_value_from_pd(Pd_Controller *controller, float scaled_value);
static bool RT_has_player_context(void);

namespace
{

struct Pd2PollTimer : public QTimer
{
	void timerEvent(QTimerEvent *e) override
	{
		PD2_poll_all_guis();
	}
};

}

static Pd2PollTimer *g_poll_timer = NULL;

static void PD2_ensure_poll_timer(void)
{
	if(g_poll_timer == NULL)
	{
		g_poll_timer = new Pd2PollTimer();
		g_poll_timer->setInterval(g_poll_gui_interval);
	}

	if(!g_poll_timer->isActive())
		g_poll_timer->start();
}

// Processes incoming GUI/network messages for all instances. Called regularly
// from the GUI thread (see Pd2PollTimer). Also sends queued GUI updates, so
// instances without a GUI (e.g. patches using netreceive) are polled too.
static void PD2_poll_all_guis(void)
{
	R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

	enum { MAX_POLL_INSTANCES = 256 };
	Data *instances[MAX_POLL_INSTANCES];
	int num_instances = 0;

	// Note: g_instances is added to and removed from by create_plugin_data and
	// cleanup_plugin_data, which both run on the GUI thread (this function's
	// thread), so no locking is needed here. The player thread reads the list
	// while holding the player lock.
	for(Data *data = g_instances ; data != NULL && num_instances < MAX_POLL_INSTANCES ; data = data->next)
		instances[num_instances++] = data;

	for(int i=0 ; i<num_instances ; i++)
	{
		// Note: Use the non-blocking variant. If a player/runner thread holds
		// the libpd lock (for example because the debug allocator check
		// stopped it), libpds2_try_poll_gui returns -1 instead of blocking
		// this (GUI) thread. We then just try again on the next timer tick.
		bool polled = false;
		for(int j=0 ; j<8 ; j++)
		{
			int ret = libpds2_try_poll_gui(instances[i]->pd);
			if (ret < 0)
				break;

			polled = true;

			if (ret == 0)
				break;
		}

		// Apply controller definitions queued by RT_add_controller (from
		// runner threads during processing, and from the loadbang during patch
		// loading). This is done on the GUI thread so that all writes to the
		// controller fields that the controller widgets read directly happen
		// on the GUI thread.
		PD2_apply_pending_controller_definitions(instances[i]);

		// Bind receivers for controllers added by messages that were just
		// processed. (Binding allocates memory, so it can't be done from the
		// player/runner threads. See RT_add_controller.) Only do this if we
		// managed to take the libpd lock at least once, so that we don't
		// block behind a stopped realtime thread.
		if (polled)
			RT_bind_pending_receivers(instances[i]);

		// Apply controller values that were received while the libpd lock was
		// held. This is done here, after libpds2_try_poll_gui has released the
		// lock, since PLUGIN_set_effect_value takes the player lock.
		for(int j=0 ; j<NUM_PD_CONTROLLERS ; j++)
			if (instances[i]->controller_has_pending_value[j]) {
				instances[i]->controller_has_pending_value[j] = false;
				apply_controller_value_from_pd(&instances[i]->controllers[j], instances[i]->controller_pending_value[j]);
			}
	}
}

static int RT_add_note_id_pos(Data *data, int64_t note_id)
{
	int ids_pos = data->ids_pos;
	int ret = ids_pos;

	data->note_ids[ids_pos] = note_id;
	if(ids_pos > data->largest_used_ids_pos)
		data->largest_used_ids_pos = ids_pos;

	//printf("Added note_id %d to pos %d. Largest pos: %d\n",(int)note_id,ids_pos,data->largest_used_ids_pos);

	// The index is going to be stored in a float. Check that the float can contain the integer. (this code can be optimized, but it probably doesnt matter very much)
	float new_pos = ids_pos+1;
	while(ids_pos == (int)new_pos)
		new_pos++;

	ids_pos = new_pos;
	if(ids_pos>=NUM_NOTE_IDS)
		ids_pos = 0;

	data->ids_pos =ids_pos;

	return ret;
}

static int RT_get_note_id_pos(Data *data, int64_t note_id)
{
	int ids_pos = data->ids_pos;
	const int num_check_back = 64; // Could get in trouble if the number of simultaneously playing notes are higher than this number.

	if(ids_pos>num_check_back)
		for(int i=ids_pos-num_check_back ; i<ids_pos ; i++)
			if(data->note_ids[i]==note_id)
				return i;

	return RT_add_note_id_pos(data, note_id);
}

static int RT_get_legal_note_id_pos(Data *data, float ids_pos)
{
	int i_ids_pos = (int)ids_pos;

	if(i_ids_pos<0)
		return -1;
	else if(ids_pos>data->largest_used_ids_pos)
		return -1;
	else
		return data->note_ids[i_ids_pos];
}

// called from radium
static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;

	// Note: Receivers for controllers added from inside a Pd callback are
	// bound by PD2_poll_all_guis (which runs on the GUI thread), since binding
	// allocates memory and this is a realtime thread.

	libpds2_process_float_noninterleaved(pd, num_frames / libpds2_blocksize(pd), (const float**) inputs, outputs);
}

// called from radium
static void RT_play_note(struct SoundPlugin *plugin, int block_delta_time, note_t note)
{

	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;
	//printf("RT_play_note. %f %d (%f)\n",note_num,(int)(volume*127),volume);
	libpds2_noteon(pd, note.midi_channel, note.pitch, note.velocity*127);
	
	{
		t_atom v[5];
		
		SETFLOAT(v + 0, RT_add_note_id_pos(data, note.id));
		SETFLOAT(v + 1, note.pitch);
		SETFLOAT(v + 2, note.velocity);
		SETFLOAT(v + 3, note.pan);
		SETFLOAT(v + 4, block_delta_time);
		
		libpds2_list(pd, "radium_receive_note_on", 5, v);
	}
}

static void RT_stop_note(struct SoundPlugin *plugin, int block_delta_time, note_t note)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;
	libpds2_noteon(pd, note.midi_channel, note.pitch, 0);
	
	{
		t_atom v[3];
		
		SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
		SETFLOAT(v + 1, note.pitch);
		SETFLOAT(v + 2, block_delta_time);
		
		libpds2_list(pd, "radium_receive_note_off", 3, v);
	}
}

// called from radium
static void RT_set_note_volume(struct SoundPlugin *plugin, int block_delta_time, note_t note)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;
	libpds2_polyaftertouch(pd, note.midi_channel, note.pitch, note.velocity*127);

	{
		t_atom v[4];

		SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
		SETFLOAT(v + 1, note.pitch);
		SETFLOAT(v + 2, note.velocity);
		SETFLOAT(v + 3, block_delta_time);
		
		libpds2_list(pd, "radium_receive_velocity", 4, v);
	}
}

// called from radium
static void RT_set_note_pitch(struct SoundPlugin *plugin, int block_delta_time, note_t note)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;

	{
		t_atom v[4]; 

		SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
		SETFLOAT(v + 1, note.pitch);
		SETFLOAT(v + 2, note.pitch);
		SETFLOAT(v + 3, block_delta_time);
		
		libpds2_list(pd, "radium_receive_pitch", 4, v);
	}
}

static void RT_send_raw_midi_message(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;

	if (MIDI_msg_byte1_remove_channel(msg)==0xb0)
		libpds2_controlchange(pd, MIDI_msg_byte1_get_channel(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg));
}

// called from radium
void RT_PD2_set_absolute_time(int64_t time)
{
	if(g_instances != NULL)
	{
		t_atom v[3];
		int sample_rate = MIXER_get_sample_rate();

		SETFLOAT(v + 0, int(time / sample_rate));
		SETFLOAT(v + 1, time % sample_rate);
		SETFLOAT(v + 2, sample_rate);

		Data *instance = g_instances;
		while(instance != NULL)
		{
			libpds2_list(instance->pd, "radium_time", 3, v);
			instance = instance->next;
		}
	} 
}

// called from radium
void RT_PD2_set_realline(int64_t time, int64_t time_nextsubline, const Place *p)
{

	if(g_instances != NULL)
	{
		t_atom v[8];
		int sample_rate = MIXER_get_sample_rate();

		SETFLOAT(v + 0, int(time / sample_rate));
		SETFLOAT(v + 1, time % sample_rate);
		SETFLOAT(v + 2, sample_rate);
		SETFLOAT(v + 3, p->line);
		SETFLOAT(v + 4, p->counter);
		SETFLOAT(v + 5, p->dividor);
		int64_t duration = time_nextsubline-time;
		SETFLOAT(v + 6, int(duration / sample_rate));
		SETFLOAT(v + 7, duration % sample_rate);

		Data *instance = g_instances;
		while(instance != NULL)
		{
			libpds2_list(instance->pd, "radium_visibleline", 8, v);
			instance = instance->next;
		}
	}
}

// called from radium
static void RT_set_effect_value(struct SoundPlugin *plugin, int block_delta_time, int effect_num, float value, enum ValueFormat value_format, FX_when when)
{
	Data *data = (Data*)plugin->data;
	pd_t *pd = data->pd;
	Pd_Controller *controller = &data->controllers[effect_num];
	float real_value;
	bool has_name;
	bool calling_from_pd;
	char name[PD_NAME_LENGTH];
	char fx_when_name[PD_FX_WHEN_NAME_LENGTH];

	{
		ScopedControllersLock lock(data);

		if(value_format==EFFECT_FORMAT_SCALED && controller->type!=EFFECT_FORMAT_BOOL)
			real_value = scale(value, 0.0, 1.0, 
							   controller->min_value, controller->max_value);
		else
			real_value = value;

		safe_float_write(&controller->value, real_value);

		has_name = strcmp(controller->name, "") != 0;
		calling_from_pd = controller->calling_from_pd;

		snprintf(name, sizeof(name), "%s", controller->name);

		switch(when)
		{
		case FX_start:
			snprintf(fx_when_name, sizeof(fx_when_name), "%s", controller->fx_when_start_name);
			break;
		case FX_middle:
			snprintf(fx_when_name, sizeof(fx_when_name), "%s", controller->fx_when_middle_name);
			break;
		case FX_end:
			snprintf(fx_when_name, sizeof(fx_when_name), "%s", controller->fx_when_end_name);
			break;
		case FX_single:
			snprintf(fx_when_name, sizeof(fx_when_name), "%s", controller->fx_when_single_name);
			break;
		default:
			fx_when_name[0] = 0;
			break;
		}

		controller->calling_from_set_effect_value = true;
	}

	if(has_name && false==calling_from_pd)
	{
		//printf("####################################################### Setting pd volume to %f / real_value: %f, for -%s-. Coming-from-pd: %d\n",value, real_value,name,calling_from_pd);

		switch(when)
		{
		case FX_start:
		case FX_middle:
		case FX_end:
		case FX_single:
			libpds2_bang(pd, fx_when_name);
			break;
		default:
			RT_message("Unknown when value: %d",when);
		}

		libpds2_float(pd, name, real_value);
	}

	{
		ScopedControllersLock lock(data);
		controller->calling_from_set_effect_value = false;
	}
}

// called from radium
static float RT_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format)
{
	Data *data = (Data*)plugin->data;
	float raw;
	int type;
	float min_value;
	float max_value;

	{
		ScopedControllersLock lock(data);
		Pd_Controller *controller = &data->controllers[effect_num];
		raw = controller->value;
		type = controller->type;
		min_value = controller->min_value;
		max_value = controller->max_value;
	}

	if(value_format==EFFECT_FORMAT_SCALED && type!=EFFECT_FORMAT_BOOL)
		return scale(raw, min_value, max_value, 0.0f, 1.0f);
	else
		return raw;
}

// called from radium
static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize)
{
	Data *data = (Data*)plugin->data;
	char name[PD_NAME_LENGTH];
	int type;
	float value;

	{
		ScopedControllersLock lock(data);
		Pd_Controller *controller = &data->controllers[effect_num];
		snprintf(name, sizeof(name), "%s", controller->name);
		type = controller->type;
		value = safe_float_read(&controller->value);
	}

	if(type==EFFECT_FORMAT_FLOAT)
		snprintf(buffer,buffersize-1,"%s: %f",!strcmp(name,"")?"<not set>":name, value);
	else
		snprintf(buffer,buffersize-1,"%s: %d",!strcmp(name,"")?"<not set>":name, (int)value);
}

// called from radium
static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui)
{
	Data *data = (Data*)plugin->data;
	//printf("####################################################### Showing Pd gui\n");

	// Note: Not called while holding the player lock. Starting the Pd GUI
	// allocates memory, and libpd's own lock makes this safe with regards to
	// the player thread. (Otherwise Radium's debug allocator check would stop
	// here since the player lock is held.)
	libpds2_show_gui(data->pd);

	// Note: The new libpds starts and stops the Pd GUI, so Pd does not send
	// gui_is_visible when the GUI is shown. Update the checkbox state here
	// instead. (Pd does send gui_is_hidden when the user closes the Pd window,
	// see libpd_patches/07-canvas-menuclose-hides.patch.)
	PDGUI_is_visible(ATOMIC_GET(data->qtgui));

	return true;
}

// called from radium
static void save_file(SoundPlugin *plugin)
{
	Data *data=(Data*)plugin->data;
	if (data->pdfile == NULL)
		return;

	// Saving allocates memory and does file I/O, so this must not be called
	// while holding the player lock.
	QFileInfo qfileinfo(data->pdfile->fileName());
	libpds2_savefile(data->pd, data->file,
									 qfileinfo.fileName().toUtf8().constData(),
									 qfileinfo.absolutePath().toUtf8().constData());
}

// called from radium
static void hide_gui(struct SoundPlugin *plugin)
{
	Data *data = (Data*)plugin->data;
	//printf("####################################################### Showing Pd gui\n");

	// Note: See show_gui. Not called while holding the player lock.
	libpds2_hide_gui(data->pd);
	save_file(plugin);

	PDGUI_is_hidden(ATOMIC_GET(data->qtgui));
}

// called from Pd
static Pd_Controller *RT_get_controller_from_receiver(Data *data, const char *receiver_name)
{
	ScopedControllersLock lock(data);

	for(int i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		Pd_Controller *controller = &data->controllers[i];
		char name[PD_NAME_LENGTH+20];
		snprintf(name, sizeof(name), "%s-receiver", controller->name);
		if(!strcmp(name, receiver_name))
			return controller;
	}
	return NULL;
}

// Applies a value received from Pd for a controller. Must be called either
// with player context, or from the GUI thread after the libpd lock has been
// released (see PD2_poll_all_guis).
static void apply_controller_value_from_pd(Pd_Controller *controller, float scaled_value)
{
	Data *data = (Data*)controller->plugin->data;

	{
		ScopedControllersLock lock(data);
		controller->calling_from_pd = true;
	}

#if !defined(RELEASE)
	radium::ScopedBoolean scoped(g_calling_set_effect_value_from_pd);
#endif
	PLUGIN_set_effect_value(controller->plugin, -1, controller->num, scaled_value, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);

	{
		ScopedControllersLock lock(data);
		controller->calling_from_pd = false;
	}
}

// called from Pd
static void RT_pdfloathook(const char *sym, float val)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	Data *data = (Data*)plugin->data;
	if(data==NULL)
		return;

	Pd_Controller *controller = RT_get_controller_from_receiver(data, sym);
	if(controller==NULL)
		return;

	float scaled_value = 0.0f;

	{
		ScopedControllersLock lock(data);

		if (controller->calling_from_set_effect_value)
			return;

		scaled_value = scale(val, controller->min_value, controller->max_value,
							 0.0f, 1.0f);
		
		scaled_value = R_BOUNDARIES(0.0f, scaled_value, 1.0f);
	}

	if (RT_has_player_context()) {

		bool is_player_or_runner_thread = THREADING_is_player_or_runner_thread();
		if (is_player_or_runner_thread)
			RT_PLAYER_runner_lock();

		apply_controller_value_from_pd(controller, scaled_value);

		if (is_player_or_runner_thread)
			RT_PLAYER_runner_unlock();

	} else {

		// Called from the GUI thread while polling incoming GUI/network
		// messages, i.e. with the libpd lock held. PLUGIN_set_effect_value
		// takes the player lock, and the player thread takes this
		// instance's libpd lock while holding the player lock, so applying
		// it here could deadlock. Defer until the libpd lock is released.
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		data->controller_pending_value[controller->num] = scaled_value;
		data->controller_has_pending_value[controller->num] = true;
	}
}

// Called from radium. Must not be called from inside a Pd callback (where
// libpd_bind would deadlock on the libpd lock), and must not be called from
// a realtime thread since binding allocates memory.
static void RT_bind_receiver(Pd_Controller *controller)
{
	Data *data = (Data*)controller->plugin->data;

	char receive_symbol_name[PD_NAME_LENGTH+20];
	char name[PD_NAME_LENGTH];
	char fx_when_start_name[PD_FX_WHEN_NAME_LENGTH];
	char fx_when_middle_name[PD_FX_WHEN_NAME_LENGTH];
	char fx_when_end_name[PD_FX_WHEN_NAME_LENGTH];
	char fx_when_single_name[PD_FX_WHEN_NAME_LENGTH];

	{
		ScopedControllersLock lock(data);
		snprintf(receive_symbol_name, sizeof(receive_symbol_name), "%s-receiver", controller->name);
		snprintf(name, sizeof(name), "%s", controller->name);
		snprintf(fx_when_start_name, sizeof(fx_when_start_name), "%s", controller->fx_when_start_name);
		snprintf(fx_when_middle_name, sizeof(fx_when_middle_name), "%s", controller->fx_when_middle_name);
		snprintf(fx_when_end_name, sizeof(fx_when_end_name), "%s", controller->fx_when_end_name);
		snprintf(fx_when_single_name, sizeof(fx_when_single_name), "%s", controller->fx_when_single_name);
	}

	controller->pd_binding = libpds2_bind(data->pd, receive_symbol_name, controller);

	// Pre-intern the receiver names used by RT_set_effect_value, so that
	// gensym() doesn't need to allocate when bang/floats are sent from the
	// player thread later on.
	libpds2_exists(data->pd, name);
	libpds2_exists(data->pd, fx_when_start_name);
	libpds2_exists(data->pd, fx_when_middle_name);
	libpds2_exists(data->pd, fx_when_end_name);
	libpds2_exists(data->pd, fx_when_single_name);
}

// Called from radium (i.e. not from inside a Pd callback, where libpd_bind
// would deadlock on the libpd lock). Called from the GUI thread (see
// PD2_poll_all_guis) since binding allocates memory.
static void RT_bind_pending_receivers(Data *data)
{
	for(int i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		Pd_Controller *controller = &data->controllers[i];
		bool has_name;
		{
			ScopedControllersLock lock(data);
			has_name = controller->name[0] != 0;
		}
		if(has_name && controller->pd_binding == NULL)
			RT_bind_receiver(controller);
	}
}

// Pre-intern the global receiver names that Radium sends to Pd, so that
// gensym() doesn't need to allocate when messages are sent from the player
// thread later on. Must be called outside the player lock (e.g. from create_data).
static void RT_intern_global_symbols(Data *data)
{
	static const char *names[] = {
		"radium_receive_note_on",
		"radium_receive_note_off",
		"radium_receive_velocity",
		"radium_receive_pitch",
		"radium_time",
		"radium_visibleline"
	};

	for(unsigned int i=0 ; i<sizeof(names)/sizeof(names[0]) ; i++)
		libpds2_exists(data->pd, names[i]);
}

// Called from Pd (i.e. with the libpd lock held). Only queues the controller
// definition; it is applied later on the GUI thread by
// PD2_apply_pending_controller_definitions.
static void RT_add_controller(SoundPlugin *plugin, Data *data, const char *controller_name, int type, float min_value, float value, float max_value)
{
	char name[PD_NAME_LENGTH];
	snprintf(name, sizeof(name), "%s", controller_name);

	ScopedControllersLock lock(data);

	// If the same controller is already queued, update that entry instead of
	// adding another one. (Keeps the queue small and matches the old behavior
	// of rewriting an already existing controller.)
	for(int i = data->pending_controller_definitions_tail ; i != data->pending_controller_definitions_head ; i = (i + 1) % NUM_PENDING_CONTROLLER_DEFINITIONS)
	{
		PendingControllerDefinition *def = &data->pending_controller_definitions[i];
		if(!strcmp(def->name, name))
		{
			def->type = type;
			def->min_value = min_value;
			def->value = value;
			def->max_value = max_value;
			return;
		}
	}

	int head = data->pending_controller_definitions_head;
	int next_head = (head + 1) % NUM_PENDING_CONTROLLER_DEFINITIONS;

	if (next_head == data->pending_controller_definitions_tail)
		return; // Queue is full. Drop the definition. (Should not happen.)

	PendingControllerDefinition *def = &data->pending_controller_definitions[head];

	snprintf(def->name, sizeof(def->name), "%s", name);
	def->type = type;
	def->min_value = min_value;
	def->value = value;
	def->max_value = max_value;

	data->pending_controller_definitions_head = next_head;
}

static void apply_controller_definition(Data *data, const PendingControllerDefinition *def)
{
	{
		ScopedControllersLock lock(data);

		Pd_Controller *controller = NULL;
		int controller_num;

		for(controller_num=0;controller_num<NUM_PD_CONTROLLERS;controller_num++)
		{
			controller = &data->controllers[controller_num];

			if(controller->name[0]!=0 && !strcmp(controller->name, def->name))
				break;

			if(controller->name[0] == 0 || !strcmp(controller->name, ""))
				break;
		}

		if(controller_num==NUM_PD_CONTROLLERS)
			return;

		float min_value = def->min_value;
		float max_value = def->max_value;

		if (fabs(min_value-max_value) < 0.0001f)
		{
			if(fabs(min_value) < 0.001f)
				max_value = 1.0f;
			else
			{
				min_value = def->value;
				max_value = def->value + 1.0f;
			}
		}

		controller->type = def->type;
		controller->min_value = min_value;
		controller->value = def->value;
		controller->max_value = max_value;  

		strlcpy(controller->name, def->name, PD_NAME_LENGTH-1);
		snprintf(controller->fx_when_start_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_start", def->name);
		snprintf(controller->fx_when_middle_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_middle", def->name);
		snprintf(controller->fx_when_end_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_end", def->name);
		snprintf(controller->fx_when_single_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_single", def->name);

		controller->has_gui = true;
	}

	// Note: The receiver is not bound here. Binding allocates memory and takes
	// the libpd lock, so it's deferred to RT_bind_pending_receivers.
	PDGUI_schedule_clearing(ATOMIC_GET(data->qtgui));
}

// Called from the GUI thread.
static void PD2_apply_pending_controller_definitions(Data *data)
{
	while(true)
	{
		PendingControllerDefinition def;
		{
			ScopedControllersLock lock(data);

			if (data->pending_controller_definitions_tail == data->pending_controller_definitions_head)
				break;

			def = data->pending_controller_definitions[data->pending_controller_definitions_tail];
			data->pending_controller_definitions_tail = (data->pending_controller_definitions_tail + 1) % NUM_PENDING_CONTROLLER_DEFINITIONS;
		}

		apply_controller_definition(data, &def);
	}
}


// Note that hooks are always called from the player thread.
// called from Pd
static void RT_pdmessagehook(const char *source, const char *controller_name, int argc, t_atom *argv)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	Data *data = (Data*)plugin->data;
	
	if( !strcmp(source, "libpd"))
	{
		//printf("controller_name: -%s-\n",controller_name);
		if(!strcmp(controller_name, "gui_is_visible"))
			PDGUI_is_visible(ATOMIC_GET(data->qtgui));
		else if(!strcmp(controller_name, "gui_is_hidden"))
			PDGUI_is_hidden(ATOMIC_GET(data->qtgui));
		return;
	}
}

// called from Pd
static bool is_bang(t_atom atom)
{
	return libpd_is_symbol(atom); // && atom.a_w.w_symbol==&s_bang;
}

// Returns true if the current thread can safely send messages to the
// player/sequencer. Hooks are normally called from the player/runner threads,
// but since the Pd GUI is polled from the GUI thread (see PD2_poll_all_guis),
// they can also be called from there.
static bool RT_has_player_context(void)
{
	return THREADING_is_player_or_runner_thread() || PLAYER_current_thread_has_lock();
}

// called from Pd
static void RT_pdlisthook(const char *recv, int argc, t_atom *argv)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	Data *data = (Data*)plugin->data;
	int sample_rate = MIXER_get_sample_rate();
	struct Patch *patch = (struct Patch*)plugin->patch;
	
	//printf("argc: %d\n",argc);
	//printf("recv: %s\n",recv);

	// The radium_send_* messages manipulate the player/sequencer. They are
	// normally sent by the patch while audio is processed (i.e. from a
	// player/runner thread), but can also arrive while polling GUI messages
	// from the GUI thread. Ignore them in that case.
	if( !strncmp(recv, "radium_send_", 12) && !RT_has_player_context())
	{
#if !defined(RELEASE)
		printf("Ignoring %s message from a thread without player context\n", recv);
#endif
		return;
	}

	if( !strcmp(recv, "radium_controller"))
	{
		if(argc==5 &&
			 libpd_is_symbol(argv[0]) && strcmp("", libpd_get_symbol(argv[0])) &&
			 libpd_is_float(argv[1]) &&
			 libpd_is_float(argv[2]) &&
			 libpd_is_float(argv[3]) &&
			 libpd_is_float(argv[4]))
			{
				const char *name = libpd_get_symbol(argv[0]);
				int    type      = libpd_get_float(argv[1]);
				float  min_value = libpd_get_float(argv[2]);
				float  value     = libpd_get_float(argv[3]);
				float  max_value = libpd_get_float(argv[4]);
				//printf("Got something: -%s-, %d, %f, %f, %f\n", name, type, min_value, value, max_value);
				
				if(type==EFFECT_FORMAT_FLOAT || type==EFFECT_FORMAT_INT || type==EFFECT_FORMAT_BOOL)
					RT_add_controller(plugin, data, name, type, min_value, value, max_value);
				else
					printf("Unknown type: -%d-\n",type);
			}

	}
	else if( !strcmp(recv, "radium_send_note_on"))
	{
		if(argc==6 &&
			 patch!=NULL &&
			 RT_do_send_MIDI_to_receivers(plugin) &&
			 (libpd_is_float(argv[0]) || is_bang(argv[0])) &&
			 libpd_is_float(argv[1]) &&
			 libpd_is_float(argv[2]) &&
			 libpd_is_float(argv[3]) &&
			 libpd_is_float(argv[4]) &&
			 libpd_is_float(argv[5]))
			{
				int64_t note_id  = is_bang(argv[0]) ? -1 : RT_get_legal_note_id_pos(data, libpd_get_float(argv[0]));
				float   pitch    = libpd_get_float(argv[1]);
				float   velocity = libpd_get_float(argv[2]);
				float   pan      = libpd_get_float(argv[3]);
				float   seconds  = libpd_get_float(argv[4]);
				int     frames   = libpd_get_float(argv[5]);
				int64_t time     = seconds*sample_rate + frames;
				
				RT_PLAYER_runner_lock();{
					struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
					RT_PATCH_send_play_note_to_receivers(seqtrack, patch, create_note_t(NULL, note_id, pitch, velocity, pan, 0, 0, 0), time);
				}RT_PLAYER_runner_unlock();
			}
		else
			printf("Wrong args for radium_send_note_on\n");

	}
	else if( !strcmp(recv, "radium_send_note_off"))
	{
		if(argc==4 &&
			 patch!=NULL &&
			 RT_do_send_MIDI_to_receivers(plugin) &&
			 (libpd_is_float(argv[0]) || (is_bang(argv[0]))) &&
			 libpd_is_float(argv[1]) &&
			 libpd_is_float(argv[2]) &&
			 libpd_is_float(argv[3]))
			{
				int64_t note_id = is_bang(argv[0]) ? -1 : RT_get_legal_note_id_pos(data, libpd_get_float(argv[0]));
				float   pitch   = libpd_get_float(argv[1]);
				float   seconds = libpd_get_float(argv[2]);
				int     frames  = libpd_get_float(argv[3]);
				int64_t time    = seconds*sample_rate + frames;
				RT_PLAYER_runner_lock();{
					struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
					RT_PATCH_send_stop_note_to_receivers(seqtrack, patch, create_note_t2(NULL, note_id, pitch), time);
				}RT_PLAYER_runner_unlock();
			}
		else
			printf("Wrong args for radium_send_note_off\n");

	}
	else if( !strcmp(recv, "radium_send_velocity"))
	{
		if(argc==5 &&
			 patch!=NULL &&
			 RT_do_send_MIDI_to_receivers(plugin) &&
			 (libpd_is_float(argv[0]) || (is_bang(argv[0]))) &&
			 libpd_is_float(argv[1]) &&
			 libpd_is_float(argv[2]) &&
			 libpd_is_float(argv[3]) &&
			 libpd_is_float(argv[4]))
			{
				int64_t note_id  = is_bang(argv[0]) ? -1 : RT_get_legal_note_id_pos(data, libpd_get_float(argv[0]));
				float   notenum  = libpd_get_float(argv[1]);
				float   velocity = libpd_get_float(argv[2]);
				float   seconds  = libpd_get_float(argv[3]);
				int     frames   = libpd_get_float(argv[4]);
				int64_t time     = seconds*sample_rate + frames;
				//printf("send_velocity. id: %d, argv[0]: %f, notenum: %f, velocity: %f, seconds: %f, frames: %d\n",(int)note_id,libpd_get_float(argv[0]),notenum,velocity,seconds,frames);
				RT_PLAYER_runner_lock();{
					struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
					RT_PATCH_send_change_velocity_to_receivers(seqtrack, patch, create_note_t(NULL, note_id, notenum, velocity, 0, 0, 0, 0), time);
				}RT_PLAYER_runner_unlock();
			}
		else
			printf("Wrong args for radium_send_velocity\n");

	}
	else if( !strcmp(recv, "radium_send_pitch"))
	{
		if(argc==5 &&
			 patch!=NULL &&
			 RT_do_send_MIDI_to_receivers(plugin) &&
			 (libpd_is_float(argv[0]) || (is_bang(argv[0]))) &&
			 libpd_is_float(argv[1]) &&
			 libpd_is_float(argv[2]) &&
			 libpd_is_float(argv[3]) &&
			 libpd_is_float(argv[4]))
			{
				int64_t note_id = is_bang(argv[0]) ? -1 : RT_get_legal_note_id_pos(data, libpd_get_float(argv[0]));
				float   notenum = libpd_get_float(argv[1]);
				float   pitch   = libpd_get_float(argv[2]);
				float   seconds = libpd_get_float(argv[3]);
				int     frames  = libpd_get_float(argv[4]);
				int64_t time    = seconds*sample_rate + frames;
				RT_PLAYER_runner_lock();{
					struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
					RT_PATCH_send_change_pitch_to_receivers(seqtrack, patch, create_note_t(NULL, note_id, notenum, 0, pitch, 0, 0, 0), time);
				}RT_PLAYER_runner_unlock();
			}
		else
			printf("Wrong args for radium_send_pitch\n");

	}
	else if( !strcmp(recv, "radium_send_blockreltempo"))
	{
		if(argc==1 &&
			 libpd_is_float(argv[0]))
			{
				double tempo = libpd_get_float(argv[0]);

				if(tempo>100 || tempo <0.0001)
				{
					printf("Illegal tempo: %f. Must be between 0.0001 and 100.\n",tempo);
					tempo = R_BOUNDARIES(0.0001, tempo, 100);
				}
				
				{
					struct SeqBlock *curr_seqblock = RT_get_curr_seqblock();

					if (curr_seqblock != NULL)
					{
						struct Blocks *block = curr_seqblock->block;

						if (block!=NULL && !equal_doubles(tempo, ATOMIC_DOUBLE_GET(block->reltempo)))
						{
							ATOMIC_DOUBLE_SET(block->reltempo, tempo);
							GFX_ScheduleRedraw();
							//printf("   SCHEDULING redraw\n");
						}
					}
				}
			}
		else
			printf("Wrong args for radium_send_blockreltempo\n");

	}
}

// Schedules a function that needs player context to run on the player thread.
// Used by hooks that can be called from the GUI thread while polling incoming
// GUI/network messages (see PD2_poll_all_guis).
static void RT_schedule_on_player_thread(std::function<void(void)> func)
{
	radium::Scheduled_RT_functions rt_functions;
	rt_functions.add(func);
	rt_functions.schedule_to_run_on_player_thread();
}

static void RT_noteonhook_apply(SoundPlugin *plugin, int channel, int pitch, int velocity)
{
	volatile struct Patch *patch = plugin->patch;
	
	if(patch==NULL)
		return;

	if (!RT_do_send_MIDI_to_receivers(plugin))
		return;
		
	RT_PLAYER_runner_lock();{
		struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
		if(velocity>0)
			RT_PATCH_send_play_note_to_receivers(seqtrack, (struct Patch*)patch, create_note_t(NULL, -1, pitch, (float)velocity / 127.0f, 0.0f, channel, 0, 0), -1);
		else
			RT_PATCH_send_stop_note_to_receivers(seqtrack, (struct Patch*)patch, create_note_t(NULL, -1, pitch, 0, 0, channel, 0, 0), -1);
	}RT_PLAYER_runner_unlock();
		
	//  printf("Got note on %d %d %d (%p) %f\n",channel,pitch,velocity,d,(float)velocity / 127.0f);
}

// called from Pd
static void RT_noteonhook(int channel, int pitch, int velocity)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	if (!RT_has_player_context()) {
		// Called from the GUI thread while polling incoming GUI/network
		// messages. Note scheduling has to happen on the player thread.
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		RT_schedule_on_player_thread([plugin, channel, pitch, velocity](){
			RT_noteonhook_apply(plugin, channel, pitch, velocity);
		});
		return;
	}

	RT_noteonhook_apply(plugin, channel, pitch, velocity);
}

static void RT_polyaftertouchhook_apply(SoundPlugin *plugin, int channel, int pitch, int velocity)
{
	volatile struct Patch *patch = plugin->patch;
	
	if(patch==NULL)
		return;

	if (!RT_do_send_MIDI_to_receivers(plugin))
		return;
		
	RT_PLAYER_runner_lock();{
		struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
		RT_PATCH_send_change_velocity_to_receivers(seqtrack, (struct Patch*)patch, create_note_t(NULL, -1, pitch, (float)velocity / 127.0f, 0, channel, 0, 0), -1);
	}RT_PLAYER_runner_unlock();
	
	//printf("Got poly aftertouch %d %d %d (%p)\n",channel,pitch,velocity,d);
}

// called from Pd
static void RT_polyaftertouchhook(int channel, int pitch, int velocity)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	if (!RT_has_player_context()) {
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		RT_schedule_on_player_thread([plugin, channel, pitch, velocity](){
			RT_polyaftertouchhook_apply(plugin, channel, pitch, velocity);
		});
		return;
	}

	RT_polyaftertouchhook_apply(plugin, channel, pitch, velocity);
}

static void RT_controlchangehook_apply(SoundPlugin *plugin, int channel, int cc, int value)
{
	struct Patch *patch = plugin->patch;
	
	//printf("Got MIDI control %x %x %x (%p)\n",channel,cc,value,d);
	
	if(patch==NULL)
		return;

	if (patch->patchdata==NULL) // Happens when loading song.
		return;

	if (RT_do_send_MIDI_to_receivers(plugin))
	{
		struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
		RT_PATCH_send_raw_midi_message_to_receivers(seqtrack, patch, MIDI_msg_pack3(0xb0 | channel, cc, value), -1);
	}
}

// called from Pd
static void RT_controlchangehook(int channel, int cc, int value)
{
	SoundPlugin *plugin = (SoundPlugin*)libpd_get_instancedata();
	if(plugin==NULL)
		return;

	if (!RT_has_player_context()) {
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());
		RT_schedule_on_player_thread([plugin, channel, cc, value](){
			RT_controlchangehook_apply(plugin, channel, cc, value);
		});
		return;
	}

	bool is_player_or_runner_thread = THREADING_is_player_or_runner_thread();
	
	if (is_player_or_runner_thread)
		RT_PLAYER_runner_lock();
	else
	{
		R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());
	}

	RT_controlchangehook_apply(plugin, channel, cc, value);
		
	if (is_player_or_runner_thread)
		RT_PLAYER_runner_unlock();
}

static QTemporaryFile *create_temp_pdfile()
{
	QString destFileNameTemplate = QDir::tempPath()+QDir::separator()+"radium_XXXXXX.pd";
	return new QTemporaryFile(destFileNameTemplate);
}

static QTemporaryFile *get_pdfile_from_state(hash_t *state)
{
	QTemporaryFile *pdfile = create_temp_pdfile();
	
	if (!pdfile->open())
	{
		GFX_Message(NULL, "Error. Unable to create temporary file. Disk full?");
	}
	else
	{
		QTextStream out(pdfile);
		int num_lines = HASH_get_int(state, "num_lines");
		
		for(int i=0; i<num_lines; i++)
			out << STRING_get_qstring(HASH_get_string_at(state, "line", i)) + "\n";

		pdfile->close();
	}
	
	return pdfile;
}

// http://www.java2s.com/Code/Cpp/Qt/Readtextfilelinebyline.htm
static void put_pdfile_into_state(const SoundPlugin *plugin, QFile *file, hash_t *state)
{
	if (file == NULL)
		return;
	
	// Make sure any edits done in the Pd GUI are written to the file before
	// reading it. Note: Saving allocates memory and does file I/O, so this
	// must not be done while holding the player lock.
	save_file((SoundPlugin*)plugin);

	if (!file->open(QIODevice::ReadOnly | QIODevice::Text))
	{
		GFX_Message(NULL, "Unable to open temporary Pd file for reading");
		return;
	}
	
	QTextStream in(file);
	
	int i=0;
	QString line = in.readLine();
	while (!line.isNull())
	{
		//printf("line: -%s-\n",line.toUtf8().constData());
		HASH_put_string_at(state, "line", i, STRING_create(line));
		i++;
		line = in.readLine();
	}
	
	HASH_put_int(state, "num_lines", i);
	
	file->close();
}

static QString get_search_path()
{
	return STRING_get_qstring(OS_get_full_program_file_path("pd").id);
}

static Data *create_data(QTemporaryFile *pdfile, struct SoundPlugin *plugin, float sample_rate, int block_size)
{
	R_ASSERT(pdfile != NULL);
	
	Data *data = (Data*)V_calloc(1,sizeof(Data));
	
	data->largest_used_ids_pos = -1;

	SPINLOCK_INIT(data->controllers_lock);

	int i;
	for(i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		data->controllers[i].display_name = NULL;
		data->controllers[i].plugin = plugin;
		data->controllers[i].num = i;
		data->controllers[i].max_value = 1.0f;
	}

	int blocksize;
	pd_t *pd;

	char puredatapath[1024];
	snprintf(puredatapath,1023,"%s/packages/libpd/pure-data",OS_get_program_path());
	pd = libpds2_create(true, puredatapath);
	if(pd==NULL)
	{
		ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
		msgBox->setText(QString(libpds2_strerror()));
		msgBox->setStandardButtons(QMessageBox::Ok);
		safeExec(msgBox, false); // set program_state_is_valid to false. Not sure if it's safe to set it to true.
		V_free(data);
		return NULL;
	}

	data->pd = pd;

	QString search_path = get_search_path();
	libpds2_add_to_search_path(pd, search_path.toUtf8().constData());

	libpds2_set_floathook(pd, RT_pdfloathook);
	libpds2_set_messagehook(pd, RT_pdmessagehook);
	libpds2_set_listhook(pd, RT_pdlisthook);

	libpds2_set_hook_data(pd, plugin);

	libpds2_set_noteonhook(pd, RT_noteonhook);
	libpds2_set_polyaftertouchhook(pd, RT_polyaftertouchhook);
	libpds2_set_controlchangehook(pd, RT_controlchangehook);

	libpds2_init_audio(pd, plugin->type->num_inputs, plugin->type->num_outputs, sample_rate);
		
	blocksize = libpds2_blocksize(pd);

	if( (block_size % blocksize) != 0)
		GFX_Message(NULL, "PD's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", blocksize, block_size);

	// compute audio    [; pd dsp 1(
	libpds2_start_message(pd, 1); // one entry in list
	libpds2_add_float(pd, 1.0f);
	libpds2_finish_message(pd, "pd", "dsp");

	plugin->data = data; // plugin->data is used before this function ends. (No, only data, seems like. We can send 'data' instead of 'plugin' to the hooks.) (well, PD2_recreate_controllers_from_state uses plugin->data)

	libpds2_bind(pd, "radium_controller", plugin);
	libpds2_bind(pd, "radium_send_note_on", plugin);
	libpds2_bind(pd, "radium_send_note_off", plugin);
	libpds2_bind(pd, "radium_send_velocity", plugin);
	libpds2_bind(pd, "radium_send_pitch", plugin);
	libpds2_bind(pd, "radium_send_blockreltempo", plugin);
	libpds2_bind(pd, "libpd", plugin);

	data->pdfile = pdfile;

	QFileInfo qfileinfo(pdfile->fileName());
	printf("name: %s, dir: %s\n",qfileinfo.fileName().toUtf8().constData(), qfileinfo.absolutePath().toUtf8().constData());
	
	data->file = libpds2_openfile(pd, qfileinfo.fileName().toUtf8().constData(), qfileinfo.absolutePath().toUtf8().constData());
	
	R_ASSERT(data->file != NULL);
	if (data->file==NULL)
		return NULL;

	// Controllers defined by the patch during load (loadbang) were queued by
	// RT_add_controller since it's called from inside a Pd callback. Apply and
	// bind them here, after libpd_openfile has released the libpd lock.
	PD2_apply_pending_controller_definitions(data);
	RT_bind_pending_receivers(data);
	RT_intern_global_symbols(data);
	
	return data;
}

static QTemporaryFile *create_new_tempfile(QString *fileName)
{
	// create
	QFile source(*fileName);
	QTemporaryFile *pdfile = create_temp_pdfile();

	// open
	if (!pdfile->open())
	{
		GFX_Message(NULL, "Error. Unable to create tempfile \"%s\". Disk full?", fileName->toUtf8().constData());
	}
	else if (!source.open(QIODevice::ReadOnly))
	{
		GFX_Message(NULL, "Error. Unable to create temporary file. Disk full?");
	}
	else
	{
		printf("filename: -%s-\n",pdfile->fileName().toUtf8().constData());

		// copy
		pdfile->write(source.readAll());
	}

	// close
	if (pdfile->isOpen())
		pdfile->close();

	if (source.isOpen())
		source.close();

	return pdfile;
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading)
{
	QTemporaryFile *pdfile;

	if (state==NULL)
		pdfile = create_new_tempfile((QString *)plugin_type->data);
	else
		pdfile = get_pdfile_from_state(state);

	Data *data = create_data(pdfile, plugin, sample_rate, block_size);

	if(state!=NULL)
		PD2_recreate_controllers_from_state(plugin, state);

	PLAYER_lock();{
		data->next = g_instances;    
		g_instances = data;
	}PLAYER_unlock();

	PD2_ensure_poll_timer();

	plugin->num_visible_outputs = 2;
	plugin->automatically_set_num_visible_outputs = true;
	
	return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin)
{
	Data *data = (Data*)plugin->data;
	printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);

	// remove element from g_instances
	{
		Data *prev = NULL;
		Data *instance = g_instances;
		while(instance != data)
		{
			prev = instance;
			instance = instance->next;
		}
		PLAYER_lock();{
			if(prev==NULL)
				g_instances = instance->next;
			else
				prev->next = instance->next;
		}PLAYER_unlock();
	}

	if(g_poll_timer != NULL && g_instances == NULL)
		g_poll_timer->stop();
	
	libpds2_closefile(data->pd, data->file);  
	libpds2_delete(data->pd);

	// Not necessary anymore since widgets are now deleted. (and it's not very healthy to call PDGUI_clear on a deleted widget)
	//PDGUI_clear(ATOMIC_GET(data->qtgui));

	delete data->pdfile;

	for(int i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		Pd_Controller *controller = &data->controllers[i];
		free((void*)controller->display_name);
	}
	
	V_free(data);
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num)
{
	Data *data = (Data*)plugin->data;
	ScopedControllersLock lock(data);
	Pd_Controller *controller = &data->controllers[effect_num];

	return controller->type;
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num)
{
	Data *data = (Data*)plugin->data;

	// Return a per-thread copy instead of a pointer into the mutable
	// controllers table. The small ring of buffers makes it safe to use the
	// result of several calls at the same time on the same thread.
	static __thread char name_buffers[8][PD_NAME_LENGTH];
	static __thread int name_buffer_pos = 0;

	char *ret = name_buffers[name_buffer_pos];
	name_buffer_pos = (name_buffer_pos + 1) % (int)(sizeof(name_buffers)/sizeof(name_buffers[0]));

	{
		ScopedControllersLock lock(data);
		Pd_Controller *controller = &data->controllers[effect_num];

		if (controller->name[0] == 0)
			snprintf(ret, PD_NAME_LENGTH, " %d", effect_num);
		else
			snprintf(ret, PD_NAME_LENGTH, "%s", controller->name);
	}

	return ret;
}

void PD2_set_qtgui(SoundPlugin *plugin, void *qtgui)
{
	Data *data = (Data*)plugin->data;
	ATOMIC_SET(data->qtgui, qtgui);
}

Pd_Controller *PD2_get_controller(SoundPlugin *plugin, int n)
{
	Data *data = (Data*)plugin->data;
	return &data->controllers[n];
}

void PD2_set_controller_type(SoundPlugin *plugin, int n, int type)
{
	Data *data = (Data*)plugin->data;
	ScopedControllersLock lock(data);
	data->controllers[n].type = type;
}

void PD2_set_controller_min_max(SoundPlugin *plugin, int n, float min_value, float max_value)
{
	Data *data = (Data*)plugin->data;
	ScopedControllersLock lock(data);
	data->controllers[n].min_value = min_value;
	data->controllers[n].max_value = max_value;
}

static bool controller_name_exists(Data *data, const char *name)
{
	ScopedControllersLock lock(data);

	for(int i=0;i<NUM_PD_CONTROLLERS;i++)
		if(!strcmp(name, data->controllers[i].name))
			return true;

	return false;
}

const wchar_t *PD2_set_controller_name(SoundPlugin *plugin, int n, const wchar_t *wname)
{
	Data *data = (Data*)plugin->data;
	Pd_Controller *controller = &data->controllers[n];
	const char *name = STRING_get_chars(wname);
	
	{
		ScopedControllersLock lock(data);
		if(!strcmp(controller->name, name))
			return wname;
	}

	if (controller_name_exists(data, name))
	{
		char old_name[PD_NAME_LENGTH];
		{
			ScopedControllersLock lock(data);
			snprintf(old_name, sizeof(old_name), "%s", controller->name);
		}

		showAsyncMessage(talloc_format("A controller \"%S\" already exists", wname));
		return STRING_create(old_name);
	}
	
	controller->display_name = wcsdup(wname);
	
	// Should check if it is different before binding. (but also if it is already binded)

	ADD_UNDO(PdControllers_CurrPos(const_cast<struct Patch*>(plugin->patch)));

	{
		// Only the name is copied here. Unbinding/binding allocates memory
		// (and libpd_unbind takes the libpd lock), which is not allowed while
		// holding the controllers lock.
		ScopedControllersLock lock(data);
		strlcpy(controller->name, name, PD_NAME_LENGTH-1);
	}

	if(controller->pd_binding != NULL)
		libpds2_unbind(data->pd, controller->pd_binding);

	RT_bind_receiver(controller);

	return wname;
}

void PD2_recreate_controllers_from_state(SoundPlugin *plugin, const hash_t *state)
{
	Data *data=(Data*)plugin->data;

	PDGUI_clear(ATOMIC_GET(data->qtgui));

	int i;
	for(i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		Pd_Controller *controller = &data->controllers[i];
		bool has_name;

		if(controller->pd_binding!=NULL)
		{
			libpds2_unbind(data->pd, controller->pd_binding);
			controller->pd_binding = NULL;
		}

		controller->display_name = NULL;
		if (HASH_has_key_at(state, "name", i))
		{
			const wchar_t *name = HASH_get_string_at(state, "name", i);
			controller->display_name = wcsdup(name);
		}

		// Read the state outside the controllers lock, so that possible
		// assertions/messages from the hash functions are not run while
		// holding a spinlock.
		const char *name = controller->display_name == NULL ? NULL : STRING_get_chars(controller->display_name);
		int type = HASH_get_int_at(state, "type", i);
		float min_value = HASH_get_float_at(state, "min_value", i);
		float value = HASH_get_float_at(state, "value", i);
		float max_value = HASH_get_float_at(state, "max_value", i);
		bool has_gui = HASH_get_int_at(state, "has_gui", i)==1 ? true : false;
		bool config_dialog_visible = HASH_get_int_at(state, "config_dialog_visible", i)==1 ? true : false;

		{
			ScopedControllersLock lock(data);

			if(name==NULL || !strcmp(name,""))
				controller->name[0] = 0;
			else
				strlcpy(controller->name, name, PD_NAME_LENGTH-1);
		
			controller->type      = type;
			controller->min_value = min_value;
			controller->value = value;
			controller->max_value = max_value;
			controller->has_gui   = has_gui;
			controller->config_dialog_visible = config_dialog_visible;

			has_name = controller->name[0] != 0;
		}

		if(has_name)
		{
			RT_bind_receiver(controller);
		}
	}

	volatile struct Patch *patch = plugin->patch;
	if(patch != NULL)
		GFX_update_instrument_widget((struct Patch*)patch);
}

void PD2_put_controllers_to_state(const SoundPlugin *plugin, hash_t *state)
{
	Data *data=(Data*)plugin->data;

	int i;
	for(i=0;i<NUM_PD_CONTROLLERS;i++)
	{
		Pd_Controller *controller = &data->controllers[i];
		const wchar_t *display_name;
		int type;
		float min_value;
		float value;
		float max_value;
		int has_gui;
		int config_dialog_visible;
		char name[PD_NAME_LENGTH];

		{
			ScopedControllersLock lock(data);

			display_name = controller->display_name;
			type = controller->type;
			min_value = controller->min_value;
			value = controller->value;
			max_value = controller->max_value;
			has_gui = controller->has_gui ? 1 : 0;
			config_dialog_visible = controller->config_dialog_visible ? 1 : 0;
			snprintf(name, sizeof(name), "%s", controller->name);
		}

		if (display_name != NULL)
			HASH_put_string_at(state, "name", i, display_name);
		else
			HASH_put_chars_at(state, "name", i, name);
		HASH_put_int_at(state, "type", i, type);
		HASH_put_float_at(state, "min_value", i, min_value);
		HASH_put_float_at(state, "value", i, value);
		HASH_put_float_at(state, "max_value", i, max_value);
		HASH_put_int_at(state, "has_gui", i, has_gui);
		HASH_put_int_at(state, "config_dialog_visible", i, config_dialog_visible);
	}
}

static void create_state(const struct SoundPlugin *plugin, hash_t *state)
{
	printf("\n\n\n ********** CREATE_STATE ************* \n\n\n");
	Data *data = (Data*)plugin->data;

	PD2_put_controllers_to_state(plugin, state);

	put_pdfile_into_state(plugin, data->pdfile, state);
}

// Warning! undo is created here (for simplicity). It's not common to call the undo creation function here, so beware of possible circular dependencies in the future.
void PD2_delete_controller(SoundPlugin *plugin, int controller_num)
{
	Data *data=(Data*)plugin->data;

	R_ASSERT_RETURN_IF_FALSE(plugin->patch!=NULL);
	
	ADD_UNDO(PdControllers_CurrPos((struct Patch*)plugin->patch));

	int i;
	hash_t *state = HASH_create(NUM_PD_CONTROLLERS);

	for(i=0;i<NUM_PD_CONTROLLERS-1;i++)
	{
		int s = i>=controller_num ? i+1 : i;
		Pd_Controller *controller = &data->controllers[s];
		const wchar_t *display_name;
		int type;
		float min_value;
		float value;
		float max_value;
		int has_gui;
		int config_dialog_visible;

		{
			ScopedControllersLock lock(data);
			display_name = controller->display_name;
			type = controller->type;
			min_value = controller->min_value;
			value = controller->value;
			max_value = controller->max_value;
			has_gui = controller->has_gui;
			config_dialog_visible = controller->config_dialog_visible;
		}

		HASH_put_string_at(state, "name", i, display_name == NULL ? L"" : display_name);
		HASH_put_int_at(state, "type", i, type);
		HASH_put_float_at(state, "min_value", i, min_value);
		HASH_put_float_at(state, "value", i, value);
		HASH_put_float_at(state, "max_value", i, max_value);
		HASH_put_int_at(state, "has_gui", i, has_gui);
		HASH_put_int_at(state, "config_dialog_visible", i, config_dialog_visible);
	}

	HASH_put_chars_at(state, "name", NUM_PD_CONTROLLERS-1, "");
	HASH_put_int_at(state, "type", NUM_PD_CONTROLLERS-1, EFFECT_FORMAT_FLOAT);
	HASH_put_float_at(state, "min_value", NUM_PD_CONTROLLERS-1, 0.0);
	HASH_put_float_at(state, "value", NUM_PD_CONTROLLERS-1, 0.0);
	HASH_put_float_at(state, "max_value", NUM_PD_CONTROLLERS-1, 1.0);
	HASH_put_int_at(state, "has_gui", NUM_PD_CONTROLLERS-1, 0);
	HASH_put_int_at(state, "config_dialog_visible", i, 0);

	PD2_recreate_controllers_from_state(plugin, state);
}


static void add_plugin(const wchar_t *name, QString filename)
{
	SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

	plugin_type->type_name                = "Pd2";
	plugin_type->name                     = V_strdup(STRING_get_chars(name));
	plugin_type->info                     = "Pd2 is made by Miller Puckette. Radium uses the official libpd library to access it.\nlibpd is made by Peter Brinkmann and the libpd team.";
	plugin_type->num_inputs               = 16;
	plugin_type->num_outputs              = 16;
	plugin_type->is_instrument            = true;
	plugin_type->note_handling_is_RT      = false;
	plugin_type->num_effects              = NUM_PD_CONTROLLERS;
	plugin_type->will_never_autosuspend   = true;
	plugin_type->get_effect_format        = get_effect_format;
	plugin_type->get_effect_name          = get_effect_name;
	plugin_type->effect_is_RT             = NULL;
	plugin_type->create_plugin_data       = create_plugin_data;
	plugin_type->cleanup_plugin_data      = cleanup_plugin_data;

	plugin_type->show_gui         = show_gui;
	plugin_type->hide_gui         = hide_gui;

	plugin_type->RT_process       = RT_process;
	plugin_type->play_note        = RT_play_note;
	plugin_type->set_note_volume  = RT_set_note_volume;
	plugin_type->set_note_pitch   = RT_set_note_pitch;
	plugin_type->send_raw_midi_message   = RT_send_raw_midi_message;
	plugin_type->stop_note        = RT_stop_note;
	plugin_type->set_effect_value = RT_set_effect_value;
	plugin_type->get_effect_value = RT_get_effect_value;
	plugin_type->get_display_value_string = get_display_value_string;

	//plugin_type->recreate_from_state = recreate_from_state;
	plugin_type->create_state        = create_state;

	plugin_type->data                = (void*)new QString(filename);

	//PR_add_menu_entry(PluginMenuEntry::normal(plugin_type));
	if(STRING_equals(name,""))
		PR_add_plugin_type_no_menu(plugin_type);
	else
		PR_add_plugin_type(plugin_type);
}

static void build_plugins(QDir dir, const char *menu_name)
{
	printf(">> dir: -%s-\n",dir.absolutePath().toUtf8().constData());
	PR_add_menu_entry(PluginMenuEntry::level_up(menu_name != NULL ? menu_name : dir.dirName().toUtf8().constData()));

	dir.setSorting(QDir::Name);

	// browse dirs first.
	dir.setFilter(QDir::Dirs |  QDir::NoDotAndDotDot);

	{
		QFileInfoList list = dir.entryInfoList();
		
		for (int i = 0; i < list.size(); ++i)
		{
			QFileInfo fileInfo = list.at(i);
			//if(fileInfo.absoluteFilePath() != dir.absoluteFilePath())
				build_plugins(QDir(fileInfo.absoluteFilePath()), NULL);
		}
	}

		// Then the files
	dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
	{
		QFileInfoList list = dir.entryInfoList();
		
		for (int i = 0; i < list.size(); ++i)
		{
			QFileInfo fileInfo = list.at(i);
			printf("   file: -%s-\n",fileInfo.absoluteFilePath().toUtf8().constData());
			if(fileInfo.suffix()=="pd")
			{
				if(fileInfo.baseName()==QString("New_Audio_Effect"))
					add_plugin(STRING_create(""), fileInfo.absoluteFilePath());
				add_plugin(STRING_create(fileInfo.baseName().replace("_"," ")), fileInfo.absoluteFilePath());
			}
		}
	}

	printf("<< dir: -%s-\n",dir.absolutePath().toUtf8().constData());
	PR_add_menu_entry(PluginMenuEntry::level_down());
}

void create_pd2_plugin(void)
{
	build_plugins(QDir(get_search_path()+OS_get_directory_separator()+"Pd"), "Pd2");
}


#endif // WITH_PD2
