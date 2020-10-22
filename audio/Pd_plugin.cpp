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

Serious bug:
============
* Fixed for now by doing:

diff --git a/Qt/Qt_instruments.cpp b/Qt/Qt_instruments.cpp
index 738826d..b9aaadd 100644
--- a/Qt/Qt_instruments.cpp
+++ b/Qt/Qt_instruments.cpp
@@ -763,7 +763,7 @@ void GFX_remove_patch_gui(struct Patch *patch){
 
 void GFX_update_instrument_patch_gui(struct Patch *patch){
   printf("Called GFX_update_instrument_patch_gui for patch \"%s\"\n",patch==NULL?"<>":patch->name);
-  if(patch!=NULL && patch->instrument->PP_Update!=NULL)
+  if(patch!=NULL && patch->patchdata!=NULL && patch->instrument->PP_Update!=NULL)
     patch->instrument->PP_Update(patch->instrument,
                                  patch);

But I'm not sure if this is a proper fix.



Terminal output and backtrace:


####################################################### Setting pd volume to 0.000000 / real_value: 0.000000, for -frequency-
*(((( EFFNUM_OUTPUT_VOLUME. val: -0.002502. MIN_DB: -40. Target: 0.999424. plugin->volume: 0.999712
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
####################################################### Setting pd volume to 0.000000 / real_value: 0.000000, for -frequency-
Calling Undo_do for 74. Current value: 0.000000. Now setting it back to 1.000000
*(((( EFFNUM_OUTPUT_VOLUME. val: -0.002502. MIN_DB: -40. Target: 0.999424. plugin->volume: 0.999712
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
hookCalling Undo_do for 74. Current value: 1.000000. Now setting it back to 0.000000
*(((( EFFNUM_OUTPUT_VOLUME. val: -0.002502. MIN_DB: -40. Target: 0.999424. plugin->volume: 0.999712
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Calling Undo_do for 0. Current value: 0.008900. Now setting it back to 0.163000
####################################################### Setting pd volume to 0.163000 / real_value: 0.163000, for -volume-
*(((( EFFNUM_OUTPUT_VOLUME. val: -0.002502. MIN_DB: -40. Target: 0.999424. plugin->volume: 0.999712
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
####################################################### Setting pd volume to 0.163000 / real_value: 0.163000, for -volume-
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
*** undo_do_track called. Tracknum: 0
Called GFX_update_instrument_patch_gui for patch "<>"
Called GFX_update_instrument_patch_gui for patch "Pd 2"
Calling Undo_do for 2. Current value: 0.532300. Now setting it back to 0.000000
####################################################### Setting pd volume to 0.000000 / real_value: 0.000000, for -volume2-
*(((( EFFNUM_OUTPUT_VOLUME. val: -0.002502. MIN_DB: -40. Target: 0.999424. plugin->volume: 0.999712
Called GFX_update_instrument_patch_gui for patch "<>"
Called GFX_update_instrument_patch_gui for patch "Pd 1"
0 set to 1

Program received signal SIGSEGV, Segmentation fault.
0x00000000004a9b9e in Audio_instrument_widget::Audio_instrument_widget (this=0x3149210, parent=0x114c380, patch=0x13c6540) at Qt/Qt_audio_instrument_widget_callbacks.h:96
96	    if(plugin->type->play_note==NULL)
Missing separate debuginfos, use: debuginfo-install OpenEXR-libs-1.7.0-4.fc17.x86_64 boost-system-1.48.0-14.fc17.x86_64 cyrus-sasl-lib-2.1.23-31.fc17.x86_64 fftw-libs-single-3.3.3-5.fc17.x86_64 flac-1.2.1-9.fc17.x86_64 glibc-2.15-58.fc17.x86_64 glibmm24-2.32.1-1.fc17.x86_64 gsm-1.0.13-6.fc17.x86_64 ilmbase-1.0.2-4.fc17.x86_64 jamin-0.97.16-3.20111031cvs.fc17.x86_64 kdelibs-4.10.5-1.fc17.x86_64 ladspa-calf-plugins-0.0.18.6-6.fc17.x86_64 ladspa-guitarix-plugins-0.25.2-1.fc17.x86_64 ladspa-wasp-plugins-0.9.5.1-2.fc17.x86_64 libcurl-7.24.0-6.fc17.x86_64 libidn-1.24-1.fc17.x86_64 liblo-0.26-4.fc17.x86_64 liblrdf-0.5.0-3.fc17.x86_64 libsigc++20-2.2.10-2.fc17.x86_64 libssh2-1.4.1-2.fc17.x86_64 libvorbis-1.3.3-1.fc17.x86_64 libxml2-2.7.8-9.fc17.x86_64 libxslt-1.1.26-10.fc17.x86_64 nspr-4.9.6-1.fc17.x86_64 nss-3.14.3-1.fc17.x86_64 nss-softokn-freebl-3.14.3-1.fc17.x86_64 nss-util-3.14.3-1.fc17.x86_64 openldap-2.4.33-3.fc17.x86_64 qt-4.8.4-17.fc17.x86_64 qt-x11-4.8.4-17.fc17.x86_64 qt4-theme-quarticurve-0.0-0.17.beta8.fc17.x86_64 raptor2-2.0.7-1.fc17.x86_64 xz-libs-5.1.2-1alpha.fc17.x86_64 yajl-2.0.4-1.fc17.x86_64
(gdb) p plugin
$1 = (SoundPlugin *) 0x0
(gdb) bt
#0  0x00000000004a9b9e in Audio_instrument_widget::Audio_instrument_widget (this=0x3149210, parent=0x114c380, patch=0x13c6540) at Qt/Qt_audio_instrument_widget_callbacks.h:96
#1  0x0000000000480762 in create_audio_instrument_widget (patch=0x13c6540) at Qt/Qt_instruments.cpp:373
#2  0x000000000048142c in GFX_PP_Update (patch=0x13c6540) at Qt/Qt_instruments.cpp:714
#3  0x00000000005935ac in AUDIO_PP_Update (instrument=0x10c9e60, patch=0x13c6540) at audio/audio_instrument.c:315
#4  0x00000000004815f8 in GFX_update_instrument_patch_gui (patch=0x13c6540) at Qt/Qt_instruments.cpp:768
#5  0x0000000000464444 in Undo () at common/undo.c:307
#6  0x00000000004ce809 in undo () at api/api_undo.c:37
#7  0x000000000043da6c in ER_gotKey (key=72, a=2, down=true) at common/eventreciever.c:187
#8  0x000000000043dc56 in EventTreater (in_tevent=0x99d6b0, window=0x132ec40) at common/eventreciever.c:241
#9  0x000000000043e0a3 in EventReciever (in_tevent=0x99d6b0, window=0x132ec40) at common/eventreciever.c:352
#10 0x00000000004d06d5 in X11Event_KeyPress (keynum=72, keystate=4, window=0x132ec40) at X11/X11_keyboard.c:274
#11 0x00000000004d0768 in X11_KeyPress (event=0x7fffffffc790, window=0x132ec40) at X11/X11_keyboard.c:285
#12 0x00000000004d091c in X11_KeyboardFilter (event=0x7fffffffc790) at X11/X11_keyboard.c:332
#13 0x000000000046ade2 in MyApplication::x11EventFilter (this=0xe94e40, event=0x7fffffffc790) at Qt/Qt_Main.cpp:107
#14 0x000000334aa3681c in ?? () from /lib64/libQtGui.so.4
#15 0x000000334aa43e1b in QApplication::x11ProcessEvent(_XEvent*) () from /lib64/libQtGui.so.4
#16 0x000000334aa6a90c in ?? () from /lib64/libQtGui.so.4
#17 0x00007ffff7cf3825 in g_main_dispatch (context=0xe96c40) at gmain.c:2539
#18 g_main_context_dispatch (context=context@entry=0xe96c40) at gmain.c:3075
#19 0x00007ffff7cf3b58 in g_main_context_iterate (context=context@entry=0xe96c40, block=block@entry=1, dispatch=dispatch@entry=1, self=<optimized out>) at gmain.c:3146
#20 0x00007ffff7cf3c14 in g_main_context_iteration (context=0xe96c40, may_block=1) at gmain.c:3207
#21 0x000000334a3a5e96 in QEventDispatcherGlib::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#22 0x000000334aa6a5ee in ?? () from /lib64/libQtGui.so.4
#23 0x000000334a37651f in QEventLoop::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#24 0x000000334a3767a8 in QEventLoop::exec(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#25 0x000000334a37b638 in QCoreApplication::exec() () from /lib64/libQtCore.so.4
#26 0x0000000000469f8b in radium_main (arg=0x7ffff10c052c "") at Qt/Qt_Main.cpp:530
#27 0x00000000004cba06 in init_radium (arg=0x7ffff10c052c "", gkf=<function at remote 0x7ffff0cf3aa0>) at api/api_common.c:61
#28 0x00000000004c6a38 in _wrap_init_radium (self=0x0, args=('', <function at remote 0x7ffff0cf3aa0>)) at api/radium_wrap.c:572
#29 0x00000039c1edd0e1 in call_function (oparg=<optimized out>, pp_stack=0x7fffffffce78) at /usr/src/debug/Python-2.7.3/Python/ceval.c:4098
#30 PyEval_EvalFrameEx (f=f@entry=Frame 0x102c7c0, for file /home/kjetil/radium-qt4/bin/start.py, line 137, in <module> (), throwflag=throwflag@entry=0) at /usr/src/debug/Python-2.7.3/Python/ceval.c:2740
#31 0x00000039c1eddb1f in PyEval_EvalCodeEx (co=co@entry=0x7ffff0fdf0b0, globals=globals@entry=
    {'false': 0, 'keyhandles': [<KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000ea8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000d40>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000d88>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0ceaf38>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0ceafc8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4050>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4098>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf40e0>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4128>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4170>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf41b8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4200>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4248>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4290>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf42d8>, <KeyHandler(keyslist=...(truncated), 
    locals=locals@entry=
    {'false': 0, 'keyhandles': [<KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000ea8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000d40>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff1000d88>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0ceaf38>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0ceafc8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4050>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4098>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf40e0>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4128>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4170>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf41b8>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4200>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4248>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf4290>, <KeyHandler(keyslist=[], handlers=[]) at remote 0x7ffff0cf42d8>, <KeyHandler(keyslist=...(truncated), 
    args=args@entry=0x0, argcount=argcount@entry=0, kws=kws@entry=0x0, kwcount=kwcount@entry=0, defs=defs@entry=0x0, defcount=defcount@entry=0, closure=closure@entry=0x0)
    at /usr/src/debug/Python-2.7.3/Python/ceval.c:3330
#32 0x00000039c1eddbf2 in PyEval_EvalCode (co=co@entry=0x7ffff0fdf0b0, globals=globals@entry=

                                */

#ifdef WITH_PD

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>

#include <libpds.h>

#include <QTemporaryFile>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QTextStream>
#include <QVector>
#include <QCoreApplication>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "undo_pd_controllers_proc.h"

#include "../common/OS_Player_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_settings_proc.h"
#include "../common/patch_proc.h"
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

#include "Pd_plugin_proc.h"

#define NUM_NOTE_IDS (8192*4)

namespace {
  
struct Data{
  pd_t *pd;

  Pd_Controller controllers[NUM_PD_CONTROLLERS];
  void *file;

  QTemporaryFile *pdfile;

  DEFINE_ATOMIC(void *, qtgui);

  struct Data *next;

  int largest_used_ids_pos;
  int ids_pos;
  int64_t note_ids[NUM_NOTE_IDS]; 
};

}


static Data *g_instances = NULL; // protected by the player lock

static int RT_add_note_id_pos(Data *data, int64_t note_id){
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

static int RT_get_note_id_pos(Data *data, int64_t note_id){
  int ids_pos = data->ids_pos;
  const int num_check_back = 64; // Could get in trouble if the number of simultaneously playing notes are higher than this number.

  if(ids_pos>num_check_back)
    for(int i=ids_pos-num_check_back ; i<ids_pos ; i++)
      if(data->note_ids[i]==note_id)
        return i;

  return RT_add_note_id_pos(data, note_id);
}

static int RT_get_legal_note_id_pos(Data *data, float ids_pos){
  int i_ids_pos = (int)ids_pos;

  if(i_ids_pos<0)
    return -1;
  else if(ids_pos>data->largest_used_ids_pos)
    return -1;
  else
    return data->note_ids[i_ids_pos];
}

// called from radium
static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;

  libpds_process_float_noninterleaved(pd, num_frames / libpds_blocksize(pd), (const float**) inputs, outputs);
}

// called from radium
static void RT_play_note(struct SoundPlugin *plugin, int block_delta_time, note_t note){

  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  //printf("RT_play_note. %f %d (%f)\n",note_num,(int)(volume*127),volume);
  libpds_noteon(pd, note.midi_channel, note.pitch, note.velocity*127);
  
  {
    t_atom v[5];
    
    SETFLOAT(v + 0, RT_add_note_id_pos(data, note.id));
    SETFLOAT(v + 1, note.pitch);
    SETFLOAT(v + 2, note.velocity);
    SETFLOAT(v + 3, note.pan);
    SETFLOAT(v + 4, block_delta_time);
    
    libpds_list(pd, "radium_receive_note_on", 5, v);
  }
}

static void RT_stop_note(struct SoundPlugin *plugin, int block_delta_time, note_t note){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  libpds_noteon(pd, note.midi_channel, note.pitch, 0);
  
  {
    t_atom v[3];
    
    SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
    SETFLOAT(v + 1, note.pitch);
    SETFLOAT(v + 2, block_delta_time);
    
    libpds_list(pd, "radium_receive_note_off", 3, v);
  }
}

// called from radium
static void RT_set_note_volume(struct SoundPlugin *plugin, int block_delta_time, note_t note){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  libpds_polyaftertouch(pd, note.midi_channel, note.pitch, note.velocity*127);

  {
    t_atom v[4];

    SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
    SETFLOAT(v + 1, note.pitch);
    SETFLOAT(v + 2, note.velocity);
    SETFLOAT(v + 3, block_delta_time);
    
    libpds_list(pd, "radium_receive_velocity", 4, v);
  }
}

// called from radium
static void RT_set_note_pitch(struct SoundPlugin *plugin, int block_delta_time, note_t note){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;

  {
    t_atom v[4]; 

    SETFLOAT(v + 0, RT_get_note_id_pos(data, note.id));
    SETFLOAT(v + 1, note.pitch);
    SETFLOAT(v + 2, note.pitch);
    SETFLOAT(v + 3, block_delta_time);
    
    libpds_list(pd, "radium_receive_pitch", 4, v);
  }
}

static void RT_send_raw_midi_message(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;

  if (MIDI_msg_byte1_remove_channel(msg)==0xb0)
    libpds_controlchange(pd, MIDI_msg_byte1_get_channel(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg));
}

// called from radium
void RT_PD_set_absolute_time(int64_t time){ 
  if(g_instances != NULL) {
    t_atom v[3];
    int sample_rate = MIXER_get_sample_rate();

    SETFLOAT(v + 0, int(time / sample_rate));
    SETFLOAT(v + 1, time % sample_rate);
    SETFLOAT(v + 2, sample_rate);

    Data *instance = g_instances;
    while(instance != NULL){
      libpds_list(instance->pd, "radium_time", 3, v);
      instance = instance->next;
    }
  } 
}

// called from radium
void RT_PD_set_realline(int64_t time, int64_t time_nextsubline, const Place *p){

  if(g_instances != NULL) {
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
    while(instance != NULL){
      libpds_list(instance->pd, "radium_visibleline", 8, v);
      instance = instance->next;
    }
  }
}

// called from radium
/*
void RT_PD_set_line(int64_t time, int64_t time_nextline, int line){

  if(g_instances != NULL) {
    t_atom v_line[6];
    int sample_rate = MIXER_get_sample_rate();

    const struct Blocks *block = RT_get_curr_seqblock()->block;

    int64_t duration = block->times[line+1].time - block->times[line].time;
      
    SETFLOAT(v_line + 0, int(time / sample_rate));
    SETFLOAT(v_line + 1, time % sample_rate);
    SETFLOAT(v_line + 2, sample_rate);
    SETFLOAT(v_line + 3, line);
    SETFLOAT(v_line + 4, int(duration/sample_rate));
    SETFLOAT(v_line + 5, duration % sample_rate);            

    Data *instance = g_instances;
    while(instance != NULL){
      libpds_list(instance->pd, "radium_line", 6, v_line);
      instance = instance->next;
    }
  }
}
*/

// called from radium
static void RT_set_effect_value(struct SoundPlugin *plugin, int block_delta_time, int effect_num, float value, enum ValueFormat value_format, FX_when when) {
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  Pd_Controller *controller = &data->controllers[effect_num];
  float real_value;

  if(value_format==EFFECT_FORMAT_SCALED && controller->type!=EFFECT_FORMAT_BOOL)
    real_value = scale(value, 0.0, 1.0, 
                       controller->min_value, controller->max_value);
  else
    real_value = value;

  safe_float_write(&controller->value, real_value);

  if(strcmp(controller->name, "")) {
    //printf("####################################################### Setting pd volume to %f / real_value: %f, for -%s-. Coming-from-pd: %d\n",value, real_value,name,controller->calling_from_pd);
    controller->calling_from_set_effect_value = true; {

      if(false==controller->calling_from_pd) {
        switch(when){
        case FX_start:
          libpds_bang(pd, controller->fx_when_start_name);
          break;
        case FX_middle:
          libpds_bang(pd, controller->fx_when_middle_name);
          break;
        case FX_end:
          libpds_bang(pd, controller->fx_when_end_name);
          break;
        case FX_single:
          libpds_bang(pd, controller->fx_when_single_name);
          break;
        default:
          RT_message("Unknown when value: %d",when);
        }

        libpds_float(pd, controller->name, real_value);
      }

    } controller->calling_from_set_effect_value = false;
  }
}

// called from radium
static float RT_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format) {
  Data *data = (Data*)plugin->data;
  float raw = data->controllers[effect_num].value;
  if(value_format==EFFECT_FORMAT_SCALED && data->controllers[effect_num].type!=EFFECT_FORMAT_BOOL)
    return scale(raw,
                 data->controllers[effect_num].min_value, data->controllers[effect_num].max_value,
                 0.0f, 1.0f);
  else
    return raw;
}

// called from radium
static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  Pd_Controller *controller = &data->controllers[effect_num];

  const char *name = controller->name;

  if(controller->type==EFFECT_FORMAT_FLOAT)
    snprintf(buffer,buffersize-1,"%s: %f",!strcmp(name,"")?"<not set>":name, safe_float_read(&controller->value));
  else
    snprintf(buffer,buffersize-1,"%s: %d",!strcmp(name,"")?"<not set>":name, (int)safe_float_read(&controller->value));
}

// called from radium
static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  Data *data = (Data*)plugin->data;
  //printf("####################################################### Showing Pd gui\n");
  PLAYER_lock();{
    libpds_show_gui(data->pd);
  }PLAYER_unlock();

  return true;
}

// called from radium
static void save_file(SoundPlugin *plugin) {
  Data *data=(Data*)plugin->data;
  libpds_request_savefile(data->pd, data->file);
}

// called from radium
static void hide_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  //printf("####################################################### Showing Pd gui\n");
  PLAYER_lock();{
    libpds_hide_gui(data->pd);
    save_file(plugin);
  }PLAYER_unlock();
}

// called from Pd
static void RT_pdfloathook(void *d, const char *sym, float val){
  Pd_Controller *controller = (Pd_Controller*)d;

  //printf("pdfloathook. calling_from_set_effect_value: %s\n",controller->calling_from_set_effect_value?"true":"false");

  if( ! controller->calling_from_set_effect_value) {

    float scaled_value = scale(val, controller->min_value, controller->max_value,
                               0.0f, 1.0f);
    scaled_value = R_BOUNDARIES(0.0f, scaled_value, 1.0f);
    
    controller->calling_from_pd = true; {
      RT_PLAYER_runner_lock();{
#if !defined(RELEASE)
        radium::ScopedBoolean scoped(g_calling_set_effect_value_from_pd);
#endif
        PLUGIN_set_effect_value(controller->plugin, -1, controller->num, scaled_value, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
      }RT_PLAYER_runner_unlock();
    } controller->calling_from_pd = false;
  }
}

// called from Pd
static void RT_bind_receiver(Pd_Controller *controller){
  char receive_symbol_name[PD_NAME_LENGTH+20];
  snprintf(receive_symbol_name, PD_NAME_LENGTH+19, "%s-receiver", controller->name);
  controller->pd_binding = libpds_bind(((Data*)controller->plugin->data)->pd, receive_symbol_name, controller);
}

// called from Pd
static void RT_add_controller(SoundPlugin *plugin, Data *data, const char *controller_name, int type, float min_value, float value, float max_value){
  Pd_Controller *controller;
  int controller_num;
  bool creating_new = true;

  for(controller_num=0;controller_num<NUM_PD_CONTROLLERS;controller_num++) {
    controller = &data->controllers[controller_num];

    if(controller->name[0]!=0 && !strcmp(controller->name, controller_name)) {
      creating_new = false;
      break;
    }

    if(controller->name[0] == 0 || !strcmp(controller->name, ""))
      break;
  }

  if(controller_num==NUM_PD_CONTROLLERS)
    return;

  if (fabs(min_value-max_value) < 0.0001f) {
    if(fabs(min_value) < 0.001f)
      max_value = 1.0f;
    else {
      min_value = value;
      max_value = value + 1.0f;
    }
  }

  controller->type = type;
  controller->min_value = min_value;
  controller->value = value;
  controller->max_value = max_value;  

  strncpy(controller->name, controller_name, PD_NAME_LENGTH-1);
  snprintf(controller->fx_when_start_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_start", controller_name);
  snprintf(controller->fx_when_middle_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_middle", controller_name);
  snprintf(controller->fx_when_end_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_end", controller_name);
  snprintf(controller->fx_when_single_name, PD_FX_WHEN_NAME_LENGTH-1, "%s-fx_single", controller_name);

  controller->has_gui = true;

  if (creating_new==true)
    RT_bind_receiver(controller);

  PDGUI_schedule_clearing(ATOMIC_GET(data->qtgui));
}


// Note that hooks are always called from the player thread.
// called from Pd
static void RT_pdmessagehook(void *d, const char *source, const char *controller_name, int argc, t_atom *argv){
  SoundPlugin *plugin = (SoundPlugin*)d;

  Data *data = (Data*)plugin->data;
  
  if( !strcmp(source, "libpd")) {
    //printf("controller_name: -%s-\n",controller_name);
    if(!strcmp(controller_name, "gui_is_visible"))
      PDGUI_is_visible(ATOMIC_GET(data->qtgui));
    else if(!strcmp(controller_name, "gui_is_hidden"))
      PDGUI_is_hidden(ATOMIC_GET(data->qtgui));
    return;
  }
}

// called from Pd
static bool is_bang(t_atom atom){
  return libpd_is_symbol(atom); // && atom.a_w.w_symbol==&s_bang;
}

// called from Pd
static void RT_pdlisthook(void *d, const char *recv, int argc, t_atom *argv) {
  SoundPlugin *plugin = (SoundPlugin*)d;
  Data *data = (Data*)plugin->data;
  int sample_rate = MIXER_get_sample_rate();
  struct Patch *patch = (struct Patch*)plugin->patch;
  
  //printf("argc: %d\n",argc);
  //printf("recv: %s\n",recv);

  if( !strcmp(recv, "radium_controller")) {
    if(argc==5 &&
       libpd_is_symbol(argv[0]) && strcmp("", libpd_get_symbol(argv[0])) &&
       libpd_is_float(argv[1]) &&
       libpd_is_float(argv[2]) &&
       libpd_is_float(argv[3]) &&
       libpd_is_float(argv[4]))
      {
        char  *name      = libpd_get_symbol(argv[0]);
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

  } else if( !strcmp(recv, "radium_send_note_on")) {
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

  } else if( !strcmp(recv, "radium_send_note_off")) {
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

  } else if( !strcmp(recv, "radium_send_velocity")) {
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

  } else if( !strcmp(recv, "radium_send_pitch")) {
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

  } else if( !strcmp(recv, "radium_send_blockreltempo")) {
    if(argc==1 &&
       libpd_is_float(argv[0]))
      {
        double tempo = libpd_get_float(argv[0]);

        if(tempo>100 || tempo <0.0001) {
          printf("Illegal tempo: %f. Must be between 0.0001 and 100.\n",tempo);
          tempo = R_BOUNDARIES(0.0001, tempo, 100);
        }
        
        {
          struct SeqBlock *curr_seqblock = RT_get_curr_seqblock();

          if (curr_seqblock != NULL) {
            struct Blocks *block = curr_seqblock->block;

            if (block!=NULL && !equal_doubles(tempo, ATOMIC_DOUBLE_GET(block->reltempo))){
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

// called from Pd
static void RT_noteonhook(void *d, int channel, int pitch, int velocity){
  SoundPlugin *plugin = (SoundPlugin*)d;
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
static void RT_polyaftertouchhook(void *d, int channel, int pitch, int velocity){
  SoundPlugin *plugin = (SoundPlugin*)d;
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
static void RT_controlchangehook(void *d, int channel, int cc, int value){
  SoundPlugin *plugin = (SoundPlugin*)d;
  struct Patch *patch = plugin->patch;
  
  //printf("Got MIDI control %x %x %x (%p)\n",channel,cc,value,d);
  
  if(patch==NULL)
    return;

  if (patch->patchdata==NULL) // Happens when loading song.
    return;

  bool is_player_or_runner_thread = THREADING_is_player_or_runner_thread();
  
  if (is_player_or_runner_thread)
    RT_PLAYER_runner_lock();
  else{
    R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());
  }

  if (RT_do_send_MIDI_to_receivers(plugin)){
    struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
    RT_PATCH_send_raw_midi_message_to_receivers(seqtrack, patch, MIDI_msg_pack3(0xb0 | channel, cc, value), -1);
  }
    
  if (is_player_or_runner_thread)
    RT_PLAYER_runner_unlock();
}

static QTemporaryFile *create_temp_pdfile(){
  QString destFileNameTemplate = QDir::tempPath()+QDir::separator()+"radium_XXXXXX.pd";
  return new QTemporaryFile(destFileNameTemplate);
}

static QTemporaryFile *get_pdfile_from_state(hash_t *state){
  QTemporaryFile *pdfile = create_temp_pdfile();
  pdfile->open();

  QTextStream out(pdfile);
  int num_lines = HASH_get_int(state, "num_lines");

  for(int i=0; i<num_lines; i++)
    out << STRING_get_qstring(HASH_get_string_at(state, "line", i)) + "\n";

  pdfile->close();

  return pdfile;
}

// http://www.java2s.com/Code/Cpp/Qt/Readtextfilelinebyline.htm
static void put_pdfile_into_state(const SoundPlugin *plugin, QFile *file, hash_t *state){
  Data *data=(Data*)plugin->data;
  void *request;
  PLAYER_lock();{
    request = libpds_request_savefile(data->pd, data->file);
  }PLAYER_unlock();

  libpds_wait_until_file_is_saved(data->pd, request, 2.0);

  file->open(QIODevice::ReadOnly | QIODevice::Text);

  QTextStream in(file);

  int i=0;
  QString line = in.readLine();
  while (!line.isNull()) {
    //printf("line: -%s-\n",line.toUtf8().constData());
    HASH_put_string_at(state, "line", i, STRING_create(line));
    i++;
    line = in.readLine();
  }

  HASH_put_int(state, "num_lines", i);

  file->close();
}

static QString get_search_path() {
  return STRING_get_qstring(OS_get_full_program_file_path("pd").id);
}

static Data *create_data(QTemporaryFile *pdfile, struct SoundPlugin *plugin, float sample_rate, int block_size){
  Data *data = (Data*)V_calloc(1,sizeof(Data));
  
  data->largest_used_ids_pos = -1;

  int i;
  for(i=0;i<NUM_PD_CONTROLLERS;i++) {
    data->controllers[i].display_name = NULL;
    data->controllers[i].plugin = plugin;
    data->controllers[i].num = i;
    data->controllers[i].max_value = 1.0f;
  }

  int blocksize;
  pd_t *pd;

  char puredatapath[1024];
  snprintf(puredatapath,1023,"%s/packages/libpd-master/pure-data",OS_get_program_path());
  pd = libpds_create(true, puredatapath);
  if(pd==NULL) {
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setText(QString(libpds_strerror()));
    msgBox->setStandardButtons(QMessageBox::Ok);
    safeExec(msgBox, false); // set program_state_is_valid to false. Not sure if it's safe to set it to true.
    V_free(data);
    return NULL;
  }

  data->pd = pd;

  QString search_path = get_search_path();
  libpds_add_to_search_path(pd, search_path.toUtf8().constData());

  libpds_set_floathook(pd, RT_pdfloathook);
  libpds_set_messagehook(pd, RT_pdmessagehook);
  libpds_set_listhook(pd, RT_pdlisthook);

  libpds_set_hook_data(pd, plugin);

  libpds_set_noteonhook(pd, RT_noteonhook);
  libpds_set_polyaftertouchhook(pd, RT_polyaftertouchhook);
  libpds_set_controlchangehook(pd, RT_controlchangehook);

  libpds_init_audio(pd, plugin->type->num_inputs, plugin->type->num_outputs, sample_rate);
    
  blocksize = libpds_blocksize(pd);

  if( (block_size % blocksize) != 0)
    GFX_Message(NULL, "PD's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", blocksize, block_size);

  // compute audio    [; pd dsp 1(
  libpds_start_message(pd, 1); // one entry in list
  libpds_add_float(pd, 1.0f);
  libpds_finish_message(pd, "pd", "dsp");

  plugin->data = data; // plugin->data is used before this function ends. (No, only data, seems like. We can send 'data' instead of 'plugin' to the hooks.) (well, PD_recreate_controllers_from_state uses plugin->data)

  libpds_bind(pd, "radium_controller", plugin);
  libpds_bind(pd, "radium_send_note_on", plugin);
  libpds_bind(pd, "radium_send_note_off", plugin);
  libpds_bind(pd, "radium_send_velocity", plugin);
  libpds_bind(pd, "radium_send_pitch", plugin);
  libpds_bind(pd, "radium_send_blockreltempo", plugin);
  libpds_bind(pd, "libpd", plugin);

  data->pdfile = pdfile;

  QFileInfo qfileinfo(pdfile->fileName());
  printf("name: %s, dir: %s\n",qfileinfo.fileName().toUtf8().constData(), qfileinfo.absolutePath().toUtf8().constData());

  data->file = libpds_openfile(pd, qfileinfo.fileName().toUtf8().constData(), qfileinfo.absolutePath().toUtf8().constData());
  
  R_ASSERT(data->file != NULL);
  if (data->file==NULL)
    return NULL;
  
  return data;
}

static QTemporaryFile *create_new_tempfile(QString *fileName){
  // create
  QFile source(*fileName);
  QTemporaryFile *pdfile = create_temp_pdfile();

  // open
  printf("open: %d\n",pdfile->open());
  source.open(QIODevice::ReadOnly);

  printf("filename: -%s-\n",pdfile->fileName().toUtf8().constData());

  // copy
  pdfile->write(source.readAll());

  // close
  pdfile->close();
  source.close();

  return pdfile;
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  QTemporaryFile *pdfile;

  if (state==NULL)
    pdfile = create_new_tempfile((QString *)plugin_type->data);
  else
    pdfile = get_pdfile_from_state(state);

  Data *data = create_data(pdfile, plugin, sample_rate, block_size);

  if(state!=NULL)
    PD_put_controllers_to_state(plugin, state);

  PLAYER_lock();{
    data->next = g_instances;    
    g_instances = data;
  }PLAYER_unlock();

  plugin->num_visible_outputs = 2;
  plugin->automatically_set_num_visible_outputs = true;
  
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);

  // remove element from g_instances
  {
    Data *prev = NULL;
    Data *instance = g_instances;
    while(instance != data){
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
  
  libpds_closefile(data->pd, data->file);  
  libpds_delete(data->pd);

  // Not necessary anymore since widgets are now deleted. (and it's not very healthy to call PDGUI_clear on a deleted widget)
  //PDGUI_clear(ATOMIC_GET(data->qtgui));

  delete data->pdfile;

  for(int i=0;i<NUM_PD_CONTROLLERS;i++){
    Pd_Controller *controller = &data->controllers[i];
    free((void*)controller->display_name);
  }
  
  V_free(data);
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  Pd_Controller *controller = &data->controllers[effect_num];

  return controller->type;
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  static char **notused_names = NULL;

  if(notused_names==NULL){
    notused_names = (char**)calloc(sizeof(char*), NUM_PD_CONTROLLERS);
    for(int i=0;i<NUM_PD_CONTROLLERS;i++)
      notused_names[i] = strdup(talloc_format(" %d",i));
  }

  Data *data = (Data*)plugin->data;
  Pd_Controller *controller = &data->controllers[effect_num];

  if (!strcmp(controller->name, ""))
    return notused_names[effect_num];
  else
    return controller->name;
}

void PD_set_qtgui(SoundPlugin *plugin, void *qtgui){
  Data *data = (Data*)plugin->data;
  ATOMIC_SET(data->qtgui, qtgui);
}

Pd_Controller *PD_get_controller(SoundPlugin *plugin, int n){
  Data *data = (Data*)plugin->data;
  return &data->controllers[n];
}

static bool controller_name_exists(const Data *data, const char *name){
  for(int i=0;i<NUM_PD_CONTROLLERS;i++)
    if(!strcmp(name, data->controllers[i].name))
      return true;

  return false;
}

const wchar_t *PD_set_controller_name(SoundPlugin *plugin, int n, const wchar_t *wname){
  Data *data = (Data*)plugin->data;
  Pd_Controller *controller = &data->controllers[n];
  const char *name = STRING_get_chars(wname);
  
  if(!strcmp(controller->name, name))
    return wname;

  if (controller_name_exists(data, name)){
    showAsyncMessage(talloc_format("A controller \"%S\" already exists", wname));
    return STRING_create(controller->name);
  }
  
  controller->display_name = wcsdup(wname);
  
  // Should check if it is different before binding. (but also if it is already binded)

  ADD_UNDO(PdControllers_CurrPos(const_cast<struct Patch*>(plugin->patch)));

  PLAYER_lock();{
    strncpy(controller->name, name, PD_NAME_LENGTH-1);

    if(controller->pd_binding != NULL)
      libpds_unbind(data->pd, controller->pd_binding);

    RT_bind_receiver(controller);

  }PLAYER_unlock();

  return wname;
}

void PD_recreate_controllers_from_state(SoundPlugin *plugin, const hash_t *state){
  Data *data=(Data*)plugin->data;

  PDGUI_clear(ATOMIC_GET(data->qtgui));

  int i;
  for(i=0;i<NUM_PD_CONTROLLERS;i++) {
    Pd_Controller *controller = &data->controllers[i];

    if(controller->pd_binding!=NULL) {
      libpds_unbind(data->pd, controller->pd_binding);
      controller->pd_binding = NULL;
    }

    controller->display_name = NULL;
    if (HASH_has_key_at(state, "name", i)){
      const wchar_t *name = HASH_get_string_at(state, "name", i);
      controller->display_name = wcsdup(name);
    }
    
    {
      const char *name = controller->display_name == NULL ? NULL : STRING_get_chars(controller->display_name);
      if(name==NULL || !strcmp(name,""))
        controller->name[0] = 0;
      else
        strncpy(controller->name, name, PD_NAME_LENGTH-1);
    }
    
    controller->type      = HASH_get_int_at(state, "type", i);
    controller->min_value = HASH_get_float_at(state, "min_value", i);
    controller->value = HASH_get_float_at(state, "value", i);
    controller->max_value = HASH_get_float_at(state, "max_value", i);
    controller->has_gui   = HASH_get_int_at(state, "has_gui", i)==1 ? true : false;
    controller->config_dialog_visible = HASH_get_int_at(state, "config_dialog_visible", i)==1 ? true : false;

    if(controller->name[0] != 0) {
      RT_bind_receiver(controller);
    }
  }

  volatile struct Patch *patch = plugin->patch;
  if(patch != NULL)
    GFX_update_instrument_widget((struct Patch*)patch);
}

void PD_put_controllers_to_state(const SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  int i;
  for(i=0;i<NUM_PD_CONTROLLERS;i++) {
    Pd_Controller *controller = &data->controllers[i];
    if (controller->display_name != NULL)
      HASH_put_string_at(state, "name", i, controller->display_name);
    else
      HASH_put_chars_at(state, "name", i, controller->name);
    HASH_put_int_at(state, "type", i, controller->type);
    HASH_put_float_at(state, "min_value", i, controller->min_value);
    HASH_put_float_at(state, "value", i, controller->value);
    HASH_put_float_at(state, "max_value", i, controller->max_value);
    HASH_put_int_at(state, "has_gui", i, controller->has_gui ? 1 : 0);
    HASH_put_int_at(state, "config_dialog_visible", i, controller->config_dialog_visible ? 1 : 0);
  }
}

static void create_state(const struct SoundPlugin *plugin, hash_t *state){
  printf("\n\n\n ********** CREATE_STATE ************* \n\n\n");
  Data *data = (Data*)plugin->data;

  PD_put_controllers_to_state(plugin, state);

  put_pdfile_into_state(plugin, data->pdfile, state);
}

// Warning! undo is created here (for simplicity). It's not common to call the undo creation function here, so beware of possible circular dependencies in the future.
void PD_delete_controller(SoundPlugin *plugin, int controller_num){
  Data *data=(Data*)plugin->data;

  R_ASSERT_RETURN_IF_FALSE(plugin->patch!=NULL);
  
  ADD_UNDO(PdControllers_CurrPos((struct Patch*)plugin->patch));

  int i;
  hash_t *state = HASH_create(NUM_PD_CONTROLLERS);

  for(i=0;i<NUM_PD_CONTROLLERS-1;i++) {
    int s = i>=controller_num ? i+1 : i;
    Pd_Controller *controller = &data->controllers[s];
    HASH_put_string_at(state, "name", i, controller->display_name == NULL ? L"" : controller->display_name);
    HASH_put_int_at(state, "type", i, controller->type);
    HASH_put_float_at(state, "min_value", i, controller->min_value);
    HASH_put_float_at(state, "value", i, controller->value);
    HASH_put_float_at(state, "max_value", i, controller->max_value);
    HASH_put_int_at(state, "has_gui", i, controller->has_gui);
    HASH_put_int_at(state, "config_dialog_visible", i, controller->config_dialog_visible);
  }

  HASH_put_chars_at(state, "name", NUM_PD_CONTROLLERS-1, "");
  HASH_put_int_at(state, "type", NUM_PD_CONTROLLERS-1, EFFECT_FORMAT_FLOAT);
  HASH_put_float_at(state, "min_value", NUM_PD_CONTROLLERS-1, 0.0);
  HASH_put_float_at(state, "value", NUM_PD_CONTROLLERS-1, 0.0);
  HASH_put_float_at(state, "max_value", NUM_PD_CONTROLLERS-1, 1.0);
  HASH_put_int_at(state, "has_gui", NUM_PD_CONTROLLERS-1, 0);
  HASH_put_int_at(state, "config_dialog_visible", i, 0);

  PD_recreate_controllers_from_state(plugin, state);
}


static void add_plugin(const wchar_t *name, QString filename) {
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "Pd";
  plugin_type->name                     = V_strdup(STRING_get_chars(name));
  plugin_type->info                     = "Pd is made by Miller Puckette. Radium uses a modified version of libpd to access it.\nlibpd is made by Peter Brinkmann.";
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

static void build_plugins(QDir dir){
  printf(">> dir: -%s-\n",dir.absolutePath().toUtf8().constData());
  PR_add_menu_entry(PluginMenuEntry::level_up(dir.dirName().toUtf8().constData()));

  dir.setSorting(QDir::Name);

  // browse dirs first.
  dir.setFilter(QDir::Dirs |  QDir::NoDotAndDotDot);

  {
    QFileInfoList list = dir.entryInfoList();
    
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo fileInfo = list.at(i);
      //if(fileInfo.absoluteFilePath() != dir.absoluteFilePath())
        build_plugins(QDir(fileInfo.absoluteFilePath()));
    }
  }

    // Then the files
  dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
  {
    QFileInfoList list = dir.entryInfoList();
    
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo fileInfo = list.at(i);
      printf("   file: -%s-\n",fileInfo.absoluteFilePath().toUtf8().constData());
      if(fileInfo.suffix()=="pd") {
        if(fileInfo.baseName()==QString("New_Audio_Effect"))
          add_plugin(STRING_create(""), fileInfo.absoluteFilePath());
        add_plugin(STRING_create(fileInfo.baseName().replace("_"," ")), fileInfo.absoluteFilePath());
      }
    }
  }

  printf("<< dir: -%s-\n",dir.absolutePath().toUtf8().constData());
  PR_add_menu_entry(PluginMenuEntry::level_down());
}

void create_pd_plugin(void){
  build_plugins(QDir(get_search_path()+OS_get_directory_separator()+"Pd"));
}


#else

// !WITH_PD

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPluginRegistry_proc.h"
#include "Pd_plugin.h"
#include "Pd_plugin_proc.h"

void create_pd_plugin(void){
}

const wchar_t *PD_set_controller_name(SoundPlugin *plugin, int n, const wchar_t *name) {return name;}
Pd_Controller *PD_get_controller(SoundPlugin *plugin, int n) {return NULL;}
void PD_set_qtgui(SoundPlugin *plugin, void *qtgui) {}
void PD_delete_controller(SoundPlugin *plugin, int controller_num) {}

void PD_recreate_controllers_from_state(SoundPlugin *plugin, const hash_t *state) {}
void PD_create_controllers_from_state(SoundPlugin *plugin, hash_t *state) {}

#endif // WITH_PD
