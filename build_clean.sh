#!/usr/bin/env bash

set -eEu

#set -x

source $(dirname "${0}")/bash_setup.sh

branch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')
T=/tmp/radium_objects_$branch/

API=api

rm -f $T*.o *.o */*.o featurelist *~ */*~ */*~ */*/*~ core bin/core */*.pyc */*/*.pyc $API/radium_wrap.c $API/wrapfunclist.c $API/radium.i $API/radium_proc.h api/s7_types_code.c api/s7_types_code_init.c api/s7_types.h bin/protos.conf bin/radium bin/radium_linux.bin python/core bin/X11_XSendEvent bin/X11_Qtstuff.py makescript.sh Qt/mQt_instruments_widget_callbacks.h Qt/Qt_midi_instrument_widget.h Qt/Qt_audio_instrument_widget.h Qt/mQt_sample_requester_widget_callbacks.h Qt/mQt_control_change_widget_callbacks.h Qt/mQt* Qt/Qt_preferences.h Qt/Qt_vst_paths_widget.h mixergui/mQM* mixergui/images.cpp a.out common/keyboard_sub_ids.h common/visual_op_queue_proc.h w dependencies_ok buildtype.opt flagopts.opt bin/radium_crashreporter bin/radium_crashreporter.exe bin/radium_error_message bin/radium_progress_window bin/radium_progress_window.exe bin/radium_error_message.exe bin/radium.bin.exe bin/radium_check_jack_status bin/radium_check_jack_status.exe bin/radium_check_opengl bin/radium_check_opengl.exe bin/radium_plugin_scanner bin/radium_plugin_scanner.exe bin/radium_addr2line bin/radium_addr2line.exe bin/radium_pcinfo bin/radium_pcinfo.exe linux_objs/* darwinx_objs/* mingw_objs/* */*/*/Juce_plugins.o bin/s7webserver/moc_s7webserver.cpp audio/mfaustqt2.cpp bin/styles/* api/mapi_gui.cpp sndlib_built sndlib_objectfiles/* bin/packages/sndlib/*.o

rm -fr /tmp/radium_bin /tmp/radium_objects

rm -f audio/zita_rev.cpp audio/stk_flute.cpp audio/stk_bowed.cpp audio/stk_blow_bottle.cpp audio/stk_bass.cpp audio/stk_blow_hole.cpp audio/stk_brass.cpp audio/stk_clarinet.cpp audio/stk_flute_stk.cpp audio/stk_glass_harmonica.cpp audio/stk_harpsi.cpp audio/stk_modal_bar.cpp audio/stk_NLF_eks.cpp audio/stk_NLF_fm.cpp audio/stk_piano.cpp audio/stk_saxophony.cpp audio/stk_sitar.cpp audio/stk_tibetan_bowl.cpp audio/stk_tuned_bar.cpp audio/stk_uni_bar.cpp audio/stk_voice_form.cpp audio/faust_tapiir.cpp audio/faust_multibandcomp.cpp audio/faust_system_eq.cpp audio/faust_system_tremolo.cpp audio/faust_system_lowpass.cpp audio/faust_system_highpass.cpp audio/faust_system_lowshelf.cpp audio/faust_system_highshelf.cpp audio/system_compressor.cpp 

export GCC=gcc

cd pluginhost/Builds/Linux && make clean && rm -fr build


#	cd $T && rm -f *.o
#	cd bin/packages/sndlib && $(MAKE) clean && rm -f sndlib.a sndlib.so
