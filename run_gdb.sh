#!/usr/bin/env bash

set -ueE
#set -x

#if [ -v QT_QPA_PLATFORM_PLUGIN_PATH ] ; then
#source ~/.bashrc
#fi

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR

source configuration.sh #>/dev/null

if [ -v RADIUM_NSM_EXECUTABLE_NAME ] ; then
        export RADIUM_NSM_EXECUTABLE_NAME=$(basename -- "$0")
fi

PWD=`pwd`
export LSAN_OPTIONS=suppressions=$PWD/SanitizerSuppr.txt
export ASAN_OPTIONS="detect_leaks=0,abort_on_error=1,max_malloc_fill_size=1048576,detect_odr_violation=2,detect_container_overflow=0,suppressions=$PWD/SanitizerSupprAddr.txt"

#
# earlier we also had these ASAN_OPTIONS:
#
# allocator_may_return_null=1,  # Don't know why it was added...
# new_delete_type_mismatch=0,   # Because of Qt, but seems to have been fixed now.
# alloc_dealloc_mismatch=0,     # Because of various Vst plugins.

export UBSAN_OPTIONS="print_stacktrace=1:abort_on_error=1"
#suppressions=`pwd`/SanitizerSuppr.txt:
export TSAN_OPTIONS="history_size=7,second_deadlock_stack=1,print_stacktrace=1:abort_on_error=1,suppressions=$PWD/SanitizerSuppr.txt"


THIS_DIR=$(dirname $(readlink -f $0))

if uname -s |grep Linux > /dev/null ; then
    XCB_LIB_DIR=$THIS_DIR/bin/packages/libxcb-1.13/src/.libs
    
    if ! file $XCB_LIB_DIR >/dev/null ; then
	echo "Unable to find directory $XCB_LIB_DIR"
	exit -1
    fi

    export LD_LIBRARY_PATH=$XCB_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
fi

export LD_LIBRARY_PATH="$PWD/bin/packages/python27_install/lib:$PWD/bin/packages/faust/build/lib"${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

# To avoid buggy qt plugins from crashing radium (very common).
unset QT_QPA_PLATFORMTHEME
unset QT_QPA_PLATFORM
unset QT_PLUGIN_PATH

# Need this one though
export QT_QPA_PLATFORM_PLUGIN_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`

# To avoid freezing X
export USE_SAFE_POPUP="1"

export G_DEBUG="fatal-warnings,gc-friendly"

#ulimit -s 655360

#debugger="gdb"
debugger="lldb"
#debugger="nnd"
#debugger="lldb -O 'env $FAUST_LD_LIB_PATH'" # LLDB + FAUST/LLVM

dostartnow=false
running_in_emacs=false

while [[ $# -gt 0 ]]; do
  case $1 in
      -s|--start)
		  dostartnow=true
		  shift
		  ;;
      -i|--in-emacs)
		  running_in_emacs=true;
		  shift
		  ;;
      *)
		  break;
		  ;;
  esac
done

debugger_argline=""

if [ "$debugger" = "gdb" ] ; then
	if $dostartnow ; then
		debugger_argline="-ex run --args"
	fi
	if $running_in_emacs ; then
		debugger_argline="$debugger_argline -i=im"
	fi
	debugger_argline="$debugger_argline --args"
elif [[ "$debugger" = "lldb"* ]] ; then
	if $dostartnow ; then
		debugger_argline="-o run"
	fi
	debugger_argline="$debugger_argline --"
fi



unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     EXECUTABLE="/tmp/radium_bin/radium_linux.bin";;
    Darwin*)    EXECUTABLE="/tmp/radium_bin/radium_macos.bin";;
    *)          EXECUTABLE="where_is_radium_for_\"${unameOut}\"?_(change_these_lines_in_run_gdb.sh_to_fix_this)";;
esac

#ldd -r $EXECUTABLE

rm -f /tmp/runradiumgdb*.sh

#exec gdb -i=mi $EXECUTABLE

exename=/tmp/runradiumgdb$$.sh

echo "G_DEBUG="fatal-warnings,gc-friendly" USE=libedit/readline exec $debugger $debugger_argline $@ $EXECUTABLE ; killall -9 radium_progress_window ; killall -9 radium_crashreporter"  > $exename

chmod a+rx $exename

exec $exename
#bash $exename

# without gdb:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter


# to test crashreporter, comment out the above line, and uncomment the next line:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@
