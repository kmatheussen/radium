set -eEu

source bash_setup.sh



########################################################
#
#
#  Configuration. (default values)
#
#  You might want to edit this file if building Radium.
#
#  If developing Radium, it's probably better to create
#  a script that sets your values and let that script
#  start "./build_linux.sh".
#
#  You can also compile Radium with custom values
#  like this:
#
#  RADIUM_USE_CLANG=1 ./build_linux.sh
#
#  Variables are not overwritten in this file, they
#  only provide the default values. This can be
#  convenient e.g if you want to create different
#  scripts for building different versions of Radium,
#  or you don't want to risk accidentally pusing changes
#  in this file to the repository.
#
########################################################


# (do some stuff first)
ORG_PWD=`pwd`
cd $(dirname ${BASH_SOURCE[0]})

source helpers.sh




########################################################
# If set to 1, use clang/clang++ instead of gcc/g++
#
set_var RADIUM_USE_CLANG 0



########################################################
# If enabled, include the Pd instrument
# Only Linux. Other platforms ignore this variable.
#
set_var INCLUDE_PDDEV 1




########################################################
# If enabled, include the FaustDev instrument.
#
set_var INCLUDE_FAUSTDEV 1




########################################################
# If you want to include the FaustDev instrument,
# but don't want to support the llvm backend for it,
# then set the variable to 1. Without LLVM, faust
# can only use the slower interpreter backend.
#
set_var INCLUDE_FAUSTDEV_BUT_NOT_LLVM 0




########################################################
# If enabled, use QWebEngine instead of QtWebKit
#
# QWebEngine doesn't work as well as QtWebKit in Radium,
# but it can be enabled if you don't bother installing
# qt-webkit. (Installing QtWebKit can be quite a hassle
# sometimes!)
#
set_var USE_QWEBENGINE 0



########################################################
# Set to 0 to make the demo version of the program.
#
# The demo version has the following three restrictions:
#
# 1. "Export soundfile(s)" in the file menu is disabled.
# 2. Only two VST/VST3/AU/LV2 plugins can run at
#    the same time in a song. (Note: There are no
#    restrictions on the number of Ladspa plugins or
#    built-in plugins.)
# 3. A "nag" window requesting you to buy the program
#    shows up at program startup and after each time
#    you save a song.
#
# If you want to make a package for a large Linux
# distribution such as Ubuntu; Or, if you want to
# include Radium in Homebrew or Macports on macOS:
# I would kindly request that you set this value to 0.
#
set_var FULL_VERSION 1




########################################################
# RADIUM_QTDIR is the directory where qt5 is installed.
# If set to 0, we will try to find it automatically.
# Note that if settings RADIUM_QTDIR you might also have
# to set either the PKGqt variable or the
# QT_PKG_CONFIGURATION_PATH as well, so that we don't
# mix Qt version. (The build system checks this for
# you, and will tell you if you need to set either
# of these variables.)
#
set_var RADIUM_QTDIR 0 # This is the directory where we find bin/qmake, bin/uic, and bin/moc.
set_var PKGqt 0 # Can be set to another pkg-config than is not first in PATH.
set_var QT_PKG_CONFIGURATION_PATH 0 # PKG_CONFIG_PATH will be set to this value.




########################################################
# When developing Radium, this value should proabably
# be set to 1. If you just want to compile the program,
# set it to 0.
#
set_var WARNINGS_AS_ERRORS 1




########################################################
# Set to the minimum Macos version you want the program
# to run on. (Obviously ignored on the other platforms)
#
set_var MACOSX_DEPLOYMENT_TARGET 12.0




########################################################
#
# Values below here should often/usually work without modification.
#
########################################################

# Note: Run-scripts have not been updated to use
# other pythons than the one included with Radium,
# so setting this value to something else might
# lead to unexpected behaviors.
#
set_var PYTHONEXE `./find_python_path.sh`
if ! env |grep PYTHONEXE_NOT_AVAILABLE_YET ; then
    assert_env_path_exists $PYTHONEXE
fi
   
set_var PKG `which pkg-config`
assert_env_path_exists $PKG

set_var RADIUM_RELEASE_CFLAGS ""

if ! is_0 $PKGqt ; then
    assert_env_path_exists $PKGqt
else
    export PKGqt=$PKG
fi

if ! is_0 $QT_PKG_CONFIGURATION_PATH ; then
    assert_env_path_exists $QT_PKG_CONFIGURATION_PATH
    export PKGqt="PKG_CONFIG_PATH=$QT_PKG_CONFIGURATION_PATH $PKGqt"
fi


QMAKE=$(./find_moc_and_uic_paths.sh qmake)

assert_env_path_exists $QMAKE

assert_env_path_exists $(./find_moc_and_uic_paths.sh uic)

assert_env_path_exists $(./find_moc_and_uic_paths.sh moc)

if ${QMAKE} -query QT_VERSION | grep -v '^5.1' ; then
    handle_failure "Seems like it's the wrong qt version. We need Qt newer than 5.10. Set RADIUM_QTDIR to correct directory to fix".
fi


if [ "$($PKGqt --libs-only-L Qt5Core)" != "" ] ; then
    A=$($PKGqt --libs-only-L Qt5Core)
    B="-L$($QMAKE -query QT_INSTALL_PREFIX)/lib"
    if [ "$A" != "$B" ] ; then
	handle_failure "$PKGqt and $QMAKE doesn't seem to point to the same Qt:\n" \
		       "PKG: \"$A\".\n" \
		       "${QMAKE}: \"$B\""
    fi
else
    if [ "$($QMAKE -query QT_INSTALL_PREFIX)" != "/usr" ] ; then
	handle_failure "\"$PKGqt --libs-only-L Qt5Core\" gave no output. It's assumed that qt is installed in /usr, but it's not. it's installed in \"$(QMAKE -query QT_INSTALL_PREFIX)\". This is only an indication that something is wrong. If you think it is, just comment out this line and try again. (please also provide patch)"
    fi
fi

if is_set INCLUDE_FAUSTDEV ; then
    if ! is_set INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
	export FAUST_USES_LLVM="jadda"
    fi
fi

if is_set FAUST_USES_LLVM ; then
    
    set_var LLVM_CONFIG_BIN `which llvm-config`
    assert_env_path_exists LLVM_CONFIG_BIN
    
    old_path=""
    
    if is_set LD_LIBRARY_PATH ; then
	old_path=":$LD_LIBRARY"
    fi
    
    if uname -s |grep Linux > /dev/null ; then
	
	set_var FAUST_LD_LIB_PATH "LD_LIBRARY_PATH=`${LLVM_CONFIG_BIN} --libdir`$old_path"
	
    elif uname -s |grep Darwin > /dev/null ; then

	set_var FAUST_LD_LIB_PATH "DYLD_LIBRARY_PATH=`${LLVM_CONFIG_BIN} --libdir`$old_path"

    else
	handle_failure "unknown architecture"
    fi
    
fi


if uname -s |grep Darwin ; then
    
    if is_set FAUST_USES_LLVM ; then
	export MACOS_LLVM_TARGET=`${LLVM_CONFIG_BIN} --host-target`
    else
	export MACOS_LLVM_TARGET="Thiscodeisnotsupposedtobecompiled"
    fi
    
fi




########################################################
# Values below here should normally not be edited.
#
########################################################

if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export FULL_CCC_PATH=`which clang++`
else
    export FULL_CCC_PATH=`which g++`
fi

if ! uname -s |grep Linux > /dev/null ; then
    unset INCLUDE_PDDEV
fi

if [ ${FULL_VERSION} -eq 0 ] ; then
    true
elif [ ${FULL_VERSION} -eq 1 ] ; then
    true
else
    handle_failure "FULL_VERSION must be 0 or 1: ${FULL_VERSION}"
fi

# Used by the makefile
export ALL_SHELL_SCRIPTS=$(ls -1 *.sh | tr '\n' ' ')

#echo $ALL_SHELL_SCRIPTS

cd $ORG_PWD
unset ORG_PWD
