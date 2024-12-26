set -eEu


source bash_setup.sh



########################################################
#
#
#  Configuration. (default values)
#
#  You might want to edit this file if building Radium.
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
# qt5
#
set_var PKGqt 0 # Can be set to another pkg-config than is not first in PATH.
set_var QT_PKG_CONFIGURATION_PATH 0 # PKG_CONFIG_PATH will be set to this value.
set_var QMAKE 0
set_var UIC 0
set_var MOC 0




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

if [ -v RADIUM_CONFIGURATION_HAS_BEEN_SETUP ] ; then
    echo "Something is wrong. configuration.sh should not include itself."
    exit -1
fi

RADIUM_CONFIGURATION_HAS_BEEN_SETUP=1


# Note: Run-scripts have not been updated to use
# other pythons than the one included with Radium,
# so setting this value to something else might
# lead to unexpected behaviors.
#
set_var PYTHONEXE `./find_python_path.sh`
if ! env |grep PYTHONEXE_NOT_AVAILABLE_YET ; then
    assert_exe_exists $PYTHONEXE
fi
   
set_var PKG `which pkg-config`
assert_exe_exists $PKG

set_var RADIUM_RELEASE_CFLAGS ""

if is_0 $PKGqt ; then
    export PKGqt=$PKG
fi

assert_exe_exists $PKGqt

if ! is_0 $QT_PKG_CONFIGURATION_PATH ; then
    assert_exe_exists $QT_PKG_CONFIGURATION_PATH
    export PKGqt="PKG_CONFIG_PATH=$QT_PKG_CONFIGURATION_PATH $PKGqt"
fi

if is_0 $QMAKE ; then
    if which qmake-qt5 ; then
        export QMAKE=$(which qmake-qt5)
    else
        export QMAKE=$(which qmake)
    fi
fi

if is_0 $UIC ; then
    if which uic-qt5 ; then
        export UIC=$(which uic-qt5)
    else
        export UIC=$(which uic)
    fi
fi

if is_0 $MOC ; then
    if which moc-qt5 ; then
        export MOC=$(which moc-qt5)
    else
        export MOC=$(which moc)
    fi
fi


assert_exe_exists $QMAKE
assert_exe_exists $UIC
assert_exe_exists $MOC

if ${QMAKE} -query QT_VERSION | grep -v '^5.1' ; then
    handle_failure "Seems like qmake has the wrong version. We need Qt newer than 5.10, but not Qt6. Set QMAKE to correct path to fix".
fi

if ${UIC} --version | grep -v '^uic 5.1' ; then
    handle_failure "Seems like uic has the wrong version. We need Qt newer than 5.10, but not Qt6. Set UIC to correct path to fix".
fi

if ${MOC} --version | grep -v '^moc 5.1' ; then
    handle_failure "Seems like moc has the wrong version. We need Qt newer than 5.10, but not Qt6. Set MOC to correct path to fix".
fi


if [ "$($PKGqt --libs-only-L Qt5Core)" != "" ] ; then
    if env |grep QMAKE_LIBS_ONLY_L_SHOULD_BE_EMPTY ; then
        handle_failure "Not empty"
    fi
        
    A=$($PKGqt --libs-only-L Qt5Core | xargs)
    B="-L$($QMAKE -query QT_INSTALL_PREFIX)/lib"
    if [ "$A" != "$B" ] ; then
	handle_failure "$PKGqt and $QMAKE doesn't seem to point to the same Qt:\n" \
		       "PKG: \"$A\".\n" \
		       "${QMAKE}: \"$B\""
    fi
else
    if ! is_set QMAKE_LIBS_ONLY_L_SHOULD_BE_EMPTY ; then
        if [ "$($QMAKE -query QT_INSTALL_PREFIX)" != "/usr" ] ; then
	    handle_failure "\"$PKGqt --libs-only-L Qt5Core\" gave no output. It's assumed that qt is installed in /usr, but it's not. it's installed in \"$($QMAKE -query QT_INSTALL_PREFIX)\". If this is correct, then simply set QMAKE_LIBS_ONLY_L_SHOULD_BE_EMPTY=1 in your build script."
        fi
    fi
fi

if ! is_0 $INCLUDE_FAUSTDEV ; then
    if is_0 $INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
	export FAUST_USES_LLVM=1
    fi
fi


set_var FAUST_USES_LLVM 0

if ! is_0 $FAUST_USES_LLVM ; then
    
    set_var LLVM_CONFIG_BIN `which llvm-config`
    assert_exe_exists $LLVM_CONFIG_BIN
    
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
    
    if ! is_0 $FAUST_USES_LLVM ; then
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
