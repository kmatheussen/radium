
########################################################
#
#
#  Configuration. You might want to edit this file
#  if building Radium.
#
#
########################################################




########################################################
# If enabled, use clang/clang++ instead of gcc/g++
#
export RADIUM_USE_CLANG=1




########################################################
# If enabled, include the Pd instrument
# Only Linux. Other platforms ignore this variable.
#
export INCLUDE_PDDEV="jadda"




########################################################
# If enabled, include the FaustDev instrument.
#
export INCLUDE_FAUSTDEV="jadda"




########################################################
# If you want to include the FaustDev instrument,
# but don't want to support the llvm backend for it,
# then uncomment this line. Without LLVM, faust
# can only use the slower interpreter backend.
#
# export INCLUDE_FAUSTDEV_BUT_NOT_LLVM="jadda"




########################################################
# If enabled, use QWebEngine instead of QtWebKit
#
# QWebEngine doesn't work as well as QtWebKit in Radium,
# but it can be enabled if you don't bother installing
# qt-webkit. (Installing QtWebKit can be quite a hassle
# sometimes!)
#
# export USE_QWEBENGINE=1





########################################################
#
# Values below here should often/usually work without modification.
#
########################################################

if env |grep INCLUDE_FAUSTDEV ; then
    if ! env |grep INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
	export FAUST_USES_LLVM="jadda"
    fi
fi


export PYTHONEXE=`./find_python_path.sh`

export PKG=`which pkg-config`

if env |grep FAUST_USES_LLVM ; then
    
    if uname -s |grep Linux ; then
	
	export LLVM_PATH=`dirname($dirname($which llvm-config))`
	export FAUST_LD_LIB_PATH="LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LLVM_PATH}/lib"
	
    elif uname -s |grep Darwin ; then
	
	export LLVM_PATH=/opt/local/libexec/llvm-18
	export FAUST_LD_LIB_PATH="DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${LLVM_PATH}/lib"

    else
	echo "unknown architecture"
	exit -1
    fi
    
fi

print_error_and_exit()
{
    RED='\033[1;31m'
    LIGHT_CYAN='\033[0;36m'
    NC='\033[0m'
    printf "${RED}Error: ${LIGHT_CYAN}${1}${NC} (in configuration.sh)\n"
    exit -1
}

check_if_exists()
{
    if [ ! -f $1 ] ; then
	print_error_and_exit "\"${1}\" doesn't seem to exist..."
    fi
}

check_if_exists $PYTHONEXE
check_if_exists $PKG

if env |grep FAUST_USES_LLVM ; then
    check_if_exists $LLVM_PATH/bin/llvm-config
fi

if uname -s |grep Darwin ; then
    
    export MACOSX_DEPLOYMENT_TARGET=12.0

    if env |grep FAUST_USES_LLVM ; then
	export MACOS_LLVM_TARGET="$($LLVM_PATH/bin/llvm-config --host-target)"
    else
	export MACOS_LLVM_TARGET="Thiscodeisnotsupposedtobecompiled"
    fi
    
fi



########################################################
# Values below here should normally not be edited.
#
########################################################

export RADIUM_QT_VERSION=5


if env | grep RADIUM_USE_CLANG ; then
    export FULL_CCC_PATH=`which clang++`
else
    export FULL_CCC_PATH=`which g++`
fi

if ! uname -s |grep Linux ; then
    unset INCLUDE_PDDEV
fi

export ALL_SHELL_SCRIPTS=$(ls -1 *.sh | tr '\n' ' ')
