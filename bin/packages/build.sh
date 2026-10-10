#!/usr/bin/env bash

set -eEu
#set -x

export PYTHONEXE_NOT_AVAILABLE_YET=1

export INCLUDE_FAUSTDEV_BUT_NOT_LLVM=1

# Might want to uncomment the line below to make faust build with support for llvm.
#export RADIUM_USE_CLANG=1

pushd ../../
source configuration.sh
popd

unset CFLAGS
unset CFLAGS
unset CPPFLAGS
unset LDFLAGS
unset CXXFLAGS

RELEASE_BUILD=true
#RELEASE_BUILD=false

if $RELEASE_BUILD ; then
	export COMMON_CFLAGS="-mtune=generic -fPIC -fno-strict-aliasing -Wno-misleading-indentation "
else
	export COMMON_CFLAGS="-ggdb3 -fno-omit-frame-pointer -fPIC -fno-strict-aliasing -Wno-misleading-indentation "
fi

if uname -s |grep Darwin ; then
    if [[ -z "${MACOSX_DEPLOYMENT_TARGET}" ]]; then
	echo "MACOSX_DEPLOYMENT_TARGET not set"
	exit -1
    fi
    export COMMON_CFLAGS="$COMMON_CFLAGS -mmacosx-version-min=${MACOSX_DEPLOYMENT_TARGET}"
    # Universal binaries. (Used for faust, fluidsynth, qscintilla, etc.)
    export DARWIN_ARCH_FLAGS="-arch arm64 -arch x86_64 "
    export DARWIN_MIN_FLAGS="-mmacosx-version-min=${MACOSX_DEPLOYMENT_TARGET}"
    export DARWIN_CMAKEOPT="-DCMAKE_OSX_ARCHITECTURES=\"arm64;x86_64\" -DCMAKE_OSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET}"
else
    export DARWIN_ARCH_FLAGS=""
    export DARWIN_MIN_FLAGS=""
    export DARWIN_CMAKEOPT=""
fi
   
if ! arch |grep -e arm -e aarch64 ; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -msse2 -mfpmath=sse "
fi

if [[ $RADIUM_USE_CLANG == 0 ]] ; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -fmax-errors=5 "
fi

set_var QT_CPPFLAGS ""
set_var QT_LDFLAGS ""

export CFLAGS="$COMMON_CFLAGS "
export CPPFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS "
export CXXFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS "
export LDFLAGS="$QT_LDFLAGS"

DASCC=gcc
DASCXX=g++


if ! is_0 $RADIUM_USE_CLANG ; then
    DASCC=clang
    DASCXX=clang++
fi




#rm -f /tmp/radium
#ln -s `pwd` /tmp/radium
PWD=`pwd`
PREFIX=`dirname $PWD/$0`
#echo $PREFIX
#exit

#http://www.python.org
#tar xvzf Python-2.2.2.tgz
#cd Python-2.2.2
#./configure --prefix=/tmp/radium --with-threads
#make
#make install
#cd ..



#tar xvf setxkbmap_56346c72127303a445a273217f7633c2afb29cfc.tar
#cd setxkbmap
#make clean
#./configure --prefix=/usr
#make
#cd ..


build_faust() {

	rm -fr faust
	tar xvzf faust-2.88.0.tar.gz
	mv faust-2.88.0 faust
	cd faust
	
	#patch -p0 < ../faust_polydsp_fadeout.patch
	#patch -p0 < ../faust_soundfiles_clickfix.patch
	patch -p0 < ../faust_soundfile_padding.patch
	patch -p0 < ../faust_fopenat_nochdir.patch
	patch -p0 < ../faust_fix_hang.patch
	patch -p0 < ../faust_fix_eval_recursion.patch
	patch -p0 < ../faust_fix_apply_memo.patch
	patch -p0 < ../faust_fix_svg_folding.patch
	patch -p0 < ../faust_fix_fft_route.patch
	
	### this line is needed to build on artix
	#export LIBNCURSES_PATH=$(shell find /usr -name libncursesw_g.a)
    
	cp ../faust_targets.cmake build/targets/most.cmake
    
	if is_0 $FAUST_USES_LLVM ; then
		cp ../faust_radium_nonllvm.cmake build/backends/most.cmake
	else
		cp ../faust_radium_llvm.cmake build/backends/most.cmake
		export ORGTEMPPATH=$PATH
		export PATH=$($LLVM_CONFIG_BIN --bindir):$PATH
		#echo "PATH: $PATH"
	fi
	
	# Use all CPUs when building faust.
	JOBS=$(nproc)
	#sed -i.backup "s/(BUILDLOCATION)$/(BUILDLOCATION) -j${JOBS}/" Makefile
	#exit -1

	echo "\n\nNote: Faust might fail if built with gcc. To work around that, simply build faust with clang instead, temporarily setting RADIUM_USE_CLANG=1 only when building faust.\n\n"
	
	# release build
	BUILDOPT="--config Release -j${JOBS}" VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` $DARWIN_CMAKEOPT" make most
	
	if ! is_0 $FAUST_USES_LLVM ; then
		export PATH=$ORGTEMPPATH
		unset ORGTEMPPATH
	fi
    
    
	# debug build
	#VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Debug -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` " make most
    
	cd ..
}

build_libpds() {

    NOWARNS="-Wno-return-mismatch -Wno-implicit-function-declaration -Wno-int-conversion -Wno-implicit-int -Wno-strict-prototypes -Wno-incompatible-pointer-types -std=gnu17"
    
    rm -fr libpd-master
    tar xvzf libpd-master.tar.gz
    cd libpd-master/
    sed -i.backup "/define CFLAGS/ s|\")| -I/usr/include/tirpc $NOWARNS\")|" make.scm
    sed -i.backup 's/k_cext$//' make.scm
    sed -i.backup 's/oscx //' make.scm
    sed -i.backup "s/gcc -O3/gcc -fcommon -O3 $NOWARNS/" make.scm

    GLIBCVERSION=`ldd --version | head -n 1 | awk '{print $NF}'`
    if [[ $GLIBCVERSION > 2.34 ]] ; then
	sed -i.backup 's/#define fsqrt sqrt/#include <math.h>\nstatic float fsqrt(float val){return sqrt(val);}/g' pure-data/extra/fiddle~/fiddle~.c
	sed -i.backup 's/fsqrt/myfsqrt/g' pure-data/extra/fiddle~/fiddle~.c
    fi
    
    make clean
    CPPFLAGS="$CFLAGS $NOWARNS" make -j`nproc`
    cd ..
}


# Official libpd (https://github.com/libpd/libpd), tracking current Pd Vanilla.
# Built with multiple instance support (PDINSTANCE/PDTHREADS).
build_libpd() {

    rm -fr libpd
    tar xvzf libpd-0.16.1.tar.gz
    cd libpd/

    # Apply Radium's patches.
    for patch_file in ../libpd_patches/*.patch ; do
        patch -p1 --batch --forward < "$patch_file"
    done

    # Always build with debug symbols and frame pointers. The Makefile's
    # OPT_CFLAGS contains -fomit-frame-pointer, but ADDITIONAL_CFLAGS is
    # appended last, so -fno-omit-frame-pointer overrides it.
    LIBPD_DEBUG_FLAGS="-g -fno-omit-frame-pointer"

    LIBPD_LINK_FLAGS=""

    if uname -s |grep Darwin ; then
        # Build universal, same as the other Mac packages. (Debug builds of
        # Radium are arm64 only, while release builds are arm64+x86_64.)
        LIBPD_DEBUG_FLAGS="$LIBPD_DEBUG_FLAGS $DARWIN_ARCH_FLAGS $DARWIN_MIN_FLAGS"
        LIBPD_LINK_FLAGS="$DARWIN_ARCH_FLAGS $DARWIN_MIN_FLAGS"

        # The dylib is placed in the bundled packages directory, both for dev
        # builds (via the /tmp/radium_bin/packages symlink) and for Radium.app.
        LIBPD_LINK_FLAGS="$LIBPD_LINK_FLAGS -Wl,-install_name,@executable_path/packages/libpd/libs/libpd.dylib"

        # The Mac GUI needs Tcl/Tk 8.6. The system /usr/bin/wish is Tk 8.5,
        # which is killed by newer versions of MacOS, so let sys_startgui()
        # try one of these first. WISH must expand to a C string literal.
        for wish in /opt/local/libexec/tk-quartz/wish8.6 /opt/local/bin/wish8.6 /opt/homebrew/bin/wish8.6 ; do
            if [ -x "$wish" ] ; then
                LIBPD_DEBUG_FLAGS="$LIBPD_DEBUG_FLAGS -DWISH='\"$wish\"'"
                break
            fi
        done
    else
        LIBPD_LINK_FLAGS="-Wl,-soname,libpd.so"
    fi

    make clean
    make -j`nproc` libpd STATIC=true MULTI=true UTIL=true EXTRA=true ADDITIONAL_CFLAGS="$LIBPD_DEBUG_FLAGS" ADDITIONAL_LDFLAGS="$LIBPD_LINK_FLAGS"

    # Note: Radium's libpds wrapper (libpds/) for pd2 (not pd1) is compiled directly into the
    # Radium binary by Makefile.Qt, so there's nothing to build here.

    cd ..
}


# Build the dynamically loaded Pd externals used by the Pd2 instrument
# (OSC and networking objects). They are compiled against the libpd headers
# with the same multi-instance settings as libpd itself (-DPDINSTANCE
# -DPDTHREADS), since pd_this is a TLS symbol and the s_float/s_list globals
# are per-instance macros that are not exported by the MULTI build of libpd.
build_pd_externals() {

    COMPAT_HEADER="`pwd`/pd_externals_compat.h"
    LIBPD_INCLUDE="`pwd`/libpd/pure-data/src"
    EXTERNALS_DIR="`pwd`/../pd/externals"

    if uname -s |grep Darwin ; then
        PDEXT=pd_darwin
        # pd-lib-builder's name for the shared support library when both class
        # sources and shared sources are defined.
        SHARED_LIB=libiemnet.pd_darwin.dylib
        # pd-lib-builder builds for the host architecture by default, which is
        # the architecture Radium itself runs as. (The deployment target is
        # picked up from cflags by pd-lib-builder.)
        CFLAGS_EXTERNALS="-DPD -DPDINSTANCE -DPDTHREADS -mmacosx-version-min=${MACOSX_DEPLOYMENT_TARGET}"
    else
        PDEXT=pd_linux
        SHARED_LIB=libiemnet.pd_linux.so
        CFLAGS_EXTERNALS="-DPD -DPDINSTANCE -DPDTHREADS"
    fi

    rm -rf osc-0.3.1
    tar xzf osc-0.3.1.tar.gz
    # OSC_timeTag.c puts the storage class in the wrong order for compilers
    # where PERTHREAD expands to __thread ("__thread static").
    sed -i.bak 's/PERTHREAD static/static PERTHREAD/' osc-0.3.1/OSC_timeTag.c
    make -C osc-0.3.1 -j`nproc` PDINCLUDEDIR="$LIBPD_INCLUDE" \
        cflags="$CFLAGS_EXTERNALS"

    rm -rf pd-iemnet-0.3.0
    tar xzf pd-iemnet-0.3.0.tar.gz
    # The iemnet sources target Pd <= 0.51, which still had the global
    # error() function (pd_error() is the replacement). Makefile.local is
    # included by the iemnet Makefile, so the flags can be added without
    # overriding its own -DVERSION setting.
    echo "cflags += $CFLAGS_EXTERNALS -include $COMPAT_HEADER" \
        > pd-iemnet-0.3.0/Makefile.local
    make -C pd-iemnet-0.3.0 -j`nproc` PDINCLUDEDIR="$LIBPD_INCLUDE"

    rm -rf "$EXTERNALS_DIR"
    mkdir -p "$EXTERNALS_DIR"
    cp osc-0.3.1/*.$PDEXT osc-0.3.1/*-help.pd "$EXTERNALS_DIR/"
    cp pd-iemnet-0.3.0/*.$PDEXT "pd-iemnet-0.3.0/$SHARED_LIB" \
        pd-iemnet-0.3.0/*-help.pd pd-iemnet-0.3.0/udpsndrcv.pd "$EXTERNALS_DIR/"
}


build_qhttpserver() {

    rm -fr qhttpserver-master
    tar xvzf qhttpserver-master.tar.gz
    cd qhttpserver-master/
    echo "CONFIG += staticlib" >> src/src.pro
	patch -p1 <../qhttpserver_qt6.patch
    $QMAKE
    make -j`nproc` # necessary to create the moc files.
    cd ..
}


#rm -fr sndlib
#tar xvzf sndlib.tar.gz
#cd sndlib
#patch -p0 <../sndlib.patch
#cd ..


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
build_gc() {
    GC_VERSION=8.2.8
    LIBATOMIC_VERSION=7.8.0
    rm -fr gc-$GC_VERSION libatomic_ops-$LIBATOMIC_VERSION
    tar xvzf gc-$GC_VERSION.tar.gz
    #mv bdwgc-$GC_VERSION gc-$GC_VERSION
    tar xvzf libatomic_ops-$LIBATOMIC_VERSION.tar.gz
    cd gc-$GC_VERSION
    ln -s ../libatomic_ops-$LIBATOMIC_VERSION libatomic_ops
    #echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){ABORT("GC not configured properly");}' >>malloc.c
    echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){}' >>malloc.c
    echo '#if defined(GC_ASSERTIONS) || !defined(NO_DEBUGGING)' >>malloc.c
    echo "#error "nope"" >>malloc.c
    echo "#endif" >>malloc.c
    #patch -p1 <../gcdiff.patch
    #./autogen.sh
    CFLAGS="-mtune=generic -g -O2" ./configure --enable-static --disable-shared --disable-gc-debug --disable-gc-assertions
    CFLAGS="-mtune=generic -g -O2" make -j`nproc`
    cd ..
}

build_fluidsynth() {

    rm -fr fluidsynth-1.1.6
    tar xvzf fluidsynth-1.1.6.tar.gz
    cd fluidsynth-1.1.6
    make clean
    # Note: Old autoconf preprocessor sanity checks fail when CFLAGS/CPPFLAGS contain
    # multiple -arch flags ("cannot use 'cpp-output' output with multiple -arch options"),
    # so configure is run without arch flags, and arch flags are passed to make instead.
    CFLAGS="-fPIC -fno-strict-aliasing -O3 -DDEFAULT_SOUNDFONT=\\\"\\\"" CPPFLAGS="-fPIC -fno-strict-aliasing -O3" CXXFLAGS="-fPIC -fno-strict-aliasing -O3" LDFLAGS="" CC=$DASCC CXX=$DASCXX ./configure --enable-static --disable-aufile-support --disable-pulse-support --disable-alsa-support --disable-libsndfile-support --disable-portaudio-support --disable-oss-support --disable-midishare --disable-jack-support --disable-coreaudio --disable-coremidi --disable-dart --disable-lash --disable-ladcca --disable-aufile-support --disable-dbus-support --without-readline
    # --enable-debug
    make -j`nproc` CFLAGS="-fPIC -fno-strict-aliasing -O3 -DDEFAULT_SOUNDFONT=\\\"\\\" $DARWIN_MIN_FLAGS $DARWIN_ARCH_FLAGS" CPPFLAGS="-fPIC -fno-strict-aliasing -O3 $DARWIN_MIN_FLAGS $DARWIN_ARCH_FLAGS" CXXFLAGS="-fPIC -fno-strict-aliasing -O3 $DARWIN_MIN_FLAGS $DARWIN_ARCH_FLAGS" LDFLAGS="$DARWIN_MIN_FLAGS $DARWIN_ARCH_FLAGS"
    cd ..
}

build_libgig ()
{
	exit -1 # this stuff is done in the makefile now. The lines below are only kept for reference.
	rm -fr libgig
	tar xvjf libgig-4.2.0.tar.bz2
	mv libgig-4.2.0 libgig
	cd libgig
	patch -p1 <../libgig_dont_check_loops.patch
	patch -p1 <../libgig_win32.patch
	make clean
	CFLAGS="-O3 -fno-strict-aliasing" CPPFLAGS="-O3 -fno-strict-aliasing" CXXFLAGS="-O3 -fno-strict-aliasing" CC=$DASCC CXX=$DASCXX ./configure
	CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j8
	cd ..
}


build_qscintilla() {
    # QScintilla
    rm -fr QScintilla_gpl-2.10.8 QScintilla_src-2.14.0
    tar xvzf QScintilla_src-2.14.0.tar.gz 
    cd QScintilla_src-2.14.0/src
    echo "CONFIG += staticlib" >> qscintilla.pro
    if uname -s |grep Darwin ; then
	$QMAKE QMAKE_CFLAGS+="$DARWIN_ARCH_FLAGS" QMAKE_CXXFLAGS+="$DARWIN_ARCH_FLAGS" QMAKE_LFLAGS+="$DARWIN_ARCH_FLAGS"
	# install_name_tool can't handle fat archive members. Not needed for a static lib anyway.
	sed -i '' '/install_name_tool -id/d' Makefile
    else
	$QMAKE
    fi
    patch -p0 <../../qscintilla.patch
    make -j`nproc`
    cd ../..
}


#if [[ $RADIUM_QT_VERSION == 5 ]]
#then
#    rm -fr qtstyleplugins-src-5.0.0/
#    tar xvzf qtstyleplugins-src-5.0.0.tar.gz
#    cd qtstyleplugins-src-5.0.0/
#    `../../../find_moc_and_uic_paths.sh qmake`
#    CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
#    cd ../
#fi

build_xcb() {
    
    if ! is_0 ${RADIUM_BUILD_LIBXCB:-1} ; then
	
        rm -fr xcb-proto-1.13/
        tar xvjf xcb-proto-1.13.tar.bz2
        cd xcb-proto-1.13/
        mkdir install
        ./configure --prefix=`pwd`/install PYTHON=$PYTHONEXE
        make -j`nproc`
        make install
        cd ..
        
        rm -fr libxcb-1.13
        tar xvjf libxcb-1.13.tar.bz2 
        cd libxcb-1.13
        #patch -p1 <../libxcb-1.12.patch
        export XCBPROTO_LIBS=`pwd`/../xcb-proto-1.13/install/lib # don't append to PKG_CONFIG_PATH because of set -u (or use default args)
        export XCBPROTO_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" ./configure PYTHON=$PYTHONEXE
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
        cd ..
    
    fi
}

source ./build_python27.sh

build_faust
build_qhttpserver
build_gc
build_fluidsynth
build_python27
build_qscintilla # Note: Linking fails on Mac. Just ignore it.

if uname -s |grep Linux ; then
    if ! arch |grep -e arm -e aarch64 ; then
        build_libpds
    fi
    build_xcb
	echo "Finished compiling libpds and xcb" # need this line to avoid script failing if all the lines above are commented out.
fi

if uname -s |grep -e Linux -e Darwin ; then
    build_libpd
    build_pd_externals
    echo "Finished building libpd and pd externals" # need this line to avoid script failing if the line above is commented out.
fi


touch deletemetorebuild
