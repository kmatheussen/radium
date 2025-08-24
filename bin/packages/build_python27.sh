#!/bin/bash

set -eEu
#set -x

build_python27 () {
    local macos_patchfiles=$(cat <<-END
    patch-Makefile.pre.in.diff \
                    patch-setup.py.diff \
                    patch-Lib-cgi.py.diff \
                    patch-Lib-ctypes-macholib-dyld.py.diff \
                    patch-configure.diff \
                    patch-libedit.diff \
                    enable-loadable-sqlite-extensions.patch \
                    patch-_osx_support.py.diff \
                    darwin20.diff \
                    arm.patch \
                    implicit.patch \
                    openssl_ver.patch \
                    libedit-types.patch \
		    patch-getpath.diff
END
	  )

    rm -fr python27_install
    mkdir python27_install

    local version=2.7.18

    rm -fr Python-$version
    tar xvzf Python-$version.tgz

    cd Python-$version

    if uname |grep Darwin > /dev/null ; then
	for file in $macos_patchfiles ; do
	    echo "FILE: $file"
	    if [ ! -f ../python27_macport_patches/$file ] ; then
		echo "File not found: -" ../python27_macport_patches/$file + "-"
		exit -1
	    fi
	    patch -p0 < ../python27_macport_patches/$file
	done

	patch -p1 <../python27_disable_macos.patch

    else
	
	patch -p0 <../python27_disable_linux.patch
	
    fi
    
    CC=clang
    CXX=clang++
    CFLAGS=-Wno-implicit-function-declaration
    CPPCFLAGS=-Wno-implicit-function-declaration
    
    ./configure CC=clang CXX=clang++ CFLAGS=-Wno-implicit-function-declaration CPPCFLAGS=-Wno-implicit-function-declaration --prefix=$(realpath `pwd`/../python27_install) --without-gcc --without-threads --disable-ipv6 --without-system-ffi --disable-toolbox-glue --disable-framework --enable-shared
    # 
    # 
    # 
    #
    # --enable-optimizations

    CC=clang CXX=clang++ CFLAGS=-Wno-implicit-function-declaration CPPCFLAGS=-Wno-implicit-function-declaration make -j`nproc`

    make install
    export LD_LIBRARY_PATH="$(realpath `pwd`/../python27_install/lib)"${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
    cd ..
}

#build_python27

