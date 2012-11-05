#!/bin/sh
# autoconf_builder.sh
# Created by Toshi Nagata on 07/04/18.
#
# Copyright 2007 Toshi Nagata.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted 
# provided that the following conditions are met:
#
#   1. Redistributions of source code must retain the above copyright notice, this list of 
# conditions and the following disclaimer.
#   2. Redistributions in binary form must reproduce the above copyright notice, this list of 
# conditions and the following disclaimer in the documentation and/or other materials provided 
# with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, 
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, 
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  Influential environmental variables:
#  ACTION(*): autoconf - run "make -f Makefile.cvs"; configure - run "configure";
#		make - run "make"; install - run "make install";
#		build (or empty string) - autoconf && configure && make (&& install)
#		"install" step is only done if WITH_INSTALL is non-empty
#  BUILD_STYLE(*): The build style. If it ends with "ppc" or "i386", it also specifies the architecture.
#		If it ends with "UB" or "Universal", it creates the Universal Binaries for the products
#		specified by $UB_PRODUCTS (see below).
#		If any of the products are missing, then this script is recursively called for 
#		$BUILD_STYLE's that have "ppc" and "i386" in place of "UB" or "Universal".
#  WITH_INSTALL: When non-empty, "install" action is automatically done in "build" action.
#  CFLAGS, CXXFLAGS: Compiler flags
#  CONFIG_OPTIONS: Options for "configure"
#  BUILD_BASE_DIR: Temporary building and install directory. By default "$PWD/build". The source
#		tree is duplicated (by use of symbolic links) in $BUILD_BASE_DIR/$BUILD_STYLE/<basename>.build,
#		where <basename> is the basename of the toplevel directory of the source tree (which also
#		should be the current directory when this script is invoked). The product is "installed"
#		into $BUILD_BASE_DIR/$BUILD_STYLE/local.
#  SDKROOT: The root directory for SDK
#  UB_PRODUCTS: The products for which Universal Binaries are to be created. The paths should be
#		relative to $BUILD_BASE_DIR/$BUILD_STYLE/local. Eg. bin/some_executable, lib/some_library.a.
#  UB_ARCHS: The target architectures for the "UB" build style. If not specified, "ppc i386" is assumed.
#  UB_ONEPASS: When non-empty, building "UB" is done with the compiler flag like "-arch i386 -arch ppc".
#       Otherwise, binaries for each architecture are separately built and combined later by lipo.
#
#  The variables marked with (*) are automatically set by Xcode.

BASE_DIR=$PWD
BASE_NAME=`basename $PWD`
UB_ARCHS=${UB_ARCHS:-"ppc i386"}

function rel2abs () {
	(cd "$1"; pwd)
}

function abs2rel () {
	/usr/bin/perl -e 'use File::Spec; print File::Spec->abs2rel($ARGV[0], $ARGV[1]), "\n";' "$1" "$2"
}

function link_recursive () {
	local arg base i
	arg="$1"
	base=`basename "$arg"`
	if test -d "$arg"; then
		if expr "$base" = "CVS" "|" `rel2abs "$arg"` = "$BUILD_BASE_DIR" >/dev/null; then
			echo "Skipping directory $arg"
		else
			echo "Copying directory $arg"
			mkdir -p "$base"
			cd "$base"
			if ! expr "$arg" : "^/" >/dev/null; then
				arg="../$arg"
			fi
			for i in "$arg"/*; do
				link_recursive "$i"
			done
			cd ..
		fi
	else
		echo "Linking $arg"
		ln -sf $arg
	fi
}

#  Sanity checks
if test "x$BUILD_STYLE" = "x"; then
	BUILD_STYLE="Default"
fi
if test "x$ACTION" = "x"; then
	ACTION="build"
fi
if test "x$BUILD_BASE_DIR" = "x"; then
	BUILD_BASE_DIR=$PWD/../temp_build
fi
mkdir -p "$BUILD_BASE_DIR" || exit $?
export BUILD_BASE_DIR=`rel2abs "$BUILD_BASE_DIR"`

if test -e "$SDKROOT"; then
	SYSROOT_CFLAGS="-isysroot $SDKROOT"
else
	SYSROOT_CFLAGS=
fi
if ! expr "$arg" : ".*/usr/local/bin.*" >/dev/null; then
	PATH=${PATH/\/usr\/bin/\/usr\/local\/bin:\/usr\/bin}
fi
ARCH_CFLAGS=""
ARCH_CONFIG_OPTIONS=""
case "$BUILD_STYLE" in
    *ppc)
	ARCH="ppc"
	ARCH_CFLAGS="-arch ppc"
	;;
	*ppc64)
	ARCH="ppc64"
	ARCH_CFLAGS="-arch ppc64"
	ARCH_CONFIG_OPTIONS="--host=ppc64-apple-darwin8"
	if expr "$BASE_NAME" : "libsndfile" >/dev/null; then
		#  For shortcut endianness detection in libsndfile 1.0.17
		export ac_cv_c_byte_order=big
	fi
	;;
    *i386)
	ARCH="i386"
	ARCH_CFLAGS="-arch i386 -msse -msse2"
	ARCH_CONFIG_OPTIONS="--host=i386-apple-darwin8"
	;;
	*x86_64)
	ARCH="x86_64"
	ARCH_CFLAGS="-arch x86_64 -m64"
	ARCH_CONFIG_OPTIONS="--host=x86_64-apple-darwin8"
	;;
	*UB)
	ARCH="UB"
	BUILD_STYLE_BASE=${BUILD_STYLE/UB/}
	;;
    *Universal)
	ARCH="UB"	
	BUILD_STYLE_BASE=${BUILD_STYLE/Universal/}
	;;
    *)
	echo "Warning: architecture cannot be recognized from the build style"
	ARCH="unknown"
	;;
esac

case "$BUILD_STYLE" in
    Development|Default)
	OPT_CFLAGS="-O2 -g"
	;;
    Deployment*)
	OPT_CFLAGS="-O3"
	;;
esac

if test "x$ARCH" = "xUB"; then
	for arch in $UB_ARCHS; do
		case $arch in
			ppc)
			ARCH_CFLAGS="$ARCH_CFLAGS -arch ppc"
			;;
			*ppc64)
			ARCH_CFLAGS="$ARCH_CFLAGS -arch ppc64"
			;;
			*i386)
			ARCH_CFLAGS="$ARCH_CFLAGS -arch i386 -msse -msse2"
			;;
			*x86_64)
			ARCH_CFLAGS="$ARCH_CFLAGS -arch x86_64 -m64"
			;;
		esac
	done
fi
		
if test "x$ARCH" = "xUB" -a "x$ACTION" != "xclean" -a "x$UB_ONEPASS" = "x"; then
	#  Test the existence of the products
	for arch in $UB_ARCHS; do
		style=${BUILD_STYLE_BASE}${arch}
#	BUILD_STYLE_PPC=${BUILD_STYLE_BASE}ppc
#	BUILD_STYLE_386=${BUILD_STYLE_BASE}i386
#	for style in $BUILD_STYLE_PPC $BUILD_STYLE_386; do
		missing=no
		for i in $UB_PRODUCTS; do
			if ! test -e "$BUILD_BASE_DIR/$style/local/$i"; then
				missing=yes
			fi
		done
		if test "$missing" = "yes"; then
			BUILD_STYLE_SAVE=$BUILD_STYLE
			export BUILD_STYLE=$style
			echo "Building with BUILD_STYLE=$style"
			/bin/sh $0 || exit $?
			BUILD_STYLE=$BUILD_STYLE_SAVE
		fi
	done
	mkdir -p "$BUILD_BASE_DIR/$BUILD_STYLE/local" || exit $?
	cd "$BUILD_BASE_DIR"
	for i in $UB_PRODUCTS; do
		archbins=""
		for arch in $UB_ARCHS; do
			archbins="$archbins $BUILD_BASE_DIR/${BUILD_STYLE_BASE}${arch}/local/$i"
		done
		mkdir -p "$BUILD_STYLE/local/"`dirname $i` || exit $?
		echo "Creating universal binary $BUILD_STYLE/local/$i"
		lipo -create $archbins -output "$BUILD_STYLE/local/$i" || exit $?
	done
	exit $?
fi

export CFLAGS="$SYSROOT_CFLAGS $ARCH_CFLAGS $OPT_CFLAGS $CFLAGS"
export CXXFLAGS="$SYSROOT_CFLAGS $ARCH_CFLAGS $OPT_CFLAGS $CXXFLAGS"

#  Move to the working directory
BUILD_DIR="$BUILD_BASE_DIR/$BUILD_STYLE/$BASE_NAME.build"
mkdir -p "$BUILD_DIR"
BUILD_DIR=`rel2abs "$BUILD_DIR"`
CONFIG_OPTIONS="--prefix=$BUILD_BASE_DIR/$BUILD_STYLE/local $CONFIG_OPTIONS"

#  Display all environments
set

cd $BUILD_DIR

#  Clean if specified
if test "x$ACTION" = "xclean"; then
	# if test "x$WITH_INSTALL" != "x" -a -e "Makefile"; then
	#	echo "Doing make uninstall"
	#	make uninstall
	# fi
    echo "Removing files in $BUILD_DIR"
	cd $BASE_DIR
    rm -rf "$BUILD_DIR"
	echo "Removing product files in $BUILD_BASE_DIR/$BUILD_STYLE/local"
	(cd "$BUILD_BASE_DIR/$BUILD_STYLE/local"; rm -rf $UB_PRODUCTS)
    exit $?
fi

if test "x$ACTION" = "xbuild" -o "x$ACTION" = "xconfigure"; then
	#  Copy the source files if necessary
	if test ! -e Makefile -a ! -e configure -a ! -e Makefile.cvs; then
		echo "Copying the source files to $BUILD_DIR"
		for i in `abs2rel "$BASE_DIR" "$BUILD_DIR"`/*; do
			link_recursive $i
		done
	fi
	#  Make ./configure if necessary
	if test -e Makefile.cvs -a ! -e configure; then
		echo "Running make -f Makefile.cvs"
		make -f Makefile.cvs || exit $?
	fi
	#  Run ./configure if necessary
	if test -e configure -a ! -e Makefile; then
		CONFIG_ARGS="$CONFIG_OPTIONS $ARCH_CONFIG_OPTIONS $CONFIG_ENVS"
		echo "Running configure $CONFIG_ARGS"
		./configure $CONFIG_ARGS || exit $?
	fi
fi

if test "x$ACTION" = "xbuild" -o "x$ACTION" = "xmake"; then
	#  make
	echo "Running make"
	make || exit $?
fi

#  make install if specified
if test "x$ACTION" = "xinstall" -o "(" "x$ACTION" = "xbuild" -a "x$WITH_INSTALL" != "x" ")"; then
    echo "Running make install"
    make install || exit $?
fi
