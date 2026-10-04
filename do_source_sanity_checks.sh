##!/usr/bin/env bash


set -eEu

export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

echo
echo "Doing some source sanity checks. This might take a few seconds...."
echo

remove_known_sanity_exceptions() {
	grep -v Python-2.7.18 \
		|grep -v python2.7/ \
		|grep -v lrdf.c \
		|grep -v unused_files \
		|grep -v GTK \
		|grep -v test\/ \
		|grep -v X11\/ \
		|grep -v amiga \
		|grep -v faust-examples \
		|grep -v temp\/ \
		|grep -v backup \
		|grep -v mingw \
		|grep -v Dropbox \
		|grep -v packages \
		|grep -v python-midi \
		|grep -v "No such file or directory" \
		|grep -v bu\/ \
		|grep -v pure-data \
		|grep -v libpd \
		|grep -v pluginhost \
		|grep -v bin/scheme \
		|grep -v rtmidi \
		|grep -v python \
		|grep -v weakjack \
		|grep -v radium_wrap_1.c \
		|grep -v keybindings.conf \
		|grep -v bin/help \
		|grep -v Visualization-Library-master \
		|grep -v faust_llm_test
}

if grep -n static\  */*.h */*.hpp */*/*.hpp */*/*/*.hpp */*/*/*/*.hpp */*/*.h */*/*/*/*.h */*/*.cpp */*.c */*.cpp */*.m */*/*.c */*/*.cpp */*/*/*.c */*/*/*/*.c */*/*/*.cpp 2>&1 | grep "\[" | grep -v "\[\]"|grep -v static\ void |grep -v "\[NO_STATIC_ARRAY_WARNING\]" |remove_known_sanity_exceptions ; then
	echo
	echo "ERROR in line(s) above. Static arrays may decrease GC performance notably.";
	echo
	exit -1
fi

if [ -d .git ] ; then
	if git grep -n R_ASSERT|grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\= |grep -v build_linux_common.sh ; then
        echo
        echo "ERROR in line(s) above. An R_ASSERT line is possibly wrongly used.";
        echo
        exit -1
	fi            
	
	if git grep -n assert\(|grep -v START_JUCE_APPLICATION |grep -v assert\(\) |grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\= |grep -v build_linux_common.sh ; then
        echo
        echo "ERROR in line(s) above. An R_ASSERT line is possibly wrongly used.";
        echo
        exit -1
	fi            
	
	if git grep -n -e if\( --or -e if\ \( *|grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\=|grep -v "\[NO_IF_=_WARNING\]" |remove_known_sanity_exceptions ; then
        echo
        echo "ERROR in line(s) above. A single '=' can not be placed on the same line as an if.";
        echo
        exit -1
	fi            
fi
