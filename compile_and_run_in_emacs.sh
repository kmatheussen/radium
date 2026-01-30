#!/usr/bin/env bash

#set -x
set -eEu

runit="./run_gdb.sh"

print_help(){
	echo "  Usage in emacs:"
	echo
	echo "  1. M-x compile"
	echo
	echo "  2. Use this line as compile command:"
	echo
	echo "BUILDTYPE=DEBUG ./compile_and_run_in_emacs"
	echo
}

while [[ $# -gt 0 ]]; do
  case $1 in
    -dct|--dont-close-terminal)
		runit="bash -c '$runit ; bash'"
		shift
		;;
    -*|--*)
		echo
		echo "  Unknown option: \"$1\""
		echo
		print_help
		exit 1
		;;
  esac
done

#echo "RUNIT: \"$runit\""
#exit -1

#set -x

start_terminal_and_runit()
{	
	eval gnome-terminal --full-screen -- $runit
}

./build_linux.sh -j`nproc` && start_terminal_and_runit
