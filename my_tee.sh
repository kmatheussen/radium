#!/usr/bin/env bash

#set -o verbose
set -eEu
#set -x

source $(dirname "${0}")/vars.sh

#echo "tee: ===============================New my_tee process. Parent pid: $2. Parent log: $1 . My pid: $BASHPID ============================================="
#echo "tee: mytee: $$"
#echo "tee: parent pid: $2"
FILENAME=$1
PARENTPID=$2
RESTARGS=$@
#echo "tee: filename: $1"

# These values should not change...
# (Note: I used etimes= earlier to check the age of the parent, but sometimes the age wasn't updated, so it seemed like the process had died and someone else had taken over the pid.)
# Hopefully this works.
# (returns empty string if process doesn't exists)
get_constant_parent_ps_info() {
    ps -o group=,user=,args=,pgid=,lstart= -p $PARENTPID 2>/dev/null
}

testfunc ()
{
    local parent_ps_info=$(get_constant_parent_ps_info)

    if [ "${parent_ps_info}" != "" ] ; then
	echo "YES: -$parent_ps_info-"
    else
	echo "NO"
    fi
}
#testfunc
#echo "success"
#exit

PARENT_CONSTANT_PS_INFO=$(get_constant_parent_ps_info)

echo $PARENT_CONSTANT_PS_INFO

tee_print ()
{
    printf "tee: ${GREEN}$@${NC}\n" >>/tmp/log.txt
}

parent_is_alive ()
{
    parent_ps_info=$(get_constant_parent_ps_info)

    if [ "${parent_ps_info}" != "" ] ; then
	if [ "${parent_ps_info}" == "${PARENT_CONSTANT_PS_INFO}" ] ; then
	    true ; return
	else
	    tee_print "Someone else taken over parents pid? Now: \"${parent_ps_info}\". Expected: \"${PARENT_CONSTANT_PS_INFO}\""
	fi
    fi

    false
}

PARENT_HAS_FLUSHED=0

notify_that_parent_is_ready () {
    #tee_print "Got USR1 signal. Value: $PARENT_HAS_FLUSHED"
    if [ $PARENT_HAS_FLUSHED -eq 1 ] ; then
	tee_print "my_tee.sh: Something is very wrong."
	exit -1
    fi    
    PARENT_HAS_FLUSHED=1
    tee_print "Got USR1 signal. Setting 'parent_has_flushed' to 1. My Pid: $BASHPID"
}

trap 'notify_that_parent_is_ready' USR1

#trap 'tee_print Got SIGCHLD ' SIGCHLD
#trap 'tee_print GOT SIGPIPIE' SIGPIPE

flush () {
    if [ $PARENT_HAS_FLUSHED -eq 1 ] ; then
	tee_print "Flush: Something is very wrong."
	exit -1
    fi
    
    #tee_print "sending USR1 to parent"

    tee_print "Flush: Ajour. Sending back USR1 signal and go to sleep."
    
    kill -s USR1 $PARENTPID

    while [ $PARENT_HAS_FLUSHED -eq 0 ] ; do
	tee_print "Flush: Waiting... $BASHPID"
	if ! parent_is_alive ; then
	    tee_print "$1: Parent seems to have died. Exiting"
	    #while read -r line ; do
	#	massage_log_line "${line}"
	#	echo
	 #   done
	    exit
	    #break
	fi
	#if wait ; then
	#    true
	#else
	#    true
	#fi
	sleep 0.1
	    
	#while read -r line ; do
	#    massage_log_line "${line}"
	 #   echo
	#done
    done

    PARENT_HAS_FLUSHED=0
    tee_print "Flush: Woken up. Setting 'parent_has_flushed' back to 0. Going back to main loop."
}

massage_log_line () {    
    if [ $# -ne 1 ] ; then
	echo ""
	return
    fi

    local line="${1}"

    #echo "${line}"
    #return
    local indentation="  "
    if [[ "$line" =~ $LOG_SPLIT_WORD ]] ; then
	#indentation=$(echo "${line}" | awk -F "$LOG_SPLIT_WORD" '{print $1}') #| wc -m
	#indentation="$indentation$indentation"
	sourcefile=$(echo "${line}" | awk -F "$LOG_SPLIT_WORD" '{print $2}')
	linenum=$(echo "${line}" | awk -F "$LOG_SPLIT_WORD" '{print $3}')
	command=$(echo "${line}" | awk -F "$LOG_SPLIT_WORD" '{print $4}')
	printf "${RED}|${NC} $indentation$TRACENUM_PLACEHOLDER $LIGHT_CYAN${sourcefile}:${linenum}:\t${GREEN} \$ ${command}${NC}"
    else
	printf "${RED}|${NC}   $indentation > ${line}"
    fi
}

#usr1_trap(){
#    printf 'Here is the PID: %d\nExiting right-now!\n' $BASHPID
#    exit
#}

# Register USR1 signal handler
#trap 'echo "MY_TEE got USR1" ; sleep 0' USR1

#while :; do
#   sleep 9223372036854775807 & # int max (2^63 - 1)
#   wait $!
#done

while read -r line ; do
    #echo "${line}" >> "${TMP_LOG_FILE}"
    #if [ $DO_LOG_TO_FILE -eq 1 ] ; then
    #echo "${DO_LOG_TO_FILE}: $(massage_log_line \"${line}\")" >> "${TMP_LOG_FILE}"
    #fi
    #echo "*******line: \"${line}\" ${RESTARGS}}"
    if [ "${line}" == "" ] ; then
	echo ""
	true
    elif [[ "${line}" == "${DO_NOT_LOG_PS4}*" ]] ; then
	#echo "B"
	true
    elif [[ "${line}" == "$REQUEST_MY_TEE_TO_FLUSH_MESSAGE" ]] ; then
	#echo "E"
	flush "REQUEST_MY_TEE_TO_FLUSH_MESSAGE"
    elif [[ "${line}" == "${REQUEST_MY_TEE_TO_EXIT_MESSAGE}" ]] ; then
	tee_print "Main loop: Got request to exit"
	#echo "E: \"${line}\""
	#read -t 1 -n 10000 discard 
	#sleep 2
	#tee_print "exiting now\n\n\n"
	#exit 0 #sync_flush "REQUEST_MY_TEE_TO_FLUSH_MESSAGE"
	break
    else
	if [[ ! "$line" =~ $LOG_SPLIT_WORD ]] ; then
	    echo "${line}"
	fi	    
	#echo "F: \"${LOG_SPLIT_WORD}\""
	massaged=$(massage_log_line "${line}")
	#printf "my_tee. Line: -${line}-\n" | tee -p "${1}" #>/dev/null #>> "${TMP_LOG_FILE}"

	#echo "massaged: -${massaged}"
	echo "${massaged}" >>"${FILENAME}"
	#if [[ ! "$line" =~ $LOG_SPLIT_WORD ]] ; then
	#	echo "Line: ${line}"
	#   efi
	#kill -s USR1 $PARENTPID
    fi
done

#cat "${1}"

#echo "MY_TEE exited" >>/tmp/log.txt
#sleep 2    
tee_print "finished"
