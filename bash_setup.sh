# Something like this should be included in the top of all the other bash scripts:
# source $(dirname "${0}")/bash_setup.sh

if [ -v RADIUM_BASH_SETUP_HAS_BEEN_SETUP ] ; then
    true ; return
fi

RADIUM_BASH_SETUP_HAS_BEEN_SETUP=1

#set -o verbose

set -u # error if using an unset variable
set -o errexit # same as "set -e"
set -o errtrace # same as "set -E". When enabled, $LINENO has the correct value in exit traps when trapped from a function.

source $(dirname "${0}")/vars.sh

print_error_and_exit () {
    	for line in "$@" ; do
	    printf "${RED}Error: ${LIGHT_CYAN}${line}${NC}\n"
	done
	
	exit -1
}


handle_failure () {
    print_error_and_exit $@
}

true ; return
# experimental exit trap handler below. Not very much point, just for fun.


par_print ()
{
    printf "par: ${LIGHT_CYAN}$@${NC}\n" >>/tmp/log.txt
}

MY_TEE_HAS_FLUSHED=0

notify_my_tee_has_flushed () {
    if [ $MY_TEE_HAS_FLUSHED -eq 1 ] ; then
	par_print "bash_setup.sh: Something is very wrong."
	exit -1
    fi    
    MY_TEE_HAS_FLUSHED=1
    par_print "Got USR1 signal. Setting 'has_flushed' to 1"
}

flush_my_tee () {
    if [ $MY_TEE_HAS_FLUSHED -eq 1 ] ; then
	par_print "$1: Something is very wrong."
	exit -1
    fi
    
    par_print "$1: flush-requester: Send req and go to sleep."

    echo "Dette går kankskje"
    printf "Eller dette\n"
    echo "$REQUEST_MY_TEE_TO_FLUSH_MESSAGE"

    while [ $MY_TEE_HAS_FLUSHED -eq 0 ] ; do
	#if wait ; then
	#    break
	#else
	#    break
	#fi
	sleep 0.1
    done

    MY_TEE_HAS_FLUSHED=0
    par_print "$1: flush-requester: Got woken up. Setting 'has_flushed' back to 0"
}

notify_tee_can_continue () {
    par_print "$1: Ready to continue. Sending USR1 signal."
    kill -s USR1 $MY_TEE_PID
}

PS4=' $LOG_SPLIT_WORD$(basename ${BASH_SOURCE})$LOG_SPLIT_WORD${LINENO}$LOG_SPLIT_WORD' # Include line numbers in error trace
#export PS4="  $LIGHT_CYAN$(basename ${BASH_SOURCE}):\${LINENO}:\t${GREEN} \$ "
#export PS4='  $(printit ${BASH_SOURCE} ${LINENO}) ' # "LIGHT_CYAN$(basename ${BASH_SOURCE}):\${LINENO}:\t${GREEN} \$ "

#####################################################
# Set up temp log file. Only used in failure handling.
# File is deleted afterwards.
#####################################################

TMP_LOG_FILE=$(mktemp -t radium_build_script_XXXXXXXXXX.${$}.temp_log)
if [ $? -ne 0 ]; then
    echo "$0: Unable to create temp file."
    exit -1
fi

if [ ! -f "${TMP_LOG_FILE}" ] ; then
    echo "$0: Temp file was not created: \"${TMP_LOG_FILE}\". (strange)"
    exit -1
fi
#set -o xtrace # same as "set -x"
#echo "TEMP: ${TMP_LOG_FILE}"

DO_DELETE_TMP_LOG_FILE=1

export IS_INSIDE_EXIT_TRAP=0

exit_trap () {
    set +o xtrace # If not the program just quits when calling wait below, and the script returns false.

    if [ $IS_INSIDE_EXIT_TRAP -eq 1 ] ; then
	par_print "Ouch. Bug in the error handler."
	exit -1
    fi

    par_print "EXIT_TRAP: ${IS_INSIDE_EXIT_TRAP}"
    
    IS_INSIDE_EXIT_TRAP=1
    
    # No need to manually call flush my_tee here. When my_tee gets this message,
    # everything we want have already been flushed.
    echo ${REQUEST_MY_TEE_TO_EXIT_MESSAGE}

    par_print "EXIT_TRAP: Waiting for tee to finish $BASHPID ${$} $MYPID"

    #sleep 5&
    #local sleep_pid=$!
    #wait -f $sleep_pid

    #local gotit=$(wait -f $MY_TEE_PID)
    #local gotit=$(wait -f $BASHPID)

    #par_print "GOTIT: -${gotit}-"
    #until [ ! ps -p $MY_TEE_PID ] ; do
	#par_print "wait..."
	#sleep 0.1
    #done

    #sleep 1
    if wait $MY_TEE_PID ; then
	par_print  "EXIT: wait returned true"
    else
	par_print "EXIT: wait returned false"
    fi

    [ $DO_DELETE_TMP_LOG_FILE -eq 1 ] && rm -f -- ${TMP_LOG_FILE}
    
    par_print "EXIT_TRAP: That's it, we finished at the right place."
    # (we'll never get here. Looks like when my-tee dies, we die to)
}

trap 'exit_trap' EXIT

#trap 'flush_my_tee EXIT_TRAP ; notify_tee_can_continue EXIT ; echo >/dev/null ; wait ; [ $DO_DELETE_TMP_LOG_FILE -eq 1 ] && rm -f -- ${TMP_LOG_FILE} ' EXIT

#echo "hello" | massage_log_line > /tmp/txt.txt
#exit 0


my_tee_no () {
    cat |
	while read line ; do
	    #echo "${line}" >> "${TMP_LOG_FILE}"
	    #if [ $DO_LOG_TO_FILE -eq 1 ] ; then
		#echo "${DO_LOG_TO_FILE}: $(massage_log_line \"${line}\")" >> "${TMP_LOG_FILE}"
	    #fi
	    printf "my_tee. LOG_TO_FILE: ${DO_LOG_TO_FILE}. Line: -${line}\n" | tee -p "${TMP_LOG_FILE}" #>/dev/null #>> "${TMP_LOG_FILE}"
	    #if [[ ! "$line" =~ $LOG_SPLIT_WORD ]] ; then
	#	echo "Line: ${line}"
	 #   fi
	done
}

MYPID=$BASHPID


#echo "HERE1"    
#if [ -v RADIUM_VERBOSE ] ; then
#exec  > >(massage_log_line | tee -p "${TMP_LOG_FILE}") #| echo "Piped from tee")
#exec  &> >(massage_log_line2 "${TMP_LOG_FILE}") #| echo "Piped from tee")

#echo "PID: $$. Name: $0 $1"
#sleep 500000
#exit

exec  &> >($BASH $(dirname "${0}")/my_tee.sh "${TMP_LOG_FILE}" $MYPID) #tee -p /dev/null | massage_log_line2 | grep -v "WEFIJOIJWEF") #"${TMP_LOG_FILE}") #| echo "Piped from tee")

MY_TEE_PID=$!

#exec  &> >(tee -p "${TMP_LOG_FILE}") #| echo "Piped from tee")
#exec | massage_log_line

#echo $$
#echo "HERE2"
#echo $$

trap 'notify_my_tee_has_flushed' USR1
#SIGCHILD

# BUG in bash 5.2. Can't use. Sigh.
#trap "echo" SIGCHLD


#echo "SIGCHILD: $SIGCHILD"
#trap 'notify_my_tee_has_flushed' SIGCHLD
#trap 'sigchild_trap' SIGCHLD


#trap 'par_print GOT SIGPIPIE' SIGPIPE

#else      
#    exec &> >(tee "${TMP_LOG_FILE}" | grep -v -E "^[ ]*${LOG_SPLIT_WORD}")
#fi

# Enable trace logging, sent to the log file.
set -o xtrace # same as "set -x"
#echo "HERE3"

#echo "PS4: -$PS4-"
PROGRAM_NAME_FULL_PATH="${0}"

PROGRAM_NAME=$(basename "$PROGRAM_NAME_FULL_PATH")
PROGRAM_ALL_ARGS="${0} $@"
PROGRAM_DIRNAME=`dirname $0`

#printf "============================  BASH SETUP FOR ${PROGRAM_ALL_ARGS}: Pid ${$}. Log-file: $TMP_LOG_FILE . my_tee pid: ${MY_TEE_PID} =======================================\n"


maybe_print_backtrace_line () {
    if ! caller $1 > /dev/null ; then
	return 0
    fi
    
    local trace=$(caller $1)
    local linenum=$(echo "$trace" | awk -F " " '{print $1}')
    local function=$(echo "$trace" | awk -F " " '{print $2}')
    local sourcefile=$(echo "$trace" | awk -F " " '{print $3}')
    local line="$(awk "NR==$linenum" "$(realpath $PROGRAM_DIRNAME)/$sourcefile")"
    printf "${RED}|${NC}    $1. $LIGHT_CYAN${sourcefile}:${linenum}${NC}. Func: ${LIGHT_CYAN}\"${function}\": ${GREEN} \"${line}\"${NC}\n"
}

IS_ALREADY_HANDLING_FAILURE=0

handle_failure () {
    #set +o xtrace
    if [ $IS_ALREADY_HANDLING_FAILURE -eq 1 ] ; then
	par_print "Ouch. Bug in the error handler."
	exit -1;
    fi

    IS_ALREADY_HANDLING_FAILURE=1
    
    flush_my_tee "HANDLE_FAILURE"

    local LOG=$(tail -n 22 ${TMP_LOG_FILE} | tac | tail -n+3)
    notify_tee_can_continue "HANDLE_FAILURE"
	
    DO_DELETE_TMP_LOG_FILE=0
    DO_LOG_TO_FILE=0
    #PS4=${DO_NOT_LOG_PS4}
    #sleep 3

    echo
    printf "${RED}+==============Error when running ${LIGHT_CYAN}\"${PROGRAM_ALL_ARGS}\". Pid: $BASHPID ${RED}==============================${NC}\n"
    #echo "${RED}|${NC}  Line: ${BASH_LINENO[0]}."
    if [ $# -ne 0 ] ; then
	printf "${RED}|${NC} ---------\n"
	printf "${RED}|${NC}  Message:\n"
	printf "${RED}|${NC} ---------\n"
	for line in "$@" ; do
	    printf "${RED}|${NC}     ${RED}Error: ${LIGHT_CYAN}${line}${NC}\n"
	done
    printf "${RED}| -------------------------------------------------------------${NC}\n"
    fi
    printf "${RED}|${NC} Backtrace using \"caller\":${NC}\n"
    #maybe_print_backtrace_line 0
    maybe_print_backtrace_line 1
    maybe_print_backtrace_line 2
    maybe_print_backtrace_line 3
    maybe_print_backtrace_line 4
    maybe_print_backtrace_line 5
    maybe_print_backtrace_line 6
    maybe_print_backtrace_line 7
    maybe_print_backtrace_line 8
    maybe_print_backtrace_line 9
    maybe_print_backtrace_line 10
    printf "${RED}| ------------------------------------------------------${NC}\n"
    printf "${RED}|${NC} Trace log. Last 20 (max) lines of ${GREEN}\"$TMP_LOG_FILE\"${NC} in reveres order:\n"
    #echo "${RED}|${NC} -----------------------------------"
    #echo "${LOG}" >/tmp/txt.txt
    #printf "${LOG}\n"
    #readarray -t lines <<< "$LOG"
 #$(tail -n 22 ${TMP_LOG_FILE} | head -n 20 | sed "s/++*/+/")
    #echo "LINES: $lines"
    #exit 0

    #tail -n 122 "${TMP_LOG_FILE}" | head -n 120
    #echo "============WC LOG-file1: $(wc ${TMP_LOG_FILE})"
    #sleep 3
    #echo "============WC LOG-file2: $(wc ${TMP_LOG_FILE})"
    #cat "${TMP_LOG_FILE}"
    local pos=1
    echo "${LOG}" | while IFS= read -r line ; do
	if echo "${line}" |grep " $TRACENUM_PLACEHOLDER " > /dev/null ; then
	    echo "${line}" |sed "s/ $TRACENUM_PLACEHOLDER /${pos}. /"
	    pos=$((pos + 1))
	else
	    echo "$line"
	fi	    
    done
    
    #for line in "${LOG}" ; do
#	printf "\n================================================NEW LINE==========================================\n"
 #   done
    
    #while IFS="" read -r line; do
	#massage_log_line $line
    #done <<< "${LOG}"
    printf "${RED}+----------------------------------------------------------------${NC}\n"
    #echo "============WC LOG-file3: $(wc ${TMP_LOG_FILE})"
    #echo "BASH_SETUP exited" >>/tmp/log.txt
    echo
    #flush_my_tee "HANDLE_FAILURE-end"
    
    exit -1
}

#handle_failure

#exit
trap 'handle_failure' ERR
#trap 'failure' ERR

#echo "par: where does this go1?" 1>&2
#echo "par: and this?" >/dev/stderr
#echo "par: finished"
#echo "par: where does this go2?" 1>&2
