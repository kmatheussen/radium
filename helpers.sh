

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



is_set(){
    [ -v $1 ]
}

#export GAKK=""
#if is_set GAKK ; then
#    echo SET
#else
#    echo NOT
#fi
#exit

# Sets a variable unless already set.
#
# Behavior:
#   * If the variable was already set, we use that value instead and ignore the second argument.
#   Then:
#   * If the value is different than 0 (even empty), the function will set the value to 1.
#   * If the value is 0, the function will unset the value.
#
set_var()
{
    if ! is_set $1 ; then
	export $1=$2
    fi

    local val=${!1}

    # Are the two first tests below doing exactly the same?

    if [ "${val}" -eq "0" ] ; then
	echo "A"
	unset $1
    elif [ ${val} -eq 0 ] ; then
	echo "B"
	unset $1
    else
	export $1=1
    fi
}

#export GAKK
#set_var GAKK 4
#echo "GAKK: -${GAKK}-"
#exit
