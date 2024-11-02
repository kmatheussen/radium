

print_error_and_exit()
{
    RED='\033[1;31m'
    LIGHT_CYAN='\033[0;36m'
    NC='\033[0m'
    printf "${RED}Error: ${LIGHT_CYAN}${1}${NC}\n"
    exit -1
}

assert_env_path_exists()
{
    if is_empty ${!1} ; then
	print_error_and_exit "\"${1}\" must be set to a path"
    fi
    
    if [ ! -f ${!1} ] ; then
	print_error_and_exit "\"${!1}\" doesn't seem to exist. (As value for ${1})"
    fi
}


is_empty()
{
    [ -z $1 ]
}

# same as "has_value", except that it also returns true for empty variables.
is_defined()
{
    [ -v $1 ]
}

# returns true if defined and not empty
is_set()
{
    is_defined $1 && ! is_empty $1
}

# E.g "is_set_and_equals GAKK 5"
is_set_and_equals()
{
    is_defined $1 && [[ "${!1}" == "$2" ]]
}




# Sets a variable unless already set.
#
# Behavior:
#   * If the variable was already set, we use that value instead
#     and ignore the second argument. Othervice we set it to the
#     value of the second argument.
#   Afterwards
#   * If the value is empty, we set the variable to 1.
#   * If the value is 0, we unset the variable.
#
set_var()
{
    if [ "$#" -ne 2 ]; then
	echo "${FUNCNAME}: Illegal number of arguments: \"set_var $@\""
	exit -1
    fi
	
    if ! is_defined $1 ; then
	export $1=$2
    fi

    local val="${!1}"

    #echo "1: -${1}- 2: -${2}- val: ${val}"

    if is_empty $val ; then
	export $1=1
    elif [ "${val}" = "0" ] ; then
	unset $1
    fi
}

#export GAKK
#set_var GAKK 4
#echo "GAKK: -${GAKK}-"
#exit
