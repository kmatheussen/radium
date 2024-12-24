



source $(dirname "${0}")/bash_setup.sh


assert_bin_exists()
{
    if [ ! -f $(which "${1}") ] ; then
	handle_failure "\"${1}\" doesn't seem to exist."
    fi
}

is_empty()
{
    [ -z $1 ]
}

# same as "has_value", except that it also returns true for empty variables.
# Note: call like this: is_defined VAR_NAME
# Not like this: is_defined $VAR_NAME
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

is_0()
{
    [ "${1}" == "0" ]
}

#trap 'handle_failure' ERR

assert_var_value()
{
    if [ "${!1}" != "$2" ] ; then
	handle_failure "\"${1}\" should have the value \"$2\". Instead it has the value \"${!1}\""
    fi	
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
	handle_failure "${FUNCNAME}: Illegal number of arguments: \"set_var $@\""
    fi
	
    if ! is_defined $1 ; then
	export $1=$2
    fi
    return
    
    local val="${!1}"

    echo "1: -${1}- 2: -${2}- val: ${val}"

    if is_empty $val ; then
	export $1=1
    elif [ "${val}" != "0" ] ; then	
	uns $1
    fi
}

#export GAKK
#set_var GAKK 4
#echo "GAKK: -${GAKK}-"
#exit

