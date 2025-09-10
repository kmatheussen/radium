



source $(dirname "${0}")/bash_setup.sh


assert_exe_exists()
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
set_var()
{
    if [ "$#" -ne 2 ]; then
	handle_failure "${FUNCNAME}: Illegal number of arguments: \"set_var $@\""
    fi

    if ! is_defined $1 ; then
	export $1="$2"
    fi
}

#export GAKK
#set_var GAKK 4
#echo "GAKK: -${GAKK}-"
#exit

