set -eEu

if [ -v RADIUM_BASH_VARS_HAS_BEEN_SETUP ] ; then
    return 0
fi

export RADIUM_BASH_VARS_HAS_BEEN_SETUP=1

export RED='\033[1;31m'
export LIGHT_CYAN='\033[0;36m'
export GREEN='\033[0;32m'
export YELLOW='\033[0;33m'
export NC='\033[0m'

export DO_NOT_LOG_PS4="DONOTLOG"
export LOG_SPLIT_WORD="__s__"

export REQUEST_MY_TEE_TO_FLUSH_MESSAGE="REQUEST_MY_TEE_TO_FLUSH"
export REQUEST_MY_TEE_TO_EXIT_MESSAGE="REQUEST_MY_TEE_TO_EXIT"

export DO_LOG_TO_FILE=1

export TRACENUM_PLACEHOLDER="_"
