#!/usr/bin/env bash
#
# Wrapper for the faust_llm_test runner. Invoke this (not the binary, not
# make) so the opencode permission allow-rule for this exact command applies:
#
#     faust_llm_test/run.sh [options] <prompt> ...
#
# The wrapper builds the runner first (a no-op when it is up to date), then
# runs it with all arguments passed through. Use --log <file> to choose the
# LLM log path (otherwise $RADIUM_LLM_LOG or ~/.radium/llm.log is used).

set -eEu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

make -s -C "$SCRIPT_DIR" >/dev/null

exec "$SCRIPT_DIR/faust_llm_test" "$@"
