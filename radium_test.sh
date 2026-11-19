#!/usr/bin/env bash
#
# Stateful launcher for testing Radium on a private X server.
#
# Usage:
#   ./radium_test.sh [global options] <command> [arguments...]
#
# Commands:
#   start [radium args...]   Start Radium and wait until the GUI is ready
#   stop                     Stop the running instance (keeps the log)
#   restart [radium args...] Stop, then start
#   status                   Print state; exit 0 if running
#   port                     Print the control port
#   display                  Print the X display name
#   scheme '<expr>'          Evaluate a Scheme expression via the control server
#   shot [file]              Save a screenshot (default <state>/screenshot.png)
#   xwd <args...>            Run xwd against the private display
#   xdotool <args...>        Run xdotool against the private display
#   windows                  List window titles on the private display
#   tree [N]                 Show the first N lines of the window tree
#   xwininfo [args...]       Run xwininfo against the private display
#   sleep <seconds>          Sleep (keeps waits inside one invocation)
#   log [N]                  Show the last N log lines (default 200)
#   log -f                   Follow the log
#   attach [lldb|gdb]        Attach a debugger, continue, dump a backtrace on a stop
#   bt                       Show the backtrace from the last attach
#   clean                    Stop and remove the state directory
#
# Global options (before the command):
#   --state DIR              State directory (default /tmp/opencode/radium-test)
#   --backend opengl|null    GPU backend (default opengl)
#   --fallback               Use the current $DISPLAY instead of Xvfb
#   --keep-home              Keep the throwaway HOME directory
#
# Environment:
#   RADIUM_TEST_STATE               Default state directory
#   RADIUM_TEST_START_TIMEOUT=180   Seconds to wait for the server and window
#   RADIUM_TEST_SETTLE=2            Extra seconds after the window appears
#   RADIUM_TEST_STOP_TIMEOUT=45     Seconds to wait for the helper to stop
#   RADIUM_XVFB_STOP_TIMEOUT=30     Seconds the helper waits after SIGTERM before
#                                   it kills Radium
#   RADIUM_XVFB_BACKEND, RADIUM_XVFB_FALLBACK, RADIUM_XVFB_KEEP_HOME
#                                   Defaults for the options above
#
# The low-level work is done by radium_xvfb.sh. This wrapper adds a persistent
# state directory, so each step of a test is a single short command instead of
# an ad-hoc shell script with background jobs, loops and exported variables.
# That is also why it lives outside bin/: install.sh must not ship it.
#
# Every invocation starts with "./radium_test.sh ...", so one permission rule
# ("./radium_test.sh *") covers the whole test workflow.

set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
xvfb="$repo/radium_xvfb.sh"

state="${RADIUM_TEST_STATE:-/tmp/opencode/radium-test}"
backend="${RADIUM_XVFB_BACKEND:-opengl}"
fallback="${RADIUM_XVFB_FALLBACK:-0}"
keep_home="${RADIUM_XVFB_KEEP_HOME:-0}"

die() {
  printf 'radium_test: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage: ./radium_test.sh [global options] <command> [arguments...]

Commands:
  start [radium args...]   Start Radium and wait until the GUI is ready
  stop                     Stop the running instance (keeps the log)
  restart [radium args...] Stop, then start
  status                   Print state; exit 0 if running
  port                     Print the control port
  display                  Print the X display name
  scheme '<expr>'          Evaluate a Scheme expression via the control server
  shot [file]              Save a screenshot (default <state>/screenshot.png)
  xwd <args...>            Run xwd against the private display
  xdotool <args...>        Run xdotool against the private display
  windows                  List window titles on the private display
  tree [N]                 Show the first N lines of the window tree
  xwininfo [args...]       Run xwininfo against the private display
  sleep <seconds>          Sleep (keeps waits inside one invocation)
  log [N]                  Show the last N log lines (default 200)
  log -f                   Follow the log
  attach [lldb|gdb]        Attach a debugger, continue, dump a backtrace on a stop
  bt                       Show the backtrace from the last attach
  clean                    Stop and remove the state directory

Global options (before the command):
  --state DIR              State directory (default /tmp/opencode/radium-test)
  --backend opengl|null    GPU backend (default opengl)
  --fallback               Use the current $DISPLAY instead of Xvfb
  --keep-home              Keep the throwaway HOME directory

See the header of this script for the environment variables.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --state)
      [ $# -ge 2 ] || die "--state needs a directory"
      state="$2"
      shift 2
      ;;
    --state=*)
      state="${1#--state=}"
      shift
      ;;
    --backend)
      [ $# -ge 2 ] || die "--backend needs a value (opengl or null)"
      backend="$2"
      shift 2
      ;;
    --backend=*)
      backend="${1#--backend=}"
      shift
      ;;
    --fallback)
      fallback=1
      shift
      ;;
    --keep-home)
      keep_home=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    -*)
      die "unknown option: $1 (see --help)"
      ;;
    *)
      break
      ;;
  esac
done

[ $# -ge 1 ] || {
  usage >&2
  exit 2
}

command_name="$1"
shift

case "$backend" in
  opengl|null) ;;
  *) die "backend must be \"opengl\" or \"null\", not \"$backend\"" ;;
esac

[ "$fallback" = 0 ] || [ "$fallback" = 1 ] ||
  die "fallback must be 0 or 1"
[ "$keep_home" = 0 ] || [ "$keep_home" = 1 ] ||
  die "keep-home must be 0 or 1"

mkdir -p "$state"
state="$(cd "$state" && pwd)"

portfile="$state/control-port"
displayfile="$state/display"
pidfile="$state/helper.pid"
radium_pidfile="$state/radium.pid"
exitstatusfile="$state/exit-status"
debugger_log="$state/debugger.log"
logfile="$state/radium.log"
default_shot="$state/screenshot.png"

helper_pid() {
  [ -s "$pidfile" ] || return 1
  local pid
  pid="$(cat "$pidfile")"
  case "$pid" in
    ''|*[!0-9]*) return 1 ;;
  esac
  kill -0 "$pid" 2>/dev/null || return 1

  # Make sure the PID was not recycled by an unrelated process.
  if [ -r "/proc/$pid/cmdline" ]; then
    case "$(tr '\0' ' ' <"/proc/$pid/cmdline")" in
      *radium_xvfb.sh*) ;;
      *) return 1 ;;
    esac
  fi

  printf '%s\n' "$pid"
}

is_running() {
  helper_pid >/dev/null 2>&1
}

read_port() {
  [ -s "$portfile" ] || return 1
  local port
  port="$(cat "$portfile")"
  case "$port" in
    ''|*[!0-9]*) return 1 ;;
  esac
  printf '%s\n' "$port"
}

read_display() {
  [ -s "$displayfile" ] || return 1
  local display
  display="$(cat "$displayfile")"
  [ -n "$display" ] || return 1
  printf '%s\n' "$display"
}

# Prints the pid of the running Radium process, if the wrapper recorded one and
# the process (and its pid) are still ours.
read_radium_pid() {
  [ -s "$radium_pidfile" ] || return 1
  local pid
  pid="$(cat "$radium_pidfile")"
  case "$pid" in
    ''|*[!0-9]*) return 1 ;;
  esac
  kill -0 "$pid" 2>/dev/null || return 1
  if [ -r "/proc/$pid/cmdline" ]; then
    case "$(tr '\0' ' ' <"/proc/$pid/cmdline")" in
      *radium_linux.bin*) ;;
      *) return 1 ;;
    esac
  fi
  printf '%s\n' "$pid"
}

# Explains how the last Radium process ended: cleanly, killed by SIGKILL, or
# with an error (a crash usually exits with status 1).
report_exit_status() {
  if [ ! -s "$exitstatusfile" ]; then
    printf 'radium_test: no Radium exit status was recorded (the wrapper was killed before it could write one)\n' >&2
    return 0
  fi

  local status
  status="$(cat "$exitstatusfile")"

  case "$status" in
    0)
      printf 'Radium exited cleanly (status 0)\n'
      ;;
    137)
      printf 'radium_test: Radium was killed by SIGKILL, not crashed. The wrapper gave up after RADIUM_XVFB_STOP_TIMEOUT=%ss; raise it if Radium needs longer to shut down.\n' \
        "${RADIUM_XVFB_STOP_TIMEOUT:-30}" >&2
      ;;
    1)
      printf 'radium_test: Radium exited with status 1, which usually means the crash reporter handled a crash. See %s\n' "$logfile" >&2
      ;;
    134)
      printf 'radium_test: Radium was aborted by SIGABRT (status 134), usually an assertion or a sanitizer error. See %s\n' "$logfile" >&2
      ;;
    143)
      printf 'radium_test: Radium was terminated by SIGTERM (status 143) before it could shut down. This is not a crash.\n' >&2
      ;;
    *)
      printf 'radium_test: Radium exited with status %s. See %s\n' "$status" "$logfile" >&2
      ;;
  esac
}

clear_run_files() {
  rm -f "$pidfile" "$portfile" "$displayfile" "$radium_pidfile"
}

require_port() {
  read_port || die "Radium is not running (no control port in $state; use ./radium_test.sh start)"
}

require_display() {
  read_display || die "no display recorded in $state; is Radium running?"
}

port_answers() {
  local port="$1"
  curl -s --max-time 2 -o /dev/null \
    -X POST -H 'Content-Type: text/plain' -d '(+ 1 2)' \
    "http://127.0.0.1:$port/"
}

window_exists() {
  local display="$1"
  command -v xdotool >/dev/null 2>&1 || return 0
  DISPLAY="$display" xdotool search --classname radium >/dev/null 2>&1
}

tail_log() {
  if [ -f "$logfile" ]; then
    tail -n 40 "$logfile" >&2 || true
  fi
}

do_start() {
  command -v curl >/dev/null 2>&1 || die "curl was not found"
  [ -x "$xvfb" ] || die "$xvfb is not executable"

  if is_running; then
    die "already running (pid $(cat "$pidfile"), port $(cat "$portfile" 2>/dev/null || echo '?')); use stop or restart"
  fi

  clear_run_files
  rm -f "$exitstatusfile" "$debugger_log"
  : >"$logfile"

  RADIUM_CONTROL_PORT_FILE="$portfile" \
  RADIUM_XVFB_DISPLAY_FILE="$displayfile" \
  RADIUM_XVFB_RADIUM_PID_FILE="$radium_pidfile" \
  RADIUM_XVFB_EXIT_STATUS_FILE="$exitstatusfile" \
  RADIUM_XVFB_BACKEND="$backend" \
  RADIUM_XVFB_FALLBACK="$fallback" \
  RADIUM_XVFB_KEEP_HOME="$keep_home" \
    nohup "$xvfb" "$@" >"$logfile" 2>&1 &

  local pid=$!
  printf '%s\n' "$pid" >"$pidfile"

  local timeout="${RADIUM_TEST_START_TIMEOUT:-180}"
  local started=$SECONDS
  local port="" display=""

  # Wait until the control server answers. The port file is written before the
  # GUI is ready, so this alone is not enough.
  while [ $((SECONDS - started)) -lt "$timeout" ]; do
    if ! kill -0 "$pid" 2>/dev/null; then
      printf 'radium_test: Radium exited during startup\n' >&2
      tail_log
      clear_run_files
      exit 1
    fi
    if port="$(read_port)" && display="$(read_display)" &&
       port_answers "$port"; then
      break
    fi
    port=""
    display=""
    sleep 0.1
  done

  if [ -z "$port" ] || [ -z "$display" ]; then
    printf 'radium_test: timed out after %ss waiting for the control server\n' \
      "$timeout" >&2
    tail_log
    clear_run_files
    exit 1
  fi

  # Wait for the editor window. This is best effort: a missing xdotool or an
  # unexpected window class should not fail an otherwise healthy start.
  local window_timeout="${RADIUM_TEST_WINDOW_TIMEOUT:-30}"
  local window_deadline=$((SECONDS + window_timeout))
  while ! window_exists "$display" &&
        [ "$SECONDS" -lt "$window_deadline" ] &&
        kill -0 "$pid" 2>/dev/null; do
    sleep 0.2
  done

  if ! kill -0 "$pid" 2>/dev/null; then
    printf 'radium_test: Radium exited during startup\n' >&2
    tail_log
    clear_run_files
    exit 1
  fi

  if ! window_exists "$display"; then
    printf 'radium_test: warning: no Radium window appeared after %ss\n' \
      "$window_timeout" >&2
    printf 'radium_test: check ./radium_test.sh windows and ./radium_test.sh shot\n' >&2
  fi

  local settle="${RADIUM_TEST_SETTLE:-2}"
  if [ "$settle" != 0 ]; then
    sleep "$settle"
  fi

  printf 'state=%s\nport=%s\ndisplay=%s\nlog=%s\n' \
    "$state" "$port" "$display" "$logfile"
}

do_stop() {
  local pid radium_pid stopped=0 killed=0
  if pid="$(helper_pid)"; then
    stopped=1

    # Ask Radium itself to quit first. The wrapper is then still in its own
    # "wait", so it reaps Radium normally and records the real exit status
    # instead of being interrupted by our TERM.
    if radium_pid="$(read_radium_pid)"; then
      kill -TERM "$radium_pid" 2>/dev/null || true

      local radium_timeout="${RADIUM_XVFB_STOP_TIMEOUT:-30}"
      local radium_deadline=$((SECONDS + radium_timeout))
      while kill -0 "$radium_pid" 2>/dev/null && [ "$SECONDS" -lt "$radium_deadline" ]; do
        sleep 0.05
      done

      if kill -0 "$radium_pid" 2>/dev/null; then
        printf 'radium_test: Radium did not exit within %ss after SIGTERM, sending SIGKILL. This is NOT a Radium crash.\n' \
          "$radium_timeout" >&2
        kill -KILL "$radium_pid" 2>/dev/null || true
        killed=1
      fi
    else
      # No usable pid (for instance a crash during startup), so let the
      # wrapper do the shutdown.
      kill -TERM "$pid" 2>/dev/null || true
    fi

    # The wrapper exits as soon as Radium is gone.
    local timeout="${RADIUM_TEST_STOP_TIMEOUT:-45}"
    local deadline=$((SECONDS + timeout))
    while kill -0 "$pid" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do
      sleep 0.1
    done

    if kill -0 "$pid" 2>/dev/null; then
      printf 'radium_test: helper did not stop within %ss, killing it\n' \
        "$timeout" >&2
      kill -KILL "$pid" 2>/dev/null || true
    fi
  fi

  clear_run_files

  # Explain how Radium ended, but stay quiet for a state directory that never
  # ran anything.
  if [ "$killed" = 1 ] && [ ! -s "$exitstatusfile" ]; then
    printf 'radium_test: Radium was killed by SIGKILL, not crashed (the wrapper did not record an exit status).\n' >&2
  elif [ "$stopped" = 1 ] || [ -s "$exitstatusfile" ]; then
    report_exit_status
  fi
}

do_status() {
  local pid port display
  if pid="$(helper_pid)"; then
    port="$(read_port 2>/dev/null || echo '?')"
    display="$(read_display 2>/dev/null || echo '?')"
    printf 'running pid=%s port=%s display=%s state=%s\n' \
      "$pid" "$port" "$display" "$state"
    return 0
  fi

  clear_run_files
  if [ -s "$exitstatusfile" ]; then
    printf 'not running state=%s last-exit=%s\n' \
      "$state" "$(cat "$exitstatusfile")"
  else
    printf 'not running state=%s\n' "$state"
  fi
  return 1
}

# Attach a debugger to the running Radium process and continue it. When the
# process stops (crash, assert, SIGSEGV, ...) the backtrace is written to
# <state>/debugger.log. The command blocks until that happens; run it in the
# background, interact with Radium, and use "./radium_test.sh bt" afterwards.
do_attach() {
  local debugger="${1:-lldb}"

  [ $# -le 1 ] || die "attach takes at most one argument (lldb or gdb)"
  case "$debugger" in
    lldb|gdb) ;;
    *) die "attach expects \"lldb\" or \"gdb\", not \"$debugger\"" ;;
  esac
  command -v "$debugger" >/dev/null 2>&1 || die "$debugger was not found"

  local pid
  pid="$(read_radium_pid)" ||
    die "no running Radium found in $radium_pidfile (start it first)"

  : >"$debugger_log"
  printf 'radium_test: attaching %s to Radium (pid %s); this blocks until Radium stops or exits\n' \
    "$debugger" "$pid" >&2

  case "$debugger" in
    lldb)
      # -k is required: after "process continue" stops at a fatal signal,
      # batch mode quits instead of running the following -o commands.
      "$debugger" --batch \
        -k "thread backtrace all" \
        -p "$pid" \
        -o "process continue" >"$debugger_log" 2>&1 || true
      ;;
    gdb)
      "$debugger" --batch \
        -ex "set pagination off" \
        -ex "handle SIGTERM nostop noprint pass" \
        -p "$pid" \
        -ex "continue" \
        -ex "thread apply all backtrace" \
        -ex "detach" >"$debugger_log" 2>&1 || true
      ;;
  esac

  printf '%s\n' "$debugger_log"
  printf 'radium_test: last %s lines of %s:\n' "$debugger" "$debugger_log" >&2
  tail -n 30 "$debugger_log" >&2 || true

  if kill -0 "$pid" 2>/dev/null; then
    printf 'radium_test: Radium (pid %s) is still running; the debugger only paused it. Use ./radium_test.sh stop when you are done.\n' \
      "$pid" >&2
  fi
}

do_bt() {
  [ -f "$debugger_log" ] ||
    die "no debugger log at $debugger_log (use ./radium_test.sh attach first)"
  cat "$debugger_log"
}

do_scheme() {
  [ $# -ge 1 ] || die "scheme needs a quoted expression, e.g. '(+ 1 2)'"
  local expr="$1"
  local port
  port="$(require_port)"
  curl -sS --fail --max-time 30 \
    -X POST -H 'Content-Type: text/plain' --data-binary "$expr" \
    "http://127.0.0.1:$port/"
}

do_shot() {
  local out="${1:-$default_shot}"
  local display
  display="$(require_display)"
  command -v xwd >/dev/null 2>&1 || die "xwd was not found"
  command -v ffmpeg >/dev/null 2>&1 || die "ffmpeg was not found"

  mkdir -p "$(dirname "$out")"
  local tmp
  tmp="$(mktemp "${TMPDIR:-/tmp}/radium-shot.XXXXXX.xwd")"

  DISPLAY="$display" xwd -root -silent -out "$tmp"
  ffmpeg -loglevel error -y -i "$tmp" "$out"
  rm -f "$tmp"
  printf '%s\n' "$out"
}

do_xwd() {
  local display
  display="$(require_display)"
  command -v xwd >/dev/null 2>&1 || die "xwd was not found"
  [ $# -ge 1 ] ||
    die "xwd needs arguments, e.g. -id 0x200036 -silent -out /tmp/opencode/dialog.xwd"
  DISPLAY="$display" xwd "$@"
}

do_xdotool() {
  local display
  display="$(require_display)"
  command -v xdotool >/dev/null 2>&1 || die "xdotool was not found"
  DISPLAY="$display" xdotool "$@"
}

do_windows() {
  local display
  display="$(require_display)"
  command -v xdotool >/dev/null 2>&1 || die "xdotool was not found"

  local id name
  while IFS= read -r id; do
    [ -n "$id" ] || continue
    name="$(DISPLAY="$display" xdotool getwindowname "$id" 2>/dev/null || true)"
    [ -n "$name" ] || continue
    printf '%s\t%s\n' "$id" "$name"
  done < <(DISPLAY="$display" xdotool search --name '.*' 2>/dev/null || true)
}

do_tree() {
  local display
  display="$(require_display)"
  command -v xwininfo >/dev/null 2>&1 || die "xwininfo was not found"

  local lines=40
  if [ $# -ge 1 ]; then
    case "$1" in
      ''|*[!0-9]*) die "tree expects a line count, not \"$1\"" ;;
      *) lines="$1" ;;
    esac
    shift
  fi
  [ $# -eq 0 ] || die "tree takes at most one line count"

  DISPLAY="$display" xwininfo -root -tree | sed -n "1,${lines}p"
}

do_xwininfo() {
  local display
  display="$(require_display)"
  command -v xwininfo >/dev/null 2>&1 || die "xwininfo was not found"
  if [ $# -eq 0 ]; then
    set -- -root -tree
  fi
  DISPLAY="$display" xwininfo "$@"
}

do_sleep() {
  [ $# -eq 1 ] || die "sleep needs one number of seconds"
  case "$1" in
    ''|*[!0-9.]*|*.*.*|.*|*.)
      die "sleep expects a number, not \"$1\""
      ;;
  esac
  sleep "$1"
}

do_log() {
  [ -f "$logfile" ] || die "no log at $logfile"

  local lines=200 follow=0
  while [ $# -gt 0 ]; do
    case "$1" in
      -f|--follow)
        follow=1
        shift
        ;;
      ''|*[!0-9]*)
        die "log expects a line count or -f, not \"$1\""
        ;;
      *)
        lines="$1"
        shift
        ;;
    esac
  done

  if [ "$follow" = 1 ]; then
    tail -n "$lines" -f "$logfile"
  else
    tail -n "$lines" "$logfile"
  fi
}

do_clean() {
  do_stop

  # Refuse obviously dangerous targets; this command runs on a path that may
  # come from an environment variable or a typo.
  case "$state" in
    /|/tmp|/home|"$HOME"|"$HOME"/*)
      die "refusing to remove state directory $state"
      ;;
  esac

  rm -rf "$state"
  printf 'removed %s\n' "$state"
}

case "$command_name" in
  start)
    do_start "$@"
    ;;
  stop)
    do_stop
    ;;
  restart)
    do_stop
    do_start "$@"
    ;;
  status)
    do_status
    ;;
  attach)
    do_attach "$@"
    ;;
  bt)
    do_bt
    ;;
  port)
    require_port
    ;;
  display)
    require_display
    ;;
  scheme)
    do_scheme "$@"
    ;;
  shot)
    do_shot "$@"
    ;;
  xwd)
    do_xwd "$@"
    ;;
  xdotool)
    do_xdotool "$@"
    ;;
  windows)
    do_windows
    ;;
  tree)
    do_tree "$@"
    ;;
  xwininfo)
    do_xwininfo "$@"
    ;;
  sleep)
    do_sleep "$@"
    ;;
  log)
    do_log "$@"
    ;;
  clean)
    do_clean
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    die "unknown command: $command_name (see --help)"
    ;;
esac
