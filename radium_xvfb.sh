#!/usr/bin/env bash
#
# Run Radium on a private X server for automated tests.
#
# Usage:
#   RADIUM_CONTROL_PORT_FILE=/tmp/opencode/radium-port.XXXXXX \
#       ./radium_xvfb.sh [radium arguments...]
#
# The helper starts a private Xvfb display via xvfb-run, creates a throwaway
# HOME seeded from bin/config, and removes both when Radium exits. Killing the
# helper stops Radium and its X server.
#
# This is a development/test helper. It deliberately lives in the repository
# root, outside bin/, so install.sh does not include it in binary distributions.
#
# Environment:
#   RADIUM_CONTROL_PORT=...         First control/HTTP port to try. Port 5080 is
#                                   rejected: it is reserved for the developer's
#                                   own Radium instance, which bin/scheme/repl.py
#                                   connects to by default. When unset, a random
#                                   port in 10000-60000 is chosen.
#   RADIUM_XVFB_BACKEND=opengl|null  GPU backend to configure (default: opengl)
#   RADIUM_MAX_FPS=...              Xvfb frame rate cap (default: 60, 0 = unlimited)
#   RADIUM_XVFB_FALLBACK=1          Run on the current $DISPLAY instead of Xvfb
#   RADIUM_XVFB_DISPLAY_FILE=...    Write the display name to this file
#   RADIUM_XVFB_KEEP_HOME=1         Keep the temporary HOME for inspection
#
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
radium="$repo/bin/radium"

backend="${RADIUM_XVFB_BACKEND:-opengl}"
fallback="${RADIUM_XVFB_FALLBACK:-0}"

case "$backend" in
  opengl|null) ;;
  *)
    echo "radium_xvfb: RADIUM_XVFB_BACKEND must be \"opengl\" or \"null\"" >&2
    exit 1
    ;;
esac

if [ "$fallback" != 0 ] && [ "$fallback" != 1 ]; then
  echo "radium_xvfb: RADIUM_XVFB_FALLBACK must be 0 or 1" >&2
  exit 1
fi

# Port 5080 is reserved for the developer's own Radium instance, which
# bin/scheme/repl.py connects to by default. Never let a test instance take it.
control_port="${RADIUM_CONTROL_PORT:-}"
if [ -n "$control_port" ]; then
  case "$control_port" in
    *[!0-9]*)
      echo "radium_xvfb: RADIUM_CONTROL_PORT must be a number between 1 and 65535" >&2
      exit 1
      ;;
  esac
  if [ "$control_port" -lt 1 ] || [ "$control_port" -gt 65535 ]; then
    echo "radium_xvfb: RADIUM_CONTROL_PORT must be a number between 1 and 65535" >&2
    exit 1
  fi
  if [ "$control_port" -eq 5080 ]; then
    echo "radium_xvfb: port 5080 is reserved for the developer's Radium instance (bin/scheme/repl.py connects to it by default). Set RADIUM_CONTROL_PORT to another port." >&2
    exit 1
  fi
else
  control_port=$(( (RANDOM % 50001) + 10000 ))
  export RADIUM_CONTROL_PORT="$control_port"
  printf 'radium_xvfb: RADIUM_CONTROL_PORT=%s\n' "$control_port" >&2
fi

if [ ! -x "$radium" ]; then
  echo "radium_xvfb: $radium is not executable" >&2
  exit 1
fi

if [ "$fallback" = 0 ] && ! command -v xvfb-run >/dev/null 2>&1; then
  echo "radium_xvfb: xvfb-run was not found. Set RADIUM_XVFB_FALLBACK=1 to use the current DISPLAY." >&2
  exit 1
fi

if [ "$fallback" = 1 ] && [ -z "${DISPLAY:-}" ]; then
  echo "radium_xvfb: RADIUM_XVFB_FALLBACK=1, but DISPLAY is not set" >&2
  exit 1
fi

testhome="$(mktemp -d "${TMPDIR:-/tmp}/radium-xvfb-home.XXXXXX")"
mkdir -p "$testhome/.radium"

if [ -f "$repo/bin/config" ]; then
  cp "$repo/bin/config" "$testhome/.radium/config"
else
  cat >"$testhome/.radium/config" <<'EOF'
settings_version = 0.73 # dont change this one
last_system_font_version = 1.9.21
last_editor_font_version = 3.0.b2.3
last_color_version = 8.2.52
EOF
fi

cat >>"$testhome/.radium/config" <<EOF
rhi_backend = $backend
last_successfully_started_rhi_backend = $backend
use_jack_if_jack_server_is_running = false
EOF

if [ "$fallback" = 0 ]; then
  # Software rendering (llvmpipe) makes multisampling very expensive, and the
  # tests do not need it.
  echo "multisample_qrhi = 1" >>"$testhome/.radium/config"
fi

if [ -n "${RADIUM_CONTROL_PORT_FILE:-}" ]; then
  portfile="$RADIUM_CONTROL_PORT_FILE"
  mkdir -p "$(dirname "$portfile")"
else
  portfile="$testhome/control-port"
fi
export RADIUM_CONTROL_PORT_FILE="$portfile"
rm -f "$portfile"

pidfile="$testhome/radium.pid"
displayfile="${RADIUM_XVFB_DISPLAY_FILE:-$testhome/display}"
mkdir -p "$(dirname "$displayfile")"
rm -f "$displayfile"

export HOME="$testhome"
export QT_QPA_PLATFORM=xcb
unset QT_QPA_PLATFORMTHEME QT_PLUGIN_PATH WAYLAND_DISPLAY

if [ "$fallback" = 0 ]; then
  export LIBGL_ALWAYS_SOFTWARE=1

  # Radium caps its render loop at RADIUM_MAX_FPS when this is set. Xvfb with
  # llvmpipe has no real vsync, so without a cap the render thread runs at full
  # speed and burns CPU.
  export RADIUM_XVFB=1
  if [ -z "${RADIUM_MAX_FPS:-}" ]; then
    export RADIUM_MAX_FPS=60
  fi
fi

printf 'radium_xvfb: HOME=%s\n' "$testhome" >&2
printf 'radium_xvfb: RADIUM_CONTROL_PORT_FILE=%s\n' "$portfile" >&2
printf 'radium_xvfb: DISPLAY_FILE=%s\n' "$displayfile" >&2
if [ "$fallback" = 1 ]; then
  printf '%s\n' "$DISPLAY" >"$displayfile"
  printf 'radium_xvfb: fallback to DISPLAY=%s\n' "$DISPLAY" >&2
fi

xvfb_pid=""
radium_pid=""

cleanup() {
  local status=$?
  trap - EXIT INT TERM

  if [ -z "$radium_pid" ] && [ -n "$xvfb_pid" ] && [ -s "$pidfile" ]; then
    radium_pid="$(cat "$pidfile")"
  fi

  if [ -n "$radium_pid" ]; then
    kill -TERM "$radium_pid" 2>/dev/null || true
    local i
    for i in {1..100}; do
      kill -0 "$radium_pid" 2>/dev/null || break
      sleep 0.05
    done
    if kill -0 "$radium_pid" 2>/dev/null; then
      kill -KILL "$radium_pid" 2>/dev/null || true
    fi
  elif [ -n "$xvfb_pid" ]; then
    # xvfb-run has not reported Radium's pid yet.
    pkill -TERM -P "$xvfb_pid" 2>/dev/null || true
    kill -TERM "$xvfb_pid" 2>/dev/null || true
  fi

  if [ -n "$xvfb_pid" ]; then
    wait "$xvfb_pid" 2>/dev/null || true
  elif [ -n "$radium_pid" ]; then
    wait "$radium_pid" 2>/dev/null || true
  fi

  if [ "${RADIUM_XVFB_KEEP_HOME:-0}" = 1 ]; then
    printf 'radium_xvfb: kept HOME=%s\n' "$testhome" >&2
  else
    rm -rf "$testhome"
  fi

  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

if [ "$fallback" = 1 ]; then
  "$radium" "$@" &
  radium_pid=$!
  printf '%s\n' "$radium_pid" >"$pidfile"
  if wait "$radium_pid"; then
    status=0
  else
    status=$?
  fi
  radium_pid=""
else
  export RADIUM_XVFB_PID_FILE="$pidfile"
  export RADIUM_XVFB_DISPLAY_FILE="$displayfile"
  xvfb-run -a -s "-screen 0 1920x1080x24" \
    bash -c 'echo $$ >"$RADIUM_XVFB_PID_FILE"; echo "$DISPLAY" >"$RADIUM_XVFB_DISPLAY_FILE"; exec "$@"' \
    _ "$radium" "$@" &
  xvfb_pid=$!

  # Wait briefly for the wrapper inside xvfb-run to report Radium's pid.
  for i in {1..100}; do
    if [ -s "$pidfile" ]; then
      radium_pid="$(cat "$pidfile")"
      break
    fi
    kill -0 "$xvfb_pid" 2>/dev/null || break
    sleep 0.05
  done

  if [ -s "$displayfile" ]; then
    printf 'radium_xvfb: DISPLAY=%s\n' "$(cat "$displayfile")" >&2
  fi

  if wait "$xvfb_pid"; then
    status=0
  else
    status=$?
  fi
  xvfb_pid=""
  radium_pid=""
fi

exit "$status"
