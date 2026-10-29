#!/usr/bin/env bash
#
# Installs a pre-built antialiased Tcl/Tk 9.1 wish (Cairo backend) into the
# Pd2 pure-data tree for Linux builds.
#
# A prebuilt binary is taken from ~/tlc_tk_antialiased (a Tk development fork
# that adds antialiased canvas lines on Linux/X11, unix/tkUnixCairo.c), or
# from PD2_TCLTK_AA_WISH. It is a static Tcl/Tk build whose script libraries
# are embedded in zipfs, so only the wish executable has to be installed. See
# libpd patch 20-linux-bundled-wish.patch, which makes sys_startgui() prefer
# <libdir>/bin/wish and unset TCL_LIBRARY/TK_LIBRARY so zipfs is used.
#
# A fresh clone has no prebuilt wish. In that case this script exits non-zero
# and build_tcltk_wish_linux.sh builds the wish from the vendored Tcl/Tk
# sources in this directory.
#
# Layout installed:
#
#     pure-data/bin/wish
#
# Usage:
#   install_tcltk_aa_linux.sh [--src <dir>] [--dest <pure-data-dir>]
#                             [--wish <file>]
#
# Returns 0 if the antialiased wish was installed (or an existing antialiased
# bundled wish was kept), non-zero otherwise. build.sh falls back to
# build_tcltk_wish_linux.sh when this script returns non-zero.

set -eEu

# Returns true for the Radium Tk fork. The ::tk::x11::canvasAntialias runtime
# control is only present in that build.
is_antialiased_wish() {
    [ -f "$1" ] && grep -aq "canvasAntialias" "$1"
}

SRC="${PD2_TCLTK_AA_DIR:-${RADIUM_TCLTK_AA_DIR:-$HOME/tlc_tk_antialiased}}"
WISH="${PD2_TCLTK_AA_WISH:-${RADIUM_TCLTK_AA_WISH:-}}"
DEST="${PD2_LIBPD_DIR:+$PD2_LIBPD_DIR/pure-data}"

while [ $# -gt 0 ] ; do
    case "$1" in
        --src)
            [ $# -ge 2 ] || { echo "--src needs an argument" >&2 ; exit 1 ; }
            SRC="$2"
            shift 2
            ;;
        --dest)
            [ $# -ge 2 ] || { echo "--dest needs an argument" >&2 ; exit 1 ; }
            DEST="$2"
            shift 2
            ;;
        --wish)
            [ $# -ge 2 ] || { echo "--wish needs an argument" >&2 ; exit 1 ; }
            WISH="$2"
            shift 2
            ;;
        --help|-h)
            sed -n '2,/^set -eEu$/p' "$0" | sed 's/^# \{0,1\}//' | grep -v '^set -eEu$'
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Use --help for usage." >&2
            exit 1
            ;;
    esac
done

if [ -z "$DEST" ] ; then
    echo "No destination. Set PD2_LIBPD_DIR or pass --dest <pure-data-dir>." >&2
    exit 1
fi

if [ ! -f "$DEST/tcl/pd-gui.tcl" ] ; then
    echo "Can't find $DEST/tcl/pd-gui.tcl. Run build_libpd first." >&2
    exit 1
fi

if [ -z "$WISH" ] ; then
    # Prefer the installed copy: it is stripped and is what "make install"
    # produced, while tk/unix/wish may be an older in-tree build.
    for candidate in "$SRC/install/bin/wish9.1" "$SRC/tk/unix/wish" ; do
        if [ -x "$candidate" ] ; then
            WISH="$candidate"
            break
        fi
    done
fi

if [ -z "$WISH" ] || [ ! -x "$WISH" ] ; then
    if is_antialiased_wish "$DEST/bin/wish" ; then
        echo "Antialiased Tcl/Tk wish not found under $SRC; keeping the existing $DEST/bin/wish." >&2
        exit 0
    fi
    echo "No prebuilt antialiased Tcl/Tk wish found under $SRC." >&2
    echo "Set PD2_TCLTK_AA_WISH to the wish executable, or build Tk in $SRC first." >&2
    echo "build_tcltk_wish_linux.sh will build it from the sources in this directory." >&2
    exit 1
fi

if ! file "$WISH" | grep -q ELF ; then
    echo "$WISH is not an ELF executable." >&2
    exit 1
fi

if ! is_antialiased_wish "$WISH" ; then
    echo "$WISH is not the antialiased Tk build (no canvasAntialias support)." >&2
    exit 1
fi

mkdir -p "$DEST/bin"

echo "Installing antialiased Tcl/Tk wish from $WISH"
cp -a "$WISH" "$DEST/bin/wish"
chmod 755 "$DEST/bin/wish"

# Remove a versioned copy left over from an earlier layout, so nothing
# accidentally loads a different Tcl/Tk.
rm -f "$DEST/bin/wish9.1"

echo "Installed:"
echo "  $DEST/bin/wish"
