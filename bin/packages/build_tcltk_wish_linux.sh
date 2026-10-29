#!/usr/bin/env bash
#
# Builds the antialiased Tcl/Tk "wish" shell used by the Pd2 instrument on
# Linux, and installs it next to the pure-data directory:
#
#     pure-data/bin/wish
#
# This is the Linux counterpart of build_tcltk_wish() in build.sh (macOS
# Wish.app) and build_tcltk_wish_mingw.sh (Windows wish86.exe).
#
# The sources are vendored in this directory:
#
#     tcl9.1.1-src.tar.gz       Tcl 9.1.1 snapshot
#     tk9.1.1-aa-src.tar.gz     Tk 9.1.1 from the radium/cairo-canvas-aa-9.1.1
#                               branch of https://github.com/kmatheussen/tk,
#                               which adds antialiased canvas lines (Cairo on
#                               X11, GDI+ on Windows)
#
# Tcl/Tk are built statically (--disable-shared), so the installed wish only
# depends on normal system libraries (Cairo, Xft, fontconfig, X11, ...). Tcl
# and Tk's script libraries are embedded in zipfs, so only the wish executable
# has to be installed. See libpd patch 20-linux-bundled-wish.patch, which makes
# sys_startgui() prefer <libdir>/bin/wish and leaves TCL_LIBRARY/TK_LIBRARY
# unset so zipfs is used.
#
# Building requires the Cairo/X11 development files. On Debian/Ubuntu:
#
#     sudo apt install build-essential pkg-config libcairo2-dev libxft-dev \
#         libfontconfig1-dev libx11-dev
#
# On Fedora/RHEL:
#
#     sudo dnf install gcc make pkgconf-pkg-config cairo-devel libXft-devel \
#         fontconfig-devel libX11-devel
#
# HarfBuzz is optional; without a recent enough version Tk disables
# bidirectional text support, which Pd's GUI does not need.
#
# Usage:
#   build_tcltk_wish_linux.sh [--dest <pure-data-dir>] [--jobs <n>]
#
# The default destination is $PD2_LIBPD_DIR/pure-data.

set -eEu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

TCLTK_VERSION=9.1.1
TCL_DIR="tcl$TCLTK_VERSION"
TK_DIR="tk$TCLTK_VERSION"
TCL_TARBALL="$SCRIPT_DIR/tcl$TCLTK_VERSION-src.tar.gz"
TK_TARBALL="$SCRIPT_DIR/tk$TCLTK_VERSION-aa-src.tar.gz"

DEST="${PD2_LIBPD_DIR:+$PD2_LIBPD_DIR/pure-data}"
JOBS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"

usage() {
    sed -n '2,/^set -eEu$/p' "$0" | sed 's/^# \{0,1\}//' | grep -v '^set -eEu$'
    exit 0
}

while [ $# -gt 0 ] ; do
    case "$1" in
        --dest)
            [ $# -ge 2 ] || { echo "--dest needs an argument" >&2 ; exit 1 ; }
            DEST="$2"
            shift 2
            ;;
        --jobs|-j)
            [ $# -ge 2 ] || { echo "--jobs needs an argument" >&2 ; exit 1 ; }
            JOBS="$2"
            shift 2
            ;;
        --help|-h)
            usage
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

DEST="$(cd "$DEST" 2>/dev/null && pwd)" || {
    echo "Destination does not exist: $DEST" >&2
    exit 1
}

if [ ! -f "$DEST/tcl/pd-gui.tcl" ] ; then
    echo "Can't find $DEST/tcl/pd-gui.tcl. Run build_libpd first (the libpd" >&2
    echo "source tree must be extracted before Tcl/Tk can be installed)." >&2
    exit 1
fi

for tarball in "$TCL_TARBALL" "$TK_TARBALL" ; do
    if [ ! -f "$tarball" ] ; then
        echo "Missing source archive: $tarball" >&2
        exit 1
    fi
done

for tool in make cc pkg-config ; do
    if ! command -v "$tool" >/dev/null 2>&1 ; then
        echo "Required tool not found: $tool" >&2
        exit 1
    fi
done

# Tk 9.1 draws canvas items through Cairo on X11. Check the development files
# up front so a missing -devel package gives a useful message instead of an
# obscure configure error.
MISSING_PKGS=""
for pkg in cairo xft fontconfig x11 ; do
    if ! pkg-config --exists "$pkg" ; then
        MISSING_PKGS="$MISSING_PKGS $pkg"
    fi
done
if [ -n "$MISSING_PKGS" ] ; then
    echo "Missing Tcl/Tk build dependencies:$MISSING_PKGS" >&2
    echo >&2
    echo "Install the Cairo/X11 development files, for example:" >&2
    echo "  Debian/Ubuntu: sudo apt install build-essential pkg-config libcairo2-dev \\" >&2
    echo "                     libxft-dev libfontconfig1-dev libx11-dev" >&2
    echo "  Fedora/RHEL:   sudo dnf install gcc make pkgconf-pkg-config cairo-devel \\" >&2
    echo "                     libXft-devel fontconfig-devel libX11-devel" >&2
    exit 1
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/radium-tcltk-build.XXXXXX")"
STAGE="$WORK/install"

echo "Building antialiased Tcl/Tk $TCLTK_VERSION"
echo "  work dir: $WORK"
echo "  install:  $DEST"

cleanup() {
    local status=$?
    if [ "$status" -eq 0 ] && [ -z "${PD2_KEEP_TCLTK_SOURCES:-}" ] ; then
        rm -rf "$WORK"
    else
        echo "Tcl/Tk build files kept in $WORK" >&2
    fi
}
trap cleanup EXIT

# Do not inherit Radium's package-build flags: Tcl/Tk have their own configure
# checks and the caller's LDFLAGS/CPPFLAGS may point at unrelated libraries.
unset CC CXX AR RANLIB STRIP NM OBJDUMP CFLAGS CXXFLAGS CPPFLAGS LDFLAGS

tar xzf "$TCL_TARBALL" -C "$WORK"
tar xzf "$TK_TARBALL" -C "$WORK"

if [ ! -d "$WORK/$TCL_DIR/unix" ] || [ ! -d "$WORK/$TK_DIR/unix" ] ; then
    echo "The Tcl/Tk source archives have an unexpected layout." >&2
    exit 1
fi

build_pkg() {
    local pkgdir="$1"
    shift

    echo "Configuring $pkgdir"
    (
        cd "$WORK/$pkgdir/unix"
        ./configure \
            --prefix="$STAGE" \
            --disable-shared \
            --disable-rpath \
            "$@"
        make -j"$JOBS"
        make install
    )
}

# Tk links against Tcl's build directory, so Tcl must be built first.
build_pkg "$TCL_DIR" --enable-threads
build_pkg "$TK_DIR" \
    --with-tcl="$WORK/$TCL_DIR/unix" \
    --enable-cairo

WISH="$STAGE/bin/wish${TCLTK_VERSION%.*}"
if [ ! -f "$WISH" ] ; then
    # Older/newer Tk releases may not put the version in the name.
    WISH="$STAGE/bin/wish"
fi
if [ ! -f "$WISH" ] ; then
    echo "The Tcl/Tk build did not produce a wish executable in $STAGE/bin" >&2
    exit 1
fi

if command -v file >/dev/null 2>&1 && ! file "$WISH" | grep -q ELF ; then
    echo "$WISH is not an ELF executable." >&2
    exit 1
fi

# Verify that this really is the antialiased Tk build. The runtime controls
# (::tk::x11::canvasAntialias etc.) are only present in the Radium fork.
if ! grep -aq "canvasAntialias" "$WISH" ; then
    echo "$WISH does not look like the antialiased Tk build." >&2
    exit 1
fi

# Do not strip the executable: Tk appends its zipfs archive (library scripts)
# to the ELF file, and strip truncates the file at the last ELF section,
# which removes the archive and makes the installed wish unable to find
# init.tcl/tk.tcl. Check that the ZIP end-of-central-directory record is
# still present, so a future change to the install step fails here instead of
# producing a wish that cannot start.
if ! tail -c 70000 "$WISH" | LC_ALL=C grep -aq $'PK\x05\x06' ; then
    echo "$WISH has no zipfs archive (was it stripped?)." >&2
    exit 1
fi

mkdir -p "$DEST/bin"
cp -a "$WISH" "$DEST/bin/wish"
chmod 755 "$DEST/bin/wish"

# Remove a versioned copy left over from an earlier layout, so nothing
# accidentally loads a different Tcl/Tk.
rm -f "$DEST/bin/wish${TCLTK_VERSION%.*}"

echo
echo "Finished building antialiased Tcl/Tk $TCLTK_VERSION"
echo "  $DEST/bin/wish"
