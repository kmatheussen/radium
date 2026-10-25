#!/usr/bin/env bash
#
# Build a third-party Pd external library for Radium's Pd2 instrument, and
# install the result into bin/pd/externals.
#
# The Pd2 instrument uses a libpd built with MULTI=true (-DPDINSTANCE
# -DPDTHREADS), so externals compiled for stock single-instance Pd (including
# all deken binaries) do not load: they reference the s_float/s_symbol/...
# globals, which are per-instance macros in this libpd and are not exported.
# This script rebuilds an external from source with the required defines,
# against the exact libpd headers bundled with Radium.
#
# Usage:
#   build_pd_external.sh [options] <source-dir> [extra build args...]
#
# Options:
#   --compat        Compatibility mode for old sources: force-include
#                   pd_externals_compat.h (which provides the error() function
#                   that was removed in Pd 0.52), and fix the invalid
#                   "PERTHREAD static" declaration order (see --fix-perthread).
#   --fix-perthread Fix the "PERTHREAD static" declaration order to
#                   "static PERTHREAD", without including the compat header.
#   --install       Run the project's own install target (pd-lib-builder:
#                   "make install objectsdir=..."; cmake: "cmake --install")
#                   instead of copying binaries and help patches by pattern.
#                   Requires --lib-name. This preserves abstractions, data
#                   files and aliases the way upstream installs them.
#   --lib-name <n>  Install into <dest>/<n>/ instead of <dest>/ directly.
#                   Also used as the library name for --install.
#   --dest <dir>    Base install directory (default: <repo>/bin/pd/externals).
#   --dry-run       Print the build/install commands instead of running them.
#   -h, --help      Show this help.
#
# Environment:
#   PD2_CPPFLAGS        Extra preprocessor flags.
#   PD2_CFLAGS          Extra C compiler flags.
#   PD2_JOBS            Number of parallel build jobs (default: number of CPUs).
#   PD2_EXTERNALS_DIR   Same as --dest.
#   PD2_LIBPD_SRC       Path to the libpd pure-data/src headers to compile
#                       against (default: libpd next to this script).
#   PD2_TARGET_OS       Target OS for cross builds (Windows/Darwin/Linux).
#                       Defaults to the host OS.
#   PD2_TARGET_TRIPLET  Target triplet passed to autotools' --host.
#   PD2_CMAKE_ARGS      Extra arguments for the CMake configure step.
#   PD2_PDLIBBUILDER    Fallback pd-lib-builder directory, copied into source
#                       trees whose GitHub archive omits the submodule.
#   DARWIN_ARCH_FLAGS   Architecture flags for macOS externals (default:
#                       universal "-arch arm64 -arch x86_64"; build.sh exports
#                       the same flags it uses for the other Mac packages).
#
# Extra build args after <source-dir> are forwarded verbatim to make, configure
# or cmake, for projects that need e.g. --with-pd=... or a specific target.
#
# The script detects pd-lib-builder, autotools, cmake, and plain/old-template
# Makefiles. See AGENTS.md ("Building third-party Pd externals") for the list
# of known source-fix patterns for old libraries.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LIBPD_SRC="${PD2_LIBPD_SRC:-$SCRIPT_DIR/libpd/pure-data/src}"
COMPAT_HEADER="$SCRIPT_DIR/pd_externals_compat.h"

# Target OS for cross builds. Defaults to the host, so native builds behave
# exactly as before. 'PD2_TARGET_OS=Windows' or 'Darwin' selects the matching
# output handling below.
TARGET_OS="${PD2_TARGET_OS:-$(uname -s)}"

DEST="${PD2_EXTERNALS_DIR:-$REPO_ROOT/bin/pd/externals}"
PDLIBBUILDER_FALLBACK="${PD2_PDLIBBUILDER:-$SCRIPT_DIR/pd_externals/pd-lib-builder}"
USE_COMPAT=false
USE_PER_THREAD_FIX=false
USE_INSTALL=false
LIB_NAME=""
DRY_RUN=false
SRC=""
EXTRA_ARGS=()

usage() {
    sed -n '3,/^set -euo pipefail/p' "$0" \
        | sed -e '/^set -euo pipefail/d' -e 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --compat)
            USE_COMPAT=true
            USE_PER_THREAD_FIX=true
            shift
            ;;
        --fix-perthread)
            USE_PER_THREAD_FIX=true
            shift
            ;;
        --install)
            USE_INSTALL=true
            shift
            ;;
        --lib-name)
            if [ $# -lt 2 ]; then
                echo "Option --lib-name requires an argument." >&2
                exit 1
            fi
            LIB_NAME="$2"
            shift 2
            ;;
        --dest)
            if [ $# -lt 2 ]; then
                echo "Option --dest requires an argument." >&2
                exit 1
            fi
            DEST="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
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
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

if [ $# -eq 0 ]; then
    echo "No source directory given." >&2
    echo >&2
    usage >&2
    exit 1
fi

SRC="$1"
shift
EXTRA_ARGS=("$@")

if [ ! -d "$SRC" ]; then
    echo "Source directory not found: $SRC" >&2
    exit 1
fi
SRC="$(cd "$SRC" && pwd)"

if [ ! -f "$LIBPD_SRC/m_pd.h" ]; then
    echo "libpd headers not found in $LIBPD_SRC" >&2
    echo "Build libpd first, for example:" >&2
    echo "    cd \"$SCRIPT_DIR\" && ./build.sh" >&2
    exit 1
fi

if $USE_COMPAT && [ ! -f "$COMPAT_HEADER" ]; then
    echo "Compatibility header not found: $COMPAT_HEADER" >&2
    exit 1
fi

if $USE_INSTALL && [ -z "$LIB_NAME" ]; then
    echo "Option --install requires --lib-name." >&2
    exit 1
fi

JOBS="${PD2_JOBS:-}"
if [ -z "$JOBS" ]; then
    JOBS="$( (nproc 2>/dev/null) || (sysctl -n hw.ncpu 2>/dev/null) || echo 4 )"
fi

run() {
    if $DRY_RUN; then
        printf '+'
        printf ' %q' "$@"
        printf '\n'
    else
        "$@"
    fi
}

run_in_dir() {
    local dir="$1"
    shift
    if $DRY_RUN; then
        printf '+ cd %q &&' "$dir"
        printf ' %q' "$@"
        printf '\n'
    else
        ( cd "$dir" && "$@" )
    fi
}

# The defines must match the MULTI build of libpd: pd_this is a TLS symbol
# only when both PDINSTANCE and PDTHREADS are defined, and dropping them makes
# the external reference s_float/s_symbol/... globals that libpd doesn't have.
DEFINES="-DPD -DPDINSTANCE -DPDTHREADS"

# On macOS the externals are built universal (arm64 + x86_64), like libpd and
# the other Mac packages. DARWIN_ARCH_FLAGS is set by build.sh; the default
# covers standalone invocations of this script.
DARWIN_MIN_FLAG=""
DARWIN_ARCH_FLAGS=""
if [ "$(uname -s)" = Darwin ]; then
    if [ -n "${MACOSX_DEPLOYMENT_TARGET:-}" ]; then
        DARWIN_MIN_FLAG="-mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET"
        DEFINES="$DEFINES $DARWIN_MIN_FLAG"
    fi
    DARWIN_ARCH_FLAGS="${DARWIN_ARCH_FLAGS:--arch arm64 -arch x86_64}"
fi

COMPAT_FLAG=""
if $USE_COMPAT; then
    COMPAT_FLAG="-include $COMPAT_HEADER"
fi

# Bind references to symbols defined in the external library to the library's
# own definition. Without this, the dynamic linker's ELF symbol interposition
# makes an external write to (or call) an earlier-loaded library's symbol with
# the same name, typically libpd's: ggee's "slider_widgetbehavior" shadows the
# widget behavior of Pd's built-in [hsl]/[vsl] classes, so slider_setup()
# overwrites the built-in behavior with ggee's fatom_* functions and mapping an
# [hsl] object crashes inside ggee's create_widget(). Radium loads all externals
# into one process with dlopen(RTLD_GLOBAL), so this affects every library.
#
# -Wl,-Bsymbolic is ELF-only. Mach-O has no equivalent flag; pd-lib-builder
# builds Darwin bundles with -flat_namespace, where the same interposition can
# happen, so on macOS colliding symbols must instead be made file-local in the
# library source (static). Windows PE resolves references within a DLL locally
# by default.
SYMBOLIC_LDFLAG=""
case "$TARGET_OS" in
    Linux) SYMBOLIC_LDFLAG="-Wl,-Bsymbolic" ;;
    *) ;;
esac

if $USE_PER_THREAD_FIX; then
    # "PERTHREAD static x" does not compile when PERTHREAD is __thread
    # ("__thread static x"); the storage class must come first.
    while IFS= read -r -d '' file; do
        if grep -q 'PERTHREAD[[:space:]][[:space:]]*static' "$file"; then
            echo "Fixing PERTHREAD static in $file"
            run sed -i.bak 's/PERTHREAD[[:space:]][[:space:]]*static/static PERTHREAD/g' "$file"
            run rm -f "$file.bak"
        fi
    done < <(find "$SRC" \( -name '*.c' -o -name '*.h' \) -print0 2>/dev/null)
fi

CPPFLAGS_ALL="$DEFINES"
if [ -n "$COMPAT_FLAG" ]; then
    CPPFLAGS_ALL="$CPPFLAGS_ALL $COMPAT_FLAG"
fi
if [ -n "${PD2_CPPFLAGS:-}" ]; then
    CPPFLAGS_ALL="$CPPFLAGS_ALL $PD2_CPPFLAGS"
fi

detect_build_system() {
    if [ -f "$SRC/Makefile.pdlibbuilder" ] \
        || [ -f "$SRC/pd-lib-builder/Makefile.pdlibbuilder" ] \
        || grep -qis 'pdlibbuilder' "$SRC/Makefile" 2>/dev/null; then
        echo "pdlibbuilder"
    elif [ -f "$SRC/CMakeLists.txt" ]; then
        echo "cmake"
    elif [ -f "$SRC/configure" ] || [ -f "$SRC/configure.ac" ] || [ -f "$SRC/autogen.sh" ]; then
        echo "autotools"
    elif [ -f "$SRC/Makefile" ] || [ -f "$SRC/makefile" ] || [ -f "$SRC/GNUmakefile" ]; then
        echo "make"
    else
        echo "unknown"
    fi
}

BUILD_SYSTEM="$(detect_build_system)"
echo "Source:       $SRC"
echo "Build system: $BUILD_SYSTEM"
echo "libpd:        $LIBPD_SRC"
echo "Install to:   $DEST"

# GitHub release archives omit git submodule contents, so projects that keep
# pd-lib-builder as a submodule can be missing it. Copy the vendored fallback
# into the source tree in that case.
if [ "$BUILD_SYSTEM" = pdlibbuilder ] \
    && [ ! -f "$SRC/Makefile.pdlibbuilder" ] \
    && [ ! -f "$SRC/pd-lib-builder/Makefile.pdlibbuilder" ]; then
    if [ -f "$PDLIBBUILDER_FALLBACK/Makefile.pdlibbuilder" ]; then
        echo "Adding pd-lib-builder from $PDLIBBUILDER_FALLBACK"
        run mkdir -p "$SRC/pd-lib-builder"
        run cp -p "$PDLIBBUILDER_FALLBACK/Makefile.pdlibbuilder" \
            "$SRC/pd-lib-builder/"
    else
        echo "pd-lib-builder missing from $SRC and no fallback found at $PDLIBBUILDER_FALLBACK" >&2
        exit 1
    fi
fi

pdlibbuilder_make() {
    # pd-lib-builder builds for the native architecture only, and on Darwin
    # appends a hardcoded -mmacosx-version-min=10.6 (its internal version.flag)
    # after the flags passed through CPPFLAGS. The last deployment target wins,
    # and compiling m_pd.h with PDINSTANCE and PDTHREADS needs thread-local
    # storage (__thread pd_this), which is unsupported below macOS 10.7, so
    # every compile fails with "thread-local storage is not supported for the
    # current target". Overriding its architecture variables for the compile
    # and link steps makes the externals universal (like libpd and the other
    # Mac packages) with Radium's deployment target. Overriding its cflags
    # instead would clobber the project's own cflags (e.g. -DVERSION=...).
    local DARWIN_FLAGS=""
    if [ -n "$DARWIN_ARCH_FLAGS" ]; then
        DARWIN_FLAGS="$DARWIN_ARCH_FLAGS"
        if [ -n "$DARWIN_MIN_FLAG" ]; then
            DARWIN_FLAGS="$DARWIN_FLAGS $DARWIN_MIN_FLAG"
        fi
    fi

    # Pass the defines through CPPFLAGS rather than cflags, so that a
    # project's own cflags (e.g. -DVERSION=...) are not clobbered.
    local MAKE_EXTRA=()
    if [ -n "${PD2_CFLAGS:-}" ]; then
        # pd-lib-builder treats CFLAGS as a complete override of its default
        # warning, optimisation and architecture flags; add the architecture
        # flags back so libraries with extra compiler flags are universal too.
        MAKE_EXTRA=("CFLAGS=$PD2_CFLAGS $DARWIN_FLAGS")
    fi
    if [ -n "$SYMBOLIC_LDFLAG" ]; then
        # pd-lib-builder appends LDFLAGS to the class and shared library link
        # commands, and arch.ld.flags (which LDFLAGS defaults to) is empty on
        # ELF platforms.
        MAKE_EXTRA+=("LDFLAGS=$SYMBOLIC_LDFLAG")
    fi
    if [ -n "$DARWIN_FLAGS" ]; then
        MAKE_EXTRA+=("arch.c.flags=$DARWIN_FLAGS")
        MAKE_EXTRA+=("arch.ld.flags=$DARWIN_FLAGS")
    fi
    run env CPPFLAGS="$CPPFLAGS_ALL" \
        make -C "$SRC" -j"$JOBS" PDINCLUDEDIR="$LIBPD_SRC" \
        ${MAKE_EXTRA[@]+"${MAKE_EXTRA[@]}"} \
        ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"} "$@"
}

case "$BUILD_SYSTEM" in
    pdlibbuilder)
        pdlibbuilder_make
        ;;

    make)
        # The classic (pd-extended) Makefile template and most hand-written
        # Makefiles take CFLAGS and PD_INCLUDE/PD_PATH from the command line.
        MAKE_EXTRA=()
        if [ -n "$SYMBOLIC_LDFLAG" ]; then
            MAKE_EXTRA=("LDFLAGS=$SYMBOLIC_LDFLAG")
        fi
        run env CPPFLAGS="${PD2_CPPFLAGS:-}" \
            make -C "$SRC" -j"$JOBS" \
            PD_INCLUDE="$LIBPD_SRC" \
            PD_PATH="$(dirname "$LIBPD_SRC")" \
            CFLAGS="-Wall -W -g $DEFINES $COMPAT_FLAG ${PD2_CFLAGS:-}" \
            ${MAKE_EXTRA[@]+"${MAKE_EXTRA[@]}"} \
            ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}
        ;;

    autotools)
        if [ ! -f "$SRC/configure" ]; then
            if [ -f "$SRC/autogen.sh" ]; then
                run_in_dir "$SRC" ./autogen.sh
            else
                run_in_dir "$SRC" autoreconf -fi
            fi
        fi
        if [ -f "$SRC/configure" ] && [ ! -x "$SRC/configure" ]; then
            chmod +x "$SRC/configure" 2>/dev/null || true
        fi
        CONFIGURE_ENV=(env "CPPFLAGS=$CPPFLAGS_ALL -I$LIBPD_SRC")
        if [ -n "${PD2_CFLAGS:-}" ]; then
            CONFIGURE_ENV+=("CFLAGS=$PD2_CFLAGS")
        fi
        if [ -n "$SYMBOLIC_LDFLAG" ]; then
            CONFIGURE_ENV+=("LDFLAGS=${LDFLAGS:-} $SYMBOLIC_LDFLAG")
        fi
        CONFIGURE_HOST=()
        if [ "$TARGET_OS" = Windows ] && [ -n "${PD2_TARGET_TRIPLET:-}" ]; then
            CONFIGURE_HOST=("--host=$PD2_TARGET_TRIPLET")
        fi
        run_in_dir "$SRC" "${CONFIGURE_ENV[@]}" \
            ./configure ${CONFIGURE_HOST[@]+"${CONFIGURE_HOST[@]}"} \
            ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}
        run make -C "$SRC" -j"$JOBS"
        ;;

    cmake)
        BUILD_DIR="$SRC/.pd2-build"
        if ! $DRY_RUN; then
            rm -rf "$BUILD_DIR"
        fi
        CMAKE_LD_ARGS=()
        if [ -n "$SYMBOLIC_LDFLAG" ]; then
            CMAKE_LD_ARGS=("-DCMAKE_SHARED_LINKER_FLAGS=$SYMBOLIC_LDFLAG"
                           "-DCMAKE_MODULE_LINKER_FLAGS=$SYMBOLIC_LDFLAG")
        fi
        CMAKE_CROSS_ARGS=()
        if [ "$TARGET_OS" = Windows ]; then
            CMAKE_CROSS_ARGS+=("-DCMAKE_SYSTEM_NAME=Windows")
            if [ -n "${CC:-}" ]; then
                CMAKE_CROSS_ARGS+=("-DCMAKE_C_COMPILER=$CC")
            fi
            if [ -n "${CXX:-}" ]; then
                CMAKE_CROSS_ARGS+=("-DCMAKE_CXX_COMPILER=$CXX")
            fi
        fi
        # Word splitting of PD2_CMAKE_ARGS is intentional.
        run cmake -S "$SRC" -B "$BUILD_DIR" \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_C_FLAGS="$CPPFLAGS_ALL" \
            -DCMAKE_CXX_FLAGS="$CPPFLAGS_ALL" \
            -DPD_INCLUDE_DIR="$LIBPD_SRC" \
            -DPD_INCLUDE_DIRS="$LIBPD_SRC" \
            -DPD_SOURCE_DIR="$(dirname "$LIBPD_SRC")" \
            -DPD_PATH="$(dirname "$LIBPD_SRC")" \
            ${CMAKE_CROSS_ARGS[@]+"${CMAKE_CROSS_ARGS[@]}"} \
            ${PD2_CMAKE_ARGS:-} \
            ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"} \
            ${CMAKE_LD_ARGS[@]+"${CMAKE_LD_ARGS[@]}"}
        run cmake --build "$BUILD_DIR" -j"$JOBS"
        ;;

    *)
        echo "Unable to detect a supported build system in $SRC" >&2
        echo "(expected pd-lib-builder, CMakeLists.txt, configure/configure.ac, or a Makefile)" >&2
        exit 1
        ;;
esac

INSTALL_DIR="$DEST"
if [ -n "$LIB_NAME" ]; then
    INSTALL_DIR="$DEST/$LIB_NAME"
fi

if ! $DRY_RUN; then
    mkdir -p "$INSTALL_DIR"
fi

# Use the project's own install target when requested. This preserves
# abstractions, data files and aliases the way upstream installs them.
INSTALLED=false
if $USE_INSTALL; then
    case "$BUILD_SYSTEM" in
        pdlibbuilder)
            # pd-lib-builder installs into $(objectsdir)/$(lib.name).
            pdlibbuilder_make install objectsdir="$DEST"
            INSTALLED=true
            ;;
        cmake)
            run cmake --install "$SRC/.pd2-build" --prefix "$INSTALL_DIR"
            INSTALLED=true
            ;;
        *)
            echo "Warning: --install is not supported for the $BUILD_SYSTEM build system; copying files by pattern." >&2
            ;;
    esac
fi

if ! $DRY_RUN && $INSTALLED && [ ! -d "$INSTALL_DIR" ]; then
    echo "The install target did not create $INSTALL_DIR" >&2
    exit 1
fi

# Filename extensions produced by pd-lib-builder (".pd_linux"/".pd_darwin"/
# ".dll") and by deken-style builds (".linux-amd64-32.so", ...).
case "$TARGET_OS" in
    Darwin)
        CLASS_GLOBS=( '*.pd_darwin' '*.darwin-amd64-*.so' '*.darwin-arm64-*.so' )
        SHARED_GLOBS=( 'lib*.pd_darwin.dylib' )
        ;;
    Windows)
        CLASS_GLOBS=( '*.dll' )
        SHARED_GLOBS=( 'lib*.dll' )
        ;;
    *)
        CLASS_GLOBS=( '*.pd_linux' '*.linux-amd64-*.so' '*.linux-arm64-*.so' '*.linux-i386-*.so' '*.linux-arm-*.so' )
        SHARED_GLOBS=( 'lib*.pd_linux.so' )
        ;;
esac

copy_found() {
    local pattern="$1"
    local maxdepth="$2"
    while IFS= read -r -d '' file; do
        run cp -p "$file" "$INSTALL_DIR/"
    done < <(find "$SRC" -maxdepth "$maxdepth" -name "$pattern" -print0 2>/dev/null)
}

if ! $INSTALLED; then
    # Class externals and shared support libraries (libraries built from
    # shared.sources, such as iemnet, reference lib<name>.pd_linux.so /
    # lib<name>.pd_darwin.dylib through $ORIGIN / @loader_path, so both must
    # end up in the same directory).
    for pattern in "${CLASS_GLOBS[@]}"; do
        copy_found "$pattern" 3
    done
    for pattern in "${SHARED_GLOBS[@]}"; do
        copy_found "$pattern" 3
    done
fi

# Help patches and top-level abstractions/meta files. Also done after an
# install target, which is harmless and covers libraries whose datafiles
# list is incomplete.
copy_found '*-help.pd' 3
for file in "$SRC"/*.pd; do
    [ -e "$file" ] || continue
    run cp -p "$file" "$INSTALL_DIR/"
done

# Verify that the shared objects really were linked with -Wl,-Bsymbolic (see
# SYMBOLIC_LDFLAG above). A build system that silently drops the flag would
# reintroduce the symbol interposition the flag protects against, so treat it
# as a hard error instead of trusting make/configure/cmake to pass it on.
if [ -n "$SYMBOLIC_LDFLAG" ] && ! $DRY_RUN; then
    if ! command -v readelf >/dev/null 2>&1; then
        echo "Warning: readelf not found; cannot verify $SYMBOLIC_LDFLAG." >&2
    else
        num_binaries=0
        not_symbolic=()
        # Only the binaries at the top of the install directory are built by
        # this script. Subdirectories may contain prebuilt data such as ELSE's
        # bundled TkDND Tcl extensions, which are not loaded by libpd.
        while IFS= read -r -d '' file; do
            num_binaries=$((num_binaries + 1))
            if ! readelf -dW "$file" 2>/dev/null \
                | grep -qE '\(FLAGS\).*SYMBOLIC|\(SYMBOLIC\)'; then
                not_symbolic+=("$file")
            fi
        done < <(find "$INSTALL_DIR" -maxdepth 1 -type f \
            \( -name '*.pd_linux' -o -name '*.pd_linux.so' \
               -o -name '*.linux-*.so' -o -name 'lib*.so' \) -print0)
        if [ ${#not_symbolic[@]} -gt 0 ]; then
            echo "ERROR: these shared objects were not linked with $SYMBOLIC_LDFLAG:" >&2
            printf '  %s\n' "${not_symbolic[@]}" >&2
            echo "External libraries must bind their own symbols locally: with" >&2
            echo "dlopen(RTLD_GLOBAL) they otherwise overwrite same-named libpd" >&2
            echo "symbols (e.g. ggee's slider_widgetbehavior)." >&2
            exit 1
        fi
        if [ "$num_binaries" -gt 0 ]; then
            echo "Verified $SYMBOLIC_LDFLAG on $num_binaries shared object(s)."
        fi
    fi
fi

if $DRY_RUN; then
    echo "Dry run only; nothing was built or installed."
else
    echo
    if $INSTALLED; then
        echo "Done. Installed with the project's install target into $INSTALL_DIR."
    else
        echo "Done. Installed into $INSTALL_DIR."
    fi
    echo "The Pd2 instrument searches $DEST at runtime."
fi
