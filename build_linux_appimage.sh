#!/usr/bin/env bash

# Creates a self-contained Radium AppImage for Linux using linuxdeploy:
#   https://github.com/linuxdeploy/linuxdeploy
#
# Usage:
#   ./build_linux_appimage.sh
#
# Environment variables:
#   RADIUM_APPIMAGE_TOOLS_DIR    Directory to store/download the linuxdeploy
#                                tools in. (default: $HOME/.cache/radium_appimage_tools)
#   RADIUM_APPIMAGE_SKIP_BUILD   Set to 1 to skip building radium.
#                                (default: 0, i.e. radium is built)
#
# Requirements: Radium must be buildable on this machine (see build_linux.sh),
# and curl must be installed. Note that the AppImage will require glibc
# version 2.34 or newer.

set -eEu

THIS_DIR="$(dirname "$(readlink -f "$0")")"

TOOLS_DIR="${RADIUM_APPIMAGE_TOOLS_DIR:-$HOME/.cache/radium_appimage_tools}"

LINUXDEPLOY_APPIMAGE="$TOOLS_DIR/linuxdeploy-x86_64.AppImage"
APPIMAGETOOL_APPIMAGE="$TOOLS_DIR/appimagetool-x86_64.AppImage"

LINUXDEPLOY_URL="https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage"
APPIMAGETOOL_URL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage"

BUILD_DIR="/tmp/radium_appimage_build"
APPDIR="$BUILD_DIR/AppDir"
INSTALL_PREFIX="$BUILD_DIR/install_prefix"

cd "$THIS_DIR"

# 1. Version

VERSION=$(sed -n 's/^RADIUM_VERSION[[:space:]]*=[[:space:]]*//p' "$THIS_DIR/Makefile" | head -1)
if [ -z "$VERSION" ]; then
    echo "Error: Unable to find RADIUM_VERSION in Makefile." >&2
    exit -1
fi

OUTPUT="$THIS_DIR/Radium-$VERSION-x86_64.AppImage"

# 2. Build radium

if [ "${RADIUM_APPIMAGE_SKIP_BUILD:-0}" != 1 ]; then
    printf "Building Radium (%s)...\n" "$VERSION"
    BUILDTYPE=RELEASE "$THIS_DIR/build_linux.sh" -j"$(nproc)"
fi

if [ ! -x "$THIS_DIR/bin/radium" ] ; then
    echo "Error: bin/radium does not exist. Build radium first." >&2
    exit -1
fi

# 3. Download tools

mkdir -p "$TOOLS_DIR"

download_if_missing() {
    local file="$1"
    local url="$2"
    if [ ! -e "$file" ]; then
        echo "Downloading $url"
        curl -fL -o "$file" "$url"
        chmod +x "$file"
    fi
}

download_if_missing "$LINUXDEPLOY_APPIMAGE" "$LINUXDEPLOY_URL"
download_if_missing "$APPIMAGETOOL_APPIMAGE" "$APPIMAGETOOL_URL"

if [ ! -e /dev/fuse ]; then
    export APPIMAGE_EXTRACT_AND_RUN=1
fi

# 4. Create AppDir

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# install.sh stages a normal radium installation.
"$THIS_DIR/install.sh" "$INSTALL_PREFIX"

rm -rf "$APPDIR"
mkdir -p "$APPDIR/usr"

# Radium expects all helper programs and resource files to be located in the
# same directory as the radium_linux.bin binary, so the whole radium
# installation is placed into usr/bin/.
cp -a "$INSTALL_PREFIX/radium" "$APPDIR/usr/bin"

# Remove stale windows binaries.
find "$APPDIR/usr/bin" -maxdepth 1 -name '*.exe' -delete

# The ladspa directory is a symlink to a directory outside the radium tree,
# so it would be broken inside the AppImage. Replace it with the actual
# plugins. Radium refuses to start without these plugins. (It can't find
# the "AM pitchshift" plugin otherwise.)
LADSPA_DIR="${RADIUM_LADSPA_PLUGINS_DIR:-$(dirname "$THIS_DIR")/common-ladspa-plugins/build/linux64/dlls}"
if [ ! -d "$LADSPA_DIR" ]; then
    echo "Error: LADSPA plugins directory $LADSPA_DIR not found." >&2
    echo "       Set RADIUM_LADSPA_PLUGINS_DIR to point to the directory where the LADSPA plugins have been built." >&2
    exit -1
fi
rm -f "$APPDIR/usr/bin/ladspa"
mkdir -p "$APPDIR/usr/bin/ladspa"
cp -a "$LADSPA_DIR/." "$APPDIR/usr/bin/ladspa/"

if ! file "$APPDIR/usr/bin/radium_linux.bin" | grep -q ELF ; then
    echo "Error: usr/bin/radium_linux.bin is not an ELF binary." >&2
    exit -1
fi

# qt.conf makes Qt load its plugins from usr/plugins.
cp "$THIS_DIR/AppImage/qt.conf" "$APPDIR/usr/bin/qt.conf"

# Qt is needed to bundle the Qt plugins.
if [ -z "${QMAKE:-}" ]; then
    if ! command -v qmake >/dev/null 2>&1 ; then
        echo "Error: qmake not found in PATH. Make sure Qt6 is installed." >&2
        exit -1
    fi
    QMAKE="$(command -v qmake)"
fi
export QMAKE

QT_PLUGINS_DIR="$("$QMAKE" -query QT_INSTALL_PLUGINS)"
QT_LIBS_DIR="$("$QMAKE" -query QT_INSTALL_LIBS)"

# Copy the Qt plugins radium needs. This is done manually instead of using
# linuxdeploy-plugin-qt since the patchelf step of that plugin corrupts the
# Qt plugins. (The DT_INIT dynamic tag is not updated when patchelf moves the
# .init section, causing crashes when the plugin is loaded.)
# The plugins don't need patched rpaths since AppRun puts usr/lib into
# LD_LIBRARY_PATH.
for plugin_type in \
    iconengines \
    imageformats \
    networkinformation \
    platforminputcontexts \
    platforms \
    tls \
    xcbglintegrations ; do
    mkdir -p "$APPDIR/usr/plugins/$plugin_type"
    cp -a "$QT_PLUGINS_DIR/$plugin_type/." "$APPDIR/usr/plugins/$plugin_type/"
done

# libqxcb.so depends on libQt6XcbQpa, which is not linked into radium itself,
# so linuxdeploy won't deploy it. Copy it manually.
mkdir -p "$APPDIR/usr/lib"
cp -a "$QT_LIBS_DIR/libQt6XcbQpa.so"* "$APPDIR/usr/lib/"

mkdir -p "$APPDIR/usr/share/applications" "$APPDIR/usr/share/icons/hicolor/256x256/apps"
cp "$THIS_DIR/AppImage/radium.desktop" "$APPDIR/"
cp "$THIS_DIR/AppImage/radium.desktop" "$APPDIR/usr/share/applications/"
cp "$THIS_DIR/icons/radium_256x256x32.png" "$APPDIR/usr/share/icons/hicolor/256x256/apps/radium.png"
cp "$THIS_DIR/icons/radium_256x256x32.png" "$APPDIR/radium.png"

cp "$THIS_DIR/AppImage/AppRun" "$APPDIR/AppRun"
chmod +x "$APPDIR/AppRun"

# 5. Deploy with linuxdeploy

# Help linuxdeploy find libraries that are bundled with radium itself.
export LD_LIBRARY_PATH="$APPDIR/usr/bin/packages/python27_install/lib:$APPDIR/usr/bin/packages/faust/build/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

EXES=""
for f in \
    usr/bin/radium_linux.bin \
    usr/bin/radium_crashreporter \
    usr/bin/radium_error_message \
    usr/bin/radium_progress_window \
    usr/bin/radium_check_jack_status \
    usr/bin/radium_check_recent_libxcb \
    usr/bin/radium_plugin_scanner ; do
    if [ -x "$APPDIR/$f" ] && file "$APPDIR/$f" | grep -q ELF ; then
        EXES="$EXES -e $APPDIR/$f"
    else
        echo "Warning: Skipping $f. File not found or not an ELF binary." >&2
    fi
done

if [ -z "$EXES" ]; then
    echo "Error: No ELF executables found in the AppDir." >&2
    exit -1
fi

"$LINUXDEPLOY_APPIMAGE" --appdir "$APPDIR" \
    $EXES \
    -d "$THIS_DIR/AppImage/radium.desktop" \
    -i "$THIS_DIR/icons/radium_256x256x32.png"

# Remove libraries that must not be bundled into the AppImage.
# (see AppImage/excludelist)
while read -r lib ; do
    case "$lib" in
        ''|'#'*) continue ;;
    esac
    find "$APPDIR/usr/lib" -maxdepth 1 -name "$lib" -delete
done < "$THIS_DIR/AppImage/excludelist"

# 6. Create AppImage

"$APPIMAGETOOL_APPIMAGE" "$APPDIR" "$OUTPUT"

echo
echo "AppImage created: $OUTPUT"
echo
echo "Test it by running:"
echo "  $OUTPUT"
