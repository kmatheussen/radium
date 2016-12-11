Radium - The Music Editor
=========================

## Introduction

Radium is a free (as in speech) music editor with a novel interface.
It's inspired by trackers, but has fewer limitations and uses graphics to show musical data. 

## Installation

### Overview

1. Get the Source. For stable releases (advised) go [here](https://github.com/kmatheussen/radium/releases). Otherwise see [Get The Source From Git](#get-the-source-from-git).
2. Install dependencies. See [Build Dependencies](#build-dependencies).
3. Do this:
```bash
export RADIUM_QT_VERSION=5
make packages
BUILDTYPE=RELEASE ./build_linux.sh -j7
QT_QPA_PLATFORM_PLUGIN_PATH=`$(./find_moc_and_uic_paths.sh qmake) -query QT_INSTALL_PLUGINS` bin/radium
```

### Get The Source From Git

WARNING: Only release-tagged commits should be considered stable.  If
you only "git clone" and compile that version of Radium, there's a
good chance the program won't compile, or will crash.

To clone the git repository:
```bash
git clone git@github.com:kmatheussen/radium.git
```

Note that after a `git pull` you may want to:
```bash
make very_clean
```
since not all dependencies are tracked in the makefile.

### Debug Build (For Developing Radium)

To debug build (lots of assertions and very slow, don't build in debug mode if you are just testing the program), do:
```bash
export RADIUM_QT_VERSION=5
export QT_QPA_PLATFORM_PLUGIN_PATH=`$(./find_moc_and_uic_paths.sh qmake) -query QT_INSTALL_PLUGINS`
./make_and_run_linux.sh
```

### Build Dependencies

#### All Platforms

VST headers from steinberg. The necessary files should be incuded in
"VST Audio Plug-Ins SDK", which you can download here:

  http://www.steinberg.net/en/company/developers.html

After downloading the VST SDK, unpack it into your ~/SDKs folder.

#### Fedora

In addition to OpenGL, you also need:

```
      python2-devel
      alsa-lib-devel
      jack-audio-connection-kit-devel
      libsamplerate-devel
      liblrdf-devel
      libsndfile-devel
      ladspa-devel
      glib2-devel
      ladspa-calf-plugins
      binutils-devel
      libtool-ltdl
      libtool
      tk
      libogg-devel
      libvorbis-devel
      speex-devel
      fftw-devel
      guile
      libxkbfile-devel
      xorg-x11-util-macros
      cmake
      libXrandr-devel
      llvm-devel
      boost-devel
      openssl-devel
      ncurses-devel
      xcb-util-keysyms-devel
      qt5-qtbase-gui
      qt5-qttools-common
      qt5-qtwebkit-devel
      qt5-qtx11extras-devel
      qt5-qttools-sttic
```

Or in one line:

```bash
qt4-devel python2-devel alsa-lib-devel jack-audio-connection-kit-devel libsamplerate-devel liblrdf-devel libsndfile-devel ladspa-devel glib2-devel ladspa-calf-plugins binutils-devel libtool-ltdl libtool tk libogg-devel libvorbis-devel speex-devel fftw-devel guile libxkbfile-devel xorg-x11-util-macros cmake libXrandr-devel qtwebkit-devel llvm-devel boost-devel openssl-devel ncurses-devel xcb-util-keysyms-devel qt5-qtbase-gui qt5-qtwebkit-devel qt5-qtx11extras-devel qt5-qttools-static
```
(tested on Fedora 17/19/20/22 64 bit)

#### Ubuntu/Debian/etc.

##### Main packages, Ubuntu 12

In addition to OpenGL, you also need:

```
      python2-dev
      libasound2-dev
      libjack-jackd2-dev or libjack-jackd1-dev
      libsamplerate-dev
      liblrdf-dev
      libsndfile-dev
      ladspa-sdk
      glib2-dev
      calf-plugins
      binutils-dev
      libc6-dev
      tk8.5
      libogg-dev
      libvorbis-dev
      libspeex-dev
      fftw-dev
      fftw3-dev
      guile
      libxkbfile-dev
      xorg-x11-util-macros
      cmake
      libfreetype6-dev
      libxinerama-dev
      libxcursor-dev
      libxrandr-dev
      llvm-dev
      libboost-all-dev
      libssl-dev
      ncurses-dev
      libxcb-keysyms1-dev
      qt5-dev
      qt5webkit-dev
      qt5x11extras-dev
      qt5-qttools-static-dev
```

Or in one line:

```bash
sudo apt-get install python2-dev libasound2-dev libjack-jackd2-dev libsamplerate-dev liblrdf-dev libsndfile-dev ladspa-sdk glib2-dev calf-plugins binutils-dev libc6-dev tk8.5 libogg-dev libvorbis-dev libspeex-dev fftw-dev fftw3-dev guile libxkbfile-dev xorg-x11-util-macros cmake libfreetype6-dev libxinerama-dev libxcursor-dev libxrandr-dev libqtwebkit-dev llvm-dev libboost-all-dev libssl-dev ncurses-dev libxcb-keysyms1-dev qt5-dev qt5webkit-dev qt5x11extras-dev qt5-qttools-static-dev
```

##### Ubuntu LTS:

I needed glib-2.0-dev instead of glib2-dev, plus a package called libglib2.0-dev. I also needed to install python2.7-dev instead of python2-dev.

##### On debian stable, it has been reported that you also need libfftw3-dev.

##### Ubuntu 14: Replace glib-2.0-dev with libglib2.0-dev. Add libiberty-dev


## To Enable All Keyboard Shortcuts On Mac OS X

Then go to

     System Preferences -> Accessibility

or

     System Preferences -> Universal Access

... and make sure the button "Enable access for assistive devices" is ON.    
This button is placed at the bottom of the Accessibility page.
(I don't know where the button is placed in the "Universal Access" page)

This is necessary for Radium to be allowed to (temporarily) turn off OSX system shortcuts.

## Acknowledgment

See http://users.notam02.no/~kjetism/radium/development.php

## Contact

k.s.matheussen@notam02.no
http://www.notam02.no/radium/
