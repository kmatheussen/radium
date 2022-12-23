/*

    IMPORTANT! This file is auto-generated each time you save your
    project - if you alter its contents, your changes may be overwritten!

    There's a section below where you can add your own custom code safely, and the
    Introjucer will preserve the contents of that block, but the best way to change
    any of these definitions is by using the Introjucer's project settings.

    Any commented-out settings will assume their default values.

*/

#ifndef __JUCE_APPCONFIG_LQNELU__
#define __JUCE_APPCONFIG_LQNELU__

//==============================================================================
// [BEGIN_USER_CODE_SECTION]

// (You can add your own code in this section, and the Introjucer will not overwrite it)

// [END_USER_CODE_SECTION]

//==============================================================================
#define JUCE_MODULE_AVAILABLE_juce_audio_basics          1
#define JUCE_MODULE_AVAILABLE_juce_audio_devices         1
#define JUCE_MODULE_AVAILABLE_juce_audio_formats         1
#define JUCE_MODULE_AVAILABLE_juce_audio_processors      1
#define JUCE_MODULE_AVAILABLE_juce_core                  1
#define JUCE_MODULE_AVAILABLE_juce_data_structures       1
#define JUCE_MODULE_AVAILABLE_juce_events                1
#define JUCE_MODULE_AVAILABLE_juce_graphics              1
#define JUCE_MODULE_AVAILABLE_juce_gui_basics            1
#define JUCE_MODULE_AVAILABLE_juce_gui_extra             1

//==============================================================================
#ifndef    JUCE_STANDALONE_APPLICATION
 #define   JUCE_STANDALONE_APPLICATION 1
#endif

#define JUCE_GLOBAL_MODULE_SETTINGS_INCLUDED 1


#define JUCE_MODAL_LOOPS_PERMITTED 1

//==============================================================================
// juce_audio_devices flags:


// disabling these two. Don't feel too good about it, but it is quite intrusive.
#define JUCE_REPORT_APP_USAGE 0
#define JUCE_DISPLAY_SPLASH_SCREEN 0

#ifndef    JUCE_ASIO
 #define JUCE_ASIO 1
#endif

#ifndef    JUCE_WASAPI
 #define JUCE_WASAPI 1
#endif

#ifndef    JUCE_WASAPI_EXCLUSIVE
 #define JUCE_WASAPI_EXCLUSIVE 1
#endif

#ifndef    JUCE_DIRECTSOUND
 #define JUCE_DIRECTSOUND 1
#endif

#ifndef    JUCE_ALSA
  #define JUCE_ALSA 1 // for MIDI
#endif

#ifndef    JUCE_JACK
 #define JUCE_JACK 0
#endif

#ifndef    JUCE_USE_ANDROID_OPENSLES
 //#define JUCE_USE_ANDROID_OPENSLES
#endif

#ifndef    JUCE_USE_WINRT_MIDI
 #define JUCE_USE_WINRT_MIDI 1
#endif

#ifndef    JUCE_USE_CDREADER
 #define JUCE_USE_CDREADER 0
#endif

#ifndef    JUCE_USE_CDBURNER
 #define JUCE_USE_CDBURNER 0
#endif

//==============================================================================
// juce_audio_formats flags:

#ifndef    JUCE_USE_FLAC
 #define JUCE_USE_FLAC 0
#endif

#ifndef    JUCE_USE_OGGVORBIS
 #define JUCE_USE_OGGVORBIS 0
#endif

#ifndef    JUCE_USE_MP3AUDIOFORMAT
 #define JUCE_USE_MP3AUDIOFORMAT 0
#endif

#ifndef    JUCE_USE_LAME_AUDIO_FORMAT
 #define JUCE_USE_LAME_AUDIO_FORMAT 0
#endif

#ifndef    JUCE_USE_WINDOWS_MEDIA_FORMAT
 #define JUCE_USE_WINDOWS_MEDIA_FORMAT 0
#endif

//==============================================================================
// juce_audio_processors flags:

#ifndef    JUCE_PLUGINHOST_VST
 #define   JUCE_PLUGINHOST_VST 1
#endif

#ifndef    JUCE_PLUGINHOST_VST3
 #define JUCE_PLUGINHOST_VST3 1
#endif

#ifndef    JUCE_PLUGINHOST_AU
 #define JUCE_PLUGINHOST_AU 1
#endif

#ifndef    JUCE_PLUGINHOST_LADSPA
 #define   JUCE_PLUGINHOST_LADSPA 0
#endif

#ifndef JUCE_PLUGINHOST_LV2
 #define JUCE_PLUGINHOST_LV2 1
#endif

//==============================================================================
// juce_core flags:

#ifndef    JUCE_FORCE_DEBUG
 //#define JUCE_FORCE_DEBUG
#endif

#ifndef    JUCE_LOG_ASSERTIONS
 //#define JUCE_LOG_ASSERTIONS
#endif

#ifndef    JUCE_CHECK_MEMORY_LEAKS
 //#define JUCE_CHECK_MEMORY_LEAKS
#endif

#ifndef    JUCE_DONT_AUTOLINK_TO_WIN32_LIBRARIES
 //#define JUCE_DONT_AUTOLINK_TO_WIN32_LIBRARIES
#endif

#ifndef    JUCE_INCLUDE_ZLIB_CODE
 //#define JUCE_INCLUDE_ZLIB_CODE
#endif

#ifndef    JUCE_USE_CURL
 #define JUCE_USE_CURL 0
#endif

//==============================================================================
// juce_graphics flags:

#ifndef    JUCE_USE_COREIMAGE_LOADER
 //#define JUCE_USE_COREIMAGE_LOADER
#endif

#ifndef    JUCE_USE_DIRECTWRITE
 //#define JUCE_USE_DIRECTWRITE
#endif

//==============================================================================
// juce_gui_basics flags:

#ifndef    JUCE_ENABLE_REPAINT_DEBUGGING
 //#define JUCE_ENABLE_REPAINT_DEBUGGING
#endif

#ifndef    JUCE_USE_XSHM
 //#define JUCE_USE_XSHM
#endif

#ifndef    JUCE_USE_XRENDER
 //#define JUCE_USE_XRENDER
#endif

#ifndef    JUCE_USE_XCURSOR
 //#define JUCE_USE_XCURSOR
#endif

//==============================================================================
// juce_gui_extra flags:

#ifndef    JUCE_WEB_BROWSER
 #define JUCE_WEB_BROWSER 0
#endif

#ifndef    JUCE_ENABLE_LIVE_CONSTANT_EDITOR
 #define JUCE_ENABLE_LIVE_CONSTANT_EDITOR 0
#endif

//==============================================================================
// juce_video flags:

#ifndef    JUCE_USE_CAMERA
 #define   JUCE_USE_CAMERA 0
#endif

#endif  // __JUCE_APPCONFIG_LQNELU__
