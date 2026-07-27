#pragma once

#define VERSION_LETTER ""
#define DISKVERSION 1.24
#define BLOCKDISKVERSION 0.94
#define TRACKDISKVERSION 0.94
#define SETTINGSVERSION 0.73

/* Define this when profiling with sas/c. */
// #define NOPLAYER


#if defined(QFONT_H)
#  define DEFAULT_SYSTEM_FONT_FAMILY "Lato"
#  define DEFAULT_SYSTEM_FONT_SIZE 11
#  define DEFAULT_SYSTEM_FONT_WEIGHT QFont::Black

#  define DEFAULT_EDITOR_FONT_FAMILY "Cousine"
#  define DEFAULT_EDITOR_FONT_SIZE 12
#  define DEFAULT_EDITOR_FONT_WEIGHT QFont::Normal
#endif

