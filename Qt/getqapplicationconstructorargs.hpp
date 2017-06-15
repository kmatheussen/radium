
#ifndef _RADIUM_QT_GETQAPPLICATIONCONSTRUCTORARGS_HPP
#define _RADIUM_QT_GETQAPPLICATIONCONSTRUCTORARGS_HPP

static inline char **getQApplicationConstructorArgs(int &argc, char **argv){
#if defined(FOR_WINDOWS) || defined(FOR_MACOSX)
  // Use freetype font
  argc = 4;
  char **ft_argv = (char**)calloc(sizeof(char*), argc);
  ft_argv[0] = strdup(argv[0]);
  ft_argv[1] = strdup("-platform");
#if defined(FOR_WINDOWS)
  ft_argv[2] = strdup("windows:fontengine=freetype");
#else
  ft_argv[2] = strdup("cocoa:fontengine=freetype");
#endif
  return ft_argv;
#else
  return argv;
#endif
}

#endif
