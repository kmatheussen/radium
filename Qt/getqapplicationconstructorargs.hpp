#pragma once


static inline char **getQApplicationConstructorArgs(int &argc, char **argv){
  
#if defined(FOR_WINDOWS) // || defined(FOR_MACOSX)
  
  // Add command line arguments to use the freetype font engine (much better font rendering)
  argc += 2;

  char **ft_argv = (char**)calloc(sizeof(char*), argc);
  
  for(int i=0;i<argc-2;i++)
    ft_argv[i] = strdup(argv[i]);

  ft_argv[argc-2] = strdup("-platform");
  
#if defined(FOR_WINDOWS)
  ft_argv[argc-1] = strdup("windows:fontengine=freetype");
#else
  ft_argv[argc-1] = strdup("cocoa:fontengine=freetype");
#endif
  
  return ft_argv;

#else

  return argv; // linux already uses freetype

#endif
}
