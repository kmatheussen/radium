#if defined(FOR_MACOSX)

#import <Cocoa/Cocoa.h>


void cocoa_set_best_resolution(void *view){
#if 0
  NSOpenGLView *nsview = view;
  [nsview setWantsBestResolutionOpenGLSurface:YES];
  printf("hepp\n");
  //getchar();
#endif
 [[[[NSApplication sharedApplication] mainWindow] contentView] setWantsBestResolutionOpenGLSurface:YES];
}

#endif
