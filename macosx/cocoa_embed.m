
#if 0

NSView* superView = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 100, 100)];
NSView* subView   = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 100, 100)];
[superView addSubview:subView]; /* Here */

[superView setAutoresizesSubviews:YES];
[subView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];

NSLog(@"subview's frame before resizing: %@", NSStringFromRect([subView frame]));
[superView setFrame:NSMakeRect(0, 0, 200, 100)];
NSLog(@"subview's frame after  resizing: %@", NSStringFromRect([subView frame]));

#endif

#if 0

#import <Cocoa/Cocoa.h>

#include <gdk/gdkquartz.h>

#include "cocoa_embed_proc.h"

void cocoa_set_nsview_size(GdkWindow *view,int width, int height){
  NSView* nsview = gdk_quartz_window_get_nsview(view);

  //[nsview setFrameSize:NSMakeSize(width, height)];
  [nsview setFrame:NSMakeRect(0, 0, width, height)];
}

void cocoa_embed(void *super_view, GdkWindow *sub_view){
  NSView* superView = super_view;
  NSView* subView = gdk_quartz_window_get_nsview(sub_view);

  [superView addSubview:subView];

  [superView setAutoresizesSubviews:YES];
  [subView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];

/*
  NSLog(@"subview's frame before resizing: %@", NSStringFromRect([subView frame]));
  [superView setFrame:NSMakeRect(0, 0, 200, 100)];
  NSLog(@"subview's frame after  resizing: %@", NSStringFromRect([subView frame]));
*/
}


#endif

