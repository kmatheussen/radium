/* Copyright 2012-2026 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#ifdef FOR_MACOSX

#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>
#import <objc/runtime.h>

#undef EVENT_H

#include "../common/nsmtracker.h"


#include "cocoa_Helpers_proc.h"



void OS_OSX_show_icon_in_dock(void){

  //  ProcessSerialNumber psn;
  //  if (GetCurrentProcess(&psn) == noErr)
    {
      ProcessSerialNumber psn = { 0, kCurrentProcess };
      TransformProcessType(&psn, 
                           kProcessTransformToForegroundApplication);
    }
  
  // didn't work.
  //[NSApp setPresentationOptions: [NSApp presentationOptions] | NSApplicationPresentationHideMenuBar];
}

/*
void OS_OSX_get_os_version(void){
  //[[NSProcessInfo processInfo] operatingSystemVersion];
  [[NSProcessInfo processInfo] operatingSystem];
}
*/

void OS_OSX_set_cursorpos(int x, int y){
  CGPoint pos;
  pos.x = x;
  pos.y = y;
  CGWarpMouseCursorPosition(pos);
}


#if 0
bool OS_SYSTEM_window_is_actually_visible(void *void_nsview){
  return false;
  /*
  NSView *view = (NSView*)void_nsview;
  return [[view window] isVisible]; // Must use occlusion function which is only available from 10.9.
  */
}
#endif

bool OS_OSX_is_key_window(void *void_nsview){
  NSView *view = (NSView*)void_nsview;
  return [[view window] isKeyWindow];
}

static bool s_very_fullscreen = false;
static NSRect s_org_frame;
static NSWindowStyleMask s_org_style_mask;

// Reroute the native macOS fullscreen operation (green button, and the
// "Enter Full Screen" menu command if there is one) to Radium's own
// home-made fullscreen (same as F11) instead of the native fullscreen
// that moves the window to another Space.
//
// Qt does not provide any event for the green button, so we have to
// intercept the AppKit-level toggleFullScreen: message sent to the main
// window's NSWindow. We do that by swizzling the toggleFullScreen: method
// of the window's class (QNSWindow) with an implementation that calls
// OS_OSX_toggle_main_window_fullscreen() when the receiver is the main
// window, and delegates to the original implementation for all other
// windows.
//
// Note: swapping the window instance's class with object_setClass was
// tried first, but it crashes in AppKit's _NSTrackingAreaAKManager
// (CFRelease of NULL) while updating the title bar button tracking areas.
// Class-level method swizzling avoids touching any instance's class.
//
// Only the one window instance passed to
// OS_OSX_install_fullscreen_button_redirect is rerouted. No other window
// in the application (mixer strips, plugin GUIs, file dialogs, etc.)
// changes behavior. performZoom: is not overridden, so double-clicking
// the title bar keeps its normal zoom behavior.

static IMP s_org_toggleFullScreen_imp = NULL;
static NSWindow *s_fullscreen_redirect_window = nil;

static bool OS_OSX_window_is_native_fullscreen(NSWindow *window)
{
	return ([window styleMask] & NSWindowStyleMaskFullScreen) != 0;
}

static void radium_fullscreen_redirect_toggleFullScreen(id self, SEL _cmd, id sender)
{
	if (self == s_fullscreen_redirect_window && !OS_OSX_window_is_native_fullscreen(self))
		OS_OSX_toggle_main_window_fullscreen();
	else
		((void(*)(id, SEL, id))s_org_toggleFullScreen_imp)(self, _cmd, sender);
}

void OS_OSX_install_fullscreen_button_redirect(void *void_nsview)
{
	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
		return;

	s_fullscreen_redirect_window = window;

	if (s_org_toggleFullScreen_imp != NULL)
		return; // Already swizzled.

	Class window_class = object_getClass(window);
	if (window_class == NULL)
		return;

	Method method = class_getInstanceMethod(window_class, @selector(toggleFullScreen:));
	if (method != NULL)
	{
		s_org_toggleFullScreen_imp = method_getImplementation(method);
		method_setImplementation(method, (IMP)radium_fullscreen_redirect_toggleFullScreen);
	}
	else
	{
		s_org_toggleFullScreen_imp = class_getMethodImplementation([NSWindow class], @selector(toggleFullScreen:));
		class_addMethod(window_class, @selector(toggleFullScreen:), (IMP)radium_fullscreen_redirect_toggleFullScreen, "v@:@");
	}
}

void OS_OSX_show_very_fullscreen(void *void_nsview)
{
	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
		return;

	OS_OSX_install_fullscreen_button_redirect(void_nsview); // Idempotent. Ensures the redirect is installed even if Qt has recreated the native window.

	if (OS_OSX_window_is_native_fullscreen(window))
	{
		// Defensive: the style mask can't be changed in native macOS
		// fullscreen. Callers only reach here when OS_OSX_window_is_fullscreen
		// returned false, so this should not happen, but never call
		// setStyleMask: in that state.
		[window toggleFullScreen:nil];
		return;
	}

	if (!s_very_fullscreen)
	{
		s_org_frame = [window frame];
		s_org_style_mask = [window styleMask];
	}

	[window setStyleMask:NSWindowStyleMaskBorderless];

	NSRect screenFrame;
	if ([window screen] != nil)
		screenFrame = [window screen].frame;
	else
		screenFrame = [NSScreen mainScreen].frame;

	[window setFrame:screenFrame display:YES];

	//[window setLevel:CGShieldingWindowLevel()+1];
	// Level 100 = just below NSPopUpMenuWindowLevel (101), so popup menus
	// (right-click, menu bar, submenus, comboboxes) render above the
	// fullscreen window, while it still covers dock (20), menu bar (24),
	// tool (3), and normal (0) windows.
	//[window setLevel:NSPopUpMenuWindowLevel - 1];
	//[window setLevel:1];

	[NSApp setPresentationOptions: NSApplicationPresentationHideDock | NSApplicationPresentationHideMenuBar];

	[window makeKeyAndOrderFront:nil];

	s_very_fullscreen = true;
}

void OS_OSX_unshow_very_fullscreen(void *void_nsview)
{
	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
		return;

	OS_OSX_install_fullscreen_button_redirect(void_nsview); // Idempotent. Ensures the redirect is installed even if Qt has recreated the native window.

	[NSApp setPresentationOptions: NSApplicationPresentationDefault];

	[window setLevel:NSNormalWindowLevel];

	if (OS_OSX_window_is_native_fullscreen(window))
	{
		// Window is in native macOS fullscreen (green button). The style
		// mask can't be changed in this state, and there is no saved Radium
		// frame/style mask to restore, so just exit native fullscreen.
		[window toggleFullScreen:nil];
		s_very_fullscreen = false;
		return;
	}

	if (s_very_fullscreen)
	{
		[window setFrame:s_org_frame display:YES];
		[window setStyleMask:s_org_style_mask];
	}

	[window makeKeyAndOrderFront:nil];

	s_very_fullscreen = false;
}

bool OS_OSX_window_is_fullscreen(void *void_nsview)
{
	if (s_very_fullscreen)
		return true;

	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
		return false;

	return OS_OSX_window_is_native_fullscreen(window);
}

static bool has_notch(NSScreen* screen)
{
	if (screen == NULL)
	{
		R_ASSERT_NON_RELEASE(false);
		return false;
	}
	
	if (@available(macOS 12.0, *))
	{
		NSEdgeInsets insets = [screen safeAreaInsets];
		return insets.top > 0;
	}
	
	return false;
}

bool OS_OSX_has_notch(void *void_nsview)
{
	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
	{
		R_ASSERT_NON_RELEASE(false);
		return false;
	}
	
    // Return the NSScreen the window is currently on
    NSScreen *screen = [window screen];
	
	return has_notch(screen);
}

bool OS_OSX_get_notch_rect(void *void_nsview, int &x1, int &y1, int &x2, int &y2)
{
	NSView *view = (NSView*)void_nsview;
	NSWindow *window = [view window];
	if (window == nil)
	{
		R_ASSERT_NON_RELEASE(false);
		return false;
	}
	
	// Return the NSScreen the window is currently on
	NSScreen *screen = [window screen];
	
	if (!has_notch(screen))
	{
		return false;
	}

	if (@available(macOS 12.0, *))
	{
		NSRect leftArea = [screen auxiliaryTopLeftArea];
		NSRect rightArea = [screen auxiliaryTopRightArea];
		
		// No additional unobscured areas => no notch (e.g. displays with
		// rounded corners but no notch report non-zero safe area insets
		// while the auxiliary areas are empty).
		if (NSEqualRects(leftArea, NSZeroRect) || NSEqualRects(rightArea, NSZeroRect))
		{
			return false;
		}
		
		NSEdgeInsets insets = [screen safeAreaInsets];
		
		// Calculate the gap between the left and right active areas
		double leftMaxX = leftArea.origin.x + leftArea.size.width;
		double rightMinX = rightArea.origin.x;
		
		// A gap of zero or less means there is no notch.
		if (leftMaxX >= rightMinX)
		{
			return false;
		}
		
		x1 = leftMaxX;
		x2 = rightMinX;
		
		// y=0 is at the top of the screen. The notch spans the top
		// safe-area inset band.
		y1 = 0;
		y2 = insets.top;
		
		return true;
	}

	return false;
}

#endif // FOR_MACOSX




