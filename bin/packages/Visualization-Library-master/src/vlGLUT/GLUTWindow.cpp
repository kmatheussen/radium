/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include <vlGLUT/GLUTWindow.hpp>
#include <vlGraphics/Applet.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Time.hpp>

using namespace vlGLUT;

//-----------------------------------------------------------------------------
std::map< int, GLUTWindow* > GLUTWindow::mWinMap;
//-----------------------------------------------------------------------------
GLUTWindow::GLUTWindow()
{
  mInited = false;
  mHandle = 0;
}
//-----------------------------------------------------------------------------
GLUTWindow::GLUTWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x, int y, int width, int height)
{
  mInited = false;
  mHandle = 0;

  initGLUTWindow(title, info, x, y, width, height);
}
//-----------------------------------------------------------------------------
bool GLUTWindow::initGLUTWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x, int y, int width, int height)
{
  setOpenGLContextInfo(info);

  int flags = GLUT_RGB;
  if (info.rgbaBits().a()) flags |= GLUT_ALPHA;
  if (info.accumRGBABits().r()|info.accumRGBABits().g()|info.accumRGBABits().b()|info.accumRGBABits().a()) flags |= GLUT_ACCUM;
  if (info.doubleBuffer()) flags |= GLUT_DOUBLE;
  if (info.depthBufferBits()) flags |= GLUT_DEPTH;
  if (info.stencilBufferBits()) flags |= GLUT_STENCIL;
  if (info.multisample()) flags |= GLUT_MULTISAMPLE;
  if (info.stereo()) flags |= GLUT_STEREO;

  #if defined(WIN32)
    if (info.fullscreen())
    {
      DEVMODE devmode;
      EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,&devmode);
      width  = devmode.dmPelsWidth;
      height = devmode.dmPelsHeight;
    }
  #endif
  mFullscreen = info.fullscreen();
  glutInitDisplayMode( flags );
  glutInitWindowSize( width, height );
  glutInitWindowPosition( x, y );

  mHandle = glutCreateWindow( title.toStdString().c_str() ) ;
  if (info.fullscreen())
    glutFullScreen();

  glutSetWindow( handle() );

  initGLContext();
  // dispatchInitEvent();

  setVSyncEnabled(info.vSync());

  glutSetIconTitle(title.toStdString().c_str());
  mWinMap[handle()] = this;

  glutSetKeyRepeat(GLUT_KEY_REPEAT_OFF);
  initKeymap();

  // install callbacks
  glutDisplayFunc( glut_display_func );
  glutReshapeFunc( glut_reshape_func );
  glutKeyboardFunc( glut_keyboard_func );
  glutKeyboardUpFunc( glut_keyboard_up_func );
  glutMouseFunc( glut_mouse_func );
  glutMotionFunc( glut_motion_func );
  glutPassiveMotionFunc( glut_passive_motion_func );
  glutVisibilityFunc( glut_visibility_func );
  glutSpecialFunc( glut_special_func );
  glutSpecialUpFunc( glut_special_up_func );
#if !defined(__APPLE__)
  glutMouseWheelFunc( glut_mouse_wheel_func );
#endif

  glutReshapeWindow(width, height);

  // glutEntryFunc( glut_entry_func );
  glutWMCloseFunc( glut_wmclose_func );
#if !defined(__APPLE__)
  glutCloseFunc( glut_close_func );
#endif

  // used for continuous update
  glutIdleFunc(  glut_idle_func  );

  // GLUT does not set the appropriate current window when calling these two, so we cannot use them.
  // glutTimerFunc( millisecs, glut_timer_func, value );

  return true;

  /*** GLUT functions that would be nice to support ***/

  ///*
  // * Initialization functions, see fglut_init.c
  // */
  //glutInit( int* pargc, char** argv );
  //glutInitWindowPosition( int x, int y );
  //glutInitWindowSize( int width, int height );
  //glutInitDisplayMode( unsigned int displayMode );
  //glutInitDisplayString( const char* displayMode );

  ///*
  // * Process loop function, see freeglut_main.c
  // */
  //glutMainLoop( void );

  ///*
  // * Window management functions, see freeglut_window.c
  // */
  //glutCreateWindow( const char* title );
  //glutCreateSubWindow( int window, int x, int y, int width, int height );
  //glutDestroyWindow( int window );
  //glutSetWindow( int window );
  //glutGetWindow( void );
  //glutSetWindowTitle( const char* title );
  //glutSetIconTitle( const char* title );
  //glutReshapeWindow( int width, int height );
  //glutPositionWindow( int x, int y );
  //glutShowWindow( void );
  //glutHideWindow( void );
  //glutIconifyWindow( void );
  //glutPushWindow( void );
  //glutPopWindow( void );
  //glutFullScreen( void );

  ///*
  // * Display-connected functions, see freeglut_display.c
  // */
  //glutPostWindowRedisplay( int window );
  //glutPostRedisplay( void );
  //glutSwapBuffers( void );

  ///*
  // * Mouse cursor functions, see freeglut_cursor.c
  // */
  //glutWarpPointer( int x, int y );
  //glutSetCursor( int cursor );

  ///*
  // * Overlay stuff, see freeglut_overlay.c
  // */
  //glutEstablishOverlay( void );
  //glutRemoveOverlay( void );
  //glutUseLayer( GLenum layer );
  //glutPostOverlayRedisplay( void );
  //glutPostWindowOverlayRedisplay( int window );
  //glutShowOverlay( void );
  //glutHideOverlay( void );

  ///*
  // * Menu stuff, see freeglut_menu.c
  // */
  //glutCreateMenu( void (* callback)( int menu ) );
  //glutDestroyMenu( int menu );
  //glutGetMenu( void );
  //glutSetMenu( int menu );
  //glutAddMenuEntry( const char* label, int value );
  //glutAddSubMenu( const char* label, int subMenu );
  //glutChangeToMenuEntry( int item, const char* label, int value );
  //glutChangeToSubMenu( int item, const char* label, int value );
  //glutRemoveMenuItem( int item );
  //glutAttachMenu( int button );
  //glutDetachMenu( int button );

  ///*
  // * Global callback functions, see freeglut_callbacks.c
  // */
  //glutTimerFunc( unsigned int time, void (* callback)( int ), int value );
  //glutIdleFunc( void (* callback)( void ) );

  ///*
  // * Window-specific callback functions, see freeglut_callbacks.c
  // */
  //glutKeyboardFunc( void (* callback)( unsigned char, int, int ) );
  //glutSpecialFunc( void (* callback)( int, int, int ) );
  //glutReshapeFunc( void (* callback)( int, int ) );
  //glutVisibilityFunc( void (* callback)( int ) );
  //glutDisplayFunc( void (* callback)( void ) );
  //glutMouseFunc( void (* callback)( int, int, int, int ) );
  //glutMotionFunc( void (* callback)( int, int ) );
  //glutPassiveMotionFunc( void (* callback)( int, int ) );
  //glutEntryFunc( void (* callback)( int ) );

  //glutKeyboardUpFunc( void (* callback)( unsigned char, int, int ) );
  //glutSpecialUpFunc( void (* callback)( int, int, int ) );
  //glutJoystickFunc( void (* callback)( unsigned int, int, int, int ), int pollInterval );
  //glutMenuStateFunc( void (* callback)( int ) );
  //glutMenuStatusFunc( void (* callback)( int, int, int ) );
  //glutOverlayDisplayFunc( void (* callback)( void ) );
  //glutWindowStatusFunc( void (* callback)( int ) );

  //glutSpaceballMotionFunc( void (* callback)( int, int, int ) );
  //glutSpaceballRotateFunc( void (* callback)( int, int, int ) );
  //glutSpaceballButtonFunc( void (* callback)( int, int ) );
  //glutButtonBoxFunc( void (* callback)( int, int ) );
  //glutDialsFunc( void (* callback)( int, int ) );
  //glutTabletMotionFunc( void (* callback)( int, int ) );
  //glutTabletButtonFunc( void (* callback)( int, int, int, int ) );

  ///*
  // * State setting and retrieval functions, see freeglut_state.c
  // */
  //glutGet( GLenum query );
  //glutDeviceGet( GLenum query );
  //glutGetModifiers( void );
  //glutLayerGet( GLenum query );

  ///*
  // * Font stuff, see freeglut_font.c
  // */
  //glutBitmapCharacter( void* font, int character );
  //glutBitmapWidth( void* font, int character );
  //glutStrokeCharacter( void* font, int character );
  //glutStrokeWidth( void* font, int character );
  //glutBitmapLength( void* font, const unsigned char* string );
  //glutStrokeLength( void* font, const unsigned char* string );

  ///*
  // * Game mode functions, see freeglut_gamemode.c
  // */
  //glutGameModeString( const char* string );
  //glutEnterGameMode( void );
  //glutLeaveGameMode( void );
  //glutGameModeGet( GLenum query );

  ///*
  // * Video resize functions, see freeglut_videoresize.c
  // */
  //glutVideoResizeGet( GLenum query );
  //glutSetupVideoResizing( void );
  //glutStopVideoResizing( void );
  //glutVideoResize( int x, int y, int width, int height );
  //glutVideoPan( int x, int y, int width, int height );

  ///*
  // * Misc keyboard and joystick functions, see freeglut_misc.c
  // */
  //glutIgnoreKeyRepeat( int ignore );
  //glutSetKeyRepeat( int repeatMode );
}
//-----------------------------------------------------------------------------
bool GLUTWindow::setFullscreen(bool fs)
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    if (fs)
    {
      #if defined(WIN32)
        DEVMODE devmode;
        EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,&devmode);
        int width  = devmode.dmPelsWidth;
        int height = devmode.dmPelsHeight;
        glutPositionWindow( 0, 0 );
        glutReshapeWindow( width, height );
      #endif
      glutFullScreen();
    }
    else
    {
      glutPositionWindow( 0, 0 );
      glutReshapeWindow( 640, 480 );
    }
    glutSetWindow( prev );
    mFullscreen = fs;
    return true;
  }
  else
    return false;
}
//-----------------------------------------------------------------------------
void GLUTWindow::setMouseVisible(bool visible)
{
  mMouseVisible = visible;
  if (visible)
    glutSetCursor(GLUT_CURSOR_LEFT_ARROW);
  else
    glutSetCursor(GLUT_CURSOR_NONE);
}
//-----------------------------------------------------------------------------
void GLUTWindow::setMousePosition(int x, int y)
{
  glutWarpPointer(x,y);
}
//-----------------------------------------------------------------------------
void GLUTWindow::update()
{
  if ( handle() )
    glutPostWindowRedisplay( handle() );
}
//-----------------------------------------------------------------------------
void GLUTWindow::makeCurrent()
{
  if ( handle() )
    glutSetWindow( handle() );
}
//-----------------------------------------------------------------------------
void GLUTWindow::destroyWindow()
{
  // according to GLUT specs pag 44 we can do this
  if ( handle() )
  {
    // should trigger glut_close_func
    glutDestroyWindow( handle() );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::updateOverlay()
{
  glutPostOverlayRedisplay();
}
//-----------------------------------------------------------------------------
void GLUTWindow::swapBuffers()
{
  glutSwapBuffers();
}
//-----------------------------------------------------------------------------
void GLUTWindow::show()
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    glutShowWindow();
    glutSetWindow( prev );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::hide()
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    glutHideWindow();
    glutSetWindow( prev );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::getFocus()
{
  if ( handle() )
    glutSetWindow( handle() );
}
//-----------------------------------------------------------------------------
void GLUTWindow::setWindowTitle(const vl::String& title)
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    glutSetWindowTitle( title.toStdString().c_str() );
    glutSetWindow( prev );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::setPosition(int x, int y)
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    glutPositionWindow(x, y);
    glutSetWindow( prev );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::setSize(int w, int h)
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    glutReshapeWindow(w, h);
    glutSetWindow( prev );
  }
}
//-----------------------------------------------------------------------------
vl::ivec2 GLUTWindow::position() const
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    vl::ivec2 v( glutGet(GLUT_WINDOW_X), glutGet(GLUT_WINDOW_Y) );
    glutSetWindow( prev );
    return v;
  }
  else
    return vl::ivec2(0,0);
}
//-----------------------------------------------------------------------------
vl::ivec2 GLUTWindow::size() const
{
  if ( handle() )
  {
    int prev = glutGetWindow();
    glutSetWindow( handle() );
    vl::ivec2 v( glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT) );
    glutSetWindow( prev );
    return v;
  }
  else
    return vl::ivec2(0,0);
}
//-----------------------------------------------------------------------------
void GLUTWindow::initKeymap()
{
  mKeymap.clear();

  mKeymap[27]  = vl::Key_Escape;
  mKeymap[127] = vl::Key_Delete;
  mKeymap[8] = vl::Key_BackSpace;
  mKeymap[13] = vl::Key_Return;
  // mKeymap['/'] = vl::Key_Clear;
  mKeymap[' '] = vl::Key_Space;
  mKeymap['`'] = vl::Key_QuoteLeft;
  mKeymap['-'] = vl::Key_Minus;
  mKeymap['='] = vl::Key_Equal;
  mKeymap['['] = vl::Key_LeftBracket;
  mKeymap[']'] = vl::Key_RightBracket;
  mKeymap[';'] = vl::Key_Semicolon;
  mKeymap['\''] = vl::Key_Quote;
  mKeymap['\\'] = vl::Key_BackSlash;
  mKeymap[','] = vl::Key_Comma;
  mKeymap['.'] = vl::Key_Period;
  mKeymap['/'] = vl::Key_Slash;
  mKeymap['\t'] = vl::Key_Tab;
  mKeymap['!'] = vl::Key_Exclam;
  mKeymap['"'] = vl::Key_QuoteDbl;
  mKeymap['#'] = vl::Key_Hash;
  mKeymap['$'] = vl::Key_Dollar;
  mKeymap['&'] = vl::Key_Ampersand;
  mKeymap['('] = vl::Key_LeftParen;
  mKeymap[')'] = vl::Key_RightParen;
  mKeymap['*'] = vl::Key_Asterisk;
  mKeymap['+'] = vl::Key_Plus;
  mKeymap[':'] = vl::Key_Colon;
  mKeymap['<'] = vl::Key_Less;
  mKeymap['>'] = vl::Key_Greater;
  mKeymap['?'] = vl::Key_Question;
  mKeymap['@'] = vl::Key_At;
  mKeymap['|'] = vl::Key_Caret;
  mKeymap['_'] = vl::Key_Underscore;

  mKeymap['q'] = vl::Key_Q;
  mKeymap['w'] = vl::Key_W;
  mKeymap['e'] = vl::Key_E;
  mKeymap['r'] = vl::Key_R;
  mKeymap['t'] = vl::Key_T;
  mKeymap['y'] = vl::Key_Y;
  mKeymap['u'] = vl::Key_U;
  mKeymap['i'] = vl::Key_I;
  mKeymap['o'] = vl::Key_O;
  mKeymap['p'] = vl::Key_P;
  mKeymap['a'] = vl::Key_A;
  mKeymap['s'] = vl::Key_S;
  mKeymap['d'] = vl::Key_D;
  mKeymap['f'] = vl::Key_F;
  mKeymap['g'] = vl::Key_G;
  mKeymap['h'] = vl::Key_H;
  mKeymap['j'] = vl::Key_J;
  mKeymap['k'] = vl::Key_K;
  mKeymap['l'] = vl::Key_L;
  mKeymap['z'] = vl::Key_Z;
  mKeymap['x'] = vl::Key_X;
  mKeymap['c'] = vl::Key_C;
  mKeymap['v'] = vl::Key_V;
  mKeymap['b'] = vl::Key_B;
  mKeymap['n'] = vl::Key_N;
  mKeymap['m'] = vl::Key_M;

  mKeymap['Q'] = vl::Key_Q;
  mKeymap['W'] = vl::Key_W;
  mKeymap['E'] = vl::Key_E;
  mKeymap['R'] = vl::Key_R;
  mKeymap['T'] = vl::Key_T;
  mKeymap['Y'] = vl::Key_Y;
  mKeymap['U'] = vl::Key_U;
  mKeymap['I'] = vl::Key_I;
  mKeymap['O'] = vl::Key_O;
  mKeymap['P'] = vl::Key_P;
  mKeymap['A'] = vl::Key_A;
  mKeymap['S'] = vl::Key_S;
  mKeymap['D'] = vl::Key_D;
  mKeymap['F'] = vl::Key_F;
  mKeymap['G'] = vl::Key_G;
  mKeymap['H'] = vl::Key_H;
  mKeymap['J'] = vl::Key_J;
  mKeymap['K'] = vl::Key_K;
  mKeymap['L'] = vl::Key_L;
  mKeymap['Z'] = vl::Key_Z;
  mKeymap['X'] = vl::Key_X;
  mKeymap['C'] = vl::Key_C;
  mKeymap['V'] = vl::Key_V;
  mKeymap['B'] = vl::Key_B;
  mKeymap['N'] = vl::Key_N;
  mKeymap['M'] = vl::Key_M;

  mKeymap['0'] = vl::Key_0;
  mKeymap['1'] = vl::Key_1;
  mKeymap['2'] = vl::Key_2;
  mKeymap['3'] = vl::Key_3;
  mKeymap['4'] = vl::Key_4;
  mKeymap['5'] = vl::Key_5;
  mKeymap['6'] = vl::Key_6;
  mKeymap['7'] = vl::Key_7;
  mKeymap['8'] = vl::Key_8;
  mKeymap['9'] = vl::Key_9;
}
//-----------------------------------------------------------------------------
vl::EKey GLUTWindow::mapAsciiKey(unsigned char ascii)
{
  vl::EKey key;
  if(mKeymap.find(ascii) == mKeymap.end())
    key = vl::Key_Unknown;
  else
    key = mKeymap[ascii];

  return key;
}
//-----------------------------------------------------------------------------
vl::EKey GLUTWindow::mapSpecialKey(int special_key)
{
  vl::EKey vl_key = vl::Key_Unknown;
  switch(special_key)
  {
    case GLUT_KEY_F1: vl_key = vl::Key_F1; break;
    case GLUT_KEY_F2: vl_key = vl::Key_F2; break;
    case GLUT_KEY_F3: vl_key = vl::Key_F3; break;
    case GLUT_KEY_F4: vl_key = vl::Key_F4; break;
    case GLUT_KEY_F5: vl_key = vl::Key_F5; break;
    case GLUT_KEY_F6: vl_key = vl::Key_F6; break;
    case GLUT_KEY_F7: vl_key = vl::Key_F7; break;
    case GLUT_KEY_F8: vl_key = vl::Key_F8; break;
    case GLUT_KEY_F9: vl_key = vl::Key_F9; break;
    case GLUT_KEY_F10: vl_key = vl::Key_F10; break;
    case GLUT_KEY_F11: vl_key = vl::Key_F11; break;
    case GLUT_KEY_F12: vl_key = vl::Key_F12; break;
    case GLUT_KEY_LEFT: vl_key = vl::Key_Left; break;
    case GLUT_KEY_UP: vl_key = vl::Key_Up; break;
    case GLUT_KEY_RIGHT: vl_key = vl::Key_Right; break;
    case GLUT_KEY_DOWN: vl_key = vl::Key_Down; break;
    case GLUT_KEY_PAGE_UP: vl_key = vl::Key_PageUp; break;
    case GLUT_KEY_PAGE_DOWN: vl_key = vl::Key_PageDown; break;
    case GLUT_KEY_HOME: vl_key = vl::Key_Home; break;
    case GLUT_KEY_END: vl_key = vl::Key_End; break;
    case GLUT_KEY_INSERT: vl_key = vl::Key_Insert; break;
    default:
      vl_key = vl::Key_Unknown;
  }
  return vl_key;
}
//-----------------------------------------------------------------------------
void GLUTWindow::updateModifiers()
{
  keyRelease(vl::Key_Shift);
  keyRelease(vl::Key_Alt);
  keyRelease(vl::Key_Ctrl);
  int modifiers = glutGetModifiers();
  if (modifiers & GLUT_ACTIVE_SHIFT)
    keyPress(vl::Key_Shift);
  if (modifiers & GLUT_ACTIVE_CTRL)
    keyPress(vl::Key_Ctrl);
  if (modifiers & GLUT_ACTIVE_ALT)
    keyPress(vl::Key_Alt);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_keyboard_func(unsigned char ch, int, int)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();
  win->dispatchKeyPressEvent(ch, win->mapAsciiKey(ch) );
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_keyboard_up_func(unsigned char ch, int, int)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();
  win->dispatchKeyReleaseEvent(ch, win->mapAsciiKey(ch) );
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_special_func(int key, int, int)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();
  win->dispatchKeyPressEvent(0, mapSpecialKey(key) );
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_special_up_func(int key, int, int)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();
  win->dispatchKeyReleaseEvent(0, mapSpecialKey(key) );
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_mouse_func(int button, int state, int x, int y)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();

  vl::EMouseButton btn = vl::UnknownButton;
  switch(button)
  {
    case GLUT_LEFT_BUTTON: btn = vl::LeftButton; break;
    case GLUT_MIDDLE_BUTTON: btn = vl::MiddleButton; break;
    case GLUT_RIGHT_BUTTON: btn = vl::RightButton; break;
  }

  if (state == GLUT_DOWN)
    win->dispatchMouseDownEvent(btn, x, y);
  else
  if (state == GLUT_UP)
    win->dispatchMouseUpEvent(btn, x, y);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_motion_func(int x, int y)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);

  // win->updateModifiers();

  win->dispatchMouseMoveEvent(x, y);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_passive_motion_func(int x, int y)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);

  // !!!
  if (!win->mInited)
    return;

  // win->updateModifiers();

  win->dispatchMouseMoveEvent(x, y);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_mouse_wheel_func(int, int rotation, int, int)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  win->updateModifiers();

  win->dispatchMouseWheelEvent(rotation);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_visibility_func(int visibility)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  // win->updateModifiers(); // cannot be called from here

  win->dispatchVisibilityEvent(visibility == GLUT_VISIBLE);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_reshape_func(int w, int h)
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);

  win->framebuffer()->setWidth(w);
  win->framebuffer()->setHeight(h);

  if (win->mInited == false)
  {
    win->mInited = true;
    win->dispatchInitEvent();
  }

  win->dispatchResizeEvent(w, h);
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_display_func()
{
  int cur_win = glutGetWindow();
  GLUTWindow* win = mWinMap[cur_win];
  VL_CHECK(win);
  // win->updateModifiers(); // cannot be called from here;

  win->dispatchRunEvent();

  //if (win->continuousUpdate())
  //  win->update();
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_close_func()
{
  int cur_win = glutGetWindow();
  if (mWinMap.find(cur_win) != mWinMap.end())
  {
    GLUTWindow* win = mWinMap[cur_win];
    VL_CHECK(win);
    // win->updateModifiers() cannot be called from here
    win->dispatchDestroyEvent();
    win->mHandle = 0;
    win->mInited = false;
    win->mKeymap.clear();
    mWinMap.erase( cur_win );
  }
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_wmclose_func()
{
  glut_close_func();
}
//-----------------------------------------------------------------------------
void GLUTWindow::glut_idle_func()
{
  bool sleep = true;
  for(std::map< int, GLUTWindow* >::iterator it = mWinMap.begin(); it != mWinMap.end(); ++it)
  {
    if (it->second->continuousUpdate())
    {
      it->second->update();
      sleep = false;
    }
  }
  if (sleep)
  {
    vl::Time::sleep(10);
  }
}
//-----------------------------------------------------------------------------
void vlGLUT::atexit_visualization_library_shutdown()
{
  // Workaround for GLUT bug calling the exit function once per window opened +1
  // if one explicitly pushes the X button of one of the windows.
  static bool alread_called = false;
  if (alread_called)
  {
    return;
  }
  else
  {
    vl::VisualizationLibrary::shutdown();
    alread_called = true;
    return;
  }
}
//-----------------------------------------------------------------------------
