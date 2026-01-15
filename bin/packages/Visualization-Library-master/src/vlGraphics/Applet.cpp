/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlGraphics/Applet.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlGraphics/RenderQueue.hpp>
#include <vlCore/Time.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Log.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
Applet::Applet()
{
  VL_DEBUG_SET_OBJECT_NAME()
  mFrameCount = 0;
  mStartTime  = Time::currentTime();
  mFPS        = 0;
  mReadPixels = new ReadPixels;
  mAppletName = "AppletNoName";
}
//-----------------------------------------------------------------------------
void Applet::initialize()
{
  // if the user didn't provide one use the one installed by default
  ref<Rendering> rend = rendering() && rendering()->as<Rendering>() ? rendering()->as<Rendering>() : new Rendering;
  setRendering(rend.get());

  // installs a SceneManagerActorTree as the default scene manager
  mSceneManagerActorTree = new SceneManagerActorTree;
  rend->sceneManagers()->push_back(sceneManager());

  // render target attached externally
  // ...

  mFly       = new GhostCameraManipulator;
  mTrackball = new TrackballManipulator;
  mFly->setEnabled(false);
  mTrackball->setEnabled(true);

  bindManipulators( rend->camera() );
}
//-----------------------------------------------------------------------------
void Applet::updateEvent()
{
  // FPS counter
  if (Time::currentTime() - mStartTime > 0.500f)
  {
    double secs = (Time::currentTime() - mStartTime);
    mFPS = mFrameCount / secs;
    mFrameCount = 0;
    mStartTime = Time::currentTime();
  }
  mFrameCount++;

  // update the scene content
  updateScene();

  // set frame time for all the rendering
  real now_time = Time::currentTime();
  rendering()->setFrameClock( now_time );

  // execute rendering
  rendering()->render();

  // show rendering
  if ( openglContext()->hasDoubleBuffer() )
    openglContext()->swapBuffers();

  VL_CHECK_OGL();

  // useful for debugging
  // wglMakeCurrent(NULL,NULL);
}
//-----------------------------------------------------------------------------
void Applet::keyReleaseEvent(unsigned short, EKey key)
{
  if (key == Key_Escape)
    openglContext()->quitApplication();
  else
  if (key == Key_T)
  {
    mFly->setEnabled(false);
    mTrackball->setEnabled(true);
  }
  else
  if (key == Key_F)
  {
    mTrackball->setEnabled(false);
    mFly->setEnabled(true);
  }
  else
  if (key == Key_F1)
    openglContext()->setFullscreen( !openglContext()->fullscreen() );
  else
  if (key == Key_F5)
  {
    mReadPixels->setup( 0, 0, openglContext()->width(), openglContext()->height(), RDB_BACK_LEFT, false );
    rendering()->onFinishedCallbacks()->push_back( mReadPixels.get() );
    mReadPixels->setRemoveAfterCall(true);
    Time time;
    String filename = Say( appletName() + " - %n%02n%02n%02n%02n.png") << time.year() << time.month() << time.dayOfMonth() << time.hour() << time.second();
    mReadPixels->setSavePath( filename );
    Log::print( Say("Saved screenshot: '%s'\n") << filename );
    openglContext()->update();
  }
  else
  if (key == Key_U)
    openglContext()->update();
  else
  if (key == Key_C)
    openglContext()->setContinuousUpdate( !openglContext()->continuousUpdate() );
}
//-----------------------------------------------------------------------------
void Applet::resizeEvent(int w, int h)
{
  // if a simple Rendering is attached as the rendering root than update viewport and projection matrix.
  Rendering* rend = cast<Rendering>(rendering());
  if (rend)
  {
    VL_CHECK( w == rend->renderer()->framebuffer()->width() );
    VL_CHECK( h == rend->renderer()->framebuffer()->height() );
    rend->camera()->viewport()->setWidth( w );
    rend->camera()->viewport()->setHeight( h );
    rend->camera()->setProjectionPerspective();
  }
}
//-----------------------------------------------------------------------------
void Applet::bindManipulators(Camera* camera)
{
  mFly->setCamera( camera );
  mTrackball->setCamera( camera );
  mTrackball->setTransform( NULL );
  mTrackball->setPivot( vec3(0,0,0) );
}
//-----------------------------------------------------------------------------
void Applet::addedListenerEvent(OpenGLContext* ogl_context)
{
  VL_CHECK(ogl_context)
  VL_CHECK(mFly->openglContext() == NULL);
  ogl_context->addEventListener( mFly.get() );

  VL_CHECK(mTrackball->openglContext() == NULL);
  mTrackball->setPivot( vec3(0,0,0) );
  ogl_context->addEventListener( mTrackball.get() );
}
//-----------------------------------------------------------------------------
void Applet::removedListenerEvent(OpenGLContext* ogl_context)
{
  ogl_context->removeEventListener( mTrackball.get() );
  ogl_context->removeEventListener( mFly.get() );
}
//-----------------------------------------------------------------------------
void Applet::destroyEvent()
{
  mFly->setCamera(NULL);
  mTrackball->setCamera(NULL);
  mTrackball->setTransform(NULL);
  mRendering = NULL;
}
//-----------------------------------------------------------------------------
