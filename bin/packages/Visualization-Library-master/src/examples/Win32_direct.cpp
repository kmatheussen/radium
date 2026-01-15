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

#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/TrackballManipulator.hpp>
#include <vlGraphics/UIEventListener.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlWin32/Win32Window.hpp>

class MyApplet: public vl::UIEventListener
{
public:
  MyApplet()
  {
    // allocate rendering and scene manager
    mRendering    = new vl::Rendering;
    mSceneManager = new vl::SceneManagerActorTree;
    mRendering->sceneManagers()->push_back( mSceneManager.get() );
  }

  // ----- UIEventListener -----

  virtual void initEvent() 
  {
    // --- scene setup ---

    // bind the Transform with the transform tree of the rendring pipeline 
    mCubeTransform = new vl::Transform;
    rendering()->transform()->addChild( mCubeTransform.get() );

    // create the teapot's Geometry and compute its normals to support lighting 
    vl::ref<vl::Geometry> teapot = vl::makeTeapot( vl::vec3(0,0,0), 15 );
    teapot->computeNormals();

    // setup the effect to be used to render the teapot 
    vl::ref<vl::Effect> effect = new vl::Effect;
    // enable depth test and lighting 
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    // add a Light to the scene, since no Transform is associated to the Light it will follow the camera 
    effect->shader()->setRenderState( new vl::Light, 0 );
    // enable the standard OpenGL lighting 
    effect->shader()->enable(vl::EN_LIGHTING);
    // set the front and back material color of the teapot 
    // "gocMaterial" stands for "get-or-create Material"
    effect->shader()->gocMaterial()->setDiffuse( vl::gold );
    effect->shader()->gocMaterial()->setSpecular( vl::white );
    effect->shader()->gocMaterial()->setShininess( 60 );

    // add actor to the scene
    sceneManager()->tree()->addActor( teapot.get(), effect.get(), mCubeTransform.get()  );

    // --- render target, viewport, camera position ---

    // install the render target belonging to the opengl window
    rendering()->renderer()->setFramebuffer( openglContext()->framebuffer() );

    // set clear color to white
    rendering()->camera()->viewport()->setClearColor( vl::white );

    // define the camera position and orientation
    vl::vec3 eye    = vl::vec3(0,10,20); // camera position
    vl::vec3 center = vl::vec3(0,0,0);   // point the camera is looking at
    vl::vec3 up     = vl::vec3(0,1,0);   // up direction
    vl::mat4 view_mat = vl::mat4::getLookAt(eye, center, up);
    rendering()->camera()->setViewMatrix( view_mat );

    // --- trackball manipulator ---

    vl::ref<vl::TrackballManipulator> trackball = new vl::TrackballManipulator;
    // bind the camera to be manipulated
    trackball->setCamera( rendering()->camera() );
    // manipulate the camera's transform
    trackball->setTransform(NULL);
    // rotation pivot coordinates
    trackball->setPivot( vl::vec3(0,0,0) );
    // install the trackball
    openglContext()->addEventListener(trackball.get());
  }

  virtual void updateEvent() 
  {
    // --- scene update ---

    // rotates the teapot around the Y axis 15 degrees per second 
    vl::real degrees = vl::Time::currentTime() * 15;
    vl::mat4 matrix = vl::mat4::getRotation( degrees, 0,1,0 );
    mCubeTransform->setLocalMatrix( matrix );

    // --- rendering ---

    // set frame time (used for animation updates etc.)
    rendering()->setFrameClock( vl::Time::currentTime() );

    // execute rendering
    rendering()->render();

    // swap back and front buffers
    if ( openglContext()->hasDoubleBuffer() )
      openglContext()->swapBuffers();
  }

  virtual void resizeEvent(int w, int h) 
  {
    // update viewport dimensions
    rendering()->camera()->viewport()->set( 0, 0, w, h );

    // update the projection matrix
    vl::mat4 proj_matr = vl::mat4::getPerspective( 60 /*FOV*/, (float)w/h, 5/*near plane*/, 100/*far plane*/);
    rendering()->camera()->setProjectionMatrix( proj_matr, vl::PMT_PerspectiveProjection );

    /* can also be done like this:
    (the camera already knows the viewport dimensions and aspect ratio) 
    rendering()->camera()->setProjectionPerspective( 60, 5, 100 ); */
  }

  virtual void destroyEvent() {}

  virtual void enableEvent(bool) {}

  virtual void addedListenerEvent(vl::OpenGLContext*) {}

  virtual void removedListenerEvent(vl::OpenGLContext*) {}

  virtual void mouseMoveEvent(int, int) {}

  virtual void mouseUpEvent(vl::EMouseButton, int, int) {}

  virtual void mouseDownEvent(vl::EMouseButton, int, int) {}

  virtual void mouseWheelEvent(int) {}

  virtual void keyPressEvent(unsigned short, vl::EKey) {}

  virtual void keyReleaseEvent(unsigned short, vl::EKey) {}

  virtual void fileDroppedEvent(const std::vector<vl::String>&) {}

  virtual void visibilityEvent(bool) {}

  // ----- user methods -----

  const vl::Rendering* rendering() const { return mRendering.get(); }
  vl::Rendering* rendering() { return mRendering.get(); }

  const vl::SceneManagerActorTree* sceneManager() const { return mSceneManager.get(); }
  vl::SceneManagerActorTree* sceneManager() { return mSceneManager.get(); }

public:
  vl::ref<vl::Rendering> mRendering;
  vl::ref<vl::SceneManagerActorTree> mSceneManager;
  vl::ref<vl::Transform> mCubeTransform;
};

int APIENTRY WinMain(HINSTANCE /*hCurrentInst*/, HINSTANCE /*hPreviousInst*/, LPSTR /*lpszCmdLine*/, int /*nCmdShow*/)
{
  /* open a console so we can see the applet's output on stdout */
  vl::showWin32Console();

  /* init Visualization Library */
  vl::VisualizationLibrary::init();

  /* setup the OpenGL context format */
  vl::OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 8,8,8,0 );
  format.setDepthBufferBits(24);
  format.setStencilBufferBits(8);
  format.setFullscreen(false);
  format.setMultisampleSamples(16);
  format.setMultisample(false);

  /* Initialize the OpenGL context and window properties */
  vl::ref<vlWin32::Win32Window> win32_window = new vlWin32::Win32Window;
  int x = 0, y = 0, width = 512, height= 512;
  win32_window->initWin32GLWindow(NULL, NULL, "Golden Teapot", format, x, y, width, height );

  /* bind the applet so it receives all the GUI events related to the OpenGLContext */
  win32_window->addEventListener( new MyApplet );

  /* show the window */
  win32_window->show();

  /* run the Win32 message loop */
  int res = vlWin32::messageLoop();

  /* deallocate the window with all the OpenGL resources before shutting down Visualization Library */
  win32_window = NULL;

  /* shutdown Visualization Library */
  vl::VisualizationLibrary::shutdown();

  return res; 
}
// Have fun!
