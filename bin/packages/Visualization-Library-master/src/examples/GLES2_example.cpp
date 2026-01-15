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

#include <vlCore\VisualizationLibrary.hpp>
#include <vlGraphics/Applet.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlCore\Log.hpp>
#include <vlCore\Time.hpp>
#include <vlEGL\EGLWindow.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
class App_RotatingCube: public vl::Applet
{
public:
  // called once after the OpenGL window has been opened 
  void initEvent()
  {
    // allocate the Transform 
    mCubeTransform = new vl::Transform;

    // bind the Transform with the transform tree of the rendring pipeline 
    rendering()->as<vl::Rendering>()->transform()->addChild( mCubeTransform.get() );

    // create the cube's Geometry and compute its normals to support lighting 
    vl::ref<vl::Geometry> cube = vl::makeTeapot( vl::fvec3(0,0,0), 10, 8);
    cube->computeNormals();
    cube->setColorArray(vl::gold);

    // setup the effect to be used to render the cube 
    vl::ref<vl::Effect> effect = new vl::Effect;
    // enable depth test
    effect->shader()->enable(vl::EN_DEPTH_TEST);

    // simple GLSL shader
    effect->shader()->gocGLSLProgram()->attachShader( new vl::GLSLVertexShader  ("/glsl/gles2_simple_light.vert") );
    effect->shader()->gocGLSLProgram()->attachShader( new vl::GLSLFragmentShader("/glsl/gles2_simple_light.frag") );
    
    // bind vertex attribute semantics
    effect->shader()->gocGLSLProgram()->bindAttribLocation(vl::VA_Position, "A_Position");
    effect->shader()->gocGLSLProgram()->bindAttribLocation(vl::VA_Normal, "A_Normal");
    effect->shader()->gocGLSLProgram()->bindAttribLocation(vl::VA_Color, "A_Color");

    /* link the GLSL program */
    effect->shader()->gocGLSLProgram()->linkProgram();

    // define light position
    effect->shader()->gocGLSLProgram()->gocUniform("U_LightPosition")->setUniform(vl::fvec3(0, 25, 0));

    // install our scene manager, we use the SceneManagerActorTree which is the most generic
    vl::ref<vl::SceneManagerActorTree> scene_manager = new vl::SceneManagerActorTree;
    rendering()->as<vl::Rendering>()->sceneManagers()->push_back(scene_manager.get());

    // add the cube to the scene using the previously defined effect and transform 
    scene_manager->tree()->addActor( cube.get(), effect.get(), mCubeTransform.get()  );
  }

  // called every frame 
  virtual void updateScene()
  {
    // rotates the cube around the Y axis 45 degrees per second 
    vl::Real degrees = vl::Time::currentTime() * 45.0f;
    vl::mat4 matrix = vl::mat4::getRotation( degrees, 0,1,0 );
    mCubeTransform->setLocalMatrix( matrix );
  }

protected:
  vl::ref<vl::Transform> mCubeTransform;
};
//-----------------------------------------------------------------------------
int main ( int argc, char *argv[] )
{ 
  struct SContract
  {
    /* Init VL on startup */
    SContract() { vl::VisualizationLibrary::init(); }
    
    /* Shutdown VL on exit */
    ~SContract() { vl::VisualizationLibrary::shutdown(); }
  
  } contract;
  
  ref<vlEGL::EGLWindow> egl_window = new vlEGL::EGLWindow;

  vl::OpenGLContextFormat format;
  format.setDoubleBuffer(true);
  format.setRGBABits( 5,6,5,0 );
  format.setDepthBufferBits(16);
  format.setStencilBufferBits(0);
  format.setMultisampleSamples(0);
  format.setMultisample(false);
  format.setVSync(false);
  /* NOTE THIS */
  format.setContextClientVersion(2);

  if ( !egl_window->initEGLWindow(NULL, "OpenGL ES 2.x Example", format, 0, 0, 640, 480) )
  {
    Log::error("EGLWindow::initEGLWindow() failed!\n");
    return 1;
  }

  /* create the applet to be run */
  ref<Applet> applet = new App_RotatingCube;
  applet->initialize();

  egl_window->addEventListener( applet.get() );

  /* target the window so we can render on it */
  applet->rendering()->as<Rendering>()->renderer()->setFramebuffer( egl_window->framebuffer() );
  
  /* black background */
  applet->rendering()->as<Rendering>()->camera()->viewport()->setClearColor( black );
  
  /* define the camera position and orientation */
  vec3 eye    = vec3(0,10,35); // camera position
  vec3 center = vec3(0,0,0);   // point the camera is looking at
  vec3 up     = vec3(0,1,0);   // up direction
  mat4 view_mat = mat4::getLookAt(eye, center, up);
  applet->rendering()->as<Rendering>()->camera()->setViewMatrix( view_mat );

  /* shows the window and dispatches the first resize event */
  egl_window->show();

  /* run! */
  return vlEGL::messageLoop();
}
