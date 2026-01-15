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

#include "BaseDemo.hpp"
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/RenderingTree.hpp>

class App_ShaderOverride: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    // retrieve our vl::Rendering object

    mRendering = rendering()->as<vl::Rendering>();

    // setup transforms

    mCubeTransform1 = new vl::Transform;
    mCubeTransform2 = new vl::Transform;
    mCubeTransform3 = new vl::Transform;
    mRendering->transform()->addChild( mCubeTransform1.get() );
    mRendering->transform()->addChild( mCubeTransform2.get() );
    mRendering->transform()->addChild( mCubeTransform3.get() );

    // setup sphere geometry

    const vl::real fsize = 8;
    vl::ref<vl::Geometry> ball = vl::makeUVSphere( vl::vec3(0,0,0), fsize, 8, 8 );
    ball->computeNormals();

    // setup solid effect

    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_BLEND);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_CULL_FACE);
    effect->shader()->gocMaterial()->setDiffuse( vl::gold );
    effect->shader()->gocMaterial()->setTransparency( 0.5f );

    // creates 3 actors and adds them to the scene

    vl::Actor* cube1 = sceneManager()->tree()->addActor( ball.get(), effect.get(), mCubeTransform1.get() );
    vl::Actor* cube2 = sceneManager()->tree()->addActor( ball.get(), effect.get(), mCubeTransform2.get() );
    vl::Actor* cube3 = sceneManager()->tree()->addActor( ball.get(), effect.get(), mCubeTransform3.get() );

    // setup three wireframe override-shaders

    vl::ref<vl::Shader> sh1 = new vl::Shader;
    vl::ref<vl::Shader> sh2 = new vl::Shader;
    vl::ref<vl::Shader> sh3 = new vl::Shader;

    // common settings

    sh1->enable(vl::EN_LIGHTING);
    sh1->enable(vl::EN_CULL_FACE);
    sh1->gocPolygonMode()->set(vl::PM_LINE,vl::PM_LINE);
    sh1->gocLineWidth()->set(2.0f);

    // share all the states except for the glMaterial ones

    sh2->shallowCopyFrom(*sh1);
    sh3->shallowCopyFrom(*sh1);

    // assign a different flat material color to each shader

    sh1->gocMaterial()->setFlatColor(vl::red);
    sh2->gocMaterial()->setFlatColor(vl::green);
    sh3->gocMaterial()->setFlatColor(vl::blue);

    // setup the renderer that will override the shaders

    vl::ref<vl::Renderer> wire_renderer = new vl::Renderer;
    // keep the color buffer from the solid rendering but clear the depth buffer
    wire_renderer->setClearFlags(vl::CF_DO_NOT_CLEAR);
    // target the same render target
    wire_renderer->setFramebuffer( mRendering->renderers()[0]->framebuffer() );
    // add wireframe renderer
    mRendering->renderers().push_back(wire_renderer.get());

    // setup shader override masks
    // note that we can override the Shader at the vl::Renderer level but
    // we can also override the Effect at the vl::Rendering level, see App_EffectOverride.hpp

    wire_renderer->shaderOverrideMask()[0x01] = sh1;
    wire_renderer->shaderOverrideMask()[0x02] = sh2;
    wire_renderer->shaderOverrideMask()[0x04] = sh3;

    cube1->setEnableMask(0x01);
    cube2->setEnableMask(0x02);
    cube3->setEnableMask(0x04);
  }

  virtual void updateScene()
  {
    vl::real degrees = vl::Time::currentTime() * 45.0f;
    vl::mat4 matrix;
    
    matrix.rotate( degrees, 0,1,0 );
    matrix.translate(-10,0,0);
    mCubeTransform1->setLocalMatrix( matrix );

    matrix.setIdentity();
    matrix.rotate( degrees, 0,1,0 );
    matrix.translate(0,0,0);
    mCubeTransform2->setLocalMatrix( matrix );

    matrix.setIdentity();
    matrix.rotate( degrees, 0,1,0 );
    matrix.translate(+10,0,0);
    mCubeTransform3->setLocalMatrix( matrix );
  }

  void resizeEvent(int w, int h)
  {
    vl::Camera* camera = mRendering->camera();
    camera->viewport()->setWidth ( w );
    camera->viewport()->setHeight( h );
    camera->setProjectionPerspective();
  }

protected:
  vl::ref<vl::Rendering> mRendering;
  vl::ref<vl::Transform> mCubeTransform1;
  vl::ref<vl::Transform> mCubeTransform2;
  vl::ref<vl::Transform> mCubeTransform3;
};

// Have fun!

BaseDemo* Create_App_ShaderOverride() { return new App_ShaderOverride; }
