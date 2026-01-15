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

#ifndef App_RotatingCube_INCLUDE_ONCE
#define App_RotatingCube_INCLUDE_ONCE

#include <vlGraphics/Applet.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/SceneManagerActorTree.hpp>
#include <vlGraphics/Rendering.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/Light.hpp>

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
    vl::ref<vl::Geometry> cube = vl::makeBox( vl::vec3(0,0,0), 10, 10, 10 );
    cube->computeNormals();

    // setup the effect to be used to render the cube 
    vl::ref<vl::Effect> effect = new vl::Effect;
    // enable depth test and lighting 
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    // add a Light to the scene, since no Transform is associated to the Light it will follow the camera 
    effect->shader()->setRenderState( new vl::Light, 0 );
    // enable the standard OpenGL lighting 
    effect->shader()->enable(vl::EN_LIGHTING);
    // set the front and back material color of the cube 
    // "gocMaterial" stands for "get-or-create Material"
    effect->shader()->gocMaterial()->setDiffuse( vl::crimson );

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
    vl::real degrees = vl::Time::currentTime() * 45.0f;
    vl::mat4 matrix = vl::mat4::getRotation( degrees, 0,1,0 );
    mCubeTransform->setLocalMatrix( matrix );
  }

protected:
  vl::ref<vl::Transform> mCubeTransform;
};
// Have fun!

#endif
