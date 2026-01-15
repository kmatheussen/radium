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
#include <vlGraphics/Light.hpp>

using namespace vl;

class App_NearFarOptimization: public BaseDemo
{
public:
  // called once after the OpenGL window has been opened 
  void initEvent()
  {
    // allocate the Transform 
    mCubeTransform = new Transform;
    // bind the Transform with the transform tree of the rendring pipeline 
    rendering()->as<Rendering>()->transform()->addChild( mCubeTransform.get() );

    float fsize = 2500;
    // create the cube's Geometry and compute its normals to support lighting 
    ref<Geometry> ball1 = makeIcosphere( vec3(0,0,0), fsize, 2, false );
    ball1->computeNormals();
    ref<Geometry> ball2 = makeIcosphere( vec3(0,0,0), fsize*1.001f, 2, false );
    ball2->computeNormals();

    // setup the effect1 to be used to render the cube 
    ref<Effect> effect1 = new Effect;
    // enable depth test and lighting 
    effect1->shader()->enable(EN_DEPTH_TEST);
    // add a Light to the scene, since no Transform is associated to the Light it will follow the camera 
    effect1->shader()->setRenderState( new Light, 0 );
    // enable the standard OpenGL lighting 
    effect1->shader()->enable(EN_LIGHTING);
    // set the front and back material color of the cube 
    // "gocMaterial" stands for "get-or-create Material"
    effect1->shader()->gocMaterial()->setDiffuse( crimson );

    // setup the effect2 to be used to render the cube 
    ref<Effect> effect2 = new Effect;
    // enable depth test and lighting 
    effect2->shader()->enable(EN_DEPTH_TEST);
    // add a Light to the scene, since no Transform is associated to the Light it will follow the camera 
    effect2->shader()->setRenderState( new Light, 0 );
    // enable the standard OpenGL lighting 
    effect2->shader()->enable(EN_LIGHTING);
    // set the front and back material color of the cube 
    // "gocMaterial" stands for "get-or-create Material"
    effect2->shader()->gocMaterial()->setDiffuse( gold );

    // add the cube to the scene using the previously defined effect and transform 
    sceneManager()->tree()->addActor( ball1.get(), effect1.get(), mCubeTransform.get()  );
    sceneManager()->tree()->addActor( ball2.get(), effect2.get(), mCubeTransform.get()  );
    sceneManager()->computeBounds();

    trackball()->adjustView( rendering()->as<Rendering>(), vec3(0,0,1), vec3(0,1,0), 1.0f );
  }

  // called every frame 
  virtual void updateScene()
  {
    // rotates the cube around the Y axis 45 degrees per second 
    real degrees = Time::currentTime() * 45.0f;
    mat4 matrix = mat4::getRotation( degrees, 0,1,0 );
    mCubeTransform->setLocalMatrix( matrix );

    // periodically toggle near/far optimization
    if ( ::sin( Time::currentTime() * 3.14159265 / 2 ) > 0 )
      rendering()->as<Rendering>()->setNearFarClippingPlanesOptimized(true);
    else
    {
      rendering()->as<Rendering>()->setNearFarClippingPlanesOptimized(false);
      // restore default perspective near/far values
      rendering()->as<Rendering>()->camera()->setProjectionPerspective(60.0f, 0.5f, 10000.0f);
    }
  }

protected:
  ref<Transform> mCubeTransform;
  bool mNearFarOptimized;
};

// Have fun!

BaseDemo* Create_App_NearFarOptimization() { return new App_NearFarOptimization; }
