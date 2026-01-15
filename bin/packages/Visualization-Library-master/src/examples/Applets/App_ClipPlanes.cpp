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
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/ClipPlane.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

class App_ClipPlanes: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    // load model
    vl::ref<vl::Geometry> model = vl::loadResource("/models/3ds/monkey.3ds")->get<vl::Geometry>(0);
    model->computeNormals();

    // install transform
    mClipTr = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(mClipTr.get());

    // to be used later
    vl::ref<vl::Light> light = new vl::Light;

    // demonstrates multipassing clipping

    vl::ref<vl::Effect> clip_fx  = new vl::Effect;
    vl::ref<vl::Shader> clip1_sh = new vl::Shader; 
    vl::ref<vl::Shader> clip2_sh = new vl::Shader;
    // setup clipping pass 1
    clip1_sh->setRenderState( light.get(), 0 );
    clip1_sh->enable(vl::EN_LIGHTING);
    clip1_sh->enable(vl::EN_DEPTH_TEST);
    clip1_sh->gocMaterial()->setBackDiffuse(vl::yellow);
    clip1_sh->gocLightModel()->setTwoSide(true);
    // clipping plane 1 setup
    clip1_sh->gocClipPlane(0)->setPlane( vl::Plane(0.2f, vl::vec3(0,+1,0)) );
    clip1_sh->gocClipPlane(0)->bindTransform( mClipTr.get() );
    // setup clipping pass 2
    clip2_sh->setRenderState( light.get(), 0 );
    clip2_sh->enable(vl::EN_LIGHTING);
    clip2_sh->enable(vl::EN_DEPTH_TEST);
    clip2_sh->gocMaterial()->setBackDiffuse(vl::green);
    clip2_sh->gocLightModel()->setTwoSide(true);
    // clipping plane 2 setup
    clip2_sh->gocClipPlane(0)->setPlane( vl::Plane(0.2f, vl::vec3(0,-1,0)) );
    clip2_sh->gocClipPlane(0)->bindTransform( mClipTr.get() );
    // install the two passes for LOD 0
    clip_fx->setLOD(0, clip1_sh.get(), clip2_sh.get());
    // add model to the scene
    sceneManager()->tree()->addActor( model.get(), clip_fx.get(), NULL );

    // renders a plane for visual feedback

    // setup effect
    vl::ref<vl::Effect> plane_fx = new vl::Effect;
    plane_fx->setRenderRank(1); // draw after the clipped model
    plane_fx->shader()->enable(vl::EN_DEPTH_TEST);
    plane_fx->shader()->enable(vl::EN_BLEND);
    plane_fx->shader()->gocLightModel()->setTwoSide(true);
    plane_fx->shader()->gocColor()->setValue(vl::fvec4(1,0,0,0.3f)); // transparent red
    // add plane actor
    vl::ref<vl::Geometry> plane = vl::makeGrid( vl::vec3(0,0,0), 4,4, 2,2 );
    sceneManager()->tree()->addActor( plane.get(), plane_fx.get(), mClipTr.get() );
  }

  virtual void updateScene()
  {
    // animate the clipping planes and the rendered plane
    vl::real t = (vl::real)vl::Time::currentTime();
    mClipTr->setLocalMatrix( vl::mat4::getRotation(t*90.0f, sin(t*(vl::fPi-3.0f)),1,cos(t)) );
  }

protected:
  vl::ref<vl::Transform> mClipTr;
};

// Have fun!

BaseDemo* Create_App_ClipPlanes() { return new App_ClipPlanes; }
