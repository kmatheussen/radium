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

class App_ScatterPlot3D: public BaseDemo
{
  vl::ref<vl::Transform> _tr;
  int _test_number;

public:
  App_ScatterPlot3D(int test): _test_number(test) {}
  
  void updateScene()
  {
    _tr->setLocalMatrix(vl::mat4::getRotation( vl::Time::currentTime()*45, 0,1,0 ) );
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    vl::ref<vl::Effect> effect = new vl::Effect;

    if (_test_number == 0)
    {
      effect->shader()->gocPointSize()->set(5.0f);
      effect->shader()->enable(vl::EN_POINT_SMOOTH);
      effect->shader()->enable(vl::EN_DEPTH_TEST);
      effect->shader()->enable(vl::EN_CULL_FACE);
      effect->shader()->enable(vl::EN_BLEND);
      effect->shader()->gocHint()->setPointSmoothHint(vl::HM_NICEST);
    }
    else
    if (_test_number == 1)
    {
      effect->shader()->gocPointSize()->set(10.0f);

      if (vl::Has_Point_Sprite)
      {
        vl::ref<vl::Image> img = vl::loadImage("/images/particle.tif");
        effect->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( img.get() ) );
        effect->shader()->gocTexEnv(0)->setPointSpriteCoordReplace(true);
        effect->shader()->enable(vl::EN_POINT_SPRITE); 
      }
      else
        vl::Log::error("Point sprites not supported.\n");

       effect->shader()->enable(vl::EN_DEPTH_TEST);
       effect->shader()->enable(vl::EN_CULL_FACE);
       effect->shader()->enable(vl::EN_ALPHA_TEST);
       effect->shader()->gocAlphaFunc()->set(vl::FU_GEQUAL, 1.0f - 0.02f);
       effect->shader()->enable(vl::EN_BLEND);
    }

    std::vector< vl::vec3 > pointset;
    for(int i=0; i<100000; i++)
    {
      vl::vec3 v;
      v.x() = vl::random(-1,+1)*320;
      v.y() = vl::random(-1,+1)*320;
      v.z() = vl::random(-1,+1)*320;
      pointset.push_back(v);
    }

    _tr = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(_tr.get());
    vl::ref<vl::Geometry> points = vl::makePoints( pointset );
    sceneManager()->tree()->addActor( points.get(), effect.get(), _tr.get() );

    vl::ref<vl::Effect> ax_effect = new vl::Effect;
    ax_effect->shader()->enable(vl::EN_LIGHTING);
    ax_effect->shader()->enable(vl::EN_DEPTH_TEST);
    ax_effect->shader()->enable(vl::EN_CULL_FACE);
    ax_effect->shader()->setRenderState( new vl::Light, 0 );
    ax_effect->shader()->gocMaterial()->setDiffuse(vl::red);

    vl::ref<vl::Geometry> axis1 = vl::makeCylinder( vl::vec3(-360,0,-360), 10, 360*2 );
    sceneManager()->tree()->addActor( axis1.get(), ax_effect.get(), _tr.get() );
    axis1->computeNormals();

    vl::ref<vl::Geometry> axis2 = vl::makeCylinder( vl::vec3(0,0,0), 10, 360*2 );
    sceneManager()->tree()->addActor( axis2.get(), ax_effect.get(), _tr.get() );
    axis2->transform( vl::mat4::getTranslation(-360,-360,0) * vl::mat4::getRotation(90,1,0,0) );
    axis2->computeNormals();

    vl::ref<vl::Geometry> axis3 = vl::makeCylinder( vl::vec3(0,0,0), 10, 360*2 );
    sceneManager()->tree()->addActor( axis3.get(), ax_effect.get(), _tr.get() );
    axis3->transform( vl::mat4::getTranslation(0,-360,-360) * vl::mat4::getRotation(90,0,0,1) );
    axis3->computeNormals();

    vl::ref<vl::Geometry> top1 = vl::makeCone( vl::vec3(0,20,0), 20, 40 );
    sceneManager()->tree()->addActor( top1.get(), ax_effect.get(), _tr.get() );
    top1->transform( vl::mat4::getTranslation(-360,360,-360) );
    top1->computeNormals();

    vl::ref<vl::Geometry> top2 = vl::makeCone( vl::vec3(0,20,0), 20, 40 );
    sceneManager()->tree()->addActor( top2.get(), ax_effect.get(), _tr.get() );
    top2->transform( vl::mat4::getTranslation(-360,-360, 360) * vl::mat4::getRotation(90,1,0,0) );
    top2->computeNormals();

    vl::ref<vl::Geometry> top3 = vl::makeCone( vl::vec3(0,20,0), 20, 40 );
    sceneManager()->tree()->addActor( top3.get(), ax_effect.get(), _tr.get() );
    top3->transform( vl::mat4::getTranslation(360,-360,-360) * vl::mat4::getRotation(90,0,0,-1) );
    top3->computeNormals();
  }

};

// Have fun!

BaseDemo* Create_App_ScatterPlot3D(int test) { return new App_ScatterPlot3D(test); }
