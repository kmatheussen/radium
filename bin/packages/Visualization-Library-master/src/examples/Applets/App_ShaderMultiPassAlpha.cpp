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
#include <vlCore/Colors.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/DistanceLODEvaluator.hpp>
#include <vlGraphics/PixelLODEvaluator.hpp>
#include <vlGraphics/Light.hpp>

/*
This applet tests multipassing with alpha blending. The objects must be sorted based on their distance from the camera 
since their first shader pass has alpha enabled. Wire frame of objects in the background must be visible through closer
transparent objects.
*/
class App_ShaderMultipassAlpha: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    /* effect to be applied to the sphere */
    vl::ref<vl::Effect> sphere_fx = new vl::Effect;

    /* pass #0: solid pass */
    sphere_fx->shader()->enable(vl::EN_DEPTH_TEST);
    sphere_fx->shader()->enable(vl::EN_CULL_FACE);
    sphere_fx->shader()->enable(vl::EN_LIGHTING);
    sphere_fx->shader()->setRenderState( new vl::Light, 0 );
    sphere_fx->shader()->enable(vl::EN_BLEND);
    sphere_fx->shader()->gocMaterial()->setFrontDiffuse( vl::fvec4(1.0f, 0.0f, 0.0f, 0.6f) );

    /* pass #1: wireframe pass */
    vl::ref<vl::Shader> wirepass = new vl::Shader;
    wirepass->enable(vl::EN_DEPTH_TEST);
    wirepass->enable(vl::EN_BLEND);
    wirepass->enable(vl::EN_LINE_SMOOTH);
    wirepass->enable(vl::EN_POLYGON_OFFSET_LINE);
    wirepass->gocHint()->setLineSmoothHint(vl::HM_NICEST);
    wirepass->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    wirepass->gocPolygonOffset()->set(-1.0f, -1.0f);
    wirepass->gocColor()->setValue(vl::green);

    /* set wireframe shader as second pass */
    sphere_fx->lod(0)->push_back(wirepass.get());

    /* create template sphere */
    vl::ref<vl::Geometry> sphere = vl::makeUVSphere(vl::vec3(0,0,0), 6, 10, 10);
    sphere->computeNormals();

    /* generate a ring of spheres */
    const int actor_count = 20;
    for(int i=0;i<actor_count; i++)
    {
      vl::ref<vl::Transform> tr = new vl::Transform;
      vl::real t = 360.0f / actor_count * i;
      vl::vec3 v = vl::mat4::getRotation(t, 0,1,0) * vl::vec3(30,0,0);
      tr->setLocalMatrix( vl::mat4::getTranslation(v) );
      rendering()->as<vl::Rendering>()->transform()->addChild(tr.get());
      sceneManager()->tree()->addActor( sphere.get(), sphere_fx.get(), tr.get() );
    }
  }

  /* rotate the camera around the sphere */
  void updateScene()
  {
    vl::vec3 eye( 40, 0, 0 );
    eye = vl::mat4::getRotation( vl::Time::currentTime() * 5.0f, 0, 1, 0 ) * eye;
    vl::mat4 m = vl::mat4::getLookAt( eye, vl::vec3(0,0,0), vl::vec3(0,1,0) );
    rendering()->as<vl::Rendering>()->camera()->setViewMatrix(m);
  }
};

// Have fun!

BaseDemo* Create_App_ShaderMultiPassAlpha() { return new App_ShaderMultipassAlpha(); }
