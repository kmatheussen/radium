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

// mic fixme: implement and cleaup this demo test

#include "BaseDemo.hpp"
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>

class App_MiniEarth: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->enable(vl::EN_DEPTH_TEST);

    // vl::ref<vl::Texture> texture = new vl::Texture("/images/world.topo.bathy.200406.3x8192x4096.png");
    vl::ref<vl::Texture> texture = new vl::Texture("/images/world.topo.bathy.200406.3x2048x1024.png");
    // vl::ref<vl::Texture> texture = new vl::Texture("/images/land_lights_16384.tif");
    // vl::ref<vl::Texture> texture = new vl::Texture("/images/land_ocean_ice_cloud_8192.tif");

    fx->shader()->gocTextureSampler(0)->setTexture(texture.get());

    //vl::ref<vl::Shader> wire = new vl::Shader;
    //wire->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    //wire->gocPolygonOffset()->set(-1, -1);
    //wire->enable(vl::EN_POLYGON_OFFSET_LINE);
    //wire->enable(vl::EN_DEPTH_TEST);
    //fx->lod(0)->push_back(wire.get());

    float dx = 0.5f / texture->width();
    float dy = 0.5f / texture->height();
    vl::ref<vl::Geometry> earth = vl::makeGrid( vl::vec3(0,0,0), vl::fPi*2.0f, vl::fPi, 40, 40, true, vl::fvec2(dx,dy), vl::fvec2(1-dx,1-dy) );
    vl::ArrayFloat3* coord3 = vl::cast<vl::ArrayFloat3>( earth->vertexArray() );
    vl::ref<vl::ArrayFloat3> norm3 = new vl::ArrayFloat3;
    earth->setNormalArray(norm3.get());
    norm3->resize(coord3->size());
    for(size_t i=0; i<coord3->size(); ++i)
    {
      float longitude = coord3->at(i).x();
      float latitude  = coord3->at(i).z();
      coord3->at(i)   = vl::fmat4::getRotation(longitude*vl::fRAD_TO_DEG, 0,-1,0) * vl::fmat4::getRotation(latitude*vl::fRAD_TO_DEG, 1,0,0) * vl::fvec3(0,0,1);
      norm3->at(i)    = coord3->at(i);
      norm3->at(i).normalize();
    }

    sceneManager()->tree()->addActor(earth.get(), fx.get());
  }

protected:
};

// Have fun!

BaseDemo* Create_App_MiniEarth() { return new App_MiniEarth; }
