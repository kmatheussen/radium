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
#include <vlGraphics/DrawPixels.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Geometry.hpp>

class App_DrawPixels: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    // transform used for the moving cube and star

    vl::ref< vl::Transform > transf = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(transf.get());

    // effect for 2d rendering

    vl::ref<vl::Effect> pixel_fx = new vl::Effect;
    pixel_fx->shader()->enable(vl::EN_BLEND);
    pixel_fx->shader()->enable(vl::EN_ALPHA_TEST);
    pixel_fx->shader()->gocAlphaFunc()->set(vl::FU_GEQUAL, 0.9f);

    // points rendering

    vl::ref<vl::ImagePBO> circle16 = new vl::ImagePBO("/images/circle16.png");

    mPoints = new vl::DrawPixels;
    for(int i=0; i<1000; ++i)
    {
      vl::ref<vl::DrawPixels::Pixels> pixels = new vl::DrawPixels::Pixels(circle16.get(), 0,0);
      mPoints->draws()->push_back( pixels.get() );
      vl::ivec2 pos;
      pos.x() = int(openglContext()->width()  / 2.0 + rand()%300 - 150);
      pos.y() = int(openglContext()->height() / 2.0 + rand()%300 - 150);
      pixels->setPosition(pos);
      pixels->setAlign(vl::AlignHCenter | vl::AlignVCenter);
    }

    mPoints->generatePixelBufferObjects(vl::BU_STATIC_DRAW, true);

    sceneManager()->tree()->addActor( mPoints.get(), pixel_fx.get(), NULL )->setRenderRank(0);

    // split star rendering

    vl::ref<vl::ImagePBO> star_img = new vl::ImagePBO("/images/star.png");

    int w = star_img->width() / 2;
    mStar1 = new vl::DrawPixels::Pixels( star_img.get(), 0, 0, 0, 0, w, w );
    mStar2 = new vl::DrawPixels::Pixels( star_img.get(), 0, 0, w, w, w, w );
    mStar3 = new vl::DrawPixels::Pixels( star_img.get(), 0, 0, w, 0, w, w);
    mStar4 = new vl::DrawPixels::Pixels( star_img.get(), 0, 0, 0, w, w, w);

    mStar1->setAlign(vl::AlignRight|vl::AlignTop);
    mStar2->setAlign(vl::AlignLeft|vl::AlignBottom);
    mStar3->setAlign(vl::AlignLeft|vl::AlignTop);
    mStar4->setAlign(vl::AlignRight|vl::AlignBottom);

    mStar = new vl::DrawPixels;
    mStar->draws()->push_back( mStar1.get() );
    mStar->draws()->push_back( mStar2.get() );
    mStar->draws()->push_back( mStar3.get() );
    mStar->draws()->push_back( mStar4.get() );

    mStar->generatePixelBufferObjects(vl::BU_STATIC_DRAW, true);

    sceneManager()->tree()->addActor( mStar.get(), pixel_fx.get(), transf.get() )->setRenderRank(1);

    // moving cube

    vl::ref<vl::Effect> cube_fx = new vl::Effect;
    cube_fx->shader()->enable(vl::EN_DEPTH_TEST);
    cube_fx->shader()->enable(vl::EN_LIGHTING);
    cube_fx->shader()->setRenderState( new vl::Light, 0 );

    vl::ref<vl::Geometry> cube = vl::makeBox( vl::vec3(0,0,0), 1, 1, 1 );
    cube->computeNormals();
    mCube = sceneManager()->tree()->addActor(cube.get(), cube_fx.get(), transf.get() );
    mCube->setRenderRank(2); // draw after 2d objects
  }

  virtual void updateScene()
  {
    vl::ivec2 pos;
    vl::ivec2 pos1 = pos + vl::ivec2(vl::fvec2(-1,-1) * (float)((1+sin(vl::Time::currentTime()*vl::fPi*2.0f/5.0f))*100.0f));
    vl::ivec2 pos2 = pos + vl::ivec2(vl::fvec2(+1,+1) * (float)((1+sin(vl::Time::currentTime()*vl::fPi*2.0f/5.0f))*100.0f));
    vl::ivec2 pos3 = pos + vl::ivec2(vl::fvec2(+1,-1) * (float)((1+sin(vl::Time::currentTime()*vl::fPi*2.0f/5.0f))*100.0f));
    vl::ivec2 pos4 = pos + vl::ivec2(vl::fvec2(-1,+1) * (float)((1+sin(vl::Time::currentTime()*vl::fPi*2.0f/5.0f))*100.0f));
    mStar1->setPosition( pos1 );
    mStar2->setPosition( pos2 );
    mStar3->setPosition( pos3 );
    mStar4->setPosition( pos4 );

    vl::mat4 mat = vl::mat4::getRotation( (float)vl::Time::currentTime()*45.0f, 1,1,1 );
    mat.translate( sin(vl::Time::currentTime()*vl::fPi*2.0f/2.0f)*2.0f, 0, 0 );
    mCube->transform()->setLocalMatrix( mat );
  }

protected:
  vl::ref<vl::Actor> mCube;
  vl::ref<vl::DrawPixels> mStar;
  vl::ref<vl::DrawPixels> mPoints;
  vl::ref<vl::DrawPixels::Pixels> mStar1;
  vl::ref<vl::DrawPixels::Pixels> mStar2;
  vl::ref<vl::DrawPixels::Pixels> mStar3;
  vl::ref<vl::DrawPixels::Pixels> mStar4;
};

// Have fun!

BaseDemo* Create_App_DrawPixels() { return new App_DrawPixels; }
