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
#include <vlGraphics/DepthSortCallback.hpp>
#include <vlGraphics/Light.hpp>
#include <vlCore/Vector4.hpp>

class App_PointSplatting: public BaseDemo
{
public:
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    trackball()->setTransform(NULL);

    // setup texture
    vl::ref<vl::Image> img = vl::loadImage("/volume/VLTest.dat")->convertFormat(vl::IF_LUMINANCE);

    vl::ref<vl::Actor> splat_actor = new vl::Actor;
    splat_actor->actorEventCallbacks()->push_back( new vl::DepthSortCallback );
    init_LUMINANCE_UBYTE( splat_actor.get(), img.get(), vl::fvec3(10,10,10), 80, 255, TransferRGBA_255() );

    vl::ref<vl::Effect> fx = new vl::Effect;

    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->gocLightModel()->setTwoSide(true);
    // enable color material if you want to see per-point colors
    fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
    fx->shader()->enable( vl::EN_BLEND );
    fx->shader()->enable(vl::EN_POINT_SMOOTH);
    fx->shader()->gocPointSize()->set(1.5f);
    splat_actor->setEffect(fx.get());

    sceneManager()->tree()->addActor( splat_actor.get() );

  }

protected:
  class TransferRGBA_255
  {
  public:
    void operator()(const unsigned char& /*val*/, unsigned char&r, unsigned char&g, unsigned char&b, unsigned char&a) const
    {
      r = 255;
      g = 255;
      b = 255;
      a = 255;
    }
  };

  class TransferRGB_255_A_Value
  {
  public:
    void operator()(const unsigned char& val, unsigned char&r, unsigned char&g, unsigned char&b, unsigned char&a) const
    {
      r = 255;
      g = 255;
      b = 255;
      a = val;
    }
  };

  template<class TF>
  void init_LUMINANCE_UBYTE(
    vl::Actor* actor,
    vl::Image* img,
    vl::fvec3 scale = vl::fvec3(10.0f, 10, 10.0f),
    int threshold_min = 32,
    int threshold_max = 255,
    const TF& transfer_func = TransferRGBA_255() )
  {
    if (img->type() != vl::IT_UNSIGNED_BYTE || img->format() != vl::IF_LUMINANCE)
    {
      vl::Log::error("init_LUMINANCE_UBYTE() error:\n" + img->print() );
      return;
    }

    vl::ref<vl::Geometry> geometry;
    vl::ref<vl::ArrayFloat3> points;
    vl::ref<vl::DrawElementsUInt> draw_elements;
    vl::ref<vl::ArrayFloat3> eye_space_points;

    eye_space_points = new vl::ArrayFloat3;
    geometry = new vl::Geometry;
    points = new vl::ArrayFloat3;
    vl::ref<vl::ArrayFloat3> norms = new vl::ArrayFloat3;
    draw_elements = new vl::DrawElementsUInt(vl::PT_POINTS);
    vl::ref<vl::ArrayUByte4> color = new vl::ArrayUByte4;

    geometry->setVertexArray( points.get() );
    geometry->setNormalArray( norms.get() );
    geometry->setColorArray( color.get() );
    geometry->drawCalls()->push_back( draw_elements.get() );

    int point_count = 0;
    for(int index = 0, z=0; z<img->depth(); ++z)
    {
      for(int y=0; y<img->height(); ++y)
      {
        for(int x=0; x<img->width(); ++x, ++index)
        {
          int val = img->pixels()[ x + y*img->pitch() + z*img->height()*img->pitch() ];
          if ( val >= threshold_min && val <= threshold_max)
            ++point_count;
        }
      }
    }

    #ifndef NDEBUG
      vl::Log::print(vl::Say("original points = %6.0nK\n") << img->depth() * img->height() * img->width() / 1000.0f );
      vl::Log::print(vl::Say("accepted points = %6.0nK\n") << point_count / 1000.0f);
      vl::Log::print(vl::Say("simplif. ratio  = %n%%\n") << (1.0f - (float)point_count / (img->depth() * img->height() * img->width())) * 100.0f );
    #endif

    points->resize( point_count );
    norms->resize( point_count );
    draw_elements->indexBuffer()->resize( point_count );
    color->resize( point_count );

    for(int index = 0, z=0; z<img->depth(); ++z)
    {
      for(int y=0; y<img->height(); ++y)
      {
        for(int x=0; x<img->width(); ++x)
        {
          int val = img->pixels()[ x + y*img->pitch() + z*img->height()*img->pitch() ];

          if ( val >= threshold_min && val <= threshold_max)
          {
            float tx = (float)x / ( img->width() -1 );
            float ty = (float)y / ( img->height()-1 );
            float tz = (float)z / ( img->depth() -1 );

            points->at(index) = vl::fvec3(tx*scale.x() - scale.x()*0.5f, ty*scale.y() - scale.y()*0.5f, tz*scale.z() - scale.z()*0.5f);

            // compute normal
            int x1 = x-1;
            int x2 = x+1;
            int y1 = y-1;
            int y2 = y+1;
            int z1 = z-1;
            int z2 = z+1;
            x2 = x2 > img->width()  -1 ? img->width() -1 : x2;
            y2 = y2 > img->height() -1 ? img->height()-1 : y2;
            z2 = z2 > img->depth()  -1 ? img->depth() -1 : z2;
            x1 = x1 < 0 ? 0 : x1;
            y1 = y1 < 0 ? 0 : y1;
            z1 = z1 < 0 ? 0 : z1;
            float vx1 = img->pixels()[ x1 + y*img->pitch() + z*img->height()*img->pitch() ];
            float vx2 = img->pixels()[ x2 + y*img->pitch() + z*img->height()*img->pitch() ];
            float vy1 = img->pixels()[ x + y1*img->pitch() + z*img->height()*img->pitch() ];
            float vy2 = img->pixels()[ x + y2*img->pitch() + z*img->height()*img->pitch() ];
            float vz1 = img->pixels()[ x + y*img->pitch() + z1*img->height()*img->pitch() ];
            float vz2 = img->pixels()[ x + y*img->pitch() + z2*img->height()*img->pitch() ];

            vl::fvec3 normal(vx1-vx2, vy1-vy2, vz1-vz2);
            normal.normalize();
            norms->at(index) = normal;

            unsigned char r=0,g=0,b=0,a=0;
            transfer_func(img->pixels()[x + y*img->pitch() + z*img->height()*img->pitch()], r,g,b,a);
            color->at(index) = vl::ubvec4(r,g,b,a);

            draw_elements->indexBuffer()->at(index) = index;

            ++index;
          }
        }
      }
    }

    actor->setLod(0, geometry.get());
    geometry->setDisplayListEnabled(false);
    geometry->setBufferObjectEnabled(true);

    if (vl::Has_GL_ARB_vertex_buffer_object)
    {
      color->bufferObject()->setBufferData(vl::BU_STATIC_DRAW);
      points->bufferObject()->setBufferData(vl::BU_STATIC_DRAW);
      norms->bufferObject()->setBufferData(vl::BU_STATIC_DRAW);
      draw_elements->indexBuffer()->bufferObject()->setBufferData(vl::BU_DYNAMIC_DRAW);
    }

    eye_space_points->resize( points->size() );
  }
};

// Have fun!

BaseDemo* Create_App_PointSplatting() { return new App_PointSplatting; }
