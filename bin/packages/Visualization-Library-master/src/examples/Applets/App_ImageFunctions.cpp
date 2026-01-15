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
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlCore/plugins/ioPNG.hpp>
#include <vlCore/plugins/ioJPG.hpp>

class App_ImageFunctions: public BaseDemo
{
public:
  virtual void initEvent()
  {
    if ( !vl::canLoad("png") || !vl::canLoad("jpg") || !vl::canLoad("tga") || !vl::canLoad("tif") )
    {
      vl::Log::error("App_ImageFunctions test requires the following modules: VL_IO_2D_JPG, VL_IO_2D_PNG, VL_IO_2D_TIFF and VL_IO_2D_TGA.\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    vl::Log::notify(appletInfo());

    bool use_mipmaps = true;

    vl::ref<vl::Image> img1 = vl::loadImage("/images/holebox.tif");
    // make sure it's in the right format
    img1 = img1->convertFormat(vl::IF_RGBA)->convertType(vl::IT_UNSIGNED_BYTE);
    vl::ref<vl::Image> img2;

    // check load/save routines
    vl::ref<vl::Image> img_a;
    vl::ref<vl::Image> img_b;
    vl::ref<vl::Image> img_c;
    vl::ref<vl::Image> img_d;

    vl::LoadWriterJPG* jpg_load_writer = vl::defLoadWriterManager()->loadWriter<vl::LoadWriterJPG>();
    if (jpg_load_writer)
    {
      jpg_load_writer->setQuality(90);
      vl::saveImage(img1.get(), "img_a.jpg");
      img_a = vl::loadImage("img_a.jpg");
    }

    vl::LoadWriterPNG* png_load_writer = vl::defLoadWriterManager()->loadWriter<vl::LoadWriterPNG>();
    if (png_load_writer)
    {
      png_load_writer->setCompression(9);
      vl::saveImage(img1.get(), "img_b.png");
      img_b = vl::loadImage("img_b.png");
    }

    vl::saveImage(img1.get(), "img_c.tga");
    img_c = vl::loadImage("img_c.tga");

    vl::saveImage(img1.get(), "img_d.tif");
    img_d = vl::loadImage("img_d.tif");

    VL_CHECK(img_a)
    VL_CHECK(img_b)
    VL_CHECK(img_c)
    VL_CHECK(img_d)

    // test image nearest sampling IF_RGBA/IT_UNSIGNED_BYTE
    vl::ref<vl::Image> img_x = new vl::Image;
    // copy image
    *img_x = *img1;
    // set it to black
    memset(img_x->pixels(), 0, img_x->requiredMemory());
    for(int x=0; x<img_x->width(); ++x)
    {
      for(int y=0; y<img_x->height(); ++y)
      {
        vl::fvec4 color = img1->sample(x,y);
        unsigned char* pixel = img_x->pixels() + img_x->pitch()*y + x*4;
        pixel[0] = (unsigned char)(color.r() * 255.0);
        pixel[1] = (unsigned char)(color.g() * 255.0);
        pixel[2] = (unsigned char)(color.b() * 255.0);
        pixel[3] = (unsigned char)(color.a() * 255.0);
      }
    }
    // test image linear sampling IF_RGBA/IT_UNSIGNED_BYTE
    vl::ref<vl::Image> img_y = new vl::Image;
    // copy image

    *img_y = *img1;
    // set it to black
    memset(img_y->pixels(), 0, img_y->requiredMemory());
    for(int x=0; x<img_y->width(); ++x)
    {
      for(int y=0; y<img_y->height(); ++y)
      {
        vl::fvec4 color = img1->sampleLinear((float)x,(float)y);
        unsigned char* pixel = img_y->pixels() + img_y->pitch()*y + x*4;
        pixel[0] = (unsigned char)(color.r() * 255.0);
        pixel[1] = (unsigned char)(color.g() * 255.0);
        pixel[2] = (unsigned char)(color.b() * 255.0);
        pixel[3] = (unsigned char)(color.a() * 255.0);
      }
    }

    // check image type conversion
    #if 1
      img2 = img1
      ->convertType(vl::IT_FLOAT)
      ->convertType(vl::IT_SHORT)
      ->convertType(vl::IT_INT)
      ->convertType(vl::IT_BYTE)
      ->convertType(vl::IT_UNSIGNED_BYTE)
      ->convertType(vl::IT_UNSIGNED_SHORT)
      ->convertType(vl::IT_UNSIGNED_INT)
     ;
    #endif

    // check image format conversion
    #if 1
      img2 = img1
        ->convertFormat(vl::IF_LUMINANCE_ALPHA)
        ->convertFormat(vl::IF_BGRA)
        ->convertFormat(vl::IF_RGBA)
        ->convertFormat(vl::IF_LUMINANCE_ALPHA)
     ;
    #endif

    // test image nearest sampling IF_LUMINANCE_ALPHA/IT_UNSIGNED_INT
    vl::ref<vl::Image> img_z = new vl::Image;
    // copy image
    *img_z = *img2;
    // set it to black
    memset(img_z->pixels(), 0, img_z->requiredMemory());
    for(int x=0; x<img_z->width(); ++x)
    {
      for(int y=0; y<img_z->height(); ++y)
      {
        vl::fvec4 color = img2->sample(x,y);
        unsigned char* pixel = img_z->pixels() + img_z->pitch()*y + x*2;
        pixel[0] = (unsigned char)(color.r() * 255.0);
        pixel[1] = (unsigned char)(color.a() * 255.0);
      }
    }

    // test image linear sampling IF_LUMINANCE_ALPHA/IT_UNSIGNED_INT
    vl::ref<vl::Image> img_w = new vl::Image;
    // copy image
    *img_w = *img2;
    // set it to black
    memset(img_w->pixels(), 0, img_w->requiredMemory());
    for(int x=0; x<img_w->width(); ++x)
    {
      for(int y=0; y<img_w->height(); ++y)
      {
        vl::fvec4 color = img2->sampleLinear((float)x,(float)y);
        unsigned char* pixel = img_w->pixels() + img_w->pitch()*y + x*2;
        pixel[0] = (unsigned char)(color.r() * 255.0);
        pixel[1] = (unsigned char)(color.a() * 255.0);
      }
    }

    vl::ref<vl::Geometry> quad = vl::makeGrid( vl::vec3(0,0,0), 10, 10, 2, 2, true, vl::fvec2(0,0), vl::fvec2(1,1) );
    quad->transform( vl::mat4::getRotation(-90, 1,0,0), false );

    // (1)
    // transform
    vl::ref<vl::Transform> tr_1 = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_1.get());
    tr_1->setLocalMatrix(vl::mat4::getTranslation(-5,0,0));
    // effect
    vl::ref<vl::Effect> fx_1 = new vl::Effect;
    fx_1->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_1 = new vl::Texture( img1.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_1->shader()->gocTextureSampler(0)->setTexture( texture_1.get() );
    fx_1->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_1->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_1.get(), tr_1.get() );

    // (2)
    // transform
    vl::ref<vl::Transform> tr_2 = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_2.get());
    tr_2->setLocalMatrix(vl::mat4::getTranslation(+5,0,0));
    // effect
    vl::ref<vl::Effect> fx_2 = new vl::Effect;
    fx_2->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_2 = new vl::Texture( img2.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_2->shader()->gocTextureSampler(0)->setTexture( texture_2.get() );
    fx_2->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_2->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_2.get(), tr_2.get() );

    // (x)
    // transform
    vl::ref<vl::Transform> tr_x = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_x.get());
    tr_x->setLocalMatrix(vl::mat4::getTranslation(-2.5-5,+7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_x = new vl::Effect;
    fx_x->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_x = new vl::Texture( img_x.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_x->shader()->gocTextureSampler(0)->setTexture( texture_x.get() );
    fx_x->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_x->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_x.get(), tr_x.get() );

    // (y)
    // transform
    vl::ref<vl::Transform> tr_y = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_y.get());
    tr_y->setLocalMatrix(vl::mat4::getTranslation(-2.5,+7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_y = new vl::Effect;
    fx_y->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_y = new vl::Texture( img_y.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_y->shader()->gocTextureSampler(0)->setTexture( texture_y.get() );
    fx_y->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_y->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_y.get(), tr_y.get() );

    // (z)
    // transform
    vl::ref<vl::Transform> tr_z = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_z.get());
    tr_z->setLocalMatrix(vl::mat4::getTranslation(+2.5,+7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_z = new vl::Effect;
    fx_z->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_z = new vl::Texture( img_z.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_z->shader()->gocTextureSampler(0)->setTexture( texture_z.get() );
    fx_z->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_z->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_z.get(), tr_z.get() );

    // (w)
    // transform
    vl::ref<vl::Transform> tr_w = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_w.get());
    tr_w->setLocalMatrix(vl::mat4::getTranslation(+2.5+5.0,+7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_w = new vl::Effect;
    fx_w->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_w = new vl::Texture( img_w.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_w->shader()->gocTextureSampler(0)->setTexture( texture_w.get() );
    fx_w->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_w->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_w.get(), tr_w.get() );

    // (a)
    // transform
    vl::ref<vl::Transform> tr_a = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_a.get());
    tr_a->setLocalMatrix(vl::mat4::getTranslation(-2.5-5,-7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_a = new vl::Effect;
    fx_a->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_a = new vl::Texture( img_a.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_a->shader()->gocTextureSampler(0)->setTexture( texture_a.get() );
    fx_a->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_a->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_a.get(), tr_a.get() );

    // (b)
    // transform
    vl::ref<vl::Transform> tr_b = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_b.get());
    tr_b->setLocalMatrix(vl::mat4::getTranslation(-2.5,-7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_b = new vl::Effect;
    fx_b->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_b = new vl::Texture( img_b.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_b->shader()->gocTextureSampler(0)->setTexture( texture_b.get() );
    fx_b->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_b->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_b.get(), tr_b.get() );

    // (c)
    // transform
    vl::ref<vl::Transform> tr_c = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_c.get());
    tr_c->setLocalMatrix(vl::mat4::getTranslation(+2.5,-7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_c = new vl::Effect;
    fx_c->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_c = new vl::Texture( img_c.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_c->shader()->gocTextureSampler(0)->setTexture( texture_c.get() );
    fx_c->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_c->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_c.get(), tr_c.get() );

    // (d)
    // transform
    vl::ref<vl::Transform> tr_d = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(tr_d.get());
    tr_d->setLocalMatrix(vl::mat4::getTranslation(+2.5+5,-7.5,0) * vl::mat4::getScaling(0.5f,0.5f,1.0f));
    // effect
    vl::ref<vl::Effect> fx_d = new vl::Effect;
    fx_d->shader()->enable(vl::EN_BLEND);
    // setup texture
    vl::ref<vl::Texture> texture_d = new vl::Texture( img_d.get(), vl::TF_RGBA, use_mipmaps, false );
    fx_d->shader()->gocTextureSampler(0)->setTexture( texture_d.get() );
    fx_d->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    fx_d->shader()->gocTextureSampler(0)->texture()->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);
    // add actor
    sceneManager()->tree()->addActor( quad.get(), fx_d.get(), tr_d.get() );
  }
};

// Have fun!

BaseDemo* Create_App_ImageFunctions() { return new App_ImageFunctions; }
