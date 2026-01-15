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

// color blinking effect
class BlinkShaderAnimator: public vl::ShaderAnimator
{
public:
  void updateShader(vl::Shader* shader, vl::Camera*, vl::real cur_time)
  {
    int c = (int)( cur_time * 15.0 ) % 2;
    vl::fvec4 color;
    if (c == 0) 
      color = vl::gold;
    else
      color = vl::red;
    shader->gocMaterial()->setFlatColor( color );
  }
};

// texture roto-scale effect applied on the second texture unit
class TexRotScaleShaderAnimator: public vl::ShaderAnimator
{
public:
  void updateShader(vl::Shader* shader, vl::Camera*, vl::real cur_time)
  {
    vl::fmat4 mat;
    mat.translate(-0.5,-0.5,0.0f);
    mat.rotate( cur_time * 90, 0, 0, 1 );
    float s = 0.5f + (float)sin( cur_time * vl::fPi * 2.0f )*0.5f + 0.5f;
    mat.scale( s, s, s );
    mat.translate(+0.5,+0.5,0.0f);
    shader->gocTextureMatrix(1)->setMatrix( mat );
  }
};

// color fading effect
class MyShaderAnimator1: public vl::ShaderAnimator
{
public:
  void updateShader(vl::Shader* shader, vl::Camera* , vl::real cur_time)
  {
    float t = (float)sin( cur_time*vl::fPi*2.0f )*0.5f + 0.5f;
    vl::fvec4 col = vl::red*t + vl::blue*(1-t);
    shader->gocMaterial()->setFlatColor(col);
  }
};

/*
This demo shows how to use of the following techniques: shader LOD, shader animation, shader multipassing.
*/
class App_ShaderMultiPassLODAnim: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    // to be used later
    vl::ref<vl::Light> light = new vl::Light;
    vl::ref<vl::Texture> texture;

    // define LOD evaluator with 3 distance ranges: [0] --- 70 --- 100 --- [inf]
    vl::ref<vl::DistanceLODEvaluator> lod_eval = new vl::DistanceLODEvaluator;
    lod_eval->distanceRangeSet().push_back(70);
    lod_eval->distanceRangeSet().push_back(100);

    // texture roto-scaling effect
    vl::ref<vl::Shader> tex_rot_scale_sh = new vl::Shader;
    tex_rot_scale_sh->enable(vl::EN_DEPTH_TEST);
    tex_rot_scale_sh->enable(vl::EN_CULL_FACE);
    tex_rot_scale_sh->enable(vl::EN_LIGHTING);
    tex_rot_scale_sh->setRenderState( light.get(), 0 );
    texture = new vl::Texture("/images/holebox.tif", vl::TF_RGBA);
    tex_rot_scale_sh->gocTextureSampler(0)->setTexture( texture.get() );
    // on this unit we'll apply the roto-scale effect
    texture = new vl::Texture("/images/star2.tif", vl::TF_RGBA);
    tex_rot_scale_sh->gocTextureSampler(1)->setTexture( texture.get() );
    tex_rot_scale_sh->gocTextureSampler(1)->texture()->getTexParameter()->setWrapS(vl::TPW_REPEAT);
    tex_rot_scale_sh->gocTextureSampler(1)->texture()->getTexParameter()->setWrapT(vl::TPW_REPEAT);
    // install shader animator
    tex_rot_scale_sh->setShaderAnimator( new TexRotScaleShaderAnimator );

    // simple texture effect
    vl::ref<vl::Shader> tex_sh = new vl::Shader;
    tex_sh->enable(vl::EN_DEPTH_TEST);
    tex_sh->enable(vl::EN_CULL_FACE);
    tex_sh->enable(vl::EN_LIGHTING);
    tex_sh->setRenderState( light.get(), 0 );
    texture = new vl::Texture("/images/holebox.tif", vl::TF_RGBA);
    tex_sh->gocTextureSampler(0)->setTexture( texture.get() );

    // wireframe outline blinking shader
    vl::ref<vl::Shader> blink_sh = new vl::Shader;
    blink_sh->enable(vl::EN_DEPTH_TEST);
    blink_sh->enable(vl::EN_CULL_FACE);
    blink_sh->enable(vl::EN_LIGHTING);
    blink_sh->setRenderState( light.get(), 0 );
    blink_sh->gocPolygonMode()->set(vl::PM_LINE,vl::PM_LINE);
    blink_sh->gocFrontFace()->set(vl::FF_CW);
    blink_sh->gocLineWidth()->set(3.0f);
    // install shader animator
    blink_sh->setShaderAnimator(new BlinkShaderAnimator);

    // define the effect with 3 lods
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->setLOD( 0, tex_rot_scale_sh.get(), blink_sh.get() ); // note multi-pass
    effect->setLOD( 1, tex_sh.get(), blink_sh.get() ); // note multi-pass
    effect->setLOD( 2, tex_sh.get() );

    // install LOD evaluator
    effect->setLODEvaluator(lod_eval.get());

    // generate template geometry
    vl::ref<vl::Geometry> box = vl::makeBox( vl::vec3(0,0,0), 5,5,5);
    box->computeNormals();
    // use the same texture coordinates for unit #0 and unit #1
    box->setTexCoordArray(1, box->texCoordArray(0));

    // form a ring with cubes
    for(int i=0; i<24; ++i)
    {
      vl::ref<vl::Transform> tr = new vl::Transform;
      rendering()->as<vl::Rendering>()->transform()->addChild(tr.get());
      vl::mat4 mat;
      mat.translate(30,0,0);
      mat.rotate( 360.0f / 20.0f * i, 0, 1, 0);
      tr->setLocalMatrix( mat );
      sceneManager()->tree()->addActor(box.get(), effect.get(), tr.get());
    }

    // add central cube
    sceneManager()->tree()->addActor(box.get(), effect.get(), new vl::Transform);
  }

  // animate the scene
  void updateScene()
  {
    vl::real t = sin( vl::Time::currentTime() * vl::fPi * 2.0f / 8.0f ) * 0.5f + 0.5f;
    vl::vec3 eye( 130*t+5, t*20+5, 0 );
    eye = vl::mat4::getRotation( vl::Time::currentTime() * 15.0f, 0, 1, 0 ) * eye;
    vl::mat4 m = vl::mat4::getLookAt( eye, vl::vec3(0,0,0), vl::vec3(0,1,0) );
    rendering()->as<vl::Rendering>()->camera()->setViewMatrix(m);
  }
};

// Have fun!

BaseDemo* Create_App_ShaderMultiPassLODAnim() { return new App_ShaderMultiPassLODAnim(); }
