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
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>

class App_GLSLImageProcessing: public BaseDemo
{
public:
  virtual void updateScene()
  {
    if (mTimer.elapsed() > 3)
    {
      mTimer.start();
      ++mTest;
      updateEffect();
    }
  }

  virtual void initEvent()
  {
    if (!vl::Has_GLSL)
    {
      vl::Log::error("OpenGL Shading Language not supported.\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    vl::Log::notify(appletInfo());

    /*openglContext()->setContinuousUpdate(false);*/

    mTest = 0;

    mEffectNames[0] = "No effect";
    mEffectNames[1] = "Blur";
    mEffectNames[2] = "Sharpen";
    mEffectNames[3] = "Edge";
    mEffectNames[4] = "Emboss";
    mEffectNames[5] = "Grayscale";
    mEffectNames[6] = "Contrast";
    mEffectNames[7] = "Brighten";
    mEffectNames[8] = "Darken";

    // camera setup
    rendering()->as<vl::Rendering>()->setNearFarClippingPlanesOptimized(false);
    rendering()->as<vl::Rendering>()->camera()->setProjectionOrtho(-0.5f);
    rendering()->as<vl::Rendering>()->camera()->setModelingMatrix( vl::mat4() );

    // disable trackball and ghost camera manipulator
    trackball()->setEnabled(false);
    ghostCameraManipulator()->setEnabled(false);

    // dummy empty image
    mImage = new vl::Image(5,5,0, 1, vl::IF_RGBA, vl::IT_UNSIGNED_BYTE);
    memset(mImage->pixels(), 0xFF, mImage->requiredMemory());

    // texture setup
    mTexture = new vl::Texture;
    mTexture->getTexParameter()->setWrapS(vl::TPW_CLAMP);
    mTexture->getTexParameter()->setWrapT(vl::TPW_CLAMP);
    mTexture->getTexParameter()->setMinFilter(vl::TPF_LINEAR);
    mTexture->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    mTexture->prepareTexture2D(mImage.get(), vl::TF_RGBA);
    mTextureMatrix = new vl::TextureMatrix;

    vl::ref<vl::Effect> postproc_fx = new vl::Effect;
    postproc_fx->shader()->gocTextureSampler(0)->setTexture(mTexture.get());
    postproc_fx->shader()->setRenderState( mTextureMatrix.get(), 0 );

    vl::ref<vl::Effect> original_fx = new vl::Effect;
    original_fx->shader()->gocTextureSampler(0)->setTexture(mTexture.get());

    mGLSLProgram = postproc_fx->shader()->gocGLSLProgram();
    mGLSLProgram->attachShader( new vl::GLSLFragmentShader("/glsl/image_processing.fs") );

    VL_CHECK_OGL()

    mGridEffect = vl::makeGrid( vl::vec3(0,0,0), 1.0f, 1.0f, 4, 4, true, vl::fvec2(0,0), vl::fvec2(1,1) );
    mGridEffect->transform(vl::mat4::getRotation(-90,1,0,0));

    mGridOriginal = vl::makeGrid( vl::vec3(0.25f,0,0), 0.5f, 1.0f, 4, 4, true, vl::fvec2(0,0), vl::fvec2(0.5f,1) );
    mGridOriginal->transform(vl::mat4::getRotation(-90,1,0,0));
    mGridOriginal->texCoordArray(0)->transform( vl::mat4::getTranslation(0.5f, 0,0) );

    mTransform = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild(mTransform.get());
    vl::Actor* effect_act = sceneManager()->tree()->addActor(mGridEffect.get(), postproc_fx.get(), mTransform.get());
    effect_act->setRenderRank(0);
    vl::Actor* original_act = sceneManager()->tree()->addActor(mGridOriginal.get(), original_fx.get(), mTransform.get());
    original_act->setRenderRank(1);

    // text
    mText = new vl::Text;
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 10) );
    mText->setAlignment( vl::AlignHCenter | vl::AlignBottom );
    mText->setViewportAlignment( vl::AlignHCenter | vl::AlignBottom );
    mText->translate(0,5,0);
    mText->setColor(vl::white);
    mText->setBackgroundColor(vl::fvec4(0,0,0,.75f));
    mText->setBackgroundEnabled(true);
    updateText();

    vl::ref<vl::Effect> txt_fx = new vl::Effect;
    txt_fx->shader()->enable(vl::EN_BLEND);
    vl::Actor* txt_act = sceneManager()->tree()->addActor(mText.get(), txt_fx.get());
    // draw the text for last
    txt_act->setRenderRank(2);

    // start timer
    mTimer.start();

    viewImage("/images/toy.jpg");
  }

  void updateText()
  {
    mText->setText( mEffectNames[mTest] );
  }

  void fileDroppedEvent(const std::vector<vl::String>& files)
  {
    viewImage(files[0]);
  }

  void viewImage(const vl::String& file)
  {
    mImage = vl::loadImage(file);

    mTexture->prepareTexture2D(mImage.get(), vl::TF_RGBA);

    // perfectly center the texture texels (see GL_CLAMP documentation)
    vl::fmat4 m;
    float x_texel = 1.0f/mImage->width();
    float y_texel = 1.0f/mImage->height();
    float x_scale = 1.0f - x_texel;
    float y_scale = 1.0f - y_texel;
    m.scale(x_scale, y_scale, 1.0f);
    m.translate(x_texel/2.0f, y_texel/2.0f, 0.0f);
    mTextureMatrix->setMatrix(m);

    // set "image_width" uniform
    vl::ref<vl::Uniform> image_width = mGLSLProgram->gocUniform("image_width");
    image_width->setUniformF((float)mImage->width());
    // set "image_height" uniform
    vl::ref<vl::Uniform> image_height = mGLSLProgram->gocUniform("image_height");
    image_height->setUniformF((float)mImage->width());

    int w = rendering()->as<vl::Rendering>()->renderer()->framebuffer()->width();
    int h = rendering()->as<vl::Rendering>()->renderer()->framebuffer()->height();
    resizeEvent( w, h );
    mTimer.start();
    mTest = 0;
    updateEffect();
  }

  void resizeEvent(int w, int h)
  {
    vl::Camera* camera = rendering()->as<vl::Rendering>()->camera();
    camera->viewport()->setWidth(w);
    camera->viewport()->setHeight(h);
    camera->setProjectionOrtho(-0.5f);

    if (mImage)
    {
      vl::mat4 m;
      m.translate(w/2.0f, h/2.0f, 0.0f);

      float x_scaling = (float)w / mImage->width();
      float y_scaling = (float)h / mImage->height();
      float scaling   = x_scaling < y_scaling ? x_scaling : y_scaling;

      m = m * vl::mat4::getScaling(scaling*mImage->width(), scaling*mImage->height(), scaling);
      mTransform->setLocalMatrix(m);
    }
  }

  void keyPressEvent(unsigned short, vl::EKey key)
  {
    if (key == vl::Key_Right)
      ++mTest;
    else
    if (key == vl::Key_Left)
      --mTest;

    updateEffect();

    mTimer.start();
  }

  void updateEffect()
  {
    if (mTest == -1)
      mTest = mTestCount-1;
    if (mTest == mTestCount)
      mTest = 0;
    // set "test" uniform
    vl::ref<vl::Uniform> test_uni = mGLSLProgram->gocUniform("test");
    test_uni->setUniformI(mTest);
    updateText();
  }

protected:
  int mTest;
  vl::ref<vl::GLSLProgram> mGLSLProgram;
  vl::ref<vl::Geometry> mGridEffect;
  vl::ref<vl::Geometry> mGridOriginal;
  vl::ref<vl::Image> mImage;
  vl::ref<vl::Texture> mTexture;
  vl::ref<vl::TextureMatrix> mTextureMatrix;
  vl::ref<vl::Transform> mTransform;
  vl::ref<vl::Text> mText;
  vl::Time mTimer;
  static const int mTestCount = 9;
  const char* mEffectNames[mTestCount];
};

// Have fun!

BaseDemo* Create_App_GLSLImageProcessing() { return new App_GLSLImageProcessing; }
