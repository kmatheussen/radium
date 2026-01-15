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
#include <vlGraphics/Text.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/FontManager.hpp>

using namespace vl;

class App_Fractals: public BaseDemo
{
public:
  void mouseDownEvent(EMouseButton btn, int x, int y)
  {
    if (mMode != NoMode)
      return;
    else
    if (btn == LeftButton)
      mMode = TranslateMode;
    else
    if (btn == RightButton)
      mMode = ZoomMode;

    mMouseStartPos.x() = x;
    mMouseStartPos.y() = y;
    mStartZoom         = mZoom;
    mStartCenter.x()   = mXcenter;
    mStartCenter.y()   = mYcenter;
  }

  void mouseUpEvent(EMouseButton btn, int /*x*/, int /*y*/)
  {
    if (btn == LeftButton && mMode == TranslateMode)
      mMode = NoMode;
    else
    if (btn == RightButton && mMode == ZoomMode)
      mMode = NoMode;
  }

  void mouseMoveEvent(int x, int y)
  {
    if (mMode == ZoomMode)
    {
      float delta = 1.0f - (y - mMouseStartPos.y()) / 200.0f;
      mZoom = mStartZoom * delta;
      if (mZoom < 0.00001f)
        mZoom = 0.00001f;
      openglContext()->update();
    }
    else
    if (mMode == TranslateMode)
    {
      float deltax = 0.5f * mZoom * (x - mMouseStartPos.x()) / rendering()->as<Rendering>()->camera()->viewport()->width();
      float deltay = 0.5f * mZoom * (y - mMouseStartPos.y()) / rendering()->as<Rendering>()->camera()->viewport()->height();
      mXcenter = mStartCenter.x() - deltax;
      mYcenter = mStartCenter.y() + deltay;
      openglContext()->update();
    }
    updateText();
  }

  void keyPressEvent(unsigned short ch, EKey key)
  {
    BaseDemo::keyPressEvent(ch, key);
    if (key == Key_F7)
      generateAndSaveFractalViaCPU();
    else
    if (key == Key_F9)
    {
      mTextureToggle = !mTextureToggle;
      mTexture->setMipLevel( 0, mTextureToggle ? mSpectrum1.get() : mSpectrum2.get(), false );
    }
    else
    if (key == Key_H)
      mHelpOn = !mHelpOn;
    updateText();
  }

  virtual void updateScene()
  {
    mGLSLProgram->useProgram();
    glUniform1f(mGLSLProgram->getUniformLocation("ColorOffset"), mColorOffset = (float)fract(Time::currentTime() * 0.5) );
    glUniform1f(mGLSLProgram->getUniformLocation("Zoom"),    mZoom);
    glUniform1f(mGLSLProgram->getUniformLocation("Xcenter"), mXcenter);
    glUniform1f(mGLSLProgram->getUniformLocation("Ycenter"), mYcenter);
  }

  void updateText()
  {
    if (mHelpOn)
    {
      mText->setText(
        "HELP:\n"
        "\n"
        "<H>  to toggle this help\n"
        "<F1> toggle fullscreen mode\n"
        "<F5> take a screenshot of the window\n"
        "<F7> take a screenshot of the fractal (CPU computed)\n"
        "<F9> toggle color palette\n"
        "<Left Button> translates the view\n"
        "<Right Button> zooms the view\n"
      );
      mText->setAlignment( AlignVCenter | AlignHCenter );
      mText->setViewportAlignment( AlignVCenter| AlignHCenter );
    }
    else
    {
      String text = Say("coords = %.8n %.8n; zoom = %.7n") << mXcenter << mYcenter << mZoom;
      mText->setText(text);
      mText->setAlignment( AlignBottom | AlignHCenter );
      mText->setViewportAlignment( AlignBottom | AlignHCenter );
    }
  }

  void initEvent()
  {
    if (!Has_GLSL)
    {
      Log::error("OpenGL Shading Language not supported.\n");
      Time::sleep(2000);
      exit(1);
    }

    vl::Log::notify(appletInfo());

    mMode = NoMode;
    mHelpOn = true;
    mTextureToggle = true;

    mText = new Text;
    mText->setText("Mandelbrot!");
    mText->setFont( defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 10) );
    mText->setMode( Text2D );
    mText->setColor(vl::white);
    mText->setBackgroundColor( fvec4(0,0,0,0.75f) );
    mText->setBackgroundEnabled(true);
    mText->setAlignment( AlignBottom | AlignLeft );
    mText->setViewportAlignment( AlignBottom | AlignLeft );
    mText->translate(10,10,0);
    ref<Effect> text_effect = new Effect;
    text_effect->shader()->enable(EN_BLEND);
    Actor* text_actor = sceneManager()->tree()->addActor( mText.get(), text_effect.get() );
    text_actor->setRenderRank(1);

    updateText();

    /*CPU energy saving off*/
    openglContext()->setContinuousUpdate(true);

    /* bind Effect */
    ref<Effect> effect = new Effect;

    /* screen aligned quad */
    ref<Geometry> geom = vl::makeGrid( vec3(0,0,0), 2, 2, 2, 2, true, fvec2(0,0), fvec2(1,1) );
    geom->transform( mat4::getRotation( -90, 1,0,0 ) );
    sceneManager()->tree()->addActor( geom.get(), effect.get() );

    // camera setup
    rendering()->as<Rendering>()->camera()->setProjectionMatrix( mat4::getIdentity(), vl::PMT_UserProjection );
    rendering()->as<Rendering>()->camera()->setModelingMatrix( mat4::getIdentity() );

    // disable trackball and ghost camera manipulator
    trackball()->setEnabled(false);
    ghostCameraManipulator()->setEnabled(false);

    /* fractal variables variables */

    mMaxIterations = 1500;

    // normal
    #if 0
      mZoom    = 1.0f;
      mXcenter = -1.36f;
      mYcenter = 0.0f;
    #elif 1
      // tendrilis
      mZoom = 0.15f;
      mXcenter = -0.0002f;
      mYcenter = 0.7383f;
    #else
      // minimandelbrot
      mZoom = 0.15f;
      mXcenter = -1.75f;
      mYcenter = 0.0f;
    #endif

    // color palette 1
    std::vector<fvec4> color_range1;
    color_range1.push_back(vl::red);
    color_range1.push_back(vl::yellow);
    color_range1.push_back(vl::green);
    color_range1.push_back(vl::blue);
    color_range1.push_back(vl::red);
    mSpectrum1 = vl::makeColorSpectrum(256,color_range1);

    // color palette 2
    std::vector<fvec4> color_range2;
    color_range2.push_back(vl::yellow);
    color_range2.push_back(vl::royalblue);
    color_range2.push_back(vl::yellow);
    mSpectrum2 = vl::makeColorSpectrum(256,color_range2);

    mTexture = new Texture;
    mTexture->prepareTexture1D( mSpectrum1.get(), TF_RGBA, false, false );
    mTexture->getTexParameter()->setMagFilter(TPF_NEAREST);
    mTexture->getTexParameter()->setMinFilter(TPF_NEAREST);
    effect->shader()->gocTextureSampler(0)->setTexture( mTexture.get() );

    mGLSLProgram = effect->shader()->gocGLSLProgram();
    mGLSLProgram->attachShader( new GLSLVertexShader("/glsl/mandelbrot.vs") );
    mGLSLProgram->attachShader( new GLSLFragmentShader("/glsl/mandelbrot.fs") );
    mGLSLProgram->linkProgram();
    mGLSLProgram->useProgram();
    glUniform1f(mGLSLProgram->getUniformLocation("ColorOffset"), mColorOffset = (float)fract(Time::currentTime() * 0.5) );
    glUniform3f(mGLSLProgram->getUniformLocation("InnerColor"),  0,0,0 );
    glUniform1f(mGLSLProgram->getUniformLocation("Zoom"),    mZoom);
    glUniform1f(mGLSLProgram->getUniformLocation("Xcenter"), mXcenter);
    glUniform1f(mGLSLProgram->getUniformLocation("Ycenter"), mYcenter);
    glUniform1f(mGLSLProgram->getUniformLocation("MaxIterations"), mMaxIterations);
    VL_CHECK_OGL();
  }

  void resizeEvent(int w, int h)
  {
    rendering()->as<Rendering>()->camera()->viewport()->setWidth(w);
    rendering()->as<Rendering>()->camera()->viewport()->setHeight(h);
  }

  void generateAndSaveFractalViaCPU()
  {
    // compute Mandelbrot on the CPU

    Image* spectrum = mTextureToggle ? mSpectrum1.get() : mSpectrum2.get();

    int pic_w = rendering()->as<Rendering>()->camera()->viewport()->width();
    int pic_h = rendering()->as<Rendering>()->camera()->viewport()->height();

    ref< Image > image = new Image;
    image->allocate2D(pic_w,pic_h,4,IF_RGBA, IT_UNSIGNED_BYTE);

    double t1 = (double)Time::currentTime();

    for(int y=0; y<pic_h; ++y)
    {
      for(int x=0; x<pic_w; ++x)
      {
        double tx = ((x / (double)image->width()) - 0.5f) * 0.5f;
        double ty = ((y / (double)image->height()) - 0.5f) * 0.5f;

        double real = tx * mZoom + mXcenter;
        double imag = ty * mZoom + mYcenter;
        double Creal = real;
        double Cimag = imag;

        double r2 = 0.0f;
        double iter;

        for( iter = 0.0f; iter < mMaxIterations && r2 < 4.0f; ++iter )
        {
          double tempreal = real;
          real = (tempreal * tempreal) - (imag * imag) + Creal;
          imag = 2.0f * tempreal * imag + Cimag;
          r2 = (real*real) + (imag*imag);
        }

        // base the color on the number of iterations

        if (r2 < 4.0f)
        {
          image->pixels()[x*4 + image->pitch()*y + 0] = 0;
          image->pixels()[x*4 + image->pitch()*y + 1] = 0;
          image->pixels()[x*4 + image->pitch()*y + 2] = 0;
          image->pixels()[x*4 + image->pitch()*y + 3] = 255;
        }
        else
        {
          int px_s = int(spectrum->width() * fract(iter*0.01f+mColorOffset));
          image->pixels()[x*4 + image->pitch()*y + 0] = spectrum->pixels()[px_s*4 + 0];
          image->pixels()[x*4 + image->pitch()*y + 1] = spectrum->pixels()[px_s*4 + 1];
          image->pixels()[x*4 + image->pitch()*y + 2] = spectrum->pixels()[px_s*4 + 2];
          image->pixels()[x*4 + image->pitch()*y + 3] = spectrum->pixels()[px_s*4 + 3];
        }
      }
    }

    double t2 = (double)Time::currentTime();

    Log::print(Say("CPU mandelbrot fractal generation time = %n\n") << t2-t1);

    saveImage( image.get(), Say("Mandelbrot Fractal %.0n.tif") << Time::currentTime()*10 );
  }

protected:
  ivec3 mMouseStartPos;
  enum { NoMode, TranslateMode, ZoomMode } mMode;
  float    mStartZoom;
  fvec3 mStartCenter;
  float mColorOffset;
  float mZoom;
  float mXcenter;
  float mYcenter;
  float mMaxIterations;
  ref<Text> mText;
  ref<Image> mSpectrum1;
  ref<Image> mSpectrum2;
  bool mHelpOn;
  bool mTextureToggle;
  ref<Texture> mTexture;

  ref<GLSLProgram> mGLSLProgram;
};

// Have fun!

BaseDemo* Create_App_Fractals() { return new App_Fractals; }
