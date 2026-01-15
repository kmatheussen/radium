/*************************************************************************************/
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
/*************************************************************************************/

#include "BaseDemo.hpp"
#include <vlVolume/SlicedVolume.hpp>
#include <vlVolume/VolumeUtils.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

using namespace vl;

/* ----- sliced volume visualization settings ----- */

/* Use OpenGL Shading Language to render the volume. */
static bool USE_GLSL = true;

/* If enabled, renders the volume using 3 animated lights. Requires USE_GLSL. */
static bool DYNAMIC_LIGHTS = true; 

/* If enabled, a white transfer function is used and 3 colored lights 
   are used to render the volume. Used only if DYNAMIC_LIGHTS is true. */
static bool COLORED_LIGHTS = true;

/* Use a separate 3d texture with a precomputed gradient to speedup the fragment shader.
   Requires more memory (for the gradient texture) but can speedup the rendering. */
static bool PRECOMPUTE_GRADIENT = false; // only if USE_GLSL is true.

/* The number of slices used to render the volume, the higher the number the better 
  (and slower) the rendering will be. */
static const int  SLICE_COUNT = 1000;

/* Our applet used to render and interact with the volume. */
class App_VolumeSliced: public BaseDemo
{
public:

  virtual String appletInfo()
  {
    return BaseDemo::appletInfo() + 
    "- Use the mouse wheel to change the bias used to render the volume.\n" +
    "- Drop inside the window a set of 2D files or a DDS or DAT volume to display it.\n" +
    "\n";
  }

  /* initialize the applet with a default volume */
  virtual void initEvent()
  {
    Log::notify(appletInfo());
    
    if (!Has_Texture_3D && !Has_GLSL)
    {
      Log::error("This test requires 3D texture or GLSL support!\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    // variable preconditions
    USE_GLSL &= Has_GLSL;
    DYNAMIC_LIGHTS &= USE_GLSL;
    COLORED_LIGHTS &= DYNAMIC_LIGHTS;
    PRECOMPUTE_GRADIENT &= USE_GLSL;

    // lights to be used later
    mLight0 = new Light;
    mLight1 = new Light;
    mLight2 = new Light;

    // you can color the lights!
    if (DYNAMIC_LIGHTS && COLORED_LIGHTS)
    {
      mLight0->setAmbient(fvec4(0.1f, 0.1f, 0.1f, 1.0f));
      mLight1->setAmbient(fvec4(0.0f, 0.0f, 0.0f, 1.0f));
      mLight2->setAmbient(fvec4(0.0f, 0.0f, 0.0f, 1.0f));
      mLight0->setDiffuse(vl::gold);
      mLight1->setDiffuse(vl::green);
      mLight2->setDiffuse(vl::royalblue);
    }

    // light bulbs
    if (DYNAMIC_LIGHTS)
    {
      mLight0Tr = new Transform;
      mLight1Tr = new Transform;
      mLight2Tr = new Transform;
      rendering()->as<Rendering>()->transform()->addChild( mLight0Tr.get() );
      rendering()->as<Rendering>()->transform()->addChild( mLight1Tr.get() );
      rendering()->as<Rendering>()->transform()->addChild( mLight2Tr.get() );
      mLight0->bindTransform( mLight0Tr.get() );
      mLight1->bindTransform( mLight1Tr.get() );
      mLight2->bindTransform( mLight2Tr.get() );

      ref<Effect> fx_bulb = new Effect;
      fx_bulb->shader()->enable(EN_DEPTH_TEST);
      ref<Geometry> light_bulb = vl::makeIcosphere(vec3(0,0,0),1,1);
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight0Tr.get() );
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight1Tr.get() );
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight2Tr.get() );
    }

    ref<Effect> vol_fx = new Effect;
    vol_fx->shader()->enable(EN_DEPTH_TEST);
    vol_fx->shader()->enable(EN_BLEND);
    vol_fx->shader()->setRenderState( mLight0.get(), 0 );
    // add the other lights only if dynamic lights have to be displayed
    if (DYNAMIC_LIGHTS)
    {
      vol_fx->shader()->setRenderState( mLight1.get(), 1 );
      vol_fx->shader()->setRenderState( mLight2.get(), 2 );
    }

    // The GLSL program used to perform the actual rendering.
    // The \a volume_luminance_light.fs fragment shader allows you to specify how many 
    // lights to use (up to 4) and can optionally take advantage of a precomputed 
    // normals texture.
    if (USE_GLSL)
    {
      mGLSL = vol_fx->shader()->gocGLSLProgram();
      mGLSL->attachShader( new GLSLFragmentShader("/glsl/volume_luminance_light.fs") );
      mGLSL->attachShader( new GLSLVertexShader("/glsl/volume_luminance_light.vs") );
    }

    // transform and trackball setup
    mVolumeTr = new Transform;
    trackball()->setTransform( mVolumeTr.get() );

    // volume actor
    mVolumeAct = new Actor;
    mVolumeAct->setEffect( vol_fx.get() );
    mVolumeAct->setTransform(mVolumeTr.get());
    sceneManager()->tree()->addActor( mVolumeAct.get() );

    // sliced volume: will generate the actual actor's geometry on the fly.
    mSlicedVolume = new vl::SlicedVolume;
    mSlicedVolume->bindActor(mVolumeAct.get());
    mSlicedVolume->setSliceCount(SLICE_COUNT);
    AABB volume_box( vec3(-10,-10,-10), vec3(+10,+10,+10) );
    mSlicedVolume->setBox(volume_box);

    // volume bounding box outline
    ref<Effect> fx_box = new Effect;
    fx_box->shader()->gocPolygonMode()->set(PM_LINE, PM_LINE);
    fx_box->shader()->enable(EN_DEPTH_TEST);
    fx_box->shader()->gocColor()->setValue(vl::red);
    ref<Geometry> box_outline = vl::makeBox(volume_box);
    sceneManager()->tree()->addActor( box_outline.get(), fx_box.get(), mVolumeTr.get() );

    // bias text
    mBiasText = new Text;
    mBiasText->setFont( defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 12) );
    mBiasText->setAlignment( AlignHCenter | AlignBottom);
    mBiasText->setViewportAlignment( AlignHCenter | AlignBottom );
    mBiasText->translate(0,5,0);
    mBiasText->setBackgroundEnabled(true);
    mBiasText->setBackgroundColor(fvec4(0,0,0,0.75));
    mBiasText->setColor(vl::white);
    ref<Effect> effect = new Effect;
    effect->shader()->enable(EN_BLEND);
    sceneManager()->tree()->addActor(mBiasText.get(), effect.get());

    // bias uniform
    mVolumeAct->gocUniform("val_threshold")->setUniformF(0.5f);
    mAlphaBias = mVolumeAct->getUniform("val_threshold");

    // update alpha bias text
    mouseWheelEvent(0);

    // let's get started with the default volume!
    setupVolume( loadImage("/volume/VLTest.dat") );
  }

  /* load files drag&dropped in the window */
  void fileDroppedEvent(const std::vector<String>& files)
  {
    if(files.size() == 1) // if there is one file load it directly
    {      
      if (files[0].endsWith(".dat"))
      {
        ref<Image> vol_img = loadImage(files[0]);
        if (vol_img)
          setupVolume(vol_img);
      }
    }
    else // if there is more than one file load all the files and assemble a 3D image
    {      
      // sort files by their name
      std::vector<String> files_sorted = files;
      std::sort(files_sorted.begin(), files_sorted.end());
      // load the files
      std::vector< ref<Image> > images;
      for(unsigned int i=0; i<files_sorted.size(); ++i)
      {
        images.push_back( loadImage(files_sorted[i]) );
        if (files_sorted[i].endsWith(".dcm"))
          images.back()->contrastHounsfieldAuto();
      }
      // assemble the volume
      ref<Image> vol_img = assemble3DImage(images);
      // set the volume
      if (vol_img)
        setupVolume(vol_img);
    }
  }

  /* visualize the given volume */
  void setupVolume(ref<Image> img)
  {
    Effect* vol_fx = mVolumeAct->effect();

    // remove shader uniforms
    vol_fx->shader()->setUniformSet(NULL);
    // remove GLSLProgram
    vol_fx->shader()->eraseRenderState(vl::RS_GLSLProgram);
    // keep texture unit #0
    // vol_fx->shader()->eraseRenderState(RS_TextureSampler,0); 
    // remove texture unit #1 and #2
    vol_fx->shader()->eraseRenderState(RS_TextureSampler, 1);
    vol_fx->shader()->eraseRenderState(RS_TextureSampler, 2);

    if(img->format() == IF_LUMINANCE)
    {
      ref<Image> gradient;
      if (PRECOMPUTE_GRADIENT)
      {
        // note that this can take a while...
        gradient = vl::genGradientNormals(img.get());
      }

      if (USE_GLSL)
      {
        Log::notify("IF_LUMINANCE image and GLSL supported: lighting and the transfer function will be computed in realtime.\n");

        ref<Image> trfunc;
        if (COLORED_LIGHTS)
          trfunc = vl::makeColorSpectrum(128, vl::white, vl::white); // let the lights color the volume
        else
          trfunc = vl::makeColorSpectrum(128, vl::blue, vl::royalblue, vl::green, vl::yellow, vl::crimson);
        // installs GLSLProgram
        vol_fx->shader()->setRenderState(mGLSL.get());
        // install volume image
        vol_fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( img.get() ) );
        vol_fx->shader()->gocUniform("volume_texunit")->setUniformI(0);
        mSlicedVolume->generateTextureCoordinates( ivec3(img->width(), img->height(), img->depth()) );
        // installs the transfer function as texture #1
        vol_fx->shader()->gocTextureSampler(1)->setTexture( new Texture( trfunc.get() ) );  
        vol_fx->shader()->gocUniform("trfunc_texunit")->setUniformI(1);
        vol_fx->shader()->gocUniform("trfunc_delta")->setUniformF( 0.5f / trfunc->width() );    
        // pre-computed gradient texture
        if (PRECOMPUTE_GRADIENT)
        {
          vol_fx->shader()->gocUniform("precomputed_gradient")->setUniformI(1);
          vol_fx->shader()->gocTextureSampler(2)->setTexture( new Texture( gradient.get(), TF_RGBA8, false, false ) );
          vol_fx->shader()->gocUniform("gradient_texunit")->setUniformI(2);
        }
        else
        {
          vol_fx->shader()->gocUniform("precomputed_gradient")->setUniformI(0);
          // used to compute on the fly the normals based on the volume's gradient
          vol_fx->shader()->gocUniform("gradient_delta")->setUniform(fvec3(0.5f/img->width(), 0.5f/img->height(), 0.5f/img->depth()));
        }

        // no need for alpha testing, we discard fragments inside the fragment shader
        vol_fx->shader()->disable(EN_ALPHA_TEST);
      }
      else // precompute transfer function and illumination
      {
        Log::notify("IF_LUMINANCE image and GLSL not supported: transfer function and lighting will be precomputed.\n");

        // generate simple transfer function
        ref<Image> trfunc = vl::makeColorSpectrum(128, vl::black, vl::blue, vl::green, vl::yellow, vl::red);
        // precompute volume with transfer function and lighting
        ref<Image> volume = vl::genRGBAVolume(img.get(), trfunc.get(), fvec3(1.0f,1.0f,0.0f));
        vol_fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( volume.get() ) );
        mSlicedVolume->generateTextureCoordinates( ivec3(volume->width(), volume->height(), volume->depth()) );
        vol_fx->shader()->enable(EN_ALPHA_TEST);
        vol_fx->shader()->gocAlphaFunc()->set(FU_GEQUAL, 0.3f);
      }
    }
    else // if it's a color texture just display it as it is
    {
      Log::notify("Non IF_LUMINANCE image: not using GLSL.\n");
      // install volume texture
      vol_fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( img.get() ) );
      mSlicedVolume->generateTextureCoordinates( ivec3(img->width(), img->height(), img->depth()) );
      // setup alpha test
      vol_fx->shader()->enable(EN_ALPHA_TEST);
      vol_fx->shader()->gocAlphaFunc()->set(FU_GEQUAL, 0.3f);
    }
    
    mAlphaBias->setUniformF(0.3f);
    updateText();
    openglContext()->update();
  }

  void updateText()
  {
    float bias = 0.0f;
    mAlphaBias->getUniform(&bias);
    mBiasText->setText(Say("Bias = %n") << bias);
  }

  void mouseWheelEvent(int val)
  {
    float alpha = 0.0f;
    mAlphaBias->getUniform(&alpha);
    alpha += val * 0.002f;
    alpha =  clamp(alpha, 0.0f, 1.0f);
    mAlphaBias->setUniformF(alpha);

    // used for non GLSL mode volumes
    mVolumeAct->effect()->shader()->gocAlphaFunc()->set(FU_GEQUAL, alpha);
    
    updateText();
    openglContext()->update();
  }

  /* animate the lights */
  virtual void updateScene()
  {
    if (DYNAMIC_LIGHTS)
    {
      mat4 mat;
      // light 0 transform.
      mat = mat4::getRotation( Time::currentTime()*43, 0,1,0 ) * mat4::getTranslation(20,20,20);
      mLight0Tr->setLocalMatrix(mat);
      // light 1 transform.
      mat = mat4::getRotation( Time::currentTime()*47, 0,1,0 ) * mat4::getTranslation(-20,0,0);
      mLight1Tr->setLocalMatrix(mat);
      // light 2 transform.
      mat = mat4::getRotation( Time::currentTime()*47, 0,1,0 ) * mat4::getTranslation(+20,0,0);
      mLight2Tr->setLocalMatrix(mat);
    }
  }

  protected:
    ref<Transform> mVolumeTr;
    ref<Transform> mLight0Tr;
    ref<Transform> mLight1Tr;
    ref<Transform> mLight2Tr;
    ref<Uniform> mAlphaBias;
    ref<Text> mBiasText;
    ref<Light> mLight0;
    ref<Light> mLight1;
    ref<Light> mLight2;
    ref<GLSLProgram> mGLSL;
    ref<Actor> mVolumeAct;
    ref<vl::SlicedVolume> mSlicedVolume;
};

// Have fun!

BaseDemo* Create_App_VolumeSliced() { return new App_VolumeSliced; }
