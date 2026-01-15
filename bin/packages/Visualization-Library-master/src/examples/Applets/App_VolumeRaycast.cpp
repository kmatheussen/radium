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
#include <vlVolume/RaycastVolume.hpp>
#include <vlVolume/VolumeUtils.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

using namespace vl;

/* 
  This example demonstrates how to implement raycasting using the vl::RaycastVolume class and a few ad-hoc fragment shaders.
   
  The user has several controls as well:
   
  Left/Right arrow keys changes the raycasting mode:
  - Isosurface_Mode
  - Isosurface_Transp_Mode, 
  - MIP_Mode
  - RaycastBrightnessControl_Mode
  - RaycastDensityControl_Mode
  - RaycastColorControl_Mode

  Mouse wheel:
  - In Isosurface_Mode controls the iso-value of the isosurface
  - In Isosurface_Transp_Mode controls the iso-value of the isosurface
  - In MIP_Mode all the volume values less than this are discarded
  - In RaycastBrightnessControl_Mode controls the brightness of the voxels
  - In RaycastDensityControl_Mode controls the density of the voxels
  - In RaycastColorControl_Mode controls the color-bias of the voxels

  The Up/Down arrow keys are used to higher/lower the ray-advancement precision.

  The 'L' key toggles the dynamic and colored lights.
*/
class App_VolumeRaycast: public BaseDemo
{
  /* ----- raycast volume rendering options ----- */

  /* The sample step used to render the volume, the smaller the number 
     the  better ( and slower ) the rendering will be. */
  float SAMPLE_STEP;

  /* volume visualization mode */
  enum RaycastMode { 
    Isosurface_Mode, 
    Isosurface_Transp_Mode, 
    MIP_Mode, 
    RaycastBrightnessControl_Mode,
    RaycastDensityControl_Mode,
    RaycastColorControl_Mode
  } MODE;

  /* If enabled, renders the volume using 3 animated lights. */
  bool DYNAMIC_LIGHTS;

  /* If enabled 3 colored lights are used to render the volume. */
  bool COLORED_LIGHTS;

  /* Use a separate 3d texture with a precomputed gradient to speedup the fragment shader.
     Requires more memory ( for the gradient texture ) but can speedup the rendering. */
  bool PRECOMPUTE_GRADIENT;

public:
  virtual String appletInfo()
  {
    return BaseDemo::appletInfo() + 
    "- Left/Right Arrow: change raycast technique.\n" +
    "- Up/Down Arrow: changes SAMPLE_STEP.\n" +
    "- L: toggles lights (useful only for isosurface).\n" +
    "- Mouse Wheel: change the bias used to render the volume.\n" +
    "\n" +
    "- Drop inside the window a set of 2D files or a DDS or DAT volume to display it.\n" +
    "\n";
  }

  App_VolumeRaycast()
  {
    SAMPLE_STEP         = 1.0f / 512.0f;
    MODE                = RaycastBrightnessControl_Mode;
    DYNAMIC_LIGHTS      = false;
    COLORED_LIGHTS      = false;
    PRECOMPUTE_GRADIENT = false;
  }

  /* initialize the applet with a default volume */
  virtual void initEvent()
  {
    vl::Log::notify(appletInfo());

    if ( !Has_GLSL )
    {
      vl::Log::error( "OpenGL Shading Language not supported.\n" );
      vl::Time::sleep(2000);
      exit(1);
    }

    mLight0 = new Light;
    mLight1 = new Light;
    mLight2 = new Light;

    mLight0Tr = new Transform;
    mLight1Tr = new Transform;
    mLight2Tr = new Transform;
    rendering()->as<Rendering>()->transform()->addChild( mLight0Tr.get() );
    rendering()->as<Rendering>()->transform()->addChild( mLight1Tr.get() );
    rendering()->as<Rendering>()->transform()->addChild( mLight2Tr.get() );

    // volume transform
    mVolumeTr = new Transform;

    // val_threshold: manipulated via mouse wheel
    // - In Isosurface_Mode controls the iso-value of the isosurface
    // - In Isosurface_Transp_Mode controls the iso-value of the isosurface
    // - In MIP_Mode all the volume values less than this are discarded
    // - In RaycastBrightnessControl_Mode controls the brightness of the voxels
    // - In RaycastDensityControl_Mode controls the density of the voxels
    // - In RaycastColorControl_Mode controls the color-bias of the voxels
    mValThreshold = new Uniform( "val_threshold" );
    mValThreshold->setUniformF( 0.5f );

    // default volume image
    mVolumeImage = loadImage( "/volume/VLTest.dat" );

    setupScene();
  }

  void setupScene()
  {
    // scrap previous scene
    sceneManager()->tree()->eraseAllChildren();
    sceneManager()->tree()->actors()->clear();
    mLight0->bindTransform( NULL );
    mLight1->bindTransform( NULL );
    mLight2->bindTransform( NULL );
  
    ref<Effect> volume_fx = new Effect;
    // we don't necessarily need this:
    // volume_fx->shader()->enable( EN_BLEND ); 
    volume_fx->shader()->enable( EN_CULL_FACE );
    volume_fx->shader()->enable( EN_DEPTH_TEST );

    // NOTE
    // in these cases we render the back faces and raycast in back to front direction
    // in the other cases we render the front faces and raycast in front to back direction
    if ( MODE == RaycastBrightnessControl_Mode || MODE == RaycastDensityControl_Mode || MODE == RaycastColorControl_Mode )
    {
      volume_fx->shader()->enable( vl::EN_CULL_FACE );
      volume_fx->shader()->gocCullFace()->set( vl::PF_FRONT );
    }

    volume_fx->shader()->setRenderState( mLight0.get(), 0 );

    // light bulbs
    if ( DYNAMIC_LIGHTS )
    {
      // you can color the lights!
      if ( COLORED_LIGHTS )
      {
        mLight0->setAmbient( fvec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        mLight1->setAmbient( fvec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        mLight2->setAmbient( fvec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        mLight0->setDiffuse( vl::gold );
        mLight1->setDiffuse( vl::green );
        mLight2->setDiffuse( vl::royalblue );
      }

      // add the other two lights
      volume_fx->shader()->setRenderState( mLight1.get(), 1 );
      volume_fx->shader()->setRenderState( mLight2.get(), 2 );

      // animate the three lights
      mLight0->bindTransform( mLight0Tr.get() );
      mLight1->bindTransform( mLight1Tr.get() );
      mLight2->bindTransform( mLight2Tr.get() );

      // add also a light bulb actor
      ref<Effect> fx_bulb = new Effect;
      fx_bulb->shader()->enable( EN_DEPTH_TEST );
      ref<Geometry> light_bulb = vl::makeIcosphere( vec3( 0,0,0 ),1,1 );
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight0Tr.get() );
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight1Tr.get() );
      sceneManager()->tree()->addActor( light_bulb.get(), fx_bulb.get(), mLight2Tr.get() );
    }

    // the GLSL program that performs the actual raycasting
    mGLSL = volume_fx->shader()->gocGLSLProgram();
    mGLSL->gocUniform( "sample_step" )->setUniformF( SAMPLE_STEP );
    
    // attach vertex shader (common to all the raycasting techniques)
    mGLSL->attachShader( new GLSLVertexShader( "/glsl/volume_luminance_light.vs" ) );
    
    // attach fragment shader implementing the specific raycasting tecnique
    if ( MODE == Isosurface_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast_isosurface.fs" ) );
    else
    if ( MODE == Isosurface_Transp_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast_isosurface_transp.fs" ) );
    else
    if ( MODE == MIP_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast_mip.fs" ) );
    else
    if ( MODE == RaycastBrightnessControl_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast01.fs" ) );
    else
    if ( MODE == RaycastDensityControl_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast02.fs" ) );
    else
    if ( MODE == RaycastColorControl_Mode )
      mGLSL->attachShader( new GLSLFragmentShader( "/glsl/volume_raycast03.fs" ) );

    // manipulate volume transform with the trackball
    trackball()->setTransform( mVolumeTr.get() );

    // volume actor
    mVolumeAct = new Actor;
    mVolumeAct->setEffect( volume_fx.get() );
    mVolumeAct->setTransform( mVolumeTr.get() );
    sceneManager()->tree()->addActor( mVolumeAct.get() );
    // bind val_threshold uniform to the volume actor
    mVolumeAct->setUniform( mValThreshold.get() );

    // RaycastVolume will generate the actual actor's geometry upon setBox() invocation.
    // The geometry generated is actually a simple box with 3D texture coordinates.
    mRaycastVolume = new vl::RaycastVolume;
    mRaycastVolume->bindActor( mVolumeAct.get() );
    AABB volume_box( vec3( -10,-10,-10 ), vec3( +10,+10,+10 ) );
    mRaycastVolume->setBox( volume_box );

    // val_threshold text
    mValThresholdText = new Text;
    mValThresholdText->setFont( defFontManager()->acquireFont( "/font/bitstream-vera/VeraMono.ttf", 12 ) );
    mValThresholdText->setTextAlignment( TextAlignCenter );
    mValThresholdText->setAlignment( AlignHCenter | AlignBottom );
    mValThresholdText->setViewportAlignment( AlignHCenter | AlignBottom );
    mValThresholdText->translate( 0,5,0 );
    mValThresholdText->setBackgroundEnabled( true );
    mValThresholdText->setBackgroundColor( fvec4( 0,0,0,0.75 ) );
    mValThresholdText->setColor( vl::white );
    ref<Effect> effect = new Effect;
    effect->shader()->enable( EN_BLEND );
    sceneManager()->tree()->addActor( mValThresholdText.get(), effect.get() );
    updateText();

    // let's visualize the volume!
    setupVolume();
  }

  /* visualize the given volume */
  void setupVolume()
  {
    Effect* volume_fx = mVolumeAct->effect();

    // for semplicity we don't distinguish between different image formats, i.e. IF_LUMINANCE, IF_RGBA etc.

    ref<Image> gradient;
    if ( PRECOMPUTE_GRADIENT )
    {
      // note that this can take a while...
      gradient = vl::genGradientNormals( mVolumeImage.get() );
    }

    // install volume image as textue #0
    volume_fx->shader()->gocTextureSampler( 0 )->setTexture( new vl::Texture( mVolumeImage.get(), TF_LUMINANCE8, false, false ) );
    volume_fx->shader()->gocUniform( "volume_texunit" )->setUniformI( 0 );
    mRaycastVolume->generateTextureCoordinates( ivec3(mVolumeImage->width(), mVolumeImage->height(), mVolumeImage->depth()) );

    // generate a simple colored transfer function
    ref<Image> trfunc;
    if ( COLORED_LIGHTS && DYNAMIC_LIGHTS )
      trfunc = vl::makeColorSpectrum( 128, vl::white, vl::white ); // let the lights color the volume
    else
      trfunc = vl::makeColorSpectrum( 128, vl::blue, vl::royalblue, vl::green, vl::yellow, vl::crimson );

    // installs the transfer function as texture #1
    volume_fx->shader()->gocTextureSampler( 1 )->setTexture( new Texture( trfunc.get() ) );
    volume_fx->shader()->gocUniform( "trfunc_texunit" )->setUniformI( 1 );
    
    // gradient computation, only use for isosurface methods
    if ( MODE == Isosurface_Mode || MODE == Isosurface_Transp_Mode )
    {
      if ( PRECOMPUTE_GRADIENT )
      {
        volume_fx->shader()->gocUniform( "precomputed_gradient" )->setUniformI( 1 /*true*/ );
        volume_fx->shader()->gocTextureSampler( 2 )->setTexture( new Texture( gradient.get(), TF_RGBA, false, false ) );
        volume_fx->shader()->gocUniform( "gradient_texunit" )->setUniformI( 2 );
      }
      else
      {
        volume_fx->shader()->gocUniform( "precomputed_gradient" )->setUniformI( 0 /*false*/ );
        // used to compute on the fly the normals based on the volume's gradient
        volume_fx->shader()->gocUniform( "gradient_delta" )->setUniform( fvec3( 0.5f/mVolumeImage->width(), 0.5f/mVolumeImage->height(), 0.5f/mVolumeImage->depth() ) );
      }
    }

    // update text
    updateText();

    // refresh window
    openglContext()->update();
  }

  /* load files drag&dropped in the window */
  void fileDroppedEvent( const std::vector<String>& files )
  {
    mVolumeImage = NULL;

    if( files.size() == 1 ) // if there is one file load it directly
    {      
      if ( files[0].endsWith( ".dat" ) || files[0].endsWith( ".dds" ) )
      {
        mVolumeImage = loadImage( files[0] );
        if ( mVolumeImage )
          setupVolume();
      }
    }
    else // if there is more than one file load and sort them and assemble a 3D image
    {      
      // sort files by their name
      std::vector<String> files_sorted = files;
      std::sort( files_sorted.begin(), files_sorted.end() );
      // load the files
      std::vector< ref<Image> > images;
      for( unsigned int i=0; i<files_sorted.size(); ++i )
      {
        images.push_back( loadImage( files_sorted[i] ) );
        if (files_sorted[i].endsWith(".dcm"))
          images.back()->contrastHounsfieldAuto();
      }
      // assemble the volume
      mVolumeImage = assemble3DImage( images );
      // set the volume
      if ( mVolumeImage )
        setupVolume();
    }

    if ( !mVolumeImage )
      Log::error("Error loading volume data!\n");
  }

  void updateText()
  {
    // update the val_threshold value and the raycast technique name
    String technique_name;
    switch(MODE)
    {
      case Isosurface_Mode: technique_name               = "raycast isosurface >"; break;
      case Isosurface_Transp_Mode: technique_name        = "< raycast transparent isosurface >"; break;
      case MIP_Mode: technique_name                      = "< raycast maximum intensity projection >"; break;
      case RaycastBrightnessControl_Mode: technique_name = "< raycast brightness control >"; break;
      case RaycastDensityControl_Mode: technique_name    = "< raycast density control >"; break;
      case RaycastColorControl_Mode: technique_name      = "< raycast color control"; break;
    };

    float val_threshold = 0;
    mValThreshold->getUniform( &val_threshold );
    mValThresholdText->setText( Say( "val_threshold = %n\n" "sample_step = 1.0 / %.0n\n" "%s" ) << val_threshold << 1.0f / SAMPLE_STEP << technique_name);
  }

  void updateValThreshold( int val )
  {
    float val_threshold = 0.0f;
    mValThreshold->getUniform( &val_threshold );
    val_threshold += val * 0.01f;
    val_threshold = clamp( val_threshold, 0.0f, 1.0f );
    mValThreshold->setUniformF( val_threshold );

    updateText();
    openglContext()->update();
  }

  void mouseWheelEvent( int val )
  {
    updateValThreshold( val );
  }

  /* animate the lights */
  virtual void updateScene()
  {
    if ( DYNAMIC_LIGHTS )
    {
      mat4 mat;
      // light 0 transform.
      mat = mat4::getRotation( Time::currentTime()*43, 0,1,0 ) * mat4::getTranslation( 20,20,20 );
      mLight0Tr->setLocalMatrix( mat );
      // light 1 transform.
      mat = mat4::getRotation( Time::currentTime()*47, 0,1,0 ) * mat4::getTranslation( -20,0,0 );
      mLight1Tr->setLocalMatrix( mat );
      // light 2 transform.
      mat = mat4::getRotation( Time::currentTime()*47, 0,1,0 ) * mat4::getTranslation( +20,0,0 );
      mLight2Tr->setLocalMatrix( mat );
    }
  }

  virtual void keyPressEvent(unsigned short, EKey key)
  {
    // left/right arrows change raycast technique
    RaycastMode modes[] = { Isosurface_Mode, Isosurface_Transp_Mode, MIP_Mode, RaycastBrightnessControl_Mode, RaycastDensityControl_Mode, RaycastColorControl_Mode };
    int mode = MODE;
    if (key == vl::Key_Right)
      mode++;
    else
    if (key == vl::Key_Left)
      mode--;
    MODE = modes[ vl::clamp(mode, 0, 5) ];

    // up/down changes SAMPLE_STEP
    if (key == vl::Key_Up)
    {
      SAMPLE_STEP = 1.0f / ( (1.0f / SAMPLE_STEP) * 1.3f ); // more precision
    }
    else
    if (key == vl::Key_Down)
    {
      SAMPLE_STEP = 1.0f / ( (1.0f / SAMPLE_STEP) / 1.3f ); // less precision
    }

    // L key toggles lights (useful only for isosurface)
    if (key == vl::Key_L)
    {
      if (!DYNAMIC_LIGHTS)
      {
        DYNAMIC_LIGHTS = true;
        COLORED_LIGHTS = false;
      }
      else
      if (DYNAMIC_LIGHTS && !COLORED_LIGHTS)
      {
        DYNAMIC_LIGHTS = true;
        COLORED_LIGHTS = true;
      }
      else
      {
        DYNAMIC_LIGHTS = false;
        COLORED_LIGHTS = false;
      }
    }

    setupScene();
  }

  private:
    ref<Transform> mVolumeTr;
    ref<Transform> mLight0Tr;
    ref<Transform> mLight1Tr;
    ref<Transform> mLight2Tr;
    ref<Uniform> mValThreshold;
    ref<Text> mValThresholdText;
    ref<Light> mLight0;
    ref<Light> mLight1;
    ref<Light> mLight2;
    ref<GLSLProgram> mGLSL;
    ref<Actor> mVolumeAct;
    ref<vl::RaycastVolume> mRaycastVolume;
    ref<Image> mVolumeImage;
};
// Have fun!

BaseDemo* Create_App_VolumeRaycast() { return new App_VolumeRaycast; }
