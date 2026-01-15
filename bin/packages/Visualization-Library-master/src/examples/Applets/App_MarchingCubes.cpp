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
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/DepthSortCallback.hpp>
#include <vlVolume/MarchingCubes.hpp>
#include <vlVolume/VolumePlot.hpp>
#include <ctime>

class App_MarchingCubes: public BaseDemo
//
// This applet demonstrates: how to use marching cubes to generate isosurfaces, how to visualize multiple intersecting volumes, 
// how to manage transparencies, how to color the isosurface using texturing and vertex color, how to implement a simple metaballs demo
// and how to simulate a metaballs/water fountain.
//
// Test 0: drag and drop a volume in the window and use the mouse wheel to visualize the appropriate iso-surface level.
// Test 1: multiple self-intersecting transparent isosurfaces visualization.
// Test 2: isosurface colorize via texturing.
// Test 3: isosurface colorize via vertex color.
// Test 4: animated metaball demo implemented on top of the marching cube algorithm.
// Test 5: animated fountain based on marching cubes.
// Test 6: 3D function plotting with vl::VolumePlot
//
{
public:
  App_MarchingCubes(): mTest(4) {}

  void initEvent()
  {
    vl::Log::notify(appletInfo());
    openglContext()->setContinuousUpdate(true);

    srand((unsigned int)time(NULL));

    mThreshold = 0.44f;
    mFountainSpeed = 2.5f;

    // setup metaballs data

    mMetaball.resize(mParticleCount);
    mMetaballVelocity.resize(mParticleCount);
    mMetaballsFrames.resize(mFrameCount);
    for(int iframe=0; iframe<mFrameCount; ++iframe)
    {
      mMetaballsFrames[iframe].resize(mParticleCount);
      for(int iball=0; iball<mParticleCount; ++iball)
      {
        mMetaballsFrames[iframe][iball].x() = (rand()%100 / 100.0f)*0.6f+0.2f;
        mMetaballsFrames[iframe][iball].y() = (rand()%100 / 100.0f)*0.6f+0.2f;
        mMetaballsFrames[iframe][iball].z() = (rand()%100 / 100.0f)*0.6f+0.2f;
      }
    }

    // load images

    mVolumeImage = vl::loadImage("/volume/VLTest.dat")->convertFormat(vl::IF_LUMINANCE)->convertType(vl::IT_FLOAT);
    mColorImage  = vl::loadImage("/volume/VLTest.dat");

    // transform

    mTransform = new vl::Transform;
    rendering()->as<vl::Rendering>()->transform()->addChild( mTransform.get() );

    // text setup

    mText = new vl::Text();
    mText->setDisplayListEnabled(!vl::Has_GLES);
    mText->setFont( vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10) );
    mText->setMargin(5);
    mText->setViewportAlignment(vl::AlignTop | vl::AlignHCenter);
    mText->setAlignment(vl::AlignTop | vl::AlignHCenter);
    mText->setTextAlignment(vl::TextAlignCenter);
    mText->setColor(vl::white);
    mText->setBackgroundColor(vl::fvec4(0,0,0,.75f));
    mText->setBackgroundEnabled(true);
    mText->setText("Marching Cubes Demo");

    vl::ref< vl::Effect > text_fx = new vl::Effect;
    text_fx->shader()->enable(vl::EN_BLEND);

    mTextActor = new vl::Actor( mText.get(), text_fx.get() );
    sceneManager()->tree()->addActor( mTextActor.get() );

    setupTest();
  }

  virtual void updateScene()
  {
    if (mTest == 4)
      runTest4();
    else
    if (mTest == 5)
      runTest5();
  }

  void runTest4()
  {
    // rotate metaballs
    vl::vec3 axis;
    axis.x() = sin( vl::Time::currentTime()*3.1415f*2.0f/8.0f );
    axis.y() = 0;
    axis.z() = cos( vl::Time::currentTime()*3.1415f*2.0f/8.0f );
    axis.normalize();
    if (axis.isNull())
      return;
    mTransform->setLocalMatrix( vl::mat4::getRotation( vl::Time::currentTime()*120.0f, axis) );
    // animate metaballs
    float t = (float)vl::fract( vl::Time::currentTime()*0.05f );
    t = vl::clamp(t, 0.0f, 0.999f);
    int frame1 = int(t * mFrameCount);
    int frame2 = (frame1+1) % mFrameCount;
    t = vl::fract(t*mFrameCount);
    float Ha = 2*t*t*t - 3*t*t + 1;
    float Hb = -2*t*t*t + 3*t*t;
    for(unsigned iball=0; iball<mMetaball.size(); ++iball)
      mMetaball[iball] = mMetaballsFrames[frame1][iball]*Ha + mMetaballsFrames[frame2][iball]*Hb;

    int idx = 0;
    float* values = &mMarchingCubes.volumeInfo()->at(0)->volume()->value(0);
    for(int z=0; z<mMetaballsResolution; ++z)
    {
      float pz = (float)z/mMetaballsResolution;
      for(int y=0; y<mMetaballsResolution; ++y)
      {
        float py = (float)y/mMetaballsResolution;
        for(int x=0; x<mMetaballsResolution; ++x, ++idx)
        {
          values[idx] = metaballFunction((float)x/mMetaballsResolution, py, pz);
        }
      }
    }
    // notify that the volume data has changed
    mMarchingCubes.volumeInfo()->at(0)->volume()->setDataDirty();
    mMarchingCubes.run(false);
  }

  void runTest5()
  {
    // animate metaballs
    for(unsigned iball=0; iball<mMetaball.size(); ++iball)
    {
      mMetaballVelocity[iball] -= vl::fvec3(0,1.5f,0)*(float)mTimer.elapsed()*mFountainSpeed;

      mMetaball[iball] = mMetaball[iball] + mMetaballVelocity[iball]*(float)mTimer.elapsed();
      if (mMetaball[iball].y() < 0)
      {
        mMetaball[iball].y() = 0;
        mMetaball[iball].x() = 0.5f;
        mMetaball[iball].z() = 0.5f;
        mMetaballVelocity[iball].x() = (float)vl::random(-12,+12);
        mMetaballVelocity[iball].z() = (float)vl::random(-12,+12);
        mMetaballVelocity[iball].y() = 100;
        mMetaballVelocity[iball].normalize();
        mMetaballVelocity[iball] *= (17.0f + (float)vl::random(0,5))*0.05f*mFountainSpeed;
      }
    }
    mTimer.start();

    int idx = 0;
    float* values = &mMarchingCubes.volumeInfo()->at(0)->volume()->value(0);
    for(int z=0; z<mMetaballsResolution; ++z)
    {
      float pz = (float)z/mMetaballsResolution;
      for(int y=0; y<mMetaballsResolution; ++y)
      {
        float py = (float)y/mMetaballsResolution;
        for(int x=0; x<mMetaballsResolution; ++x, ++idx)
          values[idx] = metaballFunction((float)x/mMetaballsResolution, py, pz);
      }
    }
    // notify that the volume data has changed
    mMarchingCubes.volumeInfo()->at(0)->volume()->setDataDirty();
    mMarchingCubes.run(false);
  }

  float metaballFunction(float x, float y, float z)
  {
    float val = 0;
    for(unsigned i=0; i<mMetaball.size(); ++i)
    {
      float rx = x-mMetaball[i].x();
      float ry = y-mMetaball[i].y();
      float rz = z-mMetaball[i].z();
      float r2 = rx*rx+ry*ry+rz*rz;
      if (r2 == 0.0f)
        return 1.0e+10f;
      val += vl::fast1_inversesqrt(r2*r2);    // threshold = 400
      // val += vl::fast2_inversesqrt(r2*r2); // threshold = 400
      // val += 1.0f / r2;                    // threshold = 400
      // val += vl::fast1_inversesqrt(r2);    // threshold = 70
      // val += vl::fast2_inversesqrt(r2);    // threshold = 70
    }
    return val;
  }

  void keyPressEvent(unsigned short ch, vl::EKey key)
  {
    BaseDemo::keyPressEvent(ch,key);
    bool update = false;
    if (key == vl::Key_Right)
    {
      mTest++;
      update = true;
    }
    else
    if (key == vl::Key_Left)
    {
      mTest--;
      update = true;
    }
    if (update)
    {
      if (mTest > 6) mTest = 0;
      if (mTest < 0) mTest = 6;
      setupTest();
      updateText();
    }
  }

  // generates the 2 transparent volumes or a single volume
  vl::Actor* showVolumes(bool test1)
  {
    rendering()->as<vl::Rendering>()->camera()->setModelingMatrix( vl::mat4::getTranslation(0,0,20) );

    vl::ref<vl::Geometry > geom = new vl::Geometry;
    geom->setVertexArray(mMarchingCubes.mVertsArray.get());
    geom->setNormalArray(mMarchingCubes.mNormsArray.get());
    geom->drawCalls()->push_back(mMarchingCubes.mDrawElements.get());

    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    // two side lighting
    fx->shader()->gocLightModel()->setTwoSide(true);

    vl::ref<vl::Actor> act = new vl::Actor(geom.get(), fx.get(), NULL);
    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor(act.get());
    sceneManager()->tree()->addActor( mTextActor.get() );

    vl::ref<vl::Volume> volume = new vl::Volume;
    volume->setup( (float*)mVolumeImage->pixels(), false, true, vl::fvec3(-5,-5,-5), vl::fvec3(+5,+5,+5), vl::ivec3(mVolumeImage->width(), mVolumeImage->height(), mVolumeImage->depth()) );
    
    mMarchingCubes.reset();
    mMarchingCubes.volumeInfo()->push_back( new vl::VolumeInfo( volume.get(), 0.40f, /*yellow*/vl::fvec4(1, 1, 0, 0.5f)) );
    if (test1)
    {
      // generate second volume
      mMarchingCubes.volumeInfo()->push_back( new vl::VolumeInfo( volume.get(), 0.50f, /*red*/vl::fvec4(1, 0, 0, 0.5f)) );
      // enable blend
      fx->shader()->enable(vl::EN_BLEND);
      // use color array
      fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
      // polygon depth sorting
      act->actorEventCallbacks()->push_back( new vl::DepthSortCallback );
      // binds the marching cubes color array
      geom->setColorArray (mMarchingCubes.mColorArray.get());
    }

    mMarchingCubes.run(true);

    // stats
    vl::String stats;
    stats += vl::Say("vertices = %n\n") << mMarchingCubes.mVertsArray->size();
    vl::Log::print(stats);

    return act.get();
  }

  // Coloring a volume using a 3d texture can be very useful when the color of the texture is animated
  // i.e. changes relatively rapidly or when the isosurface itself changes or is animated.
  // Cons:
  // 1 - the texture can occupy a large amount of memory
  // 2 - to guarantee maximum compatibility the texture should be a cube (not a rectangle) whose side length is a power of 2.
  void textureVolumeColor(vl::Actor* vol_act)
  {
    if (!vl::Has_Texture_3D)
    {
      vl::Log::notify("textureVolumeColor() requires 3D texturing.\n");
      return;
    }

    vl::Effect* fx = vol_act->effect();

    // automatic 3D texture coordinate generation
    #if 1
      fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( mColorImage.get() ) );
      fx->shader()->gocTexGen(0)->setGenModeS(vl::TGM_OBJECT_LINEAR);
      fx->shader()->gocTexGen(0)->setGenModeT(vl::TGM_OBJECT_LINEAR);
      fx->shader()->gocTexGen(0)->setGenModeR(vl::TGM_OBJECT_LINEAR);
      /* the settings below are the defaults
      fx->shader()->gocTexGen(0)->setObjectPlaneS(vl::fvec4(1,0,0,0));
      fx->shader()->gocTexGen(0)->setObjectPlaneT(vl::fvec4(0,1,0,0));
      fx->shader()->gocTexGen(0)->setObjectPlaneR(vl::fvec4(0,0,1,0));
      fx->shader()->gocTexEnv(0)->setMode(vl::TEM_MODULATE);*/
      vol_act->lod(0)->computeBounds();
      vl::AABB box = vol_act->lod(0)->boundingBox();
      // transforms object vertex coordinates to 0..1 cube, this might create a "bleeding color" effect at the very side of the cube.
      // note that to be really precise we should use 1/2N ... 1-1/2N in each direction instead of 0..1.
      vl::mat4 tex_mat = vl::mat4::getTranslation( vl::vec3(0.5f,0.5f,0.5f) ) * 
                         vl::mat4::getScaling(1.0f/box.width(),1.0f/box.height(),1.0f/box.depth()) * 
                         vl::mat4::getTranslation( -box.center() );
      fx->shader()->gocTextureMatrix(0)->setMatrix((vl::fmat4)tex_mat);
    #endif

    // the above code is equivalent to the code below that manually generates the texture coordinates.
    #if 0
      // transforms object vertex coordinates to 0..1 cube.
      vol_act->lod(0)->computeBounds();
      vl::AABB box = vol_act->lod(0)->boundingBox();
      vl::mat4 tex_mat = vl::mat4::getTranslation( vl::vec3(0.5f,0.5f,0.5f) ) * 
                         vl::mat4::getScaling(1.0f/box.width(),1.0f/box.height(),1.0f/box.depth()) * 
                         vl::mat4::getTranslation( -box.center() );

      fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( mColorImage.get() ) );
      vl::ref<vl::ArrayFloat3> tex_array = new vl::ArrayFloat3;
      tex_array->resize( geom->vertexArray()->size() );
      geom->setTexCoordArray(0, tex_array.get());
      for(int i=0; i<tex_array->size(); ++i)
        tex_array->at(i) = ((vl::fvec4)(tex_mat * geom->vertexArray()->getAsVec4(i))).xyz();
    #endif
  }

  // Coloring a volume assigning a color to each vertex directly can save memory compared to using a 3D texture, 
  // and no 3D texturing is needed so there aren't compatibility issues. 
  // This technique is not particularly efficient if your isosurfaces are animated and you need to recompute the colors often.
  void vertexVolumeColor(vl::Actor* vol_act)
  {
    vl::Geometry* geom = vl::cast<vl::Geometry>(vol_act->lod(0));
    vl::Effect* fx = vol_act->effect();
    // use color array
    fx->shader()->gocMaterial()->setColorMaterialEnabled(true);

    // transforms object vertex coordinates to 0..1 cube.
    vol_act->lod(0)->computeBounds();
    vl::AABB box = vol_act->lod(0)->boundingBox();
    vl::mat4 tex_mat = vl::mat4::getTranslation( vl::vec3(0.5f,0.5f,0.5f) ) * 
                       vl::mat4::getScaling(1.0f/box.width(),1.0f/box.height(),1.0f/box.depth()) * 
                       vl::mat4::getTranslation( -box.center() );

    vl::ref<vl::ArrayFloat4> color_array = new vl::ArrayFloat4;
    color_array->resize( geom->vertexArray()->size() );
    geom->setColorArray(color_array.get());
    for(size_t i=0; i<color_array->size(); ++i)
    {
      vl::vec4 px = tex_mat * geom->vertexArray()->getAsVec4(i);
      px.x() *= mColorImage->width();
      px.y() *= mColorImage->height();
      px.z() *= mColorImage->depth();
      color_array->at(i) = mColorImage->sampleLinear(px.x(), px.y(), px.z());
    }
  }

  // Setups the good old metaballs demo!
  vl::Actor* setupMetaballs()
  {
    rendering()->as<vl::Rendering>()->camera()->setModelingMatrix( vl::mat4::getTranslation(0,0,25) );

    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor( mTextActor.get() );

    vl::ref<vl::Volume> volume = new vl::Volume;
    mMarchingCubes.reset();
    mMarchingCubes.volumeInfo()->push_back( new vl::VolumeInfo( volume.get(), 400.0f) );
    mMarchingCubes.volumeInfo()->at(0)->volume()->setup( NULL, false, false, vl::fvec3(-10,-10,-10), vl::fvec3(+10,+10,+10), vl::ivec3(mMetaballsResolution,mMetaballsResolution,mMetaballsResolution) );
    
    vl::ref<vl::Geometry > geom = new vl::Geometry;
    geom->setVertexArray(mMarchingCubes.mVertsArray.get());
    geom->setNormalArray(mMarchingCubes.mNormsArray.get());
    geom->drawCalls()->push_back(mMarchingCubes.mDrawElements.get());
    // disable BufferObject since we update the vertices every frame
    geom->setBufferObjectEnabled(false);

    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    // two side lighting
    fx->shader()->gocLightModel()->setTwoSide(true);

    if (vl::Has_GL_Version_1_1)
    {
      vl::ref<vl::Image> texture = vl::loadImage("/images/spheremap.png");
      fx->shader()->gocTextureSampler(0)->setTexture( new vl::Texture( texture.get() ) );
      fx->shader()->gocTexGen(0)->setGenModeS(vl::TGM_SPHERE_MAP);
      fx->shader()->gocTexGen(0)->setGenModeT(vl::TGM_SPHERE_MAP);
      fx->shader()->gocTexEnv(0)->setMode(vl::TEM_BLEND);
    }

    vl::ref<vl::Actor> act = sceneManager()->tree()->addActor(geom.get(), fx.get(), mTransform.get());

    return act.get();
  }

  // Setups the metaballs fountain demo!
  vl::Actor* setupFountain()
  {
    rendering()->as<vl::Rendering>()->camera()->setModelingMatrix( vl::mat4::getTranslation(0,0,25) );

    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor( mTextActor.get() );

    vl::ref<vl::Volume> volume = new vl::Volume;
    mMarchingCubes.reset();
    mMarchingCubes.volumeInfo()->push_back( new vl::VolumeInfo( volume.get(), 400.0f) );
    mMarchingCubes.volumeInfo()->at(0)->volume()->setup( NULL, false, false, vl::fvec3(-10,-10,-10), vl::fvec3(+10,+10,+10), vl::ivec3(mMetaballsResolution,mMetaballsResolution,mMetaballsResolution) );
    
    vl::ref<vl::Geometry > geom = new vl::Geometry;
    geom->setBoundingBox( vl::AABB( vl::vec3(-10,-10,1-10), vl::vec3(10,10,10) ) );
    geom->setBoundingSphere( geom->boundingBox() );
    geom->setVertexArray(mMarchingCubes.mVertsArray.get());
    geom->setNormalArray(mMarchingCubes.mNormsArray.get());
    geom->drawCalls()->push_back(mMarchingCubes.mDrawElements.get());
    // disable BufferObject since we update the vertices every frame
    geom->setBufferObjectEnabled(false);

    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    // two side lighting
    fx->shader()->gocLightModel()->setTwoSide(true);
    fx->shader()->gocMaterial()->setDiffuse(vl::royalblue);

    vl::ref<vl::Actor> act = sceneManager()->tree()->addActor(geom.get(), fx.get(), mTransform.get());

    for(unsigned iball=0; iball<mMetaball.size(); ++iball)
    {
      mMetaball[iball].y() = 0;
      mMetaball[iball].x() = 0.5f;
      mMetaball[iball].z() = 0.5f;
      mMetaballVelocity[iball].x() = (float)vl::random(-15,+15);
      mMetaballVelocity[iball].z() = (float)vl::random(-15,+15);
      mMetaballVelocity[iball].y() = 100;
      mMetaballVelocity[iball].normalize();
      mMetaballVelocity[iball] *= (5.0f + (float)vl::random(0,20))*0.05f*mFountainSpeed;
    }

    mTransform->setLocalMatrix(vl::mat4());

    mTimer.start();
    return act.get();
  }

  class my_func: public vl::VolumePlot::Function
  {
  public:
    virtual float operator()(float x, float y, float z) const
    {
      // return sqrt(x*x+y*y+z*z); == 0.9f
      // return exp(-y)*sin(z)+cos(z*x); // == 2.0f
      return -x/5.0f*sin(z/5.0f)+exp(y*y*y/5.0f/5.0f/5.0f); // == 0.900f
    }
  };

  // Shows how to use vl::VolumePlot to create a 3D plot.
  void setup3Dplot()
  {
    // reset actors and camera
    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor( mTextActor.get() );
    rendering()->as<vl::Rendering>()->camera()->setViewMatrix( vl::mat4::getLookAt( vl::vec3(5,10,20), vl::vec3(0,0,0), vl::vec3(0,1,0)) );

    float range = 5.0f;
    vl::fvec3 min_corner(-range,-range,-range);
    vl::fvec3 max_corner(+range,+range,+range);

    vl::VolumePlot plot;
    plot.setMinCorner(min_corner);
    plot.setMaxCorner(max_corner);
    plot.compute( my_func(), 0.900f );
    sceneManager()->tree()->addChild(plot.actorTreeMulti());
  }

  void loadVolume(vl::ref<vl::Image> vol_img)
  {
    // reset camera
    rendering()->as<vl::Rendering>()->camera()->setModelingMatrix( vl::mat4::getTranslation(0,0,20) );

    // reset actors
    sceneManager()->tree()->actors()->clear();
    sceneManager()->tree()->addActor( mTextActor.get() );

    // convert the image to a one-component float volume
    // and keep it alive since we use its data directly
    mDropImage = vol_img->convertFormat(vl::IF_LUMINANCE)->convertType(vl::IT_FLOAT);

    // note: we use the image data directly without making copies:
    vl::ref<vl::Volume> volume = new vl::Volume;
    volume->setup( (float*)mDropImage->pixels(), true, false, vl::fvec3(-5,-5,-5), vl::fvec3(+5,+5,+5), vl::ivec3(mDropImage->width(), mDropImage->height(), mDropImage->depth()) );

    // start timing
    vl::Time time; 
    
    #if 0
      // downsample volume data and perform timing.
      time.start();
      volume = volume->downsample();
      vl::Log::print( vl::Say("Downsampling time = %.2n\n") << time.elapsed() );
    #endif

    mMarchingCubes.reset();
    mMarchingCubes.volumeInfo()->push_back( new vl::VolumeInfo(volume.get(), mThreshold) );

    // run MarchingCubes with timing.
    time.start();
    mMarchingCubes.run(false);
    vl::Log::print( vl::Say("Marching cubes: time = %.2n, verts = %n\n") << time.elapsed() << mMarchingCubes.mVertsArray->size() );

    // setup isosurface geometry, actor and effect

    // geometry
    mIsosurfGeom = new vl::Geometry;
    // install vertex and normal arrays and primitives generated by the marching cube algorithm
    mIsosurfGeom->setVertexArray(mMarchingCubes.mVertsArray.get());
    mIsosurfGeom->setNormalArray(mMarchingCubes.mNormsArray.get());
    mIsosurfGeom->drawCalls()->push_back(mMarchingCubes.mDrawElements.get());

    #if 0
      time.start();
      vl::PolygonSimplifier ps;
      ps.simplify( 0.1f, mIsosurfGeom.get() );
      mIsosurfGeom->computeNormals();
      vl::Log::print( vl::Say("Simplification: time = %.2n\n") << time.elapsed() );
    #endif

    vl::ref<vl::Effect> fx = new vl::Effect;

    // effect
    fx->shader()->setRenderState( new vl::Light, 0 );
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    fx->shader()->enable(vl::EN_LIGHTING);
    fx->shader()->gocLightModel()->setTwoSide(true);
    fx->shader()->gocMaterial()->setBackDiffuse(vl::green);

    // show the volume in wireframe to see the tessellation.
#if defined(VL_OPENGL)
    fx->lod(0)->push_back( new vl::Shader );
    fx->shader(0,1)->enable(vl::EN_CULL_FACE);
    fx->shader(0,1)->enable(vl::EN_DEPTH_TEST);
    fx->shader(0,1)->enable(vl::EN_POLYGON_OFFSET_LINE);
    fx->shader(0,1)->gocPolygonOffset()->set(-1.0f, -1.0f);
    fx->shader(0,1)->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    fx->shader(0,1)->gocColor()->setValue(vl::royalblue);
#endif

    // actor
    vl::ref<vl::Actor> actor = new vl::Actor(mIsosurfGeom.get(), fx.get());

    // add actor to the scene
    sceneManager()->tree()->addActor( actor.get() );

    updateText();
  }

  void setupTest()
  {
    sceneManager()->tree()->eraseAllChildren();
    if (mTest == 1)
      showVolumes(true);
    else
    if (mTest == 2)
      textureVolumeColor( showVolumes(false) );
    else
    if (mTest == 3)
      vertexVolumeColor( showVolumes(false) );
    else
    if (mTest == 4)
      setupMetaballs();
    else
    if (mTest == 5)
      setupFountain();
    else
    if (mTest == 6)
      setup3Dplot();

    updateText();
  }

  void updateText()
  {
    vl::String str;
    if(mTest == 0)
      str = vl::Say("Marching Cubes Test #0 - Volume Viewer - threshold = %.2n\n(drop a .dat file)") << mThreshold;
    else
    if(mTest == 1)
      str = "Marching Cubes Test #1 - Multiple Volumes & Transparency";
    else
    if(mTest == 2)
      str = "Marching Cubes Test #2 - Texture Colorized Volumes";
    else
    if(mTest == 3)
      str = "Marching Cubes Test #3 - Vertex Colorized Volumes";
    else
    if(mTest == 4)
      str = "Marching Cubes Test #4 - Metaballs";
    else
    if(mTest == 5)
      str = "Marching Cubes Test #5 - Fountain";
    else
    if(mTest == 6)
      str = "Marching Cubes Test #6 - 3D Function Plotting";

    str += "\n(press the <- or -> key to change test)";
    mText->setText( str );
    mText->setDisplayListDirty(true);
  }

  void mouseWheelEvent(int w)
  {
    if (mTest != 0)
      return;

    if (w>0)
      mThreshold += 0.010f;
    else
      mThreshold -= 0.010f;

    mThreshold = vl::clamp(mThreshold, 0.0f, 1.0f);

    vl::Time time; time.start();
    mMarchingCubes.volumeInfo()->at(0)->setThreshold(mThreshold);
    mMarchingCubes.run(false);
    if (mIsosurfGeom)
      mIsosurfGeom->setBufferObjectDirty(true);

    vl::Log::print( vl::Say("Marching cubes: time = %.2n, verts = %n\n") << time.elapsed() << mMarchingCubes.mVertsArray->size() );

    updateText();

    openglContext()->update();
  }

  void fileDroppedEvent(const std::vector<vl::String>& files)
  {
    mTest = 0;

    if(files.size() == 1)
    {
      if (files[0].endsWith(".dat"))
      {
        vl::ref<vl::Image> vol_img = vl::loadImage(files[0]);
        loadVolume(vol_img);
      }
    }
    else
    {
      std::vector<vl::String> files_sorted = files;
      std::sort(files_sorted.begin(), files_sorted.end());
      std::vector< vl::ref<vl::Image> > images;
      for(unsigned int i=0; i<files_sorted.size(); ++i)
        images.push_back(vl::loadImage(files_sorted[i])->convertFormat(vl::IF_LUMINANCE)->convertType(vl::IT_UNSIGNED_BYTE));
      vl::ref<vl::Image> vol_img = vl::assemble3DImage(images);
      if (vol_img)
        loadVolume(vol_img);
    }
  }

  void resizeEvent(int w, int h)
  {
    BaseDemo::resizeEvent(w,h);
    mText->setDisplayListDirty(true);
  }

protected:
  vl::ref<vl::ResourceDatabase> mResDB;
  vl::ref<vl::Transform> mTransform;
  vl::ref< vl::Text > mText;
  vl::ref< vl::Actor > mTextActor;

  // volume
  vl::MarchingCubes mMarchingCubes;
  float mThreshold;
  std::vector<vl::fvec3> mMetaball;
  std::vector<vl::fvec3> mMetaballVelocity;
  vl::Time mTimer;
  std::vector< std::vector<vl::fvec3> > mMetaballsFrames;
  static const int mMetaballsResolution = 32;
  static const int mParticleCount = 25;
  static const int mFrameCount    = 20;
  float mFountainSpeed;
  vl::ref<vl::Image> mVolumeImage;
  vl::ref<vl::Image> mColorImage;
  vl::ref<vl::Image> mDropImage;
  vl::ref<vl::Geometry> mIsosurfGeom;
  int mTest;
};

// Have fun!

BaseDemo* Create_App_MarchingCubes() { return new App_MarchingCubes; }
