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
#include <vlGraphics/Array.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/GLSL.hpp>

using namespace vl;

/* 
 * You can find the documentatio for this example in the offical documentation at:
 * Quick Start Guides -> Texturing
 */

class App_Texturing: public BaseDemo
{
public:
  void multitexturing()
  {
    if (!Has_Multitexture)
    {
      Log::error("Multitexturing not supported.\n");
      return;
    }

    // create a box with texture coordinates
    const bool generate_tex_coords = true;
    ref<Geometry> box = makeBox( vec3(0,0,0), 5,5,5, generate_tex_coords );
    box->computeNormals();
    // IMPORTANT: makeBox() filled for use box->texCoordArray(0) however in order to use multi-texturing we need
    // texture coordinates also for unit #1 so we make texture unit #1 share the texture coordinates with unit #0.
    box->setTexCoordArray(1, box->texCoordArray(0));

    // load base texture
    // note: TF_UNKNOWN tells VL to set the texture format to whatever format the image is.
    ref<Texture> tex_holebox = new Texture("/images/holebox.tif", TF_UNKNOWN, mMipmappingOn );
    tex_holebox->getTexParameter()->setMagFilter(TPF_LINEAR);
    tex_holebox->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);

    // load detail texture
    ref<Texture> tex_detail  = new Texture("/images/detail.tif", TF_UNKNOWN, mMipmappingOn );
    tex_detail->getTexParameter()->setMagFilter(TPF_LINEAR);
    tex_detail->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);

	  // IMPORTANT: since we requested mipmapping we set the MinFilter to GL_LINEAR_MIPMAP_LINEAR, i.e. trilinear filtering.
    // Note also that using a mipmapped filter with a texture that has no mipmaps will typically show a black texture.
	  
    // You can set the MinFilter to any of GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST, 
	  // GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR. However rembember that you can set the MagFilter only to 
    // GL_NEAREST or GL_LINEAR as mipmapping does not make any sense for texture magnification.

    ref<Light> light = new Light;

    // single texture effect with alpha testing
    ref<Effect> fx_right_cube = new Effect;
    fx_right_cube->shader()->setRenderState( light.get(), 0 );
    fx_right_cube->shader()->enable(EN_LIGHTING);
    fx_right_cube->shader()->enable(EN_DEPTH_TEST);
    fx_right_cube->shader()->enable(EN_BLEND);
    fx_right_cube->shader()->enable(EN_ALPHA_TEST);
    fx_right_cube->shader()->gocAlphaFunc()->set(FU_GEQUAL, 0.98f);
    fx_right_cube->shader()->gocLightModel()->setTwoSide(true);
    fx_right_cube->shader()->gocTextureSampler(0)->setTexture( tex_holebox.get() );

    // multi-texture effect with alpha testing
    ref<Effect> fx_left_cube = new Effect;
    fx_left_cube->shader()->setRenderState( light.get(), 0 );
    fx_left_cube->shader()->enable(EN_LIGHTING);
    fx_left_cube->shader()->enable(EN_DEPTH_TEST);
    fx_left_cube->shader()->enable(EN_BLEND);
    fx_left_cube->shader()->enable(EN_ALPHA_TEST);
    fx_left_cube->shader()->gocAlphaFunc()->set(FU_GEQUAL, 0.98f);
    fx_left_cube->shader()->gocLightModel()->setTwoSide(true);
    fx_left_cube->shader()->gocTextureSampler(0)->setTexture( tex_holebox.get() );
    fx_left_cube->shader()->gocTextureSampler(1)->setTexture( tex_detail.get() );
    fx_left_cube->shader()->gocTexEnv(1)->setMode(TEM_MODULATE); // modulate texture #0 and #1

    // add right box
    mRightCubeTransform = new Transform;
    rendering()->as<Rendering>()->transform()->addChild(mRightCubeTransform.get());
    sceneManager()->tree()->addActor( box.get(), fx_right_cube.get(), mRightCubeTransform.get() );

    // add left box
    mLeftCubeTransform = new Transform;
    rendering()->as<Rendering>()->transform()->addChild(mLeftCubeTransform.get());
    sceneManager()->tree()->addActor( box.get(), fx_left_cube.get(),  mLeftCubeTransform.get() );
  }

  void texture3D()
  {
    if(!Has_Texture_3D)
    {
      Log::error("Texture 3D not supported.\n");
      return;
    }

    // Create a 2x2 vertices quad facing the camera
    mQuad3DTex = makeGrid( vec3(0,0,0), 10.0f, 10.0f, 2, 2 );
    // Rotate plane toward the user, otherwise it would be on the x/z plane
    mQuad3DTex->transform( mat4::getRotation(90, 1,0,0), false );

    // Texture coordinates to be animated in updateScene()
    mTexCoords_3D = new ArrayFloat3;
    mTexCoords_3D->resize( 2*2 );
    mQuad3DTex->setTexCoordArray(0, mTexCoords_3D.get());

    // Effect used by the actor
    ref<Effect> fx_3d = new Effect;
    fx_3d->shader()->enable(EN_DEPTH_TEST);

    // Add and position the actor in the scene
    Actor* act_3d = sceneManager()->tree()->addActor( mQuad3DTex.get(), fx_3d.get(), new Transform );
    act_3d->transform()->setLocalAndWorldMatrix( mat4::getTranslation(-6,+6,-6) );

    // Setup a 3D texture with mipmapping
    ref<Texture> texture_3d = new Texture;
    // Load "/volume/VLTest.dat" which is a 3D image and prepare a 3D texture from it
    texture_3d->prepareTexture3D( "/volume/VLTest.dat", TF_UNKNOWN, mMipmappingOn );
    texture_3d->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_3d->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
    fx_3d->shader()->gocTextureSampler(0)->setTexture( texture_3d.get() );
  }

  void texture2DArray()
  {
    if(!Has_Texture_Array)
    {
      Log::error("Texture 2d array not supported.\n");
      return;
    }

    // Create a 2x2 vertices quad facing the camera
    mQuad2DArrayTex = makeGrid( vec3(0,0,0), 10.0f, 10.0f, 2, 2 );
    // Rotate plane toward the user
    mQuad2DArrayTex->transform( mat4::getRotation(90, 1,0,0), false );

    // Texture coordinates to be animated in updateScene()
    mTexCoords_2DArray = new ArrayFloat3;
    mTexCoords_2DArray->resize( 2*2 );
    mQuad2DArrayTex->setTexCoordArray(0, mTexCoords_2DArray.get());

    // Create the effect used by the actor
    ref<Effect> fx_2darray = new Effect;
    fx_2darray->shader()->enable(EN_DEPTH_TEST);

    // Add and position the actor in the scene
    Actor* act_2darray = sceneManager()->tree()->addActor( mQuad2DArrayTex.get(), fx_2darray.get(), new Transform );
    act_2darray->transform()->setLocalAndWorldMatrix( mat4::getTranslation(+6,+6,-6) );

    // Load a 3D image, VL considers 3D images equivalent to an array of 2D images.
    ref<Image> img_volume = loadImage("/volume/VLTest.dat");
    m2DArraySize = img_volume->depth();

    // Create the 2D texture array and bind it to unit #0
    ref<Texture> texture_2darray = new Texture;
    texture_2darray->prepareTexture2DArray( img_volume.get(), TF_RGBA, mMipmappingOn );
    texture_2darray->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_2darray->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
    fx_2darray->shader()->gocTextureSampler(0)->setTexture( texture_2darray.get() );
      
    // IMPORTANT
    // We need a GLSL program that uses 'sampler2DArray()' to access the 1D and 2D texture arrays!
    GLSLProgram* glsl = fx_2darray->shader()->gocGLSLProgram();
    glsl->attachShader( new GLSLFragmentShader("/glsl/texture_2d_array.fs") );
    // Bind the sampler to unit #0
    glsl->gocUniform("sampler0")->setUniformI(0);
  }

  void texture1DArray()
  {
    if(!Has_Texture_Array)
    {
      Log::error("Texture 1d array not supported.\n");
      return;
    }

    // Load a 2D texture, VL considers 2D images equivalent to arrays of 1D images.
    ref<Image> img_holebox = loadImage("/images/holebox.tif");    
    m1DArraySize = img_holebox->height();

    // Create a grid with img_holebox->height() slices
    mQuad1DArrayTex = makeGrid( vec3(0,0,0), 10, 10, 2, img_holebox->height() );
    mQuad1DArrayTex->transform( mat4::getRotation(90, 1,0,0), false );

    // Texture coordinates to be animated in updateScene()
    mTexCoords_1DArray = new ArrayFloat2;
    mTexCoords_1DArray->resize( 2 * img_holebox->height() );
    mQuad1DArrayTex->setTexCoordArray(0, mTexCoords_1DArray.get());

    // Create the effect used by the actor
    ref<Effect> fx_1darray = new Effect;
    fx_1darray->shader()->enable(EN_DEPTH_TEST);

    // Add and position the actor in the scene
    Actor* act_1darray = sceneManager()->tree()->addActor( mQuad1DArrayTex.get(), fx_1darray.get(), new Transform );
    act_1darray->transform()->setLocalAndWorldMatrix( mat4::getTranslation(+6,-6,-6) );

    // Create the 1D texture array and bind it to unit #0
    ref<Texture> texture_1darray = new Texture;
    texture_1darray->prepareTexture1DArray( img_holebox.get(), TF_RGBA, mMipmappingOn );
    texture_1darray->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_1darray->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
    fx_1darray->shader()->gocTextureSampler(0)->setTexture( texture_1darray.get() );
      
    // IMPORTANT
    // We need a GLSL program that uses 'sampler1DArray()' to access the 1D and 2D texture arrays!
    GLSLProgram* glsl = fx_1darray->shader()->gocGLSLProgram();
    glsl->attachShader( new GLSLFragmentShader("/glsl/texture_1d_array.fs") );
    glsl->gocUniform("sampler0")->setUniformI(0);
  }

  void textureRectangle()
  {
    if(!Has_Texture_Rectangle)
    {
      Log::error("Texture rectangle not supported.\n");
      return;
    }

    ref<Image> img_holebox = loadImage("/images/holebox.tif");    

    // Create a box that faces the camera
    // Generate non-normalized uv coordinates, i.e. from <0,0> to <img_holebox->width(), img_holebox->height()>
    float s_max = (float)img_holebox->width();
    float t_max = (float)img_holebox->height();
    ref<Geometry> quad_rect = makeGrid( vec3(0,0,0), 10.0f, 10.0f, 2, 2, true, fvec2(0, 0), fvec2(s_max, t_max) );
    quad_rect->transform( mat4::getRotation(90, 1,0,0), false );

    // Effect used by the actor
    ref<Effect> fx_rect = new Effect;
    fx_rect->shader()->enable(EN_DEPTH_TEST);

    // Add and position the actor in the scene
    Actor* act_rect = sceneManager()->tree()->addActor( quad_rect.get(), fx_rect.get(), new Transform );
    act_rect->transform()->setLocalAndWorldMatrix( mat4::getTranslation(-6,-6,-6) );

    // Setup the texture rectangle
    ref<Texture> texture_rectangle = new Texture;
    // Note that mipmapping is not an option for texture rectangles since they do not support mipmaps
    texture_rectangle->prepareTextureRectangle( img_holebox.get(), TF_RGBA );
    // Set non-mipmapping filters for the texture
    texture_rectangle->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_rectangle->getTexParameter()->setMinFilter(TPF_LINEAR);
    // GL_REPEAT (the default) is not allowed with texture rectangle so we set it to GL_CLAMP
    texture_rectangle->getTexParameter()->setWrapS(TPW_CLAMP);
    texture_rectangle->getTexParameter()->setWrapT(TPW_CLAMP);
    texture_rectangle->getTexParameter()->setWrapR(TPW_CLAMP);
    fx_rect->shader()->gocTextureSampler(0)->setTexture( texture_rectangle.get() );
  }

  void sphericalMapping()
  {
    if (Has_GLES)
    {
      Log::error("Spherical mapping texture coordinate generation not supported.\n");
      return;
    }

    // Effect used by the actor
    mFXSpheric = new Effect;
    mFXSpheric->shader()->enable(EN_DEPTH_TEST);
    mFXSpheric->shader()->enable(EN_CULL_FACE);
    mFXSpheric->shader()->enable(EN_LIGHTING);
    mFXSpheric->shader()->setRenderState( new Light, 0 );

    // Add sphere mapped torus
    // makeTorus() also generates the normals which are needed by GL_SPHERE_MAP texture coordinate generation mode
    ref<Geometry> torus = makeTorus(vec3(), 8,3, 40,40);
    mActSpheric = sceneManager()->tree()->addActor( torus.get(), mFXSpheric.get(), new Transform );
    rendering()->as<Rendering>()->transform()->addChild( mActSpheric->transform() );

    // Create a 2d texture sphere map
    ref<Texture> texture_sphere_map = new Texture;
    texture_sphere_map->prepareTexture2D( "/images/spheremap_klimt.jpg", TF_UNKNOWN, mMipmappingOn );
    texture_sphere_map->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_sphere_map->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
    mFXSpheric->shader()->gocTextureSampler(0)->setTexture( texture_sphere_map.get() );

    // Enable spherical mapping texture coordinate generation for s and t
    mFXSpheric->shader()->gocTexGen(0)->setGenModeS(TGM_SPHERE_MAP);
    mFXSpheric->shader()->gocTexGen(0)->setGenModeT(TGM_SPHERE_MAP);
  }

  void cubeMapping()
  {
    if (!Has_Cubemap_Textures)
    {
      Log::error("Texture cubemap not supported.\n");
      return;
    }

    ref<Image> img_cubemap = loadCubemap(
      "/images/cubemap/cubemap00.png",  // (x+) right
      "/images/cubemap/cubemap01.png",  // (x-) left
      "/images/cubemap/cubemap02.png",  // (y+) top
      "/images/cubemap/cubemap03.png",  // (y-) bottom
      "/images/cubemap/cubemap04.png",  // (z+) back
      "/images/cubemap/cubemap05.png"); // (z-) front

    // Effect used by the actor
    mFXCubic = new Effect;
    mFXCubic->shader()->enable(EN_DEPTH_TEST);
    mFXCubic->shader()->enable(EN_CULL_FACE);
    mFXCubic->shader()->enable(EN_LIGHTING);
    mFXCubic->shader()->setRenderState( new Light, 0 );

    // Add cube-mapped torus
    // makeTorus() also generates the normals which are needed by GL_REFLECTION_MAP texture coordinate generation mode
    ref<Geometry> torus = makeTorus( vec3(), 8,3, 40,40 );
    mActCubic = sceneManager()->tree()->addActor( torus.get(), mFXCubic.get(), new Transform );
    rendering()->as<Rendering>()->transform()->addChild( mActCubic->transform() );

    // Create the cube-map texture
    ref<Texture> texture_cubic = new Texture;
    texture_cubic->prepareTextureCubemap( img_cubemap.get(), TF_RGBA, mMipmappingOn );
    // Texture filtering modes
    texture_cubic->getTexParameter()->setMagFilter(TPF_LINEAR);
    texture_cubic->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
    // Clamp to edge to minimize seams
    texture_cubic->getTexParameter()->setWrapS(TPW_CLAMP_TO_EDGE);
    texture_cubic->getTexParameter()->setWrapT(TPW_CLAMP_TO_EDGE);
    texture_cubic->getTexParameter()->setWrapR(TPW_CLAMP_TO_EDGE);
    // Install the texture on unit #0
    mFXCubic->shader()->gocTextureSampler(0)->setTexture( texture_cubic.get() );

    // Enable automatic texture generation for s, t, r on unit #0
    mFXCubic->shader()->gocTexGen(0)->setGenModeS(TGM_REFLECTION_MAP);
    mFXCubic->shader()->gocTexGen(0)->setGenModeT(TGM_REFLECTION_MAP);
    mFXCubic->shader()->gocTexGen(0)->setGenModeR(TGM_REFLECTION_MAP);
    
    // Align the cube-map to the world space axes rather than eye space axes.
    mFXCubic->shader()->gocTextureMatrix(0)->setUseCameraRotationInverse(true);
  }

  void initEvent()
  {
    // Log applet info
    Log::notify(appletInfo());

    // Default values
    mMipmappingOn = true;
    mLodBias = 0.0;

    // Show all the texture types tests
    multitexturing();
    textureRectangle();
    texture3D();
    texture2DArray();
    texture1DArray();
    sphericalMapping();
    cubeMapping();
  }

  void updateScene()
  {
    // 5 seconds period
    float t = sin( Time::currentTime()*fPi*2.0f/5.0f) * 0.5f + 0.5f;
    t = t * (1.0f - 0.02f*2) + 0.02f;

    // Rotating cubes

    if (Has_Multitexture)
    {
      mRightCubeTransform->setLocalMatrix( mat4::getTranslation(+6,0,0) * mat4::getRotation( Time::currentTime()*45, 0, 1, 0) );
      mLeftCubeTransform ->setLocalMatrix( mat4::getTranslation(-6,0,0) * mat4::getRotation( Time::currentTime()*45, 0, 1, 0) );
    }

    // 3D texture coordinates animation

    if (mTexCoords_3D)
    {
      // Animate the z texture coordinate.
      mTexCoords_3D->at(0) = fvec3(0, 0, t);
      mTexCoords_3D->at(1) = fvec3(0, 1, t);
      mTexCoords_3D->at(2) = fvec3(1, 0, t);
      mTexCoords_3D->at(3) = fvec3(1, 1, t);
      // Mark texture coords as dirty to update its BufferObjects.
      mTexCoords_3D->setBufferObjectDirty(true);
      // Request the quad geometry to check its BufferObjects at the next rendering.
      mQuad3DTex->setBufferObjectDirty(true);
    }

    // 2D texture array coordinates animation

    if (mTexCoords_2DArray)
    {
      // Animate the z texture coordinate.
      // Note that unlike for 3D textures in 2d array textures the z coordinate
      // is not defined between 0..1 but between 1..N where N is the number of
      // texture layers present in the texture array.
      mTexCoords_2DArray->at(0) = fvec3(0, 0, t*m2DArraySize);
      mTexCoords_2DArray->at(1) = fvec3(0, 1, t*m2DArraySize);
      mTexCoords_2DArray->at(2) = fvec3(1, 0, t*m2DArraySize);
      mTexCoords_2DArray->at(3) = fvec3(1, 1, t*m2DArraySize);
      // Mark texture coords as dirty to update its BufferObjects.
      mTexCoords_2DArray->setBufferObjectDirty(true);
      // Request the quad geometry to check its BufferObjects at the next rendering.
      mQuad2DArrayTex->setBufferObjectDirty(true);
    }

    // 1D texture array coordinates animation

    if (mTexCoords_1DArray)
    {
      for(int i=0; i<m1DArraySize; ++i)
      {
        // Create some waving animation
        float x_offset = 0.1f * cos( t*3.14159265f + 10.0f*((float)i/m1DArraySize)*3.14159265f );
        // Note: the y texture coordinate is an integer value between 0 and N where N 
        // is the number of texture 1D layers present in the texture array
        mTexCoords_1DArray->at(i*2+0) = fvec2(0+x_offset, (float)i);
        mTexCoords_1DArray->at(i*2+1) = fvec2(1+x_offset, (float)i);
      }
      // Mark texture coords as dirty to update its BufferObjects.
      mTexCoords_1DArray->setBufferObjectDirty(true);
      // Request the quad geometry to check its BufferObjects at the next rendering.
      mQuad1DArrayTex->setBufferObjectDirty(true);
    }

    // Spherical mapped torus animation

    if (mActSpheric)
    {
      // Just rotate the torus
      mActSpheric->transform()->setLocalMatrix( mat4::getTranslation(0,+6,0)*mat4::getRotation(45*Time::currentTime(),1,0,0) );
      mActSpheric->transform()->computeWorldMatrix();
    }

    // Cube mapped torus animation

    if (mActCubic)
    {
      // Just rotate the torus
      mActCubic->transform()->setLocalMatrix( mat4::getTranslation(0,-6,0)*mat4::getRotation(45*Time::currentTime(),1,0,0) );
      mActCubic->transform()->computeWorldMatrix();
    }
  } 

  void mouseWheelEvent(int w)
  {
    // Change the LOD bias of the texture to simulate sharp/dull reflections.

    mLodBias += w*0.3f;
    mLodBias = clamp(mLodBias, 0.0f, 4.0f);

    mFXSpheric->shader()->gocTexEnv(0)->setLodBias(mLodBias);
    mFXCubic->shader()->gocTexEnv(0)->setLodBias(mLodBias);
  }

protected:
  ref<Geometry> mQuad3DTex;
  ref<Geometry> mQuad2DArrayTex;
  ref<Geometry> mQuad1DArrayTex; 
  ref<Transform> mRightCubeTransform;
  ref<Transform> mLeftCubeTransform;
  ref<ArrayFloat3> mTexCoords_3D;
  ref<ArrayFloat3> mTexCoords_2DArray;
  ref<ArrayFloat2> mTexCoords_1DArray;
  int m1DArraySize;
  int m2DArraySize;
  ref<Actor> mActSpheric;
  ref<Actor> mActCubic;
  ref<Effect> mFXSpheric;
  ref<Effect> mFXCubic;
  float mLodBias;
  bool mMipmappingOn;
};

// Have fun!

BaseDemo* Create_App_Texturing() { return new App_Texturing; }
