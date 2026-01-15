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
#include <vlGraphics/DistanceLODEvaluator.hpp>
#include <vlGraphics/BezierSurface.hpp>
#include <vlGraphics/plugins/ioVLX.hpp>
#include <vlCore/ZippedFile.hpp>
#include <vlCore/GlobalSettings.hpp>
#include <vlGraphics/DistanceLODEvaluator.hpp>
#include <vlGraphics/PixelLODEvaluator.hpp>
#include <vlGraphics/TriangleStripGenerator.hpp>
#include <vlGraphics/expandResourceDatabase.hpp>

using namespace vl;

class App_VLX: public BaseDemo
{
public:

  virtual void initEvent()
  {
    Log::notify(appletInfo());

    smokeTest();
    teapotTest();
  }

  // VLX serialization test using VLXSerializer directly.
  void teapotTest()
  {
    sceneManager()->tree()->actors()->clear();

    ref<Geometry> geom = makeTeapot( vec3(0,0,0), 10, 6 );
    geom->computeNormals();
    geom->setColorArray( geom->normalArray() );

    ref<Effect> fx = new Effect;
    fx->shader()->enable(EN_LIGHTING);
    fx->shader()->enable(EN_DEPTH_TEST);
    fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
    fx->shader()->setRenderState( new Light, 0 );

    ref<Actor> act = new Actor( geom.get(), fx.get(), new Transform );

    GLSLProgram* glsl = fx->shader()->gocGLSLProgram();
    glsl->attachShader( new GLSLVertexShader("/glsl/perpixellight.vs") );
    glsl->attachShader( new GLSLFragmentShader("/glsl/perpixellight.fs") );

    VLXSerializer vlx_serializer;
    String vlt_path;
    String vlb_path;

    act->transform()->setLocalMatrix( mat4::getTranslation(+10, +10, 0) );
    vlt_path = globalSettings()->defaultDataPath() + "/vlx/teapot1.vlt";
    vlx_serializer.saveVLT(vlt_path, act.get()); VL_CHECK(!vlx_serializer.error());
    ref<Actor> act1 = vlx_serializer.loadVLT(vlt_path)->as<Actor>(); VL_CHECK(!vlx_serializer.error()); VL_CHECK(act1.get())

    act->transform()->setLocalMatrix( mat4::getTranslation(+10, -10, 0) );
    vlb_path = globalSettings()->defaultDataPath() + "/vlx/teapot2.vlb";
    vlx_serializer.saveVLB(vlb_path, act.get()); VL_CHECK(!vlx_serializer.error());
    ref<Actor> act2 = vlx_serializer.loadVLB(vlb_path)->as<Actor>(); VL_CHECK(!vlx_serializer.error()); VL_CHECK(act2.get())

    glsl->shader(0)->setPath("");
    glsl->shader(0)->setSource("");
    glsl->shader(1)->setPath("");
    glsl->shader(1)->setSource("");

    act->transform()->setLocalMatrix( mat4::getTranslation(-10, +10, 0) );
    vlt_path = globalSettings()->defaultDataPath() + "/vlx/teapot3.vlt";
    vlx_serializer.saveVLT(vlt_path, act.get()); VL_CHECK(!vlx_serializer.error());
    ref<Actor> act3 = vlx_serializer.loadVLT(vlt_path)->as<Actor>(); VL_CHECK(!vlx_serializer.error()); VL_CHECK(act3.get())

    act->transform()->setLocalMatrix( mat4::getTranslation(-10, -10, 0) );
    vlb_path = globalSettings()->defaultDataPath() + "/vlx/teapot4.vlb";
    vlx_serializer.saveVLB(vlb_path, act.get()); VL_CHECK(!vlx_serializer.error());
    ref<Actor> act4 = vlx_serializer.loadVLB(vlb_path)->as<Actor>(); VL_CHECK(!vlx_serializer.error()); VL_CHECK(act4.get())

    ref<Camera> cam = new Camera;
    cam->setViewMatrix( mat4::getLookAt( vec3(0,0,50), vec3(0,0,0), vec3(0,1,0) ) );
    vlt_path = globalSettings()->defaultDataPath() + "/vlx/camera.vlt";
    vlx_serializer.saveVLT(vlt_path, cam.get()); VL_CHECK(!vlx_serializer.error());
    cam = vlx_serializer.loadVLT(vlt_path)->as<Camera>(); VL_CHECK(!vlx_serializer.error());

    sceneManager()->tree()->actors()->push_back(act1.get());
    sceneManager()->tree()->actors()->push_back(act2.get());
    sceneManager()->tree()->actors()->push_back(act3.get());
    sceneManager()->tree()->actors()->push_back(act4.get());
    rendering()->as<Rendering>()->camera()->setModelingMatrix( cam->modelingMatrix() );
  }

  // raw i/o serialization test with no visualization using writeResource()/loadResource()
  void smokeTest()
  {
    Log::notify(appletInfo());

    // --- --- ResourceDatabase --- ---
    ref<ResourceDatabase> res_db = new ResourceDatabase;
    res_db->setObjectName("resource db name");

    ref<Geometry> geom1 = makeTeapot( vec3(0,0,0), 10, 4 );
    geom1->setObjectName("teapot with normals");
    geom1->computeNormals();
    geom1->setColorArray( geom1->normalArray() );
    geom1->setTexCoordArray( 0, geom1->normalArray() );
    // PatchParameter
    geom1->drawCalls()->at(0)->setPatchParameter( new PatchParameter );

    ref<Geometry> geom2 = makeTeapot( vec3(0,0,0), 10, 4 );
    geom2->setObjectName("teapot without normals");
    TriangleStripGenerator::stripfy(geom2.get(), 22, false, false, true);
    geom2->mergeDrawCallsWithMultiDrawElements(PT_TRIANGLE_STRIP);
    res_db->resources().push_back( geom2.get() );

    ref<Geometry> geom3 = makeTeapot( vec3(0,0,0), 10, 4 );
    geom3->setObjectName("teapot without normals again");
    TriangleStripGenerator::stripfy(geom3.get(), 22, false, false, true);
    geom3->mergeDrawCallsWithPrimitiveRestart(PT_TRIANGLE_STRIP);
    res_db->resources().push_back( geom3.get() );

    ref<Geometry> geom4 = makeBox( vec3(0,0,0), 10, 10, 10 );
    res_db->resources().push_back( geom4.get() );

    ref<Geometry> geom5 = makeIcosphere( vec3(0,0,0), 10, 0 );
    geom5->computeNormals();
    geom5->setColorArray( geom5->normalArray() );
    geom5->makeGLESFriendly();
    geom5->convertToVertexAttribs();
    geom5->drawCalls()->push_back( geom5->drawCalls()->back() );
    res_db->resources().push_back( geom5.get() );

    ref<Effect> fx = new Effect;
    fx->setObjectName("effect's name");
    fx->shader()->setObjectName("shader's name");
    fx->shader()->enable(EN_LIGHTING);
    fx->shader()->enable(EN_DEPTH_TEST);
    fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
    fx->shader()->setRenderState( new Light, 0 ); fx->shader()->gocLight(0)->setObjectName("light's name");
    fx->shader()->setRenderState( new Light, 1 );
    fx->shader()->setRenderState( new ClipPlane(10, vec3(1,2,3)), 0 );
    fx->shader()->setRenderState( new ClipPlane(10, vec3(1,2,3)), 1 );

    fx->shader()->gocBlendEquation();

    fx->shader()->gocColor()->setValue( vl::crimson );
    fx->shader()->gocSecondaryColor()->setValue( vl::yellow.rgb() );
    fx->shader()->gocNormal()->setValue( fvec3(1,2,3) );
    fx->shader()->gocVertexAttrib(0)->setValue( vl::royalblue );
    fx->shader()->gocVertexAttrib(1)->setValue( vl::red );
    fx->shader()->gocVertexAttrib(1)->setObjectName("my vertex attrib name");

    ref<Actor> act = new Actor( geom1.get(), fx.get(), new Transform );
    act->setObjectName("actor's name");
    act->setObjectName("actor's name");
    act->transform()->translate(1, 2 ,3);
    for(int i=0; i<10; ++i)
    {
      ref<Transform> tr = new Transform;
      tr->setObjectName("some transform's name");
      switch( rand() % 4 )
      {
      case 0: tr->translate( random(-5,+5), random(-5,+5), random(-5,+5) ); break;
      case 1: tr->rotate( random(0, 360), random(0,1), random(0,1), random(0,1) ); break;
      case 2: tr->scale( random(1,10), random(1,10), random(1,10) ); break;
      case 3: tr->setLocalMatrix( mat4::getPerspective(30, 1, 1, 1000) ); break;
      }
      act->transform()->addChild( tr.get() );
    }

    if (vl::Has_GLSL)
    {
      GLSLProgram* glsl = fx->shader()->gocGLSLProgram();
      glsl->setObjectName("glsl shader name");
      glsl->attachShader( new GLSLVertexShader        ("/glsl/smooth_triangle.vs") );
      glsl->attachShader( new GLSLFragmentShader      ("/glsl/perpixellight.fs") );
      if (vl::Has_GL_ARB_tessellation_shader)
      {
        glsl->attachShader( new GLSLTessControlShader   ("/glsl/smooth_triangle.tcs") );
        glsl->attachShader( new GLSLTessEvaluationShader("/glsl/smooth_triangle.tes") );
      }
      if (vl::Has_Geometry_Shader)
        glsl->attachShader( new GLSLGeometryShader      ("/glsl/smooth_triangle.gs") );
  #if 0
      glsl->shader(0)->setPath(""); glsl->shader(0)->setSource("");
      glsl->shader(1)->setPath(""); glsl->shader(1)->setSource("");
      glsl->shader(2)->setPath(""); glsl->shader(2)->setSource("");
      glsl->shader(3)->setPath(""); glsl->shader(3)->setSource("");
      glsl->shader(4)->setPath(""); glsl->shader(4)->setSource("");
  #endif
      // glslprogram uniforms
      glsl->gocUniform("dummy_uniform1")->setUniformF(123.456f);
      glsl->gocUniform("dummy_uniform2")->setUniformD(789.123);
      glsl->gocUniform("dummy_uniform2")->setUniformI(3141592);

      glsl->bindFragDataLocation(0, "frag_data_location_dummy1");
      glsl->bindFragDataLocation(1, "frag_data_location_dummy2");
      glsl->addAutoAttribLocation(0, "attrib_position");
      glsl->addAutoAttribLocation(1, "attrib_normal");
      glsl->addAutoAttribLocation(3, "attrib_color");
    }

    // actor uniforms
#if 1 
    act->gocUniform("mic_uniform1")->setUniformF(24.0f);
    act->gocUniform("mic_uniform2")->setUniformI(11);
    act->gocUniform("mic_uniform3")->setUniformD(100);

    fvec4 float4(1,2,3,4);
    ivec4 int4(5,6,7,8);
    dvec4 double4(9,0,1,2);

    act->gocUniform("mic_uniform4")->setUniform(float4);
    act->gocUniform("mic_uniform5")->setUniform(int4);
    act->gocUniform("mic_uniform6")->setUniform(double4);

    fvec4 float4_arr[] = { fvec4(1,2,3,4), fvec4(5,6,7,8), fvec4(9,0,1,2) };
    ivec4 int4_arr[] = { ivec4(1,2,3,4), ivec4(5,6,7,8), ivec4(9,0,1,2) };
    dvec4 double4_arr[] = { dvec4(1,2,3,4), dvec4(5,6,7,8), dvec4(9,0,1,2) };

    act->gocUniform("mic_uniform7")->setUniform(3, float4_arr);
    act->gocUniform("mic_uniform8")->setUniform(3, int4_arr);
    act->gocUniform("mic_uniform9")->setUniform(3, double4_arr);

    float float1_arr[]   = { 1, 2, 3, 4, 5 };
    int int1_arr[]       = { 1, 2, 3, 4, 5 };
    double double1_arr[] = { 1, 2, 3, 4, 5 };

    act->gocUniform("mic_uniform10")->setUniform(5, float1_arr);
    act->gocUniform("mic_uniform11")->setUniform(5, int1_arr);
    act->gocUniform("mic_uniform12")->setUniform(5, double1_arr);

    act->gocUniform("mat4x4")->setUniform( mat4::getTranslation(10,20,30) );
    act->gocUniform("mat3x3")->setUniform( mat3::getTranslation(40,50) );
    act->gocUniform("mat2x2")->setUniform( mat2(1,2,3,4) );

    mat4 mat4_arr[] = { mat4::getTranslation(10,20,30), mat4::getTranslation(40,50,60), mat4::getTranslation(70,80,90) };
    act->gocUniform("mat4x4_array")->setUniform( 3, mat4_arr );

    // act->gocUniform("empty");
#endif

    // Textures
    fx->shader()->gocTextureSampler(0)->setTexture( new Texture );
    fx->shader()->gocTextureSampler(0)->texture()->prepareTexture2D( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN );
    // TextureSampler can override a Texture's TexParameter with this.
    fx->shader()->gocTextureSampler(0)->setTexParameter( new TexParameter );
    ref<Texture> tex;
    tex = new Texture; tex->prepareTexture1D( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture1D( 10, TF_RGBA ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2D( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2D( 10, 20, TF_RGBA ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture3D( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture3D( 10, 20, 30, TF_RGBA ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTextureCubemap( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTextureCubemap( 10, 20, TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTextureRectangle( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTextureRectangle( 10, 20, TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture1DArray( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture1DArray( 10, 100, TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2DArray( new Image("ignore_file_not_found.jpg"), TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2DArray( 10, 20, 100, TF_UNKNOWN ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2DMultisample( 10, 20, TF_RGBA, 16, false ); res_db->resources().push_back( tex.get() );
    tex = new Texture; tex->prepareTexture2DMultisampleArray( 10, 20, 100, TF_RGBA, 16, false ); res_db->resources().push_back( tex.get() );
    ref<ArrayFloat3> arr = new ArrayFloat3; arr->resize(10);
    tex = new Texture; tex->prepareTextureBuffer( TF_RGBA, arr->bufferObject() ); res_db->resources().push_back( tex.get() );
    tex->setObjectName("some texture's name");

    // PixelLODEvaluator
    ref<PixelLODEvaluator> pix_eval = new PixelLODEvaluator;
    pix_eval->pixelRangeSet().push_back(11);
    pix_eval->pixelRangeSet().push_back(22);
    pix_eval->pixelRangeSet().push_back(33);
    act->setLODEvaluator( pix_eval.get() );

    // DistanceLODEvaluator
    ref<DistanceLODEvaluator> dist_eval = new DistanceLODEvaluator;
    dist_eval->distanceRangeSet().push_back(11);
    dist_eval->distanceRangeSet().push_back(22);
    dist_eval->distanceRangeSet().push_back(33);
    act->setLODEvaluator( dist_eval.get() );

    // DepthSortCallback
    act->actorEventCallbacks()->push_back( new DepthSortCallback );

    // Actor
    res_db->resources().push_back(act.get());

    // Camera
    res_db->resources().push_back( new Camera );
    res_db->resources().back()->setObjectName("camera's name");

    // Viewport
    res_db->resources().push_back( new Viewport );
    res_db->resources().back()->setObjectName("viewport's name");

    // Expand resources: extracts and sorts Shaders, Effects, Renderables, RenderStates, Transforms etc. so that the resulting VLX files look more readable.
    // Without this the resources are inlined in the VLXStructure that uses it the first time.
    expandResourceDatabase(res_db.get());

    String vlx_path;
    vlx_path = globalSettings()->defaultDataPath() + "/vlx/smoke_test.vlt";
    /*res_db =*/ vl::loadResource(vlx_path); // load first (backwards compatibility check)
    vl::writeResource(vlx_path, res_db.get()); // save
    res_db = vl::loadResource(vlx_path); // load
    vl::writeResource(vlx_path, res_db.get()); // save again

    vlx_path = globalSettings()->defaultDataPath() + "/vlx/smoke_test.vlb";
    /*res_db =*/ vl::loadResource(vlx_path); // load first (backwards compatibility check)
    vl::writeResource(vlx_path, res_db.get()); // save
    res_db = vl::loadResource(vlx_path); // load
    vl::writeResource(vlx_path, res_db.get()); // save
  }

};

// Have fun!

BaseDemo* Create_App_VLX() { return new App_VLX; }
