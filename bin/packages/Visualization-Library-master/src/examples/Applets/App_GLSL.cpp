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
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/GLSL.hpp>

using namespace vl;

class App_GLSL: public BaseDemo
{
public:
  App_GLSL()
  {
  }

  void initEvent()
  {
    if (!Has_GLSL)
    {
      Log::error("OpenGL Shading Language not supported.\n");
      Time::sleep(2000);
      exit(1);
    }

    trackball()->setPivot(vl::vec3(4.5f, 4.5f, 0.0f));

    Log::notify(appletInfo());

    ref<ResourceDatabase> res_db = loadResource("/models/3ds/monkey.3ds");
    mModel = res_db->get<Geometry>(0);
    mModel->computeNormals();
    AABB aabb = mModel->boundingBox();
    mModel->transform( mat4::getTranslation(-aabb.center()) );

    ref<Light> light = new Light;

    ref<GLSLProgram> glsl;

    ref<GLSLVertexShader> perpixellight_vs = new GLSLVertexShader("/glsl/perpixellight.vs");

    //glsl = new GLSLProgram;
    //mGLSL.push_back(glsl);
    //glsl->attachShader( perpixellight_vs.get() );
    //glsl->attachShader( new GLSLFragmentShader("/glsl/perpixellight.fs") );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( new GLSLVertexShader("/glsl/examples/toyball.vs") );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/toyball.fs") );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( perpixellight_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/perpixellight_toon.fs") );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( perpixellight_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/heat.fs") );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( perpixellight_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/perpixellight_interlaced.fs") );

    ref<GLSLVertexShader>   noise_vs   = new GLSLVertexShader("/glsl/examples/noise.vs");
    ref<GLSLFragmentShader> noise3D_fs = new GLSLFragmentShader("/glsl/noise3D.glsl");

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/alien.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/cloud.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/granite.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/marble.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/marble2.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/turbulence.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/woody.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( noise_vs.get() );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/ribbon.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( new GLSLVertexShader("/glsl/examples/noisebump.vs") );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/noisebump.fs") );
    glsl->attachShader( noise3D_fs.get() );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( new GLSLVertexShader("/glsl/examples/hatching.vs") );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/hatching.fs") );

    glsl = new GLSLProgram;
    mGLSL.push_back(glsl);
    glsl->attachShader( new GLSLVertexShader("/glsl/examples/stripes.vs") );
    glsl->attachShader( new GLSLFragmentShader("/glsl/examples/stripes.fs") );

    if (Has_Geometry_Shader)
    {
      glsl = new GLSLProgram;
      mGLSL.push_back(glsl);
      // a vertex shader is always needed when using geometry shaders
      glsl->attachShader( new GLSLVertexShader("/glsl/examples/diffuse.vs") );
      glsl->attachShader( new GLSLGeometryShader("/glsl/examples/triangle_fur.gs") );
      glsl->setGeometryInputType(GIT_TRIANGLES);
      glsl->setGeometryOutputType(GOT_TRIANGLE_STRIP);
      glsl->setGeometryVerticesOut( 3*6 );
    }
    else
    {
      Log::print("Geomery shaders not supported.\n");
    }

    mGLSL.resize(4*4);

    for(int y=0; y<4; ++y)
    {
      for(int x=0; x<4; ++x)
      {
        ref<Effect> fx = new Effect;
        fx->shader()->setRenderState( light.get(), 0 );
        fx->shader()->enable(EN_LIGHTING);
        fx->shader()->enable(EN_DEPTH_TEST);
        // fx->shader()->enable(EN_CULL_FACE);
        GLSLProgram* glsl = mGLSL[x + y*4].get();
        if (glsl)
          fx->shader()->setRenderState(glsl);
        ref<Transform> tr = new Transform;
        real grid= 3.0;
        tr->setLocalAndWorldMatrix( mat4::getTranslation(x*grid, y*grid, 0) );
        sceneManager()->tree()->addActor( mModel.get(), fx.get(), tr.get() );
      }
    }
  }

  void fileDroppedEvent(const std::vector<String>& files)
  {
    ref<ResourceDatabase> db = loadResource(files[0]);
    Geometry* geom = db->get<Geometry>(0);
    if (!geom)
      geom = db->get<Actor>(0)->lod(0)->as<Geometry>();
    if (geom)
    {
      mModel->shallowCopyFrom( *geom );
      mModel->computeBounds();
      AABB aabb = mModel->boundingBox();
      real norm = aabb.width() > aabb.height() ? aabb.width() : aabb.height();
      norm = norm > aabb.depth() ? norm : aabb.depth();
      real scale = 1.5;
      mModel->transform( vl::mat4::getTranslation( -aabb.center() ) );
      mModel->transform( vl::mat4::getScaling( scale / norm, scale / norm, scale / norm ) );
    }
  }

  void updateScene()
  {
  }

protected:
  ref<Geometry> mModel;
  std::vector< ref<GLSLProgram> > mGLSL;
};

// Have fun!

BaseDemo* Create_App_GLSL() { return new App_GLSL; }
