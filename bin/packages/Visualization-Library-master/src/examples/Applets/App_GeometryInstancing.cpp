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
#include <vlGraphics/DrawElements.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/GLSL.hpp>

class App_GeometryInstancing: public BaseDemo
{
public:
  virtual void updateScene()
  {
    // note that this creates a delay on the application of the camera transform

    bool update_matrices = !mTimer.isStarted( ) || mTimer.elapsed() > 1.0f/30.0f;

    if (update_matrices)
      mTimer.start();
    else
      return;

    vl::fmat4 mat_array[1000];
    vl::fmat3 mat_array_norm[1000];

    for (int batch = 0; batch < 10; ++batch)
    {
      // update matrices
      vl::fmat4 view = (vl::fmat4)rendering()->as<vl::Rendering>()->camera()->viewMatrix();
      for(int k=0; k<100; ++k)
      {
        int i = batch*100 + k;

        float x = float( i % 10)  * 5.0f;
        float y = float( ( i % 100) / 10 ) * 5.0f;
        float z = float( i / 100) * 5.0f;
        float deg = 0;
        deg = (float)vl::Time::currentTime() * 240.0f;

        mat_array[i].rotate(deg, rot[batch*100+k]);
        mat_array[i].translate(x,y,z);
        mat_array[i] = view * mat_array[i];
        mat_array_norm[i] = mat_array[i].getInverse().transpose().get3x3();
      }

      // update the uniforms
      _model_view_matrix[batch]->setUniform(100, mat_array      + batch * 100);
      _normal_matrix    [batch]->setUniform(100, mat_array_norm + batch * 100);
    }
  }

  virtual void initEvent()
  {
    /* This test shows how to implement an advanced form of geometry instancing on the GPU.
       The test creates 10 batches of 100 objects each, each batch is assigned to a GLSL program,
       the matrix and normal matrices are passed as uniforms. One actor is sufficient to represent
       a batch or 100 objects. The matrices are updated once every 30 seconds thus gaining a
       significant performance improvement.
    */

    if (!vl::Has_GL_EXT_draw_instanced)
    {
      vl::Log::error("GL_EXT_draw_instanced not supported.\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    vl::ref<vl::Geometry> box_set = vl::makeBox( vl::vec3(0,0,0), 2, 2, 2, false);
    box_set->computeNormals();
    box_set->setObjectName("Box Set");

    /* setting multiple instances is as easy as calling this function! */
    vl::DrawArrays* draw_arrays = vl::cast<vl::DrawArrays>( box_set->drawCalls()->at(0) );
    VL_CHECK(draw_arrays)
    draw_arrays->setInstances(100);

    /* create a AABB that approximately reflects the geometry generated by the shader */
    box_set->setBoundingBox( vl::AABB(vl::vec3(-1,-1,-1), vl::vec3(45+1, 45+1, 45+1)) );

    for(int batch=0; batch<10; ++batch)
    {
      /* box effect */
      vl::ref<vl::Effect> fx = new vl::Effect;
      fx->shader()->setRenderState( new vl::Light, 0 );
      fx->shader()->enable(vl::EN_LIGHTING);
      fx->shader()->enable(vl::EN_CULL_FACE);
      fx->shader()->enable(vl::EN_DEPTH_TEST);

      /* use a multi instancing vertex shader */
      vl::GLSLProgram* glsl = fx->shader()->gocGLSLProgram();
      glsl->attachShader( new vl::GLSLVertexShader("/glsl/geometry_instancing.vs") );

      /* add the objects to the scene */

      vl::Actor* actor = sceneManager()->tree()->addActor( box_set.get(), fx.get() );

      _model_view_matrix[batch] = new vl::Uniform("model_view_matrix");
      _normal_matrix[batch]     = new vl::Uniform("normal_matrix");

      #if 1
        // the uniforms must be per actor
        actor->gocUniformSet()->setUniform( _model_view_matrix[batch].get() );
        actor->gocUniformSet()->setUniform( _normal_matrix[batch].get() );
      #else
        // is not coorect to send the uniform per-shader
        fx->shader()->setUniform( _model_view_matrix[batch].get() );
        fx->shader()->setUniform( _normal_matrix[batch].get() );
      #endif
    }

    /* aabb effect */
    vl::ref<vl::Effect> box_fx = new vl::Effect;
    box_fx->shader()->setRenderState( new vl::Light, 0 );
    box_fx->shader()->enable(vl::EN_DEPTH_TEST);
    box_fx->shader()->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    box_fx->shader()->gocColor()->setValue(vl::red);

    /* shows bounding box */
    vl::ref<vl::Geometry> box = vl::makeBox( box_set->boundingBox() );
    box->setObjectName("Wire box");
    box->computeNormals();
    sceneManager()->tree()->addActor( box.get(), box_fx.get() );

    for(unsigned i=0; i<1000; ++i)
    {
      rot[i].x() = rand()%100 - 50.0f;
      rot[i].y() = rand()%100 - 50.0f;
      rot[i].z() = rand()%100 - 50.0f;
      rot[i].normalize();
    }
  }

  vl::ref<vl::Uniform> _model_view_matrix[10];
  vl::ref<vl::Uniform> _normal_matrix[10];
  vl::fvec3 rot[1000];
  vl::Time mTimer;
};

// Have fun!

BaseDemo* Create_App_GeometryInstancing() { return new App_GeometryInstancing; }
