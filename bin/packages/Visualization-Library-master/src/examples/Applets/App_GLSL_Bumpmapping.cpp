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
#include <vlCore/VisualizationLibrary.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/GLSL.hpp>

const int TORUS_SEGS = 25;

class App_GLSL_Bumpmapping: public BaseDemo
{
public:
  void initEvent()
  {
    if (!vl::Has_GLSL)
    {
      vl::Log::error("OpenGL Shading Language not supported.\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    vl::Log::notify(appletInfo());

    mTransform = new vl::Transform;

    // generate torus, with normals and uv coords
    vl::ref<vl::Geometry> model = vl::makeTorus( vl::vec3(0,0,0), 10, 2, TORUS_SEGS, TORUS_SEGS, 2.0f );
    model->transform( vl::mat4::getRotation( 45.0f, 1.0f, 1.0f, 0.0f ) );

    // setup effect
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->setRenderState( new vl::Light, 0 );
    effect->shader()->enable(vl::EN_LIGHTING);
    effect->shader()->enable(vl::EN_DEPTH_TEST);
    effect->shader()->enable(vl::EN_CULL_FACE);
    mTorus = sceneManager()->tree()->addActor( model.get(), effect.get(), mTransform.get() );

    // setup texture
    vl::ref<vl::Texture> texture0 = new vl::Texture;
    texture0->prepareTexture2D("images/normalmap.jpg", vl::TF_RGBA);
    effect->shader()->gocTextureSampler(0)->setTexture(texture0.get());
    texture0->getTexParameter()->setAnisotropy(16.0);
    texture0->getTexParameter()->setMagFilter(vl::TPF_LINEAR);
    texture0->getTexParameter()->setMinFilter(vl::TPF_LINEAR_MIPMAP_LINEAR);

    // setup GLSL shader
    mGLSL = effect->shader()->gocGLSLProgram();
    mGLSL->attachShader( new vl::GLSLVertexShader("/glsl/bumpmap.vs") );
    mGLSL->attachShader( new vl::GLSLFragmentShader("/glsl/bumpmap.fs") );
    // samper0
    vl::ref<vl::Uniform> sampler0 = new vl::Uniform("sampler0");
    sampler0->setUniformI(0);
    mGLSL->setUniform( sampler0.get() );
    // light_obj_space_pos
    mLightObjSpacePosition = new vl::Uniform("light_obj_space_pos");
    mGLSL->setUniform( mLightObjSpacePosition.get() );

    // compute the tangent vector for each vertex

    vl::ref<vl::ArrayFloat3> tangent = new vl::ArrayFloat3;
    tangent->resize( model->vertexArray()->size() );

    vl::Geometry::computeTangentSpace(
      model->vertexArray()->size(), 
      (vl::fvec3*)model->vertexArray()->ptr(), 
      (vl::fvec3*)model->normalArray()->ptr(), 
      (vl::fvec2*)model->texCoordArray(0)->ptr(),
      model->drawCalls()->at(0),
      tangent->begin(), 
      NULL );

    // bind the tangent vertex attribute
    mGLSL->linkProgram();
    // note that you need to link the GLSL program before calling this
    int tangent_idx = mGLSL->getAttribLocation("tangent"); VL_CHECK( tangent_idx != -1 );
    model->setVertexAttribArray(tangent_idx, tangent.get() );

    // visualize the TBN vectors
    visualizeTangentSpace( model.get(), tangent.get() );
  }

  void visualizeTangentSpace(const vl::Geometry* model, const vl::ArrayFloat3* tangent)
  {
    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_DEPTH_TEST);

    vl::ref<vl::ArrayFloat3> ntb_verts = new vl::ArrayFloat3;
    ntb_verts->resize( model->vertexArray()->size() * 6 );

    vl::ref<vl::ArrayFloat4> ntb_cols = new vl::ArrayFloat4;
    ntb_cols->resize( model->vertexArray()->size() * 6 );

    vl::fvec3* verts = (vl::fvec3*)model->vertexArray()->ptr();
    vl::fvec3* norms = (vl::fvec3*)model->normalArray()->ptr();

    float tick_size = 0.5f;

    for( size_t i=0; i<model->vertexArray()->size(); ++i )
    {
      vl::fvec3 bitangent = vl::cross( norms[i], (*tangent)[i] );

      (*ntb_verts)[i*6 + 0] = verts[i];
      (*ntb_verts)[i*6 + 1] = verts[i] + norms[i] * tick_size;
      (*ntb_verts)[i*6 + 2] = verts[i];
      (*ntb_verts)[i*6 + 3] = verts[i] + (*tangent)[i] * tick_size;
      (*ntb_verts)[i*6 + 4] = verts[i];
      (*ntb_verts)[i*6 + 5] = verts[i] + bitangent * tick_size;

      (*ntb_cols)[i*6 + 0] = vl::red;
      (*ntb_cols)[i*6 + 1] = vl::red;
      (*ntb_cols)[i*6 + 2] = vl::green;
      (*ntb_cols)[i*6 + 3] = vl::green;
      (*ntb_cols)[i*6 + 4] = vl::blue;
      (*ntb_cols)[i*6 + 5] = vl::blue;
    }

    vl::ref<vl::Geometry> NTBGeom = new vl::Geometry;
    NTBGeom->setVertexArray( ntb_verts.get() );
    NTBGeom->setColorArray( ntb_cols.get() );
    NTBGeom->drawCalls()->push_back( new vl::DrawArrays(vl::PT_LINES, 0, ntb_verts->size() ) );
    sceneManager()->tree()->addActor( NTBGeom.get(), effect.get(), mTransform.get() );
  }

  void updateScene() 
  {
    // update the torus tranform
    mTransform->setLocalMatrix( vl::mat4::getRotation( vl::Time::currentTime() * 5.0f, 0, -1, 1 ) );
    mTransform->computeWorldMatrix();

    // world to object space matrix
    vl::mat4 obj_mat = mTransform->worldMatrix().getInverse();

    // project camera position from world to object space
    vl::vec3 camera_pos = rendering()->as<vl::Rendering>()->camera()->modelingMatrix().getT();
    vl::vec3 camera_pos_obj_space = obj_mat * camera_pos;
    mLightObjSpacePosition->setUniform( (vl::fvec3)camera_pos_obj_space );
  }

protected:
    vl::ref<vl::GLSLProgram> mGLSL;
    vl::ref<vl::Uniform> mLightObjSpacePosition;
    vl::ref<vl::Actor> mTorus;
    vl::ref<vl::Transform> mTransform;

};

// Have fun!

BaseDemo* Create_App_GLSL_Bumpmapping() { return new App_GLSL_Bumpmapping; }
