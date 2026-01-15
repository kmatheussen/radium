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
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>

#include <vlCore/FileSystem.hpp>

class App_TessellationShader: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    if(!vl::Has_GL_Version_4_0)
    {
      vl::Log::error("This test requires OpenGL 4!\n");
      vl::Time::sleep(2000);
      exit(1);
    }

    const int   patch_count      = 128;
    const float world_size       = 2500.0;
    const float height_scale     = 150.0;
    const float pixel_per_edge   = 16.0f;
    const float max_tessellation = 64.0f;

    vl::ref< vl::Geometry > geom_patch = makeGrid( vl::vec3(), world_size, world_size, patch_count, patch_count, false );

    geom_patch->convertToVertexAttribs();

    // patch parameter associated to the draw call
    vl::ref<vl::PatchParameter> patch_param = new vl::PatchParameter;
    patch_param->setPatchVertices(4);
    geom_patch->drawCalls()->at(0)->setPatchParameter( patch_param.get() );
    geom_patch->drawCalls()->at(0)->setPrimitiveType(vl::PT_PATCHES);

    vl::ref<vl::Texture> hmap = new vl::Texture("/images/ps_height_4k.jpg", vl::TF_RED, false, false);
    vl::ref<vl::Texture> tmap = new vl::Texture("/images/ps_texture_4k.jpg", vl::TF_RGBA, true, false);

    hmap->getTexParameter()->setMinFilter(vl::TPF_LINEAR);
    hmap->getTexParameter()->setMagFilter(vl::TPF_LINEAR);

    // tessellated patches fx
    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->enable(vl::EN_DEPTH_TEST);
    fx->shader()->gocTextureSampler(0)->setTexture( hmap.get() );
    fx->shader()->gocTextureSampler(1)->setTexture( tmap.get() );

    // bind all the necessary stages to the GLSLProgram
    mGLSL = fx->shader()->gocGLSLProgram();
    mGLSL->attachShader( new vl::GLSLVertexShader("glsl/tess_grid.vs") );
    mGLSL->attachShader( new vl::GLSLTessControlShader("glsl/tess_grid.tcs") );
    mGLSL->attachShader( new vl::GLSLTessEvaluationShader("glsl/tess_grid.tes") );
    mGLSL->attachShader( new vl::GLSLFragmentShader("glsl/tess_grid.fs") );
    mGLSL->gocUniform("pixel_per_edge")->setUniformF(pixel_per_edge);
    mGLSL->gocUniform("max_tessellation")->setUniformF(max_tessellation);
    mGLSL->gocUniform("screen_size")->setUniform(vl::fvec2(512,512));
    mGLSL->gocUniform("world_size")->setUniformF(world_size);
    mGLSL->gocUniform("height_scale")->setUniformF(height_scale);
    mGLSL->gocUniform("tex_heghtmap")->setUniformI(0);
    mGLSL->gocUniform("tex_diffuse")->setUniformI(1);

    mGLSL->addAutoAttribLocation( 0, "vl_Position" );

    // tessellated patches fx_wire
    vl::ref<vl::Effect> fx_wire = new vl::Effect;
    fx_wire->shader()->enable(vl::EN_DEPTH_TEST);
    fx_wire->shader()->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
    fx_wire->shader()->gocTextureSampler(0)->setTexture( hmap.get() );
    fx_wire->shader()->gocPolygonOffset()->set(-1.0f, -1.0f);
    fx_wire->shader()->enable(vl::EN_POLYGON_OFFSET_LINE);

    // bind all the necessary stages to the GLSLProgram
    mGLSLWire = fx_wire->shader()->gocGLSLProgram();
    mGLSLWire->attachShader( new vl::GLSLVertexShader("glsl/tess_grid.vs") );
    mGLSLWire->attachShader( new vl::GLSLTessControlShader("glsl/tess_grid.tcs") );
    mGLSLWire->attachShader( new vl::GLSLTessEvaluationShader("glsl/tess_grid.tes") );
    mGLSLWire->attachShader( new vl::GLSLFragmentShader("glsl/tess_grid_wire.fs") );
    mGLSLWire->gocUniform("pixel_per_edge")->setUniformF(pixel_per_edge);
    mGLSLWire->gocUniform("max_tessellation")->setUniformF(max_tessellation);
    mGLSLWire->gocUniform("screen_size")->setUniform(vl::fvec2(512,512));
    mGLSLWire->gocUniform("world_size")->setUniformF(world_size);
    mGLSLWire->gocUniform("height_scale")->setUniformF(height_scale);
    mGLSLWire->gocUniform("tex_heghtmap")->setUniformI(0);
    mGLSLWire->gocUniform("wire_color")->setUniform(vl::lightgreen);

    mGLSLWire->addAutoAttribLocation( 0, "vl_Position" );

    sceneManager()->tree()->addActor( geom_patch.get(), fx.get(), NULL )->setRenderRank(0);
    
    mWireActor = sceneManager()->tree()->addActor( geom_patch.get(), fx_wire.get(), NULL );
    mWireActor->setRenderRank(1);

    // debugging
    #if 0
      // base patch grid
      vl::ref< vl::Geometry > geom_quads = makeGrid(vl::fvec3(), world_size,world_size, patch_count,patch_count, true);
      geom_quads->setColor(vl::red);
      // base patch grid fx
      vl::ref<vl::Effect> fx_grid = new vl::Effect;
      fx_grid->shader()->gocPolygonMode()->set(vl::PM_LINE, vl::PM_LINE);
      // add grid
      sceneManager()->tree()->addActor( geom_quads.get(), fx_grid.get(), NULL )->setRenderRank(2);
    #endif
  }

  // interactively change the inner/outer tessellation levels
  void keyPressEvent(unsigned short, vl::EKey key)
  {
    if (key == vl::Key_Space)
      mWireActor->setEnableMask( mWireActor->enableMask() ? 0 : 1 );
  }

  void resizeEvent(int w, int h)
  {
    BaseDemo::resizeEvent(w,h);
    mGLSL->gocUniform("screen_size")->setUniform(vl::fvec2((float)w,(float)h));
    mGLSLWire->gocUniform("screen_size")->setUniform(vl::fvec2((float)w,(float)h));
  }

protected:
  vl::GLSLProgram* mGLSL;
  vl::GLSLProgram* mGLSLWire;
  vl::Actor* mWireActor;
};

// Have fun!

BaseDemo* Create_App_TessellationShader() { return new App_TessellationShader; }

class App_TessellationShaderTri: public BaseDemo
{
public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());

    // hemisphere base geometry
    vl::ref< vl::Geometry > geom_patch = new vl::Geometry;

    // hemisphere base geometry vertices
    vl::ref<vl::ArrayFloat3 > verts = new vl::ArrayFloat3;
    verts->resize(12);

    verts->at(0)  = vl::fvec3(1,0,0);
    verts->at(1)  = vl::fvec3(0,1,0);
    verts->at(2)  = vl::fvec3(0,0,1);
    
    verts->at(3)  = vl::fvec3(1,0,0);
    verts->at(4)  = vl::fvec3(0,0,-1);
    verts->at(5)  = vl::fvec3(0,1,0);
    
    verts->at(6)  = vl::fvec3(0,0,-1);
    verts->at(7)  = vl::fvec3(-1,0,0);
    verts->at(8)  = vl::fvec3(0,1,0);
    
    verts->at(9)  = vl::fvec3(-1,0,0);
    verts->at(10) = vl::fvec3(0,0,1);
    verts->at(11) = vl::fvec3(0,1,0);
    
    // hemisphere base geometry vertex colors
    vl::ref<vl::ArrayFloat3 > cols = new vl::ArrayFloat3;
    cols->resize(12);

    cols->at(0)  = vl::fvec3(1,0,0);
    cols->at(1)  = vl::fvec3(1,0,0);
    cols->at(2)  = vl::fvec3(1,0,0);
    
    cols->at(3)  = vl::fvec3(0,1,0);
    cols->at(4)  = vl::fvec3(0,1,0);
    cols->at(5)  = vl::fvec3(0,1,0);
    
    cols->at(6)  = vl::fvec3(1,1,0);
    cols->at(7)  = vl::fvec3(1,1,0);
    cols->at(8)  = vl::fvec3(1,1,0);
    
    cols->at(9)  = vl::fvec3(0,0,1);
    cols->at(10) = vl::fvec3(0,0,1);
    cols->at(11) = vl::fvec3(0,0,1);
    
    // vertex array
    geom_patch->setVertexArray( verts.get() );

    // color array
    geom_patch->setColorArray( cols.get() );
    
    // draw call
    vl::ref< vl::DrawArrays> da = new vl::DrawArrays(vl::PT_PATCHES, 0, verts->size());
    geom_patch->drawCalls()->push_back(da.get());
    
    // patch parameter associated to the draw call
    vl::ref<vl::PatchParameter> patch_param = new vl::PatchParameter;
    patch_param->setPatchVertices(3);
    da->setPatchParameter( patch_param.get() );

    // effect: light + depth testing
    vl::ref<vl::Effect> fx = new vl::Effect;
    fx->shader()->enable(vl::EN_DEPTH_TEST);

    // bind all the necessary stages to the GLSLProgram
    mGLSL = fx->shader()->gocGLSLProgram();
    mGLSL->attachShader( new vl::GLSLVertexShader("glsl/smooth_triangle.vs") );
    mGLSL->attachShader( new vl::GLSLTessControlShader("glsl/smooth_triangle.tcs") );
    mGLSL->attachShader( new vl::GLSLTessEvaluationShader("glsl/smooth_triangle.tes") );
    mGLSL->attachShader( new vl::GLSLGeometryShader("glsl/smooth_triangle.gs") );
    mGLSL->gocUniform("Outer")->setUniformF(10.0f);
    mGLSL->gocUniform("Inner")->setUniformF(10.0f);
    mGLSL->gocUniform("Radius")->setUniformF(1.0f);

    sceneManager()->tree()->addActor( geom_patch.get(), fx.get(), NULL );
  }

  // interactively change the inner/outer tessellation levels
  void keyPressEvent(unsigned short, vl::EKey key)
  {
    float outer = 0;
    float inner = 0;
    mGLSL->gocUniform("Outer")->getUniform(&outer);
    mGLSL->gocUniform("Inner")->getUniform(&inner);

    if (key == vl::Key_Left)
      outer--;
    else
    if (key == vl::Key_Right)
      outer++;
    else
    if (key == vl::Key_Down)
      inner--;
    else
    if (key == vl::Key_Up)
      inner++;

    inner = inner < 1 ? 1 : inner;
    outer = outer < 1 ? 1 : outer;

    mGLSL->gocUniform("Outer")->setUniformF(outer);
    mGLSL->gocUniform("Inner")->setUniformF(inner);

    vl::Log::print( vl::Say("outer = %n, inner = %n\n") << outer << inner );
  }

protected:
  vl::GLSLProgram* mGLSL;
};

// Have fun!

BaseDemo* Create_App_TessellationShaderTri() { return new App_TessellationShaderTri; }
