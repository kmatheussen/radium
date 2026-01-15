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

#include <vlGraphics/Terrain.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/GLSL.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

void Terrain::init()
{
  mChunks.clear();

  if (mWidth <= 0 || mHeight <= 0 || mDepth <= 0 || mDetailRepetitionCount <= 0)
  {
    Log::error(
        Say("Terrain initialization failed: invalid parameters.\n"
             "width = %n\n"
             "hieght = %n\n"
             "depth = %n\n"
             "detail repetition = %n\n")
        << mWidth << mHeight << mDepth << mDetailRepetitionCount
      );
    return;
  }

  if (useGLSL())
  {
    if(fragmentShader().empty() || vertexShader().empty())
    {
      Log::error("vertex shader or fragment shader not defined.\n");
      /*mFragmentShader = "/glsl/terrain.fs";
      mVertexShader   = "/glsl/terrain.vs";*/
      return;
    }
  }

  bool use_uniform_transform = false; // experimental only

  // Log::print("Loading detail texture... ");
  ref<Image> detail_img = detailTexture().empty() ? ref<Image>(NULL) : loadImage(detailTexture());

  // Log::print("Loading terrain texture... ");
  ref<Image> terrain_img = loadImage(terrainTexture());

  // Log::print("Loading heightmap... ");
  ref<Image> heightmap_img = loadImage(heightmapTexture());

  if ( (!detail_img && !detailTexture().empty()) || !terrain_img || !heightmap_img)
  {
    Log::error("Terrain initialization failed.\n");
    return;
  }

  double dx = width() / heightmap_img->width();
  double dz = depth() / heightmap_img->height();

  int x_subdivision = -1;
  for(int ch=1; ch<1024; ++ch)
  {
    for(int ts=128; ts<=1024*8; ts*=2)
    {
      // printf("[%d][%d]x = %d\n", ch, ts, ch*ts-ch+1);
      if (ch*ts-ch+1 == heightmap_img->width())
      {
        x_subdivision = ch;
        break;
      }
    }
  }

  int y_subdivision = -1;
  for(int ch=1; ch<1024; ++ch)
  {
    for(int ts=128; ts<=1024*8; ts*=2)
    {
      // printf("[%d][%d]y = %d\n", ch, ts, ch*ts-ch+1);
      if (ch*ts-ch+1 == heightmap_img->height())
      {
        y_subdivision = ch;
        break;
      }
    }
  }

  if ( x_subdivision == -1 )
  {
    Log::error("texture width must be of the type: cn*ts-cn+1 where cn=chunk-number, ts=texture-chunk-size\n");
    return;
  }
  if ( y_subdivision == -1 )
  {
    Log::error("texture height must be of the type: cn*ts-cn+1 where cn=chunk-number, ts=texture-chunk-size\n");
    return;
  }

  int xsize  =   (heightmap_img->width()  -1 + x_subdivision)/x_subdivision;
  int zsize  =   (heightmap_img->height() -1 + y_subdivision)/y_subdivision;
  int tx_xsize = (terrain_img->width()    -1 + x_subdivision)/x_subdivision;
  int tx_zsize = (terrain_img->height()   -1 + y_subdivision)/y_subdivision;

  float dtu  = float(1.0 / tx_xsize / 2.0);
  float dtv  = float(1.0 / tx_zsize / 2.0);
  float dtu2 = 1.0f - dtu;
  float dtv2 = 1.0f - dtv;
  float du   = float(1.0 / xsize / 2.0);
  float dv   = float(1.0 / zsize / 2.0);
  float du2  = 1.0f - du;
  float dv2  = 1.0f - dv;

  float detail_du   = detail_img ? float(1.0 / detail_img->width()  / 2.0) : 0;
  float detail_dv   = detail_img ? float(1.0 / detail_img->height() / 2.0) : 0;
  float detail_du2  = mDetailRepetitionCount - detail_du;
  float detail_dv2  = mDetailRepetitionCount - detail_dv;

  ref<Geometry> terr_tile;
  ref<GLSLProgram> glsl;

  ref<ArrayFloat2> tmap_uv = new ArrayFloat2; // texture map
  ref<ArrayFloat2> dmap_uv = detail_img ? new ArrayFloat2 : NULL; // detail texture map
  ref<ArrayFloat2> hmap_uv = new ArrayFloat2; // height map
  tmap_uv->resize( xsize * zsize );
  if(detail_img)
    dmap_uv->resize( xsize * zsize );
  hmap_uv->resize( xsize * zsize );
  for(int z=0; z<zsize; ++z)
  {
    for(int x=0; x<xsize; ++x)
    {
      float u = (float)x/(xsize-1); // 0 .. 1
      float v = (float)z/(zsize-1); // 0 .. 1
      tmap_uv->at(x + z*xsize).s() = (1.0f-u) * dtu + u * dtu2;
      tmap_uv->at(x + z*xsize).t() = (1.0f-v) * dtv + v * dtv2;
      if (detail_img)
      {
        dmap_uv->at(x + z*xsize).s() = (1.0f-u) * detail_du + u * detail_du2;
        dmap_uv->at(x + z*xsize).t() = (1.0f-v) * detail_dv + v * detail_dv2;
      }
      hmap_uv->at(x + z*xsize).s() = (1.0f-u) * du + u * du2;
      hmap_uv->at(x + z*xsize).t() = (1.0f-v) * dv + v * dv2;
    }
  }

  if (useGLSL())
  {
    terr_tile = vl::makeGrid( vec3(0,0,0), 1.0f, 1.0f, xsize, zsize);
    terr_tile->setTexCoordArray(0, tmap_uv.get());
    terr_tile->setTexCoordArray(1, dmap_uv.get());
    terr_tile->setTexCoordArray(2, hmap_uv.get());

    glsl = new GLSLProgram;
    ref<Uniform> Height = new vl::Uniform("Height");
    Height->setUniformF((float)height());
    glsl->setUniform( Height.get() );
    glsl->attachShader( new GLSLFragmentShader( String::loadText(fragmentShader()) ) );
    if (use_uniform_transform)
      glsl->attachShader( new GLSLVertexShader( String::loadText("/glsl/terrain_ut.vs") ) );
    else
      glsl->attachShader( new GLSLVertexShader( String::loadText(vertexShader()) ) );

    // setup GLSL program 'static' uniforms
    ref<Uniform> terrain_tex   = new Uniform("terrain_tex");
    ref<Uniform> detail_tex    = new Uniform("detail_tex");
    ref<Uniform> heightmap_tex = new Uniform("heightmap_tex");
    terrain_tex  ->setUniformI(0);
    detail_tex   ->setUniformI(1);
    heightmap_tex->setUniformI(2);
    glsl->setUniform(terrain_tex.get());
    if (!detailTexture().empty())
      glsl->setUniform(detail_tex.get());
    glsl->setUniform(heightmap_tex.get());

    AABB aabb;
    aabb.setMinCorner((real)-0.5, 0, (real)-0.5);
    aabb.setMaxCorner((real)+0.5, (real)height(), (real)+0.5);
    terr_tile->setBoundingBox( aabb );
    terr_tile->setBoundingSphere(aabb);
    terr_tile->setBoundsDirty(false);

    shaderNode()->setRenderState(IN_Propagate, glsl.get());
  }

  shaderNode()->setEnable(EN_CULL_FACE, true);
  shaderNode()->setEnable(EN_DEPTH_TEST,true);

  if (!useGLSL())
  {
    ref<TexEnv> texenv = new TexEnv;
    texenv->setMode(TEM_MODULATE);
    shaderNode()->setRenderState(IN_Propagate, texenv.get(), 1);
  }

  // generate chunks
  for(int mz=0, tz=0; mz<heightmap_img->height()-1; mz+=zsize-1, tz+=tx_zsize-1)
  {
    for(int mx=0, tx=0; mx<heightmap_img->width()-1; mx+=xsize-1, tx+=tx_xsize-1)
    {
      // effect settings for this tile
      ref<Effect> terr_fx = new Effect;
      ref<ShaderNode> shader_node = new ShaderNode;
      shaderNode()->addChild(shader_node.get());
      shader_node->setShader(terr_fx->shader());

      // terrain texture
      ref<Image> tex_image = terrain_img->subImage(tx, tz, tx_xsize, tx_zsize);
      ref<TextureSampler> tex_unit0 = new TextureSampler;
      shader_node->setRenderState(IN_Propagate, tex_unit0.get(), 0);
      tex_unit0->setTexture(new Texture(tex_image.get(), terrainTextureFormat(), false));
      tex_unit0->texture()->getTexParameter()->setMagFilter(TPF_LINEAR);
      tex_unit0->texture()->getTexParameter()->setMinFilter(TPF_LINEAR);
      tex_unit0->texture()->getTexParameter()->setWrapS(TPW_REPEAT);
      tex_unit0->texture()->getTexParameter()->setWrapT(TPW_REPEAT);

      // detail texture
      if (detail_img)
      {
        ref<TextureSampler> tex_unit1 = new TextureSampler;
        shader_node->setRenderState(IN_Propagate, tex_unit1.get(), 1);
        tex_unit1->setTexture(new Texture(detail_img.get(), detailTextureFormat(), true));
        tex_unit1->texture()->getTexParameter()->setMagFilter(TPF_LINEAR);
        tex_unit1->texture()->getTexParameter()->setMinFilter(TPF_LINEAR_MIPMAP_LINEAR);
        tex_unit1->texture()->getTexParameter()->setWrapS(TPW_REPEAT);
        tex_unit1->texture()->getTexParameter()->setWrapT(TPW_REPEAT);
        if (Has_GL_EXT_texture_filter_anisotropic)
        {
          float max = 1.0f;
          glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &max);
          tex_unit1->texture()->getTexParameter()->setAnisotropy(max);
        }
      }

      // heightmap texture
      ref<Image> hmap_image = heightmap_img->subImage(mx, mz, xsize, zsize);
      if (useGLSL())
      {
        ref<TextureSampler> tex_unit2 = new TextureSampler;
        shader_node->setRenderState(IN_Propagate, tex_unit2.get(), 2);
        tex_unit2->setTexture(new Texture(hmap_image.get(), heightmapTextureFormat(), false));
        tex_unit2->texture()->getTexParameter()->setMagFilter(TPF_NEAREST);
        tex_unit2->texture()->getTexParameter()->setMinFilter(TPF_NEAREST);
        tex_unit2->texture()->getTexParameter()->setWrapS(TPW_REPEAT);
        tex_unit2->texture()->getTexParameter()->setWrapT(TPW_REPEAT);
      }

      // compute tile transform
      dmat4 dmat;
      dmat.scale((xsize-1)*dx, 1.0, (zsize-1)*dz);
      dmat.translate(mx*dx + (xsize-1)*dx*0.5 - width()/2.0, 0, mz*dz + (zsize-1)*dz*0.5 - depth()/2.0);
      dmat.translate((dvec3)mOrigin);
      ref<Transform> transform = new Transform;
      transform->setLocalAndWorldMatrix((mat4)dmat);

      if (!useGLSL())
      {
        terr_tile = vl::makeGrid( vec3(0,0,0), 1.0f, 1.0f, xsize, zsize);
        terr_tile->setTexCoordArray(0, tmap_uv.get());
        terr_tile->setTexCoordArray(1, dmap_uv.get());

        ref<ArrayFloat3> verts = terr_tile->vertexArray()->as<ArrayFloat3>(); VL_CHECK(verts.get());
        for(int z=0; z<zsize; ++z)
        {
          for(int x=0; x<xsize; ++x)
          {
            fvec4 sample = hmap_image->sample(x,z) * (float)height();
            int index = x + xsize * z;
            verts->at(index).y() = sample.r();
          }
        }
      }

      // collect the terrain chunks to be inserted later in the ActorKdTree

      ref<Actor> actor = new Actor(terr_tile.get(), terr_fx.get(), (use_uniform_transform && useGLSL())?NULL:transform.get());
      mChunks.push_back(actor.get());

      if (use_uniform_transform && useGLSL())
      {
        #if 1
          actor->setUniformSet( new UniformSet );
          ref<Uniform> uniform_matrix = new Uniform("matrix");
          uniform_matrix->setUniform( (fmat4)dmat );
          actor->setUniform( uniform_matrix.get() );
        #else
          ref<Uniform> uniform_matrix = new Uniform("matrix");
          uniform_matrix->setUniform( (mat4)dmat );
          terr_fx->shader()->setUniform( uniform_matrix.get() );
        #endif
      }

      #if 0 // for debuggin purposes
        // terr_fx->shader()->gocMaterial()->setColorMaterialEnabled(true);
        terr_tile->setColorArray( fvec4(rand()%256/255.0f,rand()%256/255.0f,rand()%256/255.0f) );
      #endif
    }
  }

  shaderNode()->updateHierarchy();
  tree()->buildKdTree(mChunks);
}
