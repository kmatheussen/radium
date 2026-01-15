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

#include "ioMD2.hpp"
#include <vlCore/Time.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>

using namespace vl;

namespace 
{
  // MD2 structures and defines
  class LoaderMD2
  {
  public:
    LoaderMD2(): verbose(false) {} 

    enum {
      MD2_MAX_TRIANGLES = 4096,
      MD2_MAX_VERTICES  = 2048,
      MD2_MAX_TEXCOORDS = 2048,
      MD2_MAX_FRAMES    = 512,
      MD2_MAX_SKINS     = 32,
      MD2_MAX_FRAMESIZE = MD2_MAX_VERTICES * 4 + 128
    };

    class md2_header_info
    {
    public:
       int magic;
       int version;
       int skinWidth;
       int skinHeight;
       int frameSize;
       int numSkins;
       int numVertices;
       int numTexCoords;
       int numTriangles;
       int numGlCommands;
       int numFrames;
       int offsetSkins;
       int offsetTexCoords;
       int offsetTriangles;
       int offsetFrames;
       int offsetGlCommands;
       int offsetEnd;
    };

    class md2_vertex_info
    {
    public:
      unsigned char vertex[3];
      unsigned char light_norm_index;
    };

    class md2_triangle_info
    {
    public:
       short vert_idx[3];
       short uv_idx[3];
    };

    class md2_uv_info
    {
    public:
       short u, v;
    };

    class md2_frame_info
    {
    public:
       float scale[3];
       float translate[3];
       char name[16];
       md2_vertex_info vertices[1];
    };

    class md2_skin_info
    {
    public:
      unsigned char name[64];
    };

    md2_header_info header;
    std::vector<md2_frame_info*> md2_frame;
    std::vector<md2_uv_info> md2_uv;
    std::vector<md2_triangle_info> md2_triangle;
    std::vector<md2_skin_info> md2_skin;
    bool verbose;
  };
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadMD2(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);

  if (file)
    return loadMD2( file.get() );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadMD2(VirtualFile* file)
{
  ref<ResourceDatabase> res_db = new ResourceDatabase;
  ref<Geometry> geometry = new Geometry;
  res_db->resources().push_back( geometry.get() );

  if (!file->open(OM_ReadOnly))
  {
    Log::error( Say("Error opening '%s'\n") << file->path() );
    return NULL;
  }
    
  LoaderMD2 loader;
  file->read( &loader.header, sizeof(loader.header) );
  
  if (loader.verbose)
  {
    Log::print( Say("tris  %n:\n") <<  loader.header.numTriangles);
    Log::print( Say("verts %n:\n") <<  loader.header.numVertices);
    Log::print( Say("uvs   %n:\n") <<  loader.header.numTexCoords);
    Log::print( Say("offs skins %n:\n") <<  loader.header.offsetSkins);
    Log::print( Say("offs end %n:\n") <<  loader.header.offsetEnd);
    Log::print( Say("offs frames %n:\n") <<  loader.header.offsetFrames);
    Log::print( Say("offs gl comm %n:\n") <<  loader.header.offsetGlCommands);
    Log::print( Say("offs tex coor %n:\n") <<  loader.header.offsetTexCoords);
    Log::print( Say("offs tri %n:\n") <<  loader.header.offsetTriangles);
    Log::print( Say("skinh %n:\n") <<  loader.header.skinHeight);
    Log::print( Say("skinw %n:\n") <<  loader.header.skinWidth);
  }

  // load data into memory
  file->seekSet(loader.header.offsetFrames);
  loader.md2_frame.resize(loader.header.numFrames);
  for(unsigned i=0; i<loader.md2_frame.size(); ++i) 
  {
    loader.md2_frame[i] = (LoaderMD2::md2_frame_info*)malloc(loader.header.frameSize * sizeof(char) );
    file->read(loader.md2_frame[i], loader.header.frameSize);
  }
    
  // uv
  loader.md2_uv.resize(loader.header.numTexCoords);
  file->seekSet( loader.header.offsetTexCoords );
  file->read(&loader.md2_uv[0], loader.header.numTexCoords*sizeof(LoaderMD2::md2_uv_info) );  
  
  // triangles
  loader.md2_triangle.resize(loader.header.numTriangles);
  file->seekSet(loader.header.offsetTriangles );
  file->read(&loader.md2_triangle[0], loader.header.numTriangles*sizeof(LoaderMD2::md2_triangle_info) );  
  
  // textures 
  if (loader.header.numSkins) 
  {
    loader.md2_skin.resize(loader.header.numSkins);
    file->seekSet( loader.header.offsetSkins );
    file->read(&loader.md2_skin[0], loader.header.numSkins*sizeof(LoaderMD2::md2_skin_info) );
  }
  
  // fclose(fin);
  file->close();
  
  // conversion

  std::vector< ref<ArrayFloat3> > vertex_frames;
  std::vector< ref<ArrayFloat3> > normal_frames;

  // allocate frames
  for(int i=0; i<loader.header.numFrames; ++i)
  {
    vertex_frames.push_back( new ArrayFloat3 );
    vertex_frames[i]->resize( 3 * loader.header.numTriangles );
    // normals are computed later
  }

  ref<DrawElementsUInt> polygons = new DrawElementsUInt;
  ref<ArrayFloat2> tex_coords = new ArrayFloat2;
  tex_coords->resize( 3 * loader.header.numTriangles );
  polygons->indexBuffer()->resize( 3 * loader.header.numTriangles );
  geometry->setTexCoordArray(0, tex_coords.get());
  geometry->drawCalls()->push_back( polygons.get() );

  int vert_idx = 0;
  VL_CHECK( (int)loader.md2_triangle.size() == loader.header.numTriangles )
  for(int itri=0; itri<loader.header.numTriangles; itri++) 
  {
    for( int ivert=3; ivert--; ++vert_idx )
    {
      // add uv
      float u = (float)loader.md2_uv[ loader.md2_triangle[itri].uv_idx[ivert] ].u / loader.header.skinWidth;
      float v = 1.0f - (float)loader.md2_uv[ loader.md2_triangle[itri].uv_idx[ivert] ].v / loader.header.skinHeight;
      tex_coords->at(vert_idx) = fvec2(u, v);

      // add vert
      for(int iframe=0; iframe<loader.header.numFrames; iframe++) 
      {
        fvec3 vec;
        vec.x() =      loader.md2_frame[iframe]->vertices[ loader.md2_triangle[itri].vert_idx[ivert] ].vertex[0] * loader.md2_frame[iframe]->scale[0] + loader.md2_frame[iframe]->translate[0];
        vec.y() =      loader.md2_frame[iframe]->vertices[ loader.md2_triangle[itri].vert_idx[ivert] ].vertex[2] * loader.md2_frame[iframe]->scale[2] + loader.md2_frame[iframe]->translate[2];
        vec.z() = -1 *(loader.md2_frame[iframe]->vertices[ loader.md2_triangle[itri].vert_idx[ivert] ].vertex[1] * loader.md2_frame[iframe]->scale[1] + loader.md2_frame[iframe]->translate[1]);
        vertex_frames[iframe]->at( vert_idx ) = vec;
      }
      
      // add index
      polygons->indexBuffer()->at( vert_idx ) = vert_idx;
    }
  }

  for(int iframe=0; iframe<loader.header.numFrames; iframe++) 
    free(loader.md2_frame[iframe]);

  // remove double vertices using the first vertex frame
  geometry->setVertexArray( vertex_frames[0].get() );
  geometry->setNormalArray( NULL ); // don't count the normals
  geometry->setTexCoordArray( 0, tex_coords.get() ); // count the texture coords

  // this takes away the 66% of the vertices!
  DoubleVertexRemover remover;
  remover.removeDoubles( geometry.get() );

  // install the newly created and simplified arrays
  vertex_frames[0] = cast<ArrayFloat3>(geometry->vertexArray()); VL_CHECK(vertex_frames[0]);
  tex_coords       = cast<ArrayFloat2>(geometry->texCoordArray(0)); VL_CHECK(tex_coords);
  polygons         = cast<DrawElementsUInt>(geometry->drawCalls()->at(0)); VL_CHECK(polygons);

  // simplify the remaining frames based on the translation table remover.oldToNewIndexMap()
  for(int iframe=1; iframe<loader.header.numFrames; ++iframe)
  {
    ArrayFloat3* new_vertex_frame = new ArrayFloat3;
    new_vertex_frame->resize( vertex_frames[0]->size() );

    for(size_t ivert=0; ivert<vertex_frames[iframe]->size(); ++ivert)
    {
      VL_CHECK( remover.mapOldToNew()[ivert] < new_vertex_frame->size() )
      new_vertex_frame->at( remover.mapOldToNew()[ivert] ) = vertex_frames[iframe]->at(ivert);
    }
    vertex_frames[iframe] = new_vertex_frame;
  }

  // compute normals
  normal_frames.resize( loader.header.numFrames );
  for(int iframe=0; iframe<loader.header.numFrames; iframe++) 
  {
    geometry->setVertexArray( vertex_frames[iframe].get() );
    geometry->computeNormals();
    normal_frames[iframe] = cast<ArrayFloat3>(geometry->normalArray()); VL_CHECK(normal_frames[iframe]);
    VL_CHECK( normal_frames[iframe] )
  }

  for(unsigned i=0; i<vertex_frames.size(); ++i)
  {
    vertex_frames[i]->setObjectName("vertex_frame");
    res_db->resources().push_back(vertex_frames[i].get());
  }

  for(unsigned i=0; i<normal_frames.size(); ++i)
  {
    normal_frames[i]->setObjectName("normal_frame");
    res_db->resources().push_back(normal_frames[i].get());
  }

  return res_db;
}
//-----------------------------------------------------------------------------

/* Quick MD2 Animation Map
 * stand 0,39
 * run 40,45
 * attack 46,53
 * wave 112, 122
 * die 190 197
 */

