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

#include "ioSTL.hpp"
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/TextStream.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <stdio.h>

using namespace vl;
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadSTL(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);

  if (file)
    return loadSTL( file.get() );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadSTL(VirtualFile* file)
{
  STLLoader stl;
  ref<ResourceDatabase> res_db = stl.loadSTL(file);
  return res_db;
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> STLLoader::loadBinary(VirtualFile* file)
{
  // skip header
  char header[80];
  file->read(header,80);
  unsigned int tri_count = file->readUInt32();

  ref<ArrayFloat3>  verts   = new ArrayFloat3;
  ref<ArrayFloat3>  normals = new ArrayFloat3;
  verts->resize(tri_count*3);
  normals->resize(tri_count*3);
  ref<DrawArrays> de = new DrawArrays(PT_TRIANGLES,0,tri_count*3);
  ref<Geometry> geom = new Geometry;
  geom->drawCalls()->push_back(de.get());
  geom->setVertexArray(verts.get());
  geom->setNormalArray(normals.get());

  // read triangles
  for(unsigned int i=0; i<tri_count; ++i)
  {
    fvec3 n,v1,v2,v0;
    n.x() = file->readFloat();
    n.y() = file->readFloat();
    n.z() = file->readFloat();
    v0.x() = file->readFloat();
    v0.y() = file->readFloat();
    v0.z() = file->readFloat();
    v1.x() = file->readFloat();
    v1.y() = file->readFloat();
    v1.z() = file->readFloat();
    v2.x() = file->readFloat();
    v2.y() = file->readFloat();
    v2.z() = file->readFloat();
    // skip the 2 bytes
    file->readUInt16();
    normals->at(i*3+0) = n;
    verts->at(i*3+0) = v0;
    normals->at(i*3+1) = n;
    verts->at(i*3+1) = v1;
    normals->at(i*3+2) = n;
    verts->at(i*3+2) = v2;
  }

  ref<ResourceDatabase> res_db = new ResourceDatabase;
  ref<Effect> effect = new Effect;
  res_db->resources().push_back( geom );
  res_db->resources().push_back( new Actor(geom.get(), effect.get(), NULL ) );
  res_db->resources().push_back( effect.get() );
  return res_db;
}
ref<ResourceDatabase> STLLoader::loadAscii(VirtualFile* file)
{
  TextStream line_reader;
  line_reader.setInputFile(file);
  std::string str;

  // skip header
  line_reader.readLine(str);

  std::vector<fvec3> verts;
  std::vector<fvec3> norms;

  fvec3 v[3], n;
  char parola1[32];
  char parola2[32];
  while( line_reader.readLine(str) )
  {
    sscanf(str.c_str(), "%s %s %f %f %f", parola1, parola2, &n.x(), &n.y(), &n.z());
    // skip outer loop
    line_reader.readLine(str);
    line_reader.readLine(str); sscanf(str.c_str(), "%s %f %f %f", parola1, &v[0].x(), &v[0].y(), &v[0].z());
    line_reader.readLine(str); sscanf(str.c_str(), "%s %f %f %f", parola1, &v[1].x(), &v[1].y(), &v[1].z());
    line_reader.readLine(str); sscanf(str.c_str(), "%s %f %f %f", parola1, &v[2].x(), &v[2].y(), &v[2].z());
    // skip endloop
    line_reader.readLine(str);
    // skip endfacet
    line_reader.readLine(str);
    // emit triangle
    norms.push_back(n);
    norms.push_back(n);
    norms.push_back(n);
    verts.push_back(v[0]);
    verts.push_back(v[1]);
    verts.push_back(v[2]);
  }

  ref<ArrayFloat3>  vertices   = new ArrayFloat3;
  ref<ArrayFloat3>  normals = new ArrayFloat3;
  vertices->resize(verts.size());
  normals->resize(verts.size());
  memcpy(normals ->ptr(), &norms[0], sizeof(norms[0])*norms.size());
  memcpy(vertices->ptr(), &verts[0], sizeof(verts[0])*verts.size());
  ref<DrawArrays> de = new DrawArrays(PT_TRIANGLES,0,verts.size());
  ref<Geometry> geom = new Geometry;
  geom->drawCalls()->push_back(de.get());
  geom->setVertexArray(vertices.get());
  geom->setNormalArray(normals.get());

  ref<ResourceDatabase> res_db = new ResourceDatabase;
  ref<Effect> effect = new Effect;
  res_db->resources().push_back( geom );
  res_db->resources().push_back( new Actor(geom.get(), effect.get(), NULL ) );
  res_db->resources().push_back( effect.get() );
  return res_db;
}
ref<ResourceDatabase> STLLoader::loadSTL(VirtualFile* file)
{
  file->open(OM_ReadOnly);

  char header[] = {1,2,3,4,5,0};

  file->read(header,5);
  ref<ResourceDatabase> res_db;
  if (strcmp((char*)header,"solid") == 0)
  {
    // ascii
    file->seekSet(0);
    res_db = loadAscii(file);
  }
  else
  {
    // binary
    file->seekSet(0);
    res_db = loadBinary(file);
  }

  file->close();
  return res_db;
}
//-----------------------------------------------------------------------------
