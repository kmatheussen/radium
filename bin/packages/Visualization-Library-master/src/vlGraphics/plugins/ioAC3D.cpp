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

#include "ioAC3D.hpp"
#include <vlCore/checks.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/Image.hpp>
#include <vlGraphics/Texture.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/TextStream.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <map>
#include <vlCore/LoadWriterManager.hpp>

using namespace vl;

namespace
{
  class LoaderAC3D 
  {
  public:

    class vert_info 
    {
    public:
      int Vert;
      float U;
      float V;
    };
    
    class material_info 
    {
    public:
      String Name;
      fvec3 Diffuse;
      fvec3 Ambient;
      fvec3 Emission;
      fvec3 Specular;
      float Shininess;
      float Trans;
    };

    class surface_info 
    {
    public:
      int Flags;
      int materials;
      int VertCount;
      std::vector<vert_info> Vertex;
    };

    class mesh_info 
    {
    public:
      mesh_info();
    
      String Name;
      String Data;
      String Texture;
      float TexRep[2];
      dmat4 Matrix;
      String Url;
      int NumVert;
      std::vector<fvec3> Vert;
      int NumSurf;
      std::vector<surface_info> Surface;    
      int NumKids;
    };
    
    LoaderAC3D();  
    bool parseAC3D(VirtualFile* file);  

    fvec3 readColor();  
    fvec3 readVector();
    float readFloat();
    int readInt();
    int readHex();
    String readLine();
    String readWord();
    String readName();
    void readMaterial();
    void readObject();
    
    std::vector<material_info> materials;
    std::vector<mesh_info> meshes;
    bool verbose;
    
  protected:
    ref<TextStream> mTokenizer;
    String mTmpStr;
    bool mEOF;
  };
}
//-----------------------------------------------------------------------------
// mesh_info
//-----------------------------------------------------------------------------
LoaderAC3D::mesh_info::mesh_info()
{
  TexRep[0] = 1;
  TexRep[1] = 1;
  NumVert = 0;
  NumSurf = 0;
  NumKids = 0;
}
//-----------------------------------------------------------------------------
// LoaderAC3D
//-----------------------------------------------------------------------------
LoaderAC3D::LoaderAC3D()
{
  verbose = true;
}
//-----------------------------------------------------------------------------
bool LoaderAC3D::parseAC3D(VirtualFile* file)
{
  mEOF = false;

  mTokenizer = new TextStream;
  mTokenizer->setInputFile( file );
  String token;

  mTokenizer->readString(token);

  if (token != "AC3Db")
  {
    Log::error("Not an AC3Db file!\n");
    file->close();
    return false;
  }

  while( mTokenizer->readString(token) )
  {
    if (token == "MATERIAL")
      readMaterial();
    else
    if (token == "OBJECT")
      readObject();
    else
    {
      Log::error("AC3D parse error.\n");
      file->close();
      return false;
    }
  }

  file->close();
  return true;
}
//-----------------------------------------------------------------------------
fvec3 LoaderAC3D::readColor()
{
  bool ok = true;
  ok &= mTokenizer->readString(mTmpStr); float r = mTmpStr.toFloat();
  ok &= mTokenizer->readString(mTmpStr); float g = mTmpStr.toFloat();
  ok &= mTokenizer->readString(mTmpStr); float b = mTmpStr.toFloat();
  mEOF = !ok;
  if ( !ok )
    Log::error("LoaderAC3D::readColor() IO error.\n");
  return fvec3(r,g,b);
}
//-----------------------------------------------------------------------------
fvec3 LoaderAC3D::readVector()
{
  bool ok = true;
  ok &= mTokenizer->readString(mTmpStr); float x = mTmpStr.toFloat();
  ok &= mTokenizer->readString(mTmpStr); float y = mTmpStr.toFloat();
  ok &= mTokenizer->readString(mTmpStr); float z = mTmpStr.toFloat();
  mEOF = !ok;
  if ( !ok )
    Log::error("LoaderAC3D::readVector() IO error.\n");
  return fvec3(x,y,z);
}
//-----------------------------------------------------------------------------
float LoaderAC3D::readFloat()
{
  bool ok = true;
  ok &= mTokenizer->readString(mTmpStr); float f = mTmpStr.toFloat();
  mEOF = !ok;
  if ( !ok )
    Log::error("LoaderAC3D::readFloat() IO error.\n");
  return f;
}
//-----------------------------------------------------------------------------
int LoaderAC3D::readInt()
{
  bool ok = true;
  ok &= mTokenizer->readString(mTmpStr); int i = mTmpStr.toInt();
  mEOF = !ok;
  if ( !ok )
    Log::error("LoaderAC3D::readInt() IO error.\n");
  return i;
}
//-----------------------------------------------------------------------------
int LoaderAC3D::readHex()
{
  bool ok = true;
  ok &= mTokenizer->readString(mTmpStr);
  mEOF = !ok;
  if ( !ok )
  {
    Log::error("I/O error reading hex.\n");
    return 0;
  }
  else
  {
    mTmpStr[0] = ' ';
    mTmpStr[1] = ' ';
    return mTmpStr.toInt(true);
  }
}
//-----------------------------------------------------------------------------
String LoaderAC3D::readWord()
{
  bool ok = true;
  ok &= mTokenizer->readString( mTmpStr );
  mEOF = !ok;
  if ( !ok )
  {
    Log::error("I/O error reading word.\n");
    return "";
  }
  else
  {
    if ( mTmpStr.count('"') == 1 )
    {
      Log::error( Say("The string '%s' contains a single '\"'.\n") << mTmpStr );
    }

    mTmpStr.remove('"');
    return mTmpStr;
  }
}
//-----------------------------------------------------------------------------
String LoaderAC3D::readName()
{
  bool ok = true;
  ok &= mTokenizer->readQuotedString( mTmpStr );
  mEOF = !ok;
  if ( !ok )
  {
    Log::error("I/O error reading word.\n");
    return "";
  }
  else
  {
    if ( mTmpStr.count('"') == 1 )
    {
      Log::error( Say("The string '%s' contains a single '\"'.\n") << mTmpStr );
    }

    mTmpStr.remove('"');
    return mTmpStr;
  }
}
//-----------------------------------------------------------------------------
String LoaderAC3D::readLine()
{
  bool ok = true;
  ok &= mTokenizer->readLine( mTmpStr );
  if ( !ok )
  {
    Log::error("I/O error reading line.\n");
    return "";
  }
  else
  {
    return mTmpStr;
  }
}
//-----------------------------------------------------------------------------
void LoaderAC3D::readMaterial()
{
  // MATERIAL %s rgb %f %f %f  amb %f %f %f  emis %f %f %f  spec %f %f %f  shi %d  trans %f
  material_info material;

  material.Name = readName();
  readWord(); // rgb
  material.Diffuse = readColor();
  readWord(); // amb
  material.Ambient = readColor();
  readWord(); // emis
  material.Emission = readColor();
  readWord(); // spec
  material.Specular = readColor();
  readWord(); // shi
  material.Shininess = readFloat();
  readWord(); // trans
  material.Trans = 1 - readFloat();

  materials.push_back(material);
}
//-----------------------------------------------------------------------------
void LoaderAC3D::readObject()
{
  mesh_info mesh;
  String block;

  block = readWord();

  do
  {
    block = readWord();

    if (mEOF)
      break;

    if (block == "name")
    {
      mesh.Name = readName();
    } else
    if (block == "data")
    {
      // needed to safely skip to the next line
      int data_size=0;
      mTokenizer->readLine(mTmpStr);
      data_size = mTmpStr.toInt();

      // skip data_size bytes
      unsigned char ch = 0;
      for (int i=0; i<data_size; i++ )
        mTokenizer->readToken(&ch);
    }
    else
    if (block == "texture")
    {
      mesh.Texture = readName();
    } else
    if (block == "texrep")
    {
      mesh.TexRep[0] = readFloat();
      mesh.TexRep[1] = readFloat();
    } else
    if (block == "rot")
    {
      mesh.Matrix.setX( (dvec3)readVector() );
      mesh.Matrix.setY( (dvec3)readVector() );
      mesh.Matrix.setZ( (dvec3)readVector() );
    } else
    if (block == "loc")
    {
      mesh.Matrix.setT( (dvec3)readVector() );
    } else
    if (block == "url")
    {
      mesh.Url = readName();
    } else
    if (block == "numvert")
    {
      mesh.NumVert = readInt();
      for(int i=0; i<mesh.NumVert; ++i)
      {
        mesh.Vert.push_back( readVector() );
      }
    } else
    if (block == "numsurf")
    {
      mesh.NumSurf = readInt();
      for(int i=0; i<mesh.NumSurf; ++i)
      {
        surface_info surf;
        String tmp;
        do
        {
          tmp = readWord();

          if (tmp == "SURF")
          {
            surf.Flags = readHex();
          }
          if (tmp == "mat")
          {
            surf.materials = readInt();
          }
          if (tmp == "refs")
          {
            surf.VertCount = readInt();
            for(int j=0; j<surf.VertCount; ++j)
            {
              vert_info vert;
              vert.Vert = readInt();
              vert.U = readFloat();
              vert.V = readFloat();
              surf.Vertex.push_back( vert );
            }
          }
        }
        while (tmp != "refs");
        mesh.Surface.push_back(surf);
      }
    }
    else
    if (block == "kids")
    {
      mesh.NumKids = readInt();
      meshes.push_back(mesh);
    }
    else
    {
      if (verbose)
        Log::warning( Say("unknown block '%s'\n") << block );
      // skip line
      mTokenizer->readLine(mTmpStr);
    }
  }
  while(block != "kids");
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadAC3D(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
  else
  {
    return loadAC3D(file.get() );
  }  
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::loadAC3D( VirtualFile* file)
{
  ref<ResourceDatabase> res_db = new ResourceDatabase;

  LoaderAC3D loader;
  if ( !loader.parseAC3D(file) )
    return NULL;

  // compile the material map
  std::vector< ref<Effect> > mat_map;
  for(unsigned imat=0; imat<loader.materials.size(); imat++)
  {
    ref<Effect> effect = new Effect;
    mat_map.push_back(effect.get());

    // apply material
    effect->shader()->enable(EN_DEPTH_TEST);

    effect->shader()->gocMaterial()->setAmbient( fvec4(loader.materials[ imat ].Ambient, 1.0f) );
    effect->shader()->gocMaterial()->setDiffuse( fvec4(loader.materials[ imat ].Diffuse, 1.0f) );
    effect->shader()->gocMaterial()->setEmission( fvec4(loader.materials[ imat ].Emission, 1.0f) );
    effect->shader()->gocMaterial()->setSpecular( fvec4(loader.materials[ imat ].Specular, 1.0f) );
    effect->shader()->gocMaterial()->setShininess( loader.materials[ imat ].Shininess );
    effect->shader()->gocMaterial()->setTransparency( loader.materials[ imat ].Trans );

    if ( loader.materials[ imat ].Trans < 1.0f )
    {
      effect->shader()->enable(EN_CULL_FACE);
      effect->shader()->enable(EN_BLEND);
      effect->shader()->gocBlendFunc()->set(BF_SRC_ALPHA, BF_ONE_MINUS_SRC_ALPHA);
    }
  }

  // dumps the objects
  for(unsigned imesh=0; imesh<loader.meshes.size(); imesh++)
  {
    if ( loader.meshes[imesh].Surface.empty() )
      continue;

    ref<Actor> act = new Actor;
    act->setObjectName( loader.meshes[imesh].Name.toStdString().c_str() );

    ref<Geometry> geom = new Geometry;
    geom->setObjectName( loader.meshes[imesh].Name.toStdString().c_str() );

    ref<ArrayFloat3> verts = new ArrayFloat3;
    ref<ArrayFloat2> uv    = new ArrayFloat2;
    ref<DrawElementsUInt> polys = new DrawElementsUInt;
    geom->drawCalls()->push_back( polys.get() );
    geom->setVertexArray( verts.get() );
    geom->setTexCoordArray(0, uv.get());
    act->setLod(0, geom.get());

    // we handle only one material per surface
    int mat_index = loader.meshes[imesh].Surface[0].materials;

    act->setEffect( mat_map[ mat_index ].get() );

    // polygons + vertices
    int vert_count = 0;
    for(unsigned isurf=0; isurf<loader.meshes[imesh].Surface.size(); isurf++)
    {
      if ( (loader.meshes[imesh].Surface[isurf].Flags & 0xF) != 0 )
        continue;
      for( int ivert=1; ivert<loader.meshes[imesh].Surface[isurf].VertCount-1; ivert++)
        for(int i=0; i<3; ++i)
          ++vert_count;
    }

    verts->resize( vert_count );
    uv->resize( vert_count );

    if (!vert_count)
      continue;

    // add when we are sure we have some content

    res_db->resources().push_back( act.get() );
    res_db->resources().push_back( geom.get() );

    // warning: this is a quite experimental importer.

    // !!! FIX !!! i vertici dovrebbero essere < degli indici in generale, vedi sotto
    polys->indexBuffer()->resize( vert_count );

    int idx = 0;
    for(unsigned isurf=0; isurf<loader.meshes[imesh].Surface.size(); isurf++)
    {
      if ( (loader.meshes[imesh].Surface[isurf].Flags & 0xF) != 0 ) // not a poly
        continue;
      VL_CHECK( loader.meshes[imesh].Surface[isurf].VertCount >= 3 )
      for( int ivert=1; ivert<loader.meshes[imesh].Surface[isurf].VertCount-1; ivert++)
      {
        int vert_idx[] = { 0, ivert, ivert+1 };
        for(int i=0; i<3; ++i, idx++)
        {
          // FIXME: a quanto pare i vertici non sono condivisi, ma lo dovrebbero essere
          polys->indexBuffer()->at( idx ) = idx;
          int iv = loader.meshes[imesh].Surface[isurf].Vertex[ vert_idx[i] ].Vert;
          verts->at( idx ) = loader.meshes[imesh].Vert[ iv ];
          uv->at( idx )    = fvec2(loader.meshes[imesh].Surface[isurf].Vertex[ vert_idx[i] ].U, loader.meshes[imesh].Surface[isurf].Vertex[ vert_idx[i] ].V);
        }
      }
    }

    // FIXME
    /*DoubleVertexRemover dvr;
    dvr.removeDoubles(geom.get());*/
    geom->transform( (mat4)loader.meshes[imesh].Matrix );
    // geom->computeNormals();

    // !!! FIX !!! texture e double-side sono specificati per mesh e non per materiale, VL dovrebbe creare
    // combinazioni uniche di materiali/texture/double-side

    // !!! FIX !!! texture - ci dovrebbe essere un unico materiale per ogni combinazione di double-side/texture
    if ( loader.meshes[imesh].Texture.length() )
    {
      ref<Texture> texture = new Texture;
      // locate texture
      String tex_path = loader.meshes[imesh].Texture;
      ref<VirtualFile> tex_file = defFileSystem()->locateFile(tex_path,file->path().extractPath());
      if (tex_file)
        tex_path = tex_file->path();
      texture->prepareTexture2D(tex_path, TF_RGBA);
      act->effect()->shader()->gocTextureSampler(0)->setTexture( texture.get() );
    }

    // !!! FIX !!! double-side - ci dovrebbe essere un unico materiale per ogni combinazione di double-side/texture
    if ( (loader.meshes[imesh].Surface[0].Flags & 0x20) )
    {
      act->effect()->shader()->gocLightModel()->setTwoSide(true);
      /* effect->shader()->disable(EN_CULL_FACE); */
    }

  }

  return res_db;
}
//-----------------------------------------------------------------------------
