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

#include "io3DS.hpp"
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Texture.hpp>
#include <vlCore/Image.hpp>
#include <vlGraphics/Camera.hpp>
#include <vlCore/Time.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/VisualizationLibrary.hpp>
#include <vlCore/FileSystem.hpp>
#include <set>

using namespace vl;

#define ID_3D_EDITOR_CHUNK 0x3D3D
#define ID_MAIN_CHUNK 0x4D4D
#define ID_3DS_VERSION 0x0002
#define ID_OBJECT_BLOCK 0x4000
#define ID_MATERIAL_BLOCK 0xAFFF
#define ID_COLOR_F 0x0010
#define ID_COLOR_24 0x0011
#define ID_LIN_COLOR_24 0x0012
#define ID_LIN_COLOR_F 0x0013
#define ID_INT_PERCENTAGE 0x0030
#define ID_FLOAT_PERCENTAGE 0x0031
#define ID_MATERIAL_NAME 0xA000
#define ID_MAT_AMBIENT 0xA010
#define ID_MAT_DIFFUSE 0xA020
#define ID_MAT_SPECULAR 0xA030
#define ID_MAT_SHININESS_PERCENT 0xA040
#define ID_MAT_SHININESS_STRENGTH_PERCENT 0xA041
#define ID_MAT_TRANSPARENCY 0xA050
#define ID_MAT_TWO_SIDE 0xA081
#define ID_MAT_TEXMAP 0xA200
#define ID_MAT_TEXMAP2 0xA33A
#define ID_MAT_MAPNAME 0xA300
#define ID_MAT_MAP_TILING 0xA351
#define ID_MAT_USCALE 0xA354
#define ID_MAT_VSCALE 0xA356
#define ID_MAT_UOFFSET 0xA358
#define ID_MAT_VOFFSET 0xA35A
#define ID_MAT_MAP_ROTATION 0xA35C
#define ID_TRIANGULAR_MESH 0x4100
#define ID_LOCAL_COORDS_SYSTEM 0x4160
#define ID_MAPPING_COORDS 0x4140
#define ID_SMOOTHING_GROUP_LIST 0x4150
#define ID_FACE_MATERIAL_LIST 0x4130
#define ID_FACE_LIST 0x4120
#define ID_VERTEX_LIST 0x4110
#define ID_HIERARCHY 0x4F00
#define ID_PARENT_OBJECT 0x4F10
#define ID_PIVOT_OBJECT 0x4F20
#define ID_PIVOT_LIMITS 0x4F30
#define ID_PIVOT_ORDER 0x4F40
#define ID_XLATE_RANGE 0x4F50

#define ID_KEYFRAMER_CHUNK 0xB000
  #define ID_KEYF_OBJDES 0xB002 // Mesh Information Block
    #define ID_KEYF_OBJHIERARCH 0xB010 // Object Name And Hierarchy
    #define ID_KEYF_OBJPIVOT 0xB013 // Object Pivot Point
    #define ID_KEYF_POSITION_TRACK 0xB020 // Position Track + Pivot
    #define ID_KEYF_ROTATION_TRACK 0xB021 // Rotation Track
    #define ID_KEYF_SCALE_TRACK 0xB022 // Scale Track
    #define ID_KEYF_NODE_ID 0xB030 // Node ID
  #define ID_KEY_SPOTLIGHT_NODE_TAG 0xB007 // Spot Light Information Block
  #define ID_KEY_FRAMES_START_END 0xB008 // Frames (Start and End)

//-----------------------------------------------------------------------------
// A3DSLoader
//-----------------------------------------------------------------------------
A3DSLoader::A3DSLoader()
{
}
//-----------------------------------------------------------------------------
fvec3 A3DSLoader::readVec3()
{
  fvec3 v;
  v.x() = mInputFile->readFloat();
  v.y() = mInputFile->readFloat();
  v.z() = mInputFile->readFloat();
  return v;
}
//-----------------------------------------------------------------------------
fvec3 A3DSLoader::readColByte3()
{
  fvec3 c;
  c.r() = mInputFile->readUInt8() / 255.0f;
  c.g() = mInputFile->readUInt8() / 255.0f;
  c.b() = mInputFile->readUInt8() / 255.0f;
  return c;
}
//-----------------------------------------------------------------------------
fvec3 A3DSLoader::readColFloat3()
{
  fvec3 c;
  c.r() = mInputFile->readFloat();
  c.g() = mInputFile->readFloat();
  c.b() = mInputFile->readFloat();
  return c;
}
//-----------------------------------------------------------------------------
String A3DSLoader::readLine()
{
  std::vector<unsigned char> str;
  unsigned char b;
  do {
    b= mInputFile->readUInt8();
    str.push_back(b);
  } while(b);

  return (char*)&str[0];
}
//-----------------------------------------------------------------------------
float A3DSLoader::readWordPercent()
{
  return mInputFile->readUInt16() / 100.0f;
}
//-----------------------------------------------------------------------------
float A3DSLoader::readFloatPercent()
{
  return mInputFile->readFloat() / 100.0f;
}
//-----------------------------------------------------------------------------
void A3DSLoader::readChunk()
{
  mChunkId  = mInputFile->readUInt16();
  mChunkLen = mInputFile->readUInt32();
}
//-----------------------------------------------------------------------------
bool A3DSLoader::skipChunk()
{
  mCorrupted |= !mInputFile->seekCur( mChunkLen - 6 );
  if (mCorrupted)
  {
    Log::error("3ds file is corrupted.\n");
    mObjects.clear();
    mMaterials.clear();
  }

  return !mCorrupted;
}
//-----------------------------------------------------------------------------
void A3DSLoader::read_3D_EDITOR_CHUNK()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  while(mInputFile->position() < chunk_end && !mCorrupted)
  {
    readChunk();

    switch(mChunkId)
    {
    case ID_OBJECT_BLOCK:
      read_OBJECT_BLOCK();
    break;

    case ID_MATERIAL_BLOCK:
      read_MATERIAL_BLOCK();
    break;

    default:
      skipChunk();
    }
  }
}
//-----------------------------------------------------------------------------
fvec3 A3DSLoader::readColChunk()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  fvec3 color;
  while( mInputFile->position() < chunk_end && !mCorrupted )
  {
    readChunk();
    switch(mChunkId)
    {
    case ID_COLOR_F:
      color = readColFloat3();
    break;
    case ID_COLOR_24:
      color = readColByte3();
    break;
    // skip gamma byte
    case ID_LIN_COLOR_24:
      readColByte3();
    break;
    // skip gamma float
    case ID_LIN_COLOR_F:
      readColFloat3();
    break;
    }
  }
  return color;
}
//-----------------------------------------------------------------------------
float A3DSLoader::readPercentChunk()
{
  readChunk();
  float perc=0;
  switch(mChunkId)
  {
  case ID_INT_PERCENTAGE:
    perc = readWordPercent();
  break;
  case ID_FLOAT_PERCENTAGE:
    perc = readFloatPercent();
  break;
  }
  return perc;
}
//-----------------------------------------------------------------------------
// materials
void A3DSLoader::read_MATERIAL_BLOCK()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  // unsigned int id = mChunkId;

  A3DSMaterial mat;

  while(mInputFile->position() < chunk_end && !mCorrupted)
  {
    readChunk();

    switch(mChunkId)
    {
    // Material name
    case ID_MATERIAL_NAME:
    {
      mat.mMaterialName = readLine();
    }
    break;
    // Ambient color
    case ID_MAT_AMBIENT:
    {
      mat.mAmbient = readColChunk();
    }
    break;
    // Diffuse color
    case ID_MAT_DIFFUSE:
    {
      mat.mDiffuse = readColChunk();
    }
    break;
    // Specular color
    case ID_MAT_SPECULAR:
    {
      mat.mSpecular = readColChunk();
    }
    break;
    // Shininess percent
    case ID_MAT_SHININESS_PERCENT:
    {
      mat.mShininess = readPercentChunk();
    }
    break;
    // Shininess strength percent
    case ID_MAT_SHININESS_STRENGTH_PERCENT:
    {
      mat.mShininessStrength = readPercentChunk();
    }
    break;
    // Transparency percent
    case ID_MAT_TRANSPARENCY:
    {
      mat.mTransparency = readPercentChunk();
    }
    break;
    // Double sided material
    case ID_MAT_TWO_SIDE:
    {
      mat.mDoubleSided = true;
    }
    // Texture map 1
    case ID_MAT_TEXMAP:
    {
      mat.mTexture1 = readMapChunk();
    }
    break;
    // Texture map 2
    case ID_MAT_TEXMAP2:
    {
      mat.mTexture2 = readMapChunk();
    }
    break;

    default:
      skipChunk();
    }
  }
  mMaterials.push_back(mat);
}
//-----------------------------------------------------------------------------
// A3DSObject Block
A3DSTexture A3DSLoader::readMapChunk()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  // unsigned int id = mChunkId;

  A3DSTexture tex;

  while(mInputFile->position() < chunk_end && !mCorrupted)
  {
    readChunk();

    switch(mChunkId)
    {
    // Map filename
    case ID_MAT_MAPNAME:
    {
      tex.mFileName = readLine();
      // locate the actual file
      ref<VirtualFile> file = vl::defFileSystem()->locateFile(tex.mFileName, mInputFile->path().extractPath());
      if (file)
        tex.mFileName = file->path();

    }
    break;
    // Map options
    case ID_MAT_MAP_TILING:
    {
      unsigned short flags = mInputFile->readUInt16();
      int bit[10] = { flags&(1<<0), flags&(1<<1), flags&(1<<2), flags&(1<<3),
                       flags&(1<<4), flags&(1<<5), flags&(1<<6), flags&(1<<7),
                       flags&(1<<8), flags&(1<<9) };
      tex.mOpt_tile   = (!bit[4] && !bit[0]) || (!bit[4] && bit[1]);
      tex.mOpt_decal  = (bit[4]  &&  bit[0]) || (!bit[4] && bit[1]);
      tex.mOpt_mirror           = bit[1]?true:false; // vc issues warnings otherwise
      tex.mOpt_negative         = bit[3]?true:false;
      tex.mOpt_summed_area      = bit[5]?true:false;
      tex.mOpt_use_alpha        = bit[6]?true:false;
      tex.mOpt_one_channel_tint = bit[7]?true:false;
      tex.mOpt_ignore_alpha     = bit[8]?true:false;
      tex.mOpt_rgb_tint         = bit[9]?true:false;
    }
    break;
    // U scale
    case ID_MAT_USCALE:
    {
      tex.mUScale = mInputFile->readFloat();
    }
    break;
    // V scale
    case ID_MAT_VSCALE:
    {
      tex.mVScale = mInputFile->readFloat();
    }
    break;
    // U offset
    case ID_MAT_UOFFSET:
    {
      tex.mUOffset = mInputFile->readFloat();
    }
    break;
    // V offset
    case ID_MAT_VOFFSET:
    {
      tex.mVOffset = mInputFile->readFloat();
    }
    break;
    // rotation
    case ID_MAT_MAP_ROTATION:
    {
      tex.mRotation = mInputFile->readFloat();
    }
    break;
    default:
      skipChunk();
    }
  }
  return tex;
}
//-----------------------------------------------------------------------------
// Object Block
void A3DSLoader::read_OBJECT_BLOCK()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  // unsigned int id = mChunkId;

  mObjects.push_back( A3DSObject() );
  mObjects.back().mObjName = readLine();

  while(mInputFile->position() < chunk_end && !mCorrupted)
  {
    readChunk();

    switch(mChunkId)
    {
    case ID_TRIANGULAR_MESH:
      read_TRIANGULAR_MESH();
    break;

    default:
      // Reject lights and cameras
      mObjects.pop_back();
      skipChunk();
    }
  }
}
//-----------------------------------------------------------------------------
// Triangular mesh
void A3DSLoader::read_TRIANGULAR_MESH()
{
  long long chunk_end = (int)mInputFile->position() + mChunkLen - 6;
  if ( chunk_end > mInputFile->size() )
  {
    Log::error("3ds file is corrupted.\n");
    mCorrupted = true;
    mObjects.clear();
    mMaterials.clear();
    chunk_end = mInputFile->size();
  }

  // unsigned int id = mChunkId;

  while(mInputFile->position() < chunk_end && !mCorrupted)
  {
    readChunk();

    switch(mChunkId)
    {
    // Vertices list
    case ID_VERTEX_LIST:
    {
      unsigned short vertc = mInputFile->readUInt16();
      if (!vertc)
        break;
      mObjects.back().mVertices.resize(vertc);
      #if 1
        std::vector<fvec3> verts;
        verts.resize(vertc);
        mInputFile->read(&verts[0], 3*vertc*sizeof(float));
        for(unsigned short i=0; i<vertc; ++i) 
          mObjects.back().mVertices[i].mPos = verts[i];
      #else
        for(unsigned short i=0; i<vertc; ++i) 
          mObjects.back().mVertices[i].mPos = readVec3();
      #endif
    }
    break;
    // Faces description
    case ID_FACE_LIST:
    {
      unsigned short facec= mInputFile->readUInt16();
      if (!facec)
        break;
      mObjects.back().mFaceList.resize(facec);
      #if 1
        std::vector<unsigned short> faces;
        faces.resize(facec*4);
        mInputFile->readUInt16(&faces[0], faces.size());
        for(unsigned short i=0; i<facec; ++i)
        {
          mObjects.back().mFaceList[i].mA     = faces[i*4+0];
          mObjects.back().mFaceList[i].mB     = faces[i*4+1];
          mObjects.back().mFaceList[i].mC     = faces[i*4+2];
          mObjects.back().mFaceList[i].mFlags = faces[i*4+3];
        }
      #else
        for(unsigned short i=0; i<facec; ++i)
        {
          mObjects.back().mFaceList[i].a     = mInputFile->readUInt16();
          mObjects.back().mFaceList[i].b     = mInputFile->readUInt16();
          mObjects.back().mFaceList[i].c     = mInputFile->readUInt16();
          mObjects.back().mFaceList[i].flags = mInputFile->readUInt16();
        }
      #endif
    }
    break;
    // Face material list
    case ID_FACE_MATERIAL_LIST:
    {
      String name = readLine();
      unsigned short facec = mInputFile->readUInt16();
      if (!facec)
        break;
      mObjects.back().mMatFaceMap.push_back(A3DSMaterialFaceMapping());
      mObjects.back().mMatFaceMap.back().mMaterialName = name;
      mObjects.back().mMatFaceMap.back().mMappedFace.resize(facec);
      #if 1
        mInputFile->readUInt16(&mObjects.back().mMatFaceMap.back().mMappedFace[0], facec);
      #else
        for(unsigned short i=0; i<facec; ++i)
        {
          unsigned short face = mInputFile->readUInt16();
          mObjects.back().mMatFaceMap.back().mMappedFace[i] = face;
        }
      #endif
    }
    break;
    // Smoothing group list
    case ID_SMOOTHING_GROUP_LIST:
    {
      #if 1
        if (mObjects.back().mFaceList.empty())
          break;
        std::vector<unsigned int> group;
        group.resize(mObjects.back().mFaceList.size());
        mInputFile->readUInt32(&group[0], group.size());
        for(unsigned short i=0; i<mObjects.back().mFaceList.size(); ++i)
          mObjects.back().mFaceList[i].mSmoothingGroup = group[i];
      #else
        for(unsigned short i=0; i<mObjects.back().mFaceList.size(); ++i)
          mObjects.back().mFaceList[i].mSmoothingGroup = mInputFile->readUInt32();
      #endif
    }
    break;
    // Mapping coordinates
    case ID_MAPPING_COORDS:
    {
      unsigned short vertc = mInputFile->readUInt16();
      if(!vertc)
        break;
      mObjects.back().mVertices.resize(vertc);
      #if 1
        std::vector<fvec2> tuvs;
        tuvs.resize(vertc);
        mInputFile->read(&tuvs[0], 2*vertc*sizeof(float));
        for(unsigned short i=0; i<vertc; ++i) 
          mObjects.back().mVertices[i].mUV = tuvs[i];
      #else
        for(unsigned short i=0; i<vertc; ++i)
        {
          fvec2 uv;
          uv.s() = mInputFile->readFloat();
          uv.t() = mInputFile->readFloat(); // 1 - mInputFile->readFloat(); ? see also mapping flags
          mObjects.back().mVertices[i].mUV = uv;
        }
      #endif
    }
    break;
    // Local coordinates
    case ID_LOCAL_COORDS_SYSTEM:
    {
      fmat4 m;

      fvec3 x,y,z,t;
      x = readVec3();
      y = readVec3();
      z = readVec3();
      t = readVec3();
      m.setX( x );
      m.setY( y );
      m.setZ( z );
      m.setT( t );

      mObjects.back().mCoordSystem = m;
    }
    break;

    default:
      skipChunk();
    }
  }
}
//-----------------------------------------------------------------------------
bool A3DSLoader::parse3DS( VirtualFile* file )
{
  mCorrupted = false;
  mInputFile = file;
  if ( !mInputFile->open(OM_ReadOnly) )
  {
    Log::print(Say("Could not open '%s'. \n") << mInputFile->path() );
    return false;
  }

  // Main 3DS Chunk
  readChunk();
  if (mChunkId == ID_MAIN_CHUNK)
    readChunk();
  else
  {
    Log::error( Say("'%s' is not a valid 3DS file.\n") << mInputFile->path() );
    // will try to use the chunk we just read
  }

  do
  {
    // Inside main
    switch(mChunkId)
    {
    case ID_3DS_VERSION: // 3DS Version
    {
      /*unsigned int version =*/ mInputFile->readUInt32();
    }
    break;

    // 3D Editor
    case ID_3D_EDITOR_CHUNK: 
    {
      read_3D_EDITOR_CHUNK();
    }
    break;

    default:
      skipChunk();
    }
    readChunk();
  }
  while( !mInputFile->endOfFile() && !mCorrupted );

  mInputFile->close();
  return !mCorrupted;
}
//-----------------------------------------------------------------------------
ref<ResourceDatabase> vl::load3DS(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);

  if (file)
    return load3DS( file.get() );
  else
  {
    Log::error( Say("Could not locate '%s'.\n") << path );
    return NULL;
  }
}
ref<ResourceDatabase> vl::load3DS(VirtualFile* file)
{
  if (!file)
    return NULL;

  ref<ResourceDatabase> res_db = new ResourceDatabase;

  A3DSLoader loader;
  /*bool force_double_face = false;*/
  if (!loader.parse3DS(file))
    return NULL;

  ref<Effect> default_effect = new Effect;
  res_db->resources().push_back(default_effect.get());

  default_effect->setObjectName("3ds default effect");
  default_effect->shader()->enable(EN_DEPTH_TEST);
  /*default_effect->shader()->gocLightModel()->setTwoSide(true);*/
  /* default_effect->shader()->disable(EN_CULL_FACE); */

  std::map< String, ref<Effect> > mat_map;

  for(unsigned int iobj=0; iobj<loader.mObjects.size(); ++iobj)
  {
    if (loader.mObjects[iobj].mVertices.empty())
      continue;

    if (loader.mObjects[iobj].mFaceList.empty())
      continue;

    // create a Geometry for each material group
    for(unsigned imat_map=0; imat_map<loader.mObjects[iobj].mMatFaceMap.size() || (imat_map==0&&loader.mObjects[iobj].mMatFaceMap.empty()); ++imat_map)
    {
      int mat_index = -1;

      if (!loader.mObjects[iobj].mMatFaceMap.empty())
      {
        for(unsigned imat=0; imat<loader.mMaterials.size(); ++imat)
        {
          if (loader.mObjects[iobj].mMatFaceMap[imat_map].mMaterialName == loader.mMaterials[imat].mMaterialName)
          {
            mat_index = imat;
            break;
          }
        }
      }

      if (mat_index != -1)
      {
        // assign material to mObjects
        for(unsigned int iface=0; iface<loader.mObjects[iobj].mMatFaceMap[imat_map].mMappedFace.size(); iface++)
        {
          int face_index = loader.mObjects[iobj].mMatFaceMap[imat_map].mMappedFace[iface];
          loader.mObjects[iobj].mFaceList[face_index].mMaterialIndex = mat_index;
        }
      }

      ref<Geometry> geom = new Geometry;
      ref<Actor>    act  = new Actor(geom.get());
      geom->setObjectName( loader.mObjects[iobj].mObjName.toStdString().c_str() );
      act ->setObjectName( loader.mObjects[iobj].mObjName.toStdString().c_str() );

      // builds the vertex sets: a vertex belongs to a single group

      std::set<A3DSVertex> vertex_set;
      std::vector<unsigned int> index_buffer;
      index_buffer.resize( 3 * loader.mObjects[iobj].mFaceList.size() );
      int index_counter = 0;
      for(unsigned int iface=0; iface<loader.mObjects[iobj].mFaceList.size(); iface++)
      {
        if ( loader.mObjects[iobj].mFaceList[iface].mMaterialIndex != mat_index )
          continue;

        unsigned int vertidx[] = 
        {
          loader.mObjects[iobj].mFaceList[iface].mA,
          loader.mObjects[iobj].mFaceList[iface].mB,
          loader.mObjects[iobj].mFaceList[iface].mC
        };

        for(int iv=0; iv<3; ++iv)
        {
          A3DSVertex v;
          if (vertidx[iv]>=loader.mObjects[iobj].mVertices.size())
          {
            Log::error("index out of range, 3ds file is corrupted.\n");
            return NULL;
          }
          v = loader.mObjects[iobj].mVertices[ vertidx[iv] ];
          v.mSmoothingGroup = loader.mObjects[iobj].mFaceList[iface].mSmoothingGroup;
          std::set<A3DSVertex>::iterator it = vertex_set.find(v);
          if (it == vertex_set.end())
          {
            v.mIndex = index_counter;
            vertex_set.insert(v);
            index_counter ++;
          }
          else
            v = *it;
          index_buffer[iface*3+iv] = v.mIndex;
        }
      }

      if (index_counter == 0)
        continue;

      // add after we are sure the geometry has content

      res_db->resources().push_back(geom.get());
      res_db->resources().push_back( act.get() );

      // dump vertices and uv

      ref<ArrayFloat3> vert_interf = new ArrayFloat3;
      ref<ArrayFloat2> tuvs_interf = new ArrayFloat2;
      geom->setVertexArray(vert_interf.get());
      geom->setTexCoordArray(0, tuvs_interf.get());
      vert_interf->resize( index_counter );
      tuvs_interf->resize( index_counter );
      for(std::set<A3DSVertex>::iterator ivert=vertex_set.begin(); ivert!=vertex_set.end(); ++ivert)
      {
        vert_interf->at(ivert->mIndex).x() = ivert->mPos.x();
        vert_interf->at(ivert->mIndex).y() = ivert->mPos.z();
        vert_interf->at(ivert->mIndex).z() =-ivert->mPos.y();
        tuvs_interf->at(ivert->mIndex) = ivert->mUV;
      }

      // dump indices

      ref<DrawElementsUInt> polys = new DrawElementsUInt;
      geom->drawCalls()->push_back( polys.get() );
      polys->indexBuffer()->resize( index_buffer.size() );
      VL_CHECK( polys->indexBuffer()->bytesUsed() == sizeof(index_buffer[0]) * index_buffer.size() )
      memcpy(polys->indexBuffer()->ptr(), &index_buffer[0], polys->indexBuffer()->bytesUsed());

      // matrix

      // we still have problems when the pivots are not centered on the object

      // dump materials

      if ( mat_index != -1 )
      {
        const String& mat_name = loader.mMaterials[mat_index].mMaterialName;
        if (mat_map[mat_name].get() == NULL)
        {
          mat_map[mat_name] = new Effect;
          res_db->resources().push_back(mat_map[mat_name].get());

          mat_map[mat_name]->setObjectName(mat_name.toStdString().c_str());

          float alpha = 1.0f - loader.mMaterials[mat_index].mTransparency;
          fvec4 ambient( loader.mMaterials[mat_index].mAmbient.r(), loader.mMaterials[mat_index].mAmbient.g(), loader.mMaterials[mat_index].mAmbient.b(), alpha );
          fvec4 diffuse( loader.mMaterials[mat_index].mDiffuse.r(), loader.mMaterials[mat_index].mDiffuse.g(), loader.mMaterials[mat_index].mDiffuse.b(), alpha );
          fvec4 specular( loader.mMaterials[mat_index].mSpecular.r(), loader.mMaterials[mat_index].mSpecular.g(), loader.mMaterials[mat_index].mSpecular.b(), alpha );
          specular *= loader.mMaterials[mat_index].mShininessStrength;
          mat_map[mat_name]->shader()->gocMaterial()->setAmbient( ambient );
          mat_map[mat_name]->shader()->gocMaterial()->setDiffuse( diffuse );
          mat_map[mat_name]->shader()->gocMaterial()->setSpecular( specular );
          mat_map[mat_name]->shader()->gocMaterial()->setShininess( loader.mMaterials[mat_index].mShininess * 128.0f );

          mat_map[mat_name]->shader()->enable(EN_DEPTH_TEST);
          /* mat_map[mat_name]->shader()->disable(EN_CULL_FACE); */

          if (alpha<1.0f || loader.mMaterials[mat_index].mTexture1.mOpt_use_alpha)
          {
            mat_map[mat_name]->shader()->enable(EN_BLEND);
            mat_map[mat_name]->shader()->gocBlendFunc()->set(BF_SRC_ALPHA, BF_ONE_MINUS_SRC_ALPHA);
          }
          /*if ( loader.mMaterials[mat_index].mDoubleSided || force_double_face )
          {
            mat_map[mat_name]->shader()->gocLightModel()->setTwoSide(true);
          }*/
          if ( !loader.mMaterials[mat_index].mTexture1.mFileName.empty() )
          {
            ref<Texture> texture = new Texture;
            texture->prepareTexture2D(loader.mMaterials[mat_index].mTexture1.mFileName, TF_RGBA);
            if (loader.mMaterials[mat_index].mTexture1.mOpt_tile)
            {
              texture->getTexParameter()->setWrapS(TPW_REPEAT);
              texture->getTexParameter()->setWrapT(TPW_REPEAT);
              texture->getTexParameter()->setWrapR(TPW_REPEAT);
            }
            else
            {
              texture->getTexParameter()->setWrapS(TPW_CLAMP);
              texture->getTexParameter()->setWrapT(TPW_CLAMP);
              texture->getTexParameter()->setWrapR(TPW_CLAMP);
            }
            mat_map[mat_name]->shader()->gocTextureSampler(0)->setTexture( texture.get() );
          }
        }

        act->setEffect( mat_map[mat_name].get() );
      }
      else
        act->setEffect( default_effect.get() );

      /* now managed by GeometryLoadCallback */
      /*// this will already take into consideration the smoothing groups
      geom->computeNormals();*/
    }
  }

  return res_db;
}
//-----------------------------------------------------------------------------
