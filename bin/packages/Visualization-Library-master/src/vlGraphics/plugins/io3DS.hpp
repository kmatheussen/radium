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

#if !defined(Load3DS_INCLUDE_ONCE)
#define Load3DS_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/String.hpp>
#include <vector>

namespace vl
{
  class VirtualFile;
}

namespace vl
{
//-----------------------------------------------------------------------------
  VLGRAPHICS_EXPORT ref<ResourceDatabase> load3DS(VirtualFile* file);
  VLGRAPHICS_EXPORT ref<ResourceDatabase> load3DS(const String& path);
//-----------------------------------------------------------------------------
// LoadWriter3DS
//-----------------------------------------------------------------------------
  /**
   * The LoadWriter3DS class is a ResourceLoadWriter capable of reading 3DS files.
   */
  class LoadWriter3DS: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriter3DS, ResourceLoadWriter)

  public:
    LoadWriter3DS(): ResourceLoadWriter("|3ds|", "|3ds|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return load3DS(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return load3DS(file);
    }

    //! Not supported yet.
    bool writeResource(const String& /*path*/, ResourceDatabase* /*resource*/) const
    {
      return false;
    }

    //! Not supported yet.
    bool writeResource(VirtualFile* /*file*/, ResourceDatabase* /*resource*/) const
    {
      return false;
    }
  };
//-----------------------------------------------------------------------------
// A3DSLoader
//-----------------------------------------------------------------------------
  /**
   * The A3DSTexture class represents a texture in a 3DS file.
   */
  class A3DSTexture
  {
  public:
    A3DSTexture(): mUScale(1), mVScale(1), mUOffset(1), mVOffset(1), mRotation(0),
    mOpt_tile(true), mOpt_decal(false), mOpt_mirror(false), mOpt_negative(false),
    mOpt_summed_area(false), mOpt_use_alpha(false), mOpt_one_channel_tint(false), 
    mOpt_ignore_alpha(false), mOpt_rgb_tint(false) {}

    String mFileName;
    float mUScale, mVScale, mUOffset, mVOffset, mRotation;
    bool mOpt_tile;
    bool mOpt_decal;
    bool mOpt_mirror;
    bool mOpt_negative;
    bool mOpt_summed_area; // summed area map filtering (instead of pyramidal)
    bool mOpt_use_alpha; // use alpha  (toggles RGBluma/alpha. For masks RGB means RGBluma)
    bool mOpt_one_channel_tint; // there is a one channel tint (either RGBluma or alpha)
    bool mOpt_ignore_alpha; // ignore alpha (take RGBluma even if an alpha exists (?))
    bool mOpt_rgb_tint; // there is a three channel tint (RGB tint)
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSMaterial class represents a material in a 3DS file.
   */
  class A3DSMaterial
  {
  public:
    A3DSMaterial(): mShininess(0), mShininessStrength(0), mTransparency(0), mDoubleSided(false) {}

    String mMaterialName;
    fvec3 mAmbient, mDiffuse, mSpecular;
    float mShininess, mShininessStrength;
    float mTransparency;
    bool mDoubleSided;
    A3DSTexture mTexture1;
    A3DSTexture mTexture2;
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSTriFace class represents a triangle in a 3DS file.
   */
  class A3DSTriFace
  {
  public:
    A3DSTriFace(): mA(0), mB(0), mC(0), mFlags(0), mSmoothingGroup(0), mMaterialIndex(-1) {}

    unsigned short mA,mB,mC,mFlags;
    unsigned int mSmoothingGroup;
    int mMaterialIndex;
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSMaterialFaceMapping class represents the material/face mapping in a 3DS file.
   */
  class A3DSMaterialFaceMapping
  {
  public:
    String mMaterialName;
    std::vector<unsigned short> mMappedFace;
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSVertex class represents a vertex in a 3DS file
   */
  class A3DSVertex
  {
  public:
    A3DSVertex(): mSmoothingGroup(0), mIndex(-1) {}
    bool operator<(const A3DSVertex& other) const
    {
      if (mPos.x() != other.mPos.x()) 
        return mPos.x() < other.mPos.x();
      else
      if (mPos.y() != other.mPos.y()) 
        return mPos.y() < other.mPos.y();
      else
      if (mPos.z() != other.mPos.z()) 
        return mPos.z() < other.mPos.z();
      else
      if (mUV.s() != other.mUV.s()) 
        return mUV.s() < other.mUV.s();
      else
      if (mUV.t() != other.mUV.t()) 
        return mUV.t() < other.mUV.t();
      else
        return mSmoothingGroup < other.mSmoothingGroup;
    }

    fvec3 mPos;
    fvec2 mUV;
    unsigned int mSmoothingGroup;
    int mIndex;
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSObject class represents an object in a 3DS file.
   */
  class A3DSObject
  {
  public:
    String mObjName;
    std::vector<A3DSVertex> mVertices;
    std::vector<A3DSTriFace> mFaceList;
    std::vector<A3DSMaterialFaceMapping> mMatFaceMap;
    fmat4 mCoordSystem;
  };
//-----------------------------------------------------------------------------
  /**
   * The A3DSLoader class loads an Autodesk 3DS file and generates a vector of A3DSObject and A3DSMaterial objects.
   */
  class VLGRAPHICS_EXPORT A3DSLoader
  {
  public:
    A3DSLoader();
    bool parse3DS(VirtualFile* file);

  protected:
    fvec3 readVec3();
    fvec3 readColByte3();
    fvec3 readColFloat3();
    String readLine();
    float readWordPercent();
    float readFloatPercent();
    void readChunk();
    bool skipChunk();
    void read_3D_EDITOR_CHUNK();
    fvec3 readColChunk();
    float readPercentChunk();
    void read_MATERIAL_BLOCK();
    A3DSTexture readMapChunk();
    void read_OBJECT_BLOCK();
    void read_TRIANGULAR_MESH();

  public:
    std::vector<A3DSObject> mObjects;
    std::vector<A3DSMaterial> mMaterials;

  protected:
    VirtualFile *mInputFile;
    unsigned short mChunkId;
    unsigned int mChunkLen;
    bool mCorrupted;
  };
//-----------------------------------------------------------------------------
}

#endif
