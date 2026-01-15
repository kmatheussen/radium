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

#if !defined(LoadPLY_INCLUDE_ONCE)
#define LoadPLY_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>
#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>

namespace vl
{
  class VirtualFile;
  class TextStream;
}

namespace vl
{
//-----------------------------------------------------------------------------
  ref<ResourceDatabase> loadPLY(VirtualFile* file);
  ref<ResourceDatabase> loadPLY(const String& path);
//---------------------------------------------------------------------------
// LoadWriterPLY
//---------------------------------------------------------------------------
  /**
   * The LoadWriterPLY class is a ResourceLoadWriter capable of reading PLY files.
   */
  class LoadWriterPLY: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterPLY, ResourceLoadWriter)

  public:
    LoadWriterPLY(): ResourceLoadWriter("|ply|", "|ply|") {}

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      return loadPLY(path);
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      return loadPLY(file);
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
// PlyLoader
//-----------------------------------------------------------------------------
  /**
   * Loads a PLY file.
   */
  class VLGRAPHICS_EXPORT PlyLoader
  {
  public:
    typedef enum
    {
      PlyError,
      PlyChar,
      PlyUChar,
      PlyShort,
      PlyUShort,
      PlyInt,
      PlyUInt,
      PlyFloat,
      PlyDouble
    } EType;
    //! Used by PlyLoader
    class PlyPropertyAbstract: public Object
    {
    public:
      const String& name() const { return mName; }
      void setName(const String& name) { mName = name; }
      virtual void read(VirtualFile*, bool le) = 0;
      virtual void read(TextStream* text) = 0;
    protected:
      String mName;
    };
    //! Used by PlyLoader
    class PlyScalar: public PlyPropertyAbstract
    {
    public:
      PlyScalar(): mScalarType(PlyError) { mData.mDouble = 0; }
      void setScalarType(EType type) { mScalarType = type; }
      EType scalarType() const { return mScalarType; }
      virtual void read(VirtualFile* file, bool le);
      virtual void read(TextStream* text);
      float getAsFloat() const;
      int getAsInt() const;
    protected:
      union
      {
        char mChar;
        unsigned char mUChar;
        short mShort;
        unsigned short mUShort;
        int mInt;
        unsigned int mUInt;
        float mFloat;
        double mDouble;
      } mData;
      EType mScalarType;
    };
    //! Used by PlyLoader
    class PlyScalarList: public PlyPropertyAbstract
    {
    public:
      PlyScalarList(): mScalarType(PlyError), mCountType(PlyError) {}
      void setCountType(EType type) { mCountType = type; }
      EType countType() const { return mCountType; }
      void setScalarType(EType type) { mScalarType = type; }
      EType scalarType() const { return mScalarType; }
      const std::vector<PlyScalar>& scalars() const { return mScalars; }
      std::vector<PlyScalar>& scalars() { return mScalars; }
      virtual void read(VirtualFile* file, bool le) 
      {
        PlyScalar c;
        c.setScalarType(countType());
        c.read(file,le);
        scalars().resize(c.getAsInt());
        for(unsigned i=0; i<scalars().size(); ++i)
        {
          scalars()[i].setScalarType(scalarType());
          scalars()[i].read(file,le);
        }
      }
      virtual void read(TextStream* text)
      {
        PlyScalar c;
        c.setScalarType(countType());
        c.read(text);
        scalars().resize(c.getAsInt());
        for(unsigned i=0; i<scalars().size(); ++i)
        {
          scalars()[i].setScalarType(scalarType());
          scalars()[i].read(text);
        }
      }
    protected:
      std::vector<PlyScalar> mScalars;
      EType mScalarType;
      EType mCountType;
    };
    //! Used by PlyLoader
    class PlyElement: public Object
    {
    public:
      PlyElement(): mElemCount(0) {}
      const String& name() const { return mName; }
      void setName(const String& name) { mName = name; }
      const std::vector< ref<PlyPropertyAbstract> >& properties() const { return mProperties; }
      std::vector< ref<PlyPropertyAbstract> >& properties() { return mProperties; }
      int elemCount() const { return mElemCount; }
      void setElemCount(int count) { mElemCount = count; }
      virtual void read(VirtualFile* file, bool le)
      {
        for(unsigned int i=0; i<mProperties.size(); ++i)
          mProperties[i]->read(file, le);
      }
      virtual void read(TextStream* text)
      {
        for(unsigned int i=0; i<mProperties.size(); ++i)
          mProperties[i]->read(text);
      }
      fvec3 getVertex() const
      {
        fvec3 v;
        if (mVertex[0]) v.x() = mVertex[0]->getAsFloat();
        if (mVertex[1]) v.y() = mVertex[1]->getAsFloat();
        if (mVertex[2]) v.z() = mVertex[2]->getAsFloat();
        return v;
      }
      fvec3 getNormal() const
      {
        fvec3 v;
        if (mNormal[0]) v.x() = mNormal[0]->getAsFloat();
        if (mNormal[1]) v.y() = mNormal[1]->getAsFloat();
        if (mNormal[2]) v.z() = mNormal[2]->getAsFloat();
        return v;
      }
      ubvec4 getColor() const
      {
        ubvec4 v;
        if (mColor[0]) v.r() = (unsigned char)mColor[0]->getAsInt();
        if (mColor[1]) v.g() = (unsigned char)mColor[1]->getAsInt();
        if (mColor[2]) v.b() = (unsigned char)mColor[2]->getAsInt();
        if (mColor[3]) v.a() = (unsigned char)mColor[3]->getAsInt();
        return v;
      }
      void analyze();
    protected:
      std::vector< ref<PlyPropertyAbstract> > mProperties;
      std::vector< ref<PlyScalar> > mVertex;
      std::vector< ref<PlyScalar> > mNormal;
      std::vector< ref<PlyScalar> > mColor;
      String mName;
      int mElemCount;
    };
  public:
    //! Constructor
    PlyLoader(): mBinary(false), mLittleEndian(false) {}
    //! Loads a PLY file.
    ref<ResourceDatabase> loadPly(VirtualFile* file);
    const std::vector< ref<PlyElement> >& elements() const { return mElements; }
    std::vector< ref<PlyElement> >& elements() { return mElements; }
    bool binary() const { return mBinary; }
    bool littleEndian() const { return mLittleEndian; }
    void readElements(VirtualFile* file);
    void readElements(TextStream* text);
    void newElement(PlyElement*el);
    EType translateType(const String& type);
    void analyzeHeader();
    bool readHeader(TextStream* line_reader);
  protected:
    std::vector< ref<PlyElement> > mElements;
    ref<ArrayFloat3>  mVerts;
    ref<ArrayFloat3>  mNormals;
    ref<ArrayUByte4> mColors;
    std::vector<unsigned int> mIndices;
    int mVertexIndex;
    bool mBinary;
    bool mLittleEndian;
};
};

#endif

