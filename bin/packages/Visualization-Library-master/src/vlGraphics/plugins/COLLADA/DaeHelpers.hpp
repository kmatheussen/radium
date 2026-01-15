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

#ifndef DaHelpers_INCLUDE_ONCE
#define DaHelpers_INCLUDE_ONCE

// needs to be but here because the awesome geniouses that programmed collada-dom 2.3 cleverly decided to use the same macro names as OpenGL, see domTypes.h
#ifdef _MSC_VER
  #pragma warning(disable:4100)
  #pragma warning(disable:4355)
  #pragma warning(disable:4512)
#endif
  #include <dae.h>
  #include <dae/domAny.h>
  #include <dom.h>
  #include <dom/domCOLLADA.h>
  #include <dom/domProfile_COMMON.h>
#ifdef _MSC_VER
  #pragma warning(default:4100)
  #pragma warning(default:4355)
  #pragma warning(default:4512)
#endif

#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Shader.hpp>
#include <vlGraphics/Light.hpp>
#include <vlGraphics/MultiDrawElements.hpp>
#include <vlCore/glsl_math.hpp>

namespace vl
{
  namespace Dae
  {
    //-----------------------------------------------------------------------------
    typedef enum { PT_UNKNOWN, PT_LINES, PT_LINE_STRIP, PT_POLYGONS, PT_POLYLIST, PT_TRIANGLES, PT_TRIFANS, PT_TRISTRIPS } EPrimitiveType;
    //-----------------------------------------------------------------------------
    typedef enum { OM_A_ONE, OM_RGB_ZERO } EOpaqueMode;
    //-----------------------------------------------------------------------------
    typedef enum
    {
      IS_UNKNOWN,
      IS_BINORMAL,
      IS_COLOR,
      IS_CONTINUITY,
      IS_IMAGE,
      IS_INPUT,
      IS_IN_TANGENT,
      IS_INTERPOLATION,
      IS_INV_BIND_MATRIX,
      IS_JOINT,
      IS_LINEAR_STEPS,
      IS_MORPHS_TARGET,
      IS_MORPH_WEIGHT,
      IS_NORMAL,
      IS_OUTPUT,
      IS_OUT_TANGENT,
      IS_POSITION,
      IS_TANGENT,
      IS_TEXBINORMAL,
      IS_TEXCOORD,
      IS_TEXTANGENT,
      IS_UV,
      IS_VERTEX,
      IS_WEIGHT
    } EInputSemantic;
    //-----------------------------------------------------------------------------
    //! COLLADA vertex
    struct Vert
    {
      static const int MAX_ATTRIBS = 8;

      Vert()
      {
        memset(mAttribIndex, 0xFF, sizeof(mAttribIndex));
        mIndex = (size_t)-1;
      }

      bool operator<(const Dae::Vert& other) const
      {
        for(int i=0; i<MAX_ATTRIBS; ++i)
        {
          if (mAttribIndex[i] != other.mAttribIndex[i])
            return mAttribIndex[i] < other.mAttribIndex[i];
        }
        return false;
      }

      size_t mAttribIndex[MAX_ATTRIBS];
      size_t mIndex;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA data source
    struct Source: public Object
    {
      Source()
      {
        mFloatSource = NULL;
        mIntSource   = NULL;
        mBoolSource  = NULL;
        mFieldsMask = 0;
        mStride     = 0;
        mOffset     = 0;
        mCount      = 0;
        mDataSize   = 0;
      }

      //! Initializes an accessor. An accessor can read only up to 32 floats.
      void init(domFloat_arrayRef data_src, domUint count, domUint stride, domUint offset, size_t fields_mask)
      {
        mFloatSource = data_src;
        mIntSource   = NULL;
        mBoolSource  = NULL;
        mCount       = (size_t)count;
        mStride      = (size_t)stride;
        mOffset      = (size_t)offset;
        mFieldsMask  = fields_mask;

        // count the number of scalars that will be read.
        mDataSize = 0;
        for(size_t i=0; i<32; ++i)
          if (mFieldsMask & (1<<i))
            mDataSize++;
      }

      //! Initializes an accessor. An accessor can read only up to 32 floats.
      void init(domInt_arrayRef data_src, domUint count, domUint stride, domUint offset, size_t fields_mask)
      {
        mFloatSource = NULL;
        mIntSource   = data_src;
        mBoolSource  = NULL;
        mCount       = (size_t)count;
        mStride      = (size_t)stride;
        mOffset      = (size_t)offset;
        mFieldsMask  = fields_mask;

        // count the number of scalars that will be read.
        mDataSize = 0;
        for(size_t i=0; i<32; ++i)
          if (mFieldsMask & (1<<i))
            mDataSize++;
      }

      //! Initializes an accessor. An accessor can read only up to 32 floats.
      void init(domBool_arrayRef data_src, domUint count, domUint stride, domUint offset, size_t fields_mask)
      {
        mFloatSource = NULL;
        mIntSource   = NULL;
        mBoolSource  = data_src;
        mCount       = (size_t)count;
        mStride      = (size_t)stride;
        mOffset      = (size_t)offset;
        mFieldsMask  = fields_mask;

        // count the number of scalars that will be read.
        mDataSize = 0;
        for(size_t i=0; i<32; ++i)
          if (mFieldsMask & (1<<i))
            mDataSize++;
      }

      //! Reads an element of data at the n-th position and writes it into 'output'. The number of elements that will be written can be queried by calling dataSize().
      void readData(size_t n, float* output)
      {
        size_t read_pos = mOffset + n * mStride;

        size_t pos = 0;

        if(mFloatSource)
        {
          VL_CHECK( (n < mCount) || (read_pos < mFloatSource->getValue().getCount() - mDataSize) )
          for(size_t i=0; i<32 && i<mStride; ++i)
            if (mFieldsMask & (1<<i))
              output[pos++] = (float)mFloatSource->getValue()[read_pos+i];
        }
        else
        if(mIntSource)
        {
          VL_CHECK( (n < mCount) || (read_pos < mIntSource->getValue().getCount() - mDataSize) )
          for(size_t i=0; i<32 && i<mStride; ++i)
            if (mFieldsMask & (1<<i))
              output[pos++] = (float)mIntSource->getValue()[read_pos+i];
        }
        else
        if(mBoolSource)
        {
          VL_CHECK( (n < mCount) || (read_pos < mBoolSource->getValue().getCount() - mDataSize) )
          for(size_t i=0; i<32 && i<mStride; ++i)
            if (mFieldsMask & (1<<i))
              output[pos++] = (float)mBoolSource->getValue()[read_pos+i];
        }
      }

      //! The number of elements in the source.
      size_t count() const { return mCount; }

      //! The number of elements written by readData().
      size_t dataSize() const { return mDataSize; }

    protected:
      size_t mFieldsMask;
      size_t mDataSize;
      domFloat_arrayRef mFloatSource;
      domInt_arrayRef  mIntSource;
      domBool_arrayRef mBoolSource;
      size_t mStride;
      size_t mOffset;
      size_t mCount;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA input stream
    struct Input: public Object
    {
      Input()
      {
        mSemantic = Dae::IS_UNKNOWN;
        mOffset = 0;
        mSet = 0;
      }

      ref<Dae::Source> mSource;
      Dae::EInputSemantic mSemantic;
      size_t mOffset;
      size_t mSet;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA primitive
    struct Primitive: public Object
    {
      Primitive()
      {
        mType = Dae::PT_UNKNOWN;
        mCount = 0;
        mIndexStride = 0;
      }

      Dae::EPrimitiveType mType;
      std::string mMaterial;
      std::vector< ref<Dae::Input> > mChannels;
      size_t mCount;
      std::vector<domPRef> mP;
      size_t mIndexStride;
      ref<Geometry> mGeometry;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA mesh
    struct Mesh: public Object
    {
      std::vector< ref<Dae::Input> > mVertexInputs;
      std::vector< ref<Dae::Primitive> > mPrimitives;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA node
    struct Node: public Object
    {
      Node()
      {
        mTransform = new Transform;
      }

      ref<Transform> mTransform;
      std::vector< ref<Dae::Node> > mChildren;
      std::vector< ref<Dae::Mesh> > mMesh;
      std::vector< ref<Actor> > mActors;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA surface
    struct Surface: public Object
    {
      // mic fixme: for the moment we only support 2D images.
      ref<Image> mImage; // <init_from>
    };
    //-----------------------------------------------------------------------------
    //! COLLADA sampler2D
    struct Sampler2D: public Object
    {
      Sampler2D()
      {
        // default values if no tags are found.
        mMinFilter = TPF_LINEAR;
        mMagFilter = TPF_LINEAR;
        mWrapS     = TPW_REPEAT;
        mWrapT     = TPW_REPEAT;
      }

      ref<Dae::Surface> mDaeSurface;    // <source>
      vl::ETexParamFilter mMinFilter; // <minfilter>
      vl::ETexParamFilter mMagFilter; // <magfilter>
      vl::ETexParamWrap   mWrapS;     // <wrap_s>
      vl::ETexParamWrap   mWrapT;     // <wrap_t>

      ref<Texture> mTexture; // actual VL texture implementing the sampler
    };
    //-----------------------------------------------------------------------------
    //! COLLADA newparam
    struct NewParam: public Object
    {
      ref<Dae::Sampler2D> mDaeSampler2D;
      ref<Dae::Surface> mDaeSurface;
      fvec4 mFloat4;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA color or texture input
    struct ColorOrTexture: public Object
    {
      fvec4 mColor;
      ref<Dae::Sampler2D> mSampler;
      std::string mTexCoord;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA common technique
    struct TechniqueCOMMON: public Object
    {
      TechniqueCOMMON()
      {
        mMode = Unknown;

        mEmission.mColor     = fvec4(0, 0, 0, 1);
        mAmbient.mColor      = fvec4(0, 0, 0, 1);
        mDiffuse.mColor      = fvec4(1, 0, 1, 1);
        mSpecular.mColor     = fvec4(0, 0, 0, 1);
        mShininess    = 40;

        mReflective.mColor   = fvec4(1, 1, 1, 1);
        mReflectivity = 0;

        mTransparent.mColor  = fvec4(0, 0, 0, 1);
        mOpaqueMode   = Dae::OM_A_ONE;
        mTransparency = 1;

        mIndexOfRefraction = 0;

        mBlendingOn = false;
      }

      enum { Unknown, Blinn, Phong, Lambert } mMode;

      Dae::EOpaqueMode mOpaqueMode;

      Dae::ColorOrTexture mEmission;
      Dae::ColorOrTexture mAmbient;
      Dae::ColorOrTexture mDiffuse;
      Dae::ColorOrTexture mSpecular;
      float mShininess;
      Dae::ColorOrTexture mReflective;
      float mReflectivity;
      Dae::ColorOrTexture mTransparent;
      float mTransparency;
      float mIndexOfRefraction;
      bool mBlendingOn;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA effect
    struct Effect: public Object
    {
      Effect()
      {
        mDoubleSided = false;
      }

      std::vector<ref<Dae::NewParam> > mNewParams;
      ref<Dae::TechniqueCOMMON> mDaeTechniqueCOMMON;
      bool mDoubleSided;
    };
    //-----------------------------------------------------------------------------
    //! COLLADA material
    struct Material: public Object
    {
      ref<Dae::Effect> mDaeEffect;
    };
    //-----------------------------------------------------------------------------
  }
}

#endif
