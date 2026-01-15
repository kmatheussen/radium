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

#ifndef MarchingCubes_INCLUDE_ONCE
#define MarchingCubes_INCLUDE_ONCE

#include <vlVolume/link_config.hpp>
#include <vlGraphics/Geometry.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // Volume
  //------------------------------------------------------------------------------
  /**
   * Defines the volume data to be used with a MarchingCube object.
   */
  class VLVOLUME_EXPORT Volume: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Volume, Object)

    /**
     * A Volume cell.
     */
    struct Cube
    {
      Cube(): mMin(0), mMax(0) {}
      float mMin, mMax;
      bool includes(float v) const { return v >= mMin && v <= mMax; }
    };
  public:
    Volume();

    //! Setup the volume data with the specified memory management.
    //! \param data The buffer containing the volume data. Can be NULL only if \p use_directly == \p false and \p copy_data == \p false.
    //! \param use_directly If \p true the buffer will be used directly and no internal copy will be done.
    //! \param copy_data If \p true the buffer pointed by \p data will be copied in the internal buffer used to store the volume data.
    //! The \p copy_data parameter is ignored if \p use_directly is set to \p true.
    void setup(float* data, bool use_directly, bool copy_data, const fvec3& bottom_left, const fvec3& top_right, const ivec3& slices);

    void setup(const Volume&);

    /** Returns a new volume which is half of the size of the original volume in each direction (thus requires up to 1/8th of the memory).
        Use this function when the volume data to be processed is too big or produces too many polygons.
     */
    ref<Volume> downsample() const;

    const float* values() const { return mValues; }

    float* values() { return mValues; }

    const float& value(int i) const { return mValues[i]; }

    float& value(int i) { return mValues[i]; }

    const float& value(int x, int y, int z) const { return mValues[x + mSlices.x()*y + mSlices.x()*mSlices.y()*z]; }

    float& value(int x, int y, int z) { return mValues[x + mSlices.x()*y + mSlices.x()*mSlices.y()*z]; }

    //! Computes a high quality normal (best rendering quality)
    void normalHQ(fvec3& normal, const fvec3& v, float dx, float dy, float dz);

    //! Computes a low quality normal (best performances)
    void normalLQ(fvec3& normal, const fvec3& v, float dx, float dy, float dz);

    //! Samples the volume using tri-linear interpolation sampling
    float sampleSmooth(float x, float y, float z) const;

    //! Samples the volume using nearest point sampling
    float sampleNearest(float x, float y, float z) const;

    fvec3 coordinate(int x, int y, int z) const { return mBottomLeft + fvec3(float(mCellSize.x()*x), float(mCellSize.y()*y), float(mCellSize.z()*z)); }

    const fvec3& bottomLeft() const { return mBottomLeft; }

    const fvec3& topRight() const { return mTopRight; }

    const ivec3& slices() const { return mSlices; }

    fvec3 size() const { return mSize; }

    float computeMinimum() const;

    float computeMaximum() const;

    float computeAverage() const;

    float minimum() const { return mMinimum; }

    float maximum() const { return mMaximum; }

    float average() const { return mAverage; }

    const Volume::Cube& cube(int x, int y, int z) const 
    { 
      VL_CHECK(x<slices().x()-1)
      VL_CHECK(y<slices().y()-1)
      VL_CHECK(z<slices().z()-1)
      return mCubes[ x + y*(slices().x()-1) + z*(slices().x()-1)*(slices().y()-1) ]; 
    }

    //! Returns the x/y/z size of a cell
    const fvec3& cellSize() const { return mCellSize; }

    //! Returns true if the internal data hasn't been updated since the last call to setDataDirty() or setup()
    bool dataIsDirty() const { return mDataIsDirty; }

    //! Notifies that the data of a Volume has changed and that the internal acceleration structures should be recomputed.
    void setDataDirty() { mDataIsDirty = true; }

    void setupInternalData();

  protected:
    std::vector<float> mInternalValues;
    float* mValues;
    fvec3 mBottomLeft;
    fvec3 mTopRight;
    fvec3 mSize;
    ivec3 mSlices;
    fvec3 mCellSize;
    float mMinimum;
    float mMaximum;
    float mAverage;
    bool mDataIsDirty;

    std::vector<Cube> mCubes;
  };
  //------------------------------------------------------------------------------
  // VolumeInfo
  //------------------------------------------------------------------------------
  /**
   * Defines the volume parameters to be used with a MarchingCube and Volume object.
   */
  class VolumeInfo: public Object
  {
    VL_INSTRUMENT_CLASS(vl::VolumeInfo, Object)

  public:
    VolumeInfo()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mThreshold = 0;
      mVolume = NULL;
      mVert0 = -1;
      mVertC = -1;
    }

    VolumeInfo(Volume* vol, float threshold)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mThreshold = threshold;
      mVolume = vol;
      mVert0 = -1;
      mVertC = -1;
    }

    VolumeInfo(Volume* vol, float threshold, const fvec4& color)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mColor = color;
      mThreshold = threshold;
      mVolume = vol;
      mVert0 = -1;
      mVertC = -1;
    }

    void setColor(const fvec4& col) { mColor = col; }
    const fvec4& color() const { return mColor; }

    void setThreshold(float t) { mThreshold = t; }
    float threshold() const { return mThreshold; }

    void setVolume(Volume* vol) { mVolume = vol; }
    const Volume* volume() const { return mVolume.get(); }
    Volume* volume() { return mVolume.get(); }

    void setVert0(int index) { mVert0 = index; }
    int vert0() const { return mVert0; }

    void setVertC(int count) { mVertC = count; }
    int vertC() const { return mVertC; }

  protected:
    fvec4 mColor;
    float mThreshold;
    ref<Volume> mVolume;
    int mVert0, mVertC;
  };
  //------------------------------------------------------------------------------
  // MarchingCubes
  //------------------------------------------------------------------------------
  /**
   * An efficient implementation of the Marching Cubes algorithm.
   */
  class VLVOLUME_EXPORT MarchingCubes
  {
  public:
    MarchingCubes();
  
    void run(bool generate_colors);

    void reset();

    const Collection<VolumeInfo>* volumeInfo() const { return &mVolumeInfo; }
    Collection<VolumeInfo>* volumeInfo() { return &mVolumeInfo; }

    void updateColor(const fvec3& color, int volume_index);
    void updateColor(const fvec4& color, int volume_index);
    void updateAlpha(float alpha, int volume_index);

    //! Select hight quality normals for best rendering quality, select low quality normals for best performances.
    void setHighQualityNormals(bool hq) { mHighQualityNormals = hq; }
    //! Select hight quality normals for best rendering quality, select low quality normals for best performances.
    bool highQualityNormals() const { return mHighQualityNormals; }

  public:
    ref<ArrayFloat3> mVertsArray;
    ref<ArrayFloat3> mNormsArray;
    ref<ArrayFloat4> mColorArray;
    
    // OpenGL ES does not support DrawElementsUInt
#if defined(VL_OPENGL)
    ref<DrawElementsUInt> mDrawElements;
#else
    ref<DrawElementsUShort> mDrawElements;
#endif

  protected:
    void computeEdges(Volume*, float threshold);
    void processCube(int x, int y, int z, Volume* vol, float threshold);

  private:
    std::vector<fvec3> mVerts;
    std::vector<fvec3> mNorms;
    std::vector<fvec4> mColors;
    
#if defined(VL_OPENGL)
    typedef unsigned int IndexType;
#else
    typedef unsigned short IndexType;
#endif
    std::vector<IndexType> mIndices;

    struct Edge
    {
      Edge(): mX(-1), mY(-1), mZ(-1) {}
      int mX, mY, mZ;
    };
    std::vector<Edge>  mEdges;
    std::vector<usvec3> mCubes;
    Collection<VolumeInfo> mVolumeInfo;
    bool mHighQualityNormals;

  protected:
    static const int mTriangleConnectionTable[256][16];
    static const int mCubeEdgeFlags[256];
  };
  //------------------------------------------------------------------------------
}

#endif
