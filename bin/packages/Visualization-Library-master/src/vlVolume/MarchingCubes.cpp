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

/* The marching cubes tables are from Cory Bloyd. */

#include <vlVolume/MarchingCubes.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>

using namespace vl;

/** \class vl::MarchingCubes

Pictures from the \ref pagGuideMarchingCubes "Marching Cubes" tutorial.
<center>
<table border=0 cellspacing=0 cellpadding=5>
<tr>
	<td colspan=2> <img src="pics/pagGuideMarchingCubes.jpg"> </td>
	<td colspan=2> <img src="pics/pagGuideMarchingCubes_5.jpg"> </td>
</tr>
<tr>
	<td> <img src="pics/pagGuideMarchingCubes_1.jpg"> </td>
	<td> <img src="pics/pagGuideMarchingCubes_2.jpg"> </td>
	<td> <img src="pics/pagGuideMarchingCubes_3.jpg"> </td>
	<td> <img src="pics/pagGuideMarchingCubes_4.jpg"> </td>
</tr>
</table>
</center>
 */

//------------------------------------------------------------------------------
MarchingCubes::MarchingCubes()
{
  mVertsArray = new ArrayFloat3;
  mNormsArray = new ArrayFloat3;
  mColorArray = new ArrayFloat4;

  // OpenGL ES does not support DrawElementsUInt
#if defined(VL_OPENGL)
  mDrawElements = new DrawElementsUInt(PT_TRIANGLES);
#else
  mDrawElements = new DrawElementsUShort(PT_TRIANGLES);
#endif
  mVolumeInfo.setAutomaticDelete(false);
  mHighQualityNormals = true;
}
//------------------------------------------------------------------------------
// MarchingCubes
//------------------------------------------------------------------------------
void MarchingCubes::computeEdges(Volume* vol, float threshold)
{
  // mEdges.clear();
  mEdges.resize(vol->slices().x() * vol->slices().y() * vol->slices().z());
  mCubes.clear();
  mCubes.reserve(1024);

  /////////////////////////////////////////////////////////////////////////////////
  // note: this funtion can generate double vertices when the 't' is 0.0 or 1.0
  // this is why Geometry::computeNormals() doesn't work well with MarchingCubes
  // If we find a way not to generate such vertices quickly we could use always
  // Geometry::computeNormals() which is much quicker than computing the gradient.
  /////////////////////////////////////////////////////////////////////////////////

  const float dx = vol->cellSize().x() * 0.25f;
  const float dy = vol->cellSize().y() * 0.25f;
  const float dz = vol->cellSize().z() * 0.25f;
  float v0, v1, v2, v3, t;
  int iedge = 0;
  int w = vol->slices().x() -1;
  int h = vol->slices().y() -1;
  int d = vol->slices().z() -1;
  for(unsigned short z = 0; z < vol->slices().z(); ++z)
  {
    for(unsigned short y = 0; y < vol->slices().y(); ++y)
    {
      for(unsigned short x = 0; x < vol->slices().x(); ++x, ++iedge)
      {
        if (x != w && y != h && z != d)
        {
          if (vol->cube(x,y,z).includes(threshold))
          {
            if (mCubes.capacity()-mCubes.size() == 0)
              mCubes.reserve(mCubes.size()*2);
            mCubes.push_back( usvec3(x,y,z) );
          }
          else
            continue;
        }

        if (mVerts.capacity() - mVerts.size() == 0)
        {
          mVerts.reserve( mVerts.size() * 2 );
          mNorms.reserve( mNorms.size() * 2 );
        }

        v0 = vol->value( x,y,z );
        fvec3 v0_coord = vol->coordinate(x, y, z);

        if (x != w)
        {
          v1 = vol->value( x + 1, y, z );
          if (v1!=v0)
          {
            //if (t>=0 && t<=1.0f)
            if ( (threshold>=v0 && threshold<=v1) || (threshold>=v1 && threshold<=v0) )
            {
              t = (threshold-v0)/(v1-v0);
              VL_CHECK(t>=-0.001f && t<=1.001f)
              // emit vertex
              mEdges[iedge].mX = (int)mVerts.size();
              // compute vertex and normal position
              mVerts.push_back( v0_coord * (1.0f-t) + vol->coordinate(x + 1, y, z) * t );
              if (mHighQualityNormals)
              {
                fvec3 n;
                vol->normalHQ(n, mVerts.back(), dx, dy, dz);
                mNorms.push_back(n);
              }
            }
          }
        }
        if (y != h)
        {
          v2 = vol->value( x, y + 1, z );
          if (v2!=v0)
          {
            //if (t>=0 && t<=1.0f)
            if ( (threshold>=v0 && threshold<=v2) || (threshold>=v2 && threshold<=v0) )
            {
              t = (threshold-v0)/(v2-v0);
              VL_CHECK(t>=-0.001f && t<=1.001f)
              // emit vertex
              mEdges[iedge].mY = (int)mVerts.size();
              // compute vertex and normal position
              mVerts.push_back( v0_coord * (1.0f-t) + vol->coordinate(x, y + 1, z) * t );
              if (mHighQualityNormals)
              {
                fvec3 n;
                vol->normalHQ(n, mVerts.back(), dx, dy, dz);
                mNorms.push_back(n);
              }
            }
          }
        }
        if (z != d)
        {
          v3 = vol->value( x, y, z + 1 );
          if (v3!=v0)
          {
            //if (t>=0 && t<=1.0f)
            if ( (threshold>=v0 && threshold<=v3) || (threshold>=v3 && threshold<=v0) )
            {
              t = (threshold-v0)/(v3-v0);
              VL_CHECK(t>=-0.001f && t<=1.001f)
              // emit vertex
              mEdges[iedge].mZ = (int)mVerts.size();
              // compute vertex and normal position
              mVerts.push_back( v0_coord * (1.0f-t) + vol->coordinate(x, y, z + 1) * t );
              if (mHighQualityNormals)
              {
                fvec3 n;
                vol->normalHQ(n, mVerts.back(), dx, dy, dz);
                mNorms.push_back(n);
              }
            }
          }
        }
      }
    }
  }
}
//------------------------------------------------------------------------------
void MarchingCubes::processCube(int x, int y, int z, Volume* vol, float threshold)
{
  int inner_corners = 0;

  if ( vol->value( x,     y,     z     ) < threshold ) inner_corners += 1;
  if ( vol->value( x + 1, y,     z     ) < threshold ) inner_corners += 2;
  if ( vol->value( x + 1, y + 1, z     ) < threshold ) inner_corners += 4;
  if ( vol->value( x,     y + 1, z     ) < threshold ) inner_corners += 8;
  if ( vol->value( x,     y,     z + 1 ) < threshold ) inner_corners += 16;
  if ( vol->value( x + 1, y,     z + 1 ) < threshold ) inner_corners += 32;
  if ( vol->value( x + 1, y + 1, z + 1 ) < threshold ) inner_corners += 64;
  if ( vol->value( x,     y + 1, z + 1 ) < threshold ) inner_corners += 128;

  int cut_edges = mCubeEdgeFlags[inner_corners];

  if(cut_edges == 0)
    return;

  /*
  int cell0 = x     + y * vol->slices().x()     + z * vol->slices().x()*vol->slices().y();
  int cell1 = (x+1) + y * vol->slices().x()     + z * vol->slices().x()*vol->slices().y();
  int cell2 = (x+1) + y * vol->slices().x()     + (z+1) * vol->slices().x()*vol->slices().y();
  int cell3 = x     + y * vol->slices().x()     + (z+1) * vol->slices().x()*vol->slices().y();
  int cell4 = (x+1) + (y+1) * vol->slices().x() + z * vol->slices().x()*vol->slices().y();
  int cell5 = x     + (y+1) * vol->slices().x() + z * vol->slices().x()*vol->slices().y();
  int cell6 = x     + (y+1) * vol->slices().x() + (z+1) * vol->slices().x()*vol->slices().y();
  */

  int z0 = z * vol->slices().x()*vol->slices().y();
  int z1 = (z+1) * vol->slices().x()*vol->slices().y();
  int y0 = y * vol->slices().x();
  int y1 = (y+1) * vol->slices().x();

  int cell0 = x     + y0 + z0;
  int cell1 = (x+1) + y0 + z0;
  int cell2 = (x+1) + y0 + z1;
  int cell3 = x     + y0 + z1;
  int cell4 = (x+1) + y1 + z0;
  int cell5 = x     + y1 + z0;
  int cell6 = x     + y1 + z1;

  int edge_ivert[12] =
  {
    mEdges[cell0].mX,
    mEdges[cell1].mY,
    mEdges[cell5].mX,
    mEdges[cell0].mY,

    mEdges[cell3].mX,
    mEdges[cell2].mY,
    mEdges[cell6].mX,
    mEdges[cell3].mY,

    mEdges[cell0].mZ,
    mEdges[cell1].mZ,
    mEdges[cell4].mZ,
    mEdges[cell5].mZ,
  };

  int ivertex;
  for(int icorner = 0; mTriangleConnectionTable[inner_corners][icorner]>=0; icorner+=3)
  {
    if (mIndices.capacity() - mIndices.size() == 0)
      mIndices.reserve( mIndices.size()*2 );

    ivertex = mTriangleConnectionTable[inner_corners][icorner+0];
    int a = edge_ivert[ivertex];

    ivertex = mTriangleConnectionTable[inner_corners][icorner+1];
    int b = edge_ivert[ivertex];

    ivertex = mTriangleConnectionTable[inner_corners][icorner+2];
    int c = edge_ivert[ivertex];

    if (a<0 || b<0 || c<0)
      continue;

    // skip degenerate tris #1
    if (a==b||b==c||c==a)
      continue;

    #if 0
      // skip degenerate tris #2
      fvec3 v0 = mVerts[a];
      fvec3 v1 = mVerts[b] - v0;
      fvec3 v2 = mVerts[c] - v1;
      if (cross(v2,v1).isNull())
        continue;
    #endif

    mIndices.push_back((IndexType)a);
    mIndices.push_back((IndexType)b);
    mIndices.push_back((IndexType)c);
  }
}
//------------------------------------------------------------------------------
void MarchingCubes::reset()
{
  mVertsArray->clear();
  mNormsArray->clear();
  mColorArray->clear();
  mDrawElements->indexBuffer()->clear();
  mIndices.clear();
  mVerts.clear();
  mNorms.clear();
  mColors.clear();
  mCubes.clear();
  mEdges.clear();
  mVolumeInfo.clear();
}
//------------------------------------------------------------------------------
void MarchingCubes::run(bool generate_colors)
{
  mVerts.clear();
  mNorms.clear();
  mIndices.clear();
  mVerts.reserve(1024);
  mNorms.reserve(1024);
  mColors.reserve(1024);
  mIndices.reserve(1024);

  /*Time time; time.start();*/

  for(int ivol=0; ivol<mVolumeInfo.size(); ++ivol)
  {
    Volume* vol     = mVolumeInfo.at(ivol)->volume();
    float threshold = mVolumeInfo.at(ivol)->threshold();
    int start       = (int)mVerts.size();

    if (vol->dataIsDirty())
      vol->setupInternalData();

    // note: this function takes the 90% of the time
    computeEdges(vol, threshold);

    // note: this loop takes the remaining 10% of the time
    //// the z->y->x order is important in order to minimize cache misses
    //for(int z = 0; z < mVolume->slices().z()-1; ++z)
    //  for(int y = 0; y < mVolume->slices().y()-1; ++y)
    //    for(int x = 0; x < mVolume->slices().x()-1; ++x)
    //      if(vol->cube(x,y,z).includes(threshold))
    //        processCube(x, y, z, vol, threshold);

    for(unsigned int i=0; i<mCubes.size(); ++i)
      processCube(mCubes[i].x(), mCubes[i].y(), mCubes[i].z(), vol, threshold);

    int count = (int)mVerts.size() - start;
    mVolumeInfo.at(ivol)->setVert0(start);
    mVolumeInfo.at(ivol)->setVertC(count);

    // fill color array
    if (generate_colors)
    {
      mColors.resize( mVerts.size() );
      for(int i=start; i<start+count; ++i)
        mColors[i] = mVolumeInfo.at(ivol)->color();
    }
  }

  mVertsArray->resize(mVerts.size());
  mVertsArray->setBufferObjectDirty();
  if (mVerts.size())
    memcpy(mVertsArray->ptr(), &mVerts[0], sizeof(mVerts[0]) * mVerts.size());

  mNormsArray->resize(mNorms.size());
  mNormsArray->setBufferObjectDirty();
  if (mNorms.size())
    memcpy(mNormsArray->ptr(), &mNorms[0], sizeof(mNorms[0]) * mNorms.size());

  if (generate_colors)
  {
    mColorArray->resize(mColors.size());
    mColorArray->setBufferObjectDirty();
    if (mColors.size())
      memcpy(mColorArray->ptr(), &mColors[0], sizeof(mColors[0]) * mColors.size());
  }
  else
    mColorArray->clear();

  mDrawElements->indexBuffer()->resize(mIndices.size());
  mDrawElements->indexBuffer()->setBufferObjectDirty(true);
  if (mIndices.size())
    memcpy(mDrawElements->indexBuffer()->ptr(), &mIndices[0], sizeof(mIndices[0]) * mIndices.size());

  if (!mHighQualityNormals)
  {
    ref<Geometry> geom = new Geometry;
    geom->setVertexArray(mVertsArray.get());
    geom->drawCalls()->push_back(mDrawElements.get());

    geom->computeNormals();
    mNormsArray->resize( geom->normalArray()->size() );
    mNormsArray->setBufferObjectDirty();
    memcpy(mNormsArray->ptr(), geom->normalArray()->ptr(), sizeof(mNormsArray->at(0)) * mNormsArray->size());
  }
}
//------------------------------------------------------------------------------
void MarchingCubes::updateColor(const fvec3& color, int volume_index)
{
  if(volume_index>=mVolumeInfo.size())
  {
    Log::error( Say("updateColor() volume index (%n) out of range.\n") << volume_index);
    return;
  }
  int start = mVolumeInfo.at(volume_index)->vert0();
  int count = mVolumeInfo.at(volume_index)->vertC();
  if (start+count > (int)mColorArray->size())
  {
    Log::error("updateColor() color array not preset.\n");
    return;
  }
  for(int i=start; i<start+count; ++i)
  {
    mColorArray->at(i).r() = color.r();
    mColorArray->at(i).g() = color.g();
    mColorArray->at(i).b() = color.b();
  }
}
//------------------------------------------------------------------------------
void MarchingCubes::updateColor(const fvec4& color, int volume_index)
{
  if(volume_index>=mVolumeInfo.size())
  {
    Log::error( Say("updateColor() volume index (%n) out of range.\n") << volume_index);
    return;
  }
  int start = mVolumeInfo.at(volume_index)->vert0();
  int count = mVolumeInfo.at(volume_index)->vertC();
  if (start+count > (int)mColorArray->size())
  {
    Log::error("updateColor() color array not preset.\n");
    return;
  }
  for(int i=start; i<start+count; ++i)
    mColorArray->at(i) = color;
}
//------------------------------------------------------------------------------
void MarchingCubes::updateAlpha(float alpha, int volume_index)
{
  if(volume_index>=mVolumeInfo.size())
  {
    Log::error( Say("updateColor() volume index (%n) out of range.\n") << volume_index);
    return;
  }
  int start = mVolumeInfo.at(volume_index)->vert0();
  int count = mVolumeInfo.at(volume_index)->vertC();
  if (start+count > (int)mColorArray->size())
  {
    Log::error("updateColor() color array not preset.\n");
    return;
  }
  for(int i=start; i<start+count; ++i)
    mColorArray->at(i).a() = alpha;
}
//------------------------------------------------------------------------------
// Volume
//------------------------------------------------------------------------------
Volume::Volume()
{
  VL_DEBUG_SET_OBJECT_NAME()
  setup(NULL, false, false, fvec3(0,0,0), fvec3(1.0f,1.0f,1.0f), ivec3(50,50,50));
}
//------------------------------------------------------------------------------
ref<Volume> Volume::downsample() const
{
  ref<Volume> vol = new Volume;
  int w = mSlices.x() / 2;
  int h = mSlices.y() / 2;
  int d = mSlices.z() / 2;
  if (w<1) w = 1;
  if (h<1) h = 1;
  if (d<1) d = 1;

  vol->setup(NULL, false, false, bottomLeft(), topRight(), ivec3(w,h,d));

  for(int z=0; z<d; ++z)
  {
    int z1=z*2;
    int z2=z*2+1;
    for(int y=0; y<h; ++y)
    {
      int y1=y*2;
      int y2=y*2+1;
      for(int x=0; x<w; ++x)
      {
        int x1 = x*2;
        int x2 = x*2+1;
        float v0 = value(x1,y1,z1);
        float v1 = value(x1,y1,z2);
        float v2 = value(x1,y2,z1);
        float v3 = value(x1,y2,z2);
        float v4 = value(x2,y1,z1);
        float v5 = value(x2,y1,z2);
        float v6 = value(x2,y2,z1);
        float v7 = value(x2,y2,z2);
        vol->value(x,y,z) = (v0+v1+v2+v3+v4+v5+v6+v7) * (1.0f/8.0f);
      }
    }
  }

  return vol;
}
//------------------------------------------------------------------------------
void Volume::setupInternalData()
{
  mDataIsDirty = false;
  int w = slices().x() -1;
  int h = slices().y() -1;
  int d = slices().z() -1;
  mCubes.resize(w*h*d);
  for(int z = 0; z < d; ++z)
  {
    for(int y = 0; y < h; ++y)
    {
      for(int x = 0; x < w; ++x)
      {
        float v[] = 
        {
          value(x+0,y+0,z+0),
          value(x+0,y+0,z+1),
          value(x+0,y+1,z+0),
          value(x+0,y+1,z+1),
          value(x+1,y+0,z+0),
          value(x+1,y+0,z+1),
          value(x+1,y+1,z+0),
          value(x+1,y+1,z+1)
        };
        int icube = x+w*y+w*h*z;
        mCubes[icube].mMin = v[0];
        mCubes[icube].mMax = v[0];
        for(int i=1; i<8; ++i)
        {
          if (mCubes[icube].mMin > v[i]) mCubes[icube].mMin = v[i];
          if (mCubes[icube].mMax < v[i]) mCubes[icube].mMax = v[i];
        }
      }
    }
  }
}
//------------------------------------------------------------------------------
void Volume::setup( float* data, bool use_directly, bool copy_data, const fvec3& bottom_left, const fvec3& top_right, const ivec3& slices )
{
  fvec3 size = top_right-bottom_left;
  
  if (use_directly)
  {
    VL_CHECK(data);
    // discard internal data.
    std::vector<float>().swap( mInternalValues );
    mValues = data;
  }
  else
  {
    // allocate internal data & copy
    mInternalValues.resize( slices.x() * slices.y() * slices.z() );
    mValues = &mInternalValues[0];
    if (copy_data)
      memcpy( mValues, data, slices.x() * slices.y() * slices.z() * sizeof(float) );
  }

  mBottomLeft = bottom_left;
  mTopRight   = top_right;
  mSize       = topRight()-bottomLeft();
  mSlices     = slices;
  mCellSize.x() = size.x() / (slices.x()-1);
  mCellSize.y() = size.y() / (slices.y()-1);
  mCellSize.z() = size.z() / (slices.z()-1);

  mMinimum = +1;
  mMaximum = -1;
  mAverage = 0;
  mDataIsDirty = true;
}
//------------------------------------------------------------------------------
void Volume::setup(const Volume& volume)
{
  *this = volume;
  mMinimum = +1;
  mMaximum = -1;
  mAverage = 0;
  mDataIsDirty = true;
}
//------------------------------------------------------------------------------
float Volume::sampleNearest(float x, float y, float z) const
{
  x = (x - mBottomLeft.x()) / mSize.x();
  y = (y - mBottomLeft.y()) / mSize.y();
  z = (z - mBottomLeft.z()) / mSize.z();
  if (x<0 || y<0 || z<0) return 0;
  if (x>1.0001 || y>1.0001 || z>1.0001) return 0;
  if (x > 0.9999f) x = 0.9999f;
  if (y > 0.9999f) y = 0.9999f;
  if (z > 0.9999f) z = 0.9999f;
  float xt = x * (mSlices.x()-1);
  float yt = y * (mSlices.y()-1);
  float zt = z * (mSlices.z()-1);
  int ix = int(xt);
  int iy = int(yt);
  int iz = int(zt);
  return value(ix  , iy,   iz);
}
//------------------------------------------------------------------------------
float Volume::sampleSmooth(float x, float y, float z) const
{
  x = (x - mBottomLeft.x()) / mSize.x();
  y = (y - mBottomLeft.y()) / mSize.y();
  z = (z - mBottomLeft.z()) / mSize.z();
  if (x<0 || y<0 || z<0) return 0;
  if (x>1.0f || y>1.0f || z>1.0f) return 0;
  if (x > 0.9999f) x = 0.9999f;
  if (y > 0.9999f) y = 0.9999f;
  if (z > 0.9999f) z = 0.9999f;
  float xt = x * (mSlices.x()-1);
  float yt = y * (mSlices.y()-1);
  float zt = z * (mSlices.z()-1);
  int ix = int(xt); xt -= ix;
  int iy = int(yt); yt -= iy;
  int iz = int(zt); zt -= iz;
  float val0 = value(ix  , iy,   iz);
  float val1 = value(ix+1, iy,   iz);
  float val2 = value(ix+1, iy+1, iz);
  float val3 = value(ix,   iy+1, iz);
  float val4 = value(ix  , iy,   iz+1);
  float val5 = value(ix+1, iy,   iz+1);
  float val6 = value(ix+1, iy+1, iz+1);
  float val7 = value(ix,   iy+1, iz+1);
  float xt1 = 1-xt;
  float yt1 = 1-yt;
  float zt1 = 1-zt;
  float v1 = val0*(yt1) + val3*yt;
  float v2 = val1*(yt1) + val2*yt;
  float a = v1*(xt1) + v2*xt;
  v1 = val4*(yt1) + val7*yt;
  v2 = val5*(yt1) + val6*yt;
  float b = v1*(xt1) + v2*xt;
  return a*(zt1) + b*zt;
}
//------------------------------------------------------------------------------
void Volume::normalHQ(fvec3 &normal, const fvec3& v, float dx, float dy, float dz)
{
  ////////////////////////////////////////////////////////////////////////////////
  // note: this is the performance killer, it would be nice to optimize it...
  ////////////////////////////////////////////////////////////////////////////////
  normal.x() = sampleSmooth(v.x()-dx, v.y(), v.z()) - sampleSmooth(v.x()+dx, v.y(), v.z());
  normal.y() = sampleSmooth(v.x(), v.y()-dy, v.z()) - sampleSmooth(v.x(), v.y()+dy, v.z());
  normal.z() = sampleSmooth(v.x(), v.y(), v.z()-dz) - sampleSmooth(v.x(), v.y(), v.z()+dz);
  normal.normalize();
}
//------------------------------------------------------------------------------
void Volume::normalLQ(fvec3 &normal, const fvec3& v, float dx, float dy, float dz)
{
  // this function could be optimized even more by sampling the 8 points only once,
  // and computing the x and y gradient form the same slice... but since we don't use it...
  float v0 = sampleSmooth(v.x(), v.y(), v.z());
  normal.x() = v0 - sampleSmooth(v.x()+dx, v.y(), v.z());
  normal.y() = v0 - sampleSmooth(v.x(), v.y()+dy, v.z());
  normal.z() = v0 - sampleSmooth(v.x(), v.y(), v.z()+dz);
  normal.normalize();
}
//------------------------------------------------------------------------------
float Volume::computeMinimum() const
{
  if (!mValues)
    return 0;
  float lowest = mValues[0];
  int val_count = mSlices.x() * mSlices.y() * mSlices.z();
  for(int i=1; i<val_count; ++i)
    if (mValues[i] < lowest)
      lowest = mValues[i];
  return lowest;
}
//------------------------------------------------------------------------------
float Volume::computeMaximum() const
{
  if (!mValues)
    return 0;
  float highest = mValues[0];
  int val_count = mSlices.x() * mSlices.y() * mSlices.z();
  for(int i=1; i<val_count; ++i)
    if (mValues[i] > highest)
      highest = mValues[i];
  return highest;
}
//------------------------------------------------------------------------------
float Volume::computeAverage() const
{
  if (!mValues)
    return 0;
  double average = 0;
  int val_count = mSlices.x() * mSlices.y() * mSlices.z();
  for(int i=0; i<val_count; ++i)
    average += mValues[i];
  average /= (double)val_count;
  return (float)average;
}
//------------------------------------------------------------------------------
const int MarchingCubes::mCubeEdgeFlags[256]=
{
  0x000, 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c, 0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00,
  0x190, 0x099, 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c, 0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90,
  0x230, 0x339, 0x033, 0x13a, 0x636, 0x73f, 0x435, 0x53c, 0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30,
  0x3a0, 0x2a9, 0x1a3, 0x0aa, 0x7a6, 0x6af, 0x5a5, 0x4ac, 0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0,
  0x460, 0x569, 0x663, 0x76a, 0x066, 0x16f, 0x265, 0x36c, 0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60,
  0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0x0ff, 0x3f5, 0x2fc, 0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0,
  0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x055, 0x15c, 0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950,
  0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0x0cc, 0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0,
  0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc, 0x0cc, 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0,
  0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c, 0x15c, 0x055, 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650,
  0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc, 0x2fc, 0x3f5, 0x0ff, 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0,
  0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c, 0x36c, 0x265, 0x16f, 0x066, 0x76a, 0x663, 0x569, 0x460,
  0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac, 0x4ac, 0x5a5, 0x6af, 0x7a6, 0x0aa, 0x1a3, 0x2a9, 0x3a0,
  0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c, 0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x033, 0x339, 0x230,
  0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c, 0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x099, 0x190,
  0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c, 0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x000
};
//------------------------------------------------------------------------------
const int MarchingCubes::mTriangleConnectionTable[256][16] =
{
  {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1},
  {3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1},
  {3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1},
  {3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1},
  {9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1},
  {9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
  {2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1},
  {8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1},
  {9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
  {4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1},
  {3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1},
  {1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1},
  {4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1},
  {4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1},
  {9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
  {5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1},
  {2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1},
  {9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
  {0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
  {2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1},
  {10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1},
  {4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1},
  {5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1},
  {5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1},
  {9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1},
  {0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1},
  {1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1},
  {10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1},
  {8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1},
  {2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1},
  {7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1},
  {9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1},
  {2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1},
  {11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1},
  {9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1},
  {5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1},
  {11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1},
  {11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
  {1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1},
  {9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1},
  {5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1},
  {2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
  {0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
  {5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1},
  {6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1},
  {3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1},
  {6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1},
  {5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1},
  {1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
  {10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1},
  {6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1},
  {8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1},
  {7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1},
  {3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
  {5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1},
  {0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1},
  {9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1},
  {8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1},
  {5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1},
  {0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1},
  {6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1},
  {10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1},
  {10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1},
  {8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1},
  {1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1},
  {3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1},
  {0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1},
  {10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1},
  {3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1},
  {6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1},
  {9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1},
  {8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1},
  {3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1},
  {6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1},
  {0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1},
  {10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1},
  {10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1},
  {2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1},
  {7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1},
  {7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1},
  {2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1},
  {1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1},
  {11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1},
  {8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1},
  {0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1},
  {7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
  {10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
  {2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
  {6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1},
  {7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1},
  {2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1},
  {1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1},
  {10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1},
  {10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1},
  {0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1},
  {7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1},
  {6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1},
  {8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1},
  {9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1},
  {6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1},
  {4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1},
  {10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1},
  {8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1},
  {0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1},
  {1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1},
  {8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1},
  {10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1},
  {4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1},
  {10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
  {5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
  {11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1},
  {9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
  {6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1},
  {7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1},
  {3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1},
  {7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1},
  {9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1},
  {3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1},
  {6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1},
  {9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1},
  {1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1},
  {4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1},
  {7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1},
  {6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1},
  {3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1},
  {0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1},
  {6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1},
  {0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1},
  {11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1},
  {6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1},
  {5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1},
  {9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1},
  {1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1},
  {1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1},
  {10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1},
  {0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1},
  {5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1},
  {10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1},
  {11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1},
  {9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1},
  {7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1},
  {2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1},
  {8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1},
  {9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1},
  {9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1},
  {1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1},
  {9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1},
  {9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1},
  {5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1},
  {0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1},
  {10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1},
  {2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1},
  {0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1},
  {0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1},
  {9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1},
  {5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1},
  {3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1},
  {5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1},
  {8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1},
  {0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1},
  {9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1},
  {0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1},
  {1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1},
  {3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1},
  {4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1},
  {9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1},
  {11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1},
  {11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1}, 
  {2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1},
  {9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1},
  {3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1},
  {1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1},
  {4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1},
  {4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1},
  {0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1},
  {3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1},
  {3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1},
  {0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1},
  {9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1},
  {1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
  {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
};
//------------------------------------------------------------------------------
