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

#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlCore/Time.hpp>

using namespace vl;

namespace
{
  class LessCompare
  {
  public:
    LessCompare(const Geometry* geom)
    {
      if (geom->vertexArray())
        mAttribs.push_back(geom->vertexArray());
      if (geom->normalArray())
        mAttribs.push_back(geom->normalArray());
      if (geom->colorArray())
        mAttribs.push_back(geom->colorArray());
      if (geom->secondaryColorArray())
        mAttribs.push_back(geom->secondaryColorArray());
      if (geom->fogCoordArray())
        mAttribs.push_back(geom->fogCoordArray());
      for(int i=0; i<VL_MAX_TEXTURE_UNITS; ++i)
        if (geom->texCoordArray(i))
          mAttribs.push_back(geom->texCoordArray(i));
      for(int i=0; i<geom->vertexAttribArrays()->size(); ++i)
        mAttribs.push_back(geom->vertexAttribArrays()->at(i)->data());
    }

    bool operator()(u32 a, u32 b) const 
    { 
      for(unsigned i=0; i<mAttribs.size(); ++i)
      {
        int val = mAttribs[i]->compare(a,b);
        if (val != 0)
          return val < 0;
      }
      return false;
    }

  protected:
    std::vector< const ArrayAbstract* > mAttribs;
  };

  class EqualsCompare
  {
  public:
    EqualsCompare(const Geometry* geom)
    {
      if (geom->vertexArray())
        mAttribs.push_back(geom->vertexArray());
      if (geom->normalArray())
        mAttribs.push_back(geom->normalArray());
      if (geom->colorArray())
        mAttribs.push_back(geom->colorArray());
      if (geom->secondaryColorArray())
        mAttribs.push_back(geom->secondaryColorArray());
      if (geom->fogCoordArray())
        mAttribs.push_back(geom->fogCoordArray());
      for(int i=0; i<VL_MAX_TEXTURE_UNITS; ++i)
        if (geom->texCoordArray(i))
          mAttribs.push_back(geom->texCoordArray(i));
      for(int i=0; i<geom->vertexAttribArrays()->size(); ++i)
        mAttribs.push_back(geom->vertexAttribArrays()->at(i)->data());
    }

    bool operator()(u32 a, u32 b) const 
    { 
      for(unsigned i=0; i<mAttribs.size(); ++i)
      {
        if (mAttribs[i]->compare(a,b) != 0)
          return false;
      }
      return true;
    }

  protected:
    std::vector< const ArrayAbstract* > mAttribs;
  };
}

//-----------------------------------------------------------------------------
template<class T>
ref<ArrayAbstract> VertexMapper::regenerateT(ArrayAbstract* data, const std::vector<u32>& map_new_to_old) const
{
  ref<T> in_data = cast<T>(data);
  if (in_data)
  {
    ref<T> out_data = new T;
    out_data->resize(map_new_to_old.size());
    for(unsigned i=0; i<map_new_to_old.size(); ++i)
        out_data->at(i) = in_data->at(map_new_to_old[i]);
    return out_data;
  }
  return NULL;
}
//-----------------------------------------------------------------------------
ref<ArrayAbstract> VertexMapper::regenerate(ArrayAbstract* data, const std::vector<u32>& map_new_to_old) const
{
  ref<ArrayAbstract> out_data;

  if ( (out_data = regenerateT<ArrayInt4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayInt3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayInt2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUInt4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUInt3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUInt2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayFloat4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayFloat3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayFloat2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayDouble4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayDouble3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayDouble2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayFloat1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayDouble1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUInt1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayInt1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayByte1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayShort1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUByte1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUShort1>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUByte2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUByte3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUByte4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayByte2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayByte3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayByte4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayShort2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayShort3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayShort4>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUShort2>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUShort3>(data, map_new_to_old)) )
    return out_data;
  else
  if ( (out_data = regenerateT<ArrayUShort4>(data, map_new_to_old)) )
    return out_data;

  return NULL;
}
//-----------------------------------------------------------------------------
void DoubleVertexRemover::removeDoubles(Geometry* geom)
{
  Time timer;
  timer.start();

  mMapNewToOld.clear();
  mMapOldToNew.clear();

  u32 vert_count = (u32)(geom->vertexArray() ? geom->vertexArray()->size() : geom->vertexAttribArray(VA_Position) ? geom->vertexAttribArray(VA_Position)->data()->size() : 0);
  
  VL_CHECK(vert_count);
  if (!vert_count)
    return;

  std::vector<u32> verti;
  verti.resize(vert_count);
  mMapOldToNew.resize(vert_count);

  for(u32 i=0; i<verti.size(); ++i)
  {
    verti[i] = i;
    mMapOldToNew[i] = 0xFFFFFFFF;
  }

  std::sort(verti.begin(), verti.end(), LessCompare(geom));
  EqualsCompare equal_vertex(geom);
  mMapNewToOld.reserve(vert_count);
  u32 unique_vert_idx = 0;
  for(unsigned i=1; i<verti.size(); ++i)
  {
    if ( !equal_vertex(verti[unique_vert_idx], verti[i]) )
    {
      for(unsigned j=unique_vert_idx; j<i; ++j)
        mMapOldToNew[verti[j]] = (u32)mMapNewToOld.size();
      mMapNewToOld.push_back(verti[unique_vert_idx]);
      unique_vert_idx = i;
    }
  }
  for(unsigned j=unique_vert_idx; j<verti.size(); ++j)
  {
    mMapOldToNew[verti[j]] = (u32)mMapNewToOld.size();
    mMapNewToOld.push_back(verti[unique_vert_idx]);
  }

  // regenerate vertices

  geom->regenerateVertices(mMapNewToOld);

  // regenerate DrawCall

  std::vector< ref<DrawCall> > draw_cmd;
  for(int idraw=0; idraw<geom->drawCalls()->size(); ++idraw)
    draw_cmd.push_back( geom->drawCalls()->at(idraw) );
  geom->drawCalls()->clear();

  for(u32 idraw=0; idraw<draw_cmd.size(); ++idraw)
  {
    ref<DrawElementsUInt> de = new DrawElementsUInt( draw_cmd[idraw]->primitiveType() );
    geom->drawCalls()->push_back(de.get());
    const u32 idx_count = draw_cmd[idraw]->countIndices();
    de->indexBuffer()->resize(idx_count);
    u32 i=0;
    for(IndexIterator it = draw_cmd[idraw]->indexIterator(); it.hasNext(); it.next(), ++i)
      de->indexBuffer()->at(i) = mMapOldToNew[it.index()];
  }

  Log::debug( Say("DoubleVertexRemover : time=%.2ns, verts=%n/%n, saved=%n, ratio=%.2n\n") << timer.elapsed() << mMapNewToOld.size() << verti.size() << verti.size() - mMapNewToOld.size() << (float)mMapNewToOld.size()/verti.size() );
}
//-----------------------------------------------------------------------------
