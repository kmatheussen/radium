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

#ifndef DepthSortCallback_INCLUDE_ONCE
#define DepthSortCallback_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/Camera.hpp>

namespace vl
{
  /**
   * DepthSortCallback sorts the primitives of the Geometry bound to the Actor in which the callback is installed.
   * 
   * In order to work properly DepthSortCallback requires the following:
   * - The Actor must be bound to a Geometry or subclass.
   * - Sorts only draw calls of type DrawElementsUInt/UShort/UByte.
   * - Sorts only draw calls with primitive type PT_POINTS, PT_LINES, PT_TRIANGLES, PT_QUADS.
   * - The draw calls should not use primitive restart.
   *
   * Despite the fact that the condition list seems to be quite limiting it actually covers the most common usage cases.
   * Furthermore the use of DrawElements* and the primitive types PT_POINTS, PT_LINES, PT_TRIANGLES, PT_QUADS grants
   * the maximum flexibility.
   *
   * \note
   *
   * - This callback works well with different LODs.
   * - This callback works well with multipassing, the sorting is done only once.
   * - Using DrawElementsUShort or DrawElementsUByte might result in a quicker sorting compared to DrawElementsUInt.
   *   Is therefore advisable to use them whenever possible.
   *
   *
   * \remarks
   *
   * - The sorting is based on the position of the vertices as specified by Geometry::vertexArray() or 
   *   vertexAttribArray(vl::VA_Position) and for obvious
   *   reasons cannot take into consideration transformations made in the vertex shader or in the geometry shader.
   * - The sorting is performed on a per DrawCall basis. For example, if a Geometry has 2 DrawCall A and B bound to it, 
   *   then the polygons, lines or points of A will always be rendered before the ones specified by B. If you need the
   *   two sets of polygons to be correctly sorted with respect to one another you will need to merge them in one single
   *   draw call.
   *
   * \sa \ref pagGuidePolygonDepthSorting
   */
  class DepthSortCallback: public ActorEventCallback
  {
    VL_INSTRUMENT_CLASS(vl::DepthSortCallback, ActorEventCallback)

    template<typename T>
    class Point
    {
    public:
      T A;
    };
    template<typename T>
    class Line
    {
    public:
      T A,B;
    };
    template<typename T>
    class Triangle
    {
    public:
      T A,B,C;
    };
    template<typename T>
    class Quad
    {
    public:
      T A,B,C,D;
    };
    typedef Point<unsigned int>      PointUInt;
    typedef Line<unsigned int>       LineUInt;
    typedef Triangle<unsigned int>   TriangleUInt;
    typedef Quad<unsigned int>       QuadUInt;

    typedef Point<unsigned short>    PointUShort;
    typedef Line<unsigned short>     LineUShort;
    typedef Triangle<unsigned short> TriangleUShort;
    typedef Quad<unsigned short>     QuadUShort;

    typedef Point<unsigned char>     PointUByte;
    typedef Line<unsigned char>      LineUByte;
    typedef Triangle<unsigned char>  TriangleUByte;
    typedef Quad<unsigned char>      QuadUByte;

    class PrimitiveZ
    {
    public:
      PrimitiveZ(int tri=0, float z=0.0f): mPrimitiveIndex(tri), mZ(z) {}
      unsigned int mPrimitiveIndex;
      float mZ;
    };
    class Sorter_Back_To_Front
    {
    public:
      bool operator()(const PrimitiveZ& t1, const PrimitiveZ& t2) const { return t1.mZ < t2.mZ; }
    };
    class Sorter_Front_To_Back
    {
    public:
      bool operator()(const PrimitiveZ& t1, const PrimitiveZ& t2) const { return t1.mZ > t2.mZ; }
    };

  public:
    //! Constructor.
    DepthSortCallback()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      setSortMode(SM_SortBackToFront);
    }

    void onActorDelete(Actor*) {}

    //! Performs the actual sorting
    virtual void onActorRenderStarted(Actor* actor, real /*frame_clock*/, const Camera* cam, Renderable* renderable, const Shader*, int pass)
    {
      // need to sort only for the first pass
      if (pass > 0)
        return;

      vl::mat4 matrix = cam->viewMatrix();
      if (actor && actor->transform())
        matrix *= actor->transform()->worldMatrix();

      if (matrix == mCacheMatrix)
        return;
      else
        mCacheMatrix = matrix;

      // this works well with LOD
      Geometry* geometry = renderable->as<Geometry>();

      if (!geometry)
        return;

      const ArrayAbstract* verts = geometry->vertexArray() ? geometry->vertexArray() : geometry->vertexAttribArray(vl::VA_Position) ? geometry->vertexAttribArray(vl::VA_Position)->data() : NULL;

      if (!verts)
        return;

      // computes eye-space vertex positions
      mat4 m;
      if (actor->transform())
        m = cam->viewMatrix() * actor->transform()->worldMatrix();
      else
        m = cam->viewMatrix();
      mEyeSpaceVerts.resize( verts->size() );
      // would be nice to optimize this with SEE2
      for(size_t i=0; i<verts->size(); ++i)
        mEyeSpaceVerts[i] = m * verts->getAsVec3(i);

      geometry->setBufferObjectDirty(true);
      geometry->setDisplayListDirty(true);

      for(int idraw=0; idraw<geometry->drawCalls()->size(); ++idraw)
      {
        DrawCall* dc = geometry->drawCalls()->at(idraw);
        if (dc->classType() == DrawElementsUInt::Type())
          sort<unsigned int, DrawElementsUInt>(dc->as<DrawElementsUInt>(), mSortedPointsUInt, mSortedLinesUInt, mSortedTrianglesUInt, mSortedQuadsUInt);
        else
        if (dc->classType() == DrawElementsUShort::Type())
          sort<unsigned short, DrawElementsUShort>(dc->as<DrawElementsUShort>(), mSortedPointsUShort, mSortedLinesUShort, mSortedTrianglesUShort, mSortedQuadsUShort);
        else
        if (dc->classType() == DrawElementsUByte::Type())
          sort<unsigned char, DrawElementsUByte>(dc->as<DrawElementsUByte>(), mSortedPointsUByte, mSortedLinesUByte, mSortedTrianglesUByte, mSortedQuadsUByte);
      }
    }

    template<typename T, typename deT>
    void sort(deT* polys, std::vector<Point<T> >& sorted_points, std::vector<Line<T> >& sorted_lines, std::vector<Triangle<T> >& sorted_triangles, std::vector<Quad<T> >& sorted_quads)
    {
      if (polys->primitiveType() == PT_QUADS)
      {
        // compute zetas
        mPrimitiveZ.resize( polys->indexBuffer()->size() / 4 );
        if (mPrimitiveZ.empty())
          return;

        const typename deT::index_type* it  = polys->indexBuffer()->begin();
        const typename deT::index_type* end = polys->indexBuffer()->end();
        for(unsigned iz=0; it != end; it+=4, ++iz)
        {
          mPrimitiveZ[iz].mZ = (float)(mEyeSpaceVerts[it[0]].z() + mEyeSpaceVerts[it[1]].z() + mEyeSpaceVerts[it[2]].z() + mEyeSpaceVerts[it[3]].z());
          mPrimitiveZ[iz].mPrimitiveIndex = iz;
        }

        // sort based on mPrimitiveZ
        if (sortMode() == SM_SortBackToFront)
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Back_To_Front() );
        else
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Front_To_Back() );

        // regenerate the sorted indices
        sorted_quads.resize( polys->indexBuffer()->size() / 4 );
        Quad<T>* tris = (Quad<T>*)polys->indexBuffer()->ptr();
        for(unsigned int i=0; i<mPrimitiveZ.size(); ++i)
          sorted_quads[i] = tris[ mPrimitiveZ[i].mPrimitiveIndex ];
        memcpy(&tris[0], &sorted_quads[0], sizeof(sorted_quads[0])*sorted_quads.size() );
      }
      else
      if (polys->primitiveType() == PT_TRIANGLES)
      {
        // compute zetas
        mPrimitiveZ.resize( polys->indexBuffer()->size() / 3 );
        if (mPrimitiveZ.empty())
          return;

        const typename deT::index_type* it  = polys->indexBuffer()->begin();
        const typename deT::index_type* end = polys->indexBuffer()->end();
        for(unsigned iz=0; it != end; it+=3, ++iz)
        {
          mPrimitiveZ[iz].mZ = (float)(mEyeSpaceVerts[it[0]].z() + mEyeSpaceVerts[it[1]].z() + mEyeSpaceVerts[it[2]].z());
          mPrimitiveZ[iz].mPrimitiveIndex = iz;
        }

        // sort based on mPrimitiveZ
        if (sortMode() == SM_SortBackToFront)
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Back_To_Front() );
        else
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Front_To_Back() );

        // regenerate the sorted indices
        sorted_triangles.resize( polys->indexBuffer()->size() / 3 );
        Triangle<T>* tris = (Triangle<T>*)polys->indexBuffer()->ptr();
        for(unsigned int i=0; i<mPrimitiveZ.size(); ++i)
          sorted_triangles[i] = tris[ mPrimitiveZ[i].mPrimitiveIndex ];
        memcpy(&tris[0], &sorted_triangles[0], sizeof(sorted_triangles[0])*sorted_triangles.size() );
      }
      else
      if (polys->primitiveType() == PT_LINES)
      {
        // compute zetas
        mPrimitiveZ.resize( polys->indexBuffer()->size() / 2 );
        if (mPrimitiveZ.empty())
          return;

        const typename deT::index_type* it  = polys->indexBuffer()->begin();
        const typename deT::index_type* end = polys->indexBuffer()->end();
        for(unsigned iz=0; it != end; it+=2, ++iz)
        {
          mPrimitiveZ[iz].mZ = (float)(mEyeSpaceVerts[it[0]].z() + mEyeSpaceVerts[it[1]].z());
          mPrimitiveZ[iz].mPrimitiveIndex = iz;
        }

        // sort based on mPrimitiveZ
        if (sortMode() == SM_SortBackToFront)
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Back_To_Front() );
        else
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Front_To_Back() );

        // regenerate the sorted indices
        sorted_lines.resize( polys->indexBuffer()->size() / 2 );
        Line<T>* tris = (Line<T>*)polys->indexBuffer()->ptr();
        for(unsigned int i=0; i<mPrimitiveZ.size(); ++i)
          sorted_lines[i] = tris[ mPrimitiveZ[i].mPrimitiveIndex ];
        memcpy(&tris[0], &sorted_lines[0], sizeof(sorted_lines[0])*sorted_lines.size() );
      }
      else
      if (polys->primitiveType() == PT_POINTS)
      {
        // compute zetas
        mPrimitiveZ.resize( polys->indexBuffer()->size() );
        if (mPrimitiveZ.empty())
          return;

        const typename deT::index_type* it  = polys->indexBuffer()->begin();
        const typename deT::index_type* end = polys->indexBuffer()->end();
        for(unsigned iz=0; it != end; ++it, ++iz)
        {
          mPrimitiveZ[iz].mZ = (float)mEyeSpaceVerts[it[0]].z();
          mPrimitiveZ[iz].mPrimitiveIndex = iz;
        }

        // sort based on mPrimitiveZ
        if (sortMode() == SM_SortBackToFront)
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Back_To_Front() );
        else
          std::sort( mPrimitiveZ.begin(), mPrimitiveZ.end(), Sorter_Front_To_Back() );

        // regenerate the sorted indices
        sorted_points.resize( polys->indexBuffer()->size() );
        Point<T>* tris = (Point<T>*)polys->indexBuffer()->ptr();
        for(unsigned int i=0; i<mPrimitiveZ.size(); ++i)
          sorted_points[i] = tris[ mPrimitiveZ[i].mPrimitiveIndex ];
        memcpy(&tris[0], &sorted_points[0], sizeof(sorted_points[0])*sorted_points.size() );
      }

      if (Has_BufferObject)
      {
        if (polys->indexBuffer()->bufferObject()->handle())
        {
          if (polys->indexBuffer()->bufferObject()->usage() != vl::BU_DYNAMIC_DRAW)
          {
            polys->indexBuffer()->bufferObject()->setBufferData(vl::BU_DYNAMIC_DRAW);
            polys->indexBuffer()->setBufferObjectDirty(false);
          }
          else
            polys->indexBuffer()->setBufferObjectDirty(true);
        }
      }
    }

    ESortMode sortMode() const { return mSortMode; }
    void setSortMode(ESortMode sort_mode) { mSortMode = sort_mode; }

    /**
     * Forces sorting at the next rendering.
     */
    void invalidateCache() { mCacheMatrix = vl::mat4(); }

  protected:
    std::vector<vec3> mEyeSpaceVerts;
    std::vector<PrimitiveZ> mPrimitiveZ;

    std::vector<PointUInt> mSortedPointsUInt;
    std::vector<LineUInt> mSortedLinesUInt;
    std::vector<TriangleUInt> mSortedTrianglesUInt;
    std::vector<QuadUInt> mSortedQuadsUInt;

    std::vector<PointUShort> mSortedPointsUShort;
    std::vector<LineUShort> mSortedLinesUShort;
    std::vector<TriangleUShort> mSortedTrianglesUShort;
    std::vector<QuadUShort> mSortedQuadsUShort;

    std::vector<PointUByte> mSortedPointsUByte;
    std::vector<LineUByte> mSortedLinesUByte;
    std::vector<TriangleUByte> mSortedTrianglesUByte;
    std::vector<QuadUByte> mSortedQuadsUByte;

    vl::mat4 mCacheMatrix;

    ESortMode mSortMode;
  };
}

#endif
