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

#ifndef PolygonSimplifier_INCLUDE_ONCE
#define PolygonSimplifier_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Vector3.hpp>
#include <vlCore/glsl_math.hpp>
#include <vlGraphics/link_config.hpp>
#include <vector>
#include <algorithm>

namespace vl
{
  class Geometry;
//-----------------------------------------------------------------------------
// PolygonSimplifier
//-----------------------------------------------------------------------------
  /**
   * The PolygonSimplifier class reduces the amount of polygons present in a Geometry using a quadric error metric.
   * The algorithm simplifies only the position array of the Geometry all the other vertex attributes will be discarded.
  */
  class VLGRAPHICS_EXPORT PolygonSimplifier: public Object
  {
    VL_INSTRUMENT_CLASS(vl::PolygonSimplifier, Object)

  public:
    class Vertex;
    //-----------------------------------------------------------------------------
    // QErr
    //-----------------------------------------------------------------------------
    //! The quadric error metric as defined by PolygonSimplifier.
    class QErr
    {
    public:
      QErr()
      {
        a2 = 0.0;
        ab = 0.0;
        ac = 0.0;
        ad = 0.0;
        b2 = 0.0;
        bc = 0.0;
        bd = 0.0;
        c2 = 0.0;
        cd = 0.0;
        d2 = 0.0;
      }

      QErr(const dvec3& n, double d, double w = 1.0)
      {
        a2 = w * n.x() * n.x();
        ab = w * n.x() * n.y();
        ac = w * n.x() * n.z();
        ad = w * n.x() * d;
        b2 = w * n.y() * n.y();
        bc = w * n.y() * n.z();
        bd = w * n.y() * d;
        c2 = w * n.z() * n.z();
        cd = w * n.z() * d;
        d2 = w * d*d;

        // indeterminate cases
        VL_CHECK( d2 == d2 )
      }

      dmat3 matrix() const
      {
        return dmat3(
          a2, ab, ac,
          ab, b2, bc,
          ac, bc, c2 );
      }

      dvec3 vector() const
      {
        return dvec3( ad, bd, cd );
      }

      double offset() const
      {
        return d2;
      }

      double evaluate(const dvec3& v) const
      {
        return v.x()*v.x()*a2 + 2*v.x()*v.y()*ab + 2*v.x()*v.z()*ac + 2*v.x()*ad
                              + v.y()*v.y()*b2   + 2*v.y()*v.z()*bc + 2*v.y()*bd
                                                 + v.z()*v.z()*c2   + 2*v.z()*cd
                                                                    + d2;
      }

      bool analyticSolution(dvec3& v) const
      {
#if 0
        dmat3 Ainv;
        double det = matrix().getInverse(Ainv); 
        if (!det)
          return false;
        v = -(Ainv*vector());
        return true;
#else
        double A = c2*b2-bc*bc;
        double B = bc*ac-c2*ab;
        double C = bc*ab-b2*ac;
        double det = a2*(A)+ab*(B)+ac*(C);
        if (fabs(det) < 0.0000001)
          return false;
        else
        {
          double inv_det = 1.0 / det;
          dmat3 Ainv( A*inv_det,             B*inv_det,             C*inv_det, 
                      (ac*bc-c2*ab)*inv_det, (c2*a2-ac*ac)*inv_det, (ab*ac-bc*a2)*inv_det,
                      (bc*ab-ac*b2)*inv_det, (ac*ab-bc*a2)*inv_det, (b2*a2-ab*ab)*inv_det );
          v = Ainv * dvec3( -ad, -bd, -cd );
          return true;
        }
#endif
      }

      QErr operator+(const QErr& other)
      {
        QErr q = *this;
        q.a2 += other.a2;
        q.ab += other.ab;
        q.ac += other.ac;
        q.ad += other.ad;
        q.b2 += other.b2;
        q.bc += other.bc;
        q.bd += other.bd;
        q.c2 += other.c2;
        q.cd += other.cd;
        q.d2 += other.d2;
        return q;
      }

      const QErr& operator+=(const QErr& other)
      {
        a2 += other.a2;
        ab += other.ab;
        ac += other.ac;
        ad += other.ad;
        b2 += other.b2;
        bc += other.bc;
        bd += other.bd;
        c2 += other.c2;
        cd += other.cd;
        d2 += other.d2;
        return *this;
      }

    protected:
      // coefficients
      double a2, ab, ac, ad;
      double     b2, bc, bd;
      double         c2, cd;
      double             d2;
    };
    //-----------------------------------------------------------------------------
    // Triangle
    //-----------------------------------------------------------------------------
    //! A Triangle as defined by PolygonSimplifier.
    class Triangle
    {
      friend class PolygonSimplifier;
      friend class Vertex;
    public:
      Triangle(): mRemoved(false)
      {
        mVertices[0] = NULL;
        mVertices[1] = NULL;
        mVertices[2] = NULL;
      }

      inline void replaceVertex( Vertex* oldv, Vertex* newv );
      inline void computeNormal();
      inline float computeArea() const;
      inline float computePotentialArea(const Vertex* oldv, const Vertex* newv) const;
      inline fvec3 computePotentialNormal(const Vertex* oldv, const Vertex* newv) const;
      inline bool hasVertex(const Vertex*v) const;
      inline bool checkTriangle() const;
      // inline float computeDistance(const fvec3&) const;
      inline QErr computeQErr() const;

      //! vertices of the triangle
      const Vertex* vertex(int index) const { return mVertices[index]; }
      Vertex* vertex(int index) { return mVertices[index]; }
      //! normal of the triangle
      const fvec3& normal() const { return mNormal; }
      //! ara of the triangle
      // float area() const { return mArea; }
      //! has this triangle been removed?
      bool removed() const { return mRemoved; }
      //! generates the QErr

    protected:
      //! vertices of the triangle
      Vertex* mVertices[3];
      //! normal of the triangle
      fvec3 mNormal;
      //! ara of the triangle
      // float mArea;
      //! has this triangle been removed?
      bool mRemoved;
    };
    //-----------------------------------------------------------------------------
    // Vertex
    //-----------------------------------------------------------------------------
    //! A Vertex as defined by PolygonSimplifier.
    class Vertex
    {
      friend class Triangle;
      friend class PolygonSimplifier;
    public:
      Vertex(): mCollapseCost(0.0f), mOriginalIndex(-1) , mSimplifiedIndex(-1), mRemoveOrder(-1),
                mRemoved(false), mProtected(false), mAlreadyProcessed(false)
      {
      }

      inline void addAdjacentVertex(Vertex* v);
      inline void removeAdjacentVertex(Vertex* v);
      inline void computeAdjacentVertices();
      inline bool checkConnectivity();
      inline bool isAdjacentVertex(Vertex*) const;
      inline bool isIncidentTriangle(Triangle*) const;
      inline void discardRemovedTriangles();
      inline void removeIncidentTriangle(const Triangle*);
      inline bool checkTriangles() const;
      inline void computeEdgePenalty();

      //! the position
      const fvec3& position() const { return mPosition; }
      //! ajacent vertices
      int adjacentVerticesCount() const { return (int)mAdjacentVerts.size(); }
      Vertex* adjacentVertex(int index) const { return mAdjacentVerts[index]; }
      //! adjacent triangles
      int incidentTrianglesCount() const { return (int)mIncidentTriangles.size(); }
      Triangle* incidentTriangle(int index) const { return mIncidentTriangles[index]; }
      //! vertex to which collapse
      Vertex* collapseVertex() const { return mCollapseVertex; }
      //! cost of the collapse
      float collapseCost() const { return mCollapseCost; }
      //! collapse position
      const fvec3& collapsePosition() const { return mCollapsePosition; }
      void setCollapsePosition(const fvec3& pos) { mCollapsePosition = pos; }
      //! when the vertex has collapsed
      int removeOrder() const { return mRemoveOrder; }
      //! has the vertex been removed
      bool removed() const { return mRemoved; }
      //! is the vertex protected?
      bool isProtected() const { return mProtected; }
      //! original index of this vertex
      int originalIndex() const { return mOriginalIndex; }
      //! Internally used to regenerated the index buffer
      int simplifiedIndex() const { return mSimplifiedIndex; }
      //! Internally used
      bool alreadyProcessed() const { return mAlreadyProcessed; }
      //! Accumulated vertex error
      const QErr& qerr() const { return mQErr; }
      void setQErr(const QErr& qerr) { mQErr = qerr; }
      void addQErr(const QErr& qerr) { mQErr += qerr; }

    protected:
      QErr mQErr;
      //! the position
      fvec3 mPosition;
      //! ajacent vertices
      std::vector< Vertex* > mAdjacentVerts;
      //! adjacent triangles
      std::vector< Triangle* > mIncidentTriangles;
      //! vertex to which collapse
      Vertex* mCollapseVertex;
      //! cost of the collapse
      float mCollapseCost;
      //! the collapse position
      fvec3 mCollapsePosition;
      //! original index of this vertex
      int mOriginalIndex;
      //! only used during index buffer regeneration
      int mSimplifiedIndex;
      //! when the vertex has collapsed
      int mRemoveOrder;
      //! has the vertex been removed
      bool mRemoved;
      //! is the vertex protected?
      bool mProtected;
      //! internally used
      bool mAlreadyProcessed;
    };

  public:
    PolygonSimplifier(): mRemoveDoubles(false), mVerbose(true), mQuick(true) {}

    void simplify();
    void simplify(const std::vector<fvec3>& in_verts, const std::vector<int>& in_tris);

    void setIntput(Geometry* geom) { mInput = geom; }
    Geometry* input() { return mInput.get(); }
    const Geometry* input() const { return mInput.get(); }

    std::vector< u32 >& targets() { return mTargets; }
    const std::vector< u32 >& targets() const { return mTargets; }

    std::vector< ref<Geometry> >& output() { return mOutput; }
    const std::vector< ref<Geometry> >& output() const { return mOutput; }

    void setProtectedVertices(const std::vector<int>& protected_verts) { mProtectedVerts = protected_verts; }

    int simplifiedVerticesCount() const { return (int)mSimplifiedVertices.size(); }
    Vertex* simplifiedVertices(int index) const { return mSimplifiedVertices[index]; }

    int simplifiedTrianglesCount() const { return (int)mSimplifiedTriangles.size(); }
    Triangle* simplifiedTriangles(int index) const { return mSimplifiedTriangles[index]; }

    void clearTrianglesAndVertices();

    bool removeDoubles() const { return mRemoveDoubles; }
    void setRemoveDoubles(bool remove_doubles) { mRemoveDoubles = remove_doubles; }

    bool verbose() const { return mVerbose; }
    void setVerbose(bool verbose) { mVerbose = verbose; }

    bool quick() const { return mQuick; }
    void setQuick(bool quick) { mQuick = quick; }

  protected:
    void outputSimplifiedGeometry();
    inline void collapse(Vertex* v);
    inline void computeCollapseInfo(Vertex* v);

  protected:
    ref<Geometry> mInput;
    std::vector< ref<Geometry> > mOutput;
    std::vector< u32 > mTargets;
    std::vector<Vertex*> mSimplifiedVertices;
    std::vector<Triangle*> mSimplifiedTriangles;
    std::vector<int> mProtectedVerts;
    bool mRemoveDoubles;
    bool mVerbose;
    bool mQuick;

  private:
    std::vector<Triangle> mTriangleLump;
    std::vector<Vertex> mVertexLump;
  };
  //-----------------------------------------------------------------------------
  // Vertex
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Vertex::addAdjacentVertex(Vertex* v)
  {
    if( v != this )
    {
      for(int i=0; i<adjacentVerticesCount(); ++i)
        if (mAdjacentVerts[i] == v)
          return;
      mAdjacentVerts.push_back(v);
    }
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Vertex::removeAdjacentVertex(Vertex* v)
  {
    VL_CHECK( v != this )
    VL_CHECK( std::find(mAdjacentVerts.begin(), mAdjacentVerts.end(), v) != mAdjacentVerts.end() )
    for(int i=0; i<adjacentVerticesCount(); ++i)
      if (mAdjacentVerts[i] == v)
      {
        mAdjacentVerts.erase(mAdjacentVerts.begin() + i);
        return;
      }
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Vertex::computeAdjacentVertices()
  {
    mAdjacentVerts.clear();
    for(int itri=0; itri<incidentTrianglesCount(); ++itri)
    {
      VL_CHECK(!mIncidentTriangles[itri]->mRemoved)
      addAdjacentVertex( mIncidentTriangles[itri]->mVertices[0] );
      addAdjacentVertex( mIncidentTriangles[itri]->mVertices[1] );
      addAdjacentVertex( mIncidentTriangles[itri]->mVertices[2] );
    }
    mRemoved = mAdjacentVerts.empty();
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Vertex::checkTriangles() const
  {
    for(int itri=incidentTrianglesCount(); itri--; )
      if ( !incidentTriangle(itri)->checkTriangle() )
        return false;
    return true;
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Vertex::computeEdgePenalty()
  {
    for(int ivert=0; ivert<adjacentVerticesCount(); ++ivert)
    {
      int edge_count = 0;
      int border_tri = -1;
      for(int itri=0; itri<incidentTrianglesCount() && edge_count<=1; ++itri)
      {
        if ( incidentTriangle(itri)->hasVertex( adjacentVertex(ivert) ) )
        {
          border_tri = itri;
          ++edge_count;
        }
      }
      if ( edge_count == 1 )
      {
        fvec3 edge = position() - adjacentVertex(ivert)->position();
        dvec3 n = (dvec3)cross(incidentTriangle(border_tri)->normal(), edge );
        n.normalize();
        double d = -dot(n,(dvec3)position());
        mQErr += QErr( n, d, dot(edge, edge) * 1.0 );
      }
    }
  }
  inline void PolygonSimplifier::Vertex::removeIncidentTriangle(const Triangle* tri)
  {
    for(int itri=incidentTrianglesCount(); itri--; )
    {
      if (mIncidentTriangles[itri] == tri)
      {
        mIncidentTriangles.erase( mIncidentTriangles.begin() + itri );
        break;
      }
    }
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Vertex::discardRemovedTriangles()
  {
    for(int itri=incidentTrianglesCount(); itri--; )
    {
      if (mIncidentTriangles[itri]->mRemoved)
        mIncidentTriangles.erase( mIncidentTriangles.begin() + itri );
    }
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Vertex::isAdjacentVertex(Vertex* v) const
  {
    for(int i=0; i<adjacentVerticesCount(); ++i)
      if ( adjacentVertex(i) == v )
        return true;
    return false;
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Vertex::isIncidentTriangle(Triangle* t) const
  {
    for(int i=0; i<incidentTrianglesCount(); ++i)
      if ( incidentTriangle(i) == t )
        return true;
    return false;
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Vertex::checkConnectivity()
  {
    VL_CHECK( mCollapseVertex )
    VL_CHECK( !mCollapseVertex->removed() )
    // check connectivity consistency
    for(int ivert=0; ivert<adjacentVerticesCount(); ++ivert)
    {
      Vertex* adj = mAdjacentVerts[ivert];
      if ( adj->removed() )
        return false;
      if( std::find(adj->mAdjacentVerts.begin(), adj->mAdjacentVerts.end(), this) == adj->mAdjacentVerts.end() )
        return false;
    }
    return true;
  }
  //-----------------------------------------------------------------------------
  // Triangle
  //-----------------------------------------------------------------------------
  inline PolygonSimplifier::QErr PolygonSimplifier::Triangle::computeQErr() const
  {
    dvec3 n = (dvec3)normal();
    double d = -dot((dvec3)vertex(0)->position(), n);
    QErr qerr(n, d, computeArea() * (1.0 / 3.0) );
    return qerr;
  }
  //-----------------------------------------------------------------------------
  inline fvec3 PolygonSimplifier::Triangle::computePotentialNormal(const Vertex* oldv, const Vertex* newv) const
  {
    fvec3 a = (mVertices[0]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[0]->mPosition);
    fvec3 b = (mVertices[1]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[1]->mPosition) - a;
    fvec3 c = (mVertices[2]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[2]->mPosition) - a;

    fvec3 n = cross(b,c);
    n.normalize();
    return n;
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Triangle::checkTriangle() const
  {
    bool ok = true;
    ok &= !mVertices[0]->removed(); VL_CHECK(ok)
    ok &= !mVertices[1]->removed(); VL_CHECK(ok)
    ok &= !mVertices[2]->removed(); VL_CHECK(ok)
    ok &= mVertices[0] != mVertices[1]; VL_CHECK(ok)
    ok &= mVertices[0] != mVertices[2]; VL_CHECK(ok)
    ok &= mVertices[1] != mVertices[2]; VL_CHECK(ok)
    return ok;
  }
  //-----------------------------------------------------------------------------
  inline bool PolygonSimplifier::Triangle::hasVertex(const Vertex*v) const
  {
    return mVertices[0] == v || mVertices[1] == v || mVertices[2] == v;
  }
  //-----------------------------------------------------------------------------
  inline float PolygonSimplifier::Triangle::computePotentialArea(const Vertex* oldv, const Vertex* newv) const
  {
    fvec3 A = (mVertices[0]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[0]->mPosition);
    fvec3 B = (mVertices[1]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[1]->mPosition) - A;
    fvec3 C = (mVertices[2]->mPosition == oldv->mPosition ? newv->mPosition : mVertices[2]->mPosition) - A;

    float base   = 0.0f;
    float height = 0.0f;
    fvec3 AC = C-A;
    fvec3 AB = B-A;
    base = AB.length();
    AB = AB * (1.0f / base); // normalize
    fvec3 h = vl::dot(AC,AB) * AB + A;
    height = (C-h).length();

    return base * height * 0.5f;
  }
  //-----------------------------------------------------------------------------
  inline float PolygonSimplifier::Triangle::computeArea() const
  {
    const fvec3& A = mVertices[0]->mPosition;
    const fvec3& B = mVertices[1]->mPosition;
    const fvec3& C = mVertices[2]->mPosition;

    float base   = 0.0f;
    float height = 0.0f;
    fvec3 AC = C-A;
    fvec3 AB = B-A;
    base = AB.length();
    if (!base)
      return 0;
    AB = AB * (1.0f / base); // normalize
    fvec3 h = vl::dot(AC,AB) * AB + A;
    height = (C-h).length();
    // indeterminate cases
    VL_CHECK( base == base )
    VL_CHECK( height == height )
    return base * height * 0.5f;
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Triangle::computeNormal()
  {
    const fvec3& a = mVertices[0]->mPosition;
    fvec3 b = mVertices[1]->mPosition - a;
    fvec3 c = mVertices[2]->mPosition - a;
    mNormal = cross(b,c);
    mNormal.normalize();
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::Triangle::replaceVertex( Vertex* oldv, Vertex* newv )
  {
    // becomes a degenerate triangle if "newv" is already here
    mRemoved = hasVertex(newv);
    if (mRemoved)
    {
      //mVertices[0]->removeIncidentTriangle(this);
      //mVertices[1]->removeIncidentTriangle(this);
      //mVertices[2]->removeIncidentTriangle(this);
    }
    else
    {
      if (mVertices[0] == oldv) mVertices[0] = newv;
      if (mVertices[1] == oldv) mVertices[1] = newv;
      if (mVertices[2] == oldv) mVertices[2] = newv;
      VL_CHECK( !mVertices[0]->mRemoved )
      VL_CHECK( !mVertices[1]->mRemoved )
      VL_CHECK( !mVertices[2]->mRemoved )
    }
  }
  //-----------------------------------------------------------------------------
  inline void PolygonSimplifier::collapse(Vertex* v)
  {
    VL_CHECK(!v->mRemoved)
    VL_CHECK(v->mCollapseVertex)
    VL_CHECK( !v->mCollapseVertex->mRemoved )
#ifndef NDEBUG
    v->checkConnectivity();
    // check connectivity consistency
    for(int ivert=0; ivert<v->adjacentVerticesCount(); ++ivert)
    {
      VL_CHECK( v->mAdjacentVerts[ivert]->checkConnectivity() )
    }
#endif

    v->mRemoved = true;

    v->mCollapseVertex->mPosition = v->mCollapsePosition;
    v->mCollapseVertex->mQErr += v->mQErr;

    for(int itri=0; itri<v->incidentTrianglesCount(); ++itri)
    {
      VL_CHECK(!v->mIncidentTriangles[itri]->mRemoved)

      // - point the triangle to use the new mCollapseVertex instead of "this"
      // - flags for removal
      v->mIncidentTriangles[itri]->replaceVertex( v, v->mCollapseVertex );

      // pass this's adjacent triangles to mCollapseVertex
      if (!v->mIncidentTriangles[itri]->mRemoved)
      {
        // check that it does not have it already, the ones in common have been marked as "removed"
        VL_CHECK( !v->mCollapseVertex->isIncidentTriangle( v->mIncidentTriangles[itri] ) )
        v->mCollapseVertex->mIncidentTriangles.push_back( v->mIncidentTriangles[itri] );
      }
    }

    // erase removed triangles from its adjacent vertices (including mCollapseVertex)
    for(int ivert=0; ivert<v->adjacentVerticesCount(); ++ivert)
      v->mAdjacentVerts[ivert]->discardRemovedTriangles();

    // update adjacent vertices of all the vertices adjacent to this (including mCollapseVertex)
    for(int ivert=0; ivert<v->adjacentVerticesCount(); ++ivert)
    {
#if 1
      // this is correct and more robust since marks as removed the vertices with 0 triangles
      v->mAdjacentVerts[ivert]->computeAdjacentVertices();
#else
      ... this is not correct since does not mark as removed the vertices that remain without triangles ...
      mAdjacentVerts[ivert]->removeAdjacentVertex(this);
      mAdjacentVerts[ivert]->addAdjacentVertex(mCollapseVertex);
      mCollapseVertex->addAdjacentVertex(mAdjacentVerts[ivert]);
#endif
    }

#ifndef NDEBUG
    for(int ivert=0; ivert<v->mCollapseVertex->adjacentVerticesCount(); ++ivert)
    {
      VL_CHECK( v->mCollapseVertex->adjacentVertex(ivert)->checkTriangles() )
    }

    // and outside even this vertex is removed.
    if ( v->mCollapseVertex->removed() )
    {
      VL_CHECK( v->mCollapseVertex->mIncidentTriangles.empty() )
      VL_CHECK( v->mCollapseVertex->mAdjacentVerts.empty() )
    }
#endif

    // --- now we work on mCollapseVertex ---

    if ( !quick() )
    {
      // update the normals, used to compute anti-folding
      for(int itri=0; itri<v->mCollapseVertex->incidentTrianglesCount(); ++itri)
      {
        VL_CHECK( !v->mCollapseVertex->mIncidentTriangles[itri]->removed() )
        VL_CHECK( v->mCollapseVertex->mIncidentTriangles[itri]->checkTriangle() )
        v->mCollapseVertex->mIncidentTriangles[itri]->computeNormal();
      }
    }

    VL_CHECK( !v->mCollapseVertex->isAdjacentVertex(v) )

    // disconnect this vertex
    v->mIncidentTriangles.clear();
    v->mAdjacentVerts.clear();
  }
  //-----------------------------------------------------------------------------
  // compute collapse cost and vertex
  inline void PolygonSimplifier::computeCollapseInfo(Vertex* v)
  {
    VL_CHECK(!v->mRemoved)
    if(v->mRemoved)
      return;

    // choose the edge with minimum cost

    // intialize with a very high cost
    v->mCollapseCost = 1.0e+38f;
    v->mCollapseVertex = NULL;
    VL_CHECK( v->adjacentVerticesCount() )
    for(int ivert=0; ivert<v->adjacentVerticesCount(); ++ivert)
    {
      VL_CHECK(!v->mAdjacentVerts[ivert]->mRemoved)

      double cost = 0.0;
      dvec3 solution;
      if (quick())
      {
        QErr qe = v->qerr();
        qe += v->mAdjacentVerts[ivert]->qerr();
        // find the best solution
        solution = (dvec3)v->position();
        solution += (dvec3)v->mAdjacentVerts[ivert]->position();
        solution *= 0.5;
        cost = qe.evaluate( solution );
      }
      else
      {
        QErr qe = v->qerr();
        qe += v->mAdjacentVerts[ivert]->qerr();
        bool analytic_ok = qe.analyticSolution(solution);
        if ( analytic_ok )
        {
          cost = qe.evaluate(solution);
          VL_CHECK(cost < 1e+38)
        }
        else
        {
          dvec3 a = (dvec3)v->position();
          dvec3 b = (dvec3)v->mAdjacentVerts[ivert]->position();
          dvec3 c = (a+b) * 0.5;
          double ae = qe.evaluate(a);
          double be = qe.evaluate(b);
          double ce = qe.evaluate(c);
          if (ae < be && ae < ce)
          {
            solution = a;
            cost = ae;
          }
          else
          if (be < ae && be < ce)
          {
            solution = b;
            cost = be;
          }
          else
          {
            solution = c;
            cost = ce;
          }
          VL_CHECK(cost < 1e+38)
        }

        int degenerate_count = 0;
        for( int itri=0; itri<v->incidentTrianglesCount() && !degenerate_count; ++itri )
        {
          // triangle to be removed
          if ( v->incidentTriangle(itri)->hasVertex(v->mAdjacentVerts[ivert]) )
            continue;

          Vertex* edgev[] = { NULL, NULL };
          if ( v == v->incidentTriangle(itri)->vertex(0) )
          {
            edgev[0] = v->incidentTriangle(itri)->vertex(1);
            edgev[1] = v->incidentTriangle(itri)->vertex(2);
          }
          else
          if ( v == v->incidentTriangle(itri)->vertex(1) )
          {
            edgev[0] = v->incidentTriangle(itri)->vertex(0);
            edgev[1] = v->incidentTriangle(itri)->vertex(2);
          }
          else
          if ( v == v->incidentTriangle(itri)->vertex(2) )
          {
            edgev[0] = v->incidentTriangle(itri)->vertex(0);
            edgev[1] = v->incidentTriangle(itri)->vertex(1);
          }

          fvec3 edge = (edgev[1]->position() - edgev[0]->position());
          fvec3 n = cross( edge, v->incidentTriangle(itri)->normal() );
          n.normalize();
          float d1 = dot( v->position() - edgev[0]->position(), n );
          float d2 = dot( (fvec3)solution - edgev[0]->position(), n );

          if (d1 * d2 < 0)
            ++degenerate_count;
        }

        // controlla i triangoli intorno a v->mAdjacentVerts[ivert]
        Vertex* u = v->mAdjacentVerts[ivert];
        for( int itri=0; itri<u->incidentTrianglesCount() && !degenerate_count; ++itri )
        {
          // triangle to be removed
          if ( u->incidentTriangle(itri)->hasVertex(v) )
            continue;

          Vertex* edgev[] = { NULL, NULL };
          if ( u == u->incidentTriangle(itri)->vertex(0) )
          {
            edgev[0] = u->incidentTriangle(itri)->vertex(1);
            edgev[1] = u->incidentTriangle(itri)->vertex(2);
          }
          else
          if ( u == u->incidentTriangle(itri)->vertex(1) )
          {
            edgev[0] = u->incidentTriangle(itri)->vertex(0);
            edgev[1] = u->incidentTriangle(itri)->vertex(2);
          }
          else
          if ( u == u->incidentTriangle(itri)->vertex(2) )
          {
            edgev[0] = u->incidentTriangle(itri)->vertex(0);
            edgev[1] = u->incidentTriangle(itri)->vertex(1);
          }

          fvec3 edge = (edgev[1]->position() - edgev[0]->position());
          fvec3 n = cross( edge, u->incidentTriangle(itri)->normal() );
          n.normalize();
          float d1 = dot( u->position() - edgev[0]->position(), n );
          float d2 = dot( (fvec3)solution - edgev[0]->position(), n );

          if (d1 * d2 < 0)
            ++degenerate_count;
        }

        // non acceptable solution, assign very high cost
        if (degenerate_count)
          cost = 1.0e+37f;
      }

      // to correctly simplify planar and cylindrical regions
      cost += ((dvec3)v->position() - solution).length() * 1.0e-12;

      // check indeterminate case
      VL_CHECK( cost == cost )
      if ( cost < v->mCollapseCost )
      {
        v->mCollapseCost     = (float)cost;
        v->mCollapseVertex   = v->mAdjacentVerts[ivert];
        v->mCollapsePosition = (fvec3)solution;
      }
    }

    VL_CHECK( v->mCollapseVertex )
  }
  //-----------------------------------------------------------------------------
}

#endif
