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

#ifndef EdgeExtractor_INCLUDE_ONCE
#define EdgeExtractor_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Vector3.hpp>
#include <vlGraphics/link_config.hpp>
#include <vector>
#include <set>

namespace vl
{
  class SceneManager;
  class Rendering;
  class Actor;
  class ActorCollection;
  class Geometry;

  /** The EdgeExtractor class extracts the edges from one or more Geometry objects.

  The edges are always extracted from the triangles or quads that are part of a vl::Geometry and can be of three types: 
  \a silhouette edges, \a crease edges and \a boundary edges. 
	
	- \a Silhouette edges are those edges that are shared by a backfacing and front facing quad or triangle. 
	
	- \a Crease edges are those edges that are shared by two triangles or quads and that form an angle >= creaseAngle(), ie: 0 means that the two triangles or quads are coplanar; the edges of a cube define 90 degrees angles; the lateral edges of a 10 faces cylinder define 36 (360/10) degrees angles and so on. 
	
	- \a Boundary edges are those edges that belong to a single triangle or quad.

  <img src="pics/pagGuideWireframe_edges.jpg">

  \par Usage
  - Extract the edges from one or more Geometry objects using one of the extractEdges() methods.
  - Assign the Geometry returned by generateEdgeGeometry() to a new Actor. This geometry will contain the edges previously extracted ready to be rendered.
  - Assign a new EdgeUpdateCallback to the previously created Actor, using the Actor::renderEventCallbacks() method.
  - Initialize the previously created EdgeUpdateCallback edges with the edges extracted by the EdgeExtractor, 
    that is, assign EdgeExtractor::edges() to EdgeUpdateCallback::edges().
 
  \sa 
  - \ref pagGuideEdgeRendering "Edge Enhancement and Wireframe Rendering Tutorial"
  - vl::EdgeRenderer
  */
  class VLGRAPHICS_EXPORT EdgeExtractor: public Object
  {
    VL_INSTRUMENT_CLASS(vl::EdgeExtractor, Object)

  public:
    //! A single edge as extracted from the EdgeExtractor class.
    class Edge
    {
    public:
      Edge(): mIsCrease(false) {}
      Edge(const fvec3& v1, const fvec3& v2)
      {
        mIsCrease = false;
        if (v1<v2)
        {
          mVertex1 = v1;
          mVertex2 = v2;
        }
        else
        {
          mVertex1 = v2;
          mVertex2 = v1;
        }
      }

      void setVertex1(const fvec3& v) { mVertex1 = v; }
      const fvec3& vertex1() const { return mVertex1; }

      void setVertex2(const fvec3& v) { mVertex2 = v; }
      const fvec3& vertex2() const { return mVertex2; }

      void setNormal1(const fvec3& v) { mNormal1 = v; }
      const fvec3& normal1() const { return mNormal1; }

      void setNormal2(const fvec3& v) { mNormal2 = v; }
      const fvec3& normal2() const { return mNormal2; }

      bool  isCrease() const { return mIsCrease; }
      void setIsCrease(bool iscrease) { mIsCrease = iscrease; }

      bool operator<(const Edge& other) const
      {
        if (vertex1() != other.vertex1())
          return vertex1() < other.vertex1();
        else
          return vertex2() < other.vertex2();
      }

      bool operator==(const Edge& other) const
      {
        return (vertex1() == other.vertex1() && vertex2() == other.vertex2()) ||
               (vertex1() == other.vertex2() && vertex2() == other.vertex1());
      }

    protected:
      fvec3 mVertex1;
      fvec3 mVertex2;
      fvec3 mNormal1;
      fvec3 mNormal2;
      bool  mIsCrease;
    };

  public:
    EdgeExtractor(): mCreaseAngle(45.0f), mWarnNonManifold(false)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    void extractEdges(Geometry* geom);
    bool extractEdges(Actor* actor);
    void extractEdges(ActorCollection* actors);
    void extractEdges(SceneManager* scenemanager);
    void extractEdges(Rendering* rendering);

    ref<Geometry> generateEdgeGeometry() const;

    const std::vector<Edge>& edges() const { return mEdges; }
    std::vector<Edge>& edges() { return mEdges; }

    void reset() { mEdges.clear(); }

    //! The minimum angle (in degrees) considered to generate crease-edges
    float creaseAngle() const { return mCreaseAngle; }
    //! The minimum angle (in degrees) considered to generate crease-edges
    void setCreaseAngle(float a) { mCreaseAngle = a; }

    bool warnNonManifold() const { return mWarnNonManifold; }
    void setWarnNonManifold(bool warn_on) { mWarnNonManifold = warn_on; }

  protected:
    void addEdge(std::set<EdgeExtractor::Edge>& edges, const EdgeExtractor::Edge& e, const fvec3& n);

  protected:
    std::vector<Edge> mEdges;
    float mCreaseAngle;
    bool mWarnNonManifold;
  };
}

#endif
