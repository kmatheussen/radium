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

#ifndef RayIntersector_INCLUDE_ONCE
#define RayIntersector_INCLUDE_ONCE

#include <vlCore/Ray.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Matrix4.hpp>
#include <vlGraphics/Frustum.hpp>

namespace vl
{
  class SceneManager;
  //-----------------------------------------------------------------------------
  // RayIntersection
  //-----------------------------------------------------------------------------
  /** The RayIntersection encapsulates all the information relative to a Ray/Actor intersection.
   */
  class RayIntersection: public Object
  {
    VL_INSTRUMENT_CLASS(vl::RayIntersection, Object)

  public:
    RayIntersection(): mActor(NULL), mDistance(0.0f) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! The intersected Actor
    void setActor(Actor* a) { mActor = a; }
    //! The intersected Actor
    const Actor* actor() const { return mActor; }
    //! The intersected Actor
    Actor* actor() { return mActor; }

    //! The intersection point, in world coordinates
    const vec3& intersectionPoint() const { return mIntersectionPoint; }
    //! The intersection point, in world coordinates
    void setIntersectionPoint(const vec3& v) { mIntersectionPoint = v; }

    //! The intersection distance
    real distance() const { return mDistance; }
    //! The intersection distance
    void setDistance(real dist) { mDistance = dist; }

  protected:
    vec3 mIntersectionPoint;
    Actor* mActor;
    real mDistance;
  };
  //-----------------------------------------------------------------------------
  // RayIntersectionGeometry
  //-----------------------------------------------------------------------------
  /** The RayIntersectionGeometry encapsulates all the information relative to a Ray/Actor intersection, 
      providing also extra information relative to the intersection on the Geometry in use by the Actor. */
  class RayIntersectionGeometry: public RayIntersection
  {
    VL_INSTRUMENT_CLASS(vl::RayIntersectionGeometry, RayIntersection)

  public:
    RayIntersectionGeometry(): mGeometry(NULL), mDrawCalls(NULL), mTriangleIndex(-1) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
      memset(mTriangle, 0xFF, sizeof(mTriangle));
    }

    //! The intersected Geometry
    Geometry* geometry() { return mGeometry; }
    //! The intersected Geometry
    const Geometry* geometry() const { return mGeometry; }
    //! The intersected DrawCall
    DrawCall* drawCalls() { return mDrawCalls; }
    //! The intersected DrawCall
    const DrawCall* drawCalls() const { return mDrawCalls; }
    //! The starting index of the intersected primitive inside drawCalls()
    int triangleIndex() const { return mTriangleIndex; }
    //! An int[3] representing the indices of the intersected triangle.
    const int* triangle() const { return mTriangle; }

    //! The intersected Geometry
    void setGeometry(Geometry* g) { mGeometry = g; }
    //! The intersected DrawCall
    void setPrimitives(DrawCall* p) { mDrawCalls = p; }
    //! The starting index of the intersected primitive inside drawCalls()
    void setTriangleIndex(int t_idx) { mTriangleIndex = t_idx; }
    //! An int[3] representing the indices of the intersected triangle.
    void setTriangle(int a, int b, int c) { mTriangle[0] = a; mTriangle[1] = b; mTriangle[2] = c; }

  protected:
    vec3 mIntersectionPoint;
    Geometry* mGeometry;
    DrawCall* mDrawCalls;
    int mTriangleIndex;
    int mTriangle[3];
    float mDistance;
  };
  //-----------------------------------------------------------------------------
  // RayIntersector
  //-----------------------------------------------------------------------------
  /** The RayIntersector class is used to detect the intersection points between a Ray and a set of Actor[s]
   */
  class VLGRAPHICS_EXPORT RayIntersector: public Object
  {
    VL_INSTRUMENT_CLASS(vl::RayIntersector, Object)

  public:
    RayIntersector()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mActors = new ActorCollection;
    }

    //! The Actors against which the intersection test is performed.
    const ActorCollection* actors() const { return mActors.get(); }
    //! The Actors against which the intersection test is performed.
    ActorCollection* actors() { return mActors.get(); }

    //! The ray in world coordinates to be intersected with the actors()
    const Ray& ray() const { return mRay; }
    //! The ray in world coordinates to be intersected with the actors()
    void setRay(const Ray& ray) { mRay = ray; }

    //! The frustum in world coordinates used to cull the objects.
    const Frustum& frustum() const { return mFrustum; }
    //! The frustum in world coordinates used to cull the objects.
    void setFrustum(const Frustum& frustum) { mFrustum = frustum; }

    //! The intersection points detected by the last intersect() call sorted according to their distance (the first one is the closest).
    const std::vector< ref<RayIntersection> >& intersections() const { return mIntersections; }

    /** Executes the intersection test.
     * \note Before calling this function the transforms and the bounding volumes of the Actor[s] to be intersected must be updated, in this order.
     * \note All the intersections are mande on the Actor's LOD level #0.
     */
    void intersect();

    /** Computes the intersections between the given ray and the Actor[s] contained in the given scene manager.
      * This is an utility function equivalent to:
      * \code
      * intersector->actors()->clear();
      * scene_manager->extractActors( *intersector->actors() );
      * intersector->setRay(ray);
      * intersector->intersect();
      * \endcode
      */
    void intersect(const Ray& ray, SceneManager* scene_manager);

  protected:
    static bool sorter(const ref<RayIntersection>& a, const ref<RayIntersection>& b) { return a->distance() < b->distance(); }

    void intersect(Actor* act);
    void intersectGeometry(Actor* act, Geometry* geom);

    // T should be either fvec3-4 or dvec3-4
    template<class T>
    void intersectTriangle(const T& a, const T& b, const T& c, int ia, int ib, int ic, Actor*, Geometry* geom, DrawCall* prim, int prim_idx);

  protected:
    Frustum mFrustum;
    std::vector< ref<RayIntersection> > mIntersections;
    ref<ActorCollection> mActors;
    Ray mRay;
  };
}

#endif
