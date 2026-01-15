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

#ifndef PortalSceneManager_INCLUDE_ONCE
#define PortalSceneManager_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Vector3.hpp>
#include <vlGraphics/SceneManager.hpp>
#include <vlCore/Log.hpp>
#include <vlGraphics/Frustum.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
  class Sector;
  class SceneManagerPortals;
//-----------------------------------------------------------------------------
  //! A planar convex polygon used to define the visibility from one Sector to another.
  //! See also:
  //! - SceneManagerPortals
  //! - Sector
  //! - \ref pagGuidePortals "Portal-Based Culling and Scene Management Tutorial"
  class VLGRAPHICS_EXPORT Portal: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Portal, Object)

    friend class SceneManagerPortals;

  public:
    //! Constructor.
    Portal()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mIsOpen = true;
      mVisitTick = 0;
    }

    //! The set of points defining the shape of the portal. The points must define a planar convex polygon and must be in world coordinates.
    std::vector<fvec3>& geometry() { return mPortalGeometry; }
    //! The set of points defining the shape of the portal. The points must define a planar convex polygon and must be in world coordinates.
    const std::vector<fvec3>& geometry() const { return mPortalGeometry; }

    //! The Sector that is behind the portal and that is seen through the portal.
    void setTargetSector(Sector* sector) { mTargetSector = sector; }
    //! The Sector that is behind the portal and that is seen through the portal.
    Sector* targetSector() { return mTargetSector; }
    //! The Sector that is behind the portal and that is seen through the portal.
    const Sector* targetSector() const { return mTargetSector; }

    //! If a Portal is closed or open. If the portal is closed then the camera cannot see the targetSector() through this portal.
    bool isOpen() const { return mIsOpen; }
    //! If a Portal is closed or open. If the portal is closed then the camera cannot see the targetSector() through this portal.
    void setIsOpen(bool is_open) { mIsOpen = is_open; }

  protected:
    //! Used internally.
    void setNormal(const fvec3& n) { mNormal = n; }
    //! Used internally.
    const fvec3& normal() const { return mNormal; }

    //! Computes the normal of the portal polygon.
    //! Must be called after all the sectors have been setup, linked to their portal, and their AABB has been updated.
    bool computeNormal();

  protected:
    std::vector<fvec3> mPortalGeometry;
    Sector* mTargetSector;
    fvec3 mNormal;
    unsigned int mVisitTick;
    bool mIsOpen;
  };
//-----------------------------------------------------------------------------
  /** Defines an area containg a set if Actor[s] that is connected to other Sector[s] through its Portal[s].
   *  See also:
   *  - SceneManagerPortals
   *  - Portal
   *  - \ref pagGuidePortals "Portal-Based Culling and Scene Management Tutorial"
   */
  class VLGRAPHICS_EXPORT Sector: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Sector, Object)

  public:
    /** A callback object called each time a Sector becomes visible through a Portal.
     *  Note: a callback can be called multiple times with the same Sector argument if a Sector is discovered multiple times through different portals.
     *  Using callbacks can be very useful to perform special actions upon sector discovery, like enabling/disabling animations, enabling/disabling 
     *  a ActorKdTree scene manager or a Terrain scene manager to render the external environment etc.
     */
    class VisibilityCallback: public Object
    {
    public:
        /** Callback.
         * Note: a callback can be called multiple times with the same Sector argument if a Sector is discovered multiple times through different portals.
         *
         * \param cam The current Camera.
         * \param psm The SceneManagerPortals that generated the callback.
         * \param s The Sector that has become visible.
         * \param p The Portal used to enter the Sector. It is set to NULL if \p s is the starting Sector (the Sector in which the camera is).
         */
        virtual void operator()(const Camera* cam, SceneManagerPortals* psm, Sector* s, Portal* p) = 0;
    };
  public:
    //! Constructor.
    Sector() 
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      mActors = new ActorCollection; 
    }

    //! The Actor object contained in a sector. An actor can be part of multiple sectors.
    ActorCollection* actors() { return mActors.get(); }
    //! The Actor object contained in a sector. An actor can be part of multiple sectors.
    const ActorCollection* actors() const { return mActors.get(); }

    //! The portals within a sector that connect it to other sectors.
    std::vector< ref<Portal> >& portals() { return mPortals; }
    //! The portals within a sector that connect it to other sectors.
    const std::vector< ref<Portal> >& portals() const { return mPortals; }

    //! A set of volumes used to test if the camera is or not inside a Sector.
    //! The volumes of a sector must not intersecate with the volumes of another sector.
    std::vector< AABB >& volumes() { return mVolumes; }
    //! A set of volumes used to test if the camera is or not inside a Sector.
    //! The volumes of a sector must not intersecate with the volumes of another sector.
    const std::vector< AABB >& volumes() const { return mVolumes; }

    //! Returns the bounding box of all the Actor[s] in the sector.
    AABB computeBoundingBox();

    std::vector< ref<VisibilityCallback> >& callbacks() { return mCallbacks; }
    const std::vector< ref<VisibilityCallback> >& callbacks() const { return mCallbacks; }
    void executeCallbacks(const Camera*cam,SceneManagerPortals* psm, Portal*p);

  protected:
    std::vector< ref<Portal> > mPortals;
    std::vector< AABB > mVolumes;
    ref< ActorCollection > mActors;
    std::vector< ref<VisibilityCallback> > mCallbacks;
  };
//-----------------------------------------------------------------------------
  /** The SceneManagerPortals calss implements a portal-based hidden surface removal algorithm to efficently render highly occluded scenes.
   * \sa
   * - \ref pagGuidePortals "Portal-Based Culling and Scene Management Tutorial"
   * - Portal
   * - Sector
   * - Actor
   * - ActorKdTree
   * - ActorTree
   * - SceneManager
   * - SceneManagerBVH
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   */
  class VLGRAPHICS_EXPORT SceneManagerPortals: public SceneManager
  {
    VL_INSTRUMENT_CLASS(vl::SceneManagerPortals, SceneManager)

  public:
    //! Constructor.
    SceneManagerPortals(): mExternalSector(new Sector), mVisitTick(1), mShowPortals(false) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! Appends to the given list all the Actor[s] contained in the scene regardless of their visibility.
    void extractActors(ActorCollection& list);
    //! Appends to the given list all the visible Actor[s] using the portal culling algorithm.
    void extractVisibleActors(ActorCollection& list, const Camera* camera);

    //! The Sectors that are part of the scene.
    std::vector< ref<Sector> >& sectors() { return mSectors; }
    //! The Sectors that are part of the scene.
    const std::vector< ref<Sector> >& sectors() const { return mSectors; }

    //! Returns the external sector. 
    //! The external sector is a special sector that is considered visible when the camera is not inside any other sector.
    //! The external sector can be used to contain objects that are outside the indoor environment defined by the other "normal" sectors.
    //! For example, you could use the SceneManagerPortals to model a house and put in the external sector all the objects that are outside
    //! the house. The portals inside the house can point to the external sector so that the objects outside the house are 
    //! rendered only if they are visible through a door or a window for maximum performances. Of course you can also go from an external sector
    //! to an internal sector just as well using one or more portals.
    Sector* externalSector() { return mExternalSector.get(); }
    //! Returns the external sector.
    const Sector* externalSector() const { return mExternalSector.get(); }

    //! Compute the normal of the sectors.
    void computePortalNormals();

    //! Calls computePortalNormals() and performs some error checking.
    void initialize();

    //! Whether portals should be shown in the rendering or not.
    bool showPortals() const { return mShowPortals; }
    //! Whether portals should be shown in the rendering or not.
    void setShowPortals(bool show) { mShowPortals = show; }
    //! Regenerates the portal actors next time their rendering is requested.
    void invalidatePortalActors() { mPortalActorMap.clear(); }

    //! The stack of frustums active at a given point during sector discovery.
    const std::vector<Frustum>& frustumStack() const { return mFrustumStack; }

  protected:
    void renderPortal(Portal* portal);
    void visitSector(Sector* prev, Sector* sector, const vec3& eye, const Camera* camera);
    Sector* computeStartingSector(const Camera* camera);

  protected:
    ref<Sector> mExternalSector;
    std::vector< ref<Sector> > mSectors;
    std::vector< ref<Actor> > mTempActors;
    std::map<Portal*, ref<Actor> > mPortalActorMap;
    std::vector<Frustum> mFrustumStack;
    unsigned mVisitTick;
    bool mShowPortals;
  };
//-----------------------------------------------------------------------------
}

#endif
