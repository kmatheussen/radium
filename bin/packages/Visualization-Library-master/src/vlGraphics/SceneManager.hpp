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

#ifndef SceneManager_INCLUDE_ONCE
#define SceneManager_INCLUDE_ONCE

#include <vlGraphics/link_config.hpp>
#include <vlCore/Object.hpp>
#include <vlCore/Sphere.hpp>

namespace vl
{
  class Actor;
  class ActorCollection;
  class Camera;

//-------------------------------------------------------------------------------------------------------------------------------------------
// SceneManager
//-------------------------------------------------------------------------------------------------------------------------------------------
  /**
   * The SceneManager class is the base class for all the scene managers.
   *
   * A SceneManager implements an algorithm used to quickly identify which objects are visible and which ones are not.
   * The algorithm can be a space partitioning scheme like a BSP, Kd-Tree, Octree etc. or a visibility based scheme like
   * sector/portals, precomputed PVS etc. or a mix of them. The set of SceneManager[s] attached to a Rendering
   * defines the scene. In fact, if an Actor does not belong to any SceneManager it will not have any chance of being rendered.
   *
   * Visualization Library allows you to bind and use multiple SceneManagers at the same time within the same Rendering.
   * Note that an Actor should belong to one and only one SceneManager otherwise you might end up rendering twice the same Actor
   * thus wasting computational resources.
   *
   * In order to implement your own scene manager you will have to dirive from the SceneManager class and provide an appropriate
   * implementation for the following methods: extractVisibleActors(), extractActors().
   * 
   * \sa
   * - ActorKdTree
   * - ActorTree
   * - SceneManager
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   * - SceneManagerPortals
   * - Actor
  */
  class VLGRAPHICS_EXPORT SceneManager: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::SceneManager, Object)

  public:
    //! Constructor.
    SceneManager();

    //! Performs frustum culling and appends the enabled and visible Actor[s] to the given ActorCollection.
    //! See also enableMask(), Actor::enableMask()
    virtual void extractVisibleActors(ActorCollection& list, const Camera* camera)= 0;

    //! Appends all the Actor[s] contained in the scene manager without performing frustum culling or checking enable masks.
    virtual void extractActors(ActorCollection& list) = 0;

    //! Computes the bounding box and bounding sphere of the scene manager and of all the Actor[s] contained in the SceneManager.
    virtual void computeBounds();

    //! Explicitly set the scene manager's bounding sphere. See also computeBounds().
    void setBoundingSphere(const Sphere& sphere) { mSphere = sphere; }
    //! Returns the scene manager's bounding sphere.
    const Sphere& boundingSphere() const { return mSphere; }

    //! Explicitly set the scene manager's bounding sphere. See also computeBounds().
    void setBoundingBox(const AABB& bbox) { mAABB = bbox; }
    //! Returns the scene manager's bounding box.
    const AABB& boundingBox() const { return mAABB; }

    //! Flags a scene manager's bounding box and bounding sphere as dirty. The bounds will be recomputed using computeBounds() at the next rendering frame.
    void setBoundsDirty(bool dirty) { mBoundsDirty = dirty; }
    //! Returns true if the scene manager's bounds should be recomputed at the next rendering frame.
    bool boundsDirty() const { return mBoundsDirty; }

    //! Used to enable or disable frustum culling or whichever culling system the scene manager implements.
    void setCullingEnabled(bool enable) { mCullingEnabled = enable; }
    //! Used to enable or disable frustum culling or whichever culling system the scene manager implements.
    bool cullingEnabled() const { return mCullingEnabled; }

    void setEnableMask(unsigned int enabled) { mEnableMask = enabled; }
    unsigned int enableMask() const { return mEnableMask; }
    //! Returns \p true if \p "a->enableMask() & enableMask()) != 0"
    bool isEnabled(Actor*a) const;

  protected:
    Sphere mSphere;
    AABB mAABB;
    unsigned int mEnableMask;
    bool mBoundsDirty;
    bool mCullingEnabled;
  };
}

#endif
