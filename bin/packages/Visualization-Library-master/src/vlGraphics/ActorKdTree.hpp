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

#ifndef ActorKdTree_INCLUDE_ONCE
#define ActorKdTree_INCLUDE_ONCE

#include <vlCore/AABB.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/math_utils.hpp>
#include <vlCore/Plane.hpp>
#include <vlCore/Collection.hpp>
#include <vlGraphics/ActorTreeAbstract.hpp>

namespace vl
{
  /**
   * ActorKdTree class extends the ActorTreeAbstract class implementing a space partitioning scheme based on a Kd-Tree.
   *
   * \note 
   * When building the Kd-Tree, Visualization Library considers the Actors' LOD level 0.
   * 
   * \sa
   * - ActorTree
   * - SceneManager
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   * - Actor
  */
  class VLGRAPHICS_EXPORT ActorKdTree: public ActorTreeAbstract
  {
    VL_INSTRUMENT_CLASS(vl::ActorKdTree, ActorTreeAbstract)

  public:
    ActorKdTree()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual int childrenCount() const;
    virtual ActorTreeAbstract* child(int i);
    virtual const ActorTreeAbstract* child(int i) const;

  /**
   * Builds a ActorKdTree with the given list of Actor[s].
   * The ActorKdTree generation routine will try to minimize the ActorKdTree depth. Note that this function
   * is relatively quick, but is not for free. Consider that using a Core 2 Duo @2.0GHz you can
   * process around 22.000 objects/sec.
   * \note This method calls prepareActors() before computing the KdTree.
   */
  void buildKdTree(ActorCollection& actors, int max_depth=100, float minimum_volume=0);

  //! Builds a ActorKdTree with the Actor[s] contained in the tree.
  //! \note This method calls prepareActors() before computing the KdTree.
  void rebuildKdTree(int max_depth=100, float minimum_volume=0);

  //! Returns the splitting plane used to divide its two child nodes
  const Plane& plane() const { return mPlane; }

  //! Returns the child node that lies in the negative space defined by the splitting plane
  ActorKdTree* childN() { return mChildN.get(); }
  //! Returns the child node that lies in the negative space defined by the splitting plane
  const ActorKdTree* childN() const { return mChildN.get(); }

  //! Returns the child node that lies in the positive space defined by the splitting plane
  ActorKdTree* childP() { return mChildP.get(); }
  //! Returns the child node that lies in the positive space defined by the splitting plane
  const ActorKdTree* childP() const { return mChildP.get(); }

  /**
   * Inserts an Actor in the ActorKdTree node hierarchy.
   * Note that the Actor is likely to be inserted in a node whose bounding volume does not surround the Actor's bounding volume.
   * For this reason after you inserted one or more Actor[s] in the ActorKdTree you should call computeAABB() on the root node of 
   * the ActorKdTree. Inserting and removing Actor[s] is an expensive operation and produces an ActorKdTree that is 
   * less balanced than the one you would get by recompiling the whole ActorKdTree from scratch.
   *
   * \return
   * The ActorKdTree node in which the Actor has been inserted.
   *
   * \sa
   * ActorTree::eraseActor()
   */
  ActorKdTree* insertActor(Actor* actor);

  /**
   * Removes the Actor[s] in the internal nodes of the ActorKdTree and uses them to create a new ActorKdTree.
   */
  ref<ActorKdTree> kdtreeFromNonLeafyActors(int max_depth=100, float minimum_volume=0);

  /**
   * Removes the Actor[s] in the internal nodes of the ActorKdTree and appends them in the given ActorCollection.
   */
  void harvestNonLeafActors(ActorCollection& actors);

  private:
    void setChildN(ActorKdTree* child) 
    { 
      VL_CHECK(child); 
      if (mChildN)
        mChildN->mParent = NULL;
      child->mParent = this;
      mChildN=child; 
    }
    void setChildP(ActorKdTree* child) 
    { 
      VL_CHECK(child); 
      if (mChildP)
        mChildP->mParent = NULL;
      child->mParent = this;
      mChildP=child; 
    }
    //! Computes a score for the plane, the closer to zero the better.
    int scorePlane(const Plane& plane, const ActorCollection& actors);
    //! Finds the best plane among different x/y/z orientation in order to divide the given
    //! list of actors included in the given AABB.
    bool findBestPlane(Plane& plane, int& counter, ActorCollection& actors);
    //! 
    void compileTree_internal(ActorCollection& acts, int& counter, int max_depth=100, float minimum_volume=0);
    //!
    void computeLocalAABB(const ActorCollection& actors);

  protected:
    Plane mPlane;
    ref<ActorKdTree> mChildN;
    ref<ActorKdTree> mChildP;
  };

}

#endif
