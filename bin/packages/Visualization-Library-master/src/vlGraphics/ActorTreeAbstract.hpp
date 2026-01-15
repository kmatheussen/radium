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

#ifndef ActorTree_INCLUDE_ONCE
#define ActorTree_INCLUDE_ONCE

#include <vlGraphics/Actor.hpp>
#include <vlCore/AABB.hpp>
#include <set>

namespace vl
{
  /** The ActorTreeAbstract class implements the interface of a generic tree containing Actor[s] in its nodes.
   * 
   * The interface of ActorTreeAbstract allows you to:
   * - Modify the Actor[s] contained in each node
   * - Traverse the tree and visit its nodes
   * - Compute the AABB of a single node or of the whole tree
   * 
   * Does \b not allow you to:
   * - Add new nodes to the tree
   *
   * \sa
   * - ActorKdTree
   * - ActorTree
   */
  class VLGRAPHICS_EXPORT ActorTreeAbstract: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::ActorTreeAbstract, Object)

  public:
    ActorTreeAbstract();

    //! Returns the number of child nodes of an ActorTreeAbstract node.
    virtual int childrenCount() const = 0;
    //! Returns the i-th child node of an ActorTreeAbstract node.
    virtual ActorTreeAbstract* child(int i) = 0;
    //! Returns the i-th child node of an ActorTreeAbstract node.
    virtual const ActorTreeAbstract* child(int i) const = 0;

    //! Returns the parent of a node.
    const ActorTreeAbstract* parent() const { return mParent; }
    //! Returns the parent of a node.
    ActorTreeAbstract* parent() { return mParent; }

    //! Returns the actors contained in a ActorTree node
    const ActorCollection* actors() const { return &mActors; }
    //! Returns the actors contained in a ActorTree node
    ActorCollection* actors() { return &mActors; }

    //! Returns the bounding box of a node. Such bounding box contains both the bounding boxes of the node's Actor[s] and of the child nodes.
    const AABB& aabb() const { return mAABB; }

    //! Recursively computes the bounding box of a node so that it includes the bounding boxes of the node's Actor[s] and of the child nodes.
    void computeAABB();

    /**
     * Extracts all the Actor[s] contained in th ActorTree hierarchy and appends them to the given ActorCollection.
     */
    void extractActors(ActorCollection& list);

    /**
     * Extracts the enabled and visible Actor[s] contained in th ActorTree hierarchy and appends them to the given ActorCollection.
     * This function implements a hierarchycal frustum culling algorithm that culls the nodes of the bounding box tree first and then
     * the single Actor[s] contained in the nodes that could not be culled.
     * See also Actor::enableMask()
     */
    void extractVisibleActors(ActorCollection& list, const Camera* camera, unsigned enable_mask=0xFFFFFFFF);

    /**
     * Removes the given Actor from the ActorTreeAbstract.
     */
    ActorTreeAbstract* eraseActor(Actor* actor);

    //! Utility function that adds an Actor and binds it to the given Renderable, Effect and Transform.
    Actor* addActor(Renderable* renderable, Effect* eff, Transform* tr=NULL);

    //! Utility function equivalent to 'actors()->push_back(actor)'.
    Actor* addActor(Actor* actor);

    /**
     * Updates the Transform and the bounds of the given Actor[s].
     * Before you create a bounding box tree or a kd-tree of Actor[s] you have to
     * make sure that their Transform and bounding volumes are up-to-date. This
     * is an utility function that lets you do that in a simple an quick way.
     */
    static void prepareActors(ActorCollection& actors);

    //! For internal use only.
    void setParent(ActorTreeAbstract* p) { mParent = p; }

  protected:
    ActorTreeAbstract* mParent;
    ActorCollection mActors;
    AABB mAABB;
  };
}

#endif
