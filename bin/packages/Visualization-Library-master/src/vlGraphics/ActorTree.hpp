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

#include <vlGraphics/ActorTreeAbstract.hpp>

#ifndef ActorTreeMulti_INCLUDE_ONCE
#define ActorTreeMulti_INCLUDE_ONCE

namespace vl
{
  /**
   * The ActorTree class implements a generic tree whose nodes contain Actor[s]. Each node of the tree can have any number of children.
   *
   * Use this class when you want to have direct control over how the Actor[s] are
   * grouped together or to implement specific space partitioning schemes like
   * BSP trees, Quadtrees, Octrees etc. For example you can use the ActorTree class 
   * to build a quadtree by assigning 4 children per node and filling them appropriately.
   * Of course you can also use an ActorTree to group a set of Actor[s] based on any other
   * principle. For example animated Actor[s] are usually kept in separate "flat" trees 
   * (for example all in a single node) as the computational cost of rebuilding every frame 
   * the BSP tree, Quadtree, Octree etc. might be to high.
   * 
   * Note that for performance reasons the ActorKdTree class derives from the ActorTreeAbstract 
   * and implements a space partitioning scheme (based on a binary tree) where the splitting 
   * planes are in turn choosen so that they are aligned to the world space x, y and z axes.
   * 
   * \sa
   * - ActorKdTree
   * - SceneManager
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   * - Actor
   */
  class VLGRAPHICS_EXPORT ActorTree: public ActorTreeAbstract
  {
    VL_INSTRUMENT_CLASS(vl::ActorTree, ActorTreeAbstract)

  public:
    ActorTree()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    virtual int childrenCount() const { return (int)mChildren.size(); }
    virtual ActorTreeAbstract* child(int i) { return mChildren[i].get(); }
    virtual const ActorTreeAbstract* child(int i) const { return mChildren[i].get(); }

    void addChild(ActorTreeAbstract* node);
    void setChild(int i, ActorTreeAbstract* node);
    void eraseChild(int i, int count=1);
    void eraseAllChildren();

  public:
    std::vector< ref<ActorTreeAbstract> > mChildren;
  };
}

#endif
