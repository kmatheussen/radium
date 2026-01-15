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

#ifndef SceneManagerVolumeTree_INCLUDE_ONCE
#define SceneManagerVolumeTree_INCLUDE_ONCE

#include <vlGraphics/SceneManager.hpp>

namespace vl
{
//-------------------------------------------------------------------------------------------------------------------------------------------
// SceneManagerBVH
//-------------------------------------------------------------------------------------------------------------------------------------------
  /**
   * The SceneManagerBVH class implements the basic functionalities for bounding-volume-hierarchy based scene managers.
   * 
   * \sa
   * - Actor
   * - ActorKdTree
   * - ActorTree
   * - SceneManager
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   * - SceneManagerPortals
   */
  template<class T>
  class VLGRAPHICS_EXPORT SceneManagerBVH: public SceneManager
  {
    VL_INSTRUMENT_CLASS(vl::SceneManagerBVH<T>, SceneManager)

  public:
    //! Sets the tree to be used by the scene manager.
    void setTree(T* bbh) { mBoundingVolumeTree = bbh; }
    //! Returns the tree used by the scene manager.
    const T* tree() const { return mBoundingVolumeTree.get(); }
    //! Returns the tree used by the scene manager.
    T* tree() { return mBoundingVolumeTree.get(); }

    virtual void extractVisibleActors(ActorCollection& list, const Camera* camera)
    {
      // extracts Actor[s] from the hierarchical volume tree
      if (cullingEnabled())
        tree()->extractVisibleActors(list, camera, enableMask());
      else
        extractActors(list);
    }

    virtual void extractActors(ActorCollection& list)
    {
      // extracts Actor[s] from the hierarchical volume tree
      tree()->extractActors(list);
    }

  protected:
    ref<T> mBoundingVolumeTree;
  };

}

#endif
