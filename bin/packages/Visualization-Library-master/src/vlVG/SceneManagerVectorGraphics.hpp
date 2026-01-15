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

#ifndef SceneManagerVectorGraphics_INCLUDE_ONCE
#define SceneManagerVectorGraphics_INCLUDE_ONCE

#include <vlGraphics/SceneManager.hpp>
#include <vlCore/Collection.hpp>
#include <vlGraphics/Frustum.hpp>

namespace vl
{
  class VectorGraphics;
//-------------------------------------------------------------------------------------------------------------------------------------------
// SceneManagerVectorGraphics
//-------------------------------------------------------------------------------------------------------------------------------------------
  /** The SceneManagerVectorGraphics class is a SceneManager that contains VectorGraphics objects.
   * The VectorGraphics objects are rendered in the order in which they are added to the SceneManagerVectorGraphics.
   * \sa
   * - Actor
   * - ActorKdTree
   * - ActorTree
   * - SceneManager
   * - SceneManagerBVH
   * - SceneManagerActorKdTree
   * - SceneManagerActorTree
   * - SceneManagerPortals */
  class SceneManagerVectorGraphics: public SceneManager
  {
    VL_INSTRUMENT_CLASS(vl::SceneManagerVectorGraphics, SceneManager)

  public:
    SceneManagerVectorGraphics() { mActorRenderRankStart = 0; mVectorGraphicObjects.setAutomaticDelete(false); }

    /** Defines the Actor's render rank to be used when extracting them from the scene manager during the rendering.
     * During the rendering when the Actor[s] are extracted they are assigned a progressive render rank starting from
     * rank_start so that they are rendered in the same order in which they were created by their VectorGraphics. Also
     * the order in which a VectorGraphics is inserted in the SceneManagerVectorGraphics determines the rendering order. */
    void setActorRenderRankStart(int rank_start) { mActorRenderRankStart = rank_start; }

    //! Returns the rendering rank start value used during the rendering of the VectorGraphics objects. See setActorRenderRankStart() for more information.
    int actorRenderRankStart() const { return mActorRenderRankStart; }

    virtual void extractVisibleActors(ActorCollection& queue, const Camera*)
    {
      if (cullingEnabled())
        // FIXME: implement 2d culling?
        extractActors(queue);
      else
        extractActors(queue);
    }

    virtual void extractActors(ActorCollection& queue)
    {
      int actor_rank = mActorRenderRankStart;
      for(int i=0; i<vectorGraphicObjects()->size(); ++i)
      {
        for(int j=0; j<vectorGraphicObjects()->at(i)->actors()->size(); ++j)
        {
          vectorGraphicObjects()->at(i)->actors()->at(j)->setRenderRank( actor_rank++ );
          queue.push_back( vectorGraphicObjects()->at(i)->actors()->at(j) );
        }
      }
    }

    //! Returns the list of VectorGraphics objects bound to a SceneManagerVectorGraphics
    Collection<VectorGraphics>* vectorGraphicObjects() { return &mVectorGraphicObjects; }

    //! Returns the list of VectorGraphics objects bound to a SceneManagerVectorGraphics
    const Collection<VectorGraphics>* vectorGraphicObjects() const { return &mVectorGraphicObjects; }

  protected:
    Collection<VectorGraphics> mVectorGraphicObjects;
    int mActorRenderRankStart;
  };
//-------------------------------------------------------------------------------------------------------------------------------------------
}

#endif
