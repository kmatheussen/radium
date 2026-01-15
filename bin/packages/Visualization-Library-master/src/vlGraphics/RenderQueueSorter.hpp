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

#ifndef RenderQueueSorter_INCLUDE_ONCE
#define RenderQueueSorter_INCLUDE_ONCE

#include <vlGraphics/RenderToken.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // RenderQueueSorter
  //------------------------------------------------------------------------------
  /**
   * The RenderQueueSorter class is the abstract base class of all the algorithms used to sort a set of RenderToken.
  */
  class RenderQueueSorter: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::RenderQueueSorter, Object)

  public:
    RenderQueueSorter()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual bool operator()(const RenderToken* a, const RenderToken* b) const = 0;
    virtual bool confirmZCameraDistanceNeed(const RenderToken*) const = 0;
    virtual bool mightNeedZCameraDistance() const = 0;
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterByShader
  //------------------------------------------------------------------------------
  //! Sorts the RenderTokens by their Shader pointer.
  // no z sort, no effect render rank, no actor render rank
  class RenderQueueSorterByShader: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterByShader, RenderQueueSorter)

  public:
    RenderQueueSorterByShader()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual bool mightNeedZCameraDistance() const { return false; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken*) const { return false; }
    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      return a->mShader < b->mShader;
    }
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterByRenderable
  //------------------------------------------------------------------------------
  //! Sorts the RenderTokens by their Renderable pointer.
  // no z sort, no effect render rank, no actor render rank
  class RenderQueueSorterByRenderable: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterByRenderable, RenderQueueSorter)

  public:
    RenderQueueSorterByRenderable()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual bool mightNeedZCameraDistance() const { return false; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken*) const { return false; }
    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      return a->mRenderable < b->mRenderable;
    }
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterBasic
  //------------------------------------------------------------------------------
  //! Sorts the RenderTokens by their Effect rank -> Actor rank -> Shader pointer -> Renderable pointer
  class RenderQueueSorterBasic: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterBasic, RenderQueueSorter)

  public:
    RenderQueueSorterBasic()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    virtual bool mightNeedZCameraDistance() const { return true; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken*) const { return false; }
    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      //  Actor's render-block
      if (a->mActor->renderBlock() != b->mActor->renderBlock())
        return a->mActor->renderBlock() < b->mActor->renderBlock();
      else
      // Effect's render-rank
      if (a->mEffectRenderRank != b->mEffectRenderRank)
        return a->mEffectRenderRank < b->mEffectRenderRank;
      else
      //  Actor's render-rank
      if (a->mActor->renderRank() != b->mActor->renderRank())
        return a->mActor->renderRank() < b->mActor->renderRank();
      else
      // shader sorting
      if (a->mShader != b->mShader)
        return a->mShader < b->mShader;
      // renderable sorting
      else
        return a->mRenderable < b->mRenderable;
    }
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterStandard
  //------------------------------------------------------------------------------
  //! Implements the default RenderQueueSorter.
  //! Sorts the RenderTokens by:
  //! -# Actor render block
  //! -# Effect render rank
  //! -# Actor render rank
  //! -# Solid objects first, translucent objects last
  //! -# Sort translucent objects back-to-front
  //! -# Sort solid objects by Shader
  //! -# Sort solid objects by Renderable
  class RenderQueueSorterStandard: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterStandard, RenderQueueSorter)

  public:
    RenderQueueSorterStandard(): mDepthSortMode(AlphaDepthSort)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    virtual bool mightNeedZCameraDistance() const { return true; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken* a) const
    { 
      return mDepthSortMode != NeverDepthSort && (mDepthSortMode == AlwaysDepthSort  ||
      (a->mShader->isBlendingEnabled() && (mDepthSortMode == AlphaDepthSort)) ); 
    }

    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      // --------------- user defined sorting ---------------

      //  Actor's render-block
      if (a->mActor->renderBlock() != b->mActor->renderBlock())
        return a->mActor->renderBlock() < b->mActor->renderBlock();
      else
      // Effect's render-rank
      if (a->mEffectRenderRank != b->mEffectRenderRank)
        return a->mEffectRenderRank < b->mEffectRenderRank;
      else
      //  Actor's render-rank
      if (a->mActor->renderRank() != b->mActor->renderRank())
        return a->mActor->renderRank() < b->mActor->renderRank();
      else

      // --------------- shader based sorting ---------------

      if ( mDepthSortMode != AlwaysDepthSort && (a->mShader->isBlendingEnabled() != b->mShader->isBlendingEnabled()))
      {
        // first render opaque objects
        return !a->mShader->isBlendingEnabled();
      }
      // A/b->mShader->isEnabled(OGL_BLEND) are equal or AlwaysDepthSort
      else
      if ( confirmZCameraDistanceNeed(a) )
      {
        // render first far objects then the close ones
        return a->mCameraDistance > b->mCameraDistance;
      }
      else

      /*// glsl
      if ( a->mShader->glslProgram() != b->mShader->glslProgram() )
        return a->mShader->glslProgram() < b->mShader->glslProgram();
      else*/

      /*sort by textures: removed*/

      // shader sorting
      if (a->mShader != b->mShader)
        return a->mShader < b->mShader;
      // renderable sorting
      else
        return a->mRenderable < b->mRenderable;
    }

    EDepthSortMode depthSortMode() const { return mDepthSortMode; }
    void setDepthSortMode(EDepthSortMode mode) { mDepthSortMode = mode; }

  public:
    EDepthSortMode mDepthSortMode;
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterOcclusion
  //------------------------------------------------------------------------------
  //! Implements a RenderQueueSorter that maximizes the z-buffer test efficiency as much as possible.
  //! Sorts the RenderTokens by:
  //! -# Actor render block
  //! -# Effect render rank
  //! -# Actor render rank
  //! -# Solid objects first, translucent objects last
  //! -# Sort solid objects front-to-back
  //! -# Sort translucent objects back-to-front
  class RenderQueueSorterOcclusion: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterOcclusion, RenderQueueSorter)

  public:
    RenderQueueSorterOcclusion()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    virtual bool mightNeedZCameraDistance() const { return true; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken*) const { return true; }

    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      // --------------- user defined sorting ---------------

      //  Actor's render-block
      if (a->mActor->renderBlock() != b->mActor->renderBlock())
        return a->mActor->renderBlock() < b->mActor->renderBlock();
      else
      // Effect's render-rank
      if (a->mEffectRenderRank != b->mEffectRenderRank)
        return a->mEffectRenderRank < b->mEffectRenderRank;
      else
      //  Actor's render-rank
      if (a->mActor->renderRank() != b->mActor->renderRank())
        return a->mActor->renderRank() < b->mActor->renderRank();
      else

      // --------------- shader based sorting ---------------

      if ( a->mShader->isBlendingEnabled() != b->mShader->isBlendingEnabled() )
      {
        // first render opaque objects
        return !a->mShader->isBlendingEnabled();
      }
      else
      // blending on: render first far objects then the close ones
      if ( a->mShader->isBlendingEnabled() )
      {
        return a->mCameraDistance > b->mCameraDistance;
      }
      else
      // blending off: render first close objects then far ones -> i.e. -> if ( !a->mShader->isBlendingEnabled() )
      if (a->mCameraDistance != b->mCameraDistance)
      {
        return a->mCameraDistance < b->mCameraDistance;
      }
      else
      // shader sorting
      if (a->mShader != b->mShader)
        return a->mShader < b->mShader;
      // renderable sorting
      else
        return a->mRenderable < b->mRenderable;
    }
  };
  //------------------------------------------------------------------------------
  // RenderQueueSorterAggressive
  //------------------------------------------------------------------------------
  //! Sorts the RenderTokens by Effect rank -> Actor rank -> blending on/off -> Z distance form the Camera -> 
  //! GLSL program -> render state set -> enable set -> texture set -> light set -> Shader pointer -> Renderable pointer
  class RenderQueueSorterAggressive: public RenderQueueSorter
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueueSorterAggressive, RenderQueueSorter)

  public:
    RenderQueueSorterAggressive(): mDepthSortMode(AlphaDepthSort)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    virtual bool mightNeedZCameraDistance() const { return true; }
    virtual bool confirmZCameraDistanceNeed(const RenderToken* a) const
    {
      return mDepthSortMode != NeverDepthSort &&
        (mDepthSortMode == AlwaysDepthSort  || (a->mShader->isBlendingEnabled() && (mDepthSortMode == AlphaDepthSort)) );
    }

    virtual bool operator()(const RenderToken* a, const RenderToken* b) const
    {
      // --------------- user defined sorting ---------------

      //  Actor's render-block
      if (a->mActor->renderBlock() != b->mActor->renderBlock())
        return a->mActor->renderBlock() < b->mActor->renderBlock();
      else
      // Effect's render-rank
      if (a->mEffectRenderRank != b->mEffectRenderRank)
        return a->mEffectRenderRank < b->mEffectRenderRank;
      else
      //  Actor's render-rank
      if (a->mActor->renderRank() != b->mActor->renderRank())
        return a->mActor->renderRank() < b->mActor->renderRank();
      else

      // --------------- shader based sorting ---------------

      if ( mDepthSortMode != AlwaysDepthSort && (a->mShader->isBlendingEnabled() != b->mShader->isBlendingEnabled()))
      {
        return !a->mShader->isBlendingEnabled(); // first render opaque objects
      }
      else // A/b->mShader->isEnabled(OGL_BLEND) are equal or AlwaysDepthSort
      if ( confirmZCameraDistanceNeed(a) )
      {
        return a->mCameraDistance > b->mCameraDistance; // render first far objects then the close ones
      }
      else

      // glsl
      if ( a->mShader->glslProgram() != b->mShader->glslProgram() )
        return a->mShader->glslProgram() < b->mShader->glslProgram();
      else

      // render state set
      if ( a->mShader->getRenderStateSet() != b->mShader->getRenderStateSet() )
        return a->mShader->getRenderStateSet() < b->mShader->getRenderStateSet();
      else

      // enables
      if ( a->mShader->getEnableSet() != b->mShader->getEnableSet() )
        return a->mShader->getEnableSet() < b->mShader->getEnableSet();
      else

      /*sort by textures: removed*/

      /*sort by lights: removed*/

      // shader sorting
      if (a->mShader != b->mShader)
        return a->mShader < b->mShader;
      // renderable sorting
      else
        return a->mRenderable < b->mRenderable;
    }

    EDepthSortMode depthSortMode() const { return mDepthSortMode; }
    void setDepthSortMode(EDepthSortMode mode) { mDepthSortMode = mode; }

  public:
    EDepthSortMode mDepthSortMode;
  };
}

#endif
