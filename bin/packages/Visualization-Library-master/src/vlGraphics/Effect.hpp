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

#ifndef Effect_INCLUDE_ONCE
#define Effect_INCLUDE_ONCE

#include <vlGraphics/Renderable.hpp>
#include <vlGraphics/LODEvaluator.hpp>
#include <vlGraphics/Shader.hpp>
#include <vlCore/Collection.hpp>
#include <vector>

namespace vl
{
  class Actor;
  //------------------------------------------------------------------------------
  // ShaderPasses
  //------------------------------------------------------------------------------
  /** A sequence of Shader objects each of which represent a rendering pass.
   Each LOD (level of detail) in an Effect corresponts a ShaderPasses.
   \sa Shader, Effect, Actor
  */
  class ShaderPasses: public Collection<Shader>
  {
    VL_INSTRUMENT_CLASS(vl::ShaderPasses, Collection<Shader>)

  public:
    /** Constructor. 
    \param pass1 The Shader (if any) to be used for pass #1
    \param pass2 The Shader (if any) to be used for pass #2
    \param pass3 The Shader (if any) to be used for pass #3
    \param pass4 The Shader (if any) to be used for pass #4
    */
    ShaderPasses(Shader* pass1=NULL, Shader* pass2=NULL, Shader* pass3=NULL, Shader* pass4=NULL )
    {
      VL_DEBUG_SET_OBJECT_NAME()
      if (pass1)
        push_back(pass1);
      if (pass2)
        push_back(pass2);
      if (pass3)
        push_back(pass3);
      if (pass4)
        push_back(pass4);
    }
  };
  //------------------------------------------------------------------------------
  // Effect
  //------------------------------------------------------------------------------
  /** Defines the sequence of Shader objects used to render an Actor.
   Typically an Effect will have only one LOD (level of detail) with 1 pass 
   (i.e. 1 Shader) but you can specify multiple LODs each of which defines its 
   own set of Shader[s].

   When a LOD has more than one Shader the Actor is rendered several times, once
   for each Shader. This technique is called multipass rendering.

   The LOD to be used during the rendering is defined at rendering time if a 
   LODEvaluator has been installed using the method setLODEvaluator(), otherwise 
   the LOD #0 is selected.
   \sa Shader, Actor, LODEvaluator, ShaderPasses
  */
  class VLGRAPHICS_EXPORT Effect: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Effect, Object)

    // use deepCopy() and shallowCopy() instead
    Effect(const Effect&): Object() {}
    Effect& operator=(const Effect&) { return *this; }

  public:
    /** Constructor. */
    Effect() 
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      mEnableMask = 0xFFFFFFFF;
      mRenderRank = 0;
      mActiveLod  = 0;
      mLODShaders[0] = new ShaderPasses(new Shader);
    }

    ref<Effect> shallowCopy(EShaderCopyMode shader_copy) const
    {
      ref<Effect> fx = new Effect;
      fx->shallowCopyFrom(*this, shader_copy);
      return fx;
    }

    Effect& shallowCopyFrom(const Effect& other, EShaderCopyMode shader_copy)
    {
      for(int i=0; i<VL_MAX_EFFECT_LOD; ++i)
        mLODShaders[i] = other.mLODShaders[i];

      if (shader_copy == SCM_OwnShaders)
      {
        // create local shallow copies of all the Shaders
        for(int lod=0; lod<VL_MAX_EFFECT_LOD; ++lod)
          for(int pass=0; mLODShaders[lod] && pass<mLODShaders[lod]->size(); ++pass)
            (*mLODShaders[lod])[pass] = (*mLODShaders[lod])[pass]->shallowCopy();
      }

      mLODEvaluator = other.mLODEvaluator;

      mActiveLod = other.mActiveLod;
      mRenderRank = other.mRenderRank;
      mEnableMask = other.mEnableMask;

      return *this;
    }

    ref<Effect> deepCopy() const
    {
      ref<Effect> fx = new Effect;
      fx->deepCopyFrom(*this);
      return fx;
    }

    Effect& deepCopyFrom(const Effect& other)
    {
      shallowCopyFrom(other, SCM_ShareShaders);

      // create local clones of all the Shaders
      for(int lod=0; lod<VL_MAX_EFFECT_LOD; ++lod)
        for(int pass=0; mLODShaders[lod] && pass<mLODShaders[lod]->size(); ++pass)
          (*mLODShaders[lod])[pass] = (*mLODShaders[lod])[pass]->deepCopy();

      return *this;
    }

    /** Modifies the rendering rank of an Actor.
      * The rendering rank affects the order in which an Actor is rendered, the greater the rank the later the Actor is rendered. The default render rank is zero.
      *
      * To know more about rendering order please see \ref pagGuideRenderOrder "Rendering Order".
      *
      * \sa Actor::setRenderRank(), Actor::setRenderBlock() */
    void setRenderRank(int rank) { mRenderRank = rank; }

    /** Returns the rendering rank of an Effect. */
    int renderRank() const { return mRenderRank; }

    /** Returns the ShaderPasses representing the specified LOD level.
      * \note It must be: 0 <= \p lod_level < VL_MAX_EFFECT_LOD. */
    const ref<ShaderPasses>& lod(int lod_level) const { return mLODShaders[lod_level]; }
    
    /** Returns the ShaderPasses representing the specified LOD level.
      * \note It must be: 0 <= \p lod_level < VL_MAX_EFFECT_LOD. */
    ref<ShaderPasses>& lod(int lod_level) { return mLODShaders[lod_level]; }

    /** Utility function, same as \p 'lod(lodi)->at(pass);' */
    Shader* shader(int lodi=0, int pass=0) { return lod(lodi)->at(pass); }
    
    /** Utility function, same as \p 'lod(lodi)->at(pass);' */
    const Shader* shader(int lodi=0, int pass=0) const { return lod(lodi)->at(pass); }
    
    /** Utility function, same as \p 'lod(lodi) = new ShaderPasses(shader1,shader2,shader3,shader4);' */
    void setLOD(int lodi, Shader* shader1, Shader* shader2=NULL, Shader* shader3=NULL, Shader* shader4=NULL) 
    { 
      VL_CHECK(lodi<VL_MAX_EFFECT_LOD)
      lod(lodi) = new ShaderPasses(shader1,shader2,shader3,shader4);
    }

    /** Installs the LODEvaluator used to compute the current LOD at rendering time. */
    void setLODEvaluator(LODEvaluator* lod_evaluator) { mLODEvaluator = lod_evaluator; }

    /** Returns the installed LODEvaluator (if any) or NULL. */
    LODEvaluator* lodEvaluator() { return mLODEvaluator.get(); }

    /** Returns the installed LODEvaluator (if any) or NULL. */
    const LODEvaluator* lodEvaluator() const { return mLODEvaluator.get(); }

    /** The enable mask of an Actor's Effect defines whether the actor should be rendered or not depending on the Rendering::enableMask(). */
    void setEnableMask(unsigned int mask) { mEnableMask = mask; }

    /** The enable mask of an Actor's Effect defines whether the actor should be rendered or not depending on the Rendering::enableMask(). */
    unsigned int enableMask() const { return mEnableMask; }

    /** If a LODEvaluator is installed computes the effect LOD to be used otherwise returns 0. */
    int evaluateLOD(Actor* actor, Camera* camera);

    /** Sets the lod to be used for rendering. It must be: 0 <= lod < VL_MAX_EFFECT_LOD. */
    void setActiveLod(int lod) 
    { 
      VL_CHECK( lod < VL_MAX_EFFECT_LOD )
      VL_CHECK( lod >= 0 )
      mActiveLod = lod; 
    }

    /** Returns the lod to be used for rendering. */
    int activeLod() const { return mActiveLod; }

  protected:
    ref<ShaderPasses> mLODShaders[VL_MAX_EFFECT_LOD];
    ref<LODEvaluator> mLODEvaluator;
    int mActiveLod;
    int mRenderRank;
    unsigned int mEnableMask;
  };
  //------------------------------------------------------------------------------
}

#endif
