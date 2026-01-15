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

#ifndef Actor_INCLUDE_ONCE
#define Actor_INCLUDE_ONCE

#include <vlCore/Collection.hpp>
#include <vlCore/Sphere.hpp>
#include <vlCore/AABB.hpp>
#include <vlCore/Transform.hpp>
#include <vlGraphics/link_config.hpp>
#include <vlGraphics/Effect.hpp>
#include <vlGraphics/Renderable.hpp>
#include <vlGraphics/LODEvaluator.hpp>
#include <vlGraphics/UniformSet.hpp>
#include <vlGraphics/Scissor.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // ActorEventCallback
  //------------------------------------------------------------------------------
  /** The ActorEventCallback class defines a callback object to react to Actor-related events.

  Usually an ActorEventCallback is used to perform a per-Actor operation 
  like changing some attributes of the Actor itself or of the associated Renderable/Geometry.
  For example the MorphingCallback class is used to aid the rendering of a MorphingCallback, while the
  DepthSortCallback class is used to perform per-Actor polygon sorting.

  \note
  You can manipulate Uniforms within this class, for more information see vl::GLSLProgram documentation.
  If you want to update the state of a Uniform variable from here you can simply call glUniform* since the 
  GLSLProgram (if any) has been already activated by the time this function is called. 
  You can also modify the Actor's uniforms using the Actor's uniform manipulation routines Actor::setUniform()
  Actor::getUniform() etc.
  
  You can test whether the shader has a GLSLProgram bound to it or not by simply testing 
  shader->glslProgram() != NULL. If you update a uniform you must ensure that all the Actor[s] using the same 
  GLSLProgram appropriately setup such uniform.

  \note
  An ActorEventCallback::onActorRenderStarted() is called once for every rendering pass, ie. if an Actor's Effect specifies three
  rendering passes the Actor callbacks will be called three times, once for each rendering pass / shader.

  \sa
  - Actor::actorEventCallbacks() */
  class VLGRAPHICS_EXPORT ActorEventCallback: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::ActorEventCallback, Object)

  public:
    ActorEventCallback(): mEnabled(true) {}

    /** Event generated just before an Actor is rendered but after the render states are ready and setup.
    Reimplement to react to this event.
    \param actor The Actor bound to this rendering callback.
    \param frame_clock The current rendering frame time, usually used for animation purposes.
    \param cam The camera used for the current rendering.
    \param renderable The currently selected Actor LOD.
    \param shader The currently active Shader.
    \param pass The current Actor[s] rendering pass. */
    virtual void onActorRenderStarted(Actor* actor, real frame_clock, const Camera* cam, Renderable* renderable, const Shader* shader, int pass) = 0;

    /** Event notifying that an Actor is being deleted. */
    virtual void onActorDelete(Actor* actor) = 0;

    void setEnabled(bool enabled) { mEnabled = enabled; }

    bool isEnabled() const { return mEnabled; }

  protected:
    bool mEnabled;
  };

  //------------------------------------------------------------------------------
  // Actor
  //------------------------------------------------------------------------------
  /** Associates a Renderable object to an Effect and Transform.
  An Actor can associate one Renderable for each LOD (level of detail) using
  the lods() function.
  The rendering order of an Actor is defined by its rendering rank, and block,
  see setRenderRank() and setRenderBlock() for the details.

  \note

  - The same Renderable can be bound to more than one Actor at the same time
  - The same Effect     can be bound to more than one Actor at the same time
  - The same Transform  can be bound to more than one Actor at the same time

  \remarks

  An Actor must always have a Renderable and Effect bound. If no Transform is 
  specified the Renderable will be rendered as if it had an identity matrix
  transformation.
  \par Uniforms

  The Uniforms defined in the Actor and the ones defined in the Shader must not
  overlap, that is, an Actor must not define Uniforms that are also present 
  in the Shader's Uniform and vice versa.

  \sa Transform, Effect, Renderable, Geometry
  */
  class VLGRAPHICS_EXPORT Actor: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Actor, Object)

  public:
    /** Constructor.
    \param renderable A Renderable defining the Actor's LOD level #0
    \param effect The Effect to be used by the Actor
    \param transform The Transform to be used by the Actor
    \param block The rendering block to which the Actor belongs
    \param rank The rendering rank to which the Actor belongs
    */
    Actor(Renderable* renderable = NULL, Effect* effect = NULL, Transform* transform = NULL, int block = 0, int rank = 0):
      mEffect(effect), mTransform(transform), mRenderBlock(block), mRenderRank(rank),
      mTransformUpdateTick(-1), mBoundsUpdateTick(-1), mEnableMask(0xFFFFFFFF), mOcclusionQuery(0), mOcclusionQueryTick(0xFFFFFFFF), mIsOccludee(true)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mActorEventCallbacks.setAutomaticDelete(false);
      setLod(0,renderable);
      // actor user data
      #if VL_ACTOR_USER_DATA
        mActorUserData = NULL;
      #endif
    }

    //! Destructor.
    virtual ~Actor();

    /** Sets the Renderable object representing the LOD level specifed by \p lod_index. */
    void setLod(int lod_index, Renderable* renderable) 
    { 
      mRenderables[lod_index] = renderable;
      
      // schedule update of the Actor's bounds.
      if (lod_index == 0)
      {
        mBoundsUpdateTick = -1;
        mAABB.setNull();
        mSphere.setNull();
      }
    }

    /** Returns the Renderable object representing the LOD level specifed by \p lod_index. */
    const Renderable* lod(int lod_index) const { return mRenderables[lod_index].get(); }

    /** Returns the Renderable object representing the LOD level specifed by \p lod_index. */
    Renderable* lod(int lod_index) { return mRenderables[lod_index].get(); }

    /** Utility function to assign one or more Renderable[s] to one or more LOD levels. */
    void setLODs(Renderable* lod0, Renderable* lod1=NULL, Renderable* lod2=NULL, Renderable* lod3=NULL, Renderable* lod4=NULL, Renderable* lod5=NULL);

    /** Binds a Transform to an Actor */
    void setTransform(Transform* transform)
    {
      mTransform = transform;
      mTransformUpdateTick = -1;
      mBoundsUpdateTick    = -1;
    }
    
    /** Returns the Transform bound tho an Actor */
    Transform* transform()  { return mTransform.get(); }
    
    /** Returns the Transform bound tho an Actor */
    const Transform* transform() const { return mTransform.get(); }

    /** Binds an Effect to an Actor */
    void setEffect(Effect* effect) { mEffect = effect; }
    
    /** Returns the Effect bound to an Actor */
    Effect* effect() { return mEffect.get(); }
    
    /** Returns the Effect bound to an Actor */
    const Effect* effect() const { return mEffect.get(); }

    /** Returns the bounding box (\p not guaranteed to be up to date) that contains this Actor. \sa boundingBoxSafe() */
    const AABB& boundingBox() const { return mAABB; }

    /** Returns the bounding sphere (\p not guaranteed to be up to date) that contains this Actor. \sa boundingSphereSafe() */
    const Sphere& boundingSphere() const { return mSphere; }

    /** Returns the bounding box (\p guaranteed to be up to date) that contains this Actor. \sa boundingBox() */
    const AABB& boundingBoxSafe() { computeBounds(); return mAABB; }

    /** Returns the bounding sphere (\p guaranteed to be up to date) that contains this Actor. \sa boundingSphere() */
    const Sphere& boundingSphereSafe() { computeBounds(); return mSphere; }

    /** Computes the bounding box and bounding sphere of an Actor. */
    void computeBounds();

    /** Returns whether the Actor's bounding box and sphere are up to date. */
    bool boundsDirty() const;

    /** Modifies the rendering rank of an Actor.

    The rendering rank affects the order in which an Actor is rendered, the greater the rank the later the Actor is rendered. 
    The default render rank is zero. 

    To know more about rendering order please see \ref pagGuideRenderOrder "Rendering Order".

    \sa setRenderBlock(), Effect::setRenderRank()
    */
    void setRenderRank(int rank) { mRenderRank = rank; }

    /**
    Modifies the rendering block of an Actor.

    The rendering block affects the order in which an Actor is rendered, the greater the block the later the Actor is rendered. 
    The default render block is zero.

    To know more about rendering order please see \ref pagGuideRenderOrder "Rendering Order".

    \sa setRenderRank(), Effect::setRenderRank()
    */
    void setRenderBlock(int block) { mRenderBlock = block; }

    /** Returns the rendering rank of an Actor. */
    int renderRank() const { return mRenderRank; }

    /** Returns the rendering block of an Actor. */
    int renderBlock() const { return mRenderBlock; }

    /** Installs the LODEvaluator used to compute the current LOD at rendering time. */
    void setLODEvaluator(LODEvaluator* lod_evaluator) { mLODEvaluator = lod_evaluator; }

    /** Returns the installed LODEvaluator (if any) or NULL. */
    LODEvaluator* lodEvaluator() { return mLODEvaluator.get(); }

    /** Returns the installed LODEvaluator (if any) or NULL. */
    const LODEvaluator* lodEvaluator() const { return mLODEvaluator.get(); }

    int evaluateLOD(Camera* camera);

    /** The enable mask of an Actor is usually used to defines whether the actor should be rendered or not 
      * depending on the Rendering::enableMask() but it can also be used for user-specific tasks (set to 0xFFFFFFFF by default).
      * See also vl::Rendering::effectOverrideMask() and vl::Renderer::shaderOverrideMask(). */
    void setEnableMask(unsigned int mask) { mEnableMask = mask; }

    /** The enable mask of an Actor is usually used to defines whether the actor should be rendered or not 
      * depending on the Rendering::enableMask() but it can also be used for user-specific tasks (set to 0xFFFFFFFF by default).
      * See also vl::Rendering::effectOverrideMask() and vl::Renderer::shaderOverrideMask(). */
    unsigned int enableMask() const { return mEnableMask; }

    // uniforms methods

    /** Equivalent to getUniformSet()->setUniform(uniform)
     \remarks
     This function performs a 'setUniformSet(new UniformSet)' if getUniformSet() is NULL. */
    void setUniform(Uniform* uniform);

    /** Equivalent to getUniformSet()->uniforms()
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    const std::vector< ref<Uniform> >& uniforms() const;

    /** Equivalent to gocUniformSet()->uniforms()
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    std::vector< ref<Uniform> >& uniforms();

    /** Equivalent to getUniformSet()->eraseUniform(name)
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    void eraseUniform(const char* name);

    /** Equivalent to getUniformSet()->eraseUniform(uniform)
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    void eraseUniform(const Uniform* uniform);

    /** Equivalent to getUniformSet()->eraseAllUniforms()
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    void eraseAllUniforms();

    /** Equivalent to getUniformSet()->getUniform(name, get_mode)
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    Uniform* gocUniform(const char* name);

    /** Equivalent to getUniformSet()->getUniform(name, get_mode)
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    Uniform* getUniform(const char* name);
    
    /** Equivalent to getUniformSet()->getUniform(name, get_mode)
     \remarks
     You must install a UniformSet with setUniformSet() before calling this function. */
    const Uniform* getUniform(const char* name) const;

    /** Installs a new UniformSet
     \sa
     - setUniform()
     - uniforms()
     - eraseUniform(const char* name)
     - eraseUniform(const Uniform* uniform)
     - eraseAllUniforms()
     - getUniform() */
    void setUniformSet(UniformSet* uniforms) { mUniformSet = uniforms; }

    /** Returns the installed UniformSet 
     \sa
     - setUniform()
     - uniforms()
     - eraseUniform(const char* name)
     - eraseUniform(const Uniform* uniform)
     - eraseAllUniforms()
     - getUniform()
    */
    const UniformSet* getUniformSet() const { return mUniformSet.get(); }
    
    /** Returns the installed UniformSet 
     \sa
     - setUniform()
     - uniforms()
     - eraseUniform(const char* name)
     - eraseUniform(const Uniform* uniform)
     - eraseAllUniforms()
     - getUniform()
    */
    UniformSet* getUniformSet() { return mUniformSet.get(); }

    /** Creates and/or returns the installed UniformSet 
     \sa
     - setUniform()
     - uniforms()
     - eraseUniform(const char* name)
     - eraseUniform(const Uniform* uniform)
     - eraseAllUniforms()
     - getUniform()
    */
    UniformSet* gocUniformSet() { if (!mUniformSet) mUniformSet = new UniformSet; return mUniformSet.get(); }

    /** Returns the list of ActorEventCallback bound to an Actor. */
    const Collection<ActorEventCallback>* actorEventCallbacks() const { return &mActorEventCallbacks; }

    /** Returns the list of ActorEventCallback bound to an Actor. */
    Collection<ActorEventCallback>* actorEventCallbacks() { return &mActorEventCallbacks; }

    /** Calls all the onActorRenderStarted() of all the ActorEventCallback installed on this Actor. */
    void dispatchOnActorRenderStarted( real frame_clock, const Camera* camera, Renderable* renderable, const Shader* shader, int pass)
    {
      for(int i=0; i<actorEventCallbacks()->size(); ++i)
      {
        ActorEventCallback& cb = *actorEventCallbacks()->at(i);
        if (cb.isEnabled())
          cb.onActorRenderStarted(this, frame_clock, camera, renderable, shader, pass);
      }
    }

    /** Calls all the onActorDelete() of all the ActorEventCallback installed on this Actor. */
    void dispatchOnActorDelete()
    {
      for(int i=0; i<actorEventCallbacks()->size(); ++i)
      {
        ActorEventCallback& cb = *actorEventCallbacks()->at(i);
        cb.onActorDelete(this);
      }
    }

    /** Sets the Scissor to be used when rendering an Actor.
     \note
     You can also define a Scissor on a per-Shader basis using the function Shader::setScissor(). 
     In case both the Shader's and the Actor's Scissor are defined the Actor's Scissor is used.
     \sa
     - Scissor
     - Shader::setScissor()
    */
    void setScissor(Scissor* scissor) { mScissor = scissor; }
    /** Returns the Scissor used when rendering an Actor.
     \sa
     - Scissor
     - Actor::setScissor()
     - Shader::setScissor()
    */
    const Scissor* scissor() const { return mScissor.get(); }
    
    /** Returns the Scissor used when rendering an Actor.
     \sa
     - Scissor
     - Actor::setScissor()
     - Shader::setScissor()
    */
    Scissor* scissor() { return mScissor.get(); }

    /** If \p is_occludee equals true an occlusion test will be performed before the rendering of the Actor (if occlusion culling is enabled)
    otherwise the Actor will always be rendered with no occlusion test even when occlusion culling is enabled. */
    void setOccludee(bool is_occludee) { mIsOccludee = is_occludee; }

    /** If \p is_occludee equals true an occlusion test will be performed before the rendering of the Actor (if occlusion culling is enabled)
    otherwise the Actor will always be rendered with no occlusion test even when occlusion culling is enabled. */
    bool isOccludee() const { return mIsOccludee; }

    /** For internal use only.
    Creates the occlusion query object name bound this Actor using the OpenGL function glGenQueries(). */
    void createOcclusionQuery();

    /** For internal use only.
    Deletes the occlusion query object name using the OpenGL function glDeleteQueries(). */
    void deleteOcclusionQuery();

    /** For internal use only.
    Returns the occlusion query object name bound this Actor as by the OpenGL function glGenQueries(). Returns 0 if no query object name has been created yet. */
    GLuint occlusionQuery() const { return mOcclusionQuery; }

    /** For internal use only. */
    void setOcclusionQueryTick(unsigned tick) { mOcclusionQueryTick = tick; }

    /** For internal use only. */
    unsigned occlusionQueryTick() const { return mOcclusionQueryTick; }

#if VL_ACTOR_USER_DATA
  public:
    void* actorUserData() { return mActorUserData; }
    const void* actorUserData() const { return mActorUserData; }
    void setActorUserData(void* user_data) { mActorUserData = user_data; }

  private:
    void* mActorUserData;
#endif

  protected:
    AABB mAABB;
    Sphere mSphere;
    ref<Effect> mEffect;
    ref<Renderable> mRenderables[VL_MAX_ACTOR_LOD];
    ref<Transform> mTransform;
    ref<LODEvaluator> mLODEvaluator;
    ref<UniformSet> mUniformSet;
    ref<Scissor> mScissor;
    Collection<ActorEventCallback> mActorEventCallbacks;
    int mRenderBlock;
    int mRenderRank;
    long long mTransformUpdateTick;
    long long mBoundsUpdateTick;
    unsigned int mEnableMask;
    GLuint mOcclusionQuery;
    unsigned mOcclusionQueryTick;
    bool mIsOccludee;
  };
  //---------------------------------------------------------------------------
  /** Defined as a simple subclass of Collection<Actor>, see Collection for more information. */
  class ActorCollection: public Collection<Actor> {};
  //---------------------------------------------------------------------------
}

#endif
