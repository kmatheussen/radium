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

#include <vlGraphics/Actor.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// Actor
//-----------------------------------------------------------------------------
Actor::~Actor()
{
  dispatchOnActorDelete();
  deleteOcclusionQuery();
}
//-----------------------------------------------------------------------------
void Actor::setLODs(Renderable* lod0, Renderable* lod1, Renderable* lod2, Renderable* lod3, Renderable* lod4, Renderable* lod5)
{
  if (lod0) { VL_CHECK(0<VL_MAX_ACTOR_LOD) setLod(0,lod0); }
  if (lod1) { VL_CHECK(1<VL_MAX_ACTOR_LOD) setLod(1,lod1); }
  if (lod2) { VL_CHECK(2<VL_MAX_ACTOR_LOD) setLod(2,lod2); }
  if (lod3) { VL_CHECK(3<VL_MAX_ACTOR_LOD) setLod(3,lod3); }
  if (lod4) { VL_CHECK(4<VL_MAX_ACTOR_LOD) setLod(4,lod4); }
  if (lod5) { VL_CHECK(5<VL_MAX_ACTOR_LOD) setLod(5,lod5); }
}
//-----------------------------------------------------------------------------
int Actor::evaluateLOD(Camera* camera)
{
  if (mLODEvaluator)
    return mLODEvaluator->evaluate(this, camera);
  else
    return 0;
}
//-----------------------------------------------------------------------------
void Actor::createOcclusionQuery()
{
  VL_CHECK_OGL();
  if (!mOcclusionQuery && Has_Occlusion_Query)
    glGenQueries(1, &mOcclusionQuery);
  VL_CHECK_OGL();
  VL_CHECK(mOcclusionQuery)
}
//-----------------------------------------------------------------------------
void Actor::deleteOcclusionQuery()
{
  if(Has_Occlusion_Query)
  {
    if (mOcclusionQuery)
    {
      glDeleteQueries(1, &mOcclusionQuery);
      mOcclusionQuery = 0;
    }
  }
}
//-----------------------------------------------------------------------------
bool Actor::boundsDirty() const
{
  // (1) renderable dirty or we're not up to date with the renderable.
  bool dirty = lod(0)->boundsDirty() || lod(0)->boundsUpdateTick() != mBoundsUpdateTick;

  // (2) we're not not up to date with the transform.
  dirty |= transform() && transform()->worldMatrixUpdateTick() != mTransformUpdateTick;

  return dirty;
}
//-----------------------------------------------------------------------------
void Actor::computeBounds()
{
  if ( lod(0) == NULL )
    return;

  bool geom_update = lod(0)->boundsDirty() || lod(0)->boundsUpdateTick() != mBoundsUpdateTick;

  if ( transform() && (geom_update || transform()->worldMatrixUpdateTick() != mTransformUpdateTick) )
  {
    lod(0)->boundingBox().transformed( mAABB, transform()->worldMatrix() );
    mTransformUpdateTick = transform()->worldMatrixUpdateTick();
    mSphere = mAABB.isNull() ? Sphere() : mAABB;
    mBoundsUpdateTick = lod(0)->boundsUpdateTick();
  }
  else
  if (geom_update)
  {
    mAABB   = lod(0)->boundingBox();
    mSphere = mAABB.isNull() ? Sphere() : mAABB;
    mBoundsUpdateTick = lod(0)->boundsUpdateTick();
  }
}
//-----------------------------------------------------------------------------
void Actor::setUniform(Uniform* uniform) { gocUniformSet()->setUniform(uniform); }
//-----------------------------------------------------------------------------
const std::vector< ref<Uniform> >& Actor::uniforms() const { return getUniformSet()->uniforms(); }
//-----------------------------------------------------------------------------
std::vector< ref<Uniform> >& Actor::uniforms() { return gocUniformSet()->uniforms(); }
//-----------------------------------------------------------------------------
void Actor::eraseUniform(const char* name) { if(getUniformSet()) getUniformSet()->eraseUniform(name); }
//-----------------------------------------------------------------------------
void Actor::eraseUniform(const Uniform* uniform) { if(getUniformSet()) getUniformSet()->eraseUniform(uniform); }
//-----------------------------------------------------------------------------
void Actor::eraseAllUniforms() { if(getUniformSet()) getUniformSet()->eraseAllUniforms(); }
//-----------------------------------------------------------------------------
Uniform* Actor::gocUniform(const char* name) { return gocUniformSet()->gocUniform(name); }
//-----------------------------------------------------------------------------
Uniform* Actor::getUniform(const char* name) { if (getUniformSet()) return getUniformSet()->getUniform(name); else return NULL; }
//-----------------------------------------------------------------------------
const Uniform* Actor::getUniform(const char* name) const { if (getUniformSet()) return getUniformSet()->getUniform(name); else return NULL; }
//-----------------------------------------------------------------------------
