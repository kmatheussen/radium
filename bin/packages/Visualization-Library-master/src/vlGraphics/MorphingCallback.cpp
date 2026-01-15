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

#include <vlGraphics/MorphingCallback.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlGraphics/GLSL.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
// MorphingCallback
//-----------------------------------------------------------------------------
MorphingCallback::MorphingCallback()
{
  VL_DEBUG_SET_OBJECT_NAME()

  mGeometry = new Geometry;
  setAnimation(0,0,0);
  resetGLSLBindings();
  setGLSLVertexBlendEnabled(false);

  mAnim_t = 0.0f;
  mFrame1 = -1;
  mFrame2 = -1;
  mLastUpdate = -1;
}
//-----------------------------------------------------------------------------
MorphingCallback::~MorphingCallback()
{
}
//-----------------------------------------------------------------------------
void MorphingCallback::onActorRenderStarted(Actor*, real frame_clock, const Camera*, Renderable*, const Shader* shader, int pass)
{
  // perform only on the first pass
  if (pass>0)
    return;

  if (!mAnimationStarted)
    return;

  mElapsedTime = frame_clock - mAnimationStartTime;
  // 30 fps update using the CPU vertex blending or continuous update if using the GPU
  bool do_update = mLastUpdate == -1 || (mElapsedTime - mLastUpdate) > 1.0f/30.0f || glslVertexBlendEnabled();
  if ( do_update )
  {
    mLastUpdate = mElapsedTime;
    real ft = mElapsedTime / mAnimationPeriod;
    ft = ft - (int)ft;
    int frame_count = mAnimationEnd - mAnimationStart + 1;
    mAnim_t  = (float)(ft * frame_count - (int)(ft * frame_count));
    mFrame1 = (int)(ft * frame_count);
    mFrame2 = (mFrame1 + 1) % frame_count;
    mFrame1 += mAnimationStart;
    mFrame2 += mAnimationStart;
    VL_CHECK(mFrame1 >= 0)
    VL_CHECK(mLastUpdate>=0)
  }

  VL_CHECK(mFrame1 != -1)
  VL_CHECK(mLastUpdate != -1)

  if (mLastUpdate == -1 || mFrame1 == -1)
    return;

  const GLSLProgram* glslprogram = shader->glslProgram();

  // from here you can change uniforms or query uniform binding location

  if ( glslVertexBlendEnabled() && glslprogram )
  {
    // memo:
    // Since every character is in a different stage of the animation they all have different vertex/normal/etc. arrays pointers,
    // thus the lazy-vertex-array setup is forced to call glVertexAttribPointer/glVertexPointer/glBindBuffer continuously.
    // We may be able to partially solve this by putting all the animations in a single ArrayFloat3 and let the draw_calls 
    // switch the frame by using the base-vertex functionality.
    // I modified the App_MorphAnimation test so that all the characters share the same animation (thus the same vertex arrays) and don't have
    // transforms attached to eliminate the cost of glLoadMatrix/glMatrixMode. The resulting frame to frame time resulted only 1.2% reduced.

    // vertex/normals frame 1
    mGeometry->setVertexArray( mVertexFrames[mFrame1].get() );
    mGeometry->setNormalArray( mNormalFrames[mFrame1].get() );

    if (!mVertexFrames[mFrame1]->bufferObject()->handle() || mVertexFrames[mFrame1]->isBufferObjectDirty())
      mVertexFrames[mFrame1]->updateBufferObject(BUM_KeepRamBuffer);

    if (!mVertexFrames[mFrame2]->bufferObject()->handle() || mVertexFrames[mFrame2]->isBufferObjectDirty())
      mVertexFrames[mFrame2]->updateBufferObject(BUM_KeepRamBuffer);

    if (!mNormalFrames[mFrame1]->bufferObject()->handle() || mNormalFrames[mFrame1]->isBufferObjectDirty())
      mNormalFrames[mFrame1]->updateBufferObject(BUM_KeepRamBuffer);

    if (!mNormalFrames[mFrame2]->bufferObject()->handle() || mNormalFrames[mFrame2]->isBufferObjectDirty())
      mNormalFrames[mFrame2]->updateBufferObject(BUM_KeepRamBuffer);

    VL_CHECK( mVertexFrames[mFrame1]->bufferObject()->handle() )
    VL_CHECK( mVertexFrames[mFrame2]->bufferObject()->handle() )
    VL_CHECK( mNormalFrames[mFrame1]->bufferObject()->handle() )
    VL_CHECK( mNormalFrames[mFrame2]->bufferObject()->handle() )

    #if 1 // faster method:

      // vertex attrib and uniform animation
      if (mVertex2_Binding == -1)
        mVertex2_Binding = glslprogram->getAttribLocation("vertex2");

      if (mNormal2_Binding == -1)
        mNormal2_Binding = glslprogram->getAttribLocation("normal2");

      if (mAnim_t_Binding  == -1)
        mAnim_t_Binding = glslprogram->getUniformLocation("anim_t");

      // vertex/normals frame 2
      mGeometry->setVertexAttribArray( mVertex2_Binding, mVertexFrames[mFrame2].get() );
      mGeometry->setVertexAttribArray( mNormal2_Binding, mNormalFrames[mFrame2].get() );
      // frame interpolation ratio
      glUniform1fv(mAnim_t_Binding, 1, &mAnim_t);

    #else // slower but simpler method:

      // vertex/normals frame 2
      mGeometry->setVertexAttribArray( glslprogram->getAttribLocation("vertex2"), false, false, mVertexFrames[mFrame2].get() );
      mGeometry->setVertexAttribArray( glslprogram->getAttribLocation("normal2"), false, false, mNormalFrames[mFrame2].get() );
      // frame interpolation ratio
      glUniform1fv(glslprogram->getUniformLocation("anim_t"), 1, &mAnim_t);
    #endif
  }
  else
  if ( do_update )
  {
    if (mGeometry->vertexArray() == NULL)
      mGeometry->setVertexArray(mVertices.get());

    if (mGeometry->normalArray() == NULL)
      mGeometry->setNormalArray(mNormals.get());

    blendFrames(mFrame1, mFrame2, mAnim_t);
  }
}
//-----------------------------------------------------------------------------
void MorphingCallback::bindActor(Actor* actor)
{
  actor->actorEventCallbacks()->push_back( this );
  actor->setLod(0, mGeometry.get());
}
//-----------------------------------------------------------------------------
void MorphingCallback::init(ResourceDatabase* res_db)
{
  if (res_db->count<Geometry>() == 0)
    return;

  Geometry* geometry = res_db->get<Geometry>(0);
  mGeometry->shallowCopyFrom( *geometry );
  mVertices = new ArrayFloat3;
  mNormals  = new ArrayFloat3;

  // setup Geometry vertex attributes

  // copy vertex frames

  for(unsigned i=0, count=res_db->count<ArrayAbstract>(); i<count; ++i)
  {
    ArrayFloat3* buffer = cast<ArrayFloat3>(res_db->get<ArrayAbstract>(i));
    if (buffer && buffer->objectName() == "vertex_frame")
    {
      mVertexFrames.push_back(buffer);
    }
    else
    if (buffer && buffer->objectName() == "normal_frame")
    {
      mNormalFrames.push_back(buffer);
    }
  }

  if (mVertexFrames.empty())
  {
    Log::error("MorphingCallback::init(): no ArrayFloat3 named 'vertex_frame' found.\n");
    return;
  }

  if (mNormalFrames.empty())
  {
    Log::error("MorphingCallback::init(): no ArrayFloat3 named 'normal_frame' found.\n");
    return;
  }

  if (mVertexFrames.size() != mNormalFrames.size())
  {
    Log::error("MorphingCallback::init(): vertex frame count differs from normal frame count.\n");
    return;
  }

  // compute AABB using the first frame

  mGeometry->setVertexArray(mVertexFrames[0].get() );
  mGeometry->setNormalArray(mNormalFrames[0].get() );
  mGeometry->computeBounds();

  mGeometry->setVertexArray(NULL);
  mGeometry->setNormalArray(NULL);
}
//-----------------------------------------------------------------------------
void MorphingCallback::blendFrames(int a, int b, float t)
{
  // allocate interpolation buffers
  if (mVertices->size() != mVertexFrames[0]->size() ||
      mNormals->size()  != mNormalFrames[0]->size() )
  {
    mVertices->resize( mVertexFrames[0]->size() );
    mNormals->resize(  mNormalFrames[0]->size() );
  }

  #if 1
    float Ha = 1-t;
    float Hb = t;
  #else
    float Ha = 2*t*t*t - 3*t*t + 1;
    float Hb = -2*t*t*t + 3*t*t;
  #endif

  for(size_t i=0; i<mVertices->size(); ++i)
  {
    mVertices->at(i) = mVertexFrames[ a ]->at(i)*Ha + mVertexFrames[ b ]->at(i)*Hb;
    mNormals->at(i)  = mNormalFrames[ a ]->at(i)*Ha + mNormalFrames[ b ]->at(i)*Hb;
  }

  if (mGeometry->isBufferObjectEnabled() && Has_BufferObject)
  {
    // mic fixme:
    // Come si vede qui' sta nomenclatura non e' chiara: 
    // sembra che stiamo semplicemente cambiano un po di flags invece stiamo updatando tutto il BufferObject!!!
    mVertices->bufferObject()->setBufferData(BU_DYNAMIC_DRAW, false);
    mNormals ->bufferObject()->setBufferData(BU_DYNAMIC_DRAW, false);
  }
}
//-----------------------------------------------------------------------------
void MorphingCallback::setAnimation(int start, int end, float period)
{
  mFrame1 = -1;
  mFrame2 = -1;
  mLastUpdate = -1;
  mElapsedTime = 0;
  mAnimationStartTime = 0;
  mAnimationStart   = start;
  mAnimationEnd     = end;
  mAnimationPeriod  = period;
  mAnimationStarted = false;
}
//-----------------------------------------------------------------------------
void MorphingCallback::startAnimation(real start_time)
{
  mAnimationStarted = true;
  mFrame1 = -1;
  mFrame2 = -1;
  mLastUpdate = -1;
  mElapsedTime = 0;
  mAnimationStartTime = start_time;
}
//-----------------------------------------------------------------------------
void MorphingCallback::stopAnimation()
{
  mAnimationStarted = false;
}
//-----------------------------------------------------------------------------
void MorphingCallback::initFrom(MorphingCallback* morph_cb)
{
  mVertices = new ArrayFloat3;
  mNormals  = new ArrayFloat3;

  // copy vertex frames

  mVertexFrames = morph_cb->mVertexFrames;
  mNormalFrames = morph_cb->mNormalFrames;

  #if 0
    // Geometry sharing method: works only wiht GLSL

    // we can have a single shared Geometry since our MorphingCallback setups the
    // appropriate position and normal arrays for every Actor just before the rendering!
    mGeometry = morph_cb->mGeometry;
  #else
    // Geometry copy method
    mGeometry->shallowCopyFrom( *morph_cb->mGeometry );

    // compute AABB using the first frame

    mGeometry->setVertexArray(morph_cb->mVertexFrames[0].get() );
    mGeometry->setNormalArray(morph_cb->mNormalFrames[0].get() );
    mGeometry->computeBounds();

    mGeometry->setVertexArray(NULL);
    mGeometry->setNormalArray(NULL);
  #endif

  setAnimation(0,0,0);
}
//-----------------------------------------------------------------------------
void MorphingCallback::resetGLSLBindings()
{
  mVertex2_Binding = -1;
  mNormal2_Binding = -1;
  mAnim_t_Binding  = -1;
}
//-----------------------------------------------------------------------------
