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

#ifndef RenderQueue_INCLUDE_ONCE
#define RenderQueue_INCLUDE_ONCE

#include <vlGraphics/RenderQueueSorter.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // RenderQueue
  //------------------------------------------------------------------------------
  /**
   * The RenderQueue class collects a list of RenderToken objects to be sorted and rendered.
  */
  class RenderQueue: public Object
  {
    VL_INSTRUMENT_CLASS(vl::RenderQueue, Object)

  public:
    RenderQueue(): mSize(0), mSizeMP(0)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mList.reserve(100);
      mListMP.reserve(100);
    }

    const RenderToken* at(int i) const { return mList[i].get(); }

    RenderToken* at(int i) { return mList[i].get(); }

    RenderToken* newToken(bool multipass)
    {
      if (multipass)
      {
        ++mSizeMP;
        if ( mSizeMP > (int)mListMP.size() )
          mListMP.push_back( new RenderToken );
        return mListMP[mSizeMP-1].get();
      }
      else
      {
        ++mSize;
        if ( mSize > (int)mList.size() )
          mList.push_back( new RenderToken );
        return mList[mSize-1].get();
      }
    }

    void clear()
    {
      mSize   = 0;
      mSizeMP = 0;
    }

    bool empty()
    {
      return mSize == 0;
    }

    int size() const
    {
      return mSize;
    }

    void sort(RenderQueueSorter* sorter, Camera* camera)
    {
      if (sorter->mightNeedZCameraDistance())
      {
        for(int i=0; i<size(); ++i)
        {
          RenderToken* tok = at(i);
          vec3 center = tok->mRenderable->boundingBox().isNull() ? vec3(0,0,0) : tok->mRenderable->boundingBox().center();
          if ( sorter->confirmZCameraDistanceNeed(tok) )
          {
            if (tok->mActor->transform())
              // tok->mCameraDistance = ( camera->viewMatrix() * (tok->mActor->transform()->worldMatrix() * center) ).lengthSquared();
              tok->mCameraDistance = -( camera->viewMatrix() * (tok->mActor->transform()->worldMatrix() * center) ).z();
            else
              // tok->mCameraDistance = ( camera->viewMatrix() * /* I* */ center ).lengthSquared();
              tok->mCameraDistance = -( camera->viewMatrix() * /* I* */ center ).z();
          }
          else
            tok->mCameraDistance = 0;
        }
      }

      VL_CHECK( sorter )
      std::sort( mList.begin(), mList.begin() + size(), Sorter( sorter ) );
    }

  private:
    class Sorter
    {
    public:
      Sorter(const RenderQueueSorter* sorter): mRenderQueueSorter(sorter) {}
      bool operator()(const ref<RenderToken>& a, const ref<RenderToken>& b) const
      {
        VL_CHECK(a && b);
        return mRenderQueueSorter->operator()(a.get(), b.get());
      }
    protected:
      const RenderQueueSorter* mRenderQueueSorter;
    };

  protected:
    // mic fixme: would be nice not to allocate these dinamically. 
    // Necessary because:
    // 1- RenderToken.mNextPass: the pointer should be stable across std::vector reallocations. 
    //    Maybe we can use indices?
    // 2- The sorting sorts only pointers instead of whole structures.
    // Note: we need two lists because the sorting must still respect the multipassing order.
    std::vector< ref<RenderToken> > mList;
    std::vector< ref<RenderToken> > mListMP;
    int mSize;
    int mSizeMP;
  };
  //------------------------------------------------------------------------------
  typedef std::map< float, ref<RenderQueue> > TRenderQueueMap;
  //------------------------------------------------------------------------------
}

#endif
