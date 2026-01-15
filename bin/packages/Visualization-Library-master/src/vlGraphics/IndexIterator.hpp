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

#ifndef IndexIterator_INCLUDE_ONCE
#define IndexIterator_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl
{
//-----------------------------------------------------------------------------
// IndexIteratorAbstract
//-----------------------------------------------------------------------------
  /** Abstract class used as base for all the index iterators specializations. */
  class IndexIteratorAbstract: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::IndexIteratorAbstract, Object)

  public:
    IndexIteratorAbstract(): mIndex(-1) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    int index() const { return mIndex; }
    virtual bool hasNext() const = 0;
    virtual bool next() = 0;

  protected:
    int mIndex;
  };
//-----------------------------------------------------------------------------
// IndexIterator
//-----------------------------------------------------------------------------
  /** Wraps a IndexIteratorAbstract to iterate over the indices of a DrawCall. */
  class IndexIterator: public Object
  {
    VL_INSTRUMENT_CLASS(vl::IndexIterator, Object)

  public:
    IndexIterator()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    void initialize(IndexIteratorAbstract* iterator) { mIterator = iterator; }
    int  index() { return mIterator->index(); }
    bool hasNext() { return mIterator->hasNext(); }
    bool next()  { return mIterator->next();  }
    bool operator++() { return next(); }

  protected:
    ref<IndexIteratorAbstract> mIterator;
  };
//-----------------------------------------------------------------------------
// IndexIteratorDrawArrays
//-----------------------------------------------------------------------------
  /** Index iterator operating used by DrawArrays. */
  class IndexIteratorDrawArrays: public IndexIteratorAbstract
  {
    VL_INSTRUMENT_CLASS(vl::IndexIteratorDrawArrays, IndexIteratorAbstract)

  public:
    IndexIteratorDrawArrays()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      initialize(0,0);
    }

    void initialize(int start, int count)
    {
      mStart  = start;
      mCount  = count;
      mCurPos = start;
      mIndex  = start;
    }

    virtual bool hasNext() const 
    { 
      return mCurPos != mStart + mCount; 
    }

    virtual bool next() 
    { 
      ++mCurPos; 
      mIndex = mCurPos;
      return true; 
    }

  protected:
    int mStart;
    int mCount;
    int mCurPos;
  };
//-----------------------------------------------------------------------------
// IndexIteratorElements
//-----------------------------------------------------------------------------
  /** Index iterator operating over DrawElements, DrawRangeElements and MultiDrawElements. */
  template<class TArray>
  class IndexIteratorElements: public IndexIteratorAbstract
  {
    VL_INSTRUMENT_CLASS(vl::IndexIteratorElements<TArray>, IndexIteratorAbstract)

  public:
    IndexIteratorElements()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      initialize( NULL, NULL, NULL, 0, false, 0 );
    }

    void initialize( const TArray* idx_array, const std::vector<GLint>* p_base_vertices, const std::vector<GLsizei>* p_vert_counts, 
                     int base_vert, bool prim_restart_on, unsigned int prim_restart_idx )
    {
      mArray = idx_array;
      mBaseVert = base_vert;
      mpBaseVertices = p_base_vertices;
      mpVertCounts   = p_vert_counts;
      mBaseCount     = 0;
      mBaseIdx       = 0;

      mPrimRestartEnabled = prim_restart_on;
      mPrimRestartIdx = prim_restart_idx;
      mCurPos = 0;
      if (mArray && mArray->size())
      {
        mIndex = mArray->at(0) + mBaseVert;
      }

      if (p_vert_counts)
      {
        VL_CHECK(p_base_vertices)
        VL_CHECK( p_base_vertices->size() == p_vert_counts->size() )

        mBaseCount = (*p_vert_counts)[mBaseIdx];

        mIndex = (*mpBaseVertices)[mBaseIdx];
      }
    }

    virtual bool hasNext() const 
    { 
      return mCurPos != (int)mArray->size(); 
    }

    virtual bool next() 
    {
      ++mCurPos;
      while( mCurPos < (int)mArray->size() && mArray->at(mCurPos) == mPrimRestartIdx && mPrimRestartEnabled  )
        ++mCurPos;
      if ( mCurPos < (int)mArray->size() )
      {
        mIndex = mArray->at(mCurPos) + mBaseVert;
        if (mpVertCounts)
        {
          VL_CHECK(mpBaseVertices)
          mBaseCount--;
          if (!mBaseCount)
          {
            mBaseIdx++;
            mBaseCount = (*mpVertCounts)[mBaseIdx];
          }
          mIndex += (*mpBaseVertices)[mBaseIdx];
        }

        return true; 
      }
      else
      {
        mIndex = -1;
        mCurPos = (int)mArray->size();
        return false; 
      }
    }

  protected:
    const TArray* mArray;
    int mBaseVert;
    int mCurPos;
    bool mPrimRestartEnabled;
    unsigned int mPrimRestartIdx;
    const std::vector<GLint>* mpBaseVertices;
    const std::vector<GLsizei>* mpVertCounts;
    int mBaseCount;
    int mBaseIdx;
  };
//-----------------------------------------------------------------------------
}

#endif
