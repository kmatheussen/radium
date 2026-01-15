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

#ifndef Collection_INCLUDE_ONCE
#define Collection_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vector>
#include <algorithm>

namespace vl
{
  //------------------------------------------------------------------------------
  // Collection
  //------------------------------------------------------------------------------
  /** Reference counted container that encapsulates the base functionalites of an std::vector<>.
   * This container can be used only with objects whose classes derive from Object. */
  template <typename T>
  class Collection: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Collection<T>, Object)

  public:
    Collection(const std::vector< ref<T> >& vector)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mVector = vector;
    }
    
    Collection()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    
    Collection& operator=(const std::vector< ref<T> >& vector)
    {
      mVector = vector;
      return *this;
    }

    operator std::vector< ref<T> >() const
    {
      return mVector;
    }

    void push_back( T* data ) { mVector.push_back(data); }
    
    void pop_back() { mVector.pop_back(); }
    
    void resize(int size) { mVector.resize(size); }
    
    int size() const { return (int)mVector.size(); }
    
    bool empty() const { return mVector.empty(); }
    
    void clear() { mVector.clear(); }
    
    const T* back() const { return mVector.back().get(); }
    
    T* back() { return mVector.back().get(); }
    
    void reserve(int capacity) { mVector.reserve(capacity); }
    
    int capacity() const { return (int)mVector.capacity(); }
    
    const ref<T>& operator[](int i) const { return mVector[i]; }
    
    ref<T>& operator[](int i) { return mVector[i]; }
    
    const T* at(int i) const { return mVector[i].get(); }
    
    T* at(int i) { return mVector[i].get(); }
    
    void swap(Collection& other) { mVector.swap(other.mVector); }

    // added functionalities

    void sort()
    {
      std::sort(mVector.begin(), mVector.end(), less);
    }

    int find(T* obj) const
    {
      typename std::vector< ref<T> >::const_iterator pos = std::find(mVector.begin(), mVector.end(), obj);
      if (pos == mVector.end())
        return -1;
      else
        return (int)(pos - mVector.begin());
    }
    
    void shrink()
    {
      Collection<T>(mVector).swap(mVector);
    }
    
    void push_back(const Collection<T>& objs )
    {
      mVector.insert(mVector.end(), objs.mVector.begin(), objs.mVector.end());
    }
    
    void insert(int start, const Collection<T>& objs )
    {
      mVector.insert(mVector.begin()+start, objs.mVector.begin(), objs.mVector.end());
    }
    
    void set(const Collection<T>& objs)
    {
      mVector = objs.mVector;
    }
    
    void erase(int start, int count)
    {
      mVector.erase(mVector.begin()+start, mVector.begin()+start+count);
    }
    
    void set(int index, T* obj) { mVector[index] = obj; }
    
    void insert(int index, T* obj) { mVector.insert(mVector.begin() + index, obj); }
    
    void erase(const T* data)
    {
      typename std::vector< ref<T> >::iterator it = std::find(mVector.begin(), mVector.end(), data);
      if (it != mVector.end())
        mVector.erase(it);
    
    }

    void eraseAt(int index) { mVector.erase(mVector.begin()+index); }

    const std::vector< ref<T> >& vector() const { return mVector; }
    
    std::vector< ref<T> >& vector() { return mVector; }

  protected:
    bool static less(const ref<T>& a, const ref<T>& b) { return *a < *b; }

  protected:
    std::vector< ref<T> > mVector;
  };
  //-----------------------------------------------------------------------------
}

#endif
