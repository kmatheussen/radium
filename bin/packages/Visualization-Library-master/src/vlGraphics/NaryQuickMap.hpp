/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#ifndef NaryQuickSet_INCLUDE_ONCE
#define NaryQuickSet_INCLUDE_ONCE

#include <vlCore/Object.hpp>

namespace vl
{
  /** Simple map used to add, remove, iterate, clear elements efficiently (all O(1)).
      To be used only when the number of keys is well defined and not too big.
      Internally used by VL to deal with enables and render states. */
  template<typename KeyType, typename ValueType, int MaxMapType>
  class NaryQuickMap: public Object
  {
  public:
    NaryQuickMap()
    {
      reset();
    }

    void reset()
    {
      mMapSize = 0;
      memset(mKeyToValue, 0, sizeof(mKeyToValue));
      memset(mValueToKey, 0, sizeof(mValueToKey));
    }

    void clear()
    {
      // no need to touch anything else.. you clever VL!
      mMapSize = 0;
    }

    int size() const { return mMapSize; }

    ValueType* begin() { return mValues; }
    ValueType* end() { return mValues + mMapSize; }
    const ValueType* begin() const { return mValues; }
    const ValueType* end() const { return mValues + mMapSize; }

    void append(KeyType key)
    {
      append(key, key);
    }

    // use this when you know that 'key' is not already in the set.
    void append(KeyType key, ValueType value)
    {
      // sovrascrivi 'key' in ogni caso
      int pos = mMapSize++;
      mKeyToValue[key] = pos;
      mValueToKey[pos] = key;
      mValues[pos] = value;
    }

    void insert(KeyType key)
    {
      insert(key, key);
    }

    void insert(KeyType key, const ValueType& value)
    {
      VL_CHECK(key < MaxMapType)
      int pos = find(key);
      if (pos == -1)
      {
        pos = mMapSize++;
        VL_CHECK(pos < MaxMapType)
        mKeyToValue[key] = pos;
        mValueToKey[pos] = key;
      }
      mValues[pos] = value;
    }

    void erase(KeyType key)
    {
      int pos = find(key);
      if (pos != -1)
      {
        // move the last object to the one being erased
        if (mMapSize>1)
        {
          // move value
          mValues[pos] = mValues[mMapSize-1];
          // move Enum
          mValueToKey[pos] = mValueToKey[mMapSize-1];
          // mark moved KeyType to point to the new pos
          mKeyToValue[mValueToKey[pos]] = pos;
        }
        mMapSize--;
        VL_CHECK(mMapSize >= 0)
      }
    }

    int find(KeyType key) const
    {
      int pos = mKeyToValue[key];
      VL_CHECK(pos >= 0)
      VL_CHECK(pos < MaxMapType)
      if (pos < mMapSize)
      {
        KeyType e = mValueToKey[pos];
        VL_CHECK(e >= 0)
        VL_CHECK(e < MaxMapType)
        if (e == key)
          return pos;
        else
          return -1;
      }
      else
        return -1;
    }

    bool hasKey(KeyType key) const
    {
      return find(key) != -1;
    }

    const ValueType& valueFromKey(KeyType key) const
    {
        VL_CHECK(key >= 0)
        VL_CHECK(key < MaxMapType)
        VL_CHECK(mKeyToValue[key] >= 0)
        VL_CHECK(mKeyToValue[key] < MaxMapType)
        return mValues[mKeyToValue[key]];
    }

    const ValueType& valueFromIndex(int i) const
    {
      return mValues[i];
    }

    KeyType key(int i) const
    {
      return mValueToKey[i];
    }

  protected:
    // Note:
    // In the case of EEnable we don't really need this 
    // (we would need only a set, not a map) but 
    // for the moment we use this for simplicity.
    ValueType mValues[MaxMapType]; 
    // given an index (< mMapSize) of a value returns it's KeyType (== index in mKeyToValue)
    KeyType mValueToKey[MaxMapType];
    // the number of elements in mValues and mValueToKey
    int mMapSize;
    // given a KeyType gives where in mValues and mValueToKey the value is.
    int mKeyToValue[MaxMapType]; 
  };

}

#endif
