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

#ifndef KeyValues_INCLUDE_ONCE
#define KeyValues_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/String.hpp>
#include <map>

namespace vl
{
  //! A set of key/value pairs usually used to associate generic information, tags, attributes etc. to another class.
  class VLCORE_EXPORT KeyValues: public Object
  {
    VL_INSTRUMENT_CLASS(vl::KeyValues, Object)

  public:
    KeyValues();
    
    bool has(const String& key) const { return mKeyValues.find(key) != mKeyValues.end(); }
    
    String value(const String& key) const;
    
    String& set(const String& key) { return mKeyValues[key]; }
    
    void erase(const String& key) { mKeyValues.erase(key); }
    
    void getKeys(std::vector<String>& keys) const;
    
    void clear() { mKeyValues.clear(); }
    
    const std::map<String,String>& keyValueMap() const { return mKeyValues; }
    
    std::map<String,String>& keyValueMap() { return mKeyValues; }
    
    void print();

  protected:
    std::map<String,String> mKeyValues;
  };
}

#endif
