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

#include <vlCore/KeyValues.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
KeyValues::KeyValues()
{
  VL_DEBUG_SET_OBJECT_NAME()
}
//-----------------------------------------------------------------------------
String KeyValues::value(const String& key) const 
{ 
  std::map<String, String>::const_iterator it = mKeyValues.find(key);
  if ( it != mKeyValues.end() ) 
    return it->second;
  else
  {
    vl::Log::error( vl::Say("KeyValues::value(): key '%s' does not exist.\n") << key );
    return String();
  }
}
//-----------------------------------------------------------------------------
void KeyValues::getKeys(std::vector<String>& keys) const
{
  keys.clear();
  for(std::map<String, String>::const_iterator it = mKeyValues.begin(); it != mKeyValues.end(); ++it)
    keys.push_back( it->first );
}
//-----------------------------------------------------------------------------
void KeyValues::print()
{
  for(std::map<String, String>::const_iterator it = mKeyValues.begin(); it != mKeyValues.end(); ++it)
    vl::Log::print( Say("%s=%s\n") << it->first << it->second );
}
//-----------------------------------------------------------------------------
