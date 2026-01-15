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

#include <vlGraphics/UniformSet.hpp>

using namespace vl;

//-----------------------------------------------------------------------------
UniformSet& UniformSet::deepCopyFrom(const UniformSet& other)
{
  mUniforms = other.mUniforms;
  for(size_t i=0; i<mUniforms.size(); ++i)
    mUniforms[i] = mUniforms[i]->clone();
  return *this;
}
//-----------------------------------------------------------------------------
void UniformSet::setUniform(Uniform* uniform, bool check_for_doubles) 
{ 
  VL_CHECK(uniform)
  if (uniform == NULL)
    return;
  if ( check_for_doubles )
  {
    for(unsigned i=0; i<mUniforms.size(); ++i)
    {
      if (mUniforms[i]->name() == uniform->name())
      {
        mUniforms[i] = uniform;
        return;
      }
    }
  }
  mUniforms.push_back( uniform );
}
//-----------------------------------------------------------------------------
void UniformSet::eraseUniform(const char* name) 
{ 
  for(unsigned i=0; i<mUniforms.size(); ++i)
    if (mUniforms[i]->name() == name)
    {
      mUniforms.erase( mUniforms.begin() + i );
      return;
    }
}
//-----------------------------------------------------------------------------
void UniformSet::eraseUniform(const Uniform* uniform) 
{ 
  for(unsigned i=0; i<mUniforms.size(); ++i)
    if (mUniforms[i] == uniform)
    {
      mUniforms.erase( mUniforms.begin() + i );
      return;
    }
}
//-----------------------------------------------------------------------------
Uniform* UniformSet::gocUniform(const char* name)
{ 
  for(unsigned i=0; i<mUniforms.size(); ++i)
    if (mUniforms[i]->name() == name)
      return mUniforms[i].get();
  ref<Uniform> uniform = new Uniform;
  uniform->setName( name );
  mUniforms.push_back(uniform);
  return uniform.get();
}
//-----------------------------------------------------------------------------
Uniform* UniformSet::getUniform(const char* name)
{ 
  for(unsigned i=0; i<mUniforms.size(); ++i)
    if (mUniforms[i]->name() == name)
      return mUniforms[i].get();
  return NULL;
}
//-----------------------------------------------------------------------------
const Uniform* UniformSet::getUniform(const char* name) const
{ 
  for(unsigned i=0; i<mUniforms.size(); ++i)
    if (mUniforms[i]->name() == name)
      return mUniforms[i].get();
  return NULL;
}
//-----------------------------------------------------------------------------
