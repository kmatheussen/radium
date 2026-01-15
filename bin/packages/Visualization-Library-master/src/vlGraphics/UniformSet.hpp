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

#ifndef UniformSet_INCLUDE_ONCE
#define UniformSet_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlGraphics/link_config.hpp>
#include <vlGraphics/Uniform.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // UniformSet
  //------------------------------------------------------------------------------
  /**
   * A set of Uniform objects managed by a Shader.
   *
   * \sa 
   * Shader, Effect, Actor
  */
  class VLGRAPHICS_EXPORT UniformSet: public Object
  {
    VL_INSTRUMENT_CLASS(vl::UniformSet, Object)

  public:
    UniformSet()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    UniformSet& deepCopyFrom(const UniformSet& other);

    UniformSet& shallowCopyFrom(const UniformSet& other) { *this = other; return *this; }

    // uniform getters and setters

    void setUniform(Uniform* uniform, bool check_for_doubles = true);

    const std::vector< ref<Uniform> >& uniforms() const { return mUniforms; }

    std::vector< ref<Uniform> >& uniforms() { return mUniforms; }

    void eraseUniform(const char* name);

    void eraseUniform(const Uniform* uniform);

    void eraseAllUniforms() { mUniforms.clear(); }

    Uniform* gocUniform(const char* name);

    Uniform* getUniform(const char* name);

    const Uniform* getUniform(const char* name) const;

  protected:
    std::vector< ref<Uniform> > mUniforms;
  };
}

#endif
