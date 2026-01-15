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

#ifndef EnableSet_INCLUDE_ONCE
#define EnableSet_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vector>

namespace vl
{
  /**
   * A set of enables managed by Shader.
   * This class substitutes for the most part the OpenGL functions glEnable() and glDisable().
   *
   * \sa Shader, Effect, Actor
  */
  class EnableSet: public Object
  {
    VL_INSTRUMENT_CLASS(vl::EnableSet, Object)

  public:
    EnableSet(): mBlendingEnabled(false)
    {
      VL_DEBUG_SET_OBJECT_NAME()
      // these two are enabled by default
      mEnables.push_back(EN_DITHER);
      // only OpenGL ES 2 does not support GL_MULTISAMPLE
#if !defined(VL_OPENGL_ES2)
      mEnables.push_back(EN_MULTISAMPLE);
#endif
    }

    // enable getter and setters

    void enable(EEnable capability)
    {
      if (capability == EN_BLEND)
        mBlendingEnabled = true;
      for(unsigned i=0; i<mEnables.size(); ++i)
        if (mEnables[i] == capability)
          return;
      mEnables.push_back( capability );
    }

    void disable(EEnable capability)
    {
      if (capability == EN_BLEND)
        mBlendingEnabled = false;
      for(unsigned i=0; i<mEnables.size(); ++i)
      {
        if (mEnables[i] == capability)
        {
          mEnables.erase( mEnables.begin() + i );
          return;
        }
      }
    }

    const std::vector<EEnable>& enables() const { return mEnables; }

    int isEnabled(EEnable capability) const
    {
      for(unsigned i=0; i<mEnables.size(); ++i)
        if (mEnables[i] == capability)
          return true;
      return false;
    }

    void disableAll() { mEnables.clear(); mBlendingEnabled=false; }

    bool isBlendingEnabled() const { return mBlendingEnabled; }

  protected:
    std::vector<EEnable> mEnables;
    bool mBlendingEnabled;
  };
}

#endif
