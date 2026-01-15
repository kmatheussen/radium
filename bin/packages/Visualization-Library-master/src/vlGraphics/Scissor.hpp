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

#ifndef Scissor_INCLUDE_ONCE
#define Scissor_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/Rect.hpp>
#include <vlGraphics/Viewport.hpp>

namespace vl
{
  /**
   * The Scissor class wraps the OpenGL function glScissor(), see http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml for more information.
   * \sa
   * - Actor::setScissor()
   * - Shader::setScissor()
   */
  class Scissor: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Scissor, Object)

  public:
    Scissor() {}

    Scissor(int x, int y, int width, int height)
    {
      setScissor(x,y,width,height);
    }
    /**
     * Enables the scissor test on the area specified by scissorRect() clipped against the given Viewport.
     */
    void enable(const Viewport* viewport) const
    {
      RectI r = viewport->rect().intersected(scissorRect());
      glEnable(GL_SCISSOR_TEST);
      if (r.isNull())
        glScissor(0,0,0,0);
      else
        glScissor(r.x(), r.y(), r.width(), r.height());
    }
    /**
     * Disables the scissor test.
     */
    void disable()
    {
      glDisable(GL_SCISSOR_TEST);
    }

    /**
     * Defines the scissor box. 
     * The \p left, \p bottom, \p right and \p top parameters are in windows coordinates.
     * See also http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml for more information.
     */
    void setScissor(int x, int y, int width, int height) { setScissor( RectI(x,y,width,height) ); }
    /**
     * Defines the scissor box. 
     * The \p left, \p bottom, \p right and \p top parameters are in windows coordinates.
     * See also http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml for more information.
     */
    void setScissor(const RectI& scissor) { mScissor = scissor; }
    /**
     * Returns the scissor box.
     */
    const RectI& scissorRect() const { return mScissor; }

    /**
     * Defines a sort of lexicographic sorting that make possible the use of the Scissor class with STL containers like std::set, std::map etc.
     */
    bool operator<(const Scissor& other) const
    {
      return mScissor < other.mScissor;
    }

  protected:
    RectI mScissor;
  };
}

#endif
