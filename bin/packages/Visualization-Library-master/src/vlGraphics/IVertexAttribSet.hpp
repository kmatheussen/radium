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

#ifndef IVertexAttribSet_INCLUDE_ONCE
#define IVertexAttribSet_INCLUDE_ONCE

#include <vlGraphics/VertexAttribInfo.hpp>
#include <vlCore/Collection.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // IVertexAttribSet
  //------------------------------------------------------------------------------
  /** Abstract interface to manipulate OpenGL's vertex attribute arrays. */
  class IVertexAttribSet
  {
  public:
    /** Conventional vertex array. */
    virtual void setVertexArray(ArrayAbstract* data) = 0;
    /** Conventional vertex array. */
    virtual const ArrayAbstract* vertexArray() const = 0;
    /** Conventional vertex array. */
    virtual ArrayAbstract* vertexArray() = 0;

    /** Conventional normal array. */
    virtual void setNormalArray(ArrayAbstract* data) = 0;
    /** Conventional normal array. */
    virtual const ArrayAbstract* normalArray() const = 0;
    /** Conventional normal array. */
    virtual ArrayAbstract* normalArray() = 0;

    /** Conventional color array. */
    virtual void setColorArray(ArrayAbstract* data) = 0;
    /** Conventional color array. */
    virtual const ArrayAbstract* colorArray() const = 0;
    /** Conventional color array. */
    virtual ArrayAbstract* colorArray() = 0;

    /** Conventional secondary color array. */
    virtual void setSecondaryColorArray(ArrayAbstract* data) = 0;
    /** Conventional secondary color array. */
    virtual const ArrayAbstract* secondaryColorArray() const = 0;
    /** Conventional secondary color array. */
    virtual ArrayAbstract* secondaryColorArray() = 0;

    /** Conventional fog array. */
    virtual void setFogCoordArray(ArrayAbstract* data) = 0;
    /** Conventional fog array. */
    virtual const ArrayAbstract* fogCoordArray() const = 0;
    /** Conventional fog array. */
    virtual ArrayAbstract* fogCoordArray() = 0;

    /** Conventional texture coords arrays. */
    virtual void setTexCoordArray(int tex_unit, ArrayAbstract* data) = 0;
    /** Conventional texture coords arrays. */
    virtual const ArrayAbstract* texCoordArray(int tex_unit) const = 0;
    /** Conventional texture coords arrays. */
    virtual ArrayAbstract* texCoordArray(int tex_unit) = 0;
    /** Number of texture coordinate arrays currently present. */
    virtual int texCoordArrayCount() const = 0;
    /** Gets the i-th texture coordinate information, where i must be 0 <= i < texCoordArrayCount(). */
    virtual void getTexCoordArrayAt(int i, int& out_tex_unit, const ArrayAbstract* &tex_array) const = 0;

    /** Binds a generic vertex attribute. */
    virtual void setVertexAttribArray(const VertexAttribInfo& info) = 0;

    /** Returns a generic vertex attribute's info. */
    virtual const VertexAttribInfo* vertexAttribArray(unsigned int attrib_location) const = 0;

    /** Returns a generic vertex attribute's info. */
    virtual VertexAttribInfo* vertexAttribArray(unsigned int attrib_location) = 0;

    /** The list of VertexAttribInfo objects bound to a Geometry. */
    virtual Collection<VertexAttribInfo>* vertexAttribArrays() = 0;

    /** The list of VertexAttribInfo objects bound to a Geometry. */
    virtual const Collection<VertexAttribInfo>* vertexAttribArrays() const = 0;
  };
}

#endif
