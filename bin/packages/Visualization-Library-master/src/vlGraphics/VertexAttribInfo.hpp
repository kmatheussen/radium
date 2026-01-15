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

#ifndef VertexAttribInfo_INLUDE_ONCE
#define VertexAttribInfo_INLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlGraphics/Array.hpp>

namespace vl
{
  //------------------------------------------------------------------------------
  // VertexAttribInfo
  //------------------------------------------------------------------------------
  /**
   * Implements a generic OpenGL Shading Language vertex attribute to be used with a Geometry, 
   * see also http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml
  */
  class VertexAttribInfo: public Object
  {
    VL_INSTRUMENT_CLASS(vl::VertexAttribInfo, Object)

  public:
    VertexAttribInfo(unsigned int location, ArrayAbstract* data, bool normalize=true, EVertexAttribInterpretation data_behav=VAI_NORMAL): mData(data), mAttribLocation(location), mInterpretation(data_behav), mNormalize(normalize) {}
    
    VertexAttribInfo(): mAttribLocation((unsigned int)-1), mInterpretation(VAI_NORMAL), mNormalize(false) {}

    //! The GPU buffer that stores the data
    void setData(ArrayAbstract* data) { mData = data; }
    
    //! The GPU buffer that stores the data
    const ArrayAbstract* data() const { return mData.get(); }
    
    //! The GPU buffer that stores the data
    ArrayAbstract* data() { return mData.get(); }
    
    //! The 'index' parameter of the vertex attribute as used with glVertexAttribPointer() and glEnableVertexAttribArray().
    //! \sa
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml
    void setAttribLocation(unsigned int index) { mAttribLocation = index; }
    
    //! The 'index' parameter of the vertex attribute as used with glVertexAttribPointer() and glEnableVertexAttribArray().
    //! \sa
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml
    unsigned int attribLocation() const { return mAttribLocation; }
    
    //! The 'normalized' parameter as used with glVertexAttribPointer()
    //! \sa
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml
    void setNormalize(bool normalize) { mNormalize = normalize; }

    //! The 'normalized' parameter as used with glVertexAttribPointer()
    //! \sa
    //! - http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml
    bool normalize() const { return mNormalize; }
    
    //! How the data is interpreted by the OpenGL, see EVertexAttribInterpretation.
    void setInterpretation(EVertexAttribInterpretation behavior) { mInterpretation = behavior; }

    //! How the data is interpreted by the OpenGL, see EVertexAttribInterpretation.
    EVertexAttribInterpretation interpretation() const { return mInterpretation; }

  protected:
    ref<ArrayAbstract> mData;
    unsigned int mAttribLocation;
    EVertexAttribInterpretation mInterpretation;
    bool mNormalize;
  };
}

#endif
