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

#ifndef RenderingTree_INCLUDE_ONCE
#define RenderingTree_INCLUDE_ONCE

#include <vlGraphics/RenderingAbstract.hpp>

namespace vl
{
  //! The RenderingTree class organizes a set of renderings into an N-ary tree.
  //! To enable the RenderingTree set the enableMask() to a value != 0, otherwise the RenderingTree will be disabled.
  class VLGRAPHICS_EXPORT RenderingTree: public RenderingAbstract
  {
    VL_INSTRUMENT_CLASS(vl::RenderingTree, RenderingAbstract)

  public:
    //! Constructor.
    RenderingTree();
    
    //! Copy constructor.
    RenderingTree(const RenderingTree& other): RenderingAbstract(other) { *this = other; } 
    
    //! Assignment operator.
    RenderingTree& operator=(const RenderingTree& other);

    //! Recursively calls the render() method of its children.
    //! \note
    //! If enableMask() == 0 then no rendering is performed and no RenderEventCallback is called.
    virtual void render();

    //! The sub-Rendering (or child-Rendering) objects of a Rendering. A sub-Rendering is rendered before it's parent and after its children.
    Collection<RenderingAbstract>* subRenderings() { return mSubRendering.get(); }
    //! The sub-Rendering (or child-Rendering) objects of a Rendering. A sub-Rendering is rendered before it's parent and after its children.
    const Collection<RenderingAbstract>* subRenderings() const { return mSubRendering.get(); }

  protected:
    ref< Collection<RenderingAbstract> > mSubRendering;
  };
}

#endif
