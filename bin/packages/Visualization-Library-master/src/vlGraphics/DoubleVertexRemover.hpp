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

#ifndef DoubleVertexRemover_INCLUDE_ONCE
#define DoubleVertexRemover_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>
#include <vector>

namespace vl
{
  //-----------------------------------------------------------------------------
  // VertexMapper
  //-----------------------------------------------------------------------------
  //! Generates a set of new vertices from the old one.
  class VLGRAPHICS_EXPORT VertexMapper: public Object
  {
    VL_INSTRUMENT_CLASS(vl::VertexMapper, Object)

  public:
    //! Regenerates a new Array based on the given mapping.
    //! \param data The array to be regenerated
    //! \param map_new_to_old Specifies the mapping from the old vetices to the new one. The \p i-th vertex of the new vertex array will use the \p map_new_to_old[i]-th vertex of the old array, 
    //! that is, \p map_new_to_old[i] specifies the \a old vertex to be used to generate the \a new \p i-th vertex.
    ref<ArrayAbstract> regenerate(ArrayAbstract* data, const std::vector<u32>& map_new_to_old) const;
  private:
    template<class T>
    ref<ArrayAbstract> regenerateT(ArrayAbstract* data, const std::vector<u32>& map_new_to_old) const;
  };
  //-----------------------------------------------------------------------------
  // DoubleVertexRemover
  //-----------------------------------------------------------------------------
  //! Removes from a Geometry the vertices with the same attributes. 
  //! As a result also all the DrawArrays prensent in the Geometry are substituted with DrawElements.
  class VLGRAPHICS_EXPORT DoubleVertexRemover: public VertexMapper
  {
    VL_INSTRUMENT_CLASS(vl::DoubleVertexRemover, VertexMapper)

  public:
    DoubleVertexRemover() {}
    void removeDoubles(Geometry* geom);
    const std::vector<u32>& mapNewToOld() const { return mMapNewToOld; }
    const std::vector<u32>& mapOldToNew() const { return mMapOldToNew; }

  protected:
    std::vector<u32> mMapNewToOld;
    std::vector<u32> mMapOldToNew;
  };
}

#endif
