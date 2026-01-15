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

#include "../3rdparty/tristripper/tri_stripper.cpp"
#include "../3rdparty/tristripper/policy.cpp"
#include "../3rdparty/tristripper/connectivity_graph.cpp"

using namespace triangle_stripper;

#include <vlGraphics/TriangleStripGenerator.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlGraphics/DoubleVertexRemover.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;

namespace
{
  void fillIndices(std::vector<unsigned int>& indices, const DrawCall* dc, bool substitute_quads)
  {
    indices.clear();

    if ( dc->primitiveType() == PT_QUADS && !substitute_quads )
      return;

    indices.reserve( 1000 );

    for(TriangleIterator trit = dc->triangleIterator(); trit.hasNext(); trit.next())
    {
      int a = trit.a();
      int b = trit.b();
      int c = trit.c();
      // skip degenerate triangles
      if (a != b && b != c)
      {
        indices.push_back(a);
        indices.push_back(b);
        indices.push_back(c);
      }
    }
  }
}

void TriangleStripGenerator::stripfy(Geometry* geom, int cache_size, bool merge_strips, bool remove_doubles, bool substitute_quads)
{
  if (remove_doubles)
  {
    DoubleVertexRemover dvr;
    dvr.removeDoubles(geom);
  }

  for( int idraw=geom->drawCalls()->size(); idraw--; )
  {
    DrawCall* dc = geom->drawCalls()->at(idraw);

    triangle_stripper::indices indices;

    fillIndices(indices, dc, substitute_quads);

    std::vector<unsigned int> algo2_strip;
    std::vector<unsigned int> algo2_tris;
    // stripfyAlgo2(algo2_strip, algo2_tris, indices, cache_size);

    tri_stripper striper(indices);
    striper.SetCacheSize(cache_size);
    striper.SetMinStripSize(4);
    primitive_vector out;
    striper.Strip(&out);

    // install new strip
    if (out.size())
    {
      geom->drawCalls()->erase(idraw,1);
      algo2_strip.reserve(indices.size());
      for(unsigned s=0; s<out.size(); ++s)
      {
        if (out[s].Type == TRIANGLE_STRIP)
        {
          algo2_strip.clear();
          for(unsigned p=0; p<out[s].Indices.size(); ++p)
            algo2_strip.push_back(out[s].Indices[p]);

          ref<DrawElementsUInt> draw_elems = new DrawElementsUInt(PT_TRIANGLE_STRIP);
          draw_elems->indexBuffer()->resize(algo2_strip.size());
          memcpy(draw_elems->indexBuffer()->ptr(), &algo2_strip[0], sizeof(unsigned int)*algo2_strip.size());
          geom->drawCalls()->push_back(draw_elems.get());
        }
        else // TRIANGLES
        {
          algo2_tris.clear();
          for(unsigned p=0; p<out[s].Indices.size(); ++p)
            algo2_tris.push_back(out[s].Indices[p]);

          ref<DrawElementsUInt> draw_elems = new DrawElementsUInt(PT_TRIANGLES);
          draw_elems->indexBuffer()->resize(algo2_tris.size());
          memcpy(draw_elems->indexBuffer()->ptr(), &algo2_tris[0], sizeof(unsigned int)*algo2_tris.size());
          geom->drawCalls()->push_back(draw_elems.get());
        }
      }
    }
  }

  if (merge_strips)
    geom->mergeTriangleStrips();
}

