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

#ifndef BezierSurface_INCLUDE_ONCE
#define BezierSurface_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>

namespace vl
{
  /** Defines one or more concatenated bicubic Bézier patches to be used with the BezierSurface class.
    See also: 
    - \ref pagGuideBezierSurfaces "Bézier Patches and Surfaces Tutorial" for a practical example on how to use the BezierSurface class.
    - BezierSurface
  */
  class VLGRAPHICS_EXPORT BezierPatch: public Object
  {
    VL_INSTRUMENT_CLASS(vl::BezierPatch, Object)

  public:
    //! The control points grid defining the bicubic Bézier patch(es).
    typedef std::vector< vec3 > Points;
    //! Constructor
    BezierPatch(): mX(0), mY(0) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }
    //! Constructor
    BezierPatch(int x, int y): mX(0), mY(0) 
    { 
      VL_DEBUG_SET_OBJECT_NAME()
      resize(x,y);
    }
    //! Defines the x and y dimensions of the control point grid defining the patch.
    //! The \p x and \p y parameters must be of the form 3*n+1 with \p n integer positive, i.e 4 (n=1), 7 (n=2), 10 (n=3) and so on.
    void resize(int x, int y);
    //! Returns the \p x dimension of the patch as specified by resize().
    int x() const { return mX; }
    //! Returns the \p y dimension of the patch as specified by resize().
    int y() const { return mY; }
    //! The control points grid defining the bicubic Bézier patch(es).
    Points& points() { return mControlPoints; }
    //! The control points grid defining the bicubic Bézier patch(es).
    const Points& points() const { return mControlPoints; }
    //! Returns the i/j control point.
    vec3& at(int i, int j) { return mControlPoints[i + mX*j]; }
    //! Returns the i/j control point.
    const vec3& at(int i, int j) const { return mControlPoints[i + mX*j]; }
  protected:
    Points mControlPoints;
    int mX;
    int mY;
  };

  /** The BezierSurface class implements a Geometry that is capable of visualizing multiple bicubic Bézier patches (see BezierPatch).
    From Wikipedia [<a href="http://en.wikipedia.org/wiki/Bézier_surface">http://en.wikipedia.org/wiki/Bézier_surface</a>]:

    <i>"Bézier surfaces are a species of mathematical spline used in computer graphics, computer-aided design, and finite element modelling. As with the Bézier curve, a Bézier surface is defined by a set of control points. Similar to interpolation in many respects, a key difference is that the surface does not, in general, pass through the central control points; rather, it is "stretched" toward them as though each were an attractive force. They are visually intuitive, and for many applications, mathematically convenient."

    "Bézier surfaces were first described in 1972 by the French engineer Pierre Bézier who used them to design automobile bodies. Bézier surfaces can be of any degree, but bicubic Bézier surfaces generally provide enough degrees of freedom for most applications."</i>

    See also: 
    - \ref pagGuideBezierSurfaces "Bézier Patches and Surfaces Tutorial" for a practical example on how to use the BezierSurface class.
    - BezierPatch
  */
  class VLGRAPHICS_EXPORT BezierSurface: public Geometry
  {
    VL_INSTRUMENT_CLASS(vl::BezierSurface, Geometry)

  public:
    //! Constructor
    BezierSurface(): mDetail(16) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! Returns the Bézier patches that are part of this Bézier surface.
    std::vector< ref<BezierPatch> >& patches() { return mPatches; }
    //! Returns the Bézier patches that are part of this Bézier surface.
    const std::vector< ref<BezierPatch> >& patches() const { return mPatches; }

    //! The sampling level of the patch, must not be less than 2. The higher the sampling level, the more detailed the patch will be.
    void setDetail(unsigned i) { mDetail = i; }

    //! The sampling level of the patch, must not be less than 2. The higher the sampling level, the more detailed the patch will be.
    unsigned detail() const { return mDetail; }

    //! Generates the Bézier surface geometry based on the current patches and detail.
    //! Note that this method does not recompte the normals of the mesh, this means that if you are using the OpenGL lighting or other 
    //! techniques requiring vertex normals you should call computeNormals() right after calling this function.
    //! \param gen_tex_coords If set to \p true the function will also generate normalized (0..1) texture coordinates.
    void updateBezierSurface(bool gen_tex_coords=true);

  protected:
    std::vector< ref<BezierPatch> > mPatches;
    unsigned mDetail;
  };
}

#endif
