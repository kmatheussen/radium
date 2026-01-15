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

#ifndef Tessellator_INCLUDE_ONCE
#define Tessellator_INCLUDE_ONCE

#include <vlGraphics/OpenGL.hpp>
#include <vlGraphics/Geometry.hpp>
#include <vlCore/Vector3.hpp>
#include <vector>

#ifndef CALLBACK
#define CALLBACK
#endif

namespace vl
{
  /**
   * Tessellates a complex polygon defined by a set of outlines into a set of triangles that can be rendered by Visualization Library.
   * For more information see the OpenGL Programmer's Guide chapter #11 "Tessellators and Quadrics".
   */
  class VLGRAPHICS_EXPORT Tessellator: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Tessellator, Object)

    typedef void (CALLBACK *callback_type)(void);
  public:

    //! Constructor.
    Tessellator();

    //! Destructor
    ~Tessellator();

    //! The contours that specify the complex polygon to be tessellated.
    const std::vector<dvec3>& contourVerts() const { return mContourVerts; }
    
    //! The contours that specify the complex polygon to be tessellated.
    std::vector<dvec3>& contourVerts() { return mContourVerts; }

    //! The contours that specify the complex polygon to be tessellated.
    const std::vector<int>& contours() const { return mContours; }
    
    //! The contours that specify the complex polygon to be tessellated.
    std::vector<int>& contours() { return mContours; }

    //! A set of triangles representing the tessellated polygon.
    const std::vector<fvec3>& tessellatedTris() const { return mTessellatedTris; }
    
    //! A set of triangles representing the tessellated polygon.
    std::vector<fvec3>& tessellatedTris() { return mTessellatedTris; }

    //! See gluTessNormal documentation.
    void setTessNormal(const fvec3& normal) { mTessNormal = normal; }
    
    //! See gluTessNormal documentation.
    const fvec3& tessNormal() const { return mTessNormal; }

    //! See gluTessProperty documentation (GLU_TESS_BOUNDARY_ONLY)
    void setBoundaryOnly(bool on) { mBoundaryOnly = on; }
    
    //! See gluTessProperty documentation (GLU_TESS_BOUNDARY_ONLY)
    bool boundaryOnly() const { return mBoundaryOnly; }

    //! See gluTessProperty documentation (GLU_TESS_TOLERANCE)
    double tolerance() const { return mTolerance; }
    
    //! See gluTessProperty documentation (GLU_TESS_TOLERANCE)
    void setTolerance(double tolerance) { mTolerance = tolerance; }

    //! See gluTessProperty documentation (GLU_TESS_WINDING_RULE)
    ETessellationWinding windingRule() const { return mWindingRule; }
    
    //! See gluTessProperty documentation (GLU_TESS_WINDING_RULE)
    void setWindingRule(ETessellationWinding rule) { mWindingRule = rule; }

    /* 
     * Tessellates the specified polygon.
     * If \p append_tessellated_tris equals \p true then the previously tessellated triangles are kept and the newly 
     * generated triangles are appended to them. This is useful when one has to tessellate several triangles and 
     * the result should be accumulated in a single triangle set.
     * 
     * After the function is called the contours() and contourVerts() are cleared.
     */
    bool tessellate(bool append_tessellated_tris=false);

    //! Utility function that calls tessellate() and creates a Geometry with the tessellated triangles.
    ref<Geometry> tessellateGeometry(bool append_tessellated_tris=false);

    void setTessellateIntoSinglePolygon(bool on) { mTessellateIntoSinglePolygon = on; }

    bool tessellateIntoSinglePolygon() const { return mTessellateIntoSinglePolygon; }
    
  protected:
    static void CALLBACK tessBeginData( GLenum type, Tessellator* tessellator );
    static void CALLBACK tessVertexData( dvec3* vec, Tessellator* tessellator );
    static void CALLBACK tessCombineData( GLdouble coords[3], dvec3 *d[4], GLfloat w[4], dvec3 **dataOut, Tessellator* tessellator );
    static void CALLBACK tessEnd(void);
    static void CALLBACK tessError( GLenum errno );
    void freeCombinedVertices();

  protected:
    // input
    std::vector<int> mContours;
    std::vector<dvec3> mContourVerts;
    // output
    std::vector<fvec3> mTessellatedTris;
    // intermediate data
    std::vector< std::vector<fvec3> > mFans;
    std::vector< std::vector<fvec3> > mTriStrips;
    std::vector< std::vector<fvec3> > mLineLoops;
    std::vector< dvec3* > mCombinedVertices;
    GLenum mPrimitiveType;
    // see gluTessNorml()
    fvec3 mTessNormal;
    // see GLU_TESS_BOUNDARY_ONLY
    bool mBoundaryOnly;
    // see GLU_TESS_TOLERANCE
    double mTolerance;
    // see GLU_TESS_WINDING_RULE
    ETessellationWinding mWindingRule;
    // tessellate into a single polygon
    bool mTessellateIntoSinglePolygon;
  };

}

#endif
