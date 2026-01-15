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

#ifndef Extrusion_INCLUDE_ONCE
#define Extrusion_INCLUDE_ONCE

#include <vlGraphics/Geometry.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/Vector3.hpp>
#include <vlCore/Matrix4.hpp>
#include <vector>

namespace vl
{
  typedef enum { SilhouetteClosed, SilhouetteOpen } ESilhouetteMode;
  /**
   * The Extrusion class generates a Geometry extruding a silhouette along a path.
   * \sa
   *
   * - \ref pagGuideExtrusion "Extrusion Tutorial" for a practical example of how to use extrusions.
   * - Interpolator, LinearInterpolator, CatmullRomInterpolator to generate silhouettes and extrusion paths.
   *
   * <img src="pics/pagGuideExtrusion3.jpg">
   */
  class VLGRAPHICS_EXPORT Extrusion: public vl::Object
  {
    VL_INSTRUMENT_CLASS(vl::Extrusion, Object)

  public:
    //! Constructor.
    Extrusion()
    {
      VL_DEBUG_SET_OBJECT_NAME()
      mSmooth     = false;
      mFillBottom = true;
      mFillTop    = true;
      mSilhouetteMode = SilhouetteClosed;
    }

    //! Performs the actual extrusion.
    vl::ref<vl::Geometry> extrude();

    //! Sets the silhouette to be extruded.
    void setSilhouette(const std::vector<vl::fvec2>& silhouette) { mSilhouette = silhouette; }
    //! Returns the silhouette to be extruded.
    const std::vector<vl::fvec2>& silhouette() const { return mSilhouette; }
    //! Returns the silhouette to be extruded.
    std::vector<vl::fvec2>& silhouette() { return mSilhouette; }

    //! Wether the silhouette is considered closed, i.e. a line-loop, or open.
    void setSilhouetteMode(ESilhouetteMode mode) { mSilhouetteMode = mode; }
    //! Wether the silhouette is considered closed, i.e. a line-loop, or open.
    ESilhouetteMode silhouetteMode() const { return mSilhouetteMode; }

    //! If true the normals of the geometry are smoothed.
    void setSmooth(bool smooth) { mSmooth = smooth; }
    //! If true the normals of the geometry are smoothed.
    bool smooth() const { return mSmooth; }

    //! Whether a set of triangles should be generated to fill the beginning of the extrusion (default is \p true).
    void setFillBottom(bool fill) { mFillBottom = fill; }
    //! Whether a set of triangles should be generated to fill the beginning of the extrusion (default is \p true).
    bool fillBottom() const { return mFillBottom; }

    //! Whether a set of triangles should be generated to fill the ending of the extrusion (default is \p true).
    void setFillTop(bool fill) { mFillTop = fill; }
    //! Whether a set of triangles should be generated to fill the ending of the extrusion (default is \p true).
    bool fillTop() const { return mFillTop; }

    //! The path along which the silhouette is extruded. 
    //! The path starts and ends with one extra control point on each side that define the orientation of the start/end extruded segments.
    const std::vector<vl::fvec3>& positionPath() const { return mPositionPath; }
    //! The path along which the silhouette is extruded.
    //! The path starts and ends with one extra control point on each side that define the orientation of the start/end extruded segments.
    std::vector<vl::fvec3>& positionPath() { return mPositionPath; }
    //! The scaling to be applied along the extrusion.
    //! There must be one scaling control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e scalingPath().size() must be equal to positionPath().size()-2.
    const std::vector<float>& scalingPath() const { return mScalingPath; }
    //! The scaling to be applied along the extrusion.
    //! There must be one scaling control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e scalingPath().size() must be equal to positionPath().size()-2.
    std::vector<float>& scalingPath() { return mScalingPath; }
    //! The rotation to be applied along the extrusion.
    //! There must be one rotation control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e rotationPath().size() must be equal to positionPath().size()-2.
    const std::vector<float>& rotationPath() const { return mRotationPath; }
    //! The rotation to be applied along the extrusion.
    //! There must be one rotation control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e rotationPath().size() must be equal to positionPath().size()-2.
    std::vector<float>& rotationPath() { return mRotationPath; }
    //! The color to be applied to the extrusion.
    //! There must be one color control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e colorPath().size() must be equal to positionPath().size()-2.
    const std::vector<vl::fvec4>& colorPath() const { return mColorPath; }
    //! The color to be applied to the extrusion.
    //! There must be one color control point for each position control point in the positionPath() with the exclusion 
    //! of the starting and ending control points, i.e colorPath().size() must be equal to positionPath().size()-2.
    std::vector<vl::fvec4>& colorPath() { return mColorPath; }

  protected:
    std::vector<vl::fvec2> mSilhouette;
    std::vector<vl::fvec3> mPositionPath;
    std::vector<float> mScalingPath; 
    std::vector<float> mRotationPath;
    std::vector<vl::fvec4> mColorPath;   
    ESilhouetteMode mSilhouetteMode;
    bool mSmooth;
    bool mFillBottom;
    bool mFillTop;
  };
}

#endif
