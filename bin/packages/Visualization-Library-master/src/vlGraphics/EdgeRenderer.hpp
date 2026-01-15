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

#ifndef EdgeRenderer_INCLUDE_ONCE
#define EdgeRenderer_INCLUDE_ONCE

#include <vlGraphics/Renderer.hpp>
#include <vlGraphics/EdgeExtractor.hpp>
#include <vlGraphics/EdgeUpdateCallback.hpp>

namespace vl
{
  /** The EdgeRenderer class implements a special Renderer that automatically extracts and renders the edges of the objects in the scene.

  In order to efficiently and transparently render the edges of the objects present in the scene the EdgeRenderer class 
  keeps an internal cache of Geometry and Actor objects that contain the extracted edges. For this
  reason if the Geometry of an Actor changes or if an Actor is removed from the scene you should call the setActorDirty() method. This way 
  the cached information relative to that Actor will be removed and will be recreated only if/when such Actor becomes visible again. Call
  the clearCache() method to invalidate the whole cache. In order to minimized the amount of memory used by the cache it is important to 
  keep the cache as clean and up to date as possible.
  The color used to render the edges can be set globally using the setDefaultLineColor() method or by Actor using the declareActor() method.

  \sa 
  - \ref pagGuideEdgeRendering "Edge Enhancement and Wireframe Rendering Tutorial"
  - vl::EdgeExtractor
  */
  class VLGRAPHICS_EXPORT EdgeRenderer: public Renderer
  {
    VL_INSTRUMENT_CLASS(vl::EdgeRenderer, Renderer)

    class WFInfo: public Object
    {
    public:
      WFInfo(): mColor( vl::black ) {}
      fvec4 mColor;
      ref<Geometry> mGeometry;
      ref<EdgeUpdateCallback> mEdgeCallback;
    };

  public:
    EdgeRenderer(): mLineWidth(1.0f), mPolygonOffsetFactor(1.0f), mPolygonOffsetUnits(1.0f), mCreaseAngle(44.0f), mShowHiddenLines(true), mShowCreases(true), mSmoothLines(true)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    const RenderQueue* render(const RenderQueue* in_render_queue, Camera* camera, real frame_clock);

    //! Generates and caches all the information needed to render the edges of the given Actor using the specified color.
    WFInfo* declareActor(Actor* act, const fvec4& color);
    //! Generates and caches all the information needed to render the edges of the given Actor.
    WFInfo* declareActor(Actor* act);

    //! Clears the cache containing the Actor and edge information.
    //! Call this function when a significant part of the scene changed or was removed.
    //! The cache will be automatically rebuild at the next rendering frames.
    void clearCache() { mActorCache.clear(); }

    //! Removes all the edge/rendering information relative to the specified Actor from the cache.
    //! Call this function when an Actor's Geometry changed or when you know that an Actor that was previously visible won't be visible anymore, for example because it has been removed from the scene.
    //! Note that if the Actor becomes visible at any point later the cache will be automatically rebuilt.
    void setActorDirty(Actor* actor) { mActorCache.erase(actor); }

    //! If set to \p true shows also the hidden lines with a dashed pattern.
    void setShowHiddenLines(bool show) { mShowHiddenLines = show; }
    //! If set to \p true shows also the hidden lines with a dashed pattern.
    bool showHiddenLines() const { return mShowHiddenLines; }

    //! If set to \p true shows not only the edges that define the silhouette of an object but also the crease edges.
    void setShowCreases(bool show) { mShowCreases = show; }
    //! If set to \p true shows not only the edges that define the silhouette of an object but also the crease edges.
    bool showCreases() const { return mShowCreases; }

    //! The minimum angle (in degrees) considered to generate crease-edges (default is 44 degrees).
    void setCreaseAngle(float degrees) { mCreaseAngle = degrees; }
    //! The minimum angle (in degrees) considered to generate crease-edges (default is 44 degrees).
    float creaseAngle() const { return mCreaseAngle; }

    //! If set to \p true the lines will be rendered using antialiasing.
    void setSmoothLines(bool smooth) { mSmoothLines = smooth; }
    //! If set to \p true the lines will be rendered using antialiasing.
    bool smoothLines() const { return mSmoothLines; }

    //! Defines the width of the rendered edges.
    void setLineWidth(float width) { mLineWidth = width; }
    //! Defines the width of the rendered edges.
    float lineWidth() const { return mLineWidth; }

    //! Defines the default color of the rendered edges. You can also define a per-Actor color using the declareActor() method.
    void setDefaultLineColor(const fvec4& c) { mDefaultLineColor = c; }
    //! Defines the default color of the rendered edges. You can also define a per-Actor color using the declareActor() method.
    const fvec4& defaultLineColor() const { return mDefaultLineColor; }

    //! Defines the \p factor parameter used to render the lines over the polygons. See also http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml for more information.
    void setPolygonOffsetFactor(float factor) { mPolygonOffsetFactor = factor; }
    //! Defines the \p factor parameter used to render the lines over the polygons. See also http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml for more information.
    float polygonOffsetFactor() const { return mPolygonOffsetFactor; }
    //! Defines the \p units parameter used to render the lines over the polygons. See also http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml for more information.
    void setPolygonOffsetUnits(float units)   { mPolygonOffsetUnits = units;   }
    //! Defines the \p units parameter used to render the lines over the polygons. See also http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml for more information.
    float polygonOffsetUnits() const  { return mPolygonOffsetUnits;  }

  protected:
    void renderSolids(Camera* camera, real frame_clock);
    void renderLines(Camera* camera);

  protected:
    std::map< ref<Actor>, ref<WFInfo> > mActorCache;
    std::map< ref<Actor>, ref<WFInfo> > mVisibleActors;
    fvec4 mDefaultLineColor;
    float mLineWidth;
    float mPolygonOffsetFactor;
    float mPolygonOffsetUnits;
    float mCreaseAngle;
    bool mShowHiddenLines;
    bool mShowCreases;
    bool mSmoothLines;
  };

}

#endif
