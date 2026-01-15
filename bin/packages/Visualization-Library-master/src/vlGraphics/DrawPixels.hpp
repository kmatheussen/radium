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

#ifndef DrawPixels_INCLUDE_ONCE
#define DrawPixels_INCLUDE_ONCE

#include <vlGraphics/Renderable.hpp>
#include <vlGraphics/ImagePBO.hpp>
#include <vlCore/Collection.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // DrawPixels
  //-----------------------------------------------------------------------------
  /**
   * Wraps the OpenGL function glDrawPixels().
   * Bind it to an Actor in order to render one or more bitmaps on the screen.
   * DrawPixels supports multiple bitmaps, can draw a subsection of a bitmap, 
   * and supports also PBOs (GL_EXT_pixel_buffer_object). See also DrawPixels::Pixels.
   *
   * \note
   * If the given Actor has an associated transform the bitmap position will follow it.
   *
   * \remarks
   * Be aware that depth test, stencil test, alpha test, clipping planes etc. 
   * can affect the rendering of the bitmap. Since this function uses the 
   * glRasterPos OpenGL function lighting, texturing and the current color might
   * affect the rendering. For more information look at the glRasterPos documentation.
  */
  //-----------------------------------------------------------------------------
  class VLGRAPHICS_EXPORT DrawPixels: public Renderable
  {
    VL_INSTRUMENT_CLASS(vl::DrawPixels, Renderable)

  public:
  //-----------------------------------------------------------------------------
  // Pixels
  //-----------------------------------------------------------------------------
    /**
     * Represents a bitmap to be drawn on the screen.
    */
    class VLGRAPHICS_EXPORT Pixels: public Object
    {
      VL_INSTRUMENT_CLASS(vl::DrawPixels::Pixels, Object)

      friend class DrawPixels;

    public:
      Pixels();
      /** Constructor.
       * The parameters 'scrx' and 'scry' define the position of the viewport in pixels where the image has to be placed.
       * If a Transform is attached to the Actor using DrawPixels scrx, scry follow the transform on the screen.
       * The parameters 'startx', 'starty', 'width' and 'height' define the sub-portion of the Image to be rendered.
       * The parameters 'width' and 'height' can be -1, in this case they will be automatically set so that the Image is shown until the top-right edge. */
      Pixels(ImagePBO* img, int scrx, int scry, int startx=0, int starty=0, int width=-1, int height=-1, int alignment = AlignBottom | AlignLeft);

      Pixels(const Pixels& other);

      Pixels& operator=(const Pixels& other);

      ~Pixels();

      const ivec2& position() const { return mPosition; }

      const ivec2& start() const { return mStart; }

      const ivec2& size() const { return mSize; }

      void setPosition( const ivec2& position) { mPosition = position; }

      void setStart( const ivec2& start) { mStart = start; }

      void setSize( const ivec2& size) { mSize = size; }

      ImagePBO* image() { return mImage.get(); }

      const ImagePBO* image() const { return mImage.get(); }

      int align() const { return mAlign; }

      void setAlign(int align) { mAlign = align; }

      /** Generates a pixel buffer object for the associated Image
       * calling image()->bufferObject()->setBufferData(usage, discard_local_storage);
       *
       * \note
       * All the Pixels object sharing the same Image will use the Image's PBO */
      bool generatePixelBufferObject(EBufferObjectUsage usage, bool discard_local_storage);

      void deletePixelBufferObject();

      bool hasPixelBufferObject() const;

    protected:
      ref<ImagePBO> mImage;
      ivec2 mPosition;
      ivec2 mStart;
      ivec2 mSize;
      int mAlign;
    };
  public:

    DrawPixels();

    void computeBounds_Implementation() { setBoundingBox(AABB()); setBoundingSphere(Sphere()); }

    /** Renders the bitamps.
     * If camera != NULL and actor != NULL and actor->transform() != NULL then 
     * the bitmaps position will follow the Actor's Transform. 
     * The \p renderer parameter is ignored. */
    void render_Implementation(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context) const;

    const Collection<Pixels>* draws() const { return &mDraws; }
    
    Collection<Pixels>* draws() { return &mDraws; }

    //! deallocate PBOs
    void deletePixelBufferObjects();

    /** Iterates on the Pixels objects and sets their Image references to NULL */
    void releaseImages();

    //! generates PBOs only for Pixels objects without a PBO handle
    bool generatePixelBufferObjects(EBufferObjectUsage usage, bool discard_local_storage);

    void setUsePixelBufferObject(bool use_pbo);

    bool usePixelBufferObject() const { return mUsePixelBufferObject; }

    // Renderable interface implementation.

    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) {}

    virtual void deleteBufferObject() {}

  protected:
    Collection<Pixels> mDraws;
    bool mUsePixelBufferObject;
  };
  //-----------------------------------------------------------------------------
}

#endif
