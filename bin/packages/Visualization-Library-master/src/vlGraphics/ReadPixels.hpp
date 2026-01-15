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

#ifndef CameraReadPixels_INCLUDE_ONCE
#define CameraReadPixels_INCLUDE_ONCE

#include <vlGraphics/Camera.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Log.hpp>
#include <vlGraphics/RenderEventCallback.hpp>
#include <vlGraphics/BufferObject.hpp>
#include <vlCore/Image.hpp>

namespace vl
{
  //-----------------------------------------------------------------------------
  // vl::readPixels()
  //-----------------------------------------------------------------------------
  /** Reads a rectangular pixel area from the specified read buffer and stores it in an Image.
   *
   * If 'store_in_pixel_buffer_object' is true the data will be copied in the GPU memory using the GL_EXT_pixel_buffer_object
   * extension, while the local buffer will be deallocated.
   *
   * \note
   * The image returned by this function might seem flipped upside down. */
  inline void readPixels(Image* image, int x, int y, int w, int h, EReadDrawBuffer read_buffer, bool store_in_pixel_buffer_object)
  {
    // clears OpenGL errors
    glGetError();

#if defined(VL_OPENGL)
    glPushClientAttrib(GL_CLIENT_PIXEL_STORE_BIT);
#endif

    glPixelStorei( GL_PACK_ALIGNMENT,   1);
#if defined(VL_OPENGL)
    glPixelStorei( GL_PACK_ROW_LENGTH,  w);
    glPixelStorei( GL_PACK_SKIP_PIXELS, 0);
    glPixelStorei( GL_PACK_SKIP_ROWS,   0);
    glPixelStorei( GL_PACK_SWAP_BYTES,  0);
    glPixelStorei( GL_PACK_LSB_FIRST,   0);
    if (Has_GL_Version_1_2||Has_GL_Version_3_0||Has_GL_Version_4_0) // Texture 3D but excluding EOS
    {
      glPixelStorei( GL_PACK_IMAGE_HEIGHT, 0 );
      glPixelStorei( GL_PACK_SKIP_IMAGES,  0 );
    }

    int prev = 0;
    glGetIntegerv( GL_READ_BUFFER, &prev ); VL_CHECK_OGL()
    glReadBuffer( read_buffer );
#endif

    #ifndef NDEBUG
      if (glGetError() != GL_NO_ERROR)
      {
        Log::print(Say("Image::readPixels() error: %s:%n\n"
          "You seem to have specified a wrong read buffer for the bound render target,\n"
          "for example you might have specified a RDB_BACK_LEFT but the active camera\n"
          "is bound to a FBO (Framebuffer Object) render target or the selected\n"
          "read buffer is FBO-specific (like RDB_COLOR_ATTACHMENT0_EXT) but the active\n"
          "camera is not bound to a FBO render target. \n") << __FILE__ << __LINE__
        );
        VL_TRAP()
      }
    #endif

    bool supports_pbo = Has_GL_ARB_pixel_buffer_object||Has_GL_EXT_pixel_buffer_object||Has_GL_Version_2_1;

    if (store_in_pixel_buffer_object && supports_pbo)
    {
      // mic fixme: test
      BufferObject* glbuf = cast<BufferObject>(image->imageBuffer()); VL_CHECK(glbuf);

      int bytes = image->requiredMemory2D(w, h, 1, image->format(), image->type());
      // allocates the PBO
      glbuf->setBufferData( bytes, NULL, glbuf->usage() );
      // bind the pbo
      VL_glBindBuffer( GL_PIXEL_PACK_BUFFER, glbuf->handle() );
      // read pixels into the pbo
      glReadPixels( x, y, w, h, image->format(), image->type(), 0);
      // unbind the pbo
      VL_glBindBuffer( GL_PIXEL_PACK_BUFFER, 0 );

      // deallocates the local storage and sets up the image configuration
      image->reset(w, h, 0, 1, image->format(), image->type(), false);

      // test GPU -> local copy
      //if (w != image->width() || h != image->height() || image->dimension() != ID_2D || image->pixels() == NULL)
      //  image->allocate2D( w, h, 1, image->format(), image->type() );
      //glbuf->downloadBufferObject();
    }
    else
    {
      if (w != image->width() || h != image->height() || image->dimension() != ID_2D || image->pixels() == NULL)
        image->allocate2D( w, h, 1, image->format(), image->type() );
      glReadPixels( x, y, w, h, image->format(), image->type(), image->pixels() );
    }

#if defined(VL_OPENGL)
    // restore read buffer
    glReadBuffer( prev );

    glPopClientAttrib();
#endif

    VL_CHECK_OGL()
  }
  //-----------------------------------------------------------------------------
  // ReadPixels
  //-----------------------------------------------------------------------------
  /**
   * A RenderEventCallback that copyes a rectangular pixel area from a source 
   * buffer to an Image at the end of a rendering.
   *
   * The actual copy is performed using the function Image::readPixels(). 
   * Using the function setSavePath() the Image will be saved
   * on the specified location.
   *
   * \sa
   * - Image
   * - Image::readPixels()
   * - RenderEventCallback
  */
  class ReadPixels: public RenderEventCallback
  {
    VL_INSTRUMENT_CLASS(vl::ReadPixels, RenderEventCallback)

  public:
    ReadPixels():
      mX ( 0 ),
      mY ( 0 ),
      mWidth ( 0 ),
      mHeight ( 0 ),
      mReadBuffer ( RDB_BACK_LEFT ),
      mStoreInPixelBufferObject(false)
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    ReadPixels(int x, int y, int width, int height, EReadDrawBuffer read_buffer, Image* image, bool store_in_pbo):
      mX ( x ),
      mY ( y ),
      mWidth  ( width ),
      mHeight ( height ),
      mReadBuffer ( read_buffer ),
      mImage ( image ),
      mStoreInPixelBufferObject( store_in_pbo )
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    virtual bool onRenderingStarted(const RenderingAbstract*)
    {
      readPixels();
      return true;
    }

    virtual bool onRenderingFinished(const RenderingAbstract*)
    {
      readPixels();
      return true;
    }

    virtual bool onRendererStarted(const RendererAbstract*)
    {
      readPixels();
      return true;
    }

    virtual bool onRendererFinished(const RendererAbstract*)
    {
      readPixels();
      return true;
    }

    void setup(int x, int y, int width, int height, EReadDrawBuffer read_buffer, bool store_in_pbo)
    {
      mX = x;
      mY = y;
      mWidth  = width;
      mHeight = height;
      mReadBuffer = read_buffer;
      mStoreInPixelBufferObject = store_in_pbo;
    }

    void setX(int x) { mX = x; }
    void setY(int y) { mY = y; }
    void setWidth(int width) { mWidth = width; }
    void setHeight(int height) { mHeight = height; }
    void setReadBuffer(EReadDrawBuffer buffer) { mReadBuffer = buffer; }
    void setImage(Image* image) { mImage = image; }
    void setSavePath(const String& path) { mSavePath = path; }

    int x() const { return mX; }
    int y() const { return mY; }
    int width() const { return mWidth; }
    int height() const { return mHeight; }
    EReadDrawBuffer readBuffer() const { return mReadBuffer; }
    Image* image() { return mImage.get(); }
    const Image* image() const { return mImage.get(); }
    const String& savePath() const { return mSavePath; }

    void setStoreInPixelBufferObject( bool use_pbo ) { mStoreInPixelBufferObject = use_pbo; }
    bool storeInPixelBufferObject() const { return mStoreInPixelBufferObject; }

  protected:
    void readPixels()
    {
      if (mImage.get() == NULL)
        mImage = new Image;
      vl::readPixels(mImage.get(), mX, mY, mWidth, mHeight, mReadBuffer, storeInPixelBufferObject() );
      if ( savePath().length() )
      {
        if (!saveImage(mImage.get(), savePath()))
          Log::error( Say("ReadPixels: unknown format for file: '%s'\n") << savePath() );
      }
    }

  protected:
    int mX;
    int mY;
    int mWidth;
    int mHeight;
    EReadDrawBuffer mReadBuffer;
    ref<Image> mImage;
    String mSavePath;
    bool mStoreInPixelBufferObject;
  };
}

#endif
