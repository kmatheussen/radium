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

#ifndef Image_INCUDE_ONCE
#define Image_INCUDE_ONCE

#include <vlCore/String.hpp>
#include <vlCore/Buffer.hpp>
#include <vlCore/Vector4.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/Rect.hpp>
#include <vlCore/KeyValues.hpp>
#include <vector>

namespace vl
{
  class VirtualFile;

  //------------------------------------------------------------------------------
  // Image
  //------------------------------------------------------------------------------
  /** Implements a generic 1d, 2d, 3d and cubemap image that can have mipmaps.
   *
   * \remarks The copy operator performs a deep copy of all the pixel data and thus
   * is to be considered an expensive operation. */
  class VLCORE_EXPORT Image: public Object
  {
    VL_INSTRUMENT_CLASS(vl::Image, Object)

  public:
    virtual ~Image();

    Image();

    Image(const String& path);

    Image(int x, int y, int z, int bytealign, EImageFormat format, EImageType type);

    Image(const Image& other);

    Image& operator=(const Image& other);

    bool isCubemap() const { return mIsCubemap; }

    bool isValid() const;

    EImageDimension dimension() const;

    void allocate();

    void allocate1D(int x, EImageFormat format, EImageType type);

    void allocate2D(int x, int y, int bytealign, EImageFormat format, EImageType type);

    void allocate3D(int x, int y, int z, int bytealign, EImageFormat format, EImageType type);

    void allocateCubemap(int x, int y, int bytealign, EImageFormat format, EImageType type);

    //! Sets up the image configuration & clears the local storage
    void reset(int x, int y, int z, int bytealign, EImageFormat format, EImageType type, bool is_cubemap);

    //! Disposes all the pixel data and resets the image settings to its defaults.
    void reset();

    int byteAlignment() const;

    void setByteAlignment(int bytealign);

    static int bitsPerPixel(EImageType type, EImageFormat format);

    int bitsPerPixel() const { return bitsPerPixel(type(),format()); }

    int requiredMemory() const;

    static int requiredMemory(int x, int y, int z, int bytealign, EImageFormat format, EImageType type, bool is_cubemap);

    static int requiredMemory1D(int x, EImageFormat format, EImageType type) { return requiredMemory(x, 0, 0, 1, format, type, false); } 

    static int requiredMemory2D(int x, int y, int bytealign, EImageFormat format, EImageType type) { return requiredMemory(x, y, 0, bytealign, format, type, false); } 

    static int requiredMemory3D(int x, int y, int z, int bytealign, EImageFormat format, EImageType type) { return requiredMemory(x, y, z, bytealign, format, type, false); } 

    static int requiredMemoryCubemap(int x, int y, int bytealign, EImageFormat format, EImageType type) { return requiredMemory(x, y, 0, bytealign, format, type, true); } 

    String print() const;

    String printType() const;

    String printFormat() const;
    
    void setWidth(int x) { mWidth = x; updatePitch(); }
    
    void setHeight(int y) { mHeight = y; }
    
    void setDepth(int z) { mDepth = z; }
    
    void setFormat(EImageFormat format) { mFormat = format; updatePitch(); }
    
    void setType(EImageType type) { mType=type; updatePitch(); }

    //! Whether an image contains relevant alpha information.
    bool hasAlpha() const { return mHasAlpha; }
    
    //! Whether an image contains relevant alpha information.
    void setHasAlpha(bool has_alpha) { mHasAlpha = has_alpha; }

    //! The number of bits dedicated to the alpha channel.
    int alphaBits() const;

    //! Whether an image represents a normal map.
    bool isNormalMap() const { return mIsNormalMap; }

    //! Whether an image represents a normal map.
    void setIsNormalMap(bool is_normalmap) { mIsNormalMap = is_normalmap; }

    //! A set of key/value couples that can be used to attach extra information to an image like DICOM information etc.
    //! Returns NULL by default.
    const KeyValues* tags() const { return mTags.get(); }

    //! A set of key/value couples that can be used to attach extra information to an image like DICOM information etc.
    //! Returns NULL by default.
    KeyValues* tags() { return mTags.get(); }

    //! A set of key/value couples that can be used to attach extra information to an image like DICOM information etc.
    void setTags(KeyValues* tags) { mTags = tags; }

    //! The buffer used to store the image pixels.
    void setImageBuffer(Buffer* buffer) { mPixels = buffer; }

    //! The buffer used to store the image pixels.
    Buffer* imageBuffer() { return mPixels.get(); }

    //! The buffer used to store the image pixels.
    const Buffer* imageBuffer() const { return mPixels.get(); }

    const unsigned char* pixels() const { if (mPixels->bytesUsed()) return mPixels->ptr(); else return NULL; }
    
    unsigned char* pixels() { if (mPixels->bytesUsed()) return mPixels->ptr(); else return NULL; }
    
    bool empty() { return pixels() == NULL; }
    
    unsigned char* pixelsZSlice(int slice);
    
    unsigned char* pixelsXP();
    unsigned char* pixelsXN();
    unsigned char* pixelsYP();
    unsigned char* pixelsYN();
    unsigned char* pixelsZP();
    unsigned char* pixelsZN();
    
    const unsigned char* pixelsXP() const;
    const unsigned char* pixelsXN() const;
    const unsigned char* pixelsYP() const;
    const unsigned char* pixelsYN() const;
    const unsigned char* pixelsZP() const;
    const unsigned char* pixelsZN() const;

    void setMipmaps(const std::vector< ref<Image> >& mipmaps) { mMipmaps = mipmaps; };
    
    const std::vector< ref<Image> >& mipmaps() const { return mMipmaps; };
    
    std::vector< ref<Image> >& mipmaps() { return mMipmaps; };
    
    int width() const { return mWidth; }
    
    int height() const { return mHeight; }
    
    int depth() const { return mDepth; }
    
    int pitch() const { return mPitch; }
    
    EImageFormat format() const { return mFormat; }
    
    EImageType type() const { return mType; }
    
    int isCompressedFormat(EImageFormat fmt);

    void flipVertically();

    /**
     * Converts the \p type() of an image.
     *
     * The source image type and the new type must be one of the following:
     * - IT_UNSIGNED_BYTE
     * - IT_BYTE         
     * - IT_UNSIGNED_SHORT
     * - IT_SHORT         
     * - IT_UNSIGNED_INT  
     * - IT_INT           
     * - IT_FLOAT
     *
     * The source image format must be one of the following:
     * - IF_RGB
     * - IF_RGBA
     * - IF_BGR
     * - IF_BGRA
     * - IF_RED
     * - IF_GREEN
     * - IF_BLUE
     * - IF_ALPHA
     * - IF_LUMINANCE
     * - IF_LUMINANCE_ALPHA
     * - IF_DEPTH_COMPONENT
    */
    ref<Image> convertType(EImageType new_type) const;

    /**
     * Converts the \p format() of an image.
     *
     * The source image type must be one of the following:
     * - IT_UNSIGNED_BYTE
     * - IT_BYTE         
     * - IT_UNSIGNED_SHORT
     * - IT_SHORT         
     * - IT_UNSIGNED_INT  
     * - IT_INT           
     * - IT_FLOAT
     *
     * The source image format and the new format must be one of the following:
     * - IF_RGB
     * - IF_RGBA
     * - IF_BGR
     * - IF_BGRA
     * - IF_RED
     * - IF_GREEN
     * - IF_BLUE
     * - IF_ALPHA
     * - IF_LUMINANCE
     * - IF_LUMINANCE_ALPHA
     */
    ref<Image> convertFormat(EImageFormat new_format) const;

    //! Equalizes the image. Returns false if the image format() or type() is not supported. This function supports both 3D images and cubemaps.
    bool equalize();

    //! Adjusts the contrast of an image. Returns false if the image format() or type() is not supported. This function supports both 3D images and cubemaps.
    bool contrast(float black, float white);

    //! Adjusts the contrast of an image using the window-center/window-with method used for CT images. Returns false if the image format() or type() is not supported. This function supports both 3D images and cubemaps.
    bool contrastHounsfieldAuto();

    //! Adjusts the contrast of an image using the window-center/window-with method used for CT images. Returns false if the image format() or type() is not supported. This function supports both 3D images and cubemaps.
    bool contrastHounsfield(float center, float width, float intercept, float range);

    //! Performs a sampling on a 1d image using linear filtering.
    fvec4 sampleLinear(double x) const;

    //! Performs a sampling on a 2d image using bilinear filtering.
    fvec4 sampleLinear(double x, double y) const;

    //! Performs a sampling on a 3d image using trilinear filtering.
    fvec4 sampleLinear(double x, double y, double z) const;

    /**
     * Returns the color associated to the specified pixel.
     *
     * The rgb values are mapped to 0..1 for all image types but IT_FLOAT.
     * The value returned for images of type IT_FLOAT is returned exactly as is stored in the image.
     *
     * The image type() must be one of the following:
     * - IT_UNSIGNED_BYTE
     * - IT_BYTE         
     * - IT_UNSIGNED_SHORT
     * - IT_SHORT         
     * - IT_UNSIGNED_INT  
     * - IT_INT           
     * - IT_FLOAT
     *
     * The image format() must be one of the following:
     * - IF_RGB
     * - IF_RGBA
     * - IF_BGR
     * - IF_BGRA
     * - IF_RED
     * - IF_GREEN
     * - IF_BLUE
     * - IF_ALPHA
     * - IF_LUMINANCE
     * - IF_LUMINANCE_ALPHA
     * - IF_DEPTH_COMPONENT
     */
    fvec4 sample(int x, int y=0, int z=0) const;

    /**
     * Creates a new image containing the specified rectangular pixel area taken from the source image.
     * The returned image is of the same type() and format() of the original one.
     * \note 
     * - This function supports only 2d images.
     * - This function does not support compressed types and formats.
    */
    ref<Image> subImage(int xstart, int ystart, int width, int height);

    /**
     * Copies the rectangular area specified by \p src of \p img_src into an Image at position \p dst.
     * The source and destination image should be of the same type() and format() for maximum performances.
     */
    void copySubImage(Image* img_src, RectI src, ivec2 dst);

    /**
     * Substitutes the color 'before' with the new color 'after'.
     * \param before is an hexadecimal representation of an RGB triplet given in the form 0xRRGGBB.
     * \param after is an hexadecimal representation of an RGBA quadruplet given in the form 0xRRGGBBAA.
     * For example 0xFF0000FF is opaque red, 0x00FF0088 is half transparent green.
     * This function can be very useful when you want to modify a specified color of an image or when you want to perform color-key
     * transparency, i.e. when you want to set the transparency of the pixels that have a particular color.
     * \note
     * - This function can be used only if the image type() is IT_UNSIGNED_BYTE and format() is either IF_RGB or IF_RGBA.
     * - If the image format() is set to IF_RGB then the alpha value specified in 'after' is ignored.
     * - If you want to be sure to keep the alpha channel intact use the substituteColorRGB_RGB() function instead.
     */
    void substituteColorRGB_RGBA(unsigned int before, unsigned int after);

    /**
     * Substitutes the color 'before' with the new color 'after'.
     * \param before is an hexadecimal representation of an RGB triplet given in the form 0xRRGGBB.
     * \param after is an hexadecimal representation of an RGB tripet given in the form 0xRRGGBB.
     * For example 0xFF0000 is opaque red, 0x00FF00 is green.
     * This function can be very useful when you want to modify a specified color of an image but you want to keep the alpha channel intact.
     * \note
     * - This function can be used only if the image type() is IT_UNSIGNED_BYTE and format() is either IF_RGB or IF_RGBA.
     * - If you want to change the alpha channel of the modified pixels use the substituteColorRGB_RGBA() function instead.
     */
    void substituteColorRGB_RGB(unsigned int before, unsigned int after);

    void substituteColorGreenKey(unsigned int col0, unsigned int col1);

    /** The file from which the image was loaded. */
    const String& filePath() const { return mFilePath; }

    /** The file from which the image was loaded. */
    void setFilePath(const String& path) { mFilePath = path; }

  protected:
    void updatePitch();

  protected:
    ref<Buffer> mPixels;
    ref<KeyValues> mTags;
    String mFilePath;
    std::vector< ref<Image> > mMipmaps;
    int mWidth;
    int mHeight;
    int mDepth;
    int mPitch;
    int mByteAlign;
    EImageFormat mFormat;
    EImageType mType;
    bool mIsCubemap;
    bool mIsNormalMap;
    bool mHasAlpha;
  };

  //! Assembles a cubemap image.
  ref<Image> createCubemap(const Image* xp, const Image* xn, const Image* yp, const Image* yn, const Image* zp, const Image* zn);

  //! Loads six images and assembles them into a cubemap image
  VLCORE_EXPORT ref<Image> loadCubemap(const String& xp_file, const String& xn_file, const String& yp_file, const String& yn_file, const String& zp_file, const String& zn_file);

  //! Loads a raw image file.
  //! \param file The file from which the data is read. This function also opens the file if it is not open already. Note that this function 
  //! never closes the file so that you can read sequentially several raw image data from the same file.
  //! \param file_offset The offset in the file from where the data is read. If set to -1 the data is read from the current file position.
  VLCORE_EXPORT ref<Image> loadRAW(VirtualFile* file, long long file_offset, int width, int height, int depth, int bytealign, EImageFormat format, EImageType type);

  //! Loads an image from the specified file
  VLCORE_EXPORT ref<Image> loadImage(VirtualFile* file);

  //! Loads an image from the specified path
  VLCORE_EXPORT ref<Image> loadImage(const String& path);

  //! Loads all the images with the specified extension from the given directory.
  VLCORE_EXPORT bool loadImagesFromDir(const String& dir_path, const String& ext, std::vector< ref<Image> >& images);

  //! Assembles the given 2D images in a single 2D image, all the images must be 2D images and have the same size, format() and type().
  VLCORE_EXPORT ref<Image> assemble3DImage(const std::vector< ref<Image> >& images);

  //! Writes an image on the specified file
  VLCORE_EXPORT bool saveImage(Image* img, VirtualFile* file);

  //! Writes an image on the specified path
  VLCORE_EXPORT bool saveImage(Image* img, const String& path);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeNonUniformColorSpectrum(size_t width, size_t col_count, const fvec4* colors, const float* col_pos);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeNonUniformColorSpectrum(int width, const std::vector<fvec4>& colors, const std::vector<float>& col_pos);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeColorSpectrum(size_t width, const std::vector<fvec4>& colors);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2, const fvec4& c3);

  //! Creates a 1D Image whose color is interpolated from left to right from the specified spectrum.
  VLCORE_EXPORT ref<Image> makeColorSpectrum(size_t width, const fvec4& c0, const fvec4& c1, const fvec4& c2, const fvec4& c3, const fvec4& c4);
}

#endif
