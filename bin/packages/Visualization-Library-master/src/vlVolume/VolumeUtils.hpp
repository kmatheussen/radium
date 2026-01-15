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

#ifndef VolumeUtils_INCLUDE_ONCE
#define VolumeUtils_INCLUDE_ONCE

#include <vlVolume/link_config.hpp>
#include <vlCore/Image.hpp>

namespace vl
{
  /** Generates an RGBA image based on the given data source and transfer function.
   * \param data The Image used as the volume data source. It must have format() equal to IF_LUMINANCE and type() equal to IT_UNSIGNED_BYTE, IT_UNSIGNED_SHORT or IT_FLOAT.
   * \param trfunc An 1D Image used as transfer function that is used to assign to each value in \p data an RGBA value in the new image.
   * The Image pointed by \p trfunc must mast have type() \p IT_UNSIGNED_BYTE and format() \p IF_RGBA.
   * \param light_dir The direction of the light in object space.
   * \param alpha_from_data If set to true the \p alpha channel of the generated image will be taken from \p data otherwise from the transfer function. */
  VLVOLUME_EXPORT ref<Image> genRGBAVolume(const Image* data, const Image* trfunc, const fvec3& light_dir, bool alpha_from_data=true);

  /** Generates an RGBA image based on the given data source and transfer function.
   * \param data The Image used as the volume data source. It must have format() equal to IF_LUMINANCE and type() equal to IT_UNSIGNED_BYTE, IT_UNSIGNED_SHORT or IT_FLOAT.
   * \param trfunc An 1D Image used as transfer function that is used to assign to each value in \p data an RGBA value in the new image.
   * The Image pointed by \p trfunc must mast have type() \p IT_UNSIGNED_BYTE and format() \p IF_RGBA.
   * \param alpha_from_data If set to true the \p alpha channel of the generated image will be taken from \p data otherwise from the transfer function.
   *
   * Unlike genRGBAVolume(Image* data, Image* trfunc, const fvec3& light_dir, bool alpha_from_data=true) this function does not 
   * compute lighting. */
  VLVOLUME_EXPORT ref<Image> genRGBAVolume(const Image* data, const Image* trfunc, bool alpha_from_data=true);

  /** Generates an image whose RGB components represent the normals computed from the input image gradient packed into 0..1 range. 
  * The format of the image is IF_RGB/IT_FLOAT which is equivalent to a 3D grid of fvec3.
  * The generated image is ready to be used as a texture for normal lookup. 
  * The original normal can be recomputed as N = (RGB - 0.5)*2.0. */
  VLVOLUME_EXPORT ref<Image> genGradientNormals(const Image* data);

  /** Internally used. */
  template<typename data_type, EImageType img_type>
  VLVOLUME_EXPORT ref<Image> genRGBAVolumeT(const Image* data, const Image* trfunc, const fvec3& light_dir, bool alpha_from_data);
  
  /** Internally used. */
  template<typename data_type, EImageType img_type>
  VLVOLUME_EXPORT ref<Image> genRGBAVolumeT(const Image* data, const Image* trfunc, bool alpha_from_data);
}

#endif
