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

#include "ioDICOM.hpp"
#include <vlCore/LoadWriterManager.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/glsl_math.hpp>

#include <gdcmReader.h>
#include <gdcmWriter.h>
#include <gdcmAttribute.h>
#include <gdcmImageReader.h>
#include <gdcmImageWriter.h>
#include <gdcmImage.h>
#include <gdcmPhotometricInterpretation.h>
#include <gdcmImage.h>
#include <gdcmImageWriter.h>

#include <memory>

using namespace vl;

//---------------------------------------------------------------------------
static void to8bits(int bits_used, void* ptr, int px_count, bool reverse)
{
  unsigned char* px = (unsigned char*)ptr;
  if (bits_used >= 8)
    return;
  unsigned int max1 = (1<<bits_used)-1;
  unsigned int max2 = 0xFF;
  for(int i=0; i<px_count; ++i)
    if (reverse)
      px[i] = 0xFF - (unsigned char)( (float)px[i]/max1*max2 );
    else
      px[i] = (unsigned char)( (float)px[i]/max1*max2 );
}
static void to16bits(int bits_used, void* ptr, int px_count, bool reverse)
{
  unsigned short* px = (unsigned short*)ptr;

  if (bits_used >= 16)
    return;
  unsigned int max1 = (1<<bits_used)-1;
  unsigned int max2 = 0xFFFF;
  for(int i=0; i<px_count; ++i)
  {
    if (reverse)
      px[i] = 0xFFFF - (unsigned short)( (float)px[i]/max1*max2 );
    else
      px[i] = (unsigned short)( (float)px[i]/max1*max2 );
  }
}
static void to32bits(int bits_used, void* ptr, int px_count, bool reverse)
{
  unsigned int* px = (unsigned int*)ptr;
  if (bits_used >= 32)
    return;
  unsigned int max1 = (1<<bits_used)-1;
  unsigned int max2 = 0xFFFFFFFF;
  for(int i=0; i<px_count; ++i)
    if (reverse)
      px[i] = 0xFFFFFFFF - (unsigned int)( (float)px[i]/max1*max2 );
    else
      px[i] = (unsigned int)( (float)px[i]/max1*max2 );
}
//-----------------------------------------------------------------------------
ref<Image> vl::loadDICOM(const String& path)
{
  ref<VirtualFile> file = defFileSystem()->locateFile(path);
  if ( !file )
  {
    Log::error( Say("File '%s' not found.\n") << path );
    return NULL;
  }
  else
    return loadDICOM(file.get());
}
//-----------------------------------------------------------------------------
//! The retuned image has attached also some tags (vl::Image::tags()) extracted from the DICOM file describing the image geometry.
ref<Image> vl::loadDICOM(VirtualFile* vfile)
{
  gdcm::ImageReader reader;

  if (!vfile->open(OM_ReadOnly))
  {
    Log::error( Say("loadDICOM: cannot open file '%s'\n") << vfile->path() );
    return NULL;
  }

  const int bufsize = 128*1024;
  char buffer[bufsize];
  long long count = 0;
  std::stringstream strstr;
  while( (count=vfile->read(buffer, bufsize)) )
    strstr.write(buffer,(int)count);
  std::istream* istr = &strstr;
  reader.SetStream( *istr );
  if( !reader.Read() )
  {
    std::cerr << "Could not read: " << vfile->path().toStdString() << std::endl;
    vfile->close();
    return NULL;
  }

  gdcm::File &file = reader.GetFile();
  const gdcm::Image &image = reader.GetImage();

  #if 0
    printf("GDCM --- --- --- --- ---\n");
    image.Print(std::cout);
  #endif

  unsigned int ndim = image.GetNumberOfDimensions();
  const unsigned int *dims = image.GetDimensions();
  gdcm::PixelFormat pf = image.GetPixelFormat();
  const gdcm::PhotometricInterpretation &pi = image.GetPhotometricInterpretation();

  /* for debugging purposes only
  const double *origin = image.GetOrigin();
  unsigned int planar_conf = image.GetPlanarConfiguration();
  unsigned int rows = image.GetRows();
  unsigned int cols = image.GetColumns();
  unsigned int buflen = image.GetBufferLength();
  int swap = image.GetNeedByteSwap();
  int overlays = image.GetNumberOfOverlays();
  */

  ref<KeyValues> tags = new KeyValues;
  tags->set("Origin")    = Say("%n %n %n") << image.GetOrigin()[0]  << image.GetOrigin()[1]  << image.GetOrigin()[2];
  tags->set("Spacing")   = Say("%n %n %n") << image.GetSpacing()[0] << image.GetSpacing()[1] << image.GetSpacing()[2];
  tags->set("Intercept") = Say("%n") << image.GetIntercept();
  tags->set("Slope")     = Say("%n") << image.GetSlope();
  tags->set("DirectionCosines") = Say("%n %n %n %n %n %n")
                                  << image.GetDirectionCosines()[0] << image.GetDirectionCosines()[1] << image.GetDirectionCosines()[2]
                                  << image.GetDirectionCosines()[3] << image.GetDirectionCosines()[4] << image.GetDirectionCosines()[5];
  tags->set("BitsStored") = Say("%n") << pf.GetBitsStored();

  gdcm::DataSet& ds = file.GetDataSet();

  {
    gdcm::Attribute<0x28,0x1050> win_center;
    const gdcm::DataElement& de = ds.GetDataElement( win_center.GetTag() );
    if(!de.IsEmpty())
    {
      win_center.SetFromDataElement( ds.GetDataElement( win_center.GetTag() ) );
      tags->set("WindowCenter") = Say("%n") << win_center.GetValue();
    }
  }

  {
    gdcm::Attribute<0x28,0x1051> win_width;
    const gdcm::DataElement& de = ds.GetDataElement( win_width.GetTag() );
    if(!de.IsEmpty())
    {
      win_width.SetFromDataElement( ds.GetDataElement( win_width.GetTag() ) );
      tags->set("WindowWidth") = Say("%n") << win_width.GetValue();
    }
  }

  {
    gdcm::Attribute<0x28,0x1052> rescale_intercept;
    const gdcm::DataElement& de = ds.GetDataElement( rescale_intercept.GetTag() );
    if(!de.IsEmpty())
    {
      rescale_intercept.SetFromDataElement( ds.GetDataElement( rescale_intercept.GetTag() ) );
      tags->set("RescaleIntercept") = Say("%n") << rescale_intercept.GetValue();
    }
    else
      tags->set("RescaleIntercept") = Say("%n") << 0;
  }

  {
    gdcm::Attribute<0x28,0x1053> rescale_slope;
    const gdcm::DataElement& de = ds.GetDataElement( rescale_slope.GetTag() );
    if(!de.IsEmpty())
    {
      rescale_slope.SetFromDataElement( ds.GetDataElement( rescale_slope.GetTag() ) );
      tags->set("RescaleSlope") = Say("%n") << rescale_slope.GetValue();
    }
    else
      tags->set("RescaleIRescaleSlopentercept") = Say("%n") << 1;
  }

  #if 0
    printf("TAGS --- --- --- --- ---\n");
    tags->print();
  #endif

  int w=1,h=1,d=1;
  if (ndim>=1) 
    w = dims[0];
  if (ndim>=2) 
    h = dims[1];
  if (ndim>=3) 
    d = dims[2];
  h = h * d;

  ref<Image> img = new Image;
  img->setObjectName( vfile->path().toStdString().c_str() );
  if (pf.GetSamplesPerPixel() == 1 && pi == gdcm::PhotometricInterpretation::PALETTE_COLOR)
  {
    if (pf.GetBitsStored() <= 8)
    {
      img->allocate2D( w, h, 1, vl::IF_RGBA, vl::IT_UNSIGNED_BYTE);
      image.GetBuffer((char*)img->pixels());

      const gdcm::LookupTable& lut = image.GetLUT();
      std::auto_ptr<char> rgba( new char[(1<<lut.GetBitSample())*4] );
      lut.GetBufferAsRGBA((unsigned char*)rgba.get());
      for(int i=w*h; i--; )
      {
        int idx = img->pixels()[i];
        img->pixels()[i*4+0] = rgba.get()[idx*4+0];
        img->pixels()[i*4+1] = rgba.get()[idx*4+1];
        img->pixels()[i*4+2] = rgba.get()[idx*4+2];
        img->pixels()[i*4+3] = rgba.get()[idx*4+3];
      }
    }
    else
    if (pf.GetBitsStored() <= 16)
    {
      img->allocate2D( w, h, 1, vl::IF_RGBA, vl::IT_UNSIGNED_BYTE);
      image.GetBuffer((char*)img->pixels());

      const gdcm::LookupTable& lut = image.GetLUT();
      std::auto_ptr<char> rgba( new char[(1<<lut.GetBitSample())*4] );
      lut.GetBufferAsRGBA((unsigned char*)rgba.get());
      for(int i=w*h; i--; )
      {
        int idx = ((unsigned short*)img->pixels())[i];
        img->pixels()[i*4+0] = rgba.get()[idx*4+0];
        img->pixels()[i*4+1] = rgba.get()[idx*4+1];
        img->pixels()[i*4+2] = rgba.get()[idx*4+2];
        img->pixels()[i*4+3] = rgba.get()[idx*4+3];
      }
    }
  }
  else
  if (pf.GetSamplesPerPixel() == 1 && (pi == gdcm::PhotometricInterpretation::MONOCHROME2 || pi == gdcm::PhotometricInterpretation::MONOCHROME1))
  {
    if (pf.GetBitsStored() <= 8)
    {
      img->allocate2D( w, h, 1, vl::IF_LUMINANCE, vl::IT_UNSIGNED_BYTE);
      image.GetBuffer((char*)img->pixels());
      if (pi == gdcm::PhotometricInterpretation::MONOCHROME1)
        to8bits(pf.GetBitsStored(), img->pixels(), w*h, true);
      else
        to8bits(pf.GetBitsStored(), img->pixels(), w*h, false);
    }
    else
    if (pf.GetBitsStored() <= 16)
    {
      img->allocate2D( w, h, 1, vl::IF_LUMINANCE, vl::IT_UNSIGNED_SHORT);
      image.GetBuffer((char*)img->pixels());
      if (pi == gdcm::PhotometricInterpretation::MONOCHROME1)
        to16bits(pf.GetBitsStored(), img->pixels(), w*h, true);
      else
        to16bits(pf.GetBitsStored(), img->pixels(), w*h, false);
    }
    else
    if (pf.GetBitsStored() <= 32)
    {
      img->allocate2D( w, h, 1, vl::IF_LUMINANCE, vl::IT_UNSIGNED_INT);
      image.GetBuffer((char*)img->pixels());
      if (pi == gdcm::PhotometricInterpretation::MONOCHROME1)
        to32bits(pf.GetBitsStored(), img->pixels(), w*h, true);
      else
        to32bits(pf.GetBitsStored(), img->pixels(), w*h, false);
    }
  }
  else
  if (pf.GetSamplesPerPixel() == 3)
  {
    if (pf.GetBitsStored() <= 8)
    {
      img->allocate2D( w, h, 1, vl::IF_RGB, vl::IT_UNSIGNED_BYTE);
      image.GetBuffer((char*)img->pixels());
      if (image.GetPlanarConfiguration())
      {
        int slice_pix_count = w*h/d;
        std::auto_ptr<char> px ( new char[slice_pix_count*3] );
        for(int slice=0; slice<d; ++slice)
        {
          char* red   = (char* )img->pixels() + slice*slice_pix_count*3 + 0;
          char* green = (char* )img->pixels() + slice*slice_pix_count*3 + slice_pix_count;
          char* blue  = (char* )img->pixels() + slice*slice_pix_count*3 + slice_pix_count*2;
          for(int i=0; i<slice_pix_count*3; i+=3, ++red, ++green, ++blue)
          {
            px.get()[i+0] = *red;
            px.get()[i+1] = *green;
            px.get()[i+2] = *blue;
          }
          memcpy(img->pixels(), px.get(), img->requiredMemory());
        }
      }
    }
    else
    if (pf.GetBitsStored() <= 16)
    {
      img->allocate2D( w, h, 1, vl::IF_RGB, vl::IT_UNSIGNED_SHORT);
      image.GetBuffer((char*)img->pixels());
      if (image.GetPlanarConfiguration())
      {
        int slice_pix_count = w*h/d;
        std::auto_ptr<short> px ( new short[slice_pix_count*3] );
        for(int slice=0; slice<d; ++slice)
        {
          short* red   = (short* )img->pixels() + slice*slice_pix_count*3 + 0;
          short* green = (short* )img->pixels() + slice*slice_pix_count*3 + slice_pix_count;
          short* blue  = (short* )img->pixels() + slice*slice_pix_count*3 + slice_pix_count*2;
          for(int i=0; i<slice_pix_count*3; i+=3, ++red, ++green, ++blue)
          {
            px.get()[i+0] = *red;
            px.get()[i+1] = *green;
            px.get()[i+2] = *blue;
          }
          memcpy(img->pixels(), px.get(), img->requiredMemory());
        }
      }
    }
    else
    {
      vl::Log::error("DICOM format not supported: SamplesPerPixel = 3, BitsStored > 16.\n\n");
      image.Print(std::cout);
      vfile->close();
      return NULL;
    }
  }
  else
  {
    vl::Log::error("DICOM format not supported.\n");
    image.Print(std::cout);
    vfile->close();
    return NULL;
  }
  vfile->close();
  img->setHeight(h/d);
  img->setDepth(d>1?d:0);
  img->setTags(tags.get());
  img->flipVertically();
  return img;
}
//---------------------------------------------------------------------------
bool vl::isDICOM(VirtualFile* file)
{
  file->open(OM_ReadOnly);
  file->seekSet(128);
  char signature[] = { 'D', 'I', 'C', 'M' };
  char buf[]       = { 0,   0,   0,   0   };
  file->read(buf,4);
  bool ok = memcmp(buf,signature,4) == 0;
  file->close();
  return ok;
}
//---------------------------------------------------------------------------
bool vl::saveDICOM(const Image* src, const String& path)
{
  return saveDICOM(src, new DiskFile(path));
}
//-----------------------------------------------------------------------------
bool vl::saveDICOM(const Image* src, VirtualFile* fout)
{
  if (src->dimension()<1 || src->dimension()>3)
  {
    vl::Log::error("saveDICOM(): uncompatible image dimension().\n");
    return false;
  }

  ref<Image> img = new Image;
  *img = *src;

  // bytes per sample
  unsigned short bps = 0; 
  switch(img->type())
  {
  case vl::IT_UNSIGNED_BYTE:  bps = 1; break;
  case vl::IT_UNSIGNED_SHORT: bps = 2; break;
  default:
    vl::Log::error("saveDICOM(): unsupported image type().\n");
    return false;
  }

  // sample count;
  unsigned short spp = 0;
  switch(img->format())
  {
    case vl::IF_ALPHA:     spp = 1; break;
    case vl::IF_LUMINANCE: spp = 1; break;
    case vl::IF_LUMINANCE_ALPHA: spp = 1; break; // converted to IF_LUMINANCE
    case vl::IF_RGB:  spp = 3; break;
    case vl::IF_BGR:  spp = 3; break; // converted to IF_RGB
    case vl::IF_RGBA: spp = 3; break; // converted to IF_RGB
    case vl::IF_BGRA: spp = 3; break; // converted to IF_RGB
    default:
      vl::Log::error("saveDICOM(): unsupported image format().\n");
      return false;
  }

  if (img->format() == vl::IF_LUMINANCE_ALPHA)
    img = img->convertFormat(vl::IF_LUMINANCE);
  if (img->format() == vl::IF_BGR)
    img = img->convertFormat(vl::IF_RGB);
  if (img->format() == vl::IF_BGRA || img->format() == vl::IF_RGBA)
    img = img->convertFormat(vl::IF_RGB);

  gdcm::SmartPointer<gdcm::Image> im = new gdcm::Image;
  im->SetNumberOfDimensions( img->dimension() );
  int dims[] = { img->width(), img->height()?img->height():1, img->depth()?img->depth():1 };
  for(int i=0; i<img->dimension(); ++i)
    im->SetDimension( i, dims[i] );

  im->GetPixelFormat().SetScalarType(bps==8?gdcm::PixelFormat::INT8:gdcm::PixelFormat::INT16);
  im->GetPixelFormat().SetBitsAllocated(bps*8);
  im->GetPixelFormat().SetBitsStored(bps*8);
  im->GetPixelFormat().SetHighBit(bps*8-1);
  im->GetPixelFormat().SetSamplesPerPixel(spp);
  gdcm::PhotometricInterpretation pi = spp==1?gdcm::PhotometricInterpretation::MONOCHROME2:gdcm::PhotometricInterpretation::RGB;
  im->SetPhotometricInterpretation(pi);
  // im->SetTransferSyntax(gdcm::TransferSyntax::JPEGBaselineProcess1);

  unsigned long len = im->GetBufferLength();
  unsigned long req = img->requiredMemory();
  if( len != req )
  {
    // is a img->byteAlignment() issue?
    vl::Log::error("saveDICOM(): buffer size computation error.\n");
    VL_TRAP()
    return false;
  }

  img->flipVertically();

  gdcm::DataElement pixeldata( gdcm::Tag(0x7fe0,0x0010) );
  pixeldata.SetByteValue( (const char*)img->pixels(), len );
  im->SetDataElement( pixeldata );

  gdcm::ImageWriter w;
  w.SetImage( *im );
  std::ostringstream ostr;
  w.SetStream(ostr);
  if( !w.Write() )
  {
    vl::Log::error("saveDICOM(): error writing stream.\n");
    return false;
  }
  fout->open(OM_WriteOnly);
  int bcount = (int)fout->write(ostr.str().c_str(), ostr.str().length());
  fout->close();
  if( bcount != (int)ostr.str().length() )
  {
    vl::Log::error("saveDICOM(): error writing file.\n");
    return false;
  }
  
  return true;
}
