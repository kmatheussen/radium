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

#if !defined(ioDICOM_INCLUDE_ONCE)
#define ioDICOM_INCLUDE_ONCE

#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/Image.hpp>

namespace vl
{
  class VirtualFile;
  class String;

  //! Loads a DICOM file.
  VLCORE_EXPORT ref<Image> loadDICOM(VirtualFile* file);
  //! Loads a DICOM file.
  VLCORE_EXPORT ref<Image> loadDICOM(const String& path);
  //! Writes a DICOM file.
  VLCORE_EXPORT bool saveDICOM(const Image* src, const String& path);
  //! Writes a DICOM file.
  VLCORE_EXPORT bool saveDICOM(const Image* src, VirtualFile* file);
  //! Checks if the given file is a DICOM file.
  VLCORE_EXPORT bool isDICOM(VirtualFile* file);

  //---------------------------------------------------------------------------
  // LoadWriterDICOM
  //---------------------------------------------------------------------------
  /** The LoadWriterDICOM class is a ResourceLoadWriter capable of reading DICOM files (.dcm, .dicom, .dic, .ima, .ph, .mag) using GDCM 2.0 library.
  \sa GDCM home page: http://creatis-www.insa-lyon.fr/software/public/Gdcm/Main.html */
  class LoadWriterDICOM: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterDICOM, ResourceLoadWriter)

  public:
    LoadWriterDICOM(): ResourceLoadWriter("|dcm|dicom|dic|ima|ph|mag|", "|dcm|dicom|dic|ima|ph|mag|") 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      ref<ResourceDatabase> res_db = new ResourceDatabase;
      ref<Image> img = loadDICOM(path);
      if (img)
        res_db->resources().push_back(img);
      return res_db;
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      ref<ResourceDatabase> res_db = new ResourceDatabase;
      ref<Image> img = loadDICOM(file);
      if (img)
        res_db->resources().push_back(img);
      return res_db;
    }

    bool writeResource(const String&, ResourceDatabase*) const
    {
      return false;
    }

    bool writeResource(VirtualFile*, ResourceDatabase*) const
    {
      return false;
    }
  };
}

#endif

