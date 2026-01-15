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

#if !defined(ioDDS_INCLUDE_ONCE)
#define ioDDS_INCLUDE_ONCE

#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/Image.hpp>

namespace vl
{
  class VirtualFile;
  class String;
  class Image;

  VLCORE_EXPORT ref<Image> loadDDS(VirtualFile* file);
  VLCORE_EXPORT ref<Image> loadDDS(const String& path);
  VLCORE_EXPORT bool isDDS(VirtualFile* file);

  //---------------------------------------------------------------------------
  // LoadWriterDDS
  //---------------------------------------------------------------------------
  /**
   * The LoadWriterDDS class is a ResourceLoadWriter capable of reading DDS files.
   */
  class LoadWriterDDS: public ResourceLoadWriter
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterDDS, ResourceLoadWriter)

  public:
    LoadWriterDDS(): ResourceLoadWriter("|dds|", "|dds|") 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    ref<ResourceDatabase> loadResource(const String& path) const 
    {
      ref<ResourceDatabase> res_db = new ResourceDatabase;
      ref<Image> img = loadDDS(path);
      if (img)
        res_db->resources().push_back(img);
      return res_db;
    }

    ref<ResourceDatabase> loadResource(VirtualFile* file) const
    {
      ref<ResourceDatabase> res_db = new ResourceDatabase;
      ref<Image> img = loadDDS(file);
      if (img)
        res_db->resources().push_back(img);
      return res_db;
    }

    bool writeResource(const String&, ResourceDatabase*) const
    {
      // mic fixme
      return false;
    }

    bool writeResource(VirtualFile*, ResourceDatabase*) const
    {
      // mic fixme
      return false;
    }
  };
}

#endif
