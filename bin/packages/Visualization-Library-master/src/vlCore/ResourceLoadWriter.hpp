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

#ifndef ResourceLoadWriter_INCLUDE_ONCE
#define ResourceLoadWriter_INCLUDE_ONCE

#include <vlCore/String.hpp>

namespace vl
{
  class ResourceDatabase;

  /**
   * The ResourceLoadWriter class is an abstract class used to implement read/write support for one or more resource types.
   */
  class ResourceLoadWriter: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::ResourceLoadWriter, Object)

  public:
    ResourceLoadWriter(const String& load_extensions, const String& write_extensions): mLoadExtensions(load_extensions), mWriteExtensions(write_extensions) {}

    virtual ref<ResourceDatabase> loadResource(const String& path) const = 0;
    virtual ref<ResourceDatabase> loadResource(VirtualFile* file) const = 0;
    virtual bool writeResource(const String& path, ResourceDatabase* resource) const = 0;
    virtual bool writeResource(VirtualFile* file, ResourceDatabase* resource) const = 0;

    //! Returns true if the given file type can be loaded.
    //! Note that the check is not case sensitive.
    bool canLoad(const String& extension) const { return mLoadExtensions.find("|"+extension.toLowerCase()+"|") != -1; }

    //! Returns the string containing the file types that can be loaded.
    //! The extensions returned are always lower-case.
    const String& loadExtensions() const { return mLoadExtensions; }

    //! Sets the set of file extensions that can be loaded.
    //! The string must be of the form "|.ext1|.ext2|.ext3|" for example "|.jpg|.png|.gif|".
    //! Note that the file extension matching is non case sensitive, so for example the above
    //! extension includes also files with the following extensions: .JPG .Png, GiF and so on.
    void setLoadExtensions(const String& extensions) { mLoadExtensions = extensions; mLoadExtensions = mLoadExtensions.toLowerCase(); }

    //! Returns true if the given file type can be written.
    //! Note that the check is not case sensitive.
    bool canWrite(const String& extension) const { return mWriteExtensions.find("|"+extension.toLowerCase()+"|") != -1; }

    //! Returns the string containing the file types that can be written.
    //! The extensions returned are always lower-case.
    const String& writeExtensions() const { return mWriteExtensions; }

    //! Sets the set of file extensions that can be written.
    //! The string must be of the form "|.ext1|.ext2|.ext3|" for example "|.jpg|.png|.gif|".
    //! Note that the file extension matching is non case sensitive, so for example the above
    //! extension includes also files with the following extensions: .JPG .Png, GiF and so on.
    void setWriteExtensions(const String& extensions) { mWriteExtensions = extensions; mWriteExtensions = mWriteExtensions.toLowerCase(); }

  protected:
    String mLoadExtensions;
    String mWriteExtensions;
  };
}

#endif
