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

#ifndef LoadWriterManager_INCLUDE_ONCE
#define LoadWriterManager_INCLUDE_ONCE

#include <vlCore/ResourceLoadWriter.hpp>
#include <vlCore/ResourceDatabase.hpp>
#include <vlCore/VirtualFile.hpp>
#include <vlCore/MemoryFile.hpp>
#include <vlCore/VisualizationLibrary.hpp>

namespace vl
{
  /** Defines an operation to be exectued to a ResourceDatabase as soon as its loaded, see also LoadWriterManager, WriteCallback. */
  class LoadCallback: public Object
  {
  public:
    virtual void operator()(ResourceDatabase* db) = 0;
  };

  /** Defines an operation to be exectued to a ResourceDatabase just before it is written, see also LoadWriterManager, LoadCallback. */
  class WriteCallback: public Object
  {
  public:
    virtual void operator()(ResourceDatabase* db) = 0;
  };

  /** The LoadWriterManager class loads and writes resources using the registered ResourceLoadWriter objects.
  You can install a LoadCallback to operate on loaded data or you can install a WriteCallback to operate on the data to be written,
  using the methods loadCallbacks() and writeCallbacks(). */
  class VLCORE_EXPORT LoadWriterManager: public Object
  {
    VL_INSTRUMENT_CLASS(vl::LoadWriterManager, Object)

  public:
    LoadWriterManager()
    { 
      VL_DEBUG_SET_OBJECT_NAME()
    }

    void registerLoadWriter(ResourceLoadWriter*);

    //! Returns the set of registered ResourceLoadWriter objects
    std::vector< ref<ResourceLoadWriter> >& loadWriters() { return mLoadWriters; }

    //! Returns the set of registered ResourceLoadWriter objects
    const std::vector< ref<ResourceLoadWriter> >& loadWriters() const { return mLoadWriters; }

    //! Returns the first ResourceLoadWriter of the specified type found.
    template<class T>
    T* loadWriter()
    {
      for(size_t i=0; i<loadWriters().size(); ++i)
      {
        T* load_writer = loadWriters()[i]->as<T>();
        if (load_writer)
          return load_writer;
      }
      return NULL;
    }

    //! Returns true if there is a ResourceLoadWriter registered to load the specified path or extension
    bool canLoad(const String& path)  const { return findLoader(path) != NULL; }

    //! Returns true if there is a ResourceLoadWriter registered to load the specified file
    bool canLoad(VirtualFile* file)   const { return findLoader(file->path()) != NULL; }

    //! Returns true if there is a ResourceLoadWriter registered to write the specified path or extension
    bool canWrite(const String& path) const { return findWriter(path) != NULL; }

    //! Returns true if there is a ResourceLoadWriter registered to write the specified file
    bool canWrite(VirtualFile* file)  const { return findWriter(file->path()) != NULL; }

    //! Returns the ResourceLoadWriter that has been registered to load the resource type specified by the given path or extension
    const ResourceLoadWriter* findLoader(const String& path) const;

    //! Returns the ResourceLoadWriter that has been registered to write the resource type specified by the given path or extension
    const ResourceLoadWriter* findWriter(const String& path) const;

    //! Returns the ResourceLoadWriter that has been registered to load the resource type specified by the given file.
    const ResourceLoadWriter* findLoader(VirtualFile* file) const;

    //! Returns the ResourceLoadWriter that has been registered to write the resource type specified by the given file.
    const ResourceLoadWriter* findWriter(VirtualFile* file) const;

    //! Loads the resource specified by the given path using the appropriate ResourceLoadWriter.
    ref<ResourceDatabase> loadResource(const String& path, bool quick=true) const;

    //! Loads the resource specified by the given file using the appropriate ResourceLoadWriter.
    ref<ResourceDatabase> loadResource(VirtualFile* file, bool quick=true) const;

    //! Writes the resource specified by the given file using the appropriate ResourceLoadWriter.
    bool writeResource(const String& path, ResourceDatabase* resource) const;

    //! Writes the resource specified by the given file using the appropriate ResourceLoadWriter.
    bool writeResource(VirtualFile* file, ResourceDatabase* resource) const;

    const std::vector< ref<LoadCallback> >& loadCallbacks() const { return mLoadCallbacks; }

    const std::vector< ref<WriteCallback> >& writeCallbacks() const { return mWriteCallbacks; }

    std::vector< ref<LoadCallback> >& loadCallbacks() { return mLoadCallbacks; }

    std::vector< ref<WriteCallback> >& writeCallbacks() { return mWriteCallbacks; }

  protected:
    std::vector< ref<ResourceLoadWriter> > mLoadWriters;
    std::vector< ref<LoadCallback> > mLoadCallbacks;
    std::vector< ref<WriteCallback> > mWriteCallbacks;
  };

  //! Returs the default LoadWriterManager used by Visualization Library.
  VLCORE_EXPORT LoadWriterManager* defLoadWriterManager();

  //! Sets the default LoadWriterManager used by Visualization Library.
  VLCORE_EXPORT void setDefLoadWriterManager(LoadWriterManager* lwm);

  //! Utility function, equivalent to defLoadWriterManager()->registerLoadWriter(rlw).
  inline void registerLoadWriter(ResourceLoadWriter* rlw) { defLoadWriterManager()->registerLoadWriter(rlw); }
}

#endif
