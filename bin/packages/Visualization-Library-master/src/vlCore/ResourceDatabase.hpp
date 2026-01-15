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

#ifndef ResourceDatabase_INCLUDE_ONCE
#define ResourceDatabase_INCLUDE_ONCE

#include <vlCore/Object.hpp>
#include <vlCore/String.hpp>
#include <vector>
#include <algorithm>

namespace vl
{
  class VirtualFile;

  /**
   * The ResourceDatabase class contains and manipulates a set of resources. It works with 
   * any kind of resources derived from vl::Object, even user-customized ones.
  */
  class VLCORE_EXPORT ResourceDatabase: public Object
  {
    VL_INSTRUMENT_CLASS(vl::ResourceDatabase, Object)

  public:
    ResourceDatabase()
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    const std::vector< ref<Object> >& resources() const { return mResources; }

    std::vector< ref<Object> >& resources() { return mResources; }

    //! Starts to look for the next object of the specified type from the given position.
    template<class T>
    T* next(int& cur_pos) const
    {
      for(unsigned i=cur_pos; i<mResources.size(); ++i)
      {
        ref<T> r = cast<T>(mResources[i].get());
        if (r)
        {
          cur_pos = i+1;
          return r.get();
        }
      }
      return NULL;
    }

    //! Returns all the objects of the specified type in the given vector.
    template<class T>
    void get( std::vector< ref<T> >& resources, bool clear_vector=true )
    {
      if (clear_vector)
        resources.clear();

      for( unsigned i=0; i<mResources.size(); ++i )
      {
        ref<T> r = cast<T>(mResources[i].get());
        if (r)
          resources.push_back(r);
      }
    }

    //! Returns all the objects of the specified type in the given vector and removes them from the ResourceDatabase.
    template<class T>
    void extract( std::vector< ref<T> >& resources, bool clear_vector=true )
    {
      if (clear_vector)
        resources.clear();

      size_t start = resources.size();

      for( unsigned i=mResources.size(); i--; )
      {
        ref<T> r = cast<T>(mResources[i].get());
        if (r)
        {
          resources.push_back(r);
          mResources.erase(mResources.begin()+i);
        }
      }

      std::reverse(resources.begin()+start, resources.end());
    }

    //! Don't use inside loops! Counts the number object of the specified type.
    template<class T>
    size_t count() const
    {
      size_t count=0;
      for(unsigned i=0; i<mResources.size(); ++i)
      {
        const T* r = cast_const<T>(mResources[i].get());
        if (r)
          ++count;
      }
      return count;
    }

    //! Don't use inside loops! Returns the j-th object of the specified type (which is different from \p resources()[j]!).
    template<class T>
    const T* get(int j) const
    {
      int count=0;
      for(unsigned i=0; i<mResources.size(); ++i)
      {
        const T* r = cast_const<T>(mResources[i].get());
        if (r)
        {
          if (count == j)
            return r;
          else
            ++count;
        }
      }
      return NULL;
    }

    //! Don't use inside loops! Returns the j-th object of the specified type (which is different from \p resources()[j]!).
    template<class T>
    T* get(int j)
    {
      int count=0;
      for(unsigned i=0; i<mResources.size(); ++i)
      {
        T* r = cast<T>(mResources[i].get());
        if (r)
        {
          if (count == j)
            return r;
          else
            ++count;
        }
      }
      return NULL;
    }

  protected:
    std::vector< ref<Object> > mResources;
  };

  //! Short version of defLoadWriterManager()->canLoad(path).
  VLCORE_EXPORT bool canLoad(const String& path);

  //! Short version of defLoadWriterManager()->canWrite(path).
  VLCORE_EXPORT bool canWrite(const String& path);

  //! Short version of defLoadWriterManager()->canLoad(file).
  VLCORE_EXPORT bool canLoad(VirtualFile* file);

  //! Short version of defLoadWriterManager()->canWrite(file).
  VLCORE_EXPORT bool canWrite(VirtualFile* file);

  //! Short version of defLoadWriterManager()->loadResource(path, quick).
  VLCORE_EXPORT ref<ResourceDatabase> loadResource(const String& path, bool quick=true);

  //! Short version of defLoadWriterManager()->loadResource(file, quick).
  VLCORE_EXPORT ref<ResourceDatabase> loadResource(VirtualFile* file, bool quick=true);

  //! Short version of defLoadWriterManager()->writeResource(path, resource).
  VLCORE_EXPORT bool writeResource(const String& path, ResourceDatabase* resource);

  //! Short version of defLoadWriterManager()->writeResource(file, resource).
  VLCORE_EXPORT bool writeResource(VirtualFile* file, ResourceDatabase* resource);
}

#endif
