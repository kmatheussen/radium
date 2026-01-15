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

#ifndef VLXRegistry_INCLUDE_ONCE
#define VLXRegistry_INCLUDE_ONCE

#include <vlCore/VLXClassWrapper.hpp>
#include <string>
#include <map>

namespace vl
{
  /** Registry of vl::VLXClassWrapper objects, used by vl::VLXSerializer, see also vl::defVLXRegistry(). */
  class VLXRegistry: public Object
  {
    VL_INSTRUMENT_CLASS(vl::VLXRegistry, Object)

  public:
    void registerClassWrapper(const TypeInfo& type, VLXClassWrapper* wrapper)
    {
      std::string tag = std::string("<") + type.name() + ">";
      mExportRegistry[type] = wrapper; 
      mImportRegistry[tag]  = wrapper; 
    }

    std::map< std::string, ref<VLXClassWrapper> >& importRegistry() { return mImportRegistry; }
    std::map< TypeInfo, ref<VLXClassWrapper> >& exportRegistry() { return mExportRegistry; }

    const std::map< std::string, ref<VLXClassWrapper> >& importRegistry() const { return mImportRegistry; }
    const std::map< TypeInfo, ref<VLXClassWrapper> >& exportRegistry() const { return mExportRegistry; }

  private:
    std::map< std::string, ref<VLXClassWrapper> > mImportRegistry;     // <tag> --> VLXClassWrapper
    std::map< TypeInfo, ref<VLXClassWrapper> > mExportRegistry; // TypeInfo --> VLXClassWrapper
  };

  VLCORE_EXPORT VLXRegistry* defVLXRegistry();
  VLCORE_EXPORT void setDefVLXRegistry(VLXRegistry* reg);
}

#endif
