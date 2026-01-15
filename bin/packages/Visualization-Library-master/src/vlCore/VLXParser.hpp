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

#ifndef VLXParser_INCLUDE_ONCE
#define VLXParser_INCLUDE_ONCE

#include <vlCore/BufferedStream.hpp>
#include <vlCore/VLXLinker.hpp>

namespace vl
{
  /** Base class for VLX parsers. */
  class VLXParser: public Object
  {
    VL_INSTRUMENT_ABSTRACT_CLASS(vl::VLXParser, Object)

  public:

    virtual bool parseHeader() = 0;

    virtual bool parse() = 0;

    //! Links the 
    bool link()
    {
      VLXLinker linker;

      for(size_t i=0; i<mStructures.size(); ++i)
        linker.add(mStructures[i].get());

      return linker.link();
    }

    //! Moves the <Metadata> key/value pairs in the Metadata map for quick and easy access and removes the <Metadata> structure.
    void parseMetadata()
    {
      mMetadata.clear();

      for(size_t i=0; i<mStructures.size(); ++i)
      {
        if (mStructures[i]->tag() == "<Metadata>")
        {
          const VLXStructure* st = mStructures[i].get();

          for(size_t ikey=0; ikey<st->value().size(); ++ikey)
            mMetadata[st->value()[ikey].key()] = st->value()[ikey].value();

          mStructures.erase( mStructures.begin() + i );
        }
      }
    }

    //! The imported structures.
    std::vector< ref<VLXStructure> >& structures() { return mStructures; }

    //! The imported structures.
    const std::vector< ref<VLXStructure> >& structures() const { return mStructures; }

    //! The imported metadata.
    const std::map< std::string, VLXValue >& metadata() const { return mMetadata; }

    //! The encoding used to encode strings.
    const std::string& encoding() const { return mEncoding; }

    //! The VLX language version.
    unsigned short version() const { return mVersion;}

  protected:
    std::string mEncoding;
    unsigned short mVersion;
    std::vector< ref<VLXStructure> > mStructures;
    std::map< std::string, VLXValue > mMetadata;
  };
}

#endif
