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

#ifndef FontManager_INCLUDE_ONCE
#define FontManager_INCLUDE_ONCE

#include <vlGraphics/Font.hpp>

namespace vl
{
  /** The FontManager class keeps a map associating a font path, size and smoothing flag to a Font object.
   * \sa
   * - Font
   * - Text
   * - Actor
   * - VectorGraphics */
  class VLGRAPHICS_EXPORT FontManager: public Object
  {
    VL_INSTRUMENT_CLASS(vl::FontManager, Object)

  public:
    //! Constructor: uses the given FT_Library handle otherwise will initialize and use its own FT_Library.
    FontManager(void* free_type_library=NULL);

    //! Destructor: releases all fonts and disposes the FT_Library if not NULL. 
    //! If you don't want the FontManager to dispose the associated FT_Library then call setFreeTypeLibrary(NULL)
    //! before the FontManager is destroyed.
    ~FontManager();

    //! Creates or returns an already created Font.
    Font* acquireFont(const String& font, int size, bool smooth=false);

    //! Returns the list of Fonts created till now.
    const std::vector< ref<Font> >& fonts() const { return mFonts; }

    //! Releases a given Font and its associated resources and memory.
    void releaseFont(Font* font);

    //! Releases all Fonts and associated resources and memory.
    void releaseAllFonts();

    //! Returns the FT_Library handle.
    const void* freeTypeLibrary() const { return mFreeTypeLibrary; }

    //! Returns the FT_Library handle.
    void* freeTypeLibrary() { return mFreeTypeLibrary; }

    //! Sets the FT_Library to the given one and returns the former one. 
    //! It is the user responsibility to dispose the returned one (if non-NULL).
    void* setFreeTypeLibrary(void* ftlib) { void* ret = mFreeTypeLibrary; mFreeTypeLibrary = ftlib; return ret; }

  protected:
    std::vector< ref<Font> > mFonts;
    void* mFreeTypeLibrary;
  };

  //! Returns the default FontManager used by Visualization Library.
  VLGRAPHICS_EXPORT FontManager* defFontManager();

  //! Sets the default FontManager used by Visualization Library.
  VLGRAPHICS_EXPORT void setDefFontManager(FontManager*);
}

#endif

