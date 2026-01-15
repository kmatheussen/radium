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

#include <vlGraphics/FontManager.hpp>
#include <vlCore/Log.hpp>
#include <algorithm>

#include "ft2build.h"
#include FT_FREETYPE_H

using namespace vl;

//-----------------------------------------------------------------------------
FontManager::FontManager(void* free_type_library)
{
  mFreeTypeLibrary = free_type_library;
  if (!free_type_library)
  {
    FT_Library freetype = NULL;
    FT_Error error = FT_Init_FreeType( &freetype );
    mFreeTypeLibrary = freetype;
    if ( error )
    {
      Log::error("FontManager::FontManager(): an error occurred during FreeType library initialization!\n");
      VL_TRAP()
    }
  }
}
//-----------------------------------------------------------------------------
FontManager::~FontManager()
{
  releaseAllFonts();
  if (mFreeTypeLibrary)
  {
    FT_Done_FreeType( (FT_Library)mFreeTypeLibrary );
    mFreeTypeLibrary = NULL;
  }
}
//-----------------------------------------------------------------------------
Font* FontManager::acquireFont(const String& path, int size, bool smooth)
{
  ref<Font> font;
  for(unsigned i=0; !font && i<mFonts.size(); ++i)
    if (fonts()[i]->filePath() == path && fonts()[i]->size() == size && fonts()[i]->smooth() == smooth)
      font = fonts()[i];

  if (!font)
  {
    font = new Font(this);
    font->loadFont(path);
    font->setSize(size);
    font->setSmooth(smooth);
    mFonts.push_back( font );
  }

  return font.get();
}
//-----------------------------------------------------------------------------
void FontManager::releaseFont(Font* font)
{ 
  std::vector< ref<Font> >::iterator it = std::find(mFonts.begin(), mFonts.end(), font);
  if (it != mFonts.end())
    mFonts.erase(it);
}
//-----------------------------------------------------------------------------
void FontManager::releaseAllFonts()
{ 
  for(unsigned i=0; i<mFonts.size(); ++i)
    mFonts[i]->releaseFreeTypeData(); 
}
//-----------------------------------------------------------------------------
