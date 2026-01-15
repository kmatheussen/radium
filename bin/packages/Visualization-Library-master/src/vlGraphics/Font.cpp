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

#include <vlGraphics/Font.hpp>
#include <vlGraphics/OpenGL.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/FileSystem.hpp>
#include <vlCore/Image.hpp>

#include <ft2build.h>
#include FT_FREETYPE_H

using namespace vl;

// FreeType error table construction start ------------------------------------------
// taken from "fterrors.h" example
#undef __FTERRORS_H__
#define FT_ERRORDEF( e, v, s )  { e, s },
#define FT_ERROR_START_LIST     {
#define FT_ERROR_END_LIST       { 0, 0 } };

const struct
{
 int          err_code;
 const char*  err_msg;
} ft_errors[] =

#include FT_ERRORS_H
// FreeType error table construction end ------------------------------------------

const char* get_ft_error_message(int error)
{
  int i=0;
  while( ft_errors[i].err_msg && ft_errors[i].err_code != error )
    ++i;
  return ft_errors[i].err_msg;
}

//-----------------------------------------------------------------------------
// Glyph
//-----------------------------------------------------------------------------
Glyph::~Glyph()
{
  if (mTextureHandle)
  {
    glDeleteTextures(1, &mTextureHandle);
    mTextureHandle = 0;
  }
}
//-----------------------------------------------------------------------------
// Font
//-----------------------------------------------------------------------------
Font::Font(FontManager* fm)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mFontManager = fm;
  mSize    = 0;
  mHeight  = 0;
  mFT_Face = NULL;
  mSmooth  = false;
  mFreeTypeLoadForceAutoHint = true;
  setSize(14);
}
//-----------------------------------------------------------------------------
Font::Font(FontManager* fm, const String& font_file, int size)
{
  VL_DEBUG_SET_OBJECT_NAME()
  mFontManager = fm;
  mSize    = 0;
  mHeight  = 0;
  mFT_Face = NULL;
  mSmooth  = false;
  mFreeTypeLoadForceAutoHint = true;
  loadFont(font_file);
  setSize(size);
}
//-----------------------------------------------------------------------------
Font::~Font()
{
  releaseFreeTypeData();
}
//-----------------------------------------------------------------------------
void Font::releaseFreeTypeData()
{
  if (mFT_Face)
  {
    if (!mFontManager->freeTypeLibrary())
    {
      vl::Log::error("Font::releaseFreeTypeData(): mFontManager->freeTypeLibrary() is NULL!\n");
      VL_TRAP()
    }
    else
    {
      FT_Done_Face(mFT_Face);
    }
    mFT_Face = NULL;
  }
}
//-----------------------------------------------------------------------------
void Font::setSize(int size)
{
  if(mSize != size)
  {
    mSize = size;
    // removes all the cached glyphs
    mGlyphMap.clear();
  }
}
//-----------------------------------------------------------------------------
void Font::loadFont(const String& path)
{
  if(path == mFilePath)
    return;

  mFilePath = path;
  // removes all the cached glyphs
  mGlyphMap.clear();

  // remove FreeType font face object
  if (mFT_Face)
  {
    FT_Done_Face(mFT_Face);
    mFT_Face = NULL;
  }

  FT_Error error = 0;

  ref<VirtualFile> font_file = defFileSystem()->locateFile( filePath() );

  if (!font_file)
    Log::error( Say("Font::loadFont('%s'): font file not found.\n") << filePath() );

  if ( font_file && font_file->load(mMemoryFile) )
  {
    if ( (int)mMemoryFile.size() == font_file->size() )
    {
      error = FT_New_Memory_Face( (FT_Library)mFontManager->freeTypeLibrary(),
                                  (FT_Byte*)&mMemoryFile[0],
                                  (int)mMemoryFile.size(),
                                  0,
                                  &mFT_Face );
    }
    else
      Log::error( Say("Font::loadFont('%s'): could not read file.\n") << filePath() );
  }

  if (error)
  {
    Log::error(Say("FT_New_Face error (%s): %s\n") << filePath() << get_ft_error_message(error) );
    VL_TRAP()
    return;
  }
}
//-----------------------------------------------------------------------------
Glyph* Font::glyph(int character)
{
  ref<Glyph>& glyph = mGlyphMap[character];

  if (glyph.get() == NULL)
  {
    glyph = new Glyph;
    glyph->setFont(this);

    // create the glyph

    FT_Error error = 0;

    error = FT_Set_Char_Size(
              mFT_Face, /* handle to face object           */
              0,        /* char_width in 1/64th of points  */
              mSize*64, /* char_height in 1/64th of points */
              96,       /* horizontal device resolution    */
              96 );     /* vertical device resolution      */

    if(error)
    {
      // Log::error(Say("FT_Set_Char_Size error: %s\n") << get_ft_error_message(error) );
      if ( (mFT_Face->face_flags & FT_FACE_FLAG_SCALABLE) == 0 && mFT_Face->num_fixed_sizes)
      {
        // look for the size which is less or equal to the given size

        int best_match_index = -1;
        int best_match_size  = 0;
        for( int i=0; i < mFT_Face->num_fixed_sizes; ++i )
        {
          int size = mFT_Face->available_sizes[i].y_ppem/64;
          // skip bigger characters
          if (size <= mSize)
          {
            if (best_match_index == -1 || (mSize - size) < (mSize - best_match_size) )
            {
              best_match_index = i;
              best_match_size  = size;
            }
          }
        }

        if (best_match_index == -1)
          best_match_index = 0;

        error = FT_Select_Size(mFT_Face, best_match_index);
        if (error)
          Log::error(Say("FT_Select_Size error (%s): %s\n") << filePath() << get_ft_error_message(error) );
        VL_CHECK(!error)
      }
      // else
      {
        Log::error(Say("FT_Set_Char_Size error (%s): %s\n") << filePath() << get_ft_error_message(error) );
        VL_TRAP()
        return glyph.get();
      }
    }

    mHeight = mFT_Face->size->metrics.height / 64.0f;

    // using FT_Load_Char instead of FT_Get_Char_Index + FT_Load_Glyph works better, probably
    // FreeType performs some extra tricks internally to better support less reliable fonts...

    // FT_UInt glyph_index = FT_Load_Char( mFT_Face, character, FT_LOAD_DEFAULT );
    // glyph->glyphIndex() = glyph_index;
    //FT_UInt glyph_index = FT_Get_Char_Index( mFT_Face, character );
    //glyph->glyphIndex() = glyph_index;
    //// we need to render it in order to have the transformed glyph_left and glyph_top
    //FT_Int32 load_flags = FT_LOAD_DEFAULT;
    //error = FT_Load_Glyph(
    //          mFT_Face,         /* handle to face object */
    //          glyph_index,  /* glyph index           */
    //          load_flags ); /* load flags, see below */
    //if(error)
    //{
    //  Log::error(Say("FT_Load_Glyph error: %s") << get_ft_error_message(error) );
    //  VL_TRAP()
    //  return glyph;
    //}

    // Note with FT 2.3.9 FT_LOAD_DEFAULT worked well, with FT 2.4 instead we ned FT_LOAD_FORCE_AUTOHINT
    // This might work well with VL's font but it might be suboptimal for other fonts.

    error = FT_Load_Char( mFT_Face, character, freeTypeLoadForceAutoHint() ? FT_LOAD_FORCE_AUTOHINT : FT_LOAD_DEFAULT );

    if(error)
    {
      Log::error(Say("FT_Load_Char error (%s): %s\n") << filePath() << get_ft_error_message(error) );
      VL_TRAP()
      glyph = NULL;
      return glyph.get();
    }

    glyph->setGlyphIndex( FT_Get_Char_Index( mFT_Face, character ) );

    error = FT_Render_Glyph(
              mFT_Face->glyph,  /* glyph slot */
              FT_RENDER_MODE_NORMAL ); /* render mode: FT_RENDER_MODE_MONO or FT_RENDER_MODE_NORMAL */

    // fonts like webdings.ttf generate an error when an unsupported char code is requested instead of
    // reverting to char code 0, so we have to do it by hand...

    if(error)
    {
      // Log::error(Say("FT_Render_Glyph error: %s") << get_ft_error_message(error) );
      // VL_TRAP()
      error = FT_Load_Glyph(
                mFT_Face,/* handle to face object */
                0,       /* glyph index           */
                FT_LOAD_DEFAULT ); /* load flags, see below */
      glyph->setGlyphIndex(0);

      error = FT_Render_Glyph(
                mFT_Face->glyph,  /* glyph slot */
                FT_RENDER_MODE_NORMAL ); /* render mode: FT_RENDER_MODE_MONO or FT_RENDER_MODE_NORMAL */
    }

    if(error)
    {
      Log::error(Say("FT_Render_Glyph error (%s): %s\n") << filePath() << get_ft_error_message(error) );
      VL_TRAP()
      glyph = NULL;
      return glyph.get();
    }

    bool ok_format = mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY || mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO;
    ok_format &= mFT_Face->glyph->bitmap.palette_mode == 0;
    ok_format &= mFT_Face->glyph->bitmap.pitch > 0 || mFT_Face->glyph->bitmap.buffer == NULL;

    if (!ok_format)
    {
      Log::error( Say("Font::glyph() error (%s): glyph format not supported. Visualization Library currently supports only FT_PIXEL_MODE_GRAY and FT_PIXEL_MODE_MONO.\n") << filePath() );
      VL_TRAP()
      return glyph.get();
    }

    if ( mFT_Face->glyph->bitmap.buffer )
    {
      if (mHeight == 0)
        mHeight = (float)mFT_Face->glyph->bitmap.rows;

      glyph->setWidth ( mFT_Face->glyph->bitmap.width);
      glyph->setHeight( mFT_Face->glyph->bitmap.rows);
      glyph->setLeft  ( mFT_Face->glyph->bitmap_left);
      glyph->setTop   ( mFT_Face->glyph->bitmap_top);

      VL_CHECK( mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY || mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO )
      VL_CHECK( mFT_Face->glyph->bitmap.palette_mode == 0 )
      VL_CHECK( mFT_Face->glyph->bitmap.pitch > 0 )

      unsigned int texhdl;
      glGenTextures( 1, &texhdl );
      glyph->setTextureHandle(texhdl);
      VL_glActiveTexture(GL_TEXTURE0);
      glBindTexture( GL_TEXTURE_2D, glyph->textureHandle() );

      int texsize[] = { 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 0 };
      int max_tex_size = 0;
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, &max_tex_size);

      int w=0, h=0, margin = 1;

      for(int i=0; texsize[i]; ++i)
      {
        if ( (texsize[i] >= glyph->width() + margin*2 && texsize[i] >= glyph->height() + margin*2) || texsize[i+1] > max_tex_size )
        {
          w = texsize[i];
          h = texsize[i];
          break;
        }
      }
      VL_CHECK(w)
      VL_CHECK(h)

#if(1)
      // tex coords DO include the border
      glyph->setS0( 0 );
      glyph->setT0( 1 );
      glyph->setS1((margin*2 + glyph->width() ) /(float)(w-1) );
      glyph->setT1( 1 -(margin*2 + glyph->height() ) /(float)(h-1) );

      // tex coords DO NOT include the border
      //glyph->setS0((float)margin /(float)(w-1));
      //glyph->setT0( 1 -(float)margin /(float)(h-1));
      //glyph->setS1(((float)margin + glyph->width() ) /(float)(w-1));
      //glyph->setT1( 1 -((float)margin + glyph->height() ) /(float)(h-1));
#else
      glyph->setS0((float)margin /(float)w);
      glyph->setT0( 1 -(float)margin /(float)h);
      glyph->setS1(((float)margin + glyph->width() ) /(float)w);
      glyph->setT1( 1 -((float)margin + glyph->height() ) /(float)h);
#endif

      ref<Image> img = new Image;
      img->allocate2D(w, h, 1, IF_RGBA, IT_UNSIGNED_BYTE);

      // init to all transparent white
      for(unsigned char *px = img->pixels(), *end = px + img->requiredMemory(); px<end; px+=4)
      {
        px[0] = 0xFF;
        px[1] = 0xFF;
        px[2] = 0xFF;
        px[3] = 0x0;
      }

      // maps the glyph on the texture leaving a 1px margin

      for(int y=0; y<glyph->height(); y++)
      {
        for(int x=0; x<glyph->width(); x++)
        {
          int offset_1 = (x+margin) * 4 + (w-1-y-margin) * img->pitch();
          int offset_2 = 0;
          if (mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO)
            offset_2 = x / 8 + y * ::abs(mFT_Face->glyph->bitmap.pitch);
          else
            offset_2 = x + y * mFT_Face->glyph->bitmap.pitch;

#if (1)
          if (mFT_Face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO)
            img->pixels()[ offset_1+3 ] = (mFT_Face->glyph->bitmap.buffer[ offset_2 ] >> (7-x%8)) & 0x1 ? 0xFF : 0x0;
          else
            img->pixels()[ offset_1+3 ] = mFT_Face->glyph->bitmap.buffer[ offset_2 ];
#else
          // debug code
          img->pixels()[ offset_1+0 ] = face->glyph->bitmap.buffer[ offset_2 ]; // 0xFF;
          img->pixels()[ offset_1+1 ] = face->glyph->bitmap.buffer[ offset_2 ]; // 0xFF;
          img->pixels()[ offset_1+2 ] = face->glyph->bitmap.buffer[ offset_2 ]; // 0xFF;
          img->pixels()[ offset_1+3 ] = 0xFF; // face->glyph->bitmap.buffer[ offset_2 ];
#endif
        }
      }

      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img->width(), img->height(), 0, img->format(), img->type(), img->pixels() ); VL_CHECK_OGL();

      if ( smooth() )
      {
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
      }
      else
      {
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
      }

      // sets anisotropy to the maximum supported
      if (Has_GL_EXT_texture_filter_anisotropic)
      {
        float max_anisotropy;
        glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &max_anisotropy);
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, max_anisotropy);
      }

      VL_CHECK_OGL();
      glBindTexture( GL_TEXTURE_2D, 0 );
    }

    glyph->setAdvance( fvec2( (float)mFT_Face->glyph->advance.x / 64.0f, (float)mFT_Face->glyph->advance.y / 64.0f ) );
  }

  return glyph.get();
}
//-----------------------------------------------------------------------------
void Font::setSmooth(bool smooth)
{
  mSmooth = smooth;
  std::map<int, ref<Glyph> >::iterator it = mGlyphMap.begin();
  for(; it != mGlyphMap.end(); ++it )
  {
    const ref<Glyph>& glyph = it->second;
    if (glyph->textureHandle() == 0)
      continue;

    glBindTexture( GL_TEXTURE_2D, glyph->textureHandle() );
    if (smooth)
    {
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
    }
    else
    {
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
    }
  }
  glBindTexture( GL_TEXTURE_2D, 0 );
}
//-----------------------------------------------------------------------------
