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

#include <vlGraphics/CoreText.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/Log.hpp>

#include <ft2build.h>
#include FT_FREETYPE_H

using namespace vl;

// mic fixme: implement me.

// Goals:
// - Run on OpenGL Core/ES.
// - Achieve a much greater rendering speed.
// - In order to do this we might sacrifice some of the usability of vl::Text

// Guidelines:
// - Viewport alignment, text transformation, transform tracking done externally.
// - Does not manipulate the GL_PROJECTION and GL_MODELVIEW matrices.
// - Text geometry and bounding boxes should be pre-computed on text change.
// - Line splitting should not be done at rendering time.
// - Should use only OpenGL Core routines.
// - Should use texture atlases and perform lazy texture binding.
// - Cleaner left to right / right to left text reversing.
// - Outline rendering should use 2 pass with enlarged glyphs instead of 5 passes or precompute an high quality outline texture.
// - Avoid using doubles and floats if possible, use integer and Rect rather floats and AABBs.

//-----------------------------------------------------------------------------
void CoreText::render_Implementation(const Actor* actor, const Shader*, const Camera* camera, OpenGLContext* gl_context) const
{
  gl_context->bindVAS(NULL, false, false);

  VL_CHECK(font())

  if (!font() || !font()->mFT_Face)
    return;

  if ( text().empty() )
    return;

  // Lighting can be enabled or disabled.
  // glDisable(GL_LIGHTING);

  // Blending must be enabled explicity by the vl::Shader, also to perform z-sort.
  // glEnable(GL_BLEND);

  // Trucchetto che usiamo per evitare z-fighting:
  // Pass #1 - fill color and stencil
  // - disable depth write mask
  // - depth test can be enabled or not by the user
  // - depth func can be choosen by the user
  // - render in the order: background, border, shadow, outline, text
  // Pass #2 - fill z-buffer
  // - enable depth write mask
  // - disable color mask
  // - disable stencil
  // - drawing background and border

  // Pass #1

  // disable z-writing
  GLboolean depth_mask=0;
  glGetBooleanv(GL_DEPTH_WRITEMASK, &depth_mask);
  glDepthMask(GL_FALSE);

  // background
  if (backgroundEnabled())
    renderBackground( actor, camera );

  // border
  if (borderEnabled())
    renderBorder( actor, camera );

  // to have the most correct results we should render the text twice one for color and stencil, the other for the z-buffer

  // shadow render
  if (shadowEnabled())
    renderText( actor, camera, shadowColor(), shadowVector() );
  // outline render
  if (outlineEnabled())
  {
    renderText( actor, camera, outlineColor(), fvec2(-1,0) );
    renderText( actor, camera, outlineColor(), fvec2(+1,0) );
    renderText( actor, camera, outlineColor(), fvec2(0,-1) );
    renderText( actor, camera, outlineColor(), fvec2(0,+1) );
  }
  // text render
  renderText( actor, camera, color(), fvec2(0,0) );

  // Pass #2
  // fills the z-buffer (not the stencil buffer): approximated to the text bbox

  // restores depth mask
  glDepthMask(depth_mask);

  if (depth_mask)
  {
    // disables writing to the color buffer
    GLboolean color_mask[4];
    glGetBooleanv(GL_COLOR_WRITEMASK, color_mask);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

    // disable writing to the stencil buffer
    int stencil_front_mask=0;
    glGetIntegerv(GL_STENCIL_WRITEMASK, &stencil_front_mask);
    int stencil_back_mask=0;
    if (Has_GL_Version_2_0)
      glGetIntegerv(GL_STENCIL_BACK_WRITEMASK, &stencil_back_mask);
    glStencilMask(0);

    // background
    renderBackground( actor, camera );

    // border
    renderBorder( actor, camera );

    // restores color writing
    glColorMask(color_mask[0],color_mask[1],color_mask[2],color_mask[3]);

    // restore the stencil masks
    glStencilMask(stencil_front_mask);
    if (Has_GL_Version_2_0)
      glStencilMaskSeparate(GL_BACK, stencil_back_mask);
  }
}
//-----------------------------------------------------------------------------
void CoreText::renderText(const Actor*, const Camera*, const fvec4& color, const fvec2& offset) const
{
  if(!mFont)
  {
    Log::error("CoreText::renderText() error: no Font assigned to the CoreText object.\n");
    VL_TRAP()
    return;
  }

  if (!font()->mFT_Face)
  {
    Log::error("CoreText::renderText() error: invalid FT_Face: probably you tried to load an unsupported font format.\n");
    VL_TRAP()
    return;
  }

  // mic fixme: these should be pre-computed when the text is set!!!

  AABB rbbox = rawboundingRect( text() ); // for text alignment
  VL_CHECK(rbbox.maxCorner().z() == 0)
  VL_CHECK(rbbox.minCorner().z() == 0)
  AABB bbox = rbbox;
  bbox.setMaxCorner( bbox.maxCorner() + vec3(2.0f*margin(), 2.0f*margin(), 0) );
  VL_CHECK(bbox.maxCorner().z() == 0)
  VL_CHECK(bbox.minCorner().z() == 0)

  // basic render states

  // mic fixme: detect GLSLProgram and use VertexAttribPointer if present.

  float texc[] = { 0,0,0,0,0,0,0,0 };
  VL_glActiveTexture( GL_TEXTURE0 );
  VL_glClientActiveTexture( GL_TEXTURE0 );
  glEnable(GL_TEXTURE_2D);
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );
  glTexCoordPointer(2, GL_FLOAT, 0, texc);

  // color
  glColor4fv(color.ptr());

  // Constant normal
  glNormal3fv( fvec3(0,0,1).ptr() );

  fvec3 vect[4];
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect[0].ptr());

  FT_Long use_kerning = FT_HAS_KERNING( font()->mFT_Face );
  FT_UInt previous = 0;

  fvec2 pen(0,0);

  // mic fixme:
  // - lines split and lines dimensions (linebox) should be precomputed on text set!
  // - or convert this function to generate a pre-computed rendering-list!

  // split the text in different lines

  VL_CHECK(text().length())

  std::vector< String > lines;
  lines.push_back( String() );
  for(int i=0; i<text().length(); ++i)
  {
    if (text()[i] == '\n')
    {
      // start new line
      lines.push_back( String() );
    }
    else
      lines.back() += text()[i];
  }

  for(unsigned iline=0; iline<lines.size(); iline++)
  {
    // strip spaces at the beginning and at the end of the line
    if (textAlignment() == TextAlignJustify)
      lines[iline].trim();

    AABB linebox = rawboundingRect( lines[iline] );
    int horz_text_align = 0;
    int just_space = 0;
    int just_remained_space = 0;
    int space_count = 0;
    for(int c=0; c<(int)lines[iline].length(); c++)
      if ( lines[iline][c] == ' ' )
        space_count++;

    if (space_count && textAlignment() == TextAlignJustify)
    {
      just_space          = int(rbbox.width() - linebox.width()) / space_count;
      just_remained_space = int(rbbox.width() - linebox.width()) % space_count;
    }

    if (layout() == RightToLeftText)
    {
      if (textAlignment() == TextAlignRight)
        horz_text_align = 0;
      else
      if (textAlignment() == TextAlignLeft)
        horz_text_align = - int(rbbox.width() - linebox.width());
      else
      if (textAlignment() == TextAlignCenter)
        horz_text_align = - int((rbbox.width() - linebox.width()) / 2.0f);
    }
    if (layout() == LeftToRightText)
    {
      if (textAlignment() == TextAlignRight)
        horz_text_align = int(rbbox.width() - linebox.width());
      else
      if (textAlignment() == TextAlignLeft)
        horz_text_align = 0;
      else
      if (textAlignment() == TextAlignCenter)
        horz_text_align = + int((rbbox.width() - linebox.width()) / 2.0f);
    }

    // this is needed so that empty strings generate empty lines
    // note that puttig '\n\n\n\n' at the beginning of a text generates
    // a wrong rendering (see it with background box activated).
    if (iline != 0 && !lines[iline].length())
    {
      pen.y() -= mFont->mHeight;
      pen.x()  = 0;
    }
    else
    for(int c=0; c<(int)lines[iline].length(); c++)
    {
      if (c == 0 && iline != 0)
      {
        pen.y() -= mFont->mHeight;
        pen.x()  = 0;
      }

      const Glyph* glyph = mFont->glyph( lines[iline][c] );

      if (!glyph)
        continue;

      if ( kerningEnabled() && use_kerning && previous && glyph->glyphIndex() )
      {
        FT_Vector delta; delta.y = 0;
        if (layout() == LeftToRightText)
        {
          FT_Get_Kerning( font()->mFT_Face, previous, glyph->glyphIndex(), FT_KERNING_DEFAULT, &delta );
          pen.x() += delta.x / 64.0f;
        }
        else
        if (layout() == RightToLeftText)
        {
          FT_Get_Kerning( font()->mFT_Face, glyph->glyphIndex(), previous, FT_KERNING_DEFAULT, &delta );
          pen.x() -= delta.x / 64.0f;
        }
        pen.y() += delta.y / 64.0f;
      }
      previous = glyph->glyphIndex();

      if (glyph->textureHandle())
      {
        glBindTexture( GL_TEXTURE_2D, glyph->textureHandle() );

        texc[0] = glyph->s0();
        texc[1] = glyph->t1();
        texc[2] = glyph->s1();
        texc[3] = glyph->t1();
        texc[4] = glyph->s1();
        texc[5] = glyph->t0();
        texc[6] = glyph->s0();
        texc[7] = glyph->t0();

        int left = layout() == RightToLeftText ? -glyph->left() : +glyph->left();

        vect[0].x() = pen.x() + glyph->width()*0 + left -1;
        vect[0].y() = pen.y() + glyph->height()*0 + glyph->top() - glyph->height() -1;

        vect[1].x() = pen.x() + glyph->width()*1 + left +1;
        vect[1].y() = pen.y() + glyph->height()*0 + glyph->top() - glyph->height() -1;

        vect[2].x() = pen.x() + glyph->width()*1 + left +1;
        vect[2].y() = pen.y() + glyph->height()*1 + glyph->top() - glyph->height() +1;

        vect[3].x() = pen.x() + glyph->width()*0 + left -1;
        vect[3].y() = pen.y() + glyph->height()*1 + glyph->top() - glyph->height() +1;

        if (layout() == RightToLeftText)
        {
          vect[0].x() -= glyph->width()-1 +2;
          vect[1].x() -= glyph->width()-1 +2;
          vect[2].x() -= glyph->width()-1 +2;
          vect[3].x() -= glyph->width()-1 +2;
        }

        vect[0].y() -= mFont->mHeight;
        vect[1].y() -= mFont->mHeight;
        vect[2].y() -= mFont->mHeight;
        vect[3].y() -= mFont->mHeight;

        // normalize coordinate orgin to the bottom/left corner
        vect[0] -= (fvec3)bbox.minCorner();
        vect[1] -= (fvec3)bbox.minCorner();
        vect[2] -= (fvec3)bbox.minCorner();
        vect[3] -= (fvec3)bbox.minCorner();

        // margin & horz_text_align
        vect[0].x() += margin() + horz_text_align;
        vect[0].y() += margin();
        vect[1].x() += margin() + horz_text_align;
        vect[1].y() += margin();
        vect[2].x() += margin() + horz_text_align;
        vect[2].y() += margin();
        vect[3].x() += margin() + horz_text_align;
        vect[3].y() += margin();

        // apply offset for outline rendering
        vect[0].x() += offset.x();
        vect[0].y() += offset.y();
        vect[1].x() += offset.x();
        vect[1].y() += offset.y();
        vect[2].x() += offset.x();
        vect[2].y() += offset.y();
        vect[3].x() += offset.x();
        vect[3].y() += offset.y();

        // text pivot
        for(int i=0; i<4; ++i)
        {
          if (textOrigin() & AlignHCenter)
          {
            VL_CHECK( !(textOrigin() & AlignRight) )
            VL_CHECK( !(textOrigin() & AlignLeft) )
            vect[i].x() -= (int)(bbox.width() / 2.0f);
          }

          if (textOrigin() & AlignRight)
          {
            VL_CHECK( !(textOrigin() & AlignHCenter) )
            VL_CHECK( !(textOrigin() & AlignLeft) )
            vect[i].x() -= (int)bbox.width();
          }

          if (textOrigin() & AlignTop)
          {
            VL_CHECK( !(textOrigin() & AlignBottom) )
            VL_CHECK( !(textOrigin() & AlignVCenter) )
            vect[i].y() -= (int)bbox.height();
          }

          if (textOrigin() & AlignVCenter)
          {
            VL_CHECK( !(textOrigin() & AlignTop) )
            VL_CHECK( !(textOrigin() & AlignBottom) )
            vect[i].y() -= int(bbox.height() / 2.0);
          }
        }

        glDrawArrays(GL_QUADS, 0, 4);

        #if (0)
          glDisable(GL_TEXTURE_2D);
          glColor3fv(vec3(1,0,0).ptr());
          glDrawArrays(GL_LINE_LOOP, 0, 4);
          glColor4fv(color.ptr());
          glEnable(GL_TEXTURE_2D);
        #endif
      }

      if (just_space && lines[iline][c] == ' ' && iline != lines.size()-1)
      {
        if (layout() == LeftToRightText)
        {
          pen.x() += just_space + (just_remained_space?1:0);
          // pen.y() += glyph->advance().y();
        }
        else
        if (layout() == RightToLeftText)
        {
          pen.x() -= just_space + (just_remained_space?1:0);
          // pen.y() -= glyph->advance().y();
        }
        if(just_remained_space)
          just_remained_space--;
      }

      if (layout() == LeftToRightText)
      {
        pen.x() += glyph->advance().x();
        // pen.y() += glyph->advance().y();
      }
      else
      if (layout() == RightToLeftText)
      {
        pen.x() -= glyph->advance().x();
        // pen.y() -= glyph->advance().y();
      }

    }
  }

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );

  VL_CHECK_OGL();

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
}
//-----------------------------------------------------------------------------
// returns the raw bounding box of the string, i.e. without alignment, margin and matrix transform.
AABB CoreText::rawboundingRect(const String& text) const
{
  AABB aabb;

  if(!font())
  {
    Log::error("CoreText::rawboundingRect() error: no Font assigned to the CoreText object.\n");
    VL_TRAP()
    return aabb;
  }

  if (!font()->mFT_Face)
  {
    Log::error("CoreText::rawboundingRect() error: invalid FT_Face: probably you tried to load an unsupported font format.\n");
    VL_TRAP()
    return aabb;
  }

  fvec2 pen(0,0);
  fvec3 vect[4];

  FT_Long use_kerning = FT_HAS_KERNING( font()->mFT_Face );
  FT_UInt previous = 0;

  for(int c=0; c<(int)text.length(); c++)
  {
    if (text[c] == '\n')
    {
      pen.y() -= mFont->mHeight ? mFont->mHeight : mFont->mSize;
      pen.x()  = 0;
      continue;
    }

    const ref<Glyph>& glyph = mFont->glyph(text[c]);

    // if glyph == NULL there was an error during its creation...
    if (glyph.get() == NULL)
      continue;

    if ( kerningEnabled() && use_kerning && previous && glyph->glyphIndex())
    {
      FT_Vector delta; delta.y = 0;
      if (layout() == LeftToRightText)
      {
        FT_Get_Kerning( font()->mFT_Face, previous, glyph->glyphIndex(), FT_KERNING_DEFAULT, &delta );
        pen.x() += delta.x / 64.0f;
      }
      else
      if (layout() == RightToLeftText)
      {
        FT_Get_Kerning( font()->mFT_Face, glyph->glyphIndex(), previous, FT_KERNING_DEFAULT, &delta );
        pen.x() -= delta.x / 64.0f;
      }
      pen.y() += delta.y / 64.0f;
    }
    previous = glyph->glyphIndex();

    if ( glyph->textureHandle() )
    {
      int left = layout() == RightToLeftText ? -glyph->left() : +glyph->left();

      vect[0].x() = pen.x() + glyph->width()*0 + left -1;
      vect[0].y() = pen.y() + glyph->height()*0 + glyph->top() - glyph->height() -1;

      vect[1].x() = pen.x() + glyph->width()*1 + left +1;
      vect[1].y() = pen.y() + glyph->height()*0 + glyph->top() - glyph->height() -1;

      vect[2].x() = pen.x() + glyph->width()*1 + left +1;
      vect[2].y() = pen.y() + glyph->height()*1 + glyph->top() - glyph->height() +1;

      vect[3].x() = pen.x() + glyph->width()*0 + left -1;
      vect[3].y() = pen.y() + glyph->height()*1 + glyph->top() - glyph->height() +1;

      if (layout() == RightToLeftText)
      {
        vect[0].x() -= glyph->width()-1 +2;
        vect[1].x() -= glyph->width()-1 +2;
        vect[2].x() -= glyph->width()-1 +2;
        vect[3].x() -= glyph->width()-1 +2;
      }

      vect[0].y() -= mFont->mHeight;
      vect[1].y() -= mFont->mHeight;
      vect[2].y() -= mFont->mHeight;
      vect[3].y() -= mFont->mHeight;
    }

    aabb.addPoint( (vec3)vect[0] );
    aabb.addPoint( (vec3)vect[1] );
    aabb.addPoint( (vec3)vect[2] );
    aabb.addPoint( (vec3)vect[3] );

    if (layout() == LeftToRightText)
      pen += glyph->advance();
    else
    if (layout() == RightToLeftText)
      pen -= glyph->advance();
  }

  return aabb;
}
//-----------------------------------------------------------------------------
void CoreText::renderBackground(const Actor*, const Camera*) const
{
  // mic fixme:
  // rendering of border and background follows different rules in 3D compared from 2D: lines and polygons follow different rasterization rules!

  // Background color
  glColor4fv(mBackgroundColor.ptr());

  // Constant normal
  glNormal3fv( fvec3(0,0,1).ptr() );

  vec3 a,b,c,d;
  AABB bbox = boundingRect(); // mic fixme: this guy recomputes the bounds again instead of using the precomputed one!!
  a = bbox.minCorner();
  b.x() = (float)bbox.maxCorner().x();
  b.y() = (float)bbox.minCorner().y();
  c = bbox.maxCorner();
  d.x() = (float)bbox.minCorner().x();
  d.y() = (float)bbox.maxCorner().y();
  // set z to 0
  a.z() = b.z() = c.z() = d.z() = 0;

  fvec3 vect[] = { (fvec3)a, (fvec3)b, (fvec3)c, (fvec3)d };
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect);

  glDrawArrays(GL_QUADS,0,4);

  glDisableClientState( GL_VERTEX_ARRAY );
}
//-----------------------------------------------------------------------------
void CoreText::renderBorder(const Actor*, const Camera*) const
{
  // mic fixme:
  // rendering of border and background follows different rules in 3D compared from 2D: lines and polygons follow different rasterization rules!

  // Border color
  glColor4fv(mBorderColor.ptr());

  // Constant normal
  glNormal3fv( fvec3(0,0,1).ptr() );

  vec3 a,b,c,d;
  AABB bbox = boundingRect(); // mic fixme: this guy recomputes the bounds again instead of using the precomputed one!!
  a = bbox.minCorner();
  b.x() = (float)bbox.maxCorner().x();
  b.y() = (float)bbox.minCorner().y();
  c = bbox.maxCorner();
  d.x() = (float)bbox.minCorner().x();
  d.y() = (float)bbox.maxCorner().y();
  // set z to 0
  a.z() = b.z() = c.z() = d.z() = 0;

  fvec3 vect[] = { (fvec3)a, (fvec3)b, (fvec3)c, (fvec3)d };
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect);

  glDrawArrays(GL_LINE_LOOP,0,4);

  glDisableClientState( GL_VERTEX_ARRAY );
}
//-----------------------------------------------------------------------------
AABB CoreText::boundingRect() const
{
  return boundingRect(text());
}
//-----------------------------------------------------------------------------
AABB CoreText::boundingRect(const String& text) const
{
  AABB bbox = rawboundingRect( text );
  bbox.setMaxCorner( bbox.maxCorner() + vec3(2.0f*margin(), 2.0f*margin(), 0) );

  // normalize coordinate orgin to the bottom/left corner
  vec3 min = bbox.minCorner() - bbox.minCorner();
  vec3 max = bbox.maxCorner() - bbox.minCorner();

  // text pivot

  if (textOrigin() & AlignHCenter)
  {
    VL_CHECK( !(textOrigin() & AlignRight) )
    VL_CHECK( !(textOrigin() & AlignLeft) )
    min.x() -= int(bbox.width() / 2.0);
    max.x() -= int(bbox.width() / 2.0);
  }

  if (textOrigin() & AlignRight)
  {
    VL_CHECK( !(textOrigin() & AlignHCenter) )
    VL_CHECK( !(textOrigin() & AlignLeft) )
    min.x() -= (int)bbox.width();
    max.x() -= (int)bbox.width();
  }

  if (textOrigin() & AlignTop)
  {
    VL_CHECK( !(textOrigin() & AlignBottom) )
    VL_CHECK( !(textOrigin() & AlignVCenter) )
    min.y() -= (int)bbox.height();
    max.y() -= (int)bbox.height();
  }

  if (textOrigin() & AlignVCenter)
  {
    VL_CHECK( !(textOrigin() & AlignTop) )
    VL_CHECK( !(textOrigin() & AlignBottom) )
    min.y() -= int(bbox.height() / 2.0);
    max.y() -= int(bbox.height() / 2.0);
  }

  AABB aabb;
  aabb.setMinCorner(min);
  aabb.setMaxCorner(max);
  return aabb;
}
//-----------------------------------------------------------------------------
