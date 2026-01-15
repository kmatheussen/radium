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

#include <vlGraphics/Text.hpp>
#include <vlGraphics/OpenGLContext.hpp>
#include <vlGraphics/Actor.hpp>
#include <vlCore/Log.hpp>

#include <ft2build.h>
#include FT_FREETYPE_H

using namespace vl;

//-----------------------------------------------------------------------------
void Text::render_Implementation(const Actor* actor, const Shader*, const Camera* camera, OpenGLContext* gl_context) const
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
  
  // restore the right color and normal since we changed them
  glColor4fv( gl_context->color().ptr() );
  glNormal3fv( gl_context->normal().ptr() );
}
//-----------------------------------------------------------------------------
void Text::renderText(const Actor* actor, const Camera* camera, const fvec4& color, const fvec2& offset) const
{
  if(!mFont)
  {
    Log::error("Text::renderText() error: no Font assigned to the Text object.\n");
    VL_TRAP()
    return;
  }

  if (!font()->mFT_Face)
  {
    Log::error("Text::renderText() error: invalid FT_Face: probably you tried to load an unsupported font format.\n");
    VL_TRAP()
    return;
  }

  int viewport[] = { camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height() };

  if (viewport[2] < 1) viewport[2] = 1;
  if (viewport[3] < 1) viewport[3] = 1;

  // note that we only save and restore the server side states

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    VL_CHECK_OGL();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    // glLoadIdentity();
    // gluOrtho2D( -0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f );

    // clever trick part #1
    fmat4 mat = fmat4::getOrtho(-0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f, -1, +1);
    mat.e(2,2) = 1.0f; // preserve the z value from the incoming vertex.
    mat.e(2,3) = 0.0f;
    glLoadMatrixf(mat.ptr());

    VL_CHECK_OGL();
  }

  AABB rbbox = rawboundingRect( text() ); // for text alignment
  VL_CHECK(rbbox.maxCorner().z() == 0)
  VL_CHECK(rbbox.minCorner().z() == 0)
  AABB bbox = rbbox;
  int applied_margin = backgroundEnabled() || borderEnabled() ? margin() : 0;
  bbox.setMaxCorner( bbox.maxCorner() + vec3(2.0f*applied_margin,2.0f*applied_margin,0) );
  VL_CHECK(bbox.maxCorner().z() == 0)
  VL_CHECK(bbox.minCorner().z() == 0)

  // basic render states

  fvec2 pen(0,0);

  float texc[] = { 0,0, 0,0, 0,0, 0,0 };
  VL_glActiveTexture( GL_TEXTURE0 );
  glEnable(GL_TEXTURE_2D);
  VL_glClientActiveTexture( GL_TEXTURE0 );
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );
  glTexCoordPointer(2, GL_FLOAT, 0, texc);

  // Constant color
  glColor4f( color.r(), color.g(), color.b(), color.a() );

  // Constant normal
  glNormal3f( 0, 0, 1 );

  fvec3 vect[4];
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect[0].ptr());

  FT_Long has_kerning = FT_HAS_KERNING( font()->mFT_Face );
  FT_UInt previous = 0;

  // viewport alignment
  fmat4 m = mMatrix;

  int w = camera->viewport()->width();
  int h = camera->viewport()->height();

  if (w < 1) w = 1;
  if (h < 1) h = 1;

  if ( !(actor && actor->transform()) && mode() == Text2D )
  {
    if (viewportAlignment() & AlignHCenter)
    {
      VL_CHECK( !(viewportAlignment() & AlignRight) )
      VL_CHECK( !(viewportAlignment() & AlignLeft) )
      // vect[i].x() += int((viewport[2]-1.0f) / 2.0f);
      m.translate( (float)int((w-1.0f) / 2.0f), 0, 0);
    }

    if (viewportAlignment() & AlignRight)
    {
      VL_CHECK( !(viewportAlignment() & AlignHCenter) )
      VL_CHECK( !(viewportAlignment() & AlignLeft) )
      // vect[i].x() += int(viewport[2]-1.0f);
      m.translate( (float)int(w-1.0f), 0, 0);
    }

    if (viewportAlignment() & AlignTop)
    {
      VL_CHECK( !(viewportAlignment() & AlignBottom) )
      VL_CHECK( !(viewportAlignment() & AlignVCenter) )
      // vect[i].y() += int(viewport[3]-1.0f);
      m.translate( 0, (float)int(h-1.0f), 0);
    }

    if (viewportAlignment() & AlignVCenter)
    {
      VL_CHECK( !(viewportAlignment() & AlignTop) )
      VL_CHECK( !(viewportAlignment() & AlignBottom) )
      // vect[i].y() += int((viewport[3]-1.0f) / 2.0f);
      m.translate( 0, (float)int((h-1.0f) / 2.0f), 0);
    }
  }

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
    int displace = 0;
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
        displace = 0;
      else
      if (textAlignment() == TextAlignLeft)
        displace = - int(rbbox.width() - linebox.width());
      else
      if (textAlignment() == TextAlignCenter)
        displace = - int((rbbox.width() - linebox.width()) / 2.0f);
    }
    if (layout() == LeftToRightText)
    {
      if (textAlignment() == TextAlignRight)
        displace = int(rbbox.width() - linebox.width());
      else
      if (textAlignment() == TextAlignLeft)
        displace = 0;
      else
      if (textAlignment() == TextAlignCenter)
        displace = + int((rbbox.width() - linebox.width()) / 2.0f);
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

      if ( kerningEnabled() && has_kerning && previous && glyph->glyphIndex() )
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

        // triangle strip layout

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
  #if (1)
          vect[0].x() -= glyph->width()-1 +2;
          vect[1].x() -= glyph->width()-1 +2;
          vect[2].x() -= glyph->width()-1 +2;
          vect[3].x() -= glyph->width()-1 +2;
  #endif
        }

        vect[0].y() -= mFont->mHeight;
        vect[1].y() -= mFont->mHeight;
        vect[2].y() -= mFont->mHeight;
        vect[3].y() -= mFont->mHeight;

  #if (1)
        // normalize coordinate orgin to the bottom/left corner
        vect[0] -= (fvec3)bbox.minCorner();
        vect[1] -= (fvec3)bbox.minCorner();
        vect[2] -= (fvec3)bbox.minCorner();
        vect[3] -= (fvec3)bbox.minCorner();
  #endif

  #if (1)
        vect[0].x() += applied_margin + displace;
        vect[1].x() += applied_margin + displace;
        vect[2].x() += applied_margin + displace;
        vect[3].x() += applied_margin + displace;

        vect[0].y() += applied_margin;
        vect[1].y() += applied_margin;
        vect[2].y() += applied_margin;
        vect[3].y() += applied_margin;
  #endif

        // apply offset for outline rendering
        vect[0].x() += offset.x();
        vect[0].y() += offset.y();
        vect[1].x() += offset.x();
        vect[1].y() += offset.y();
        vect[2].x() += offset.x();
        vect[2].y() += offset.y();
        vect[3].x() += offset.x();
        vect[3].y() += offset.y();

        // alignment
        for(int i=0; i<4; ++i)
        {
          if (alignment() & AlignHCenter)
          {
            VL_CHECK( !(alignment() & AlignRight) )
            VL_CHECK( !(alignment() & AlignLeft) )
            vect[i].x() -= (int)(bbox.width() / 2.0f);
          }

          if (alignment() & AlignRight)
          {
            VL_CHECK( !(alignment() & AlignHCenter) )
            VL_CHECK( !(alignment() & AlignLeft) )
            vect[i].x() -= (int)bbox.width();
          }

          if (alignment() & AlignTop)
          {
            VL_CHECK( !(alignment() & AlignBottom) )
            VL_CHECK( !(alignment() & AlignVCenter) )
            vect[i].y() -= (int)bbox.height();
          }

          if (alignment() & AlignVCenter)
          {
            VL_CHECK( !(alignment() & AlignTop) )
            VL_CHECK( !(alignment() & AlignBottom) )
            vect[i].y() -= int(bbox.height() / 2.0);
          }
        }

        // apply text transform
        vect[0] = m * vect[0];
        vect[1] = m * vect[1];
        vect[2] = m * vect[2];
        vect[3] = m * vect[3];

        // actor's transform following in Text2D
        if ( actor->transform() && mode() == Text2D )
        {
          vec4 v(0,0,0,1);
          v = actor->transform()->worldMatrix() * v;

          camera->project(v,v);

          // from screen space to viewport space
          v.x() -= viewport[0];
          v.y() -= viewport[1];

          if (mClampX)
            v.x() = (float)int(v.x());

          if (mClampY)
            v.y() = (float)int(v.y());

          //v.x() = (float)int(v.x());
          //v.y() = (float)int(v.y());

          vect[0].x() += (float)v.x();
          vect[0].y() += (float)v.y();
          vect[1].x() += (float)v.x();
          vect[1].y() += (float)v.y();
          vect[2].x() += (float)v.x();
          vect[2].y() += (float)v.y();
          vect[3].x() += (float)v.x();
          vect[3].y() += (float)v.y();

          // clever trick part #2
          vect[0].z() = 
          vect[1].z() = 
          vect[2].z() = 
          vect[3].z() = float((v.z() - 0.5f) / 0.5f);
        }

        glDrawArrays(GL_TRIANGLE_FAN, 0, 4); VL_CHECK_OGL();

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

  glDisableClientState( GL_VERTEX_ARRAY ); VL_CHECK_OGL();
  glDisableClientState( GL_TEXTURE_COORD_ARRAY ); VL_CHECK_OGL();

  VL_CHECK_OGL();

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix(); VL_CHECK_OGL()

    glMatrixMode(GL_PROJECTION);
    glPopMatrix(); VL_CHECK_OGL()
  }

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D,0);
}
//-----------------------------------------------------------------------------
// returns the raw bounding box of the string, i.e. without alignment, margin and matrix transform.
AABB Text::rawboundingRect(const String& text) const
{
  AABB aabb;

  if(!font())
  {
    Log::error("Text::rawboundingRect() error: no Font assigned to the Text object.\n");
    VL_TRAP()
    return aabb;
  }

  if (!font()->mFT_Face)
  {
    Log::error("Text::rawboundingRect() error: invalid FT_Face: probably you tried to load an unsupported font format.\n");
    VL_TRAP()
    return aabb;
  }

  fvec2 pen(0,0);
  fvec3 vect[4];

  FT_Long has_kerning = FT_HAS_KERNING( font()->mFT_Face );
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

    if ( kerningEnabled() && has_kerning && previous && glyph->glyphIndex())
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
#if (1)
        vect[0].x() -= glyph->width()-1 +2;
        vect[1].x() -= glyph->width()-1 +2;
        vect[2].x() -= glyph->width()-1 +2;
        vect[3].x() -= glyph->width()-1 +2;
#endif
      }

      vect[0].y() -= mFont->mHeight;
      vect[1].y() -= mFont->mHeight;
      vect[2].y() -= mFont->mHeight;
      vect[3].y() -= mFont->mHeight;

#if(0)
      // apply margin
      //if (layout() == LeftToRightText)
      //{
        vect[0].x() += margin();
        vect[1].x() += margin();
        vect[2].x() += margin();
        vect[3].x() += margin();
      //}
      //else
      //if (layout() == RightToLeftText)
      //{
      //  vect[0].x() -= margin();
      //  vect[1].x() -= margin();
      //  vect[2].x() -= margin();
      //  vect[3].x() -= margin();
      //}

      vect[0].y() += margin();
      vect[1].y() += margin();
      vect[2].y() += margin();
      vect[3].y() += margin();
#endif

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
void Text::renderBackground(const Actor* actor, const Camera* camera) const
{
  int viewport[] = { camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height() };

  if (viewport[2] < 1) viewport[2] = 1;
  if (viewport[3] < 1) viewport[3] = 1;

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    VL_CHECK_OGL();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    //glLoadIdentity();
    //gluOrtho2D( -0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f );

    // clever trick part #1
    fmat4 mat = fmat4::getOrtho(-0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f, -1, +1);
    mat.e(2,2) = 1.0f;
    mat.e(2,3) = 0.0f;
    glLoadMatrixf(mat.ptr());
    VL_CHECK_OGL();
  }

  // Constant color
  glColor4f(mBackgroundColor.r(),mBackgroundColor.g(), mBackgroundColor.b(), mBackgroundColor.a());

  // Constant normal
  glNormal3f(0, 0, 1);

  vec3 a,b,c,d;
  boundingRectTransformed( a, b, c, d, camera, mode() == Text2D ? actor : NULL );
  fvec3 vect[] = { (fvec3)a, (fvec3)b, (fvec3)c, (fvec3)d };
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect);

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableClientState( GL_VERTEX_ARRAY );

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix(); VL_CHECK_OGL()

    glMatrixMode(GL_PROJECTION);
    glPopMatrix(); VL_CHECK_OGL()
  }
}
//-----------------------------------------------------------------------------
void Text::renderBorder(const Actor* actor, const Camera* camera) const
{
  int viewport[] = { camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height() };

  if (viewport[2] < 1) viewport[2] = 1;
  if (viewport[3] < 1) viewport[3] = 1;

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    VL_CHECK_OGL();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    //glLoadIdentity();
    //gluOrtho2D( -0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f );

    // clever trick part #1
    fmat4 mat = fmat4::getOrtho(-0.5f, viewport[2]-0.5f, -0.5f, viewport[3]-0.5f, -1, +1);
    mat.e(2,2) = 1.0f;
    mat.e(2,3) = 0.0f;
    glLoadMatrixf(mat.ptr());
    VL_CHECK_OGL();
  }

  // Constant color
  glColor4f(mBorderColor.r(), mBorderColor.g(), mBorderColor.b(), mBorderColor.a());

  // Constant normal
  glNormal3f( 0, 0, 1 );

  vec3 a,b,c,d;
  boundingRectTransformed( a, b, c, d, camera, mode() == Text2D ? actor : NULL );
  fvec3 vect[] = { (fvec3)a, (fvec3)b, (fvec3)c, (fvec3)d };
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer(3, GL_FLOAT, 0, vect);

  glDrawArrays(GL_LINE_LOOP, 0, 4);

  glDisableClientState( GL_VERTEX_ARRAY );

  if (mode() == Text2D)
  {
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix(); VL_CHECK_OGL()

    glMatrixMode(GL_PROJECTION);
    glPopMatrix(); VL_CHECK_OGL()
  }
}
//-----------------------------------------------------------------------------
//! Returns the plain 2D bounding box of the text, without taking into consideration
//! the Text's matrix transform and the eventual actor's transform
AABB Text::boundingRect() const
{
  return boundingRect(text());
}
//-----------------------------------------------------------------------------
AABB Text::boundingRect(const String& text) const
{
  int applied_margin = backgroundEnabled() || borderEnabled() ? margin() : 0;
  AABB bbox = rawboundingRect( text );
  bbox.setMaxCorner( bbox.maxCorner() + vec3(2.0f*applied_margin,2.0f*applied_margin,0) );

  // normalize coordinate orgin to the bottom/left corner
  vec3 min = bbox.minCorner() - bbox.minCorner();
  vec3 max = bbox.maxCorner() - bbox.minCorner();

  // normalize coordinate orgin to the bottom/left corner

  // alignment

  if (alignment() & AlignHCenter)
  {
    VL_CHECK( !(alignment() & AlignRight) )
    VL_CHECK( !(alignment() & AlignLeft) )
    min.x() -= int(bbox.width() / 2.0);
    max.x() -= int(bbox.width() / 2.0);
  }

  if (alignment() & AlignRight)
  {
    VL_CHECK( !(alignment() & AlignHCenter) )
    VL_CHECK( !(alignment() & AlignLeft) )
    min.x() -= (int)bbox.width();
    max.x() -= (int)bbox.width();
  }

  if (alignment() & AlignTop)
  {
    VL_CHECK( !(alignment() & AlignBottom) )
    VL_CHECK( !(alignment() & AlignVCenter) )
    min.y() -= (int)bbox.height();
    max.y() -= (int)bbox.height();
  }

  if (alignment() & AlignVCenter)
  {
    VL_CHECK( !(alignment() & AlignTop) )
    VL_CHECK( !(alignment() & AlignBottom) )
    min.y() -= int(bbox.height() / 2.0);
    max.y() -= int(bbox.height() / 2.0);
  }

  // no matrix transform applied
  // ...

  // no actor's transform applied
  // ...

  AABB aabb;
  aabb.setMinCorner(min);
  aabb.setMaxCorner(max);
  return aabb;
}
//-----------------------------------------------------------------------------
//! Returns the fully transformed bounding box.
//! \p actor is needed only if you are using the actor's transform with the Text2D
//! to make the Text2D text follow the actor on the screen or you are using the Text3D to make the text
//! follow the actor's transform in 3D.
//!
//! The layout of the a, b, c and d points is the following:\n
//! \n
//! d---------c\n
//! |         |\n
//! |         |\n
//! a---------b\n
//! \n
//! Of course the above layout can be scaled, flipped, rotated and so on according to the given Text's matrix.
AABB Text::boundingRectTransformed(const Camera* camera, const Actor* actor) const
{
  vec3 a, b, c, d;
  return boundingRectTransformed(a, b, c, d, camera, actor);
}
//-----------------------------------------------------------------------------
AABB Text::boundingRectTransformed(vec3& a, vec3& b, vec3& c, vec3& d, const Camera* camera, const Actor* actor) const
{
  AABB bbox = boundingRect();

  a = bbox.minCorner();
  b.x() = (float)bbox.maxCorner().x();
  b.y() = (float)bbox.minCorner().y();
  c = bbox.maxCorner();
  d.x() = (float)bbox.minCorner().x();
  d.y() = (float)bbox.maxCorner().y();
  // set z to 0
  a.z() = b.z() = c.z() = d.z() = 0;

  // viewport alignment
  fmat4 m = mMatrix;

  int w = camera->viewport()->width();
  int h = camera->viewport()->height();

  if (w < 1) w = 1;
  if (h < 1) h = 1;

  if ( !(actor && actor->transform()) && mode() == Text2D )
  {
    if (viewportAlignment() & AlignHCenter)
    {
      VL_CHECK( !(viewportAlignment() & AlignRight) )
      VL_CHECK( !(viewportAlignment() & AlignLeft) )
      // vect[i].x() += int((viewport[2]-1.0f) / 2.0f);
      m.translate( (float)int((w-1.0f) / 2.0f), 0, 0);
    }

    if (viewportAlignment() & AlignRight)
    {
      VL_CHECK( !(viewportAlignment() & AlignHCenter) )
      VL_CHECK( !(viewportAlignment() & AlignLeft) )
      // vect[i].x() += int(viewport[2]-1.0f);
      m.translate( (float)int(w-1.0f), 0, 0);
    }

    if (viewportAlignment() & AlignTop)
    {
      VL_CHECK( !(viewportAlignment() & AlignBottom) )
      VL_CHECK( !(viewportAlignment() & AlignVCenter) )
      // vect[i].y() += int(viewport[3]-1.0f);
      m.translate( 0, (float)int(h-1.0f), 0);
    }

    if (viewportAlignment() & AlignVCenter)
    {
      VL_CHECK( !(viewportAlignment() & AlignTop) )
      VL_CHECK( !(viewportAlignment() & AlignBottom) )
      // vect[i].y() += int((viewport[3]-1.0f) / 2.0f);
      m.translate( 0, (float)int((h-1.0f) / 2.0f), 0);
    }
  }

  // ??? mix fixme: remove all these castings!
  // apply matrix transform
  a = (mat4)m * a;
  b = (mat4)m * b;
  c = (mat4)m * c;
  d = (mat4)m * d;

  // apply actor's transform
  if ( actor && actor->transform() )
  {
    if ( mode() == Text3D )
    {
      a = actor->transform()->worldMatrix() * a;
      b = actor->transform()->worldMatrix() * b;
      c = actor->transform()->worldMatrix() * c;
      d = actor->transform()->worldMatrix() * d;
    }
    else
    if ( mode() == Text2D )
    {
      // transform v
      vec4 v(0,0,0,1);
      v = actor->transform()->worldMatrix() * v;

      // project to screen
      camera->project(v,v);

      // from screen space to viewport space
      int viewport[] = { camera->viewport()->x(), camera->viewport()->y(), camera->viewport()->width(), camera->viewport()->height() };
      v.x() -= viewport[0];
      v.y() -= viewport[1];

      if (mClampX)
        v.x() = (float)int(v.x());

      if (mClampY)
        v.y() = (float)int(v.y());

      a += v.xyz();
      b += v.xyz();
      c += v.xyz();
      d += v.xyz();

      // clever trick part #2
      a.z() = 
      b.z() = 
      c.z() = 
      d.z() = (v.z() - 0.5f) / 0.5f;
    }
  }

  bbox.setNull();
  bbox.addPoint(a);
  bbox.addPoint(b);
  bbox.addPoint(c);
  bbox.addPoint(d);
  return bbox;
}
//-----------------------------------------------------------------------------
void Text::translate(float x, float y, float z)
{
  mMatrix.translate(x,y,z);
}
//-----------------------------------------------------------------------------
void Text::rotate(float degrees, float x, float y, float z)
{
  mMatrix.rotate(degrees,x,y,z);
}
//-----------------------------------------------------------------------------
void Text::resetMatrix()
{
  mMatrix.setIdentity();
}
//-----------------------------------------------------------------------------
