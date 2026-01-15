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

#ifndef CoreText_INCLUDE_ONCE
#define CoreText_INCLUDE_ONCE

#include <vlGraphics/Font.hpp>
#include <vlGraphics/Renderable.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/String.hpp>
#include <vlCore/Rect.hpp>
#include <map>

namespace vl
{
  /**
   * Experimental. Not yet functional.
  */
  class VLGRAPHICS_EXPORT CoreText: public Renderable
  {
    VL_INSTRUMENT_CLASS(vl::CoreText, Renderable)

  public:
    CoreText(): mColor(1,1,1,1), mBorderColor(0,0,0,1), mBackgroundColor(1,1,1,1), mOutlineColor(0,0,0,1), mShadowColor(0,0,0,0.5f), mShadowVector(2,-2), 
      mTextOrigin(AlignBottom|AlignLeft), mMargin(5), mLayout(LeftToRightText), mTextAlignment(TextAlignLeft), 
      mBorderEnabled(false), mBackgroundEnabled(false), mOutlineEnabled(false), mShadowEnabled(false), mKerningEnabled(true) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    //! The text to be rendered.
    const String& text() const { return mText; }
    //! The text to be rendered.
    void setText(const String& text) { mText = text; }

    //! The color of the text.
    const fvec4& color() const { return mColor; }
    //! The color of the text.
    void setColor(const fvec4& color) { mColor = color; }

    //! The margin to be left around the text.
    int margin() const { return mMargin; }
    //! The margin to be left around the text.
    void setMargin(int margin) { mMargin = margin; }

    //! The font to be used to render the text.
    const Font* font() const { return mFont.get(); }
    //! The font to be used to render the text.
    Font* font() { return mFont.get(); }
    //! The font to be used to render the text.
    void setFont(Font* font) { mFont = font; }

    //! Text layout: left to right, right to left.
    ETextLayout layout() const { return mLayout; }
    //! Text layout: left to right, right to left.
    void setLayout(ETextLayout layout) { mLayout = layout; }

    //! Text alignment: left, right, center, justify.
    ETextAlign textAlignment() const { return mTextAlignment; }
    //! Text alignment: left, right, center, justify.
    void setTextAlignment(ETextAlign align) { mTextAlignment = align; }

    //! The origin of the text (pivot point for offsetting and rotations).
    int  textOrigin() const { return mTextOrigin; }
    //! The origin of the text (pivot point for offsetting and rotations).
    void setTextOrigin(int align) { mTextOrigin = align; }

    //! If enabled text rendering uses kerning information for better quality results (slower).
    bool kerningEnabled() const { return mKerningEnabled; }
    //! If enabled text rendering uses kerning information for better quality results (slower).
    void setKerningEnabled(bool kerning) { mKerningEnabled = kerning; }

    //! If true draws a rectangular border around the text.
    bool borderEnabled() const { return mBorderEnabled; }
    //! If true draws a rectangular border around the text.
    void setBorderEnabled(bool border) { mBorderEnabled = border; }

    //! The color of the rectangular border.
    const fvec4& borderColor() const { return mBorderColor; }
    //! The color of the rectangular border.
    void setBorderColor(const fvec4& border_color) { mBorderColor = border_color; }

    //! If true draws a rectangular background below the text.
    bool backgroundEnabled() const { return mBackgroundEnabled; }
    //! If true draws a rectangular background below the text.
    void setBackgroundEnabled(bool background) { mBackgroundEnabled = background; }

    //! The color of the rectangular background.
    const fvec4& backgroundColor() const { return mBackgroundColor; }
    //! The color of the rectangular background.
    void setBackgroundColor(const fvec4& background_color) { mBackgroundColor = background_color; }

    //! If true the characters are drawn with an outline.
    bool outlineEnabled() const { return mOutlineEnabled; }
    //! If true the characters are drawn with an outline.
    void setOutlineEnabled(bool outline) { mOutlineEnabled = outline; }

    //! The color of the character outline.
    const fvec4& outlineColor() const { return mOutlineColor; }
    //! The color of the character outline.
    void setOutlineColor(const fvec4& outline_color) { mOutlineColor = outline_color; }

    //! If true a sort of shadow is rendered below the text.
    bool shadowEnabled() const { return mShadowEnabled; }
    //! If true a sort of shadow is rendered below the text.
    void setShadowEnabled(bool shadow) { mShadowEnabled = shadow; }

    //! The color of the text shadow.
    const fvec4& shadowColor() const { return mShadowColor; }
    //! The color of the text shadow.
    void setShadowColor(const fvec4& shadow_color) { mShadowColor = shadow_color; }

    //! The offset vector of the shadow.
    const fvec2& shadowVector() const { return mShadowVector; }
    //! The offset vector of the shadow.
    void setShadowVector(const fvec2& shadow_vector) { mShadowVector = shadow_vector; }

    //! Returns the plain 2D bounding box of the text, in local coordinates.
    AABB boundingRect() const;

    //! Returns the plain 2D bounding box of the text, in local coordinates.
    AABB boundingRect(const String& text) const;

    // --- Renderable interface implementation ---

    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) {}

    virtual void deleteBufferObject() {}

  protected:
    virtual void render_Implementation(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context) const;
    void computeBounds_Implementation() { setBoundingBox(AABB()); setBoundingSphere(Sphere()); }

    void renderText(const Actor*, const Camera* camera, const fvec4& color, const fvec2& offset) const;
    void renderBackground(const Actor* actor, const Camera* camera) const;
    void renderBorder(const Actor* actor, const Camera* camera) const;
    AABB rawboundingRect(const String& text) const;

  protected:
    mutable ref<Font> mFont;
    String mText;
    fvec4 mColor;
    fvec4 mBorderColor;
    fvec4 mBackgroundColor;
    fvec4 mOutlineColor;
    fvec4 mShadowColor;
    fvec2 mShadowVector;
    int mTextOrigin;
    int mMargin;
    ETextLayout mLayout;
    ETextAlign mTextAlignment;
    bool mBorderEnabled;
    bool mBackgroundEnabled;
    bool mOutlineEnabled;
    bool mShadowEnabled;
    bool mKerningEnabled;
  };
}

#endif
