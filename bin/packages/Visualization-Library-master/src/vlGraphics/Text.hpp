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

#ifndef Text_INCLUDE_ONCE
#define Text_INCLUDE_ONCE

#include <vlGraphics/Font.hpp>
#include <vlGraphics/Renderable.hpp>
#include <vlCore/vlnamespace.hpp>
#include <vlCore/String.hpp>
#include <vlCore/Rect.hpp>
#include <map>

namespace vl
{
  /**
   * A Renderable that renders text with a given Font.
   * \sa
   * - Actor
   * - VectorGraphics
  */
  class VLGRAPHICS_EXPORT Text: public Renderable
  {
    VL_INSTRUMENT_CLASS(vl::Text, Renderable)

  public:
    Text(): mColor(1,1,1,1), mBorderColor(0,0,0,1), mBackgroundColor(1,1,1,1), mOutlineColor(0,0,0,1), mShadowColor(0,0,0,0.5f), mShadowVector(2,-2), 
      mInterlineSpacing(5), mAlignment(AlignBottom|AlignLeft), mViewportAlignment(AlignBottom|AlignLeft), mMargin(5), mMode(Text2D), mLayout(LeftToRightText), mTextAlignment(TextAlignLeft), 
      mBorderEnabled(false), mBackgroundEnabled(false), mOutlineEnabled(false), mShadowEnabled(false), mClampX(true), mClampY(true), mKerningEnabled(true) 
    {
      VL_DEBUG_SET_OBJECT_NAME()
    }

    const String& text() const { return mText; }
    void setText(const String& text) { mText = text; }

    const fvec4& color() const { return mColor; }
    void setColor(const fvec4& color) { mColor = color; }

    const fvec4& borderColor() const { return mBorderColor; }
    void setBorderColor(const fvec4& border_color) { mBorderColor = border_color; }

    const fvec4& outlineColor() const { return mOutlineColor; }
    void setOutlineColor(const fvec4& outline_color) { mOutlineColor = outline_color; }

    const fvec4& backgroundColor() const { return mBackgroundColor; }
    void setBackgroundColor(const fvec4& background_color) { mBackgroundColor = background_color; }

    const fvec4& shadowColor() const { return mShadowColor; }
    void setShadowColor(const fvec4& shadow_color) { mShadowColor = shadow_color; }

    const fvec2& shadowVector() const { return mShadowVector; }
    void setShadowVector(const fvec2& shadow_vector) { mShadowVector = shadow_vector; }

    int margin() const { return mMargin; }
    void setMargin(int margin) { mMargin = margin; }

    const Font* font() const { return mFont.get(); }
    Font* font() { return mFont.get(); }
    void setFont(Font* font) { mFont = font; }

    const fmat4 matrix() const { return mMatrix; }
    void setMatrix(const fmat4& matrix) { mMatrix = matrix; }

    int  alignment() const { return mAlignment; }
    void setAlignment(int  align) { mAlignment = align; }

    int  viewportAlignment() const { return mViewportAlignment; }
    void setViewportAlignment(int  align) { mViewportAlignment = align; }

    float interlineSpacing() const { return mInterlineSpacing; }
    void setInterlineSpacing(float spacing) { mInterlineSpacing = spacing; }

    ETextMode mode() const { return mMode; }
    void setMode(ETextMode mode) { mMode = mode; }

    ETextLayout layout() const { return mLayout; }
    void setLayout(ETextLayout layout) { mLayout = layout; }

    ETextAlign textAlignment() const { return mTextAlignment; }
    void setTextAlignment(ETextAlign align) { mTextAlignment = align; }

    bool borderEnabled() const { return mBorderEnabled; }
    void setBorderEnabled(bool border) { mBorderEnabled = border; }

    bool backgroundEnabled() const { return mBackgroundEnabled; }
    void setBackgroundEnabled(bool background) { mBackgroundEnabled = background; }

    bool kerningEnabled() const { return mKerningEnabled; }
    void setKerningEnabled(bool kerning) { mKerningEnabled = kerning; }

    bool outlineEnabled() const { return mOutlineEnabled; }
    void setOutlineEnabled(bool outline) { mOutlineEnabled = outline; }

    bool shadowEnabled() const { return mShadowEnabled; }
    void setShadowEnabled(bool shadow) { mShadowEnabled = shadow; }

    bool clampX() const { return mClampX; }
    void setClampX(bool clamp) { mClampX = clamp; }

    bool clampY() const { return mClampY; }
    void setClampY(bool clamp) { mClampY = clamp; }

    virtual void render_Implementation(const Actor* actor, const Shader* shader, const Camera* camera, OpenGLContext* gl_context) const;
    void computeBounds_Implementation() { setBoundingBox(AABB()); setBoundingSphere(Sphere()); }
    AABB boundingRect() const;
    AABB boundingRect(const String& text) const;
    AABB boundingRectTransformed(vec3& a, vec3& b, vec3& c, vec3& d, const Camera* camera, const Actor* actor=NULL) const;
    AABB boundingRectTransformed(const Camera* camera, const Actor* actor=NULL) const;

    void translate(float x, float y, float z);
    void rotate(float degrees, float x, float y, float z);
    void resetMatrix();

    // Renderable interface implementation.

    virtual void updateDirtyBufferObject(EBufferObjectUpdateMode) {}

    virtual void deleteBufferObject() {}

  protected:
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
    fmat4 mMatrix;
    float mInterlineSpacing;
    int mAlignment;
    int mViewportAlignment;
    int mMargin;
    ETextMode mMode;
    ETextLayout mLayout;
    ETextAlign mTextAlignment;
    bool mBorderEnabled;
    bool mBackgroundEnabled;
    bool mOutlineEnabled;
    bool mShadowEnabled;
    bool mClampX;
    bool mClampY;
    bool mKerningEnabled;
  };
}

#endif
