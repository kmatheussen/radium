/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2013 - Raw Material Software Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

#include "juce_mac_CoreGraphicsContext.h"

//==============================================================================
class CoreGraphicsImage : public ImagePixelData
{
public:
    CoreGraphicsImage (const Image::PixelFormat format, const int w, const int h, const bool clearImage)
        : ImagePixelData (format, w, h), cachedImageRef (0)
    {
        pixelStride = format == Image::RGB ? 3 : ((format == Image::ARGB) ? 4 : 1);
        lineStride = (pixelStride * jmax (1, width) + 3) & ~3;

        imageData.allocate ((size_t) (lineStride * jmax (1, height)), clearImage);

        CGColorSpaceRef colourSpace = (format == Image::SingleChannel) ? CGColorSpaceCreateDeviceGray()
                                                                       : CGColorSpaceCreateDeviceRGB();

        context = CGBitmapContextCreate (imageData, (size_t) width, (size_t) height, 8, (size_t) lineStride,
                                         colourSpace, getCGImageFlags (format));

        CGColorSpaceRelease (colourSpace);
    }

    ~CoreGraphicsImage()
    {
        freeCachedImageRef();
        CGContextRelease (context);
    }

    LowLevelGraphicsContext* createLowLevelContext() override
    {
        freeCachedImageRef();
        sendDataChangeMessage();
        return new CoreGraphicsContext (context, height, 1.0f);
    }

    void initialiseBitmapData (Image::BitmapData& bitmap, int x, int y, Image::BitmapData::ReadWriteMode mode) override
    {
        bitmap.data = imageData + x * pixelStride + y * lineStride;
        bitmap.pixelFormat = pixelFormat;
        bitmap.lineStride = lineStride;
        bitmap.pixelStride = pixelStride;

        if (mode != Image::BitmapData::readOnly)
        {
            freeCachedImageRef();
            sendDataChangeMessage();
        }
    }

    ImagePixelData* clone() override
    {
        CoreGraphicsImage* im = new CoreGraphicsImage (pixelFormat, width, height, false);
        memcpy (im->imageData, imageData, (size_t) (lineStride * height));
        return im;
    }

    ImageType* createType() const override    { return new NativeImageType(); }

    //==============================================================================
    static CGImageRef getCachedImageRef (const Image& juceImage, CGColorSpaceRef colourSpace)
    {
        CoreGraphicsImage* const cgim = dynamic_cast<CoreGraphicsImage*> (juceImage.getPixelData());

        if (cgim != nullptr && cgim->cachedImageRef != 0)
        {
            CGImageRetain (cgim->cachedImageRef);
            return cgim->cachedImageRef;
        }

        CGImageRef ref = createImage (juceImage, colourSpace, false);

        if (cgim != nullptr)
        {
            CGImageRetain (ref);
            cgim->cachedImageRef = ref;
        }

        return ref;
    }

    static CGImageRef createImage (const Image& juceImage, CGColorSpaceRef colourSpace, const bool mustOutliveSource)
    {
        const Image::BitmapData srcData (juceImage, Image::BitmapData::readOnly);
        CGDataProviderRef provider;

        if (mustOutliveSource)
        {
            CFDataRef data = CFDataCreate (0, (const UInt8*) srcData.data, (CFIndex) (srcData.lineStride * srcData.height));
            provider = CGDataProviderCreateWithCFData (data);
            CFRelease (data);
        }
        else
        {
            provider = CGDataProviderCreateWithData (0, srcData.data, (size_t) (srcData.lineStride * srcData.height), 0);
        }

        CGImageRef imageRef = CGImageCreate ((size_t) srcData.width,
                                             (size_t) srcData.height,
                                             8, (size_t) srcData.pixelStride * 8,
                                             (size_t) srcData.lineStride,
                                             colourSpace, getCGImageFlags (juceImage.getFormat()), provider,
                                             0, true, kCGRenderingIntentDefault);

        CGDataProviderRelease (provider);
        return imageRef;
    }

    //==============================================================================
    CGContextRef context;
    CGImageRef cachedImageRef;
    HeapBlock<uint8> imageData;
    int pixelStride, lineStride;

private:
    void freeCachedImageRef()
    {
        if (cachedImageRef != 0)
        {
            CGImageRelease (cachedImageRef);
            cachedImageRef = 0;
        }
    }

    static CGBitmapInfo getCGImageFlags (const Image::PixelFormat& format)
    {
       #if JUCE_BIG_ENDIAN
        return format == Image::ARGB ? (kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Big) : kCGBitmapByteOrderDefault;
       #else
        return format == Image::ARGB ? (kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little) : kCGBitmapByteOrderDefault;
       #endif
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CoreGraphicsImage)
};

ImagePixelData::Ptr NativeImageType::create (Image::PixelFormat format, int width, int height, bool clearImage) const
{
    return new CoreGraphicsImage (format == Image::RGB ? Image::ARGB : format, width, height, clearImage);
}

//==============================================================================
CoreGraphicsContext::CoreGraphicsContext (CGContextRef c, const float h, const float scale)
    : context (c),
      flipHeight (h),
      targetScale (scale),
      lastClipRectIsValid (false),
      state (new SavedState())
{
    CGContextRetain (context);
    CGContextSaveGState(context);
    CGContextSetShouldSmoothFonts (context, true);
    CGContextSetShouldAntialias (context, true);
    CGContextSetBlendMode (context, kCGBlendModeNormal);
    rgbColourSpace = CGColorSpaceCreateDeviceRGB();
    greyColourSpace = CGColorSpaceCreateDeviceGray();
    gradientCallbacks.version = 0;
    gradientCallbacks.evaluate = SavedState::gradientCallback;
    gradientCallbacks.releaseInfo = 0;
    setFont (Font());
}

CoreGraphicsContext::~CoreGraphicsContext()
{
    CGContextRestoreGState (context);
    CGContextRelease (context);
    CGColorSpaceRelease (rgbColourSpace);
    CGColorSpaceRelease (greyColourSpace);
}

//==============================================================================
void CoreGraphicsContext::setOrigin (Point<int> o)
{
    CGContextTranslateCTM (context, o.x, -o.y);

    if (lastClipRectIsValid)
        lastClipRect.translate (-o.x, -o.y);
}

void CoreGraphicsContext::addTransform (const AffineTransform& transform)
{
    applyTransform (AffineTransform::verticalFlip ((float) flipHeight)
                                    .followedBy (transform)
                                    .translated (0, (float) -flipHeight)
                                    .scaled (1.0f, -1.0f));
    lastClipRectIsValid = false;

    jassert (getPhysicalPixelScaleFactor() > 0.0f);
    jassert (getPhysicalPixelScaleFactor() > 0.0f);
}

float CoreGraphicsContext::getPhysicalPixelScaleFactor()
{
    const CGAffineTransform t = CGContextGetCTM (context);

    return targetScale * (float) (juce_hypot (t.a, t.c) + juce_hypot (t.b, t.d)) / 2.0f;

//    return targetScale * (float) (t.a + t.d) / 2.0f;
}

bool CoreGraphicsContext::clipToRectangle (const Rectangle<int>& r)
{
    CGContextClipToRect (context, CGRectMake (r.getX(), flipHeight - r.getBottom(), r.getWidth(), r.getHeight()));

    if (lastClipRectIsValid)
    {
        // This is actually incorrect, because the actual clip region may be complex, and
        // clipping its bounds to a rect may not be right... But, removing this shortcut
        // doesn't actually fix anything because CoreGraphics also ignores complex regions
        // when calculating the resultant clip bounds, and makes the same mistake!
        lastClipRect = lastClipRect.getIntersection (r);
        return ! lastClipRect.isEmpty();
    }

    return ! isClipEmpty();
}

bool CoreGraphicsContext::clipToRectangleListWithoutTest (const RectangleList<int>& clipRegion)
{
    if (clipRegion.isEmpty())
    {
        CGContextClipToRect (context, CGRectZero);
        lastClipRectIsValid = true;
        lastClipRect = Rectangle<int>();
        return false;
    }
    else
    {
        const size_t numRects = (size_t) clipRegion.getNumRectangles();
        HeapBlock <CGRect> rects (numRects);

        int i = 0;
        for (const Rectangle<int>* r = clipRegion.begin(), * const e = clipRegion.end(); r != e; ++r)
            rects[i++] = CGRectMake (r->getX(), flipHeight - r->getBottom(), r->getWidth(), r->getHeight());

        CGContextClipToRects (context, rects, numRects);
        lastClipRectIsValid = false;
        return true;
    }
}

bool CoreGraphicsContext::clipToRectangleList (const RectangleList<int>& clipRegion)
{
    return clipToRectangleListWithoutTest (clipRegion) && ! isClipEmpty();
}

void CoreGraphicsContext::excludeClipRectangle (const Rectangle<int>& r)
{
    RectangleList<int> remaining (getClipBounds());
    remaining.subtract (r);
    clipToRectangleListWithoutTest (remaining);
}

void CoreGraphicsContext::clipToPath (const Path& path, const AffineTransform& transform)
{
    createPath (path, transform);
    CGContextClip (context);
    lastClipRectIsValid = false;
}

void CoreGraphicsContext::clipToImageAlpha (const Image& sourceImage, const AffineTransform& transform)
{
    if (! transform.isSingularity())
    {
        Image singleChannelImage (sourceImage);

        if (sourceImage.getFormat() != Image::SingleChannel)
            singleChannelImage = sourceImage.convertedToFormat (Image::SingleChannel);

        CGImageRef image = CoreGraphicsImage::createImage (singleChannelImage, greyColourSpace, true);

        flip();
        AffineTransform t (AffineTransform::verticalFlip (sourceImage.getHeight()).followedBy (transform));
        applyTransform (t);

        CGRect r = convertToCGRect (sourceImage.getBounds());
        CGContextClipToMask (context, r, image);

        applyTransform (t.inverted());
        flip();

        CGImageRelease (image);
        lastClipRectIsValid = false;
    }
}

bool CoreGraphicsContext::clipRegionIntersects (const Rectangle<int>& r)
{
    return getClipBounds().intersects (r);
}

Rectangle<int> CoreGraphicsContext::getClipBounds() const
{
    if (! lastClipRectIsValid)
    {
        CGRect bounds = CGRectIntegral (CGContextGetClipBoundingBox (context));

        lastClipRectIsValid = true;
        lastClipRect.setBounds (roundToInt (bounds.origin.x),
                                roundToInt (flipHeight - (bounds.origin.y + bounds.size.height)),
                                roundToInt (bounds.size.width),
                                roundToInt (bounds.size.height));
    }

    return lastClipRect;
}

bool CoreGraphicsContext::isClipEmpty() const
{
    return getClipBounds().isEmpty();
}

//==============================================================================
void CoreGraphicsContext::saveState()
{
    CGContextSaveGState (context);
    stateStack.add (new SavedState (*state));
}

void CoreGraphicsContext::restoreState()
{
    CGContextRestoreGState (context);

    if (SavedState* const top = stateStack.getLast())
    {
        state = top;
        stateStack.removeLast (1, false);
        lastClipRectIsValid = false;
    }
    else
    {
        jassertfalse; // trying to pop with an empty stack!
    }
}

void CoreGraphicsContext::beginTransparencyLayer (float opacity)
{
    saveState();
    CGContextSetAlpha (context, opacity);
    CGContextBeginTransparencyLayer (context, 0);
}

void CoreGraphicsContext::endTransparencyLayer()
{
    CGContextEndTransparencyLayer (context);
    restoreState();
}

//==============================================================================
void CoreGraphicsContext::setFill (const FillType& fillType)
{
    state->setFill (fillType);

    if (fillType.isColour())
    {
        CGContextSetRGBFillColor (context, fillType.colour.getFloatRed(), fillType.colour.getFloatGreen(),
                                  fillType.colour.getFloatBlue(), fillType.colour.getFloatAlpha());
        CGContextSetAlpha (context, 1.0f);
    }
}

void CoreGraphicsContext::setOpacity (float newOpacity)
{
    state->fillType.setOpacity (newOpacity);
    setFill (state->fillType);
}

void CoreGraphicsContext::setInterpolationQuality (Graphics::ResamplingQuality quality)
{
    CGContextSetInterpolationQuality (context, quality == Graphics::lowResamplingQuality
                                                ? kCGInterpolationLow
                                                : kCGInterpolationHigh);
}

//==============================================================================
void CoreGraphicsContext::fillRect (const Rectangle<int>& r, const bool replaceExistingContents)
{
    fillCGRect (CGRectMake (r.getX(), flipHeight - r.getBottom(), r.getWidth(), r.getHeight()), replaceExistingContents);
}

void CoreGraphicsContext::fillRect (const Rectangle<float>& r)
{
    fillCGRect (CGRectMake (r.getX(), flipHeight - r.getBottom(), r.getWidth(), r.getHeight()), false);
}

void CoreGraphicsContext::fillCGRect (const CGRect& cgRect, const bool replaceExistingContents)
{
    if (replaceExistingContents)
    {
      #if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_5
        CGContextClearRect (context, cgRect);
      #else
       #if MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_5
        if (CGContextDrawLinearGradient == 0) // (just a way of checking whether we're running in 10.5 or later)
            CGContextClearRect (context, cgRect);
        else
       #endif
            CGContextSetBlendMode (context, kCGBlendModeCopy);
      #endif

        fillCGRect (cgRect, false);
        CGContextSetBlendMode (context, kCGBlendModeNormal);
    }
    else
    {
        if (state->fillType.isColour())
        {
            CGContextFillRect (context, cgRect);
        }
        else if (state->fillType.isGradient())
        {
            CGContextSaveGState (context);
            CGContextClipToRect (context, cgRect);
            drawGradient();
            CGContextRestoreGState (context);
        }
        else
        {
            CGContextSaveGState (context);
            CGContextClipToRect (context, cgRect);
            drawImage (state->fillType.image, state->fillType.transform, true);
            CGContextRestoreGState (context);
        }
    }
}

void CoreGraphicsContext::fillPath (const Path& path, const AffineTransform& transform)
{
    CGContextSaveGState (context);

    if (state->fillType.isColour())
    {
        flip();
        applyTransform (transform);
        createPath (path);

        if (path.isUsingNonZeroWinding())
            CGContextFillPath (context);
        else
            CGContextEOFillPath (context);
    }
    else
    {
        createPath (path, transform);

        if (path.isUsingNonZeroWinding())
            CGContextClip (context);
        else
            CGContextEOClip (context);

        if (state->fillType.isGradient())
            drawGradient();
        else
            drawImage (state->fillType.image, state->fillType.transform, true);
    }

    CGContextRestoreGState (context);
}

void CoreGraphicsContext::drawImage (const Image& sourceImage, const AffineTransform& transform)
{
    drawImage (sourceImage, transform, false);
}

void CoreGraphicsContext::drawImage (const Image& sourceImage, const AffineTransform& transform, const bool fillEntireClipAsTiles)
{
    const int iw = sourceImage.getWidth();
    const int ih = sourceImage.getHeight();
    CGImageRef image = CoreGraphicsImage::getCachedImageRef (sourceImage, rgbColourSpace);

    CGContextSaveGState (context);
    CGContextSetAlpha (context, state->fillType.getOpacity());

    flip();
    applyTransform (AffineTransform::verticalFlip (ih).followedBy (transform));
    CGRect imageRect = CGRectMake (0, 0, iw, ih);

    if (fillEntireClipAsTiles)
    {
      #if JUCE_IOS
        CGContextDrawTiledImage (context, imageRect, image);
      #else
       #if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5
        // There's a bug in CGContextDrawTiledImage that makes it incredibly slow
        // if it's doing a transformation - it's quicker to just draw lots of images manually
        if (CGContextDrawTiledImage != 0 && transform.isOnlyTranslation())
            CGContextDrawTiledImage (context, imageRect, image);
        else
       #endif
        {
            // Fallback to manually doing a tiled fill on 10.4
            CGRect clip = CGRectIntegral (CGContextGetClipBoundingBox (context));

            int x = 0, y = 0;
            while (x > clip.origin.x)   x -= iw;
            while (y > clip.origin.y)   y -= ih;

            const int right = (int) (clip.origin.x + clip.size.width);
            const int bottom = (int) (clip.origin.y + clip.size.height);

            while (y < bottom)
            {
                for (int x2 = x; x2 < right; x2 += iw)
                    CGContextDrawImage (context, CGRectMake (x2, y, iw, ih), image);

                y += ih;
            }
        }
      #endif
    }
    else
    {
        CGContextDrawImage (context, imageRect, image);
    }

    CGImageRelease (image); // (This causes a memory bug in iOS sim 3.0 - try upgrading to a later version if you hit this)
    CGContextRestoreGState (context);
}

//==============================================================================
void CoreGraphicsContext::drawLine (const Line<float>& line)
{
    if (state->fillType.isColour())
    {
        CGContextSetLineCap (context, kCGLineCapSquare);
        CGContextSetLineWidth (context, 1.0f);
        CGContextSetRGBStrokeColor (context,
                                    state->fillType.colour.getFloatRed(), state->fillType.colour.getFloatGreen(),
                                    state->fillType.colour.getFloatBlue(), state->fillType.colour.getFloatAlpha());

        CGPoint cgLine[] = { { (CGFloat) line.getStartX(), flipHeight - (CGFloat) line.getStartY() },
                             { (CGFloat) line.getEndX(),   flipHeight - (CGFloat) line.getEndY()   } };

        CGContextStrokeLineSegments (context, cgLine, 1);
    }
    else
    {
        Path p;
        p.addLineSegment (line, 1.0f);
        fillPath (p, AffineTransform::identity);
    }
}

void CoreGraphicsContext::fillRectList (const RectangleList<float>& list)
{
    HeapBlock<CGRect> rects ((size_t) list.getNumRectangles());

    size_t num = 0;
    for (const Rectangle<float>* r = list.begin(), * const e = list.end(); r != e; ++r)
        rects[num++] = CGRectMake (r->getX(), flipHeight - r->getBottom(), r->getWidth(), r->getHeight());

    if (state->fillType.isColour())
    {
        CGContextFillRects (context, rects, num);
    }
    else if (state->fillType.isGradient())
    {
        CGContextSaveGState (context);
        CGContextClipToRects (context, rects, num);
        drawGradient();
        CGContextRestoreGState (context);
    }
    else
    {
        CGContextSaveGState (context);
        CGContextClipToRects (context, rects, num);
        drawImage (state->fillType.image, state->fillType.transform, true);
        CGContextRestoreGState (context);
    }
}

void CoreGraphicsContext::setFont (const Font& newFont)
{
    if (state->font != newFont)
    {
        state->fontRef = 0;
        state->font = newFont;

        if (OSXTypeface* osxTypeface = dynamic_cast <OSXTypeface*> (state->font.getTypeface()))
        {
            state->fontRef = osxTypeface->fontRef;
            CGContextSetFont (context, state->fontRef);
            CGContextSetFontSize (context, state->font.getHeight() * osxTypeface->fontHeightToPointsFactor);

            state->fontTransform = osxTypeface->renderingTransform;
            state->fontTransform.a *= state->font.getHorizontalScale();
            CGContextSetTextMatrix (context, state->fontTransform);
        }
    }
}

const Font& CoreGraphicsContext::getFont()
{
    return state->font;
}

void CoreGraphicsContext::drawGlyph (int glyphNumber, const AffineTransform& transform)
{
    if (state->fontRef != 0 && state->fillType.isColour())
    {
       #if JUCE_CLANG
        #pragma clang diagnostic push
        #pragma clang diagnostic ignored "-Wdeprecated-declarations"
       #endif

        if (transform.isOnlyTranslation())
        {
            CGContextSetTextMatrix (context, state->fontTransform); // have to set this each time, as it's not saved as part of the state

            CGGlyph g = (CGGlyph) glyphNumber;
            CGContextShowGlyphsAtPoint (context, transform.getTranslationX(),
                                        flipHeight - roundToInt (transform.getTranslationY()), &g, 1);
        }
        else
        {
            CGContextSaveGState (context);
            flip();
            applyTransform (transform);

            CGAffineTransform t = state->fontTransform;
            t.d = -t.d;
            CGContextSetTextMatrix (context, t);

            CGGlyph g = (CGGlyph) glyphNumber;
            CGContextShowGlyphsAtPoint (context, 0, 0, &g, 1);

            CGContextRestoreGState (context);
        }

       #if JUCE_CLANG
        #pragma clang diagnostic pop
       #endif
    }
    else
    {
        Path p;
        Font& f = state->font;
        f.getTypeface()->getOutlineForGlyph (glyphNumber, p);

        fillPath (p, AffineTransform::scale (f.getHeight() * f.getHorizontalScale(), f.getHeight())
                                     .followedBy (transform));
    }
}

bool CoreGraphicsContext::drawTextLayout (const AttributedString& text, const Rectangle<float>& area)
{
   #if JUCE_CORETEXT_AVAILABLE
    CoreTextTypeLayout::drawToCGContext (text, area, context, (float) flipHeight);
    return true;
   #else
    (void) text; (void) area;
    return false;
   #endif
}

CoreGraphicsContext::SavedState::SavedState()
    : font (1.0f), fontRef (0), fontTransform (CGAffineTransformIdentity),
      shading (0), numGradientLookupEntries (0)
{
}

CoreGraphicsContext::SavedState::SavedState (const SavedState& other)
    : fillType (other.fillType), font (other.font), fontRef (other.fontRef),
      fontTransform (other.fontTransform), shading (0),
      gradientLookupTable ((size_t) other.numGradientLookupEntries),
      numGradientLookupEntries (other.numGradientLookupEntries)
{
    memcpy (gradientLookupTable, other.gradientLookupTable, sizeof (PixelARGB) * (size_t) numGradientLookupEntries);
}

CoreGraphicsContext::SavedState::~SavedState()
{
    if (shading != 0)
        CGShadingRelease (shading);
}

void CoreGraphicsContext::SavedState::setFill (const FillType& newFill)
{
    fillType = newFill;

    if (fillType.isGradient() && shading != 0)
    {
        CGShadingRelease (shading);
        shading = 0;
    }
}

CGShadingRef CoreGraphicsContext::SavedState::getShading (CoreGraphicsContext& owner)
{
    if (shading == 0)
    {
        ColourGradient& g = *(fillType.gradient);
        numGradientLookupEntries = g.createLookupTable (fillType.transform, gradientLookupTable) - 1;

        CGFunctionRef function = CGFunctionCreate (this, 1, 0, 4, 0, &(owner.gradientCallbacks));
        CGPoint p1 (convertToCGPoint (g.point1));

        if (g.isRadial)
        {
            shading = CGShadingCreateRadial (owner.rgbColourSpace, p1, 0,
                                             p1, g.point1.getDistanceFrom (g.point2),
                                             function, true, true);
        }
        else
        {
            shading = CGShadingCreateAxial (owner.rgbColourSpace, p1,
                                            convertToCGPoint (g.point2),
                                            function, true, true);
        }

        CGFunctionRelease (function);
    }

    return shading;
}

void CoreGraphicsContext::SavedState::gradientCallback (void* info, const CGFloat* inData, CGFloat* outData)
{
    const SavedState* const s = static_cast <const SavedState*> (info);

    const int index = roundToInt (s->numGradientLookupEntries * inData[0]);
    PixelARGB colour (s->gradientLookupTable [jlimit (0, s->numGradientLookupEntries, index)]);
    colour.unpremultiply();

    outData[0] = colour.getRed()   / 255.0f;
    outData[1] = colour.getGreen() / 255.0f;
    outData[2] = colour.getBlue()  / 255.0f;
    outData[3] = colour.getAlpha() / 255.0f;
}

void CoreGraphicsContext::drawGradient()
{
    flip();
    applyTransform (state->fillType.transform);

    CGContextSetInterpolationQuality (context, kCGInterpolationDefault); // (This is required for 10.4, where there's a crash if
                                                                         // you draw a gradient with high quality interp enabled).
    CGContextSetAlpha (context, state->fillType.getOpacity());
    CGContextDrawShading (context, state->getShading (*this));
}

void CoreGraphicsContext::createPath (const Path& path) const
{
    CGContextBeginPath (context);
    Path::Iterator i (path);

    while (i.next())
    {
        switch (i.elementType)
        {
            case Path::Iterator::startNewSubPath:  CGContextMoveToPoint (context, i.x1, i.y1); break;
            case Path::Iterator::lineTo:           CGContextAddLineToPoint (context, i.x1, i.y1); break;
            case Path::Iterator::quadraticTo:      CGContextAddQuadCurveToPoint (context, i.x1, i.y1, i.x2, i.y2); break;
            case Path::Iterator::cubicTo:          CGContextAddCurveToPoint (context, i.x1, i.y1, i.x2, i.y2, i.x3, i.y3); break;
            case Path::Iterator::closePath:        CGContextClosePath (context); break;
            default:                               jassertfalse; break;
        }
    }
}

void CoreGraphicsContext::createPath (const Path& path, const AffineTransform& transform) const
{
    CGContextBeginPath (context);
    Path::Iterator i (path);

    while (i.next())
    {
        switch (i.elementType)
        {
        case Path::Iterator::startNewSubPath:
            transform.transformPoint (i.x1, i.y1);
            CGContextMoveToPoint (context, i.x1, flipHeight - i.y1);
            break;
        case Path::Iterator::lineTo:
            transform.transformPoint (i.x1, i.y1);
            CGContextAddLineToPoint (context, i.x1, flipHeight - i.y1);
            break;
        case Path::Iterator::quadraticTo:
            transform.transformPoints (i.x1, i.y1, i.x2, i.y2);
            CGContextAddQuadCurveToPoint (context, i.x1, flipHeight - i.y1, i.x2, flipHeight - i.y2);
            break;
        case Path::Iterator::cubicTo:
            transform.transformPoints (i.x1, i.y1, i.x2, i.y2, i.x3, i.y3);
            CGContextAddCurveToPoint (context, i.x1, flipHeight - i.y1, i.x2, flipHeight - i.y2, i.x3, flipHeight - i.y3);
            break;
        case Path::Iterator::closePath:
            CGContextClosePath (context); break;
        default:
            jassertfalse;
            break;
        }
    }
}

void CoreGraphicsContext::flip() const
{
    CGContextConcatCTM (context, CGAffineTransformMake (1, 0, 0, -1, 0, flipHeight));
}

void CoreGraphicsContext::applyTransform (const AffineTransform& transform) const
{
    CGAffineTransform t;
    t.a  = transform.mat00;
    t.b  = transform.mat10;
    t.c  = transform.mat01;
    t.d  = transform.mat11;
    t.tx = transform.mat02;
    t.ty = transform.mat12;
    CGContextConcatCTM (context, t);
}

//==============================================================================
#if USE_COREGRAPHICS_RENDERING && JUCE_USE_COREIMAGE_LOADER
Image juce_loadWithCoreImage (InputStream& input)
{
    MemoryBlock data;
    input.readIntoMemoryBlock (data, -1);

   #if JUCE_IOS
    JUCE_AUTORELEASEPOOL
   #endif
    {
      #if JUCE_IOS
        if (UIImage* uiImage = [UIImage imageWithData: [NSData dataWithBytesNoCopy: data.getData()
                                                                            length: data.getSize()
                                                                      freeWhenDone: NO]])
        {
            CGImageRef loadedImage = uiImage.CGImage;

      #else
        CGDataProviderRef provider = CGDataProviderCreateWithData (0, data.getData(), data.getSize(), 0);
        CGImageSourceRef imageSource = CGImageSourceCreateWithDataProvider (provider, 0);
        CGDataProviderRelease (provider);

        if (imageSource != 0)
        {
            CGImageRef loadedImage = CGImageSourceCreateImageAtIndex (imageSource, 0, 0);
            CFRelease (imageSource);
      #endif

            if (loadedImage != 0)
            {
                CGImageAlphaInfo alphaInfo = CGImageGetAlphaInfo (loadedImage);
                const bool hasAlphaChan = (alphaInfo != kCGImageAlphaNone
                                             && alphaInfo != kCGImageAlphaNoneSkipLast
                                             && alphaInfo != kCGImageAlphaNoneSkipFirst);

                Image image (NativeImageType().create (Image::ARGB, // (CoreImage doesn't work with 24-bit images)
                                                       (int) CGImageGetWidth (loadedImage),
                                                       (int) CGImageGetHeight (loadedImage),
                                                       hasAlphaChan));

                CoreGraphicsImage* const cgImage = dynamic_cast<CoreGraphicsImage*> (image.getPixelData());
                jassert (cgImage != nullptr); // if USE_COREGRAPHICS_RENDERING is set, the CoreGraphicsImage class should have been used.

                CGContextDrawImage (cgImage->context, convertToCGRect (image.getBounds()), loadedImage);
                CGContextFlush (cgImage->context);

               #if ! JUCE_IOS
                CFRelease (loadedImage);
               #endif

                // Because it's impossible to create a truly 24-bit CG image, this flag allows a user
                // to find out whether the file they just loaded the image from had an alpha channel or not.
                image.getProperties()->set ("originalImageHadAlpha", hasAlphaChan);
                return image;
            }
        }
    }

    return Image::null;
}
#endif

#if JUCE_MAC
Image juce_createImageFromCIImage (CIImage*, int, int);
Image juce_createImageFromCIImage (CIImage* im, int w, int h)
{
    CoreGraphicsImage* cgImage = new CoreGraphicsImage (Image::ARGB, w, h, false);

    CIContext* cic = [CIContext contextWithCGContext: cgImage->context options: nil];
    [cic drawImage: im inRect: CGRectMake (0, 0, w, h) fromRect: CGRectMake (0, 0, w, h)];
    CGContextFlush (cgImage->context);

    return Image (cgImage);
}

CGImageRef juce_createCoreGraphicsImage (const Image& juceImage, CGColorSpaceRef colourSpace,
                                         const bool mustOutliveSource)
{
    return CoreGraphicsImage::createImage (juceImage, colourSpace, mustOutliveSource);
}

CGContextRef juce_getImageContext (const Image& image)
{
    if (CoreGraphicsImage* const cgi = dynamic_cast <CoreGraphicsImage*> (image.getPixelData()))
        return cgi->context;

    jassertfalse;
    return 0;
}

#endif
