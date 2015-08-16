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

#ifndef JUCE_ATTRIBUTEDSTRING_H_INCLUDED
#define JUCE_ATTRIBUTEDSTRING_H_INCLUDED


//==============================================================================
/**
    A text string with a set of colour/font settings that are associated with sub-ranges
    of the text.

    An attributed string lets you create a string with varied fonts, colours, word-wrapping,
    layout, etc., and draw it using AttributedString::draw().

    @see TextLayout
*/
class JUCE_API  AttributedString
{
public:
    /** Creates an empty attributed string. */
    AttributedString();

    /** Creates an attributed string with the given text. */
    explicit AttributedString (const String& text);

    AttributedString (const AttributedString&);
    AttributedString& operator= (const AttributedString&);
   #if JUCE_COMPILER_SUPPORTS_MOVE_SEMANTICS
    AttributedString (AttributedString&&) noexcept;
    AttributedString& operator= (AttributedString&&) noexcept;
   #endif

    /** Destructor. */
    ~AttributedString();

    //==============================================================================
    /** Returns the complete text of this attributed string. */
    const String& getText() const noexcept                  { return text; }

    /** Replaces all the text.
        This will change the text, but won't affect any of the colour or font attributes
        that have been added.
    */
    void setText (const String& newText);

    /** Appends some text (with a default font and colour). */
    void append (const String& textToAppend);
    /** Appends some text, with a specified font, and the default colour (black). */
    void append (const String& textToAppend, const Font& font);
    /** Appends some text, with a specified colour, and the default font. */
    void append (const String& textToAppend, Colour colour);
    /** Appends some text, with a specified font and colour. */
    void append (const String& textToAppend, const Font& font, Colour colour);

    /** Appends another AttributedString to this one.
        Note that this will only append the text, fonts, and colours - it won't copy any
        other properties such as justification, line-spacing, etc from the other object.
    */
    void append (const AttributedString& other);

    /** Resets the string, clearing all text and attributes.
        Note that this won't affect global settings like the justification type,
        word-wrap mode, etc.
    */
    void clear();

    //==============================================================================
    /** Draws this string within the given area.
        The layout of the string within the rectangle is controlled by the justification
        value passed to setJustification().
    */
    void draw (Graphics& g, const Rectangle<float>& area) const;

    //==============================================================================
    /** Returns the justification that should be used for laying-out the text.
        This may include both vertical and horizontal flags.
    */
    Justification getJustification() const noexcept         { return justification; }

    /** Sets the justification that should be used for laying-out the text.
        This may include both vertical and horizontal flags.
    */
    void setJustification (Justification newJustification) noexcept;

    //==============================================================================
    /** Types of word-wrap behaviour.
        @see getWordWrap, setWordWrap
    */
    enum WordWrap
    {
        none,   /**< No word-wrapping: lines extend indefinitely. */
        byWord, /**< Lines are wrapped on a word boundary. */
        byChar, /**< Lines are wrapped on a character boundary. */
    };

    /** Returns the word-wrapping behaviour. */
    WordWrap getWordWrap() const noexcept                   { return wordWrap; }

    /** Sets the word-wrapping behaviour. */
    void setWordWrap (WordWrap newWordWrap) noexcept;

    //==============================================================================
    /** Types of reading direction that can be used.
        @see getReadingDirection, setReadingDirection
    */
    enum ReadingDirection
    {
        natural,
        leftToRight,
        rightToLeft,
    };

    /** Returns the reading direction for the text. */
    ReadingDirection getReadingDirection() const noexcept   { return readingDirection; }

    /** Sets the reading direction that should be used for the text. */
    void setReadingDirection (ReadingDirection newReadingDirection) noexcept;

    //==============================================================================
    /** Returns the extra line-spacing distance. */
    float getLineSpacing() const noexcept                   { return lineSpacing; }

    /** Sets an extra line-spacing distance. */
    void setLineSpacing (float newLineSpacing) noexcept;

    //==============================================================================
    /** An attribute that has been applied to a range of characters in an AttributedString. */
    class JUCE_API  Attribute
    {
    public:
        /** Creates an attribute that changes the colour for a range of characters.
            @see AttributedString::setColour()
        */
        Attribute (Range<int> range, Colour colour);

        /** Creates an attribute that changes the font for a range of characters.
            @see AttributedString::setFont()
        */
        Attribute (Range<int> range, const Font& font);

        Attribute (const Attribute&);
        ~Attribute();

        /** If this attribute specifies a font, this returns it; otherwise it returns nullptr. */
        const Font* getFont() const noexcept            { return font; }

        /** If this attribute specifies a colour, this returns it; otherwise it returns nullptr. */
        const Colour* getColour() const noexcept        { return colour; }

        /** The range of characters to which this attribute will be applied. */
        const Range<int> range;

    private:
        ScopedPointer<Font> font;
        ScopedPointer<Colour> colour;

        friend class AttributedString;
        Attribute (const Attribute&, int);
        Attribute& operator= (const Attribute&);

        JUCE_LEAK_DETECTOR (Attribute)
    };

    /** Returns the number of attributes that have been added to this string. */
    int getNumAttributes() const noexcept                       { return attributes.size(); }

    /** Returns one of the string's attributes.
        The index provided must be less than getNumAttributes(), and >= 0.
    */
    const Attribute* getAttribute (int index) const noexcept    { return attributes.getUnchecked (index); }

    //==============================================================================
    /** Adds a colour attribute for the specified range. */
    void setColour (Range<int> range, Colour colour);

    /** Removes all existing colour attributes, and applies this colour to the whole string. */
    void setColour (Colour colour);

    /** Adds a font attribute for the specified range. */
    void setFont (Range<int> range, const Font& font);

    /** Removes all existing font attributes, and applies this font to the whole string. */
    void setFont (const Font& font);

private:
    String text;
    float lineSpacing;
    Justification justification;
    WordWrap wordWrap;
    ReadingDirection readingDirection;
    OwnedArray<Attribute> attributes;

    JUCE_LEAK_DETECTOR (AttributedString)
};

#endif   // JUCE_ATTRIBUTEDSTRING_H_INCLUDED
