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

class ScrollBar::ScrollbarButton  : public Button
{
public:
    ScrollbarButton (const int direction_, ScrollBar& owner_)
        : Button (String::empty), direction (direction_), owner (owner_)
    {
        setWantsKeyboardFocus (false);
    }

    void paintButton (Graphics& g, bool over, bool down) override
    {
        getLookAndFeel().drawScrollbarButton (g, owner, getWidth(), getHeight(),
                                              direction, owner.isVertical(), over, down);
    }

    void clicked() override
    {
        owner.moveScrollbarInSteps ((direction == 1 || direction == 2) ? 1 : -1);
    }

    int direction;

private:
    ScrollBar& owner;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScrollbarButton)
};


//==============================================================================
ScrollBar::ScrollBar (const bool vertical_)
    : totalRange (0.0, 1.0),
      visibleRange (0.0, 0.1),
      singleStepSize (0.1),
      thumbAreaStart (0),
      thumbAreaSize (0),
      thumbStart (0),
      thumbSize (0),
      initialDelayInMillisecs (100),
      repeatDelayInMillisecs (50),
      minimumDelayInMillisecs (10),
      vertical (vertical_),
      isDraggingThumb (false),
      autohides (true)
{
    setRepaintsOnMouseActivity (true);
    setFocusContainer (true);
}

ScrollBar::~ScrollBar()
{
    upButton = nullptr;
    downButton = nullptr;
}

//==============================================================================
void ScrollBar::setRangeLimits (Range<double> newRangeLimit, NotificationType notification)
{
    if (totalRange != newRangeLimit)
    {
        totalRange = newRangeLimit;
        setCurrentRange (visibleRange, notification);
        updateThumbPosition();
    }
}

void ScrollBar::setRangeLimits (const double newMinimum, const double newMaximum, NotificationType notification)
{
    jassert (newMaximum >= newMinimum); // these can't be the wrong way round!
    setRangeLimits (Range<double> (newMinimum, newMaximum), notification);
}

bool ScrollBar::setCurrentRange (Range<double> newRange,
                                 const NotificationType notification)
{
    const Range<double> constrainedRange (totalRange.constrainRange (newRange));

    if (visibleRange != constrainedRange)
    {
        visibleRange = constrainedRange;

        updateThumbPosition();

        if (notification != dontSendNotification)
            triggerAsyncUpdate();

        if (notification == sendNotificationSync)
            handleUpdateNowIfNeeded();

        return true;
    }

    return false;
}

void ScrollBar::setCurrentRange (const double newStart, const double newSize, NotificationType notification)
{
    setCurrentRange (Range<double> (newStart, newStart + newSize), notification);
}

void ScrollBar::setCurrentRangeStart (const double newStart, NotificationType notification)
{
    setCurrentRange (visibleRange.movedToStartAt (newStart), notification);
}

void ScrollBar::setSingleStepSize (const double newSingleStepSize) noexcept
{
    singleStepSize = newSingleStepSize;
}

bool ScrollBar::moveScrollbarInSteps (const int howManySteps, NotificationType notification)
{
    return setCurrentRange (visibleRange + howManySteps * singleStepSize, notification);
}

bool ScrollBar::moveScrollbarInPages (const int howManyPages, NotificationType notification)
{
    return setCurrentRange (visibleRange + howManyPages * visibleRange.getLength(), notification);
}

bool ScrollBar::scrollToTop (NotificationType notification)
{
    return setCurrentRange (visibleRange.movedToStartAt (getMinimumRangeLimit()), notification);
}

bool ScrollBar::scrollToBottom (NotificationType notification)
{
    return setCurrentRange (visibleRange.movedToEndAt (getMaximumRangeLimit()), notification);
}

void ScrollBar::setButtonRepeatSpeed (const int initialDelayInMillisecs_,
                                      const int repeatDelayInMillisecs_,
                                      const int minimumDelayInMillisecs_)
{
    initialDelayInMillisecs = initialDelayInMillisecs_;
    repeatDelayInMillisecs  = repeatDelayInMillisecs_;
    minimumDelayInMillisecs = minimumDelayInMillisecs_;

    if (upButton != nullptr)
    {
        upButton  ->setRepeatSpeed (initialDelayInMillisecs, repeatDelayInMillisecs, minimumDelayInMillisecs);
        downButton->setRepeatSpeed (initialDelayInMillisecs, repeatDelayInMillisecs, minimumDelayInMillisecs);
    }
}

//==============================================================================
void ScrollBar::addListener (Listener* const listener)
{
    listeners.add (listener);
}

void ScrollBar::removeListener (Listener* const listener)
{
    listeners.remove (listener);
}

void ScrollBar::handleAsyncUpdate()
{
    double start = visibleRange.getStart(); // (need to use a temp variable for VC7 compatibility)
    listeners.call (&ScrollBar::Listener::scrollBarMoved, this, start);
}

//==============================================================================
void ScrollBar::updateThumbPosition()
{
    int newThumbSize = roundToInt (totalRange.getLength() > 0 ? (visibleRange.getLength() * thumbAreaSize) / totalRange.getLength()
                                                              : thumbAreaSize);

    LookAndFeel& lf = getLookAndFeel();

    if (newThumbSize < lf.getMinimumScrollbarThumbSize (*this))
        newThumbSize = jmin (lf.getMinimumScrollbarThumbSize (*this), thumbAreaSize - 1);

    if (newThumbSize > thumbAreaSize)
        newThumbSize = thumbAreaSize;

    int newThumbStart = thumbAreaStart;

    if (totalRange.getLength() > visibleRange.getLength())
        newThumbStart += roundToInt (((visibleRange.getStart() - totalRange.getStart()) * (thumbAreaSize - newThumbSize))
                                         / (totalRange.getLength() - visibleRange.getLength()));

    setVisible ((! autohides) || (totalRange.getLength() > visibleRange.getLength() && visibleRange.getLength() > 0.0));

    if (thumbStart != newThumbStart  || thumbSize != newThumbSize)
    {
        const int repaintStart = jmin (thumbStart, newThumbStart) - 4;
        const int repaintSize = jmax (thumbStart + thumbSize, newThumbStart + newThumbSize) + 8 - repaintStart;

        if (vertical)
            repaint (0, repaintStart, getWidth(), repaintSize);
        else
            repaint (repaintStart, 0, repaintSize, getHeight());

        thumbStart = newThumbStart;
        thumbSize = newThumbSize;
    }
}

void ScrollBar::setOrientation (const bool shouldBeVertical)
{
    if (vertical != shouldBeVertical)
    {
        vertical = shouldBeVertical;

        if (upButton != nullptr)
        {
            upButton->direction    = vertical ? 0 : 3;
            downButton->direction  = vertical ? 2 : 1;
        }

        updateThumbPosition();
    }
}

void ScrollBar::setAutoHide (const bool shouldHideWhenFullRange)
{
    autohides = shouldHideWhenFullRange;
    updateThumbPosition();
}

bool ScrollBar::autoHides() const noexcept
{
    return autohides;
}

//==============================================================================
void ScrollBar::paint (Graphics& g)
{
    if (thumbAreaSize > 0)
    {
        LookAndFeel& lf = getLookAndFeel();

        const int thumb = (thumbAreaSize > lf.getMinimumScrollbarThumbSize (*this))
                             ? thumbSize : 0;

        if (vertical)
            lf.drawScrollbar (g, *this, 0, thumbAreaStart, getWidth(), thumbAreaSize,
                              vertical, thumbStart, thumb, isMouseOver(), isMouseButtonDown());
        else
            lf.drawScrollbar (g, *this, thumbAreaStart, 0, thumbAreaSize, getHeight(),
                              vertical, thumbStart, thumb, isMouseOver(), isMouseButtonDown());
    }
}

void ScrollBar::lookAndFeelChanged()
{
    setComponentEffect (getLookAndFeel().getScrollbarEffect());

    if (isVisible())
        resized();
}

void ScrollBar::resized()
{
    const int length = vertical ? getHeight() : getWidth();

    LookAndFeel& lf = getLookAndFeel();
    const bool buttonsVisible = lf.areScrollbarButtonsVisible();
    int buttonSize = 0;

    if (buttonsVisible)
    {
        if (upButton == nullptr)
        {
            addAndMakeVisible (upButton   = new ScrollbarButton (vertical ? 0 : 3, *this));
            addAndMakeVisible (downButton = new ScrollbarButton (vertical ? 2 : 1, *this));

            setButtonRepeatSpeed (initialDelayInMillisecs, repeatDelayInMillisecs, minimumDelayInMillisecs);
        }

        buttonSize = jmin (lf.getScrollbarButtonSize (*this), length / 2);
    }
    else
    {
        upButton = nullptr;
        downButton = nullptr;
    }

    if (length < 32 + lf.getMinimumScrollbarThumbSize (*this))
    {
        thumbAreaStart = length / 2;
        thumbAreaSize = 0;
    }
    else
    {
        thumbAreaStart = buttonSize;
        thumbAreaSize = length - (buttonSize << 1);
    }

    if (upButton != nullptr)
    {
        if (vertical)
        {
            upButton->setBounds (0, 0, getWidth(), buttonSize);
            downButton->setBounds (0, thumbAreaStart + thumbAreaSize, getWidth(), buttonSize);
        }
        else
        {
            upButton->setBounds (0, 0, buttonSize, getHeight());
            downButton->setBounds (thumbAreaStart + thumbAreaSize, 0, buttonSize, getHeight());
        }
    }

    updateThumbPosition();
}

void ScrollBar::mouseDown (const MouseEvent& e)
{
    isDraggingThumb = false;
    lastMousePos = vertical ? e.y : e.x;
    dragStartMousePos = lastMousePos;
    dragStartRange = visibleRange.getStart();

    if (dragStartMousePos < thumbStart)
    {
        moveScrollbarInPages (-1);
        startTimer (400);
    }
    else if (dragStartMousePos >= thumbStart + thumbSize)
    {
        moveScrollbarInPages (1);
        startTimer (400);
    }
    else
    {
        isDraggingThumb = (thumbAreaSize > getLookAndFeel().getMinimumScrollbarThumbSize (*this))
                            && (thumbAreaSize > thumbSize);
    }
}

void ScrollBar::mouseDrag (const MouseEvent& e)
{
    const int mousePos = vertical ? e.y : e.x;

    if (isDraggingThumb && lastMousePos != mousePos && thumbAreaSize > thumbSize)
    {
        const int deltaPixels = mousePos - dragStartMousePos;

        setCurrentRangeStart (dragStartRange
                                + deltaPixels * (totalRange.getLength() - visibleRange.getLength())
                                    / (thumbAreaSize - thumbSize));
    }

    lastMousePos = mousePos;
}

void ScrollBar::mouseUp (const MouseEvent&)
{
    isDraggingThumb = false;
    stopTimer();
    repaint();
}

void ScrollBar::mouseWheelMove (const MouseEvent&, const MouseWheelDetails& wheel)
{
    float increment = 10.0f * (vertical ? wheel.deltaY : wheel.deltaX);

    if (increment < 0)
        increment = jmin (increment, -1.0f);
    else if (increment > 0)
        increment = jmax (increment, 1.0f);

    setCurrentRange (visibleRange - singleStepSize * increment);
}

void ScrollBar::timerCallback()
{
    if (isMouseButtonDown())
    {
        startTimer (40);

        if (lastMousePos < thumbStart)
            setCurrentRange (visibleRange - visibleRange.getLength());
        else if (lastMousePos > thumbStart + thumbSize)
            setCurrentRangeStart (visibleRange.getEnd());
    }
    else
    {
        stopTimer();
    }
}

bool ScrollBar::keyPressed (const KeyPress& key)
{
    if (isVisible())
    {
        if (key == KeyPress::upKey || key == KeyPress::leftKey)    return moveScrollbarInSteps (-1);
        if (key == KeyPress::downKey || key == KeyPress::rightKey) return moveScrollbarInSteps (1);
        if (key == KeyPress::pageUpKey)                            return moveScrollbarInPages (-1);
        if (key == KeyPress::pageDownKey)                          return moveScrollbarInPages (1);
        if (key == KeyPress::homeKey)                              return scrollToTop();
        if (key == KeyPress::endKey)                               return scrollToBottom();
    }

    return false;
}
