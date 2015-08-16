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

#ifndef JUCE_TABBEDCOMPONENT_H_INCLUDED
#define JUCE_TABBEDCOMPONENT_H_INCLUDED


//==============================================================================
/**
    A component with a TabbedButtonBar along one of its sides.

    This makes it easy to create a set of tabbed pages, just add a bunch of tabs
    with addTab(), and this will take care of showing the pages for you when the
    user clicks on a different tab.

    @see TabbedButtonBar
*/
class JUCE_API  TabbedComponent  : public Component
{
public:
    //==============================================================================
    /** Creates a TabbedComponent, specifying where the tabs should be placed.
        Once created, add some tabs with the addTab() method.
    */
    explicit TabbedComponent (TabbedButtonBar::Orientation orientation);

    /** Destructor. */
    ~TabbedComponent();

    //==============================================================================
    /** Changes the placement of the tabs.

        This will rearrange the layout to place the tabs along the appropriate
        side of this component, and will shift the content component accordingly.

        @see TabbedButtonBar::setOrientation
    */
    void setOrientation (TabbedButtonBar::Orientation orientation);

    /** Returns the current tab placement.
        @see setOrientation, TabbedButtonBar::getOrientation
    */
    TabbedButtonBar::Orientation getOrientation() const noexcept;

    /** Specifies how many pixels wide or high the tab-bar should be.

        If the tabs are placed along the top or bottom, this specified the height
        of the bar; if they're along the left or right edges, it'll be the width
        of the bar.
    */
    void setTabBarDepth (int newDepth);

    /** Returns the current thickness of the tab bar.
        @see setTabBarDepth
    */
    int getTabBarDepth() const noexcept                         { return tabDepth; }

    /** Specifies the thickness of an outline that should be drawn around the content component.

        If this thickness is > 0, a line will be drawn around the three sides of the content
        component which don't touch the tab-bar, and the content component will be inset by this amount.

        To set the colour of the line, use setColour (outlineColourId, ...).
    */
    void setOutline (int newThickness);

    /** Specifies a gap to leave around the edge of the content component.
        Each edge of the content component will be indented by the given number of pixels.
    */
    void setIndent (int indentThickness);

    //==============================================================================
    /** Removes all the tabs from the bar.
        @see TabbedButtonBar::clearTabs
    */
    void clearTabs();

    /** Adds a tab to the tab-bar.

        The component passed in will be shown for the tab. If deleteComponentWhenNotNeeded
        is true, then the TabbedComponent will take ownership of the component and will delete
        it when the tab is removed or when this object is deleted.

        @see TabbedButtonBar::addTab
    */
    void addTab (const String& tabName,
                 Colour tabBackgroundColour,
                 Component* contentComponent,
                 bool deleteComponentWhenNotNeeded,
                 int insertIndex = -1);

    /** Changes the name of one of the tabs. */
    void setTabName (int tabIndex, const String& newName);

    /** Gets rid of one of the tabs. */
    void removeTab (int tabIndex);

    /** Returns the number of tabs in the bar. */
    int getNumTabs() const;

    /** Returns a list of all the tab names in the bar. */
    StringArray getTabNames() const;

    /** Returns the content component that was added for the given index.
        Be careful not to reposition or delete the components that are returned, as
        this will interfere with the TabbedComponent's behaviour.
    */
    Component* getTabContentComponent (int tabIndex) const noexcept;

    /** Returns the colour of one of the tabs. */
    Colour getTabBackgroundColour (int tabIndex) const noexcept;

    /** Changes the background colour of one of the tabs. */
    void setTabBackgroundColour (int tabIndex, Colour newColour);

    //==============================================================================
    /** Changes the currently-selected tab.
        To deselect all the tabs, pass -1 as the index.
        @see TabbedButtonBar::setCurrentTabIndex
    */
    void setCurrentTabIndex (int newTabIndex, bool sendChangeMessage = true);

    /** Returns the index of the currently selected tab.
        @see addTab, TabbedButtonBar::getCurrentTabIndex()
    */
    int getCurrentTabIndex() const;

    /** Returns the name of the currently selected tab.
        @see addTab, TabbedButtonBar::getCurrentTabName()
    */
    String getCurrentTabName() const;

    /** Returns the current component that's filling the panel.
        This will return nullptr if there isn't one.
    */
    Component* getCurrentContentComponent() const noexcept          { return panelComponent; }

    //==============================================================================
    /** Callback method to indicate the selected tab has been changed.
        @see setCurrentTabIndex
    */
    virtual void currentTabChanged (int newCurrentTabIndex, const String& newCurrentTabName);

    /** Callback method to indicate that the user has right-clicked on a tab. */
    virtual void popupMenuClickOnTab (int tabIndex, const String& tabName);

    /** Returns the tab button bar component that is being used. */
    TabbedButtonBar& getTabbedButtonBar() const noexcept            { return *tabs; }

    //==============================================================================
    /** A set of colour IDs to use to change the colour of various aspects of the component.

        These constants can be used either via the Component::setColour(), or LookAndFeel::setColour()
        methods.

        @see Component::setColour, Component::findColour, LookAndFeel::setColour, LookAndFeel::findColour
    */
    enum ColourIds
    {
        backgroundColourId          = 0x1005800,    /**< The colour to fill the background behind the tabs. */
        outlineColourId             = 0x1005801,    /**< The colour to use to draw an outline around the content.
                                                         (See setOutline)  */
    };

    //==============================================================================
    /** @internal */
    void paint (Graphics&) override;
    /** @internal */
    void resized() override;
    /** @internal */
    void lookAndFeelChanged() override;

protected:
    //==============================================================================
    /** This creates one of the tab buttons.

        If you need to use custom tab components, you can override this method and
        return your own class instead of the default.
    */
    virtual TabBarButton* createTabButton (const String& tabName, int tabIndex);

    /** @internal */
    ScopedPointer<TabbedButtonBar> tabs;

private:
    //==============================================================================
    Array <WeakReference<Component> > contentComponents;
    WeakReference<Component> panelComponent;
    int tabDepth, outlineThickness, edgeIndent;

    class ButtonBar;
    friend class ButtonBar;
    void changeCallback (int newCurrentTabIndex, const String& newTabName);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TabbedComponent)
};


#endif   // JUCE_TABBEDCOMPONENT_H_INCLUDED
