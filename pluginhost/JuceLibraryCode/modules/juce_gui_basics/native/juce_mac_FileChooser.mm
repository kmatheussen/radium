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

#if JUCE_MAC

struct FileChooserDelegateClass  : public ObjCClass <NSObject>
{
    FileChooserDelegateClass()  : ObjCClass <NSObject> ("JUCEFileChooser_")
    {
        addIvar<StringArray*> ("filters");

        addMethod (@selector (dealloc),                   dealloc,            "v@:");
        addMethod (@selector (panel:shouldShowFilename:), shouldShowFilename, "c@:@@");

       #if defined (MAC_OS_X_VERSION_10_6) && MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6
        addProtocol (@protocol (NSOpenSavePanelDelegate));
       #endif

        registerClass();
    }

    static void setFilters (id self, StringArray* filters)
    {
        object_setInstanceVariable (self, "filters", filters);
    }

private:
    static void dealloc (id self, SEL)
    {
        delete getIvar<StringArray*> (self, "filters");
        sendSuperclassMessage (self, @selector (dealloc));
    }

    static BOOL shouldShowFilename (id self, SEL, id /*sender*/, NSString* filename)
    {
        StringArray* const filters = getIvar<StringArray*> (self, "filters");

        const File f (nsStringToJuce (filename));

        for (int i = filters->size(); --i >= 0;)
            if (f.getFileName().matchesWildcard ((*filters)[i], true))
                return true;

       #if (! defined (MAC_OS_X_VERSION_10_7)) || MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_7
        NSError* error;
        NSString* name = [[NSWorkspace sharedWorkspace] typeOfFile: filename error: &error];

        if ([name isEqualToString: nsStringLiteral ("com.apple.alias-file")])
        {
            FSRef ref;
            FSPathMakeRef ((const UInt8*) [filename fileSystemRepresentation], &ref, nullptr);

            Boolean targetIsFolder = false, wasAliased = false;
            FSResolveAliasFileWithMountFlags (&ref, true, &targetIsFolder, &wasAliased, 0);

            return wasAliased && targetIsFolder;
        }
       #endif

        return f.isDirectory()
                 && ! [[NSWorkspace sharedWorkspace] isFilePackageAtPath: filename];
    }
};

static NSMutableArray* createAllowedTypesArray (const StringArray& filters)
{
    if (filters.size() == 0)
        return nil;

    NSMutableArray* filterArray = [[[NSMutableArray alloc] init] autorelease];

    for (int i = 0; i < filters.size(); ++i)
    {
        const String f (filters[i].replace ("*.", ""));

        if (f == "*")
            return nil;

        [filterArray addObject: juceStringToNS (f)];
    }

    return filterArray;
}

//==============================================================================
void FileChooser::showPlatformDialog (Array<File>& results,
                                      const String& title,
                                      const File& currentFileOrDirectory,
                                      const String& filter,
                                      bool selectsDirectory,
                                      bool selectsFiles,
                                      bool isSaveDialogue,
                                      bool /*warnAboutOverwritingExistingFiles*/,
                                      bool selectMultipleFiles,
                                      FilePreviewComponent* /*extraInfoComponent*/)
{
    JUCE_AUTORELEASEPOOL
    {
        ScopedPointer<TemporaryMainMenuWithStandardCommands> tempMenu;
        if (JUCEApplicationBase::isStandaloneApp())
            tempMenu = new TemporaryMainMenuWithStandardCommands();

        StringArray* filters = new StringArray();
        filters->addTokens (filter.replaceCharacters (",:", ";;"), ";", String::empty);
        filters->trim();
        filters->removeEmptyStrings();

       #if defined (MAC_OS_X_VERSION_10_6) && MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6
        typedef NSObject<NSOpenSavePanelDelegate> DelegateType;
       #else
        typedef NSObject DelegateType;
       #endif

        static FileChooserDelegateClass cls;
        DelegateType* delegate = (DelegateType*) [[cls.createInstance() init] autorelease];
        FileChooserDelegateClass::setFilters (delegate, filters);

        NSSavePanel* panel = isSaveDialogue ? [NSSavePanel savePanel]
                                            : [NSOpenPanel openPanel];

        [panel setTitle: juceStringToNS (title)];
        [panel setAllowedFileTypes: createAllowedTypesArray (*filters)];

        if (! isSaveDialogue)
        {
            NSOpenPanel* openPanel = (NSOpenPanel*) panel;
            [openPanel setCanChooseDirectories: selectsDirectory];
            [openPanel setCanChooseFiles: selectsFiles];
            [openPanel setAllowsMultipleSelection: selectMultipleFiles];
            [openPanel setResolvesAliases: YES];
        }

        [panel setDelegate: delegate];

        if (isSaveDialogue || selectsDirectory)
            [panel setCanCreateDirectories: YES];

        String directory, filename;

        if (currentFileOrDirectory.isDirectory())
        {
            directory = currentFileOrDirectory.getFullPathName();
        }
        else
        {
            directory = currentFileOrDirectory.getParentDirectory().getFullPathName();
            filename = currentFileOrDirectory.getFileName();
        }

       #if defined (MAC_OS_X_VERSION_10_6) && (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_6)
        [panel setDirectoryURL: [NSURL fileURLWithPath: juceStringToNS (directory)]];
        [panel setNameFieldStringValue: juceStringToNS (filename)];

        if ([panel runModal] == 1 /*NSModalResponseOK*/)
       #else
        if ([panel runModalForDirectory: juceStringToNS (directory)
                                   file: juceStringToNS (filename)] == 1 /*NSModalResponseOK*/)
       #endif
        {
            if (isSaveDialogue)
            {
                results.add (File (nsStringToJuce ([[panel URL] path])));
            }
            else
            {
                NSOpenPanel* openPanel = (NSOpenPanel*) panel;
                NSArray* urls = [openPanel URLs];

                for (unsigned int i = 0; i < [urls count]; ++i)
                    results.add (File (nsStringToJuce ([[urls objectAtIndex: i] path])));
            }
        }

        [panel setDelegate: nil];
    }
}

bool FileChooser::isPlatformDialogAvailable()
{
   #if JUCE_DISABLE_NATIVE_FILECHOOSERS
    return false;
   #else
    return true;
   #endif
}

#else

//==============================================================================
bool FileChooser::isPlatformDialogAvailable()
{
    return false;
}

void FileChooser::showPlatformDialog (Array<File>&,
                                      const String& /*title*/,
                                      const File& /*currentFileOrDirectory*/,
                                      const String& /*filter*/,
                                      bool /*selectsDirectory*/,
                                      bool /*selectsFiles*/,
                                      bool /*isSaveDialogue*/,
                                      bool /*warnAboutOverwritingExistingFiles*/,
                                      bool /*selectMultipleFiles*/,
                                      FilePreviewComponent*)
{
    jassertfalse; //there's no such thing in iOS
}

#endif
