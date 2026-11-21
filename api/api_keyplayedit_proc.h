
#pragma once

// Keep in sync with the NoteInputSource values in api_keyplayedit.cpp.
enum NoteInputSource {
  NOTEINPUT_KEYBOARD = 0,
  NOTEINPUT_MIDI = 1
};

// Shared step-recording note input. Called from the main thread only.
extern LANGSPEC void NoteInput_note_on(int source,
                                       struct Tracker_Windows *window,
                                       float notenum,
                                       float velocity,
                                       bool polyphonic,
                                       bool is_repeat);

extern LANGSPEC void NoteInput_note_off(int source,
                                        struct Tracker_Windows *window,
                                        float notenum);

extern LANGSPEC void NoteInput_cancel_all(void);
