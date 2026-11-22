#pragma once

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Called from the MIDI input thread. Only fader (pitch bend) messages are
// currently queued. byte1/byte2/byte3 are the raw MIDI bytes.
extern void MCU_InputMessageHasBeenReceived(const char *port_name, int byte1, int byte2, int byte3);

// Main thread.
extern void MCU_add_port(const char *port_name);
extern void MCU_remove_port(const char *port_name);
extern void MCU_call_very_often(void);

// Called when keyboard focus changes.
extern void MCU_set_sequencer_has_focus(bool has_focus);
extern void MCU_set_editor_has_focus(bool has_focus);

// Global MCU settings.
extern bool MCU_jog_moves_by_bars(void);
extern void MCU_set_jog_moves_by_bars(bool by_bars);
extern bool MCU_display_bars_and_beats(void);
extern void MCU_set_display_bars_and_beats(bool show_bars_and_beats);

#ifdef __cplusplus
}
#endif
