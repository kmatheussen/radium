/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include <math.h>

#include <qwidget.h>
#include <qcolor.h>
#include <qapplication.h>
#include <qpalette.h>
#include <qcombobox.h>

#include "qcolordialog.h"
#include <qtimer.h>
#include <qfile.h>

#include "../common/nsmtracker.h"
#include "FocusSniffers.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <faust/gui/QTUI.h>
#pragma GCC diagnostic pop

#pragma clang diagnostic pop

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#ifdef USE_QT3
#include <qobjectlist.h>
#endif

#include "EditorWidget.h"
#include "Qt_mix_colors.h"
#include "Qt_instruments_proc.h"

#include "../common/settings_proc.h"
#include "../common/windows_proc.h"
#include "../GTK/GTK_visual_proc.h"
#include "../common/OS_settings_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"


#include "Qt_colors_proc.h"



namespace{
  struct ColorConfig{
    enum ColorNums num;
    const char *config_name;
    const char *display_name;
    bool is_separator;
  };
  struct ReplacementColorNum{
    enum ColorNums num;
    enum ColorNums replacement_num;
  };
  struct ReplacementColor{
    enum ColorNums num;
    QColor color;
  };
}

static const ColorConfig g_colorconfig[] = {
  {GUI_SEPARATOR, "", "General", true},
  {BUTTONS_SEPARATOR, "", "Buttons", true},
  {SCROLLBAR_SEPARATOR, "", "Scrollbars", true},
  {SLIDER_SEPARATOR, "", "Sliders", true},
  {TABS_SEPARATOR, "", "Tabs", true},
  {AUDIO_SEPARATOR, "", "Audio", true},
  {EDITOR_SEPARATOR, "", "Editor", true},
  {AUTOMATION_SEPARATOR, "", "Automation", true},
  {INSTRUMENT_SEPARATOR, "", "Instruments", true},
  {MIXER_SEPARATOR, "", "Mixer", true},
  {SEQUENCER_SEPARATOR, "", "Sequencer", true},
  //{_SEPARATOR, "", "", true},
  //{_SEPARATOR, "", "", true},
  
  {LOW_EDITOR_BACKGROUND_COLOR_NUM, "low_editor_background",  "Low Editor background", false},
  {TEXT_COLOR_NUM,      "color1",  "Text", false},
  {WAVEFORM_COLOR_NUM,                   "color2",  "Waveform", false},
  {AUTOMATION1_COLOR_NUM,                   "color3",  "Automation 1", false},
  {AUTOMATION2_COLOR_NUM,                   "color4",  "Automation 2", false},
  {VELOCITY1_COLOR_NUM,                   "color5",  "Note background color 1", false},
  {VELOCITY2_COLOR_NUM,                   "color6",  "Note background color 2", false},
  {CURSOR_EDIT_ON_COLOR_NUM,                   "color7",  "Cursor, edit ON", false},
  {INSTRUMENT_NAME_COLOR_NUM,                   "color8",  "Instrument name", false},
  {LOW_BACKGROUND_COLOR_NUM,                   "low_background",  "Low Background", false},
  {AUTOMATION3_COLOR_NUM,                  "color10", "Automation 3", false},
  {HIGH_BACKGROUND_COLOR_NUM,                  "high_background", "High Background", false},
  {MENU_TEXT_COLOR_NUM,                  "menu_text", "Menu text", false},
  {MENU_KEYBINDING_TEXT_COLOR_NUM,                  "menu_keybinding_text", "Menu keybinding text", false},
  {EDITOR_SLIDERS_COLOR_NUM,                  "color12", "Editor sliders", false},
  {TRACK_SLIDER_COLOR_NUM,     "track_slider", "Track slider (bottom of editor)", false},
  {LINE_SLIDER_COLOR_NUM,     "line_slider", "Line slider (left of editor)", false},
  
  {BUTTONS_COLOR_NUM,                  "button_v2", "Buttons", false},
  {BUTTONS_PRESSED_COLOR_NUM,                  "button_pressed_v2", "Buttons pressed", false},
  //{BUTTONS_NOT_ENABLED_COLOR_NUM,                  "button_not_enabled", "Buttons not enabled", false},
  {CHECK_BOX_SELECTED_COLOR_NUM,  "check_box_selected_v2",   "Selected check box", false},
  {CHECK_BOX_UNSELECTED_COLOR_NUM,  "check_box_unselected_v2",   "Unselected check box", false},
  //{CHECK_BOX_NOT_ENABLED_COLOR_NUM,  "check_box_not_enabled",   "Check box not enabled", false},

  {BUTTONS_TEXT_COLOR_NUM,                  "buttons_text", "Button Text", false},

  {PORTAMENTO_NOTE_TEXT_COLOR_NUM,                  "color14", "Portamento note text", false},
  {PORTAMENTO_END_NOTE_TEXT_COLOR_NUM,                  "portamento_end_note_text", "Portamento end note text", false},
  {VELOCITY_TEXT_COLOR_NUM,     "velocity_text", "Velocity text", false},
   
  {HIGH_EDITOR_BACKGROUND_COLOR_NUM,                  "high_editor", "High Editor background", false},
  {SCROLLBAR_COLOR_NUM,                  "scroll_bar_v2", "Scroll bar", false},
  {SCROLLBAR_BACKGROUND_COLOR_NUM,                  "scroll_bar_background_v2", "Scroll bar background", false},

  {KEYBOARD_FOCUS_BORDER_COLOR_NUM,                  "keyboard_focus_border", "Keyboard Focus Border", false},

  {SOUNDFONT_COLOR_NUM,         "soundfont",          "Browser: Soundfont", false},
  {SOUNDFILE_COLOR_NUM,         "soundfile",          "Browser: Sound file", false},
  {CURRENT_SOUNDFILE_COLOR_NUM, "current_soundfile",  "Current sound file", false},
      
  {SLIDER1_COLOR_NUM,           "slider1",            "Slider 1", false},
  {SLIDER2_COLOR_NUM,           "slider2",            "Slider 2", false},
  {SLIDER_DISABLED_COLOR_NUM,   "slider_disabled",    "Slider disabled", false},
  {SLIDER_TEXT_COLOR_NUM,       "slider_text",        "Slider Text", false},
  {SLIDER_RECORDING_COLOR_NUM,  "slider_recording",   "Slider recording", false},

  {TAB_SELECTED_COLOR_NUM,  "tab_selected",   "Selected tab", false},
  {TAB_UNSELECTED_COLOR_NUM,  "tab_unselected",   "Unselected tab", false},

  {PEAKS_COLOR_NUM,             "peaks",                    "Peaks < 0dB", false},
  {PEAKS_0DB_COLOR_NUM,         "peaks0db",                 "Peaks 0dB - 4dB", false},
  {PEAKS_4DB_COLOR_NUM,         "peaks4db",                 "Peaks > 4dB", false},

  {PIANONOTE_COLOR_NUM,         "pianonote",                "Pianoroll note", false},
  {PIANONOTE_SELECTED_COLOR_NUM,         "pianonote_selected",                "Pianoroll selected note", false},

  {AUTOMATION4_COLOR_NUM,                   "automation4",  "Automation 4", false},
  {AUTOMATION5_COLOR_NUM,                   "automation5",  "Automation 5", false},
  {AUTOMATION6_COLOR_NUM,                   "automation6",  "Automation 6", false},
  {AUTOMATION7_COLOR_NUM,                   "automation7",  "Automation 7", false},
  {AUTOMATION8_COLOR_NUM,                   "automation8",  "Automation 8", false},  
  {AUTOMATION_INDICATOR_COLOR_NUM,          "automation_indicator", "Automation indicator", false},

  {TRACK_SEPARATOR1_COLOR_NUM,              "track_separator1", "Track separator 1", false},
  {TRACK_SEPARATOR2A_COLOR_NUM,             "track_separator2_darkened", "Track separator 2 left part", false},
  {TRACK_SEPARATOR2B_COLOR_NUM,             "track_separator2",  "Track separator 2 right part", false},

  {BAR_TEXT_COLOR_NUM,                      "bar_text", "Bar text", false},
  {ZOOMLINE_TEXT_COLOR_NUM1, "zoomline_text1", "Zoom line 1", false},
  {ZOOMLINE_TEXT_COLOR_NUM2, "zoomline_text2", "Zoom line 2", false},
  {ZOOMLINE_TEXT_COLOR_NUM3, "zoomline_text3", "Zoom line 3", false},
  {ZOOMLINE_TEXT_COLOR_NUM4, "zoomline_text4", "Zoom line 4", false},
  {ZOOMLINE_TEXT_COLOR_NUM5, "zoomline_text5", "Zoom line 5", false},
  {ZOOMLINE_TEXT_COLOR_NUM6, "zoomline_text6", "Zoom line 6", false},
  {ZOOMLINE_TEXT_COLOR_NUM7, "zoomline_text7", "Zoom line 7", false},

  {TEMPOGRAPH_COLOR_NUM, "tempograph", "Tempo graph", false},
  {RANGE_COLOR_NUM, "range", "Range", false},
  {PITCH_LINE_COLOR_NUM, "pitchlines", "Pitch change lines", false},

  {PIANOROLL_OCTAVE_COLOR_NUM, "pianoroll_octave", "Pianoroll octave", false},
  {PIANOROLL_NOTE_NAME_COLOR_NUM, "pianoroll_note_name", "Pianoroll note names", false},
  {PIANOROLL_NOTE_BORDER_COLOR_NUM, "pianoroll_note_border", "Pianoroll note border", false},
  
  {CURSOR_EDIT_OFF_COLOR_NUM,     "cursor_edit_off",  "Cursor, edit OFF", false},
  {CURSOR_BORDER_COLOR_NUM,     "cursor_border",  "Cursor border current column border", false},
  {CURSOR_CURR_COLUMN_BORDER_COLOR_NUM,     "cursor_curr_column_border",  "Cursor, current column border", false},
  {PLAY_CURSOR_COLOR_NUM,     "play_cursor_edit_off",  "Play cursor", false},

  {NOTE_EVENT_INDICATOR_COLOR_NUM, "note_event_indicator",  "Note event indicator", false},
  {NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM, "note_event_indicator_border",  "Note event indicator border", false},

  {MIXER_TEXT_COLOR_NUM, "mixer_text_color", "Modular Mixer: Object text", false},
  {MIXER_BORDER_COLOR_NUM, "mixer_border_color", "Modular Mixer: Object border", false},
  
  {MIXER_EVENT_CONNECTION_COLOR_NUM, "mixer_event_connection_color", "Modular Mixer: Event connection", false},
  {MIXER_AUDIO_CONNECTION_COLOR_NUM, "mixer_audio_connection_color", "Modular Mixer: Audio connection", false},
  {MIXER_AUTOSUSPENSION_COLOR_NUM, "mixer_autosuspension_color", "Modular Mixer: Auto-suspension mixer object", false},
  //{MIXER_SELECTED_OBJECT_COLOR_NUM, "mixer_selected_object_color_num", "Mixer: Selected sound object", false},
  {MIXER_CURRENT_OBJECT_BORDER_COLOR_NUM, "mixer_current_object_border_color_num", "Modular Mixer: Current object border", false},
  {MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM, "mixer_selected_object_border_color_num", "Modular Mixer: Selected object border", false},
  {MIXERSTRIPS_CURRENT_INSTRUMENT_BORDER_COLOR_NUM, "mixerstrips_selected_object_color_num", "Mixer Strips: Current instrument border", false},
  
  {INSTRUMENT_BUS_DEFAULT_COLOR_NUM, "default_bus_color", "Default Bus", false},
  {SEQTRACK_INSTRUMENT_DEFAULT_COLOR_NUM, "default_seqtrack_instrument_color", "Default Seqtrack instrument", false},

  {SEQUENCER_CURRTRACK_BORDER_COLOR_NUM, "sequencer_currtrack_border_color", "Sequencer current track border", false},
  {SEQUENCER_CURR_SEQBLOCK_BORDER_COLOR_NUM, "sequencer_curr_seqblock_border_color", "Sequencer current seqblock border", false},
  {SEQUENCER_LANES_BACKGROUND_COLOR_NUM, "sequencer_background_color", "Sequencer lanes background", false},
  {SEQTRACKS_BACKGROUND_COLOR_NUM, "seqtracks_background_color", "Seqtracks background", false},
  {SEQUENCER_BORDER_COLOR_NUM, "sequencer_border_color", "Sequencer border", false},
  {SEQUENCER_TEXT_COLOR_NUM, "sequencer_text_color", "Sequencer text", false},
  {SEQUENCER_TEXT_CURRENT_BLOCK_COLOR_NUM, "sequencer_text_current_block_color", "Sequencer text color, current block", false},
  {SEQUENCER_NOTE_COLOR_NUM, "sequencer_note_color", "Sequencer note", false},
  {SEQUENCER_NOTE_START_COLOR_NUM, "sequencer_note_start_color", "Sequencer note start", false},

  {SEQUENCER_CURR_ENTRY_BORDER_COLOR_NUM, "sequencer_curr_entry_border_color", "Sequencer current list entry border", false},

  {SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM, "sequencer_block_background_color", "Sequencer block background", false},
  {SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM, "sequencer_multiselect_block_background_color", "Sequencer block multiselected background", false},
  {SEQUENCER_BLOCK_BORDER_COLOR_NUM, "sequencer_block_border_color", "Sequencer block border", false},
  {SEQUENCER_BLOCK_BAR_COLOR_NUM, "sequencer_block_bar_color", "Sequencer block bar", false},
  {SEQUENCER_BLOCK_BEAT_COLOR_NUM, "sequencer_block_beat_color", "Sequencer block beat", false},
  {SEQUENCER_BLOCK_SELECTED_COLOR_NUM, "sequencer_block_selected_color", "Sequencer block selected", false},
  {SEQUENCER_BLOCK_AUDIO_FILE_BACKGROUND_COLOR_NUM, "sequencer_block_audio_file_background_color", "Sequencer: Audio file background", false},
  {SEQUENCER_BLOCK_FADE_BOX_COLOR_NUM, "sequencer_block_fade_box_color", "Seqblock fade box", false},
  {SEQUENCER_BLOCK_INTERIOR_BOX_COLOR_NUM, "sequencer_block_interior_box_color", "Seqblock interior box", false},
  {SEQUENCER_BLOCK_SPEED_BOX_COLOR_NUM, "sequencer_block_speed_box_color", "Seqblock speed box", false},
  {SEQUENCER_BLOCK_STRETCH_BOX_COLOR_NUM, "sequencer_block_stretch_box_color", "Seqblock stretch box", false},
  {SEQUENCER_WAVEFORM_COLOR_NUM, "sequencer_waveform_color", "Sequencer: Waveform", false},

  {SEQUENCER_GRID_COLOR_NUM, "sequencer_grid_color", "Sequencer grid", false},
  {SEQUENCER_TRACK_BORDER1_COLOR_NUM, "sequencer_track_border1_color", "Sequencer track border color (first)", false},
  {SEQUENCER_TRACK_BORDER2_COLOR_NUM, "sequencer_track_border2_color", "Sequencer track border", false},
  {SEQUENCER_CURSOR_COLOR_NUM, "sequencer_cursor_color", "Sequencer cursor", false},
  {SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM, "sequencer_timeline_background_color", "Sequencer timeline background", false},
  {SEQUENCER_TIMELINE_ARROW_COLOR_NUM, "sequencer_tinmeline_arrow_color", "Sequencer timeline arrow", false},
  {SEQUENCER_NAVIGATOR_HANDLER_COLOR_NUM, "sequencer_navigator_handler_color", "Sequencer navigator handler", false},
  {SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM, "sequencer_navigator_grayout_color", "Sequencer navigator gray out", false},
  {SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM, "sequencer_tempo_automation_color", "Sequencer tempo automation", false},
  {SEQUENCER_MARKER_COLOR_NUM, "sequencer_marker_color", "Sequencer marker", false},
  
  {END_CONFIG_COLOR_NUM, NULL, NULL, false}
};

static ReplacementColorNum g_replacement_color_num[] = {

  //{BUTTONS_NOT_ENABLED_COLOR_NUM, BUTTONS_COLOR_NUM},
  //{CHECK_BOX_NOT_ENABLED_COLOR_NUM, CHECK_BOX_SELECTED_COLOR_NUM},

  {SOUNDFONT_COLOR_NUM, BUTTONS_COLOR_NUM}, // 13=green  
  {SOUNDFILE_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM}, // 7=bluish
  {CURRENT_SOUNDFILE_COLOR_NUM, VELOCITY2_COLOR_NUM},
    
  {SLIDER2_COLOR_NUM, BUTTONS_COLOR_NUM},
  {SLIDER_DISABLED_COLOR_NUM, HIGH_BACKGROUND_COLOR_NUM},
  {TRACK_SLIDER_COLOR_NUM, TEXT_COLOR_NUM},

  {PEAKS_COLOR_NUM, BUTTONS_COLOR_NUM},
  {PEAKS_0DB_COLOR_NUM, VELOCITY2_COLOR_NUM},
  {PEAKS_4DB_COLOR_NUM, PORTAMENTO_NOTE_TEXT_COLOR_NUM},
  {VELOCITY_TEXT_COLOR_NUM, TEXT_COLOR_NUM},

  {PIANONOTE_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},
  
  {NOTE_EVENT_INDICATOR_COLOR_NUM, EDITOR_SLIDERS_COLOR_NUM},
  {NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},
  
  {AUTOMATION4_COLOR_NUM, WAVEFORM_COLOR_NUM},
  {AUTOMATION5_COLOR_NUM, VELOCITY1_COLOR_NUM},
  {AUTOMATION6_COLOR_NUM, VELOCITY2_COLOR_NUM},
  {AUTOMATION7_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},
  {AUTOMATION8_COLOR_NUM, INSTRUMENT_NAME_COLOR_NUM},
  {AUTOMATION_INDICATOR_COLOR_NUM, PORTAMENTO_NOTE_TEXT_COLOR_NUM},

  {TRACK_SEPARATOR1_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},
  {TRACK_SEPARATOR2B_COLOR_NUM, LOW_BACKGROUND_COLOR_NUM},

  {BAR_TEXT_COLOR_NUM, INSTRUMENT_NAME_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM1, PORTAMENTO_NOTE_TEXT_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM2, WAVEFORM_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM3, AUTOMATION1_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM4, AUTOMATION2_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM5, VELOCITY1_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM6, VELOCITY2_COLOR_NUM},
  {ZOOMLINE_TEXT_COLOR_NUM7, CURSOR_EDIT_ON_COLOR_NUM},

  {PITCH_LINE_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},

  {PIANOROLL_OCTAVE_COLOR_NUM, INSTRUMENT_NAME_COLOR_NUM},
  {PIANOROLL_NOTE_NAME_COLOR_NUM, INSTRUMENT_NAME_COLOR_NUM},
  
  {CURSOR_EDIT_OFF_COLOR_NUM, VELOCITY1_COLOR_NUM},

  {PORTAMENTO_END_NOTE_TEXT_COLOR_NUM, CURSOR_EDIT_ON_COLOR_NUM},

  {END_CONFIG_COLOR_NUM, ILLEGAL_COLOR_NUM}

};

static ReplacementColor g_replacement_color[] = {
  {LOW_EDITOR_BACKGROUND_COLOR_NUM, QColor("#475253")},
  {HIGH_EDITOR_BACKGROUND_COLOR_NUM, QColor("#ff585d55")},
  {LOW_BACKGROUND_COLOR_NUM, QColor("#50585a")},
  {HIGH_BACKGROUND_COLOR_NUM, QColor("#22282a")},
  {MENU_TEXT_COLOR_NUM, QColor("#f7f5e4")},
  {MENU_KEYBINDING_TEXT_COLOR_NUM, QColor("#e57c12")},
  
  {SCROLLBAR_COLOR_NUM, QColor("black")},
  {SCROLLBAR_BACKGROUND_COLOR_NUM, QColor("#69b8b8b8")},
  
  {KEYBOARD_FOCUS_BORDER_COLOR_NUM, QColor("#ff9d00")},

  {CURSOR_BORDER_COLOR_NUM, QColor("#50000000")},
  {CURSOR_CURR_COLUMN_BORDER_COLOR_NUM, QColor("#d0ff0000")},

  {VELOCITY1_COLOR_NUM, QColor("#ff1e211e")},
  {VELOCITY2_COLOR_NUM, QColor("#ffab8d5c")},
  
  {SLIDER1_COLOR_NUM, QColor(108,65,36)},
  {SLIDER_TEXT_COLOR_NUM, QColor(0,0,0)},
  {SLIDER_RECORDING_COLOR_NUM, QColor(197,0,3)},

  {TAB_SELECTED_COLOR_NUM,  QColor("green")},
  {TAB_UNSELECTED_COLOR_NUM,  QColor("#004000")},

  {BUTTONS_COLOR_NUM, QColor("#293c58")},
  {BUTTONS_PRESSED_COLOR_NUM, QColor("#ff082441")},
  {CHECK_BOX_SELECTED_COLOR_NUM,  QColor("#ff115a2e")},
  {CHECK_BOX_UNSELECTED_COLOR_NUM,  QColor("#ff34413c")},
  {BUTTONS_TEXT_COLOR_NUM, QColor("#cccccc")},
    
  {PIANONOTE_SELECTED_COLOR_NUM, QColor("#00c8e6")},
  
  {PIANOROLL_NOTE_BORDER_COLOR_NUM, QColor("#aa000001")},
  {PLAY_CURSOR_COLOR_NUM, QColor(255, 0, 0)},
  {LINE_SLIDER_COLOR_NUM, QColor(1,1,1)},

  {RANGE_COLOR_NUM, QColor("#3e4e4790")},

  {TRACK_SEPARATOR1_COLOR_NUM, QColor("#ff0b186c")},
  {TRACK_SEPARATOR2A_COLOR_NUM, QColor("#ff303030")},

  {TEMPOGRAPH_COLOR_NUM, QColor("#ff192273")},

  {MIXER_TEXT_COLOR_NUM, QColor(1,1,1)},
  {MIXER_BORDER_COLOR_NUM, QColor(1,1,1)},

  {MIXER_EVENT_CONNECTION_COLOR_NUM, QColor(30,95,70,140)},
  {MIXER_AUDIO_CONNECTION_COLOR_NUM, QColor(50,25,70,140)},
  {MIXER_AUTOSUSPENSION_COLOR_NUM, QColor("#3ca6a6a6")},
  {MIXER_CURRENT_OBJECT_BORDER_COLOR_NUM, QColor("#ff041da7")},
  {MIXER_SELECTED_OBJECT_BORDER_COLOR_NUM, QColor("#ff81080a")},
  {MIXERSTRIPS_CURRENT_INSTRUMENT_BORDER_COLOR_NUM, QColor("white")},

  {INSTRUMENT_BUS_DEFAULT_COLOR_NUM, QColor("#1b5f72")},
  {SEQTRACK_INSTRUMENT_DEFAULT_COLOR_NUM, QColor("#692426")},

  {SEQUENCER_CURRTRACK_BORDER_COLOR_NUM, QColor("#ffffffff")},
  {SEQUENCER_CURR_SEQBLOCK_BORDER_COLOR_NUM, QColor("#ebc909")},
  {SEQUENCER_LANES_BACKGROUND_COLOR_NUM, QColor("#ff464d47")},
  {SEQTRACKS_BACKGROUND_COLOR_NUM, QColor("#ff3a4844")},
  {SEQUENCER_BORDER_COLOR_NUM, QColor("#ff010101")},
  {SEQUENCER_TEXT_COLOR_NUM, QColor("#ff010101")},
  {SEQUENCER_TEXT_CURRENT_BLOCK_COLOR_NUM, QColor("#d4d4d4")},
  {SEQUENCER_NOTE_COLOR_NUM, QColor("#960101c8")},
  {SEQUENCER_NOTE_START_COLOR_NUM, QColor("#ff000000")},

  {SEQUENCER_CURR_ENTRY_BORDER_COLOR_NUM, QColor("#005a00")},
  
  {SEQUENCER_BLOCK_BACKGROUND_COLOR_NUM, QColor("#ff8c8c8c")},
  {SEQUENCER_BLOCK_BORDER_COLOR_NUM, QColor("#ff321423")},
  {SEQUENCER_BLOCK_BAR_COLOR_NUM, QColor("#d2000000")},
  {SEQUENCER_BLOCK_BEAT_COLOR_NUM, QColor("#460a1e17")},
  {SEQUENCER_BLOCK_SELECTED_COLOR_NUM, QColor("#5400801d")},
  {SEQUENCER_BLOCK_MULTISELECT_BACKGROUND_COLOR_NUM, QColor("#ffff4811")},
  {SEQUENCER_BLOCK_AUDIO_FILE_BACKGROUND_COLOR_NUM, QColor("#64eeeeee")},
  {SEQUENCER_BLOCK_FADE_BOX_COLOR_NUM, QColor("#ff00ff00")},
  {SEQUENCER_BLOCK_INTERIOR_BOX_COLOR_NUM, QColor("#ff4c73ff")},
  {SEQUENCER_BLOCK_SPEED_BOX_COLOR_NUM, QColor("#ffff6060")},
  {SEQUENCER_BLOCK_STRETCH_BOX_COLOR_NUM, QColor("yellow")},
  {SEQUENCER_WAVEFORM_COLOR_NUM, QColor("#c8000000")},

  {SEQUENCER_GRID_COLOR_NUM, QColor("#99281980")},
  {SEQUENCER_TRACK_BORDER1_COLOR_NUM, QColor("#ff141414")},
  {SEQUENCER_TRACK_BORDER2_COLOR_NUM, QColor("#80141414")},
  {SEQUENCER_CURSOR_COLOR_NUM, QColor("#ffc81414")},
  {SEQUENCER_TIMELINE_BACKGROUND_COLOR_NUM, QColor("#ff8c8c8c")},
  {SEQUENCER_TIMELINE_ARROW_COLOR_NUM, QColor("#ff2b9d4c")},
  {SEQUENCER_NAVIGATOR_HANDLER_COLOR_NUM, QColor("#5a141414")},
  {SEQUENCER_NAVIGATOR_GRAYOUT_COLOR_NUM, QColor("#96323232")},
  {SEQUENCER_TEMPO_AUTOMATION_COLOR_NUM, QColor("#baab88")},
  {SEQUENCER_MARKER_COLOR_NUM, QColor("#0a0632")},
  
  {END_CONFIG_COLOR_NUM, QColor(1,2,3)}
};


extern struct Root *root;

static QApplication *application;
//static QColor *system_color=NULL;
//static QColor *button_color=NULL;
static bool override_default_qt_colors=true;

static QColor *g_note_colors;

__attribute__((constructor)) static void initialize_g_note_colors(void) {
  g_note_colors = new QColor[128];
}


#if 0
#define NUM_NOTE_COLOR_CONF 4
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = { // [NO_STATIC_ARRAY_WARNING]
  {128*0,                     1},
  {128*1/NUM_NOTE_COLOR_CONF, 5},
  {128*2/NUM_NOTE_COLOR_CONF, 6},
  {128*3/NUM_NOTE_COLOR_CONF, 4},
  {128*4/NUM_NOTE_COLOR_CONF, 2},
};
#endif

#if 0
#define NUM_NOTE_COLOR_CONF 9
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = { // [NO_STATIC_ARRAY_WARNING]
  {128*0,1},
  {128*1/NUM_NOTE_COLOR_CONF,2},
  {128*2/NUM_NOTE_COLOR_CONF,3},
  {128*3/NUM_NOTE_COLOR_CONF,4},
  {128*4/NUM_NOTE_COLOR_CONF,5},
  {128*5/NUM_NOTE_COLOR_CONF,6},
  {128*6/NUM_NOTE_COLOR_CONF,8},
  {128*7/NUM_NOTE_COLOR_CONF,12},
  {128*8/NUM_NOTE_COLOR_CONF,13},
  {128*9/NUM_NOTE_COLOR_CONF,14}
};
#endif

#define NUM_NOTE_COLOR_CONF 6
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = { // [NO_STATIC_ARRAY_WARNING]
  {0,PEAKS_0DB_COLOR_NUM},
  {24,PEAKS_4DB_COLOR_NUM},
  {48,PEAKS_0DB_COLOR_NUM},
  {72,PEAKS_4DB_COLOR_NUM},
  {96,WAVEFORM_COLOR_NUM},
  {120,TEXT_COLOR_NUM},
  {128,INSTRUMENT_NAME_COLOR_NUM}
};

static void configure_note_colors(void){
  for(int i=0;i<NUM_NOTE_COLOR_CONF;i++){
    QColor start_color = get_custom_qcolor(note_color_conf[i][1]);
    QColor end_color = get_custom_qcolor(note_color_conf[i+1][1]);
    int start_note = note_color_conf[i][0];
    int end_note = note_color_conf[i+1][0];

    if(start_note<64)
      start_color = get_qcolor(TEXT_COLOR_NUM);
    else
      end_color = get_qcolor(WAVEFORM_COLOR_NUM);

    for(int note=start_note;note<end_note;note++){
      //printf("setting %d (%d-%d) to %d %d %f\n",note,start_note,end_note,note_color_conf[i][1], note_color_conf[i+1][1], (float)(note-start_note)/(end_note-start_note));
      g_note_colors[note] = mix_colors(end_color, start_color, (float)(note-start_note)/(end_note-start_note));
    }
  }
}

unsigned int GFX_mix_colors(unsigned int c1, unsigned int c2, float how_much){
  return mix_colors(QColor(c1), QColor(c2), how_much).rgb();
}

namespace{
  struct MyColorDialog : public QColorDialog {
    Q_OBJECT;
    
public:

    radium::ProtectedS7Extra<func_t*> _callback;
    QColor _initial_color;
    
    MyColorDialog(QWidget *parent, func_t *callback, const char *initial_color)
      : QColorDialog(parent)
      , _callback(callback, "colordialog_callback")
      , _initial_color(initial_color)
    {
      setAttribute(Qt::WA_DeleteOnClose);
      set_window_flags(this, radium::NOT_MODAL);
      
      setOption(QColorDialog::DontUseNativeDialog, true);
      setOption(QColorDialog::ShowAlphaChannel, true);

      setCurrentColor(_initial_color);
      
      connect(this, SIGNAL(currentColorChanged(const QColor &)), this, SLOT(color_changed(const QColor &)));

      updateGeometry();
      moveWindowToCentre(this);
    }
    
    ~MyColorDialog(){
      printf("  MyColorDialog deleted\n");
    }

    bool event(QEvent *e) override{
      if (e->type() == QEvent::WindowActivate) {
        printf("Activated\n");
        obtain_keyboard_focus_without_greying(); // don't want to gray editor when we configure a color that might be displayed in it.
      } else if (e->type() == QEvent::WindowDeactivate) {
        release_keyboard_focus(); // don't want to gray editor when we configure a color that might be displayed in it.
        printf("Deactivate\n");
      }
      
      return QColorDialog::event(e);        
    }
        
    void done(int result) override{
      if (result==QDialog::Rejected)
        S7CALL(void_charpointer, _callback.v, _initial_color.name(QColor::HexArgb).toUtf8().constData());

      release_keyboard_focus();

      QColorDialog::done(result);
    }


  public slots:
    void color_changed(const QColor &col){
      printf("Color changed\n");
      S7CALL(void_charpointer,_callback.v, col.name(QColor::HexArgb).toUtf8().constData());
    }
  };
}


void GFX_color_dialog(const char *initial_color, int64_t parentguinum, func_t *callback){
  MyColorDialog *color_dialog = new MyColorDialog(API_gui_get_parentwidget(NULL, parentguinum), callback, initial_color);

  safeShow(color_dialog);
  
  #if 0
  int ret = safeExec(&color_dialog, true);  

  if (ret==QDialog::Rejected){
    
    S7CALL(void_charpointer,callback, initial_color);
    //return talloc_strdup(initial_color);
    
  }
    
  //return talloc_strdup(color_dialog.currentColor().name(QColor::HexArgb).toUtf8().constData());
  #endif
}

unsigned int GFX_get_color(enum ColorNums colornum){
  return get_qcolor(colornum).rgb();
}

unsigned int GFX_get_color_from_colorname(const char *colorname){
#if DEBUG_COLORS
  return GFX_MakeRandomColor();
#endif
  
  QColor color(colorname);
  return color.rgb();
}

const char *GFX_get_colorname_from_color(unsigned int colornum){
#if DEBUG_COLORS
  return talloc_strdup(QColor(GFX_MakeRandomColor()).name().toUtf8());
#endif
  
  QColor color(colornum);
  return talloc_strdup(color.name(QColor::HexArgb).toUtf8());
}

QHash<int, QColor> custom_colors;
static const int first_custom_colornum = 1024; // just start somewhere.

// Algorithm from http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
static QColor get_next_color(void){
  const double golden_ratio_conjugate = 0.618033988749895;
  static double h = 0.5;

  QColor color;

#if 1
  //color.setHsvF(h, 0.9, 0.95);
  if (h > 0.135 && h < 0.470) {
    float middle = 0.33; //(0.470 + 0.135) / 2.0;
    float distance = fabs(h-middle);
    float saturation = scale(distance, 0, middle, 0.3, 0.9);  // don't want too green/yellow-ish color
    float value = scale(distance, 0, middle, 0.6, 0.9);  // don't want too green/yellow-ish color
                             
    color.setHsvF(h, saturation, value);
  } else if (h > 0.77) {
    float middle = 0.77;
    float distance = fabs(h-middle);
    float saturation = scale(distance, 0, middle, 0.6, 0.9);  // don't want too purplish color
    float value = scale(distance, 0, middle, 0.6, 0.9);  // don't want too purplish color
                             
    color.setHsvF(h, saturation, value);
  } else if (h > 0.66) {
    float middle = 0.66;
    float distance = fabs(h-middle);
    float saturation = scale(distance, 0, middle, 0.7, 0.9);  // don't want too bluish color
    float value = scale(distance, 0, middle, 0.7, 0.9);  // don't want too bluish color
                             
    color.setHsvF(h, saturation, value);
  } else
#endif
    color.setHsvF(h, 0.9, 0.95);

  h += golden_ratio_conjugate;
  h = fmod(h, 1.0);

  return color;
}

unsigned int GFX_MakeRandomColor(void){//int blendcolornum, float blendfactor){
  QColor color = get_next_color().darker();//mix_colors(get_next_color(), get_qcolor(blendcolornum), blendfactor);
  //QColor color = mix_colors(get_next_color(), get_qcolor(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.12f);
  return color.rgb();
}

unsigned int GFX_MakeRandomBlockColor(void){
  //return GFX_MakeRandomColor();
  
  QColor color = get_next_color().lighter(125);
  return color.rgb();  
}
  
// if colornum==-1, create new color
int GFX_MakeRandomCustomColor(int colornum){
  static int num_colors = first_custom_colornum;

  if (colornum==-1)
    colornum = num_colors++;

  //custom_colors[colornum] = get_next_color(); //mix_colors(QColor(qrand()%100, qrand()%105, qrand()%255), get_qcolor(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.14f);
  custom_colors[colornum] = mix_colors(get_next_color(), get_qcolor(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.12f);

  return colornum;
}


/*
static bool is_configurable_color(enum ColorNums colornum){
  if (colornum==BLACK_COLOR_NUM)
    return false;
  if (colornum==WHITE_COLOR_NUM)
    return false;
  if (colornum==RED_COLOR_NUM)
    return false;

  return true;
}
*/


bool has_qcolor(int colornum){
  int i=0;
  while(g_colorconfig[i].num != END_CONFIG_COLOR_NUM){
    if (g_colorconfig[i].num==colornum)
      return g_colorconfig[i].is_separator==false;
    i++;
  }

  return false;
}

bool is_qcolor_separator(int colornum){
  int i=0;
  while(g_colorconfig[i].num != END_CONFIG_COLOR_NUM){
    if (g_colorconfig[i].num==colornum)
      return g_colorconfig[i].is_separator==true;
    i++;
  }

  return false;
}

static const ColorConfig get_color_config(enum ColorNums colornum){
  int i=0;
  while(g_colorconfig[i].num != END_CONFIG_COLOR_NUM){
    if (g_colorconfig[i].num==colornum)
      return g_colorconfig[i];
    i++;
  }

  RError("Unknown color %d", colornum);
  return g_colorconfig[0];
}

const char *get_color_display_name(enum ColorNums colornum){
  return get_color_config(colornum).display_name;
}

/*
static int get_colornum(const char *color_config_name){
  for(int i=0;i<END_CONFIG_COLOR_NUM;i++){
    if (is_configurable_color(i))
      if (!strcmp(color_config_name, get_color_config_name(i)))
        return i;
  }

  RError("Unknown color config name %s", color_config_name);
  return -1;
}
*/

static enum ColorNums get_replacement_colornum(enum ColorNums colornum){
  int i=0;
  while(g_replacement_color_num[i].num != END_CONFIG_COLOR_NUM){
    if (g_replacement_color_num[i].num==colornum)
      return g_replacement_color_num[i].replacement_num;
    i++;
  }

  return ILLEGAL_COLOR_NUM;
}

static QColor get_replacement_color(enum ColorNums colornum){
  int i=0;
  while(g_replacement_color[i].num != END_CONFIG_COLOR_NUM){
    if (g_replacement_color[i].num==colornum)
      return g_replacement_color[i].color;
    i++;
  }

  if(g_is_starting_up) {
    static bool has_shown_startup_error = false;
    if (has_shown_startup_error == false){
      GFX_Message(NULL, "Could not find color \"%s\". Your color configuration is not valid. Try deleting the file .radium/color in your home directory and start Radium again.", get_color_config(colornum).config_name);
      has_shown_startup_error = true;
    }
    return QColor(generateNewColor(1.0));
  }

  RWarning("Unable to find color %s in config file", get_color_config(colornum).config_name);

  QColor ret;  
  return ret;
}

static QColor **g_config_colors;

__attribute__((constructor)) static void initialize_g_config_colors(void) {
  g_config_colors = (QColor**)calloc(sizeof(QColor*), END_CONFIG_COLOR_NUM);
}
  

static void clear_config_color(enum ColorNums num){
  delete g_config_colors[num];
  g_config_colors[num] = NULL;
}

static void clear_config_colors(void){
  for(int i=START_CONFIG_COLOR_NUM;i<END_CONFIG_COLOR_NUM;i++)
    clear_config_color((enum ColorNums)i);
}

static QColor get_config_qcolor(enum ColorNums colornum){
  static bool has_inited = false;

  if (has_inited==false){
    has_inited = true;

    for(int i=START_CONFIG_COLOR_NUM;i<END_CONFIG_COLOR_NUM;i++)
      if (has_qcolor(i))
        get_config_qcolor((enum ColorNums)i);    
  }

  if (g_config_colors[colornum]!=NULL)
    return *g_config_colors[colornum];

  QColor col;

  const char *colorname = get_color_config(colornum).config_name;
  if (colorname==NULL)
    return col;

  if (!QString(colorname).contains("color"))
    colorname = talloc_format("%s_color", colorname);
  
  const char *colorstring = SETTINGS_read_string(colorname, "");
  
  if (strlen(colorstring) <= 1) {
    enum ColorNums replacement_colornum = get_replacement_colornum(colornum);
    if (replacement_colornum != ILLEGAL_COLOR_NUM)
      col = get_config_qcolor(replacement_colornum);
    else
      col = get_replacement_color(colornum);
  } else {
    col = QColor(colorstring);
  }

  g_config_colors[colornum] = new QColor(col);
  
  return col;
}


static QColor get_qcolor_really(enum ColorNums colornum){
  static QColor black(1,1,1);//"black");
  static QColor white(254,254,254);//"black");
  static QColor red("red");

  //if(colornum < 16)
  //  return editor->colors[colornum];
  //  RError("Unknown color. Very strange %d", (int)colornum);
  
  if (colornum < END_CONFIG_COLOR_NUM)
    return get_config_qcolor(colornum);

  if (colornum==BLACK_COLOR_NUM)
    return black;
  
  if (colornum==WHITE_COLOR_NUM)
    return white;

  if (colornum==RED_COLOR_NUM)
    return red;
  
  RError("Unknown color. Very strange %d", (int)colornum);
  return white;
}

QColor get_custom_qcolor(int colornum){
  if (colornum < END_ALL_COLOR_NUMS)
    return get_qcolor_really((enum ColorNums)colornum);
  
  if(colornum >= first_custom_colornum)
    return custom_colors[colornum];

  if(colornum > 16+128){
    RError("Illegal colornum: %d",colornum);
    colornum = colornum % (128+16);
  }

  colornum -= 16;

  static bool note_colors_configured = false;
  if(note_colors_configured==false){
    configure_note_colors();
    note_colors_configured=true;
  }

  return g_note_colors[colornum];  
}

QColor get_qcolor(enum ColorNums colornum){
#if DEBUG_COLORS
  return GFX_MakeRandomColor();
#endif
  
  return get_custom_qcolor((int)colornum);
}

QColor get_config_qcolor(QString colorname){
  static bool has_inited = false;

  static QHash<QString,enum ColorNums> s_colors; // Can't we store QColors as values here? (no, because then it doesn't work adjusting colors after startup)

  if (has_inited==false){
    int i=0;
    for(;;){
      const ColorConfig &config = g_colorconfig[i];
      if(config.num == END_CONFIG_COLOR_NUM)
        break;
      s_colors[config.config_name] = config.num;
      i++;
    }
    has_inited = true;
  }

  static QHash<QString, QColor> s_error_colors;
  
  if (s_colors.contains(colorname))
    return get_qcolor_really(s_colors[colorname]);
  else {
    auto ret = QColor(colorname);
    if (!ret.isValid()){
      
      if (s_error_colors.contains(colorname))
        return s_error_colors[colorname];
      
      // Store a valid color for this colorname to avoid the same error message popping up again. (only need to see the error once)
      s_error_colors[colorname] = get_next_color().name(QColor::HexArgb);
    }
    return ret;
  }
}

static void updatePalette(EditorWidget *my_widget, QWidget *widget, QPalette &pal){

  /*
  if(system_color==NULL){
    system_color=new QColor(SETTINGS_read_string("system_color","#d2d0d5"));
    SETTINGS_write_string("system_color",system_color->name());
  }
  */
  /*
  if(button_color==NULL){
    button_color=new QColor(SETTINGS_read_string("button_v2_color","#c1f1e3"));
    SETTINGS_write_string("button_v2_color",button_color->name());
  }
  */
  if(override_default_qt_colors==false){
    //qapplication->setPalette(t.palette());

    //my_widget->setPalette( QApplication::palette( my_widget ) );
    return;
  }

  // Background
  {

    //QColor c(0xe5, 0xe5, 0xe5);
    QColor c = get_qcolor(LOW_BACKGROUND_COLOR_NUM); //(*system_color);
    QColor b = get_qcolor(HIGH_BACKGROUND_COLOR_NUM); //(*system_color);

    if(dynamic_cast<QComboBox*>(widget)!=NULL){
      c = get_qcolor(BUTTONS_COLOR_NUM);
#if 0
      c = mix_colors(c.lighter(70),QColor(98,59,33),0.55);//editor->colors[colnum].lighter(52);
      c.setAlpha(76);
#endif
    }

    pal.setColor( QPalette::Active, QPalette::Background, b);
    pal.setColor( QPalette::Inactive, QPalette::Background, b);
    pal.setColor( QPalette::Disabled, QPalette::Background, b.lighter(95));

    pal.setColor( QPalette::Active, QPalette::Button, c);
    pal.setColor( QPalette::Inactive, QPalette::Button, c);
    pal.setColor( QPalette::Disabled, QPalette::Button, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::Base, c);
    pal.setColor(QPalette::Inactive, QPalette::Base, c);
    pal.setColor(QPalette::Disabled, QPalette::Base, c.lighter(80));

    pal.setBrush(QPalette::Highlight, (const QBrush&)QBrush(b.lighter(85)));

    pal.setColor(QPalette::Disabled, QPalette::Light, c.lighter(80));

    //pal.setBrush((QPalette::ColorGroup)QPalette::Button, QPalette::Highlight, (const QBrush&)QBrush(b.light(95)));
    //pal.setBrush(QPalette::Highlight, QPalette::Button, QBrush(c.light(80)));
    //pal.setBrush(QPalette::Highlight, QPalette::Base, QBrush(c.light(80)));


#if 0
    pal.setColor(QPalette::Active, QPalette::Window, c);
    pal.setColor(QPalette::Inactive, QPalette::Window, c);
    pal.setColor(QPalette::Disabled, QPalette::Window, c.lighter(80));
#endif

#if 0
    pal.setColor(QPalette::Active, QPalette::BrightText, c);
    pal.setColor(QPalette::Inactive, QPalette::BrightText, c);
    pal.setColor(QPalette::Disabled, QPalette::BrightText, c.lighter(80));
#endif

  }

  // Foreground, text, etc. (everything blackish)
  {
    QColor c = get_qcolor(TEXT_COLOR_NUM);
    //QColor black(QColor("black"));
    c.setAlpha(180);
    //black.setAlpha(108);

    pal.setColor(QPalette::Active, QPalette::Foreground, c);
    pal.setColor(QPalette::Inactive, QPalette::Foreground, c.lighter(93));
    pal.setColor(QPalette::Disabled, QPalette::Foreground, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::Foreground, c);
    pal.setColor(QPalette::Inactive, QPalette::Foreground, c.lighter(93));
    pal.setColor(QPalette::Disabled, QPalette::Foreground, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::ButtonText, c);
    pal.setColor(QPalette::Inactive, QPalette::ButtonText, c.lighter(93));
    pal.setColor(QPalette::Disabled, QPalette::ButtonText, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::ButtonText, c);
    pal.setColor(QPalette::Inactive, QPalette::ButtonText, c.lighter(93));
    pal.setColor(QPalette::Disabled, QPalette::ButtonText, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::Text, c);
    pal.setColor(QPalette::Inactive, QPalette::Text, c.lighter(90));
    pal.setColor(QPalette::Disabled, QPalette::Text, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::HighlightedText, c.lighter(100));
    pal.setColor(QPalette::Inactive, QPalette::HighlightedText, c.lighter(90));
    pal.setColor(QPalette::Disabled, QPalette::HighlightedText, c.lighter(80));

    pal.setColor(QPalette::Active, QPalette::Text, c);
    pal.setColor(QPalette::Inactive, QPalette::Text, c.lighter(90));
    pal.setColor(QPalette::Disabled, QPalette::Text, c.lighter(80));
  }
}

static QPalette sys_palette;

static void updateWidget(EditorWidget *my_widget,QWidget *widget){
  
  QPalette pal(widget->palette());

  updatePalette(my_widget,widget,pal);

  if(override_default_qt_colors)
    widget->setPalette(pal);
  else
    widget->setPalette(sys_palette);

  //printf("Calling update() 3\n");
  widget->update();
  
  //QFont font=QFont("Bitstream Vera Sans Mono",4);
  //widget->setFont(QApplication::font());
}

static void updateApplication(EditorWidget *my_widget,QApplication *app){
  QPalette pal(app->palette());
  updatePalette(my_widget,NULL,pal);
  app->setPalette(pal);
}

static void updateAll(EditorWidget *my_widget, QWidget *widget){
#if defined(WITH_FAUST)
  if (dynamic_cast<QsciScintilla*>(widget) != NULL)
    return;
#endif
  
  if (dynamic_cast<QTGUI*>(widget) != NULL)
    return;

  configure_note_colors();

  updateWidget(my_widget, widget);

#if 0
  if(widget->objectName () == "view"){
    widget->setStyleSheet(QString::fromUtf8("background-color: qlineargradient(spread:reflect, x1:0, y1:0, x2:0.38, y2:1, stop:0 rgba(111, 131, 111, 22), stop:1 rgba(255, 255, 255, 43));"));
  }
#endif

#if 0
  widget->setStyleSheet(QString::fromUtf8("color: qlineargradient(spread:reflect, x1:0, y1:0, x2:0.38, y2:1, stop:0 rgba(0, 3, 0, 155), stop:1 rgba(5, 5, 5, 175));"));
#endif

  const QList<QObject*> list = widget->children();
  if(list.empty()==true)
    return;

  for (int i = 0; i < list.size(); ++i) {
    QWidget *widget = dynamic_cast<QWidget*>(list.at(i));
    if(widget!=NULL)
      updateAll(my_widget,widget);
  }
}

static void updateAll(EditorWidget *my_widget){
  foreach (QWidget *widget, QApplication::allWidgets())
    updateWidget(my_widget, widget);

  //updateAll(my_widget,application->mainWidget());
  updateApplication(my_widget,application);
}

void setWidgetColors(QWidget *widget){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *my_widget = static_cast<EditorWidget*>(window->os_visual.widget);
  updateAll(my_widget,widget);
}

void setApplicationColors(QApplication *app){
  EditorWidget *my_widget = root==NULL ? NULL : root->song==NULL ? NULL : static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

  override_default_qt_colors = SETTINGS_read_bool("override_default_qt_colors",true);

#if 1
  static bool first_run = true;
  if(first_run==true){
    sys_palette = QApplication::palette();
    SETTINGS_write_bool("override_default_qt_colors",override_default_qt_colors);
    first_run=false;
  }
#endif
  printf("here\n");
  application = app;
  if(my_widget==NULL)
    updateApplication(my_widget,app);
  else
    updateAll(my_widget);
}



static void setColor(enum ColorNums num, const QRgb &rgb){
  R_ASSERT_RETURN_IF_FALSE(num<END_CONFIG_COLOR_NUM);

  GL_lock();{

#if USE_GTK_VISUAL
    GTK_SetColor(num,qRed(rgb),qGreen(rgb),qBlue(rgb));
#endif

    if (g_config_colors[num]==NULL)
      get_config_qcolor(num);

    g_config_colors[num]->setRgba(rgb);

    /*
    if(num==LOW_BACKGROUND_COLOR_NUM)
      system_color->setRgb(rgb);
    */
    /*
    else if(num==HIGH_BACKGROUND_COLOR_NUM)
      button_color->setRgb(rgb);
    */
  }GL_unlock();
}


void GFX_SetBrightness(struct Tracker_Windows *tvisual, float how_much){
  EditorWidget *editorwidget=(EditorWidget *)tvisual->os_visual.widget;
  if(g_is_starting_up)
    return;
  return;
#if 0
  float threshold = QColor(SETTINGS_read_string(talloc_format("color%d",15),"#d0d5d0")).valueF();
  
  for(int i=0;i<15;i++){
    QColor color = QColor(SETTINGS_read_string(talloc_format("color%d",i),"#d0d5d0"));
      
    //QColor color = editorwidget->colors[i];
    float value = color.valueF();
    if (value > threshold)
      color = color.lighter(scale(how_much, 0, 1, 0, 200));
    else
      color = color.darker(scale(how_much, 0, 1, 0, 200));

    if (i!=11)
      setColor((enum ColorNums)i, color.rgba());
    printf("value for %d: %f\n",i,value);
    //color.setLightntess(lightness
  }
#else
  
  how_much = scale(how_much,0,1,-1,1);

  for(int i=0;i<16;i++){
    QColor color = QColor(SETTINGS_read_string(talloc_format("color%d",i),"#d0d5d0"));

    qreal h,s,v,a;
    color.getHsvF(&h,&s,&v,&a);

    float value = R_BOUNDARIES(0,v+how_much,1);
    color.setHsvF(h, s, value, a);
    
    //QColor color = editorwidget->colors[i];
    setColor((enum ColorNums)i, color.rgba());
    
    printf("value for %d: %f. s: %f, how_much: %f\n",i,value,s,how_much);
    //color.setLightntess(lightness
  }
#endif
  
  updateAll(editorwidget);
  tvisual->must_redraw = true;
}

void testColorInRealtime(enum ColorNums num, QColor color){
  R_ASSERT_RETURN_IF_FALSE(num<END_CONFIG_COLOR_NUM);

  //printf("  alpha2: %d\n",color.alpha());
  
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *my_widget=(EditorWidget *)window->os_visual.widget;
  setColor(num,color.rgba());
  updateAll(my_widget);

  GFX_update_current_instrument_widget();

  window->must_redraw = true;
}

#include "Qt_Main_proc.h"

// Workaround, expose events are not sent when the qcolor dialog blocks it. Only necessary when using the GTK visual.
// Qt only calls qapplication->processEvents() (somehow), and not gtk_main_iteration_do(), so we do that manually here.
class Scoped_GTK_EventHandler_Timer : public QTimer{
  Q_OBJECT

public:
  Scoped_GTK_EventHandler_Timer(){
#if USE_GTK_VISUAL
    connect( this, SIGNAL(timeout()), this, SLOT(call_GTK_HandleEvents()));
    start( 10, FALSE );
#endif
  }
public slots:
  void call_GTK_HandleEvents(){
#if USE_GTK_VISUAL
    GTK_HandleEvents();
    //Qt_EventHandler();
#endif
  }
};

#include "mQt_colors.cpp"

void GFX_ResetColors(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *editorwidget = static_cast<EditorWidget*>(window->os_visual.widget);

  clear_config_colors();

  //setEditorColors(editorwidget); // read back from file.

  //system_color->setRgb(QColor(SETTINGS_read_qstring("system_color","#d2d0d5")).rgb());
  //button_color->setRgb(QColor(SETTINGS_read_qstring("button_v2_color","#c1f1e3")).rgb());
  updateAll(editorwidget);
  GFX_update_current_instrument_widget();

  window->must_redraw = true;
}

void GFX_ResetColor(enum ColorNums colornum){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *editorwidget = static_cast<EditorWidget*>(window->os_visual.widget);

  clear_config_color(colornum);

  updateAll(editorwidget);
  GFX_update_current_instrument_widget();

  window->must_redraw = true;
}

void GFX_SaveColors(filepath_t filename){
  if (isLegalFilepath(filename)){
    SETTINGS_set_custom_configfile(filename);
  }
  
  for(int i=START_CONFIG_COLOR_NUM;i<END_CONFIG_COLOR_NUM;i++) {
    if (is_qcolor_separator(i))
      continue;
    
    const char *colorname = get_color_config((enum ColorNums)i).config_name;
    
    if (!QString(colorname).contains("color"))
      colorname = talloc_format("%s_color", colorname);

    SETTINGS_write_string(colorname, get_qcolor((enum ColorNums)i).name(QColor::HexArgb));
  }

  if (isLegalFilepath(filename))
    SETTINGS_unset_custom_configfile();
}
  

static void setDefaultColors(struct Tracker_Windows *tvisual, QString configfilename){
  EditorWidget *editorwidget=(EditorWidget *)tvisual->os_visual.widget;

  clear_config_colors();
   
  QFile::remove(STRING_get_qstring(OS_get_config_filename("color0").id) + "_old");
  QFile::rename(STRING_get_qstring(OS_get_config_filename("color0").id), STRING_get_qstring(OS_get_config_filename("color0").id) + "_old");
  QFile::copy(STRING_get_qstring(OS_get_full_program_file_path(configfilename).id), STRING_get_qstring(OS_get_config_filename("color0").id));

  //setEditorColors(editorwidget); // read back from file.
  //system_color->setRgb(QColor(SETTINGS_read_qstring("system_color","#d2d0d5")).rgb());
  //button_color->setRgb(QColor(SETTINGS_read_qstring("button_v2_color","#c1f1e3")).rgb());

  updateAll(editorwidget);
  tvisual->must_redraw = true;
}


void GFX_SetDefaultColors1(struct Tracker_Windows *tvisual){
  setDefaultColors(tvisual,"colors");
}


void GFX_SetDefaultColors2(struct Tracker_Windows *tvisual){
  setDefaultColors(tvisual,"colors2");
  printf("hepp\n");
}
