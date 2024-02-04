#include "Qt_SaveRestoreWindows.h"

#include "../common/nsmtracker.h"
#include "../common/sequencer_proc.h"
#include "../common/settings_proc.h"
#include "../common/visual_proc.h"
#include "Qt_sequencer_proc.h"
#include "../api/api_proc.h"
#include "../api/api_gui_proc.h"
#include "../api/api_various_proc.h"
#include "../api/api_instruments_proc.h"
#include "../api/api_midi_proc.h"
#include "../api/api_common_proc.h"

void saveWindowsState(QWidget * mainWindow) {
    printf("\n ***Saving windows state*** \n");
    SETTINGS_write_bool("windows_settings_saved", true);

    SETTINGS_write_bool("main_window_maximized", mainWindow->isMaximized());
    SETTINGS_write_int("main_window_x", mainWindow->pos().x());
    SETTINGS_write_int("main_window_y", mainWindow->pos().y());
    SETTINGS_write_int("main_window_width", mainWindow->size().width());
    SETTINGS_write_int("main_window_height", mainWindow->size().height());
    printf("Saved main window settings: %d, %d, %d, %d \n", mainWindow->pos().x(), mainWindow->pos().y(), mainWindow->size().width(), mainWindow->size().height());

    SETTINGS_write_bool("sequencer_in_own_window", sequencerInWindow());
    if (sequencerInWindow())
    {
      printf("Sequencer in own window saving settings \n");
      QWidget * sequencerWindow = SEQUENCER_getWidget()->window();
      // When using configureSequencerWidget() we need be sure that 
      // second parameter has proper value
      SETTINGS_write_bool("position_sequencer_widget_in_mixer", false);
      SETTINGS_write_bool("sequencer_window_maximized", sequencerWindow->isMaximized());
      SETTINGS_write_int("sequencer_window_x", sequencerWindow->pos().x());
      SETTINGS_write_int("sequencer_window_y", sequencerWindow->pos().y());
      SETTINGS_write_int("sequencer_window_width", sequencerWindow->size().width());
      SETTINGS_write_int("sequencer_window_height", sequencerWindow->size().height());
      printf("Saved sequencer settings: %d, %d, %d, %d \n", sequencerWindow->pos().x(), sequencerWindow->pos().y(), sequencerWindow->size().width(), sequencerWindow->size().height());
    }
    
    SETTINGS_write_bool("main_mixer_in_own_window", mainMixerInWindow());
    if (mainMixerInWindow())
    {
      printf("Mixer in own window saving settings \n");
      QWidget * mixerWindow =  API_gui_get_widget(gui_getMainMixerGui())->window();
      SETTINGS_write_bool("main_mixer_window_maximized", mixerWindow->isMaximized());
      SETTINGS_write_int("main_mixer_window_x", mixerWindow->pos().x());
      SETTINGS_write_int("main_mixer_window_y", mixerWindow->pos().y());
      SETTINGS_write_int("main_mixer_window_width", mixerWindow->size().width());
      SETTINGS_write_int("main_mixer_window_height", mixerWindow->size().height());
      printf("Saved mixer settings: %d, %d, %d, %d \n", mixerWindow->pos().x(), mixerWindow->pos().y(), mixerWindow->size().width(), mixerWindow->size().height());
    }
}

void restoreWindowsState(QWidget * mainWindow) {
  if (!SETTINGS_read_bool("windows_settings_saved", false))
    return;

  printf("\n ***loading windows state*** \n");
  // sequnecer window  
  bool inOwnWindow = SETTINGS_read_bool("sequencer_in_own_window", false);
  if (inOwnWindow)
  {
    printf("Sequencer in own window loading settings \n");
    // When using configureSequencerWidget() we need be sure that 
    // second parameter has proper value
    SETTINGS_write_bool("position_sequencer_widget_in_mixer", false);
    configureSequencerWidget(true, true);

    bool maximized = SETTINGS_read_bool("sequencer_window_maximized", false);
    int x = SETTINGS_read_int("sequencer_window_x", 0);
    int y = SETTINGS_read_int("sequencer_window_y", 0);
    int width = SETTINGS_read_int("sequencer_window_width", 1024);
    int height = SETTINGS_read_int("sequencer_window_height", 550);
    printf("Loaded sequencer settings: %d, %d, %d, %d \n", x, y, width, height);
    QWidget * sequencerWindow = SEQUENCER_getWidget()->window();
    sequencerWindow->setGeometry(x, y, width, height);
    if (maximized)
      sequencerWindow->showMaximized();
  }

  inOwnWindow = SETTINGS_read_bool("main_mixer_in_own_window", false);
  if (inOwnWindow)
  {
    printf("Mixer in own window loading settings \n");
    setMainMixerInWindow(true);
    GFX_ShowMixer();
    QWidget * mixerWindow =  API_gui_get_widget(gui_getMainMixerGui())->window();
    bool maximized = SETTINGS_read_bool("main_mixer_window_maximized", false);
    int x = SETTINGS_read_int("main_mixer_window_x", 0);
    int y = SETTINGS_read_int("main_mixer_window_y", 0);
    int width = SETTINGS_read_int("main_mixer_window_width", 1024);
    int height = SETTINGS_read_int("main_mixer_window_height", 550);
    printf("Loaded mixer settings: %d, %d, %d, %d \n", x, y, width, height);
    
    mixerWindow->setGeometry(x, y, width, height);
    if (maximized)
      mixerWindow->showMaximized();
  }

  printf("Loading main window state \n");
  bool maximized = SETTINGS_read_bool("main_window_maximized", false);
  int x = SETTINGS_read_int("main_window_x", 0);
  int y = SETTINGS_read_int("main_window_y", 0);
  int width = SETTINGS_read_int("main_window_width", 1024);
  int height = SETTINGS_read_int("main_window_height", 550);
  printf("Loaded main window settings: %d, %d, %d, %d \n", x, y, width, height);
  mainWindow->setGeometry(x, y, width, height);
  if (maximized)
    mainWindow->showMaximized();
}
