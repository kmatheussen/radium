
#include <unistd.h>

#include <QTime>
#include <QApplication>

#include "../common/nsmtracker.h"
#include "../common/disk.h"
#include "../common/disk_save_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/undo.h"

#include "../OpenGL/Render_proc.h"

#include "../audio/SampleRecorder_proc.h"

#include "../api/api_proc.h"


#include "helpers.h"

#include "Qt_AutoBackups_proc.h"


extern struct Root *root;


//static QTime g_backuptime;

static int64_t g_undo_generation_for_last_backup = -1;

static filepath_t get_backup_filename(void){
  return make_filepath(STRING_append(dc.filename.id, L"_automatic_backup.rad"));
}

static int64_t get_backup_interval_ms(void){
  return autobackupIntervalInMinutes()*60*1000;
  //return 5000;
}

static bool is_demo_song(void){
  return STRING_starts_with2(DISK_get_absolute_file_path(dc.filename).id, OS_get_program_path2().id);
}

static void make_backup(void){
  RETURN_IF_DATA_IS_INACCESSIBLE_SAFE2();
  
  if (Undo_num_undos() == 0)
    return;

  if (isIllegalFilepath(dc.filename))
    return;

  if (is_demo_song())
    return;
  
  if (g_undo_generation_for_last_backup == g_curr_undo_generation)
    return;
  
  // Set this immediately so we don't start several BackupTimers.
  g_undo_generation_for_last_backup = g_curr_undo_generation;

  root->song->tracker_windows->message = "Please wait. Saving backup";
  GL_create(root->song->tracker_windows);

  filepath_t backup_filename = get_backup_filename();
  Save_Backup(backup_filename, root);

  root->song->tracker_windows->message = NULL;
  GL_create(root->song->tracker_windows);
    
  //printf("               BACKUP finished\n");  
}

static double g_time = 0;
static double g_curr_playing_start_time = 0;
static double g_curr_playing_duration = 0;

static void RT_BACKUP_reset_timer(void){
  g_time = TIME_get_ms();
  g_curr_playing_start_time = 0;
  g_curr_playing_duration = 0;
}

static double get_unbackuped_duration(void){
  double total = TIME_get_ms() - g_time;
  if (doSaveBackupWhilePlaying())
      return total;
  else
    return total - g_curr_playing_duration;
}

void BACKUP_call_very_often(void){
  if (MIXER_is_saving())
    return;

  if (g_radium_runs_custom_exec)
    return;
  
  static bool has_inited = false;
  if (has_inited==false){
    RT_BACKUP_reset_timer();
    has_inited=true;
  }

  if (!editor_has_keyboard_focus()) // If showing popup menu, editing text widgets, etc. we don't want to disturb the user.
    return;

  // Wait until we are sure the user doesn't use the mouse.
  {
    if (QApplication::mouseButtons() != Qt::NoButton)
      return;

    if (MOUSE_CYCLE_get_mouse_buttons() != Qt::NoButton)
      return;
  }
  
  static bool is_playing = false;
  
  Player_State player_state = ATOMIC_GET(pc->player_state);
  
  if (!is_playing && player_state==PLAYER_STATE_PLAYING){
    is_playing = true;
    g_curr_playing_start_time = TIME_get_ms();
  }

  if (is_playing && player_state==PLAYER_STATE_STOPPED){
    is_playing = false;
    double added_playing_duration = TIME_get_ms() - g_curr_playing_start_time;
    g_curr_playing_duration += added_playing_duration;
  }

  if (!doSaveBackupWhilePlaying())
    if (is_playing)
      return;
  
  //printf("duration: %f\n",get_unbackuped_duration() / 1000.0);

  if (SampleRecorder_Get_Num_Instances() > 0)
    return;

  if (get_unbackuped_duration()  > get_backup_interval_ms()){
    make_backup();
    RT_BACKUP_reset_timer();
  }
}
