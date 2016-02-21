
#include <unistd.h>

#include <QTime>
#include <QTimer>
#include <QMessageBox>
#include <QMainWindow>
#if 0
  #include <QFutureWatcher>
  #include <QFuture>
  #include <QtConcurrentRun>
#endif
#include <QApplication>

#include "../common/nsmtracker.h"
#include "../common/disk.h"
#include "../common/disk_save_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/undo.h"

#include "../api/api_proc.h"

#include "Qt_AutoBackups_proc.h"


extern QMainWindow *g_main_window;

extern struct Root *root;


static QTime g_backuptime;

static uint64_t g_undo_generation_for_last_backup = -1;

static wchar_t *get_backup_filename(void){
  return STRING_append(dc.filename, STRING_create("_automatic_backup.rad"));
}

static int64_t get_backup_interval_ms(void){
  return autobackupIntervalInMinutes()*60*1000;
  //return 5000;
}

static void aiai(void){
  printf("            STARTING TO aiai\n");

  wchar_t *backup_filename = get_backup_filename();
  Save_Backup(backup_filename, root);
  
  //usleep(1000*500);

  printf("            FINISHED aiai\n");
}


namespace{

class BackupTimer : public QTimer { // stupid workaround

  QMessageBox msgBox;

public:
  BackupTimer()
    : msgBox(g_main_window)
  {
    msgBox.setText("Please wait. Saving backup");
    msgBox.setStandardButtons(0);
    msgBox.show();
    
    setInterval(500);
    setSingleShot(true);
    start();
  }
protected:

  void 	timerEvent ( QTimerEvent * e ){
    aiai();
    printf("hello");
    delete this;
  }
};
}


static void make_backup(void){
  if (Undo_num_undos() == 0)
    return;

  if (dc.filename==NULL)
    return;

  if (g_undo_generation_for_last_backup == g_curr_undo_generation)
    return;

  // Set this immediately so we don't start several BackupTimers.
  g_undo_generation_for_last_backup = g_curr_undo_generation;

  //QMessageBox msgBox(g_main_window);

  // This was a bad idea. QFuture starts a new thread. It's completely ridiculous that there is no safe and easy way to show a "please wait" window in Qt. (could start a new process though)
#if 0
  QFutureWatcher<void> watcher;
  msgBox.connect(&watcher, SIGNAL(finished()), &msgBox, SLOT(hide()));
  QFuture<void> future = QtConcurrent::run(aiai);
  watcher.setFuture(future);    
  
  obtain_keyboard_focus();
  msgBox.exec();
  release_keyboard_focus();
#endif

  // Anyway, the almost always working workaround:
  new BackupTimer();
  
  //printf("               BACKUP finished\n");  
}

static double g_time = 0;
static double g_curr_playing_start_time = 0;
static double g_curr_playing_duration = 0;

void RT_BACKUP_reset_timer(void){
  g_time = TIME_get_ms();
  g_curr_playing_start_time = 0;
  g_curr_playing_duration = 0;
}

static double get_unbackuped_duration(void){
  double total = TIME_get_ms() - g_time;
  return total - g_curr_playing_duration;
}

void BACKUP_call_very_often(void){
  static bool has_inited = false;
  if (has_inited==false){
    RT_BACKUP_reset_timer();
    has_inited=true;
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

  if (is_playing)
    return;
  
  //printf("duration: %f\n",get_unbackuped_duration() / 1000.0);
  
  if (get_unbackuped_duration()  > get_backup_interval_ms()){
    make_backup();
    RT_BACKUP_reset_timer();
  }
}
