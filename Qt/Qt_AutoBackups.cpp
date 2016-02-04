
#include <QTime>

#include "../common/nsmtracker.h"
#include "../common/disk.h"
#include "../common/disk_save_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/undo.h"

#include "../api/api_proc.h"

#include "Qt_AutoBackups_proc.h"


extern struct Root *root;


static QTime g_backuptime;

static wchar_t *get_backup_filename(void){
  return STRING_append(dc.filename, STRING_create("_automatic_backup.rad"));
}

static int64_t get_backup_interval_ms(void){
  return autobackupIntervalInMinutes()*60*1000;
}

static void make_backup(void){
  if (Undo_num_undos_since_last_save() == 0)
    return;

  if (dc.filename==NULL)
    return;

  wchar_t *backup_filename = get_backup_filename();
  Save_Backup(backup_filename, root);
}

void BACKUP_call_very_often(void){
  static bool has_inited = false;
  if (has_inited==false){
    g_backuptime.start();
    has_inited=true;
  }

  if (ATOMIC_GET(pc->player_state)!=PLAYER_STATE_STOPPED)
    return;
  
  if (g_backuptime.elapsed() > get_backup_interval_ms()){
    make_backup();
    g_backuptime.restart();
  }
}
