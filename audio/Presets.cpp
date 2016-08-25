
#include <QVector>
#include <QString>
#include <QFileInfo>
#include <QDir>
#include <QFileDialog>
#include <QMainWindow>
#include <QCoreApplication>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/patch_proc.h"

#include "../Qt/helpers.h"
#include "../Qt/EditorWidget.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../OpenGL/Widget_proc.h"

#include "SoundPlugin.h"
#include "audio_instrument_proc.h"

#include "Presets_proc.h"



static QString last_filename;
static QString last_preset_path = "";


void PRESET_set_last_used_filename(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  last_preset_path = QFileInfo(filename).absoluteDir().path();
  last_filename = QFileInfo(filename).baseName();
}



/****************************************/
/************** LOAD ********************/
/****************************************/


static QVector<QString> get_all_presets_in_path(QString path){
  QVector<QString> ret;
  
  QDir dir(path);
  dir.setSorting(QDir::Name);
  QFileInfoList list = dir.entryInfoList();
  for (int i = 0; i < list.size(); ++i) {
    QFileInfo file_info = list.at(i);
    QString filename = file_info.fileName();
    if (filename.endsWith(".rec"))
      ret.push_back(filename);
  }

  return ret;
}


static QString request_load_preset_filename_from_requester(void){
  QString filename;
  
  obtain_keyboard_focus();{

    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getOpenFileName(
                                              g_main_window,
                                              "Load Effect configuration",
                                              last_preset_path,
#if FOR_WINDOWS
                                              "*.rec ;; All files (*)",
#else
                                              "Radium Effect Configuration (*.rec) ;; All files (*)",
#endif
                                              0,
                                              useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                              );
    }GL_unlock();
    
  }release_keyboard_focus();

  return filename;
}

static QString request_load_preset_filename(void){
  QString filename;

  if (last_preset_path=="")
    last_preset_path = QCoreApplication::applicationDirPath();
  
  QVector<QString> existing_presets = get_all_presets_in_path(last_preset_path);
  if (existing_presets.size()==0)
    return request_load_preset_filename_from_requester();

  vector_t v = {};
  
  int request_from_requester = VECTOR_push_back(&v, "Select preset from a different directory");
  VECTOR_push_back(&v, "------------");

  int start = v.num_elements;
  
  for(QString filename : existing_presets)
    VECTOR_push_back(&v, talloc_strdup(filename.toUtf8().constData()));

  int sel = GFX_Menu(NULL, NULL, "", &v);

  if (sel==-1)
    return "";
  else if (sel==request_from_requester)
    return request_load_preset_filename_from_requester();
  else
    return last_preset_path + QDir::separator() + existing_presets[sel-start];
}

static const char *request_load_preset_encoded_filename(void){
  QString filename = request_load_preset_filename();
  if (filename=="")
    return NULL;

  return STRING_get_chars(STRING_toBase64(STRING_create(filename)));
}

char *PRESET_request_load_instrument_description(void){
  const char *encoded_filename = request_load_preset_encoded_filename();  // Converting to base64 to avoid having to worry about utf8 conversion problems in filenames.
  if (encoded_filename==NULL)
    return talloc_strdup("");

  return talloc_format("2%s",encoded_filename);
}


static hash_t *get_preset_state_from_filename(QString filename){
  last_preset_path = QFileInfo(filename).absoluteDir().path();
  
  disk_t *file = DISK_open_for_reading(filename);
  if(file==NULL){
    MyQMessageBox msgBox;
    msgBox.setText("Could not open file.");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return NULL;
  }

  hash_t *state = HASH_load(file);
  if (DISK_close_and_delete(file)==false)
    return NULL;

  if(state==NULL){
    MyQMessageBox msgBox;
    msgBox.setText("File does not appear to be a valid effects settings file");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return NULL;
  }

  last_filename = QFileInfo(filename).baseName();

  return state;
}

// Note that this is the general preset loading function, and not the one that is directly called when pressing the "Load" button. (there we also have to delete the old instrument and reconnect connections)
//
// A less confusing name could perhaps be PRESET_add_instrument
//
int64_t PRESET_load(const wchar_t *filename, char *name, bool inc_usage_number, float x, float y) {
  if (name!=NULL && strlen(name)==0)
    name = NULL;

  hash_t *state = get_preset_state_from_filename(STRING_get_qstring(filename));
  if (state==NULL)
    return -1;
  
  PRESET_set_last_used_filename(filename);

  vector_t patch_states = {0};
  
  if (HASH_has_key(state, "multipreset_presets")) {
    
    hash_t *patches_state = HASH_get_hash(state, "patches");
      
    int num_presets = HASH_get_array_size(patches_state);
    for(int i = 0 ; i < num_presets ; i++)
      VECTOR_push_back(&patch_states, HASH_get_hash_at(patches_state, "patch", i));
    
  }else{
    VECTOR_push_back(&patch_states, state);
  }

  struct Patch *first_patch = NULL;
  vector_t patches = {0};
  
  VECTOR_FOR_EACH(hash_t *, patch_state, &patch_states){

    struct Patch *patch = PATCH_create_audio(NULL, NULL, name, patch_state);
    VECTOR_push_back(&patches, patch);
    
    if (patch!=NULL){

      if (first_patch == NULL)
        first_patch = patch;
      
      if (inc_usage_number){
        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        inc_plugin_usage_number(plugin->type);
      }
    }
      
  }END_VECTOR_FOR_EACH;

  if (HASH_has_key(state, "mixer_state"))
    MW_create_from_state(HASH_get_hash(state, "mixer_state"),
                         &patches,
                         x, y);

  if (first_patch==NULL)
    return -1;
  else
    return first_patch->id;
}



/****************************************/
/************** SAVE ********************/
/****************************************/



void PRESET_save(vector_t *patches, bool save_button_pressed){

  if(patches->num_elements==0){
    GFX_Message(NULL, "No instruments selected");
    return;
  }

  {
    VECTOR_FOR_EACH(struct Patch*, patch, patches){
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      if (!strcmp(plugin->type->type_name,"Bus")){
        GFX_Message(NULL, "Can not save Bus preset");
        return;
      }
      
      if (AUDIO_is_permanent_patch(patch)){
        GFX_Message(NULL, "Can not save Main Pipe preset");
        return;
      }
    }END_VECTOR_FOR_EACH;
  }
  
  obtain_keyboard_focus();

  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    filename = QFileDialog::getSaveFileName(
                                            g_main_window,
                                            "Save Effect configuration",
                                            last_preset_path,
#if FOR_WINDOWS
                                            "*.rec ;; All files (*)",
#else
                                            "Radium Effect Configuration (*.rec) ;; All files (*)",
#endif
                                            0,
                                            useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                            );
  }GL_unlock();

  release_keyboard_focus();
  
  if(filename=="")
    return;

  last_preset_path = QFileInfo(filename).absoluteDir().path();
    
  disk_t *file = DISK_open_for_writing(filename);
  
  if(file==NULL){
    MyQMessageBox msgBox;
    msgBox.setText("Could not save file.");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return;
  }

  hash_t *state;
  
  if (save_button_pressed) {
    struct Patch *patch = (struct Patch*)patches->elements[0];
    state = PATCH_get_state(patch);
  } else {
    state = HASH_create(5);
    
    HASH_put_bool(state, "multipreset_presets", true);

    {
      hash_t *patches_state = HASH_create(patches->num_elements);
      for(int i = 0 ; i < patches->num_elements ; i++){
        struct Patch *patch = (struct Patch*)patches->elements[i];
        HASH_put_hash_at(patches_state, "patch", i, PATCH_get_state(patch));
      }

      HASH_put_hash(state, "patches", patches_state);
    }

    {
      hash_t *mixer_state = MW_get_state(patches);

      HASH_put_hash(state, "mixer_state", mixer_state);
    }
  }
  
  HASH_save(state, file);
  
  DISK_close_and_delete(file);
}


