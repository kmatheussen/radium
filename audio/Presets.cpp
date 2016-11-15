
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


static hash_t *g_preset_clipboard = NULL;


static QString g_last_filename;
static QString g_last_preset_path = "";


void PRESET_set_last_used_filename(const wchar_t *wfilename){
  QString filename = STRING_get_qstring(wfilename);
  
  g_last_preset_path = QFileInfo(filename).absoluteDir().path();
  g_last_filename = QFileInfo(filename).baseName();
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

  bool need_separator = ret.size() > 0;
  bool pushed_separator=false;
      
  for (int i = 0; i < list.size(); ++i) {
    QFileInfo file_info = list.at(i);
    QString filename = file_info.fileName();
    if (filename.endsWith(".mrec")) {
      if (need_separator && pushed_separator==false) {
        ret.push_back("-----");
        pushed_separator = true;
      }
      ret.push_back(filename);
    }
  }

  return ret;
}


static QString request_load_preset_filename_from_requester(void){
  QString filename;

  R_ASSERT_RETURN_IF_FALSE2(g_radium_runs_custom_exec==false, "");
  g_radium_runs_custom_exec = true;

  obtain_keyboard_focus();{

    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getOpenFileName(
                                              g_main_window,
                                              "Load Effect configuration",
                                              g_last_preset_path,
#if FOR_WINDOWS
                                              "*.rec *.mrec ;; *.rec ;; *.mrec ;; All files (*)",
#else
                                              "Radium Effect Configuration (*.rec *.mrec) ;; Radium Single Effect Configuration (*.rec) ;; Radium Multi Effect Configuration (*.mrec) ;; All files (*)",
#endif
                                              0,
                                              QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                              );
    }GL_unlock();
    
  }release_keyboard_focus();

  g_radium_runs_custom_exec = false;
  
  return filename;
}

static QString request_load_preset_filename(void){
  QString filename;

  if (g_last_preset_path=="")
    g_last_preset_path = QCoreApplication::applicationDirPath();
  
  QVector<QString> existing_presets = get_all_presets_in_path(g_last_preset_path);
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
    return g_last_preset_path + QDir::separator() + existing_presets[sel-start];
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
  g_last_preset_path = QFileInfo(filename).absoluteDir().path();
  
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

  g_last_filename = QFileInfo(filename).baseName();

  return state;
}

static int64_t PRESET_load_multipreset(hash_t *state, char *name, bool inc_usage_number, float x, float y){

  struct Patch *first_patch = NULL;
  vector_t patches = {0};
  
  hash_t *patches_state = HASH_get_hash(state, "patches");
  int num_presets = HASH_get_array_size(patches_state);
  
  for(int i = 0 ; i < num_presets ; i++) {
    hash_t *patch_state = HASH_get_hash_at(patches_state, "patch", i);
    struct Patch *patch = PATCH_create_audio(NULL, NULL, name, patch_state, 0, 0);
    //printf("name1: -%s-, name2: -%s-, name3: %s\n",name,patch->name,HASH_get_chars(patch_state,"name"));
    //getchar();
    VECTOR_push_back(&patches, patch); // NULL values must be pushed as well.
    
    if (patch!=NULL){

      if (first_patch == NULL)
        first_patch = patch;
      
      if (inc_usage_number){
        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        inc_plugin_usage_number(plugin->type);
      }
    }
  }

  MW_create_from_state(HASH_get_hash(state, "mixer_state"),
                       &patches,
                       x, y);

  R_ASSERT(first_patch != NULL);
  
  if (first_patch==NULL)
    return -1;
  else
    return first_patch->id;
}

static int64_t PRESET_load_singlepreset(hash_t *state, char *name, bool inc_usage_number, float x, float y){
  struct Patch *patch = PATCH_create_audio(NULL, NULL, name, state, x, y);
  if (patch==NULL)
    return -1;

  if (inc_usage_number){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    inc_plugin_usage_number(plugin->type);
  }

  return patch->id;
}

static int64_t insert_preset_into_program(hash_t *state, char *name, bool inc_usage_number, float x, float y){
  bool is_multipreset = HASH_has_key(state, "multipreset_presets") && HASH_get_bool(state, "multipreset_presets");
  
  if (is_multipreset)
    return PRESET_load_multipreset(state, name, inc_usage_number, x, y);
  else
    return PRESET_load_singlepreset(state, name, inc_usage_number, x, y);
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

  return insert_preset_into_program(state, name, inc_usage_number, x, y);
}

int64_t PRESET_paste(float x, float y){
  if (g_preset_clipboard != NULL)
    return insert_preset_into_program(g_preset_clipboard, NULL, true, x, y);
  else
    return -1;
}

bool PRESET_has_copy(void){
  return g_preset_clipboard != NULL;
}


/****************************************/
/************** SAVE ********************/
/****************************************/

hash_t *get_preset_state(vector_t *patches){
  bool is_multipreset = patches->num_elements > 1;

  hash_t *state;
  
  if (is_multipreset) {
    
    state = HASH_create(5);
    
    HASH_put_bool(state, "multipreset_presets", true);
    
    HASH_put_hash(state, "patches", PATCHES_get_state(patches, true));

    {
      hash_t *mixer_state = MW_get_state(patches, false);
      
      HASH_put_hash(state, "mixer_state", mixer_state);
    }
    
  } else {
    
    struct Patch *patch = (struct Patch*)patches->elements[0];
    state = PATCH_get_state(patch);
    
  }

  return state;
}

static bool valid_patches(vector_t *patches){
  VECTOR_FOR_EACH(struct Patch*, patch, patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (!strcmp(plugin->type->type_name,"Bus")){
      GFX_Message(NULL, "Can not copy or save Bus preset");
      return false;
    }
    
    if (AUDIO_is_permanent_patch(patch)){
      GFX_Message(NULL, "Can not copy or save Main Pipe preset");
      return false;
    }
  }END_VECTOR_FOR_EACH;

  return true;
}

void PRESET_copy(vector_t *patches){
  R_ASSERT_RETURN_IF_FALSE(patches->num_elements > 0);

  if (!valid_patches(patches))
    return;

  g_preset_clipboard = get_preset_state(patches);
}
  
void PRESET_save(vector_t *patches, bool save_button_pressed){  // "save_button_pressed is the "Save" button in the instrument window.

  if(patches->num_elements==0){
    GFX_Message(NULL, "No instruments selected");
    return;
  }

  if (!valid_patches(patches))
    return;
  
  bool is_multipreset = patches->num_elements > 1;

  obtain_keyboard_focus();

  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    filename = QFileDialog::getSaveFileName(
                                            g_main_window,
                                            "Save Effect configuration",
                                            g_last_preset_path,
#if FOR_WINDOWS
                                            is_multipreset
                                             ? "*.mrec ;; All files (*)"
                                             : "*.rec ;; All files (*)",
#else
                                            is_multipreset
                                             ? "Radium Multi Effect Configuration (*.mrec) ;; All files (*)"
                                             : "Radium Effect Configuration (*.rec) ;; All files (*)",
#endif
                                            0,
                                            QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                            );
  }GL_unlock();

  release_keyboard_focus();
  
  if(filename=="")
    return;

  disk_t *file = DISK_open_for_writing(filename);
  
  if(file==NULL){
    MyQMessageBox msgBox;
    msgBox.setText("Could not save file.");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return;
  }

  g_last_preset_path = QFileInfo(filename).absoluteDir().path();

  hash_t *state = get_preset_state(patches);
  
  HASH_save(state, file);
  
  DISK_close_and_delete(file);
}


