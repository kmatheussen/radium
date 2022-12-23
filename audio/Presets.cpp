
#include <inttypes.h>
#include <QVector>
#include <QString>
#include <QFileInfo>
#include <QDir>
#include <QFileDialog>
#include <QCoreApplication>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/patch_proc.h"
#include "../common/settings_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../Qt/helpers.h"
#include "../Qt/EditorWidget.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../OpenGL/Widget_proc.h"

#include "SoundPlugin.h"
#include "SoundPluginRegistry_proc.h"
#include "audio_instrument_proc.h"
#include "Seqtrack_plugin_proc.h"

#include "../api/api_gui_proc.h"


#include "Presets_proc.h"



static hash_t *g_preset_clipboard = NULL;


static QString g_last_filename;
static QString g_last_preset_path = "";


static void PRESET_set_last_used_filename(QString filename){  
  g_last_preset_path = QFileInfo(filename).absoluteDir().path();
  g_last_filename = QFileInfo(filename).baseName();
}

void PRESET_set_last_used_filename(filepath_t wfilename){
  R_ASSERT_RETURN_IF_FALSE(isLegalFilepath(wfilename));
  QString filename = STRING_get_qstring(wfilename.id);
  PRESET_set_last_used_filename(filename);
}

filepath_t PRESET_get_current_preset_dir(void){
  if (g_last_preset_path==""){    
    return make_filepath(SETTINGS_read_qstring("preset_root_folder", QDir::homePath() + QString::fromUtf8("/Radium Presets")));
  }
  else
    return make_filepath(g_last_preset_path);  
}


/****************************************/
/************** LOAD ********************/
/****************************************/

static dynvec_t get_all_files_in_path(filepath_t wpath, QString ends_with){
  dynvec_t ret = {};

  if (g_last_preset_path=="")
    g_last_preset_path = QCoreApplication::applicationDirPath();

  QString path = isIllegalFilepath(wpath) ? g_last_preset_path : STRING_get_qstring(wpath.id);
  
  QDir dir(path);
  dir.setSorting(QDir::Name);
  QFileInfoList list = dir.entryInfoList();
  //printf("         LIST size: %d. Path: \"%s\"\n", list.size(), path.toUtf8().constData());
  for (int i = 0; i < list.size(); ++i) {
    QFileInfo file_info = list.at(i);
    QString filename = file_info.absoluteFilePath();
    //printf("              filename: -%s-\n", filename.toUtf8().constData());
    if (filename.endsWith(ends_with))
      DYNVEC_push_back(&ret, DYN_create_filepath(make_filepath(filename)));
  }

  return ret;
}

dynvec_t PRESET_get_all_rec_files_in_path(filepath_t wpath){
  return get_all_files_in_path(wpath, ".rec");
}


dynvec_t PRESET_get_all_mrec_files_in_path(filepath_t wpath){
  return get_all_files_in_path(wpath, ".mrec");
}



static void request_load_preset_filename_from_requester(int64_t parentgui, func_t *callback){
  R_ASSERT_RETURN_IF_FALSE(g_radium_runs_custom_exec==false);

  QString filename;

  QWidget *parentWidget = API_gui_get_parentwidget(NULL, parentgui);
  
  {
    radium::ScopedExec scopedExec(true);

    filename = QFileDialog::getOpenFileName(
                                            parentWidget,
                                            "Load Effect configuration",
                                            g_last_preset_path,
#ifdef FOR_WINDOWS
                                            "*.rec *.mrec ;; *.rec ;; *.mrec ;; All files (*)",
#else
                                            "Radium Effect Configuration (*.rec *.mrec) ;; Radium Single Effect Configuration (*.rec) ;; Radium Multi Effect Configuration (*.mrec) ;; All files (*)",
#endif
                                            0,
                                            QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                            );
  }

  if (!filename.isNull())
    S7CALL(void_charpointer,callback, talloc_format("2%S",STRING_toBase64(STRING_create(filename))));
}

/*
void request_load_preset_filename(func_t *callback){
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

  int sel = GFX_Menu(NULL, NULL, "", &v, true);

  if (sel==-1)
    return "";
  else if (sel==request_from_requester)
    return request_load_preset_filename_from_requester();
  else
    return g_last_preset_path + QDir::separator() + existing_presets[sel-start];
}
*/

/*
static const char *request_load_preset_encoded_filename(func_t *callback){
  QString filename = request_load_preset_filename();
  if (filename=="")
    return NULL;

  return STRING_get_chars(STRING_toBase64(STRING_create(filename)));
}
*/

void PRESET_request_load_instrument_description(int64_t parentgui, func_t *callback){
  request_load_preset_filename_from_requester(parentgui, callback);
  //request_load_preset_encoded_filename(callback);
  /*
  const char *encoded_filename = request_load_preset_encoded_filename();  // Converting to base64 to avoid having to worry about utf8 conversion problems in filenames.
  if (encoded_filename==NULL)
    return talloc_strdup("");

  return talloc_format("2%s",encoded_filename);
  */
}

static hash_t *get_preset_state_from_filename(QString filename){
  g_last_preset_path = QFileInfo(filename).absoluteDir().path();
  
  disk_t *file = DISK_open_for_reading(filename);
  if(file==NULL){
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setText("Could not open file.");
    msgBox->setStandardButtons(QMessageBox::Ok);
    msgBox->setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox, true);
    return NULL;
  }

  hash_t *state = HASH_load(file);
  if (DISK_close_and_delete(file)==false)
    return NULL;

  if(state==NULL){
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setText("File does not appear to be a valid effects settings file");
    msgBox->setStandardButtons(QMessageBox::Ok);
    msgBox->setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox, true);
    return NULL;
  }

  g_last_filename = QFileInfo(filename).baseName();

  return state;
}

static instrument_t PRESET_load_multipreset(hash_t *state, const char *name, filepath_t filename, bool inc_usage_number, bool set_as_current, bool is_visible, float x, float y){

  QHash<instrument_t, instrument_t> patch_id_mapper;
  
  struct Patch *first_patch = NULL;
  vector_t patches = {};
  
  hash_t *patches_state = HASH_get_hash(state, "patches");
  int num_presets = HASH_get_array_size(patches_state, "patch");
  
  for(int i = 0 ; i < num_presets ; i++) {
    hash_t *patch_state = HASH_get_hash_at(patches_state, "patch", i);
    struct Patch *patch = PATCH_create_audio(NULL, NULL, name, patch_state, set_as_current ? i==0 : false, is_visible, 0, 0);
    //printf("i: %d. Org id: %d. New id: %d. name1: -%s-, name2: -%s-, name3: %s\n",i, (int)HASH_get_instrument(patch_state, "id").id, (int)patch->id.id, name,patch->name,HASH_get_chars(patch_state,"name"));
    //getchar();
    VECTOR_push_back(&patches, patch); // NULL values must be pushed as well.

    patch_id_mapper[HASH_get_instrument(patch_state, "id")] = patch->id;
    
    if (patch!=NULL){

      if (first_patch == NULL)
        first_patch = patch;

      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      if(plugin != NULL) {

        plugin->preset_filename = make_filepath(V_wcsdup(filename.id));
      
        if (inc_usage_number)
          PR_inc_plugin_usage_number(plugin->type);
          
      } else {
        
        R_ASSERT_NON_RELEASE(false);
        
      }
    }
  }

  MW_create_from_state(HASH_get_hash(state, "mixer_state"),
                       &patches,
                       patch_id_mapper,
                       x, y);

  R_ASSERT(first_patch != NULL);
  
  if (first_patch==NULL)
    return createIllegalInstrument();
  else
    return first_patch->id;
}

static instrument_t PRESET_load_singlepreset(hash_t *state, const_char *name, filepath_t filename, bool inc_usage_number, bool set_as_current, bool is_visible, float x, float y){

  if (set_as_current){
    R_ASSERT_NON_RELEASE(is_visible);
  }

  struct Patch *patch = PATCH_create_audio(NULL, NULL, name, state, set_as_current, is_visible, x, y);
  if (patch==NULL)
    return createIllegalInstrument();

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  if (plugin != NULL){

    plugin->preset_filename = make_filepath(V_wcsdup(filename.id));
    
    if (inc_usage_number)
      PR_inc_plugin_usage_number(plugin->type);
    
  } else {
    
    R_ASSERT_NON_RELEASE(false);
    
  }

  return patch->id;
}

static instrument_t insert_preset_into_program(hash_t *state, const_char *name, filepath_t filename, bool inc_usage_number, bool set_as_current, bool is_visible, float x, float y){

  if (set_as_current){
    R_ASSERT_NON_RELEASE(is_visible);
  }

  bool is_multipreset = HASH_has_key(state, "multipreset_presets") && HASH_get_bool(state, "multipreset_presets");
  
  if (is_multipreset)
    return PRESET_load_multipreset(state, name, filename, inc_usage_number, set_as_current, is_visible, x, y);
  else
    return PRESET_load_singlepreset(state, name, filename, inc_usage_number, set_as_current, is_visible, x, y);
}

// Note that this is the general preset loading function, and not the one that is directly called when pressing the "Load" button. (there we also have to delete the old instrument and reconnect connections)
//
// A less confusing name could perhaps be PRESET_add_instrument
//
instrument_t PRESET_load(filepath_t wfilename, const_char *patchname, bool inc_usage_number, bool set_as_current, bool is_visible, float x, float y) {

  if (set_as_current){
    R_ASSERT_NON_RELEASE(is_visible);
  }

  if (patchname!=NULL && strlen(patchname)==0)
    patchname = NULL;

  QString filename = STRING_get_qstring(wfilename.id);
  QFileInfo info(filename);

  if (info.isAbsolute()==false && g_last_preset_path!="")
    filename = g_last_preset_path + QDir::separator() + filename;
  
  hash_t *state = get_preset_state_from_filename(filename);
  if (state==NULL)
    return createIllegalInstrument();
  
  PRESET_set_last_used_filename(filename);

  return insert_preset_into_program(state, patchname, make_filepath(filename), inc_usage_number, set_as_current, is_visible, x, y);
}

instrument_t PRESET_paste(float x, float y){
  if (g_preset_clipboard != NULL)
    return insert_preset_into_program(g_preset_clipboard, NULL, HASH_get_filepath(g_preset_clipboard, "filename"), true, true, true, x, y);
  else
    return make_instrument(-1);
}

bool PRESET_has_copy(void){
  return g_preset_clipboard != NULL;
}


/****************************************/
/************** SAVE ********************/
/****************************************/

static hash_t *get_preset_state(const vector_t *patches){
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

static bool valid_patches(const vector_t *patches){
  VECTOR_FOR_EACH(struct Patch*, patch, patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (plugin!=NULL){
      if (!strcmp(plugin->type->type_name,"Bus")){
        GFX_addMessage("Can not cut, copy, delete, or save a Bus preset"); // Workaround for Qt bug. Running a custom exec screws up QGraphicsScene mouse handling
        return false;
      }
      
      if (PLUGIN_is_for_seqtrack(plugin)){
        GFX_addMessage("Can not cut, copy, delete, or save a Seqtrack preset"); // Workaround for Qt bug. Running a custom exec screws up QGraphicsScene mouse handling
        return false;
      }
      
      if (AUDIO_is_permanent_patch(patch)){
        R_ASSERT_NON_RELEASE(false); // should be covered in "Bus" above.
        GFX_addMessage("Can not cut, copy, delete, or save the Main Bus preset"); // Workaround for Qt bug. Running a custom exec screws up QGraphicsScene mouse handling
        return false;
      }
    }else{
      R_ASSERT_NON_RELEASE(false);
    }
  }END_VECTOR_FOR_EACH;

  return true;
}

bool PRESET_copy(const vector_t *patches){
  R_ASSERT_RETURN_IF_FALSE2(patches->num_elements > 0, false);

  if (!valid_patches(patches))
    return false;

  filepath_t filepath = createIllegalFilepath();

  VECTOR_FOR_EACH(const struct Patch *, patch, patches){
    SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
    if (plugin!=NULL){
      if (isLegalFilepath(plugin->preset_filename)){
        filepath = plugin->preset_filename;
        break;
      }
    }
  }END_VECTOR_FOR_EACH;
  
  g_preset_clipboard = get_preset_state(patches);
  
  HASH_put_filepath(g_preset_clipboard, "filename", filepath);
  
  return true;
}
  
void PRESET_save(const vector_t *patches, bool save_button_pressed, int64_t parentgui){  // "save_button_pressed is the "Save" button in the instrument window.

  if(patches->num_elements==0){
    GFX_Message2(NULL, true, "No instruments selected");
    return;
  }

  if (!valid_patches(patches))
    return;
  
  bool is_multipreset = patches->num_elements > 1;

  QString filename;

  QWidget *parentWidget = API_gui_get_parentwidget(NULL, parentgui);
  
  {

    radium::ScopedExec scopedExec(true);
        
    filename = QFileDialog::getSaveFileName(
                                            parentWidget,
                                            "Save Effect configuration",
                                            g_last_preset_path,
#ifdef FOR_WINDOWS
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
  }

  if(filename=="")
    return;

  disk_t *file = DISK_open_for_writing(filename);
  
  if(file==NULL){
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true, API_gui_get_parentwidget(NULL, parentgui)));
    msgBox->setText("Could not save file.");
    msgBox->setStandardButtons(QMessageBox::Ok);
    msgBox->setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox, true);
    return;
  }

  g_last_preset_path = QFileInfo(filename).absoluteDir().path();

  hash_t *state = get_preset_state(patches);
  
  HASH_save(state, file);
  
  DISK_close_and_delete(file);
}


