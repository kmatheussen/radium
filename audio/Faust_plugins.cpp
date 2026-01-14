
#include <inttypes.h>
#include <math.h>

#include <unistd.h>

#include <string>
#include "../bin/packages/faust/architecture/faust/dsp/dsp.h"
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
#include "../bin/packages/faust/architecture/faust/dsp/llvm-dsp.h"
#endif
#include "../bin/packages/faust/architecture/faust/dsp/interpreter-dsp.h"


#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust/compiler/generator/libfaust.h"
#define HEPP 1
#undef uchar
#undef uint

#include "../bin/packages/faust/architecture/faust/gui/UI.h"


#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include "mfaustqt2.cpp"
#pragma GCC diagnostic pop


#include "../common/nsmtracker.h"

#include "../api/api_gui_proc.h"

#include "../Qt/FocusSniffers.h"
//}

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"


#pragma clang diagnostic pop

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

std::list<GUI*>  GUI::fGuiList;


#include "../common/visual_proc.h"
#include "../common/patch_proc.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/EditorWidget.h"
#include "../Qt/MyQTemporaryDir.hpp"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

namespace{
struct Data;
}

static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin);
#include "Faust_plugins_template1.cpp"
#include "Faust_plugins_template2.cpp"

#include "Faust_plugins_proc.h"


#define MAX_CHANNELS 16
#define MAX_EFFECTS 1024

namespace{

  static radium::Vector<QDialog*> g_faustqdialogs;
  static QString g_qtgui_stylesheet;

  class FaustQDialog : public RememberGeometryQDialog{

    SoundPlugin *_plugin;

  public:
    FaustQDialog(QWidget *parent, SoundPlugin *plugin)
      : RememberGeometryQDialog(parent, radium::NOT_MODAL)
      , _plugin(plugin)
    {      
      //QStyle *style = QStyleFactory::create("plastique");
      //if (style!=NULL)
      //  setStyle(style);

      if(g_qtgui_stylesheet=="")
        FAUST_change_qtguistyle(getFaustGuiStyle());

      if (g_qtgui_stylesheet != "")
        setStyleSheet(g_qtgui_stylesheet);
      
      g_faustqdialogs.push_back(this);
    }

    ~FaustQDialog(){
      g_faustqdialogs.remove(this);
      if(_plugin!=NULL)
        PLUGIN_call_me_when_gui_closes(_plugin);
    }

    void setVisible(bool visible) override {
      if(visible==false && _plugin!=NULL)
        PLUGIN_call_me_when_gui_closes(_plugin);
      RememberGeometryQDialog::setVisible(visible);
    }

  };
}

QDialog *FAUST_create_qdialog(SoundPlugin *plugin){
  return new FaustQDialog(g_main_window, plugin);
}

void FAUST_change_qtguistyle(const char *style_name){

  filepath_t filename = make_filepath(QString("packages/faust/architecture/faust/gui/Styles/") + QString(getFaustGuiStyle()) + ".qss");
  if (!OS_has_full_program_file_path(STRING_get_qstring(filename.id))){
    GFX_Message2(NULL, true, "File not found (%S)", filename.id);
    return;
  }
                                
  filename = OS_get_full_program_file_path(filename);
  disk_t *disk = DISK_open_for_reading(filename);
  if (disk==NULL){
      
    GFX_Message2(NULL, true, "File not found (%S)", filename.id);
    
  } else {
    
    QString stylesheet = DISK_read_qstring_file(disk);
    if (DISK_close_and_delete(disk)==false) {
      GFX_Message2(NULL, true, "Unable to read from %S", filename.id);
      stylesheet = "";
    }

    if (stylesheet!="")
      g_qtgui_stylesheet = stylesheet;
  }

  if (g_qtgui_stylesheet != "")
    for(auto faustqdialog : g_faustqdialogs)
      faustqdialog->setStyleSheet(g_qtgui_stylesheet);
  
}

#if 0
void FAUST_set_qtguistyle(QDialog *gui){
  /*
  if(g_qtgui_stylesheet=="")
    FAUST_change_qtguistyle(getFaustGuiStyle());

  if (g_qtgui_stylesheet != "")
    gui->setStyleSheet(g_qtgui_stylesheet);  
  */
}
#endif

#ifndef WITH_FAUST_DEV
static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin){
  return (Data*)plugin->data;
}
#endif

#ifdef WITH_FAUST_DEV



namespace{

  struct FFF_Reply {
    
    Data *data = NULL;
    QString error_message;
    bool is_instrument = false;
    interpreter_dsp_factory *interpreter_factory = NULL;
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
    llvm_dsp_factory *llvm_factory = NULL;
#endif
    dsp_factory *factory = NULL;

    bool is_empty(void){
      if (data==NULL && error_message=="") {
        //if (svg_dir!=NULL)
        //  RError("svg_dir!=NUL: %p\n",svg_dir);
        return true;
      } else
        return false;
    }
  };


struct Devdata{
  QString code;
  QString options;

#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
  bool use_interpreter_backend = true;
#else
  bool use_interpreter_backend = false;
#endif
  
  FFF_Reply reply;

  bool is_compiling; // <-- Can only be trusted if sending one request at a time. (used by the Faust_Plugin_widget constructor)

  QPointer<QDialog> qtgui_parent;

  QString svg_dir_error_message;
  MyQTemporaryDir *svg_dir = NULL;

  radium::FAUST_calledRegularlyByParentReply ready;

  Devdata()
    : options("-I\n%radium_path%/packages/faust/libraries")
    , is_compiling(false)
    , qtgui_parent(NULL)
  {
  }

  ~Devdata(){
    delete qtgui_parent.data();
  }
};

} // end anon. namespace


#include "Faust_factory_factory.cpp"



static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  return data;
}

static void dev_RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  int num_inputs = 0;
  int num_outputs = 0;

  if (data != NULL) {
    num_inputs = data->voices[0].dsp_instance->getNumInputs();
    num_outputs = data->voices[0].dsp_instance->getNumOutputs();

    if (num_inputs > MAX_CHANNELS || num_outputs > MAX_CHANNELS){
      for(int ch = 0 ; ch < MAX_CHANNELS ; ch++)
        memset(outputs[ch], 0, num_frames*sizeof(float));
      return;
    }

    if (devdata->reply.is_instrument)
      RT_process_instrument2(num_outputs, data, time, num_frames, inputs, outputs);
    else
      RT_process_effect2(data, time, num_frames, inputs, outputs);
  }

  // clear unused channels
  for(int ch = num_outputs ; ch < MAX_CHANNELS ; ch++)
    memset(outputs[ch], 0, num_frames*sizeof(float));

}

static void dev_play_note(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      play_note2(data, time, note);
}

static void dev_set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      set_note_volume2(data, time, note);
}

static void dev_set_note_pitch(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      set_note_pitch2(data, time, note);
}

static void dev_stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      stop_note2(data, time, note);
} 

static int get_num_effects(const Data *data) {
  return data->voices[0].myUI._num_effects;
}

static void dev_set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (effect_num < get_num_effects(data))
      set_effect_value2(data, effect_num, value, value_format, when);
}

static float dev_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data)){
    return 0.5;
  }
  
  return get_effect_value2(data, effect_num, value_format);
}

static void dev_get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data)){
    snprintf(buffer,buffersize," ");
    return;
  }

  get_display_value_string2(data, effect_num, buffer, buffersize);
}

static void create_state(const struct SoundPlugin *plugin, hash_t *state){
  //printf("\n\n\n ********** CREATE_STATE ************* \n\n\n");
  Devdata *devdata = (Devdata*)plugin->data;

  HASH_put_string(state, "code", STRING_toBase64(STRING_create(devdata->code)));
  HASH_put_string(state, "options", STRING_toBase64(STRING_create(devdata->options)));
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
  HASH_put_bool(state, "use_interpreter_backend", devdata->use_interpreter_backend);
#endif
}


static void FAUST_compile_now(struct SoundPlugin *plugin);

static void *dev_create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Devdata *devdata = new Devdata;

  plugin->data = devdata;

  if (state!=NULL) {
    devdata->code = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "code")));
    devdata->options = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "options")));
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
    if(HASH_has_key(state, "use_interpreter_backend"))
      devdata->use_interpreter_backend = HASH_get_bool(state, "use_interpreter_backend");
#endif
  } else
    devdata->code = DEFAULT_FAUST_DEV_PROGRAM;


  if (is_loading==false) {
    
    FAUST_start_compilation(plugin);

  } else {
  
    FAUST_compile_now(plugin);
    
  }
  
  return devdata;
}

static void dev_cleanup_plugin_data(SoundPlugin *plugin){
  fprintf(stderr,">>>>>>>>>>>>>> Cleanup_plugin_devdata called for %p\n",plugin);

  Devdata *devdata = (Devdata*)plugin->data;

  FFF_free_now(devdata->reply);

  delete devdata->svg_dir;
  
  // We can lose memory by not receiving replies in the queue, but that won't crash the program or make it unstable.
  // This should be so rare though, plus that the only consequency is to waste a little bit of memory, that it's probably not worth fixing.
  delete devdata;
}

static int dev_get_effect_format(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data == NULL || effect_num >= get_num_effects(data))
    return EFFECT_FORMAT_FLOAT;
  
  return get_effect_format2(data, effect_num);
}

static const char *dev_get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return NOTUSED_EFFECT_NAME;
  
  return get_effect_name2(data, effect_num);
}

static const char *dev_get_effect_description(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return "";
  
  return get_effect_description2(data, effect_num);
}


static bool dev_effect_is_visible(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return false;
  else
    return true;
}

static bool dev_show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  if (data!=NULL && devdata->qtgui_parent.data()!=NULL) {
    
    QWidget *parent = API_gui_get_parentwidget(NULL, parentgui);
    if (parent != NULL)
      set_window_parent(devdata->qtgui_parent.data(), parent, radium::NOT_MODAL, ShowAssertionOrThrowAPIException::SHOW_ASSERTION);
    
    data->qtgui->update(); // prevent flicker (update slider positions before showing gui)
    safeShow(devdata->qtgui_parent.data());
    data->qtgui->run();
    
    return true;
  }

  return false;
}

static void dev_hide_gui(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  if (devdata->qtgui_parent.data() != NULL){
    devdata->qtgui_parent->hide();
  }
  
  if (data!=NULL && data->qtgui!=NULL)
    data->qtgui->stop();
}

static bool dev_gui_is_visible(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  auto *parent = devdata->qtgui_parent.data();
  if (parent==NULL)
    return false;
  else
    return parent->isVisible();
}

void create_faust_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "Faust Dev";
  plugin_type->name                     = "Faust Dev";
  plugin_type->num_inputs               = MAX_CHANNELS;
  plugin_type->num_outputs              = MAX_CHANNELS;
  plugin_type->is_instrument            = true;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = MAX_EFFECTS;
  plugin_type->get_effect_format        = dev_get_effect_format;
  plugin_type->get_effect_name          = dev_get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_state             = create_state;
  plugin_type->create_plugin_data       = dev_create_plugin_data;
  plugin_type->cleanup_plugin_data      = dev_cleanup_plugin_data;

  plugin_type->RT_process       = dev_RT_process;
  plugin_type->play_note        = dev_play_note;
  plugin_type->set_note_volume  = dev_set_note_volume;
  plugin_type->set_note_pitch   = dev_set_note_pitch;
  plugin_type->stop_note        = dev_stop_note;
  plugin_type->set_effect_value = dev_set_effect_value;
  plugin_type->get_effect_value = dev_get_effect_value;
  plugin_type->get_display_value_string = dev_get_display_value_string;
  plugin_type->get_effect_description   = dev_get_effect_description;
  plugin_type->effect_is_visible = dev_effect_is_visible;

  plugin_type->show_gui = dev_show_gui;
  plugin_type->hide_gui = dev_hide_gui;
  plugin_type->gui_is_visible = dev_gui_is_visible;

  //plugin_type->plugin_takes_care_of_savable_values = true;
    
  plugin_type->info =
    "HTML: FAUST (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards. More info <A href=\"http://faust.grame.fr\">here</a>."
    "<p>"
    "Hints:\n"
    "<UL>"
    "<LI> To zoom, either the editor or a diagram, press CTRL while scrolling the mouse wheel."
    "<LI> To search for a string in the source code, press Ctrl + F."
    "<LI> Running full size window (by pressing the \"Full\" button) can be very convenient when developing."
    "</UL"
    ;

  PR_add_plugin_type(plugin_type);

  PR_add_menu_entry(PluginMenuEntry::level_up("FaustDev examples"));
  {
    PR_add_load_preset_menu_entries_in_directory(OS_get_full_program_file_path("faustdev_examples"));
  }
  PR_add_menu_entry(PluginMenuEntry::level_down());
}

/*
void FAUST_inform_about_instrument_gui(SoundPlugin *plugin, QWidget *instrument_gui){
  Devdata *devdata = (Devdata*)plugin->data;
  devdata->instrument_gui = instrument_gui;
}
*/

void FAUST_set_code(SoundPlugin *plugin, QString code){
  Devdata *devdata = (Devdata*)plugin->data;  
  devdata->code = code;
}

void FAUST_set_options(SoundPlugin *plugin, QString options){
  Devdata *devdata = (Devdata*)plugin->data;  
  devdata->options = options;
}


// Can only be trusted if sending one request at a time.
bool FAUST_is_compiling(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->is_compiling;
}
  
QString FAUST_get_code(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->code;
}

QString FAUST_get_options(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->options;
}

void FAUST_generate_cpp_code(const struct SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback){

  const Devdata *devdata = (Devdata*)plugin->data;
  
  QString code = devdata->code;
  QString options = devdata->options;

  // CPU usage should not clubber up here, at least not too much, since FAUST_generate_cpp_code is only called after the llvm/interpreter code has been created.

  // Must do this in main thread since DISK_create_non_existant_filename allocates gc-memory.
  filepath_t template_ = appendFilePaths(DISK_get_temp_dir(), make_filepath(L"radium_faust_cppsource.cpp"));
  filepath_t temp_file = DISK_create_non_existant_filename(template_);
  
  QString filename = STRING_get_qstring(temp_file.id);
      
  auto ret = QtConcurrent::run([code, options, generation, filename, callback]{

      ArgsCreator args;
      args.push_back("-o");
      args.push_back(filename);
      args.push_back(options.split("\n", Qt::SkipEmptyParts));
      
      std::string error_message2;
      
      QString message;
    
      if (generateAuxFilesFromString(
                                     "FaustDev",
                                     code.toUtf8().constData(),
                                     args.get_argc(),
                                     args.get_argv(),
                                     error_message2
                                     )
          == false)
        {

          message = QString("// Unable to create cpp source: %1").arg(error_message2.c_str());

        } else {
        
          disk_t *disk = DISK_open_for_reading(filename);
        
          if (disk==NULL){
            
            message = QString("// Error! File not found: \"") + filename.toUtf8().constData() + "\"";
          
          } else {
            
            QString cpp_code = DISK_read_qstring_file(disk);
            
            if (DISK_close_and_delete(disk)==false) {
              
              message = QString("// Error! Unable to read from \"") + filename.toUtf8().constData() + "\"";
              
            } else {
             
              message = cpp_code;
              
            }
            
          }

          QFile::remove(filename);

      }
      
      THREADING_run_on_main_thread_async([callback, generation, message]{
          callback(generation, message);
        }
        );
      
    });
  (void)ret;
}


QString FAUST_get_error_message(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;

  if (devdata->reply.error_message != "")
    return devdata->reply.error_message;

  if (devdata->svg_dir_error_message != "")
    return devdata->svg_dir_error_message;

  return "";
}

QString FAUST_get_svg_path(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  if (devdata->svg_dir==NULL || devdata->svg_dir->isValid()==false)
    return "";
  else
    return devdata->svg_dir->path() + QDir::separator() + "FaustDev-svg" + QDir::separator() + "process.svg";
}




static void FAUST_handle_new_svg_dir(struct SoundPlugin *plugin, MyQTemporaryDir *svg_dir, QString error_message){
  R_ASSERT(THREADING_is_main_thread());

  if(plugin==NULL){

    printf("     .. patch disappeared\n");
    delete svg_dir;
    
  } else {
      
    Devdata *devdata = (Devdata*)plugin->data;
    
    if(devdata->svg_dir!=NULL)
      delete devdata->svg_dir;
    
    devdata->svg_dir = svg_dir;
    devdata->svg_dir_error_message = error_message;
    
    //printf("    SVG error2: -%s-\n", error_message.toUtf8().constData());
    add_svg_ready(devdata, svg_dir != NULL);
    
  } 
}

static void copy_effects(Data *from, Data *to){
  int from_num_effects = get_num_effects(from);
  int to_num_effects = get_num_effects(to);

  for(int i1=0;i1<from_num_effects;i1++){

    for(int i2=0;i2<to_num_effects;i2++){

      if(!strcmp(get_effect_name2(from, i1), get_effect_name2(to, i2))){

        float val = get_effect_value2(from, i1, EFFECT_FORMAT_NATIVE);
        set_effect_value2(to, i2, val, EFFECT_FORMAT_NATIVE, FX_single);

        break;
      }
    }
  }
}

static bool FAUST_handle_fff_reply(struct SoundPlugin *plugin, const FFF_Reply &reply, bool is_initializing){
  R_ASSERT(THREADING_is_main_thread());

  Devdata *devdata = (Devdata*)plugin->data;
  
  if (reply.data==NULL){
    fprintf(stderr,"Error-message: -%s-\n", devdata->reply.error_message.toUtf8().constData());
    devdata->reply.error_message = reply.error_message;
    return false;
  }

  FFF_Reply old_reply = devdata->reply;

  bool do_call_qtgui_update = false;
  bool do_call_qtgui_run = false;

  // handle gui
  {
    if (devdata->qtgui_parent.data() == NULL)
      devdata->qtgui_parent = FAUST_create_qdialog(plugin);
    
    if (old_reply.data != NULL && old_reply.data->qtgui!=NULL){
      old_reply.data->qtgui->stop();
      devdata->qtgui_parent->layout()->removeWidget(old_reply.data->qtgui);
    }

    create_gui(devdata->qtgui_parent.data(), reply.data, plugin);

    if (!is_initializing && old_reply.data != NULL && reply.data != NULL){
      copy_effects(old_reply.data, reply.data); // Must do this after calling create_gui since the QTGUI functions sets sliders to the default values of the plugin (using meta information), and not the actual values.
      do_call_qtgui_update = true;
    }

    do_call_qtgui_run = devdata->qtgui_parent->isVisible();
  }

  PLAYER_lock();{
    devdata->reply = reply;
  }PLAYER_unlock();

  plugin->num_visible_outputs = R_MIN(MAX_CHANNELS, reply.data->voices[0].dsp_instance->getNumOutputs());

  if (old_reply.data != NULL){
    struct Patch *patch = (struct Patch*)plugin->patch;
    PATCH_handle_fxs_when_fx_names_have_changed(patch, true);
  }

  if (old_reply.data != NULL)
    delete_dsps_and_data1(old_reply.data);

  g_fff_thread.free_reply_data(old_reply, false);

  // QTGUI has a custom update() function that updates directly, so we must call update() after calling "devdata->reply = reply". 
  //
  if (do_call_qtgui_update)
    reply.data->qtgui->update(); // Update slider positions before starting the timer below (by calling "run"). If not, there might be flicker.
  
  if (do_call_qtgui_run)
    reply.data->qtgui->run();

  return true;
}

radium::FAUST_calledRegularlyByParentReply FAUST_calledRegularlyByParent(struct SoundPlugin *plugin){

  Devdata *devdata = (Devdata*)plugin->data;
  
  auto ret = devdata->ready; //FFF_get_reply(devdata->id);

  devdata->ready = radium::FAUST_calledRegularlyByParentReply();

  return ret;
}



void FAUST_start_compilation(struct SoundPlugin *plugin){

  Devdata *devdata = (Devdata*)plugin->data;

  const auto *patch = plugin->patch;
  instrument_t patch_id = make_instrument(-1);

  if(patch==NULL)
    R_ASSERT_NON_RELEASE(false);
  else
    patch_id = patch->id;

  FFF_request_reply(patch_id, CompileOptions(devdata));

  /*      
  devdata->is_compiling = true;

  QString code = devdata->code;
  QString options = devdata->options;

  
  QtConcurrent::run([plugin, code, options, optlevel]{    
      FFF_Reply reply;
      g_fff_thread.create_reply(code, options, optlevel, reply);
      
      QMetaObject::invokeMethod(qApp, [plugin, reply]{
          FAUST_handle_fff_reply(plugin, reply, false);
        });
      
    });
  */
}


static void FAUST_compile_now(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;

  printf("    Calling FAUST_compile_now\n");
  FFF_run_now(plugin, CompileOptions(devdata));

  //return FAUST_handle_fff_reply(plugin, FFF_get_reply_now(devdata->code, devdata->options), is_initializing);
}

bool FAUST_set_use_interpreter_backend(struct SoundPlugin *plugin, bool use_interpreter){
#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
  R_ASSERT_NON_RELEASE(false);
  return false;
#endif
  
  Devdata *devdata = (Devdata*)plugin->data;

  if (use_interpreter != devdata->use_interpreter_backend){
    devdata->use_interpreter_backend = use_interpreter;
    FAUST_start_compilation(plugin);
    return true;
  }

  return false;
}

bool FAUST_get_use_interpreter_backend(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;

  return devdata->use_interpreter_backend;
}

#endif
