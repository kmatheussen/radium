#ifndef _RADIUM_AUDIO_FAUST_PLUGINS_PROC_H
#define _RADIUM_AUDIO_FAUST_PLUGINS_PROC_H

struct SoundPlugin;

extern LANGSPEC float *FAUST_get_peak_value_pointer(struct SoundPlugin *plugin, int effect_num);
extern LANGSPEC void FAUST_change_qtguistyle(const char *style_name);

#ifdef USE_QT4
namespace radium{
  struct FAUST_calledRegularlyByParentReply{
    bool has_new_data = false;

    bool factory_is_ready = false;
    bool factory_succeeded;

    bool svg_is_ready = false;
    bool svg_succeeded;
  };
}


class QDialog;
QDialog *FAUST_create_qdialog(SoundPlugin *plugin);
//void FAUST_set_qtguistyle(QDialog *gui);

void FFF_shut_down(void);
//void FAUST_inform_about_instrument_gui(SoundPlugin *plugin, QWidget *instrument_gui);
void FAUST_set_code(SoundPlugin *plugin, QString code);
void FAUST_set_options(SoundPlugin *plugin, QString options);
bool FAUST_is_compiling(const struct SoundPlugin *plugin);
QWidget *FAUST_get_native_gui(struct SoundPlugin *plugin);
QString FAUST_get_code(const struct SoundPlugin *plugin);
QString FAUST_get_options(const struct SoundPlugin *plugin);
void FAUST_generate_cpp_code(const struct SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback);
QString FAUST_get_error_message(const struct SoundPlugin *plugin);
QString FAUST_get_svg_path(const struct SoundPlugin *plugin);
radium::FAUST_calledRegularlyByParentReply FAUST_calledRegularlyByParent(struct SoundPlugin *plugin);
void FAUST_start_compilation(struct SoundPlugin *plugin);
#define DEFAULT_FAUST_DEV_PROGRAM "import(\"stdfaust.lib\");\n\nprocess = *(v), *(v) with {\n  v = ba.db2linear(hslider(\"volume\", 0, -35, 35, 0.1));\n};\n"
QString FAUSTGUI_get_code(QWidget *widget);
bool FAUST_set_use_interpreter_backend(struct SoundPlugin *plugin, bool use_interpreter);
bool FAUST_get_use_interpreter_backend(struct SoundPlugin *plugin);
#endif

#endif
