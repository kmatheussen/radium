#ifndef _RADIUM_AUDIO_FAUST_PLUGINS_PROC_H
#define _RADIUM_AUDIO_FAUST_PLUGINS_PROC_H

struct SoundPlugin;

extern LANGSPEC float *FAUST_get_peak_value_pointer(struct SoundPlugin *plugin, int effect_num);
extern LANGSPEC void FAUST_change_qtguistyle(const char *style_name);

#if USE_QT4
enum FAUST_calledRegularlyByParentReply{
  Faust_No_New_Reply,
  Faust_Failed,
  Faust_Success
};

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
QString FAUST_get_cpp_code(const struct SoundPlugin *plugin);
QString FAUST_get_error_message(const struct SoundPlugin *plugin);
QString FAUST_get_svg_path(const struct SoundPlugin *plugin);
FAUST_calledRegularlyByParentReply FAUST_calledRegularlyByParent(struct SoundPlugin *plugin);
void FAUST_start_compilation(struct SoundPlugin *plugin);
bool FAUST_compile_now(struct SoundPlugin *plugin, bool is_initializing);
#define DEFAULT_FAUST_DEV_PROGRAM "import(\"music.lib\");\n\nprocess = *(v), *(v) with {\n  v = db2linear(hslider(\"volume\", 0, -35, 35, 0.1));\n};\n"
QString FAUSTGUI_get_code(QWidget *widget);
#endif

#endif
