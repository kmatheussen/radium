/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#include <QDirIterator>
#include <QFileInfo>
#include <QTime>

#include "Qt_sample_requester_widget.h"

#include "../audio/Sampler_plugin_proc.h"
#include "../audio/FluidSynth_plugin_proc.h"
#include "../audio/SoundFonts_proc.h"
#include "../audio/undo_plugin_state_proc.h"
#include "../common/patch_proc.h"
#include "../common/playerclass.h"

extern PlayerClass *pc;
extern QApplication *g_qapplication;

QStringList get_sample_name_filters(void){
  static bool inited=false;
  static QStringList list;

  if (inited==false){
    QStringList a;
    a
      << "*.xi"
      << "*.wav"
      << "*.aif"
      << "*.aiff"
      << "*.ogg"
      << "*.flac"
      << "*.caf"
      << "*.au"
      << "*.iff"
      << "*.w64"
      << "*.wavex"
      << "*.voc"
      << "*.mat4"
      << "*.mat5"
      << "*.sds"
      << "*.sd2"
      << "*.rf64"
      << "*.wave"
      << "*.snd"
      << "*.sound";

    for(auto b : a){
      list << b;
      list << b.toUpper();
    }
    inited = true;
  }

  return list;
}

bool file_could_be_a_sample(QString filename){
  for(auto filter : get_sample_name_filters())
    if (filename.endsWith(filter.mid(1)))
      return true;

  return false;
  
  /*
  return false
    || filename.endsWith(".xi",Qt::CaseInsensitive)
    || filename.endsWith(".wav",Qt::CaseInsensitive) 
    || filename.endsWith(".aif",Qt::CaseInsensitive) 
    || filename.endsWith(".aiff",Qt::CaseInsensitive)
    || filename.endsWith(".ogg",Qt::CaseInsensitive)
    || filename.endsWith(".flac",Qt::CaseInsensitive)
    || filename.endsWith(".caf",Qt::CaseInsensitive)
    || filename.endsWith(".au",Qt::CaseInsensitive)
    || filename.endsWith(".iff",Qt::CaseInsensitive)
    || filename.endsWith(".w64",Qt::CaseInsensitive)
    || filename.endsWith(".wavex",Qt::CaseInsensitive)
    || filename.endsWith(".voc",Qt::CaseInsensitive) 
    || filename.endsWith(".mat4",Qt::CaseInsensitive)
    || filename.endsWith(".mat5",Qt::CaseInsensitive) 
    || filename.endsWith(".sds",Qt::CaseInsensitive) 
    || filename.endsWith(".sd2",Qt::CaseInsensitive) 
    || filename.endsWith(".rf64",Qt::CaseInsensitive) 
    || filename.endsWith(".wave",Qt::CaseInsensitive)
    || filename.endsWith(".snd",Qt::CaseInsensitive)
    || filename.endsWith(".sound",Qt::CaseInsensitive);
  */
}

/*
2_channel_short.wav 2ch,452kb
tronchor.wav        1ch,364kb
tronstrn.wav        1ch,237kb
TRUMPET.WAV         1ch, 46kb
*/

static QHash<QString, QString> g_filenames_hash;
static const int k_filename_len = 23;

static QString get_sample_filename_display_string(QFileInfo file_info){
  int num_channels;

  QString full_filename = file_info.absoluteFilePath();

  {
    SF_INFO sf_info; memset(&sf_info,0,sizeof(sf_info));

    //printf("Trying to open %s\n",file_info.absoluteFilePath().toUtf8().constData());
    SNDFILE *sndfile = radium_sf_open(full_filename, SFM_READ, &sf_info);
    
    if(sndfile==NULL)
      return QString();

    num_channels = sf_info.channels;

    sf_close(sndfile);
  }

  int64_t num_bytes = file_info.size();

  QString ret =
    file_info.fileName().leftJustified(k_filename_len,'.')
    + QString::number(num_channels)+"ch,";

  {
    QString size_string;

    if(num_bytes<1000)
      size_string = QString::number(num_bytes)+"b";
    
    else if(num_bytes<1000*1024)
      size_string = QString::number(num_bytes/1024)+"kB";
    
    else if(num_bytes<1000*(1024*1024))
      size_string = QString::number(num_bytes/(1024*1024))+"MB";
    
    else
      size_string = QString::number(num_bytes/(1024*1024*1024))+"GB";
    
    ret = ret + size_string.rightJustified(5,' ');
  }

  g_filenames_hash[ret] = full_filename;

  return ret;
}

static QString get_display_name(QString filename, int bank=-1, int preset=-1){
  int fontsize = QApplication::font().pointSize();
  if(fontsize<=0)
    fontsize = 7;
  else
    fontsize = fontsize*7/8;
  
  QString str("<html><head/><body><p><span style=\" font-size:");

  str += QString::number(fontsize);
  str += QString("pt; font-weight:600; color:'%1';\">").arg(get_qcolor(CURRENT_SOUNDFILE_COLOR_NUM).name());
  str += filename;
    
  if (bank>=0)
    str += str.sprintf(", b: %d, p: %d",bank,preset);

  str += "</span></p></body></html>";
    
  return str;
}

namespace{
  struct DirectoryContentEntry{
    bool is_dir = false;
    bool is_sf2 = false;
    QString display;
  };
}

static QHash<QString, QVector<DirectoryContentEntry> > g_directory_content;

class Sample_requester_widget : public QWidget
                              , public Ui::Sample_requester_widget 
{
  Q_OBJECT;

 public:

  bool is_starting_up;
  

  //SoundPlugin *_plugin; // Nope, this value can not be stored. SoundPlugin objects must be used from the patch: (SoundPlugin*)patch->patchdata;
  radium::GcHolder<struct Patch> _patch;

  QDir _dir;
  QLineEdit *_instrument_name_widget;
  QWidget *_instrument_widget;

  QString _sf2_file;
  QString _sf2_bank;
  
  int _preview_octave;

  QLabel *_sample_name_label;

  bool _is_fluid_synth = false;
  bool _is_sample_player = false;
  
  enum{
    IN_SF2_BANK,
    IN_SF2,
    IN_DIRECTORY
  } _file_chooser_state;

  int _bookmark;

 Sample_requester_widget(QWidget *instrument_widget, QLineEdit *instrument_name_widget, QLabel *sample_name_label, Patch *patch)
   : QWidget(instrument_widget)
   , is_starting_up(true)
   , _patch(patch)
   , _instrument_name_widget(instrument_name_widget)
   , _instrument_widget(instrument_widget)
   , _preview_octave(4)
   , _sample_name_label(sample_name_label)
   , _bookmark(-1)
  {
    setupUi(this);

    // set _dir
    {
      bool use_saved_path = true;
      
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      if (plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player") || plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Click")) {

        _is_sample_player = true;
                
        bool is_default_sound;
        QString filename = STRING_get_qstring(SAMPLER_get_filename(plugin, &is_default_sound));
        if (!is_default_sound) {
          _dir = QFileInfo(filename).absoluteDir();
          use_saved_path = false;
        }

        printf("   default: %d. Filename: -%s-\n",is_default_sound, filename.toUtf8().constData());
        
      } else if (plugin->type==PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth")){

        _is_fluid_synth = true;

        bool is_default_sound;
        QString filename = STRING_get_qstring(FLUIDSYNTH_get_filename(plugin, &is_default_sound));
        if (!is_default_sound) {
          _dir = QFileInfo(filename).absoluteDir();
          use_saved_path = false;
        }

      }

      
      if (use_saved_path) {
        _bookmark = 1;
        _dir = QDir(QDir::currentPath());
        read_bookmark_and_set_dir();
      } else
        SETTINGS_write_string("samples_dir",_dir.absolutePath());
      
      if(_dir.exists()==false) {
        printf(" Dir %s didnt exist\n",_dir.absolutePath().toUtf8().constData());
        _dir = QDir(QDir::currentPath());
      }
    }
    
    _file_chooser_state = IN_DIRECTORY;

    octave->setToolTip("Preview octave.");
    up->setToolTip("Octave up (preview sound).");
    down->setToolTip("Octave down (preview sound).");

    update_file_list(false);

    updateWidgets();

    is_starting_up = false;
  }
  
  void read_bookmark_and_set_dir(void){
    QString settings_key = QString("sample_bookmarks")+QString::number(_bookmark);
    QString default_dir = SETTINGS_read_qstring(
                                                "samples_dir", // "samples_dir" contains last used sample.
                                                _dir.absolutePath()
                                                );

    QString path = SETTINGS_read_qstring(settings_key, default_dir);
    _dir = QDir(path);

    if(_dir.exists()==false) {
      static QSet<QString> already_warned_about;
      QString name = _dir.absolutePath();
      if (already_warned_about.contains(name)==false){
        vector_t v = {};
        int remove = -1;
        
        if (path != default_dir)
          remove = VECTOR_push_back(&v, "Remove bookmark");

        int ok = VECTOR_push_back(&v, "Ok");

        int ret = GFX_Message(&v, "Bookmarked sample directory \"%s\" doesn't exist anymore",name.toUtf8().constData());

        if (ret==remove){
          SETTINGS_remove(settings_key.toUtf8().constData());
        } else if(ret==ok){
          already_warned_about.insert(name);
        }
      }
      
      if (path != default_dir)
        _dir = QDir(default_dir);

      if(_dir.exists()==false)
        _dir = QDir(QDir::currentPath());
    }
  }

  void write_bookmark(void){
    if (_bookmark > 0)
      SETTINGS_write_string(QString("sample_bookmarks")+QString::number(_bookmark), _dir.absolutePath());

    SETTINGS_write_string("samples_dir",_dir.absolutePath());
  }
  
  void update_sample_name_label(QString text, int bank=-1, int preset=-1){
    //_sample_name_label->setText(QString("<font color='%1'>%2</font").arg(get_qcolor(CURRENT_SOUNDFILE_COLOR_NUM).name(), text));
    _sample_name_label->setText(get_display_name(text, bank, preset));
  }
  
  void updateWidgets(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if(_is_sample_player){
      update_sample_name_label(STRING_get_qstring(SAMPLER_get_filename_display(plugin)));
    }else if (_is_fluid_synth){
      update_sample_name_label(STRING_get_qstring(FLUIDSYNTH_get_filename_display(plugin)));
    }

    QFontMetrics fm(QApplication::font());
    int header_height = fm.height() * 4 / 3;
    horizontalWidget->setMinimumHeight(header_height);
    horizontalWidget->setMaximumHeight(header_height);
  }

  // This function can take lot of time when loading. However, we don't want this function to use a lot of time when moving the cursor to a new track in the editor.
  // (qt is really slow to create widgets...)
  void update_file_list(bool reread){
    
    struct Tracker_Windows *window = root->song->tracker_windows;
    QTime time;

    time.start();
    
    file_list->clear();
    file_list->setSortingEnabled(false);

    const char *pathtext = talloc_strdup(_dir.absolutePath().toUtf8().constData());

    printf("directory_path: \"%s\"\n",pathtext);
    path_edit->setText(_dir.absolutePath());

    QFont soundfile_font;
    {
      //QFont font("Bitstream Vera Sans Mono",8);
      soundfile_font.setFamily("Bitstream Vera Sans Mono");
      soundfile_font.setBold(true);
      soundfile_font.setStyleName("Bold");
      
      if(soundfile_font.pixelSize()>0)
        soundfile_font.setPixelSize(8);//font.pixelSize()*4/3);
      else
        soundfile_font.setPointSize(8);//font.pointSize()*4/3);
    }


    QColor soundfile_color = get_qcolor(SOUNDFILE_COLOR_NUM);              
    soundfile_color.setAlpha(150);
    QBrush soundfile_brush(soundfile_color);

    QFont sf2_font;
    {
      sf2_font.setBold(true);
      if(sf2_font.pixelSize()>0)
        sf2_font.setPixelSize(sf2_font.pixelSize()*3/2);
      else
        sf2_font.setPointSize(sf2_font.pointSize()*3/2);
    }

    QBrush sf2_brush(get_qcolor(SOUNDFONT_COLOR_NUM));

    _dir.setSorting(QDir::Name);    

    int size;

    QVector<DirectoryContentEntry> content;

    if (!reread)
      content = g_directory_content[_dir.absolutePath()];

    QFileInfoList list;

    bool use_content = !reread && content.size()>0;
    
    if (use_content){
      size = content.size();
    }else{
      list =  _dir.entryInfoList();
      size = list.size();
    }
    
    for (int i = 0; i < size; ++i) {

      if (time.elapsed() > 500 || (window->message!=NULL && time.elapsed() >= 50)){
        time.restart();
        if (GFX_ProgressIsOpen()){
          const char *message = talloc_format("Loading sample directory \"%s\" into memory. (%d / %d)", pathtext, i, list.size());
          GFX_ShowProgressMessage(message);
        } else {
          const char *message = talloc_format("Loading sample directory into memory. (%d / %d)", i, list.size());       
          window->message = message;
          GL_create(window);
        }
      }

      DirectoryContentEntry entry;

      if (use_content){
        entry = content[i];
      } else {
        QFileInfo file_info = list.at(i);
        //QListWidgetItem *item = new QListWidgetItem(file_info.fileName(),file_list);
        //item->
        QString filename = file_info.fileName();

        if(file_info.isDir()){
          entry.is_dir = true;
          entry.display = filename+"/";
        }else if(filename.endsWith(".sf2",Qt::CaseInsensitive)){
          entry.is_sf2 = true;
          entry.display = filename+"/";
        }else if(_is_sample_player){
          if(file_could_be_a_sample(filename))
            entry.display = get_sample_filename_display_string(file_info);
        }
        
        content.push_back(entry);
        
      }
      

      if(entry.is_dir){
        file_list->addItem(entry.display);
      }else if(entry.is_sf2){
        QListWidgetItem *item = new QListWidgetItem(entry.display);
        item->setForeground(sf2_brush);
        item->setFont(sf2_font);
        file_list->addItem(item);
      }else if(_is_sample_player && entry.display != ""){
        QListWidgetItem *item = new QListWidgetItem(entry.display);
        item->setForeground(soundfile_brush);
        item->setFont(soundfile_font);
        file_list->addItem(item);
      }
    }

    file_list->setCurrentRow(1);

    if (!use_content)
      g_directory_content[_dir.absolutePath()] = content;
            
    if (window->message!=NULL){      
      window->message=NULL;
      GL_create(window);
    }
  }  

  void update_sf2_file_list(hash_t *name_list){
    
    file_list->clear();
    file_list->setSortingEnabled(true);

    file_list->addItem("../");
    
    R_ASSERT(name_list!=NULL);
    if(name_list!=NULL){
      for(int i=0;i<HASH_get_array_size(name_list, "key");i++){
        QString name = STRING_get_qstring(HASH_get_string_at(name_list,"key",i));
        if(_file_chooser_state == IN_SF2)
          name = name + "/";
        
        file_list->addItem(name);
        printf("Adding \"%s\"\n",name.toUtf8().constData());
      }
    }

    file_list->setCurrentRow(0);

    if(_file_chooser_state == IN_SF2_BANK)
      path_edit->setText(_dir.absolutePath()+"/"+_sf2_file+"/"+_sf2_bank);
    else
      path_edit->setText(_dir.absolutePath()+"/"+_sf2_file);
  }

  hash_t *get_bank_names(const wchar_t *filename){
    hash_t *info = SF2_get_info(filename);
    hash_t *bank_names = HASH_get_keys_in_hash(HASH_get_hash(info,"menu"));
    return bank_names;
  }

  hash_t *get_bank(const wchar_t *filename, QString bank_name){
    hash_t *info = SF2_get_info(filename);
    R_ASSERT_RETURN_IF_FALSE2(info!=NULL, NULL);
    
    hash_t *menu = HASH_get_hash(info,"menu");
    R_ASSERT_RETURN_IF_FALSE2(menu!=NULL, NULL);
    
    hash_t *bank = HASH_get_hash(menu,bank_name.toUtf8().constData());
    R_ASSERT_RETURN_IF_FALSE2(bank!=NULL, NULL);
    
    return bank;
  }

  int get_bank_num(const wchar_t *filename, QString bank_name){
    hash_t *bank = get_bank(filename, bank_name);
    R_ASSERT_RETURN_IF_FALSE2(bank!=NULL, -1);

    hash_t *preset  = HASH_get_hash(bank,bank_name.toUtf8().constData());
    R_ASSERT_RETURN_IF_FALSE2(preset!=NULL, -1);
    
    return HASH_get_int32(preset, "bank");
  }
  
  void handle_sf2_preset_pressed(QString item_text){
    EVENTLOG_add_event(talloc_format("preset: Pressed \"%s\"\n",item_text.toUtf8().constData()));
                       
    if(item_text == "../"){
      _file_chooser_state = IN_SF2;
      update_sf2_file_list(get_bank_names(STRING_create(_sf2_file)));
      return;
    }

    hash_t *bank       = get_bank(STRING_create(_sf2_file),_sf2_bank);
    if (bank==NULL)
      return; // assertions in get_bank.
    
    hash_t *preset     = HASH_get_hash(bank,item_text.toUtf8().constData());
    R_ASSERT_RETURN_IF_FALSE(preset!=NULL);
    
    int     bank_num   = HASH_get_int32(preset,"bank");
    int     preset_num = HASH_get_int32(preset,"num");
    int     preset_bag = HASH_get_int32(preset,"bag");

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
    
    ADD_UNDO(PluginState(_patch.data(), NULL));

    bool successfully_selected = false;

    if(_is_sample_player)
      successfully_selected = SAMPLER_set_new_sample(plugin, STRING_create(_sf2_file), preset_bag);
    else if (_is_fluid_synth)
      successfully_selected = FLUIDSYNTH_set_new_preset(plugin, STRING_create(_sf2_file), bank_num, preset_num);

    if(successfully_selected){
      update_sample_name_label(_sf2_file,bank_num,preset_num);
    }

    if(successfully_selected==true && ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED){
      //printf("playing note 1\n");
      PATCH_play_note(_patch.data(), create_note_t(NULL, -1, 12*_preview_octave, 0.5f, 0.0f, 0, 0, 0));
    }
  }

  void handle_sf2_bank_pressed(QString item_text){
    printf("bank: Pressed \"%s\"\n",item_text.toUtf8().constData());

    if(item_text == "../"){
      _file_chooser_state = IN_DIRECTORY;
      update_file_list(true);
      return;
    }

    _file_chooser_state = IN_SF2_BANK;
    _sf2_bank = remove_last_char(item_text);

    hash_t *bank = get_bank(STRING_create(_sf2_file),_sf2_bank);
    if (bank != NULL){
      hash_t *preset_names = HASH_get_keys_in_hash(bank);
      update_sf2_file_list(preset_names);
    }
  }

  void handle_sf2_file_pressed(QString item_text){
    printf("sf2_file: Pressed \"%s\"\n",item_text.toUtf8().constData());

    _file_chooser_state = IN_SF2;

    _sf2_file = _dir.absolutePath() + QString(QDir::separator()) + remove_last_char(item_text);
    update_sf2_file_list(get_bank_names(STRING_create(_sf2_file)));
  }

  void handle_directory_pressed(QString item_text){
    printf("directory: Pressed \"%s\"\n",item_text.toUtf8().constData());

    QString org_directory_name = _dir.absolutePath();

    _dir.cd(item_text);
    update_file_list(true);

    write_bookmark();
    
    if(item_text=="../") {
      QList<QListWidgetItem *> items = file_list->findItems(org_directory_name+"/",Qt::MatchExactly);
      printf("cursor_entry: \"%s\", num_items: %d\n",org_directory_name.toUtf8().constData(),items.count());
      if(items.count()>0)
        file_list->setCurrentItem(items.first());
    }
  }

  void handle_file_pressed(QString item_text){
    R_ASSERT_RETURN_IF_FALSE(_is_sample_player);
    
    printf("file: Pressed \"%s\"\n",item_text.toUtf8().constData());

    if(g_filenames_hash.contains(item_text)==false)
      return;

    QString filename = g_filenames_hash[item_text];//_dir.absolutePath() + QString(QDir::separator()) + item_text;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    ADD_UNDO(PluginState(_patch.data(), NULL));

    if(SAMPLER_set_new_sample(plugin,STRING_create(filename),file_list->currentRow()-1)==true){
      if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED){
        //printf("playing note 2\n");
        PATCH_play_note(_patch.data(), create_note_t(NULL, -1, 12*_preview_octave, 0.5f, 0.0f, 0, 0, 0));
      }
      update_sample_name_label(filename);
    }
  }

  QString remove_last_char(QString item_text){
    return item_text.left(item_text.size()-1);
  }

  bool is_sf2_file_pressed(QString item_text){
    return item_text.endsWith("/") && SF2_get_info(STRING_create(_dir.absolutePath() + QString(QDir::separator()) + remove_last_char(item_text))) != NULL;
  }

  // I'm sure there is an isDir() function in Qt somewhere...
  bool is_directory_pressed(QString item_text){
    if(item_text.endsWith("/")==false)
      return false;

    if(item_text=="../")
      return true;

    if(item_text=="./")
      return true;

    QDir dir(_dir);
    dir.cd(item_text);

    if(dir.exists()==false)
      return false;

    if(_dir.absolutePath()==dir.absolutePath())
      return false;

    dir.cdUp();
    if(_dir.absolutePath()==dir.absolutePath())
      return true;

    return false;
  }

  bool is_file_pressed(QString item_text){
    if(g_filenames_hash.contains(item_text)==false)
      return false;

    QString filename = g_filenames_hash[item_text];//_dir.absolutePath() + QString(QDir::separator()) + item_text;

    return QFileInfo(filename).exists();
  }

  void handle_item_pressed(QString item_text){
    printf("item pressed\n");
    switch(_file_chooser_state){
    case IN_SF2_BANK:
      handle_sf2_preset_pressed(item_text);
      break;
    case IN_SF2:
      handle_sf2_bank_pressed(item_text);
      break;
    case IN_DIRECTORY:
      if(is_directory_pressed(item_text)==true)
        handle_directory_pressed(item_text);
      else if(_is_sample_player && is_file_pressed(item_text)==true)
        handle_file_pressed(item_text);
      else if(is_sf2_file_pressed(item_text)==true)
        handle_sf2_file_pressed(item_text);
      else
        printf("Couldn't handle that one: \"%s\"",item_text.toUtf8().constData());
      break;
    default:
      RError("weird");
    }
  }

  void bookmark_selected(int num){
    printf("bookmark %d\n",num);
    _bookmark = num;
    _file_chooser_state = IN_DIRECTORY;
    read_bookmark_and_set_dir();
    update_file_list(false);
  }

  void change_path(QString new_path){
    printf("new name: %s\n",new_path.toUtf8().constData());

    if(_dir.exists(new_path)){
      if (_dir.absolutePath() != new_path){
        _dir.setPath(new_path);
        update_file_list(false);
        SETTINGS_write_string("samples_dir",_dir.absolutePath());
      }
    }else{
      path_edit->setText(_dir.absolutePath());
    }
    set_editor_focus();
  }

public slots:

  void on_random_button_clicked(bool){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if(_is_sample_player) {
      
      ADD_UNDO(PluginState(_patch.data(), NULL));
      SAMPLER_set_random_sample(plugin, STRING_create(_dir.absolutePath()));

    } else if (_sf2_file != "" && _sf2_bank != ""){

      // TODO: Fix code below.
        
      /*
      ADD_UNDO(PluginState(_patch.data()));
      
      wchar_t *filename = STRING_create(_sf2_file);

      hash_t *bank = get_bank(filename, _sf2_bank);

      int bank_num = get_bank_num(filename, _sf2_bank);

      if (bank_num >= 0){
        hash_t *preset_names = HASH_get_keys_in_hash(bank);

        int num_presets = HASH_get_array_size(preset_names, "key");
        
        if (num_presets > 0){
          int preset_num = qrand() % num_presets;
          FLUIDSYNTH_set_new_preset(plugin, filename, bank_num, preset_num);
          //update_sample_name_label(_sf2_file,bank_num,preset_num);
        }
      }
      */
    }
  }
  
  void on_up_clicked(bool){
    if(_preview_octave>9)
      return;
    _preview_octave++;
    octave->setText(" "+QString::number(_preview_octave)+" ");
  }

  void on_down_clicked(bool){
    if(_preview_octave==1)
      return;
    _preview_octave--;
    octave->setText(" "+QString::number(_preview_octave)+" ");
  }

  void on_file_list_itemActivated ( QListWidgetItem * item ){
    bool was_normal_file = item!=NULL && !item->text().endsWith("/");
    //on_file_list_itemPressed(item);

    handle_item_pressed(item->text());

    if(was_normal_file){

      QString name;

      if(g_filenames_hash.contains(item->text())==true)
        name = QFileInfo(g_filenames_hash[item->text()]).fileName();
      else
        name = item->text();

      R_ASSERT(g_currpatch==_patch.data());

      //audio_instrument_widget->on_name_widget_editingFinished()
      file_list->setCurrentItem(NULL);

      R_ASSERT(g_currpatch==_patch.data());

      if (tab_name_has_changed(_instrument_widget,name)) {
        _instrument_name_widget->update(); //setText(name);
        CHIP_update((SoundPlugin*)_patch->patchdata);
      }

      set_editor_focus();
    }
  }

  void on_file_list_itemEntered ( QListWidgetItem * item ){
    printf("Entered!\n");
  }

  void on_file_list_itemSelectionChanged (){
    printf("Selection changed!\n");
  }

  void on_file_list_currentItemChanged ( QListWidgetItem * current, QListWidgetItem * previous ){
    printf("Current item changed!\n");
    if(is_starting_up==false && current!=NULL && !current->text().endsWith("/"))
      handle_item_pressed(current->text());
  }

  void on_file_list_itemPressed(QListWidgetItem * item ){
    printf("file_list item pressed\n");
    //handle_item_pressed(item->text());
  }


  void on_path_edit_editingFinished()
  {
    change_path(path_edit->text());
  }

  void on_bookmark1_toggled(bool is_on){
    if (is_on)
      bookmark_selected(1);
  }
  
  void on_bookmark2_toggled(bool is_on){
    if (is_on)
      bookmark_selected(2);
  }
  
  void on_bookmark3_toggled(bool is_on){
    if (is_on)
      bookmark_selected(3);
  }
  
  void on_bookmark4_toggled(bool is_on){
    if (is_on)
      bookmark_selected(4);
  }
  
  void on_bookmark5_toggled(bool is_on){
    if (is_on)
      bookmark_selected(5);
  }
  
  void on_bookmark6_toggled(bool is_on){
    if (is_on)
      bookmark_selected(6);
  }
  
  void on_bookmark7_toggled(bool is_on){
    if (is_on)
      bookmark_selected(7);
  }
  
  void on_bookmark8_toggled(bool is_on){
    if (is_on)
      bookmark_selected(8);
  }
  
  void on_bookmark9_toggled(bool is_on){
    if (is_on)
      bookmark_selected(9);
  }
  
};

void SAMPLEREQUESTER_set_path(Sample_requester_widget *w, QString new_path){
  w->change_path(new_path);
}

QString SAMPLEREQUESTER_get_path(Sample_requester_widget *w){
  return w->_dir.absolutePath();
}
    
