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



#include <sndfile.h>

#include <QDirIterator>
#include <QFileInfo>

#include "Qt_sample_requester_widget.h"

#include "../audio/Sampler_plugin_proc.h"
#include "../audio/FluidSynth_plugin_proc.h"
#include "../audio/SoundFonts_proc.h"
#include "../audio/undo_sample_proc.h"
#include "../common/patch_proc.h"
#include "../common/playerclass.h"

extern PlayerClass *pc;
extern QApplication *g_qapplication;

static bool file_could_be_a_sample(QString filename){
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
    SNDFILE *sndfile = sf_open(full_filename.toUtf8().constData(), SFM_READ, &sf_info);
    if(sndfile==NULL)
      return QString();

    num_channels = sf_info.channels;

    sf_close(sndfile);
  }

  int num_bytes = file_info.size();

  QString ret =
    file_info.fileName().leftJustify(k_filename_len,'.')
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
    
    ret = ret + size_string.rightJustify(5,' ');
  }

  g_filenames_hash[ret] = full_filename;

  return ret;
}

static QString get_display_name(const char *filename, int bank=-1, int preset=-1){
  QString str;
  int fontsize = QApplication::font().pointSize();
  if(fontsize<=0)
    fontsize = 7;
  else
    fontsize = fontsize*7/8;
  if(bank>=0)
    str.sprintf("<html><head/><body><p><span style=\" font-size:%dpt; font-weight:600; color:#4a4808;\">%s, b: %d, p: %d</span></p></body></html>",fontsize,filename,bank,preset);
  else
    str.sprintf("<html><head/><body><p><span style=\" font-size:%dpt; font-weight:600; color:#4a4808;\">%s</span></p></body></html>",fontsize,filename);
  return str;
}

class Sample_requester_widget : public QWidget
                              , public Ui::Sample_requester_widget 
{
  Q_OBJECT;

 public:

  //SoundPlugin *_plugin; // Nope, this value can not be stored. SoundPlugin objects must be used from the patch: (SoundPlugin*)patch->patchdata;
  struct Patch *_patch;

  QDir _dir;
  QLineEdit *_instrument_name_widget;
  QWidget *_instrument_widget;

  QString _sf2_file;
  QString _sf2_bank;

  int _preview_octave;

  QLabel *_sample_name_label;

  enum{
    IN_SF2_BANK,
    IN_SF2,
    IN_DIRECTORY
  } _file_chooser_state;


 Sample_requester_widget(QWidget *instrument_widget, QLineEdit *instrument_name_widget, QLabel *sample_name_label, Patch *patch)
   : QWidget(instrument_widget)
    , _patch(patch)
    , _instrument_name_widget(instrument_name_widget)
    , _instrument_widget(instrument_widget)
    , _preview_octave(4)
    , _sample_name_label(sample_name_label)
  {
    setupUi(this);
    //_dir = QDir("/home/kjetil/brenn/downloaded/temp/CATEGORY"); //QDir::currentPath();
    //_dir = QDir::currentPath();
    _dir = QDir(SETTINGS_read_string("samples_dir",QDir::currentPath().toUtf8().constData()));

    if(_dir.exists()==false)
      _dir = QDir(QDir::currentPath());

    _file_chooser_state = IN_DIRECTORY;

    octave->setToolTip("Preview octave.");
    up->setToolTip("Shift preview sound one octave up.");
    down->setToolTip("Shift preview sound one octave down");

    update_file_list();

    updateWidgets();
  }

  void updateWidgets(){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if(QString("Sample Player") == plugin->type->type_name){
      _sample_name_label->setText(get_display_name(SAMPLER_get_filename_display(plugin)));
    }else{
      _sample_name_label->setText(get_display_name(FLUIDSYNTH_get_filename_display(plugin)));
    }
  }

  void update_file_list(){
    file_list->clear();
    file_list->setSortingEnabled(false);

    printf("directory_path: \"%s\"\n",_dir.absolutePath().toUtf8().constData());
    path_edit->setText(_dir.absolutePath());

    _dir.setSorting(QDir::Name);
    
    QFileInfoList list = _dir.entryInfoList();
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo file_info = list.at(i);
      //QListWidgetItem *item = new QListWidgetItem(file_info.fileName(),file_list);
      //item->
      QString filename = file_info.fileName();

      if(file_info.isDir()){
        file_list->addItem(filename+"/");
      }else if(filename.endsWith(".sf2",Qt::CaseInsensitive)){
        QListWidgetItem *item = new QListWidgetItem(filename+"/");
        item->setForeground(QBrush(QColor("green")));
        QFont font;

        font.setBold(true);
        if(font.pixelSize()>0)
          font.setPixelSize(font.pixelSize()*3/2);
        else
          font.setPointSize(font.pointSize()*3/2);
        item->setFont(font);
        file_list->addItem(item);
      }else{
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        if(QString("Sample Player") == plugin->type->type_name){
          if(file_could_be_a_sample(filename)){
            QString display_name = get_sample_filename_display_string(file_info);
            if(display_name != QString("")){

              QListWidgetItem *item = new QListWidgetItem(display_name);
              QColor color = QColor("blue").light(80);
              color.setAlpha(150);
              item->setForeground(QBrush(color));
              QFont font;
              //QFont font("Bitstream Vera Sans Mono",8);
              font.setFamily("Bitstream Vera Sans Mono");
              font.setBold(true);
              font.setStyleName("Bold");

              if(font.pixelSize()>0)
                font.setPixelSize(8);//font.pixelSize()*4/3);
              else
                font.setPointSize(8);//font.pointSize()*4/3);
              item->setFont(font);
              file_list->addItem(item);
              //file_list->repaint();
              //g_qapplication->processEvents(); // WARNING! Removed. Got crash bug caused by this when loading and pressing key. Probably a billion other situations as well.
            }
          }
        }
      }
    }

    file_list->setCurrentRow(1);
  }

  void update_sf2_file_list(hash_t *name_list){
    file_list->clear();
    file_list->setSortingEnabled(true);

    file_list->addItem("../");

    for(int i=0;i<HASH_get_array_size(name_list);i++){
      QString name = HASH_get_string_at(name_list,"key",i);
      if(_file_chooser_state == IN_SF2)
        name = name + "/";

      file_list->addItem(name);
      printf("Adding \"%s\"\n",name.toUtf8().constData());
    }

    file_list->setCurrentRow(0);

    if(_file_chooser_state == IN_SF2_BANK)
      path_edit->setText(_dir.absolutePath()+"/"+_sf2_file+"/"+_sf2_bank);
    else
      path_edit->setText(_dir.absolutePath()+"/"+_sf2_file);
  }

  hash_t *get_bank_names(const char *filename){
    hash_t *info = SF2_get_info(filename);
    hash_t *bank_names = HASH_get_keys(HASH_get_hash(info,"menu"));
    return bank_names;
  }

  hash_t *get_bank(const char *filename, QString bank_name){
    hash_t *info = SF2_get_info(filename);
    hash_t *menu = HASH_get_hash(info,"menu");
    hash_t *bank = HASH_get_hash(menu,bank_name);
    return bank;
  }

  void handle_sf2_preset_pressed(QString item_text){
    printf("preset: Pressed \"%s\"\n",item_text.toUtf8().constData());

    if(item_text == "../"){
      _file_chooser_state = IN_SF2;
      update_sf2_file_list(get_bank_names(_sf2_file));
      return;
    }

    hash_t *bank       = get_bank(_sf2_file,_sf2_bank);
    hash_t *preset     = HASH_get_hash(bank,item_text);
    int     bank_num   = HASH_get_int(preset,"bank");
    int     preset_num = HASH_get_int(preset,"num");
    int     preset_bag = HASH_get_int(preset,"bag");

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    Undo_Sample_CurrPos(_patch);

    bool successfully_selected;

    if(QString("Sample Player") == plugin->type->type_name)
      successfully_selected = SAMPLER_set_new_sample(plugin,_sf2_file, preset_bag);
    else
      successfully_selected = FLUIDSYNTH_set_new_preset(plugin, _sf2_file, bank_num, preset_num);

    if(successfully_selected){
      _sample_name_label->setText(get_display_name(_sf2_file.toUtf8().constData(),bank_num,preset_num));
    }

    if(successfully_selected==true && pc->isplaying==false) {
      printf("playing note 1\n");
      PATCH_play_note(g_currpatch, 12*_preview_octave, -1, 0.5f, 1.0f);
    }
  }

  void handle_sf2_bank_pressed(QString item_text){
    printf("bank: Pressed \"%s\"\n",item_text.toUtf8().constData());

    if(item_text == "../"){
      _file_chooser_state = IN_DIRECTORY;
      update_file_list();
      return;
    }

    _file_chooser_state = IN_SF2_BANK;
    _sf2_bank = remove_last_char(item_text);

    hash_t *bank = get_bank(_sf2_file,_sf2_bank);
    hash_t *preset_names = HASH_get_keys(bank);
    update_sf2_file_list(preset_names);
  }

  void handle_sf2_file_pressed(QString item_text){
    printf("sf2_file: Pressed \"%s\"\n",item_text.toUtf8().constData());

    _file_chooser_state = IN_SF2;

    _sf2_file = _dir.absolutePath() + QString(QDir::separator()) + remove_last_char(item_text);
    update_sf2_file_list(get_bank_names(_sf2_file));
  }

  void handle_directory_pressed(QString item_text){
    printf("directory: Pressed \"%s\"\n",item_text.toUtf8().constData());

    QString org_directory_name = _dir.dirName();

    _dir.cd(item_text);
    update_file_list();

    SETTINGS_write_string("samples_dir",_dir.absolutePath().toUtf8().constData());
    //g_last_dir = _dir;

    if(item_text=="../") {
      QList<QListWidgetItem *> items = file_list->findItems(org_directory_name+"/",Qt::MatchExactly);
      printf("cursor_entry: \"%s\", num_items: %d\n",org_directory_name.toUtf8().constData(),items.count());
      if(items.count()>0)
        file_list->setCurrentItem(items.first());
    }
  }

  void handle_file_pressed(QString item_text){
    printf("file: Pressed \"%s\"\n",item_text.toUtf8().constData());

    if(g_filenames_hash.contains(item_text)==false)
      return;

    QString filename = g_filenames_hash[item_text];//_dir.absolutePath() + QString(QDir::separator()) + item_text;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    Undo_Sample_CurrPos(_patch);

    if(SAMPLER_set_new_sample(plugin,filename,file_list->currentRow()-1)==true){
      if(pc->isplaying==false){
        //printf("playing note 2\n");
        PATCH_play_note(g_currpatch, 12*_preview_octave, -1, 0.5f, 1.0f);
      }
      _sample_name_label->setText(get_display_name(filename));
    }
  }

  QString remove_last_char(QString item_text){
    return item_text.left(item_text.size()-1);
  }

  bool is_sf2_file_pressed(QString item_text){
    return item_text.endsWith("/") && SF2_get_info(_dir.absolutePath() + QString(QDir::separator()) + remove_last_char(item_text))!=NULL;
  }

  // I'm sure there is an isDir() function in Qt somewhere...
  bool is_directory_pressed(QString item_text){
    if(item_text.endsWith("/")==false)
      return false;
    if(item_text=="../")
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
      else if(is_file_pressed(item_text)==true)
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

public slots:

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

      _instrument_name_widget->setText(name);

      //audio_instrument_widget->on_name_widget_editingFinished()
      file_list->setCurrentItem(NULL);
      tab_name_has_changed(_instrument_widget,name);
      CHIP_update((SoundPlugin*)_patch->patchdata);
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
    if( current!=NULL && !current->text().endsWith("/"))
      handle_item_pressed(current->text());
  }

  void on_file_list_itemPressed(QListWidgetItem * item ){
    printf("file_list item pressed\n");
    //handle_item_pressed(item->text());
  }


  void on_path_edit_editingFinished()
  {
    QString new_path = path_edit->text();
    printf("new name: %s\n",new_path.toUtf8().constData());

    if(_dir.exists(new_path)){
      _dir.setPath(new_path);
      update_file_list();
      SETTINGS_write_string("samples_dir",_dir.absolutePath().toUtf8().constData());
    }else{
      path_edit->setText(_dir.absolutePath());
    }
    set_editor_focus();
  }

  
};
