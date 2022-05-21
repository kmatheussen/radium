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

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>




#include <QSpinBox>

#include <qstring.h>
#include <qlineedit.h>
#include <qsplitter.h>
#include <qevent.h>
#include <qtreeview.h>
#include <qlistwidget.h>
#include <QString>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1
#include "../common/nsmtracker.h"

#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/player_proc.h"
//#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "undo_instruments_widget_proc.h"
//#include "../common/undo_patchlist_proc.h"
#include "../common/undo_tracks_proc.h"
#include "../common/visual_proc.h"
#include "../common/settings_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/Presets_proc.h"
#include "../midi/midi_instrument.h"
#include "../midi/midi_instrument_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"

#include "Qt_MyQSlider.h"
#include "Qt_MyQLabel.h"
#include "Qt_MyQCheckBox.h"
#include "Qt_MyQButton.h"
#include "Qt_MyQSpinBox.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/Mixer_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include "Qt_colors_proc.h"


#include "Qt_instruments_proc.h"


extern QApplication *qapplication;



#include "FocusSniffers.h"

#include "EditorWidget.h"
#include "../GTK/GTK_visual_proc.h"
#include "../OpenGL/Widget_proc.h"
#include <QTreeView>
#include <QFileSystemModel>
#include <QSortFilterProxyModel>
#include <QVBoxLayout>
#include <QDebug>
#include "../api/api_common_proc.h"

#include <string>
#include <fstream>

QWidget *g_presetbrowser_widget = nullptr;
static radium::KeyboardFocusFrame *g_presetbrowser_widget_frame;


class BrowserQSortFilterProxyModel: public QSortFilterProxyModel
{
  protected:
  bool filterAcceptsRow(int source_row, const QModelIndex &source_parent) const
  {
    QString word = filterText;
    QStringList words = filterText.split(QLatin1Char(' '), Qt::SkipEmptyParts);

    QString currIndexData = sourceModel()->data(source_parent, (int)QFileSystemModel::FilePathRole).toString();
    if (currIndexData == "")
      return true;

    //QModelIndex index0 = sourceModel()->index(sourceRow, 0, sourceParent);
    qDebug() << "------------------------------------------------------------------------";
    /*qDebug() << "filtr: " + sourceModel()->data(source_parent, (int)QFileSystemModel::FilePathRole).toString();
    QString currIndexData = sourceModel()->data(source_parent, (int)QFileSystemModel::FilePathRole).toString(); 

    if (currIndexData == "") {
      qDebug() << "true";
      return true;
    }

    if (presetFolder.startsWith(currIndexData))
    {
      qDebug() << "true";
      return true;
    }

    if (currIndexData == presetFolder)
    {
      qDebug() << "true";
      return true;
    }*/

    /*
    if (currIndexData.contains(presetFolder, Qt::CaseInsensitive)) {
      if (currIndexData.contains(word, Qt::CaseInsensitive))
      {
          qDebug() << "true";
          return true;
      }
      // check childrens
            
    }*/

    // https://stackoverflow.com/questions/250890/using-qsortfilterproxymodel-with-a-tree-model
    // get source-model index for current row
    QModelIndex source_index = sourceModel()->index(source_row, 0, source_parent);

    // check current index itself :
    if( source_index.isValid() ) {
      QString key = sourceModel()->data(source_index, (int)QFileSystemModel::FilePathRole).toString();
      qDebug() << "key: " + key;

      if (presetFolder.startsWith(key))
        return true;

      if (key.startsWith(presetFolder)) {
        // check words
        bool containsAll = true;
        for (int i = 0 ; i < words.size() ; i++)
        {
          if (!key.contains(words.at(i), Qt::CaseInsensitive)) {
            containsAll = false;
            break;
          }
        }
        if (containsAll)
          return true;
      }

      // if any of children matches the filter, then current index matches the filter as well
      sourceModel()->fetchMore(source_index);
      qDebug() << "childs start: " + key;
      int nb = sourceModel()->rowCount(source_index);
      qDebug() << "rows count " << nb;
      for ( int i=0; i<nb; ++i ) {
        if ( filterAcceptsRow(i, source_index) ) {
          qDebug() << "childs stop: " + key;
          return true ;
        }
      }
      qDebug() << "childs stop: " + key;
    }
    else
      qDebug() << "something goes wrong -------------------------- ";


    qDebug() << "false";
    return false;
  }

  public:
  QString presetFolder;
  QString filterText;

  void setPresetFolder(const QString &pFolder)
  {
    presetFolder = pFolder;
    invalidateFilter();
  }

  void setFilter(const QString &filter)
  {
    this->filterText = filter;
    invalidateFilter();
  }

};


class PresetBrowser : public QWidget
{
  Q_OBJECT;

  public:
  PresetBrowser(QWidget *parent=NULL): QWidget(parent) {
    layout = new QVBoxLayout(this);
    layout->setSpacing(1);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setObjectName(QString::fromUtf8("vLayout"));

    title = new QLabel("Presets", this);
    layout->addWidget(title);

    presetFolder = "/home/and3md/akimaze/Radium/Presets";

    model.setRootPath(presetFolder);
    //filterModel.setRecursiveFilteringEnabled(true);
    filterModel.setSourceModel(&model);
    filterModel.setFilterRole((int)QFileSystemModel::FilePathRole);
    filterModel.setPresetFolder(presetFolder);

    tree = new FocusSnifferQTreeView(this);
    tree->setModel(&filterModel);
    tree->setRootIndex(filterModel.mapFromSource(model.index(presetFolder)));
    //tree->setRootIndex(filterModel.mapFromSource(model.index("")));
    tree->hideColumn(1);
    tree->hideColumn(2);
    tree->hideColumn(3);
    //tree->setHeaderHidden(true);
    layout->addWidget(tree);

    filterEdit = new FocusSnifferQLineEdit(this);
    layout->addWidget(filterEdit);

    presetDemoInstrument = createIllegalInstrument();


    connect(tree,SIGNAL(activated(const QModelIndex &)),this,SLOT(UsePreset(const QModelIndex &)));
    connect(tree,SIGNAL(pressed(const QModelIndex &)),this,SLOT(PlayPreset(const QModelIndex &)));
    connect(tree,SIGNAL(clicked(const QModelIndex &)),this,SLOT(StopPreset(const QModelIndex &)));

    connect(filterEdit,SIGNAL(editingFinished()),this,SLOT(SetFilter()));

    // https://stackoverflow.com/questions/54930898/how-to-always-expand-items-in-qtreeview
    connect(tree->model(), &QAbstractItemModel::rowsInserted,
      [this](const QModelIndex &parent, int first, int last)
      {
        // New rows have been added to parent.  Make sure parent is fully expanded.
        tree->expandRecursively(parent);
      });

  }

  virtual ~PresetBrowser() {};

  public slots:
    void UsePreset(const QModelIndex & index);
    void PlayPreset(const QModelIndex & index);
    void StopPreset(const QModelIndex & index);
    void SetFilter();

  private:
    QLabel* title;
    FocusSnifferQLineEdit * filterEdit;
    QFileSystemModel model;
    BrowserQSortFilterProxyModel filterModel;

    QVBoxLayout *layout;

    FocusSnifferQTreeView *tree;
    instrument_t presetDemoInstrument;
    int playnote_id = -1;
    QString presetFolder;
};

void PresetBrowser::SetFilter() {
    filterModel.setFilter(filterEdit->text());
}


void PresetBrowser::UsePreset(const QModelIndex & index){
  qDebug() << "activation";
  qDebug() << "file path" + index.data(QFileSystemModel::FilePathRole).toString();
  QString filePath = index.data(QFileSystemModel::FilePathRole).toString();
  QString fileName = index.data(QFileSystemModel::FileNameRole).toString();

  QFile ff(filePath);
  QFileInfo fileInfo(ff);
  if (fileInfo.exists() && fileInfo.isFile()){
    QString fileName = fileInfo.baseName();
    qDebug() << "dziłamy" << fileName;
    /*ff.open(QIODevice::ReadOnly);
    QString s;
    QTextStream s1(&ff);
    s.append(s1.readAll());
    //qDebug() << s;
    QByteArray ba = str1.toLocal8Bit();
      const char *desc = ba.data();
    int64_t patch_id = createAudioInstrumentFromDescription(desc, NULL, 1, 1);*/

    //createAudioInstrumentFromPreset(filePath, "aha", 1, 1, true);
    //PRESET_load(make_filepath(filePath), "aha", true, true, getCurrMixerSlotX(), getCurrMixerSlotY());
    //createAudioInstrumentFromPreset(make_filepath(filePath), "", getCurrMixerSlotX(), getCurrMixerSlotY(), true);
    instrument_t ins = createAudioInstrumentFromPreset(make_filepath(filePath), "", 0, 0, true); // create instrument from preset

    setInstrumentForTrack(ins, currentTrack(currentBlock(0), 0), currentBlock(0),0); // add instrument to current track
    connectAudioInstrumentToMainPipe(ins);
    autopositionInstrument(ins);
    setInstrumentName(fileName.toStdString().c_str(), ins); // set name to preset file name
    //showInstrumentGui(ins, gui_getEditorGui(), false); // show instrument gui
  }
}

void PresetBrowser::PlayPreset(const QModelIndex & index){
  startIgnoringUndo();
  if (playnote_id >= 0 && isLegalInstrument(presetDemoInstrument))
  {
    stopNote(playnote_id, 0, presetDemoInstrument);
    playnote_id = -1;
    //qDebug() << "finish preset play 1";
  }

  if (isLegalInstrument(presetDemoInstrument))
  {
    instrument_t i = presetDemoInstrument;
    presetDemoInstrument = createIllegalInstrument();
    deleteAudioConnection(i, getMainPipeInstrument());
    deleteInstrument(i);
    //qDebug() << "delete preset 2";
  }

  QString filePath = index.data(QFileSystemModel::FilePathRole).toString();
  QFile ff(filePath);
  QFileInfo fileInfo(ff);
  if (fileInfo.exists() && fileInfo.isFile()){
    //qDebug() << "create preset demo instrument";
    presetDemoInstrument = createAudioInstrumentFromPreset(make_filepath(filePath), "PresetPlayer", 1, 1, false);

    /*
    std::string line,text;
    std::ifstream in("test.txt");
    while(std::getline(in, line)) {
      text += line + "\n";
    }
            
    const char* data = text.c_str();
    requestLoadInstrumentPreset(presetDemoInstrument, data, gui_getEditorGui());
    */

    //qDebug() << "connect preset instrument ";
    connectAudioInstrumentToMainPipe(presetDemoInstrument);

    int notenum = root->keyoct + 24; // default is 36 C2 
    if(notenum>=0 && notenum<127)
      playnote_id = playNote(notenum, 120, 0, 0, presetDemoInstrument); // startuje granie nuty
  }
  stopIgnoringUndo();
}


void PresetBrowser::StopPreset(const QModelIndex & index){
  startIgnoringUndo();
  if (playnote_id >= 0 && isLegalInstrument(presetDemoInstrument)){
    stopNote(playnote_id, 0, presetDemoInstrument);
    playnote_id = -1;
    //qDebug() << "finished 1";
  }
  if (isLegalInstrument(presetDemoInstrument))
  {
    instrument_t i = presetDemoInstrument;
    presetDemoInstrument = createIllegalInstrument();
    deleteAudioConnection(i, getMainPipeInstrument());
    deleteInstrument(i);
    //qDebug() << "delete 1";
  }
  stopIgnoringUndo();
}


QWidget *createPresetBrowserWidget() {
  //g_presetbrowser_widget_frame = new radium::KeyboardFocusFrame(g_main_window, radium::KeyboardFocusFrameType::BROWSER, true);
  g_presetbrowser_widget_frame = new radium::KeyboardFocusFrame(g_main_window, radium::KeyboardFocusFrameType::EDITOR, true);
  g_presetbrowser_widget = new PresetBrowser(g_presetbrowser_widget_frame);
  g_presetbrowser_widget_frame->layout()->addWidget(g_presetbrowser_widget);

  return g_presetbrowser_widget_frame;
}

QWidget *getPresetBrowserWidget(void){
  return g_presetbrowser_widget;
}

QWidget *getPresetBrowserWidgetFrame(void){
  return g_presetbrowser_widget_frame;
}


#include "mQt_PresetBrowser.cpp"

