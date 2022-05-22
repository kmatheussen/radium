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
#include <QHBoxLayout>
#include <QDebug>
#include <QButtonGroup>
#include "../api/api_common_proc.h"

#include <string>
#include <fstream>

QWidget *g_presetbrowser_widget = nullptr;
static radium::KeyboardFocusFrame *g_presetbrowser_widget_frame = nullptr;


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

    // qDebug() << "------------------------------------------------------------------------";

    // https://stackoverflow.com/questions/250890/using-qsortfilterproxymodel-with-a-tree-model
    // get source-model index for current row
    QModelIndex source_index = sourceModel()->index(source_row, 0, source_parent);

    // check current index itself :
    if(source_index.isValid()) {
      QString key = sourceModel()->data(source_index, (int)QFileSystemModel::FilePathRole).toString();

      // add folders above our preset folder
      if (presetFolder.startsWith(key))
        return true;

      if (key.startsWith(presetFolder)) {

        // check file extension
        if (! dynamic_cast<QFileSystemModel *>(sourceModel())->isDir(source_index))
          if (!key.endsWith(".rec", Qt::CaseInsensitive))
            return false;

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

        // if any of children matches the filter, then current index matches the filter as well
        sourceModel()->fetchMore(source_index);
        int nb = sourceModel()->rowCount(source_index);

        for (int i = 0; i < nb; ++i) {
          if (filterAcceptsRow(i, source_index)) {
            return true ;
          }
        }

      }
    }
    else
      qDebug() << "something goes wrong -------------------------- ";

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
  PresetBrowser(const QString &presetRootFolder, QWidget *parent=NULL): QWidget(parent) {
    // TODO: should be get from settings
    presetFolder = presetRootFolder;

    layout = new QVBoxLayout(this);
    layout->setSpacing(1);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setObjectName(QString::fromUtf8("vLayout"));

    title = new QLabel("Presets");
    layout->addWidget(title);

    model.setRootPath(presetFolder);
    filterModel.setSourceModel(&model);
    filterModel.setFilterRole((int)QFileSystemModel::FilePathRole);
    filterModel.setPresetFolder(presetFolder);

    tree = new FocusSnifferQTreeView(this);
    tree->setModel(&filterModel);
    tree->setRootIndex(filterModel.mapFromSource(model.index(presetFolder)));
    tree->hideColumn(1);
    tree->hideColumn(2);
    tree->hideColumn(3);
    //tree->setHeaderHidden(true);
    layout->addWidget(tree);

    filterEdit = new FocusSnifferQLineEdit(this);
    filterEdit->setPlaceholderText("Search");
    layout->addWidget(filterEdit);

    noteSharpButtonsLayout = new QHBoxLayout(this);
    noteSharpButtonsLayout->setContentsMargins(0, 0, 0, 0);
    layout->addLayout(noteSharpButtonsLayout);

    noteButtonsLayout = new QHBoxLayout(this);
    noteButtonsLayout->setContentsMargins(0, 0, 0, 0);
    layout->addLayout(noteButtonsLayout);

    noteC = new MyQCheckBox("C");
    noteD = new MyQCheckBox("D");
    noteE = new MyQCheckBox("E");
    noteF = new MyQCheckBox("F");
    noteG = new MyQCheckBox("G");
    noteA = new MyQCheckBox("A");
    noteB = new MyQCheckBox("B");

    noteCSharp = new MyQCheckBox("C#");
    noteDSharp = new MyQCheckBox("D#");
    noteFSharp = new MyQCheckBox("F#");
    noteGSharp = new MyQCheckBox("G#");
    noteASharp = new MyQCheckBox("A#");

    // play only one note at time maybe there should be chords button ;)
    notesButtonGroup = new QButtonGroup(this);

    notesButtonGroup->addButton(noteC);
    noteC->setChecked(true);
    notesButtonGroup->addButton(noteD);
    notesButtonGroup->addButton(noteE);
    notesButtonGroup->addButton(noteF);
    notesButtonGroup->addButton(noteG);
    notesButtonGroup->addButton(noteA);
    notesButtonGroup->addButton(noteB);

    notesButtonGroup->addButton(noteCSharp);
    notesButtonGroup->addButton(noteDSharp);
    notesButtonGroup->addButton(noteFSharp);
    notesButtonGroup->addButton(noteGSharp);
    notesButtonGroup->addButton(noteASharp);

    // set id's
    notesButtonGroup->setId(noteC, 0);
    notesButtonGroup->setId(noteCSharp, 1);
    notesButtonGroup->setId(noteD, 2);
    notesButtonGroup->setId(noteDSharp, 3);
    notesButtonGroup->setId(noteE, 4);
    notesButtonGroup->setId(noteF, 5);
    notesButtonGroup->setId(noteFSharp, 6);
    notesButtonGroup->setId(noteG, 7);
    notesButtonGroup->setId(noteGSharp, 8);
    notesButtonGroup->setId(noteA, 9);
    notesButtonGroup->setId(noteASharp, 10);
    notesButtonGroup->setId(noteB, 11);

    noteButtonsLayout->addWidget(noteC);
    noteButtonsLayout->addWidget(noteD);
    noteButtonsLayout->addWidget(noteE);
    noteButtonsLayout->addWidget(noteF);
    noteButtonsLayout->addWidget(noteG);
    noteButtonsLayout->addWidget(noteA);
    noteButtonsLayout->addWidget(noteB);

    noteSharpButtonsLayout->addWidget(noteCSharp);
    noteSharpButtonsLayout->addWidget(noteDSharp);
    noteSharpButtonsLayout->addWidget(noteFSharp);
    noteSharpButtonsLayout->addWidget(noteGSharp);
    noteSharpButtonsLayout->addWidget(noteASharp);

    presetDemoInstrument = createIllegalInstrument();

    connect(tree,SIGNAL(activated(const QModelIndex &)),this,SLOT(usePreset(const QModelIndex &)));
    connect(tree,SIGNAL(pressed(const QModelIndex &)),this,SLOT(playPreset(const QModelIndex &)));
    connect(tree,SIGNAL(clicked(const QModelIndex &)),this,SLOT(stopPreset(const QModelIndex &)));

    connect(filterEdit,SIGNAL(editingFinished()),this,SLOT(setFilter()));

    // https://stackoverflow.com/questions/54930898/how-to-always-expand-items-in-qtreeview
    connect(&filterModel, &QAbstractItemModel::rowsInserted,
      [this](const QModelIndex &parent, int first, int last)
      {
        // New rows have been added to parent. Make sure parent is fully expanded.
        tree->expandRecursively(parent);
      });
  }

  virtual ~PresetBrowser() {};

  public slots:
    void usePreset(const QModelIndex & index);
    void playPreset(const QModelIndex & index);
    void stopPreset(const QModelIndex & index);
    void setFilter();

    void setPresetFolder(const QString &presetRootFolder);

  protected:
    int selectedNote();
  private:
    QLabel* title;
    FocusSnifferQLineEdit * filterEdit;
    QFileSystemModel model;
    BrowserQSortFilterProxyModel filterModel;

    QVBoxLayout *layout;
    QHBoxLayout *noteButtonsLayout;
    QHBoxLayout *noteSharpButtonsLayout;

    FocusSnifferQTreeView *tree;
    instrument_t presetDemoInstrument;
    int playnote_id = -1;

    QString presetFolder;
    QButtonGroup *notesButtonGroup;

    MyQCheckBox *noteC;
    MyQCheckBox *noteD;
    MyQCheckBox *noteE;
    MyQCheckBox *noteF;
    MyQCheckBox *noteG;
    MyQCheckBox *noteA;
    MyQCheckBox *noteB;

    MyQCheckBox *noteCSharp;
    MyQCheckBox *noteDSharp;
    MyQCheckBox *noteFSharp;
    MyQCheckBox *noteGSharp;
    MyQCheckBox *noteASharp;
};

void PresetBrowser::setPresetFolder(const QString &presetRootFolder) {
  presetFolder = presetRootFolder;
  filterModel.setPresetFolder(presetFolder);
  model.setRootPath(presetFolder);
  tree->setRootIndex(filterModel.mapFromSource(model.index(presetFolder)));
}

void PresetBrowser::setFilter() {
  filterModel.setFilter(filterEdit->text());
}

void PresetBrowser::usePreset(const QModelIndex & index){
  // qDebug() << "activation";
  // qDebug() << "file path" + index.data(QFileSystemModel::FilePathRole).toString();
  QString filePath = index.data(QFileSystemModel::FilePathRole).toString();
  QString fileName = index.data(QFileSystemModel::FileNameRole).toString();

  QFile ff(filePath);
  QFileInfo fileInfo(ff);
  if (fileInfo.exists() && fileInfo.isFile()){
    QString fileName = fileInfo.baseName();
    instrument_t ins = createAudioInstrumentFromPreset(make_filepath(filePath), "", 0, 0, true); // create instrument from preset

    setInstrumentForTrack(ins, currentTrack(currentBlock(0), 0), currentBlock(0),0); // add instrument to current track
    connectAudioInstrumentToMainPipe(ins);
    autopositionInstrument(ins);
    setInstrumentName(fileName.toStdString().c_str(), ins); // set name to preset file name
    //showInstrumentGui(ins, gui_getEditorGui(), false); // show instrument gui
  }
}

int PresetBrowser::selectedNote() {
  int note = notesButtonGroup->checkedId();
  if (note < 0)
    note = 0;
  return note;   
}

void PresetBrowser::playPreset(const QModelIndex & index){
  if (QGuiApplication::mouseButtons() != Qt::LeftButton)
    return;

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

    //qDebug() << "connect preset instrument ";
    connectAudioInstrumentToMainPipe(presetDemoInstrument);

    int notenum = root->keyoct + 24 + selectedNote(); // default is 36 C2 
    if(notenum>=0 && notenum<127)
      playnote_id = playNote(notenum, 120, 0, 0, presetDemoInstrument); // startuje granie nuty
  }
  stopIgnoringUndo();
}


void PresetBrowser::stopPreset(const QModelIndex & index){
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


QWidget *createPresetBrowserWidget(const QString &presetRootFolder) {
  //g_presetbrowser_widget_frame = new radium::KeyboardFocusFrame(g_main_window, radium::KeyboardFocusFrameType::BROWSER, true);
  g_presetbrowser_widget_frame = new radium::KeyboardFocusFrame(g_main_window, radium::KeyboardFocusFrameType::EDITOR, true);
  g_presetbrowser_widget = new PresetBrowser(presetRootFolder, g_presetbrowser_widget_frame);
  g_presetbrowser_widget_frame->layout()->addWidget(g_presetbrowser_widget);

  return g_presetbrowser_widget_frame;
}

QWidget *getPresetBrowserWidget(void){
  return g_presetbrowser_widget;
}

QWidget *getPresetBrowserWidgetFrame(void){
  return g_presetbrowser_widget_frame;
}

void setPresetBrowserRootFolder(const QString &folder) {
  if (g_presetbrowser_widget) {
    dynamic_cast<PresetBrowser *>(g_presetbrowser_widget)->setPresetFolder(folder);
  }
}


void showHidePresetBrowser(void){
  if (g_presetbrowser_widget_frame)
    g_presetbrowser_widget_frame->setVisible(!g_presetbrowser_widget_frame->isVisible());
  //setPresetBrowserRootFolder("/home/and3md/akimaze/Radium/Presets");
}


#include "mQt_PresetBrowser.cpp"
