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

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/visual_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/blocklist_proc.h"
#include "../api/api_common_proc.h"
#include "../common/player_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/gfx_proc.h"
#include "../common/settings_proc.h"

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include <qpushbutton.h>
#include <qsplitter.h>
#include <qapplication.h>

#if 0
#include <qwidget.h>
#include <qmainwindow.h>
#endif

#ifdef USE_QT3
#include "qlistbox.h"
#endif

#ifdef USE_QT4
#include <QListWidget>
#include <QListWidgetItem>

namespace{
class QListBox : public QListWidget{
public:
  QListBox(QWidget *parent) : QListWidget(parent) {}
  void insertItem(QString text){
    QListWidget::addItem(text);
  }
  void insertItem(const char *text){
    insertItem(QString(text));
  }
  int currentItem(){
    return QListWidget::currentRow();
  }
  void setSelected(int pos, bool something){
    QListWidget::setCurrentRow(pos);
  }
  void removeItem(int pos){
    QListWidget::takeItem(pos);
  }
};
}

#define QValueList QList
#endif



extern struct Root *root;
extern PlayerClass *pc;


static const int xborder = 0;
static const int yborder = 0;

static int button_height = 30;
static int button_width = 50;

static int get_blocklist_x2(bool stacked, int width, int height);

// Returns true if the playlist should be placed above the blocklist.
static bool do_stack(int width, int height){
  return get_blocklist_x2(false, width, height) < 150;
}

static int get_blocklist_x1(bool stacked, int width, int height){
  return xborder;
}

static int get_blocklist_y1(bool stacked, int width, int height){
  return yborder;
}

static int get_add_button_y1(bool stacked, int width, int height);
static int get_blocklist_y2(bool stacked, int width, int height){
  if(stacked)
    return get_add_button_y1(stacked,width,height) - yborder;
  else
    return height;//-yborder;
}

static int get_blocklist_x2(bool stacked, int width, int height){
  if(stacked)
    return width-xborder;
  else
    return width/2 - button_width/2 - xborder;
}

static int get_playlist_x1(bool stacked, int width, int height){
  if(stacked)
    return xborder;
  else
    return width/2 + button_width/2 + xborder;
}

static int get_add_button_y2(bool stacked, int width, int height);
static int get_playlist_y1(bool stacked, int width, int height){
  if(stacked)
    return get_add_button_y2(true,width,height) + xborder;
  else
    return yborder;
}

static int get_playlist_x2(bool stacked, int width, int height){
  return width-xborder;
}

static int get_playlist_y2(bool stacked, int width, int height){
  return height;//-yborder;
}

static int get_add_button_x1(bool stacked, int width, int height){
  if(stacked)
    return xborder; //width/2 - button_width - xborder;
  else
    return width/2 - button_width/2;
}

static int get_add_button_y1(bool stacked, int width, int height){
  if(stacked)
    return height/2 - button_height/2;
  else
    return height/2 - button_height - yborder/2;
}

static int get_add_button_x2(bool stacked, int width, int height){
  if(stacked)
    return width/2 - xborder/2;
  else
    return get_add_button_x1(stacked,width,height) + button_width;
}

static int get_add_button_y2(bool stacked, int width, int height){
  return get_add_button_y1(stacked,width,height) + button_height;
}

static int get_remove_button_x1(bool stacked, int width, int height){
  if(stacked)
    return width/2 + xborder/2;
  else
    return get_add_button_x1(stacked,width,height);
}

static int get_remove_button_y1(bool stacked, int width, int height){
  if(stacked)
    return get_add_button_y1(stacked,width,height);
  else
    return get_add_button_y1(stacked,width,height) + button_height + yborder;
}

static int get_remove_button_x2(bool stacked, int width, int height){
  if(stacked)
    return width-xborder;
  else
    return get_remove_button_x1(stacked,width,height) + button_width;
}

static int get_remove_button_y2(bool stacked, int width, int height){
  return get_remove_button_y1(stacked,width,height) + button_height;
}

static int num_visitors;

struct ScopedVisitors{
  ScopedVisitors(){
    num_visitors++;
  }
  ~ScopedVisitors(){
    num_visitors--;
  }
};

#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))

#define MOVE_WIDGET(widget) do{ \
  int x1=get_##widget##_x1(stacked,width,height); \
  int y1=get_##widget##_y1(stacked,width,height); \
  int x2=get_##widget##_x2(stacked,width,height); \
  int y2=get_##widget##_y2(stacked,width,height); \
  widget.move(MAX(xborder,x1),MAX(yborder,y1));    \
  int daswidth = MIN(x2-x1,width-xborder);            \
  int dasheight = MIN(y2-y1,height-yborder);          \
  if (daswidth>0 && dasheight>0) widget.setFixedSize(daswidth,dasheight); \
  }while(0);

class BlockSelector : public QWidget
{
  Q_OBJECT

public:
  BlockSelector(QWidget *parent)
    : QWidget(parent) //, Qt::Dialog)
    , add_button("->",this)
    , remove_button("<-",this)
    , playlist(this)
    , blocklist(this)
    , last_shown_width(0) // frustrating: SETTINGS_read_int((char*)"blocklist_width",0))
  {
    button_width = add_button.width();
    button_height = add_button.height();

    blocklist.show();
    playlist.show();
    add_button.show();
    remove_button.show();

    playlist.insertItem(" "); // Make it possible to put a block at the end of the playlist.

    resizeEvent(NULL);

#if USE_QT3
    connect(&blocklist, SIGNAL(highlighted(int)), this, SLOT(blocklist_highlighted(int)));
    connect(&blocklist, SIGNAL(selected(int)), this, SLOT(blocklist_selected(int)));
    connect(&playlist, SIGNAL(highlighted(int)), this, SLOT(playlist_highlighted(int)));
    connect(&playlist, SIGNAL(selected(int)), this, SLOT(playlist_selected(int)));
#endif

#if USE_QT4
    connect(&blocklist, SIGNAL(currentRowChanged(int)), this, SLOT(blocklist_highlighted(int)));
    connect(&blocklist, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(blocklist_doubleclicked(QListWidgetItem*)));
    connect(&playlist, SIGNAL(currentRowChanged(int)), this, SLOT(playlist_highlighted(int)));
    connect(&playlist, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(playlist_doubleclicked(QListWidgetItem*)));
#endif

    connect(&add_button, SIGNAL(pressed()), this, SLOT(add_to_playlist()));
    connect(&remove_button, SIGNAL(pressed()), this, SLOT(remove_from_playlist()));

    setWidgetColors(this);

    {
      QFontMetrics fm(QApplication::font());
      int width = fm.width("0: 0/Pretty long name");
      setFixedWidth(width);
    }
  }

  void resizeEvent(QResizeEvent *qresizeevent){
    //fprintf(stderr,"I am resized\n");

    int width = this->width();
    int height = this->height();
    bool stacked = do_stack(width, height);

    if(width<=0 || height<=0){
      printf("Warning: %d<=0 || %d<=0\n",width,height);
      return;
    }

    MOVE_WIDGET(blocklist);
    MOVE_WIDGET(playlist);
    MOVE_WIDGET(add_button);
    MOVE_WIDGET(remove_button);
  }

  QPushButton add_button;
  QPushButton remove_button;
  QListBox playlist;
  QListBox blocklist;

  int last_shown_width;

private slots:

  void add_to_playlist(void){
#ifdef USE_QT3
    int pos = playlist.currentItem();
#else
    int pos = playlist.currentRow();
#endif
    if(pos==-1)
      return;

    BL_insertCurrPos(playlist.currentItem(), getBlockFromNum(blocklist.currentItem()));

    playlist.setSelected(pos+1, true);
    if(pos<(int)playlist.count()-1)
      root->curr_playlist = pos;
  }

  void remove_from_playlist(void){
    int num = playlist.currentItem();
    if(num==-1)
      return;
    if(playlist.count()<=2)
      return;

    BL_deleteCurrPos(num);

    printf("remove from playlist\n");
  }

  void blocklist_highlighted(int num){
    if(num==-1)
      return;
    //printf("num: %d\n",num);

    if(num_visitors>0) // event created internally
      return;

    bool wasplaying = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;

    PlayStop();

    struct Tracker_Windows *window=getWindowFromNum(-1);
    struct WBlocks *wblock=getWBlockFromNum(-1,num);
    if(wblock->curr_realline == wblock->num_reallines-1)
      wblock->curr_realline = 0;

    DO_GFX(SelectWBlock(window,wblock));
    EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
    editor->updateEditor();

    if(wasplaying)
      PlayBlockFromStart(window,true);
  }

  void blocklist_doubleclicked(QListWidgetItem *item){
    add_to_playlist();
  }

  void playlist_highlighted(int num){
    if(num==-1)
      return;
    if(num==(int)playlist.count()-1)
      return;

    if(num_visitors>0) // event created internally
      return;

    PlayStop();
    root->curr_playlist = num;
  }

  void playlist_selected(int num){
    remove_from_playlist();
  }

  void playlist_doubleclicked(QListWidgetItem *item){
    remove_from_playlist();
  }
};


static BlockSelector *bs;

QWidget *create_blockselector(){
  ScopedVisitors v;

  bs = new BlockSelector (NULL);
  return bs;
}

void BS_resizewindow(void){
  ScopedVisitors v;
}

void BS_UpdateBlockList(void){
  ScopedVisitors v;

  while(bs->blocklist.count()>0)
    bs->blocklist.removeItem(0);

  struct Blocks *block=root->song->blocks;
  while(block!=NULL){
    bs->blocklist.insertItem(QString::number(block->l.num)+": "+QString(block->name));
    block=NextBlock(block);
  }

  //PyRun_SimpleString(temp);

  BS_SelectBlock(root->song->tracker_windows->wblock->block);  
}

void BS_UpdatePlayList(void){
  ScopedVisitors v;

  int pos = bs->playlist.currentItem();//root->curr_playlist;

  while(bs->playlist.count()>0)
    bs->playlist.removeItem(0);

  int lokke=0;
  struct Blocks *block=BL_GetBlockFromPos(lokke);

  while(block!=NULL){
    bs->playlist.insertItem(QString::number(lokke)+": "+QString::number(block->l.num)+"/"+QString(block->name));

    lokke++;
    block=BL_GetBlockFromPos(lokke);
  }

  bs->playlist.insertItem(" "); // Make it possible to put a block at the end of the playlist.

  BS_SelectPlaylistPos(pos);
}

void BS_SelectBlock(struct Blocks *block){
  ScopedVisitors v;
  bs->blocklist.setSelected(block->l.num, true);
}

void BS_SelectPlaylistPos(int pos){
  ScopedVisitors v;
  //printf("selectplaylistpos %d\n",pos);
  if(pos==-1)
    return;
  bs->playlist.setSelected(pos, true);
}

#if 0
static void set_widget_width(int width){
  QWidget *main_window = static_cast<QWidget*>(root->song->tracker_windows->os_visual.main_window);
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  QSplitter *splitter = editor->xsplitter;

  QValueList<int> currentSizes = splitter->sizes();
  bs->last_shown_width = currentSizes[1];
  currentSizes[0] = main_window->width()-width;
  currentSizes[1] = width;
  splitter->setSizes(currentSizes);
}
#endif

static bool is_hidden = false;

void GFX_PlayListWindowToFront(void){
  ScopedVisitors v;

  //set_widget_width(bs->last_shown_width > 30 ? bs->last_shown_width : 200);
  bs->show();

#if 0
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  QSplitter *splitter = editor->xsplitter;
  splitter->addWidget(bs);
#endif

  is_hidden = false;
}

void GFX_PlayListWindowToBack(void){
  ScopedVisitors v;

#if 0
  // Perhaps a value in percentage of screen would work. But the default size (200) is best anyway.
  if(bs->width() > 30)
    SETTINGS_write_int((char*)"blocklist_width",bs->width());
#endif

  //set_widget_width(0);
  bs->hide();
  is_hidden = true;
}

void GFX_showHidePlaylist(struct Tracker_Windows *window){
#if 0
  if(bs->width() < 10)
    GFX_PlayListWindowToFront();
  else
    GFX_PlayListWindowToBack();
#endif
  if(is_hidden)
    GFX_PlayListWindowToFront();
  else
    GFX_PlayListWindowToBack();
}

#include "mQt_Bs_edit.cpp"
