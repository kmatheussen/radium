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

#include <math.h>

#include <QFileInfo>

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/visual_proc.h"
#include "../api/api_common_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/gfx_proc.h"
#include "../common/settings_proc.h"
#include "../common/seqtrack_proc.h"
#include "../common/undo_sequencer_proc.h"

#include "../audio/SampleReader_proc.h"

#include "../api/api_proc.h"

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "Qt_MyQScrollBar.hpp"
#include "Qt_MyQButton.h"

#include "../common/OS_Bs_edit_proc.h"
#include "Qt_Bs_edit_proc.h"



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
  bool is_blocklist;
  
  QListBox(QWidget *parent, bool is_blocklist)
    : QListWidget(parent)
    , is_blocklist(is_blocklist)
  {
    setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));
  }
  void insertItem(QString text){
    QListWidget::addItem(text);
  }
  void insertItem(const char *text){
    insertItem(QString(text));
  }
  int currentItem(){
    return QListWidget::currentRow();
  }
  QString currentText(){
    return QListWidget::currentItem()->text();
  }
  void setSelected(int pos, bool something){
    QListWidget::setCurrentRow(pos);
  }
  void removeItem(int pos){
    QListWidget::takeItem(pos);
  }

  void keyPressEvent ( QKeyEvent * event ) override {
    event->ignore();
  }

  void enterEvent(QEvent *event) override {
    setCursor(Qt::ArrowCursor);
  }

  Qt::MouseButton _last_button =  Qt::NoButton;
  
  // popup menu
  void mousePressEvent(QMouseEvent *event) override {

    _last_button = event->button();
      
    QListWidget::mousePressEvent(event);

    if(_last_button==Qt::RightButton) { // Prevent Qt from emiting doubleclick signals when right-clicking.

      if (shiftPressed()==false){
        if(is_blocklist)
          evalScheme("(FROM_C-show-blocklist-popup-menu)");
        else
          evalScheme("(FROM_C-show-playlist-popup-menu)");
      }

      event->accept();
      
    } else {
      

    }

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

namespace{

  // Note: Can only be used temporarily since 'seqblock' is not gc protected.
  //
  struct PlaylistElement{

    struct SeqBlock *seqblock;
    int seqblocknum;

    bool is_current;

    bool is_last = false;
    
  private:
    
    int64_t _pause;

  public:

    static PlaylistElement last(void){
      PlaylistElement pe;
      pe.is_last = true;
      return pe;
    }
    
    static PlaylistElement pause(int seqblocknum, struct SeqBlock *seqblock, int64_t pause, bool is_current){
      R_ASSERT(pause > 0);
      
      PlaylistElement pe;
      
      pe.seqblocknum = seqblocknum;
      pe.seqblock = seqblock;
      pe._pause = pause;
      pe.is_current = is_current;
      
      return pe;
    }

    static PlaylistElement block(int seqblocknum, struct SeqBlock *seqblock, bool is_current){
      PlaylistElement pe;
      
      pe.seqblocknum = seqblocknum;
      pe.seqblock = seqblock;
      pe._pause = 0;
      pe.is_current = is_current;
      
      return pe;
    }

    static PlaylistElement illegal(void){
      PlaylistElement pe;
      
      pe.seqblocknum = -1;
      pe.seqblock = NULL;
      pe._pause = 0;
      
      return pe;
    }
    
    bool is_legal(void) const {
      return seqblocknum >= 0;
    }
    
    bool is_illegal(void) const {
      return !is_legal();
    }
    
    bool is_pause(void) const {
      return _pause > 0;
    }

    int64_t get_pause(void) const {
      R_ASSERT(_pause > 0);
      return _pause;
    }

  };
}


static QVector<PlaylistElement> get_playlist_elements(void){

  QVector<PlaylistElement> ret;
    
  struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
  
  double current_seq_time = ATOMIC_DOUBLE_GET_RELAXED(seqtrack->start_time_nonrealtime);
  
  double last_end_seq_time = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, gfx_seqblocks(seqtrack)){

    int64_t next_last_end_seq_time = seqblock->t.time2;
        
    int64_t pause_time = seqblock->t.time - last_end_seq_time;

    if (pause_time > 0){
      bool is_current = current_seq_time >= last_end_seq_time && current_seq_time < seqblock->t.time;
      PlaylistElement pe = PlaylistElement::pause(iterator666, seqblock, pause_time, is_current);
      ret.push_back(pe);
    }
    
    {
      bool is_current = current_seq_time >= seqblock->t.time && current_seq_time < next_last_end_seq_time;
      PlaylistElement pe = PlaylistElement::block(iterator666, seqblock, is_current);
      ret.push_back(pe);
    }
    
    last_end_seq_time = next_last_end_seq_time;
    
  }END_VECTOR_FOR_EACH;

  return ret;
}

static PlaylistElement get_playlist_element(int num){
  if(num < 0)
    goto ret_illegal;

  {
    QVector<PlaylistElement> elements = get_playlist_elements();
    
    if (num >= elements.size())
      goto ret_illegal;

    return elements.at(num);
  }
  
 ret_illegal:
  return PlaylistElement::illegal();
}
  

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
  return height-button_height;//-yborder;
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

// move up button
static int get_move_up_button_x1(bool stacked, int width, int height){
  return get_add_button_x1(stacked, width, height);
}
static int get_move_up_button_x2(bool stacked, int width, int height){
  return get_add_button_x2(stacked, width, height);
}
static int get_move_up_button_y1(bool stacked, int width, int height){
  return get_playlist_y2(stacked, width, height);
}
static int get_move_up_button_y2(bool stacked, int width, int height){
  return height;
}

// move down button
static int get_move_down_button_x1(bool stacked, int width, int height){
  return get_remove_button_x1(stacked, width, height);
}
static int get_move_down_button_x2(bool stacked, int width, int height){
  return get_remove_button_x2(stacked, width, height);
}
static int get_move_down_button_y1(bool stacked, int width, int height){
  return get_move_up_button_y1(stacked, width, height);
}
static int get_move_down_button_y2(bool stacked, int width, int height){
  return get_move_up_button_y2(stacked, width, height);
}


static int num_visitors = 0;

namespace{
struct ScopedVisitors{
  ScopedVisitors(){
    num_visitors++;
  }
  ~ScopedVisitors(){
    num_visitors--;
  }
};
}

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


int g_default_slider_height = 20;

namespace{
class BlockSelector : public QWidget
{
  Q_OBJECT
  
public:

  int _last_selected_audiofilenum = 0;

  BlockSelector(QWidget *parent)
    : QWidget(parent) //, Qt::Dialog)
    , add_button("Insert",this)
    , remove_button("Remove",this)
    , move_down_button(QString::fromUtf8("\u21e9"), this)
    , move_up_button(QString::fromUtf8("\u21e7"), this)
    , playlist(this, false)
    , blocklist(this, true)
    , last_shown_width(0) // frustrating: SETTINGS_read_int((char*)"blocklist_width",0))
  {
    
    QFont sansFont;

    sansFont.fromString("Cousine,11,-1,5,75,0,0,0,0,0");
    sansFont.setStyleName("Bold");
    sansFont.setPointSize(QApplication::font().pointSize());
        
    //blocklist.setFont(sansFont);
    //playlist.setFont(sansFont);
    
    //add_button.setFont(sansFont);
    //remove_button.setFont(sansFont);
    move_down_button.setFont(sansFont);
    move_up_button.setFont(sansFont);

    g_default_slider_height = sansFont.pointSize()+5;
    
    add_button.setMaximumHeight(g_default_slider_height);
    remove_button.setMaximumHeight(g_default_slider_height);
    move_down_button.setMaximumHeight(g_default_slider_height);
    move_up_button.setMaximumHeight(g_default_slider_height);

    button_width = add_button.width();
    button_height = add_button.height();

    blocklist.show();
    playlist.show();
    add_button.show();
    remove_button.show();
    move_down_button.show();
    move_up_button.show();

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
    //connect(&playlist, SIGNAL(currentRowChanged(int)), this, SLOT(playlist_highlighted(int)));
    connect(&playlist, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(playlist_doubleclicked(QListWidgetItem*)));
#endif

    connect(&blocklist, SIGNAL(itemPressed(QListWidgetItem*)), this, SLOT(blocklist_itemPressed(QListWidgetItem*)));
    connect(&playlist, SIGNAL(itemPressed(QListWidgetItem*)), this, SLOT(playlist_itemPressed(QListWidgetItem*)));
      
    connect(&add_button, SIGNAL(pressed()), this, SLOT(add_to_playlist()));
    connect(&remove_button, SIGNAL(pressed()), this, SLOT(remove_from_playlist()));

    connect(&move_down_button, SIGNAL(pressed()), this, SLOT(move_down()));
    connect(&move_up_button, SIGNAL(pressed()), this, SLOT(move_up()));

    setWidgetColors(this);

    {
      QFontMetrics fm(QApplication::font());
      int width = fm.width("0: 0/Pretty long name");
      setFixedWidth(width);

      add_button.setMaximumWidth(width/2);
      remove_button.setMaximumWidth(width/2);
      move_down_button.setMaximumWidth(width/2);
      move_up_button.setMaximumWidth(width/2);
    }

    button_width = add_button.width();
  }

  /*
  Qt::MouseButton _last_button =  Qt::NoButton;
  void mousePressEvent( QMouseEvent *qmouseevent) override {
    _last_button = qmouseevent->button();
    QWidget::mousePressEvent(qmouseevent);
  }
  */
  
  void enterEvent(QEvent *event) override {
    setCursor(Qt::ArrowCursor);
  }

  void resizeEvent(QResizeEvent *qresizeevent) override {
    radium::ScopedResizeEventTracker resize_event_tracker;
    
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
    MOVE_WIDGET(move_down_button);
    MOVE_WIDGET(move_up_button);
  }
  
  void call_very_often(void){
    //struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();

    if (is_playing() && pc->playtype==PLAYSONG) {
      QVector<PlaylistElement> elements = get_playlist_elements();

      int i;
      
      for(i = 0 ; i < elements.size() ; i++){
        const PlaylistElement &el = elements.at(i);
        if (el.is_current==true)
          break;
      }
      
      if (i != playlist.currentRow()){
        playlist.setSelected(i, true);
      }
    }
  }


#if 0
  QPushButton add_button;
  QPushButton remove_button;
  QPushButton move_down_button;
  QPushButton move_up_button;
#else
  MyQButton add_button;
  MyQButton remove_button;
  MyQButton move_down_button;
  MyQButton move_up_button;
#endif
  QListBox playlist;
  QListBox blocklist;

  int last_shown_width;

  struct SeqBlock *get_prev_playlist_block(void){
    QVector<PlaylistElement> elements = get_playlist_elements();
    
    for(int pos = playlist.currentRow() - 1 ; pos > 0 ; pos--)
      if (elements.at(pos).is_pause()==false)
        return elements.at(pos).seqblock;
    
    return NULL;
  }
                           
  struct SeqBlock *get_next_playlist_block(void){
    QVector<PlaylistElement> elements = get_playlist_elements();
    
    for(int pos = playlist.currentRow() + 1 ; pos < elements.size()  ; pos++)
      if (elements.at(pos).is_pause()==false)
        return elements.at(pos).seqblock;
    
    return NULL;
  }

  struct SeqBlock *get_seqblock_from_pos(int pos){
    const PlaylistElement pe = get_playlist_element(pos);
    
    if (pe.is_illegal())
      return NULL;
    
    return pe.seqblock;
  }

  struct Blocks *get_block_from_pos(int pos){
    const PlaylistElement pe = get_playlist_element(pos);
    
    if (pe.is_illegal())
      return NULL;
    
    return pe.seqblock->block;
  }

public slots:

  void add_to_playlist(void){

    struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
    
#ifdef USE_QT3
    int pos = playlist.currentItem();
#else
    int pos = playlist.currentRow();
#endif
    const PlaylistElement pe = get_playlist_element(pos);

    radium::ScopedUndo undo; // This line prevents the audio file from being deleted in the call to ADD_UNDO(Sequencer()) if the file in the call to SEQTRACK_insert_sample is marked as deletable.
    
    ADD_UNDO(Sequencer());
    
    if (pe.is_illegal()) {
      
      // Append block

      const struct SeqBlock *last_seqblock = (const struct SeqBlock*)VECTOR_last(&seqtrack->seqblocks);
      int64_t seqtime = last_seqblock==NULL ? 0 : SEQBLOCK_get_seq_endtime(last_seqblock);

      if (seqtrack->for_audiofiles) {
        vector_t filenames = SAMPLEREADER_get_all_filenames();
        if (filenames.num_elements > 0) {
          SEQTRACK_insert_sample(seqtrack, get_seqtracknum(seqtrack), (wchar_t*)filenames.elements[filenames.num_elements-1], seqtime, -1);
        }
        
      } else {

        if (blocklist.currentItem() < root->song->num_blocks){
          struct Blocks *block = getBlockFromNum(blocklist.currentItem());
          SEQTRACK_insert_block(seqtrack, block, seqtime, -1);
        }

      }
      
    } else {

      // Insert block
      
      int64_t seqtime = pe.seqblock->t.time;
      
      if (pe.is_pause())
        seqtime -= pe.get_pause();
      
      // Uncomment this line to keep same pause time if adding the block to a pause entry:
      //
      // SEQTRACK_insert_silence(seqtrack, seqtime, getBlockSTimeLength(block));
      
      if (seqtrack->for_audiofiles) {
        
        vector_t filenames = SAMPLEREADER_get_all_filenames();
        
        if (blocklist.currentItem() < filenames.num_elements){
          wchar_t *filename = (wchar_t*)filenames.elements[blocklist.currentItem()];
          QFileInfo info(STRING_get_qstring(filename));
          if (blocklist.currentText().endsWith(QString(info.fileName()))) {
            
            double samplerate = SAMPLEREADER_get_samplerate(filename);
            int64_t duration = SAMPLEREADER_get_sample_duration(filename);
            
            if (fabs(samplerate-pc->pfreq) > 1)
              duration = (double)duration * pc->pfreq / samplerate;
            
            SEQTRACK_insert_silence(seqtrack, seqtime, duration);
            SEQTRACK_insert_sample(seqtrack, get_seqtracknum(seqtrack), filename, seqtime, -1);
                                    
          }
        }
        
      } else {

        if (blocklist.currentItem() < root->song->num_blocks){
          struct Blocks *block = getBlockFromNum(blocklist.currentItem());
          SEQTRACK_insert_block(seqtrack, block, seqtime, -1);
        }
      }
    }
    
    playlist.setSelected(pos+1, true);
  }

  void remove_from_playlist(void){
    int pos = playlist.currentItem();

    const PlaylistElement pe = get_playlist_element(pos);
    if (pe.is_illegal())
      return;

    ADD_UNDO(Sequencer());
    
    if (pe.is_pause())
      SEQTRACK_move_all_seqblocks_to_the_right_of(SEQUENCER_get_curr_seqtrack(), pe.seqblocknum, -pe.get_pause());
    else{
      int64_t seqblock_duration = SEQBLOCK_get_seq_duration(pe.seqblock);
      SEQTRACK_delete_seqblock(SEQUENCER_get_curr_seqtrack(), pe.seqblock, true);
      SEQTRACK_move_all_seqblocks_to_the_right_of(SEQUENCER_get_curr_seqtrack(), pe.seqblocknum, -1 * seqblock_duration);
    }
    
      //BL_deleteCurrPos(num);

    //BS_UpdatePlayList();
    
    printf("remove from playlist\n");
  }

  void swap(int pos1, int pos2){
    struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
      
    QVector<PlaylistElement> elements = get_playlist_elements();

    R_ASSERT_RETURN_IF_FALSE(pos1 >= 0);
    R_ASSERT_RETURN_IF_FALSE(pos1 < elements.size());
    R_ASSERT_RETURN_IF_FALSE(pos2 >= 0);
    R_ASSERT_RETURN_IF_FALSE(pos2 < elements.size());
    
    const PlaylistElement &element1 = elements.at(pos1);
    const PlaylistElement &element2 = elements.at(pos2);

    struct SeqBlock *seqblock1 = element1.seqblock;
    struct SeqBlock *seqblock2 = element2.seqblock;

    ADD_UNDO(Sequencer());
        
    if (element2.is_pause()){
      
      SEQTRACK_move_seqblock(seqtrack, seqblock1, seqblock1->t.time + element2.get_pause());
      
    } else if (element1.is_pause()){

      SEQTRACK_move_seqblock(seqtrack, seqblock2, seqblock2->t.time - element1.get_pause());
      
    } else {

      //printf("Bef: %f %f\n", (double)seqblock1->time/MIXER_get_sample_rate(), (double)seqblock2->time/MIXER_get_sample_rate());

      hash_t *seqblock1_state = SEQBLOCK_get_state(seqtrack, seqblock1, true);
      HASH_put_int(seqblock1_state, ":seqtracknum", get_seqtracknum(seqtrack));
      HASH_remove(seqblock1_state, ":start-time");
      HASH_remove(seqblock1_state, ":end-time");
      int64_t new_start_time1 = seqblock1->t.time + seqblock2->t.time2-seqblock2->t.time;
      HASH_put_int(seqblock1_state, ":start-time", new_start_time1);
      HASH_put_int(seqblock1_state, ":end-time", new_start_time1 + SEQBLOCK_get_seq_duration(seqblock1));
                   
      {
        radium::PlayerPause pause; // Not necessary for correct operation, but to avoid restart the player more than once.

        int64_t seqblock1_time = seqblock1->t.time;
        SEQTRACK_delete_seqblock(seqtrack, seqblock1, false);
        SEQTRACK_move_seqblock(seqtrack, seqblock2, seqblock1_time);
        SEQBLOCK_insert_seqblock_from_state(seqblock1_state, SHOW_ASSERTION);
        //SEQTRACK_insert_seqblock(seqtrack, seqblock1, SEQBLOCK_get_seq_endtime(seqblock2), -1);
      }

      //printf("Aft2: %f %f\n", (double)seqblock1->time/MIXER_get_sample_rate(), (double)seqblock2->time/MIXER_get_sample_rate());
            
    }

    SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_PLAYLIST);
  }

  void move_down(void){
    int pos= playlist.currentItem();
    if(pos==-1)
      return;
    if (pos>=playlist.count()-2)
      return;

    swap(pos, pos+1);
    
    playlist.setSelected(pos+1, true);
    
  }
  
  void move_up(void){
    int pos = playlist.currentItem();
    if(pos<=0)
      return;
    if (pos==playlist.count()-1)
      return;

    swap(pos-1, pos);
        
    playlist.setSelected(pos-1, true);
  }
  
  void blocklist_highlighted(int num){
    if(num==-1)
      return;

    if(num_visitors>0) // event created internally
      return;

    EVENTLOG_add_event("BlockSelector::blocklist_highlighted");
    //printf("block high num: %d\n",num);

    struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
    R_ASSERT_RETURN_IF_FALSE(seqtrack!=NULL);
    
    if (seqtrack->for_audiofiles==true)
      _last_selected_audiofilenum = num;
    else
      selectBlock(num, -1);
  }

  void blocklist_doubleclicked(QListWidgetItem *item){
    if (blocklist._last_button==Qt::LeftButton)
      add_to_playlist();
  }

  void blocklist_itemPressed(QListWidgetItem * item){
    printf("pressed: %d\n",(int)QApplication::mouseButtons());
    
    if (QApplication::mouseButtons()==Qt::RightButton){
      
      // First make sure this one is selected.
      int pos = blocklist.currentItem();
      blocklist_highlighted(pos);
      
      if (shiftPressed()){
        struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
        if (seqtrack->for_audiofiles) {

          vector_t filenames = SAMPLEREADER_get_all_filenames();
          if (pos < filenames.num_elements) {

            const wchar_t *filename = static_cast<const wchar_t*>(filenames.elements[pos]);
            
            SEQUENCER_remove_sample_from_song(filename);
            SAMPLEREADER_remove_filename_from_filenames(filename);
            
          }
          
        } else {
          
          deleteBlock(pos,-1);
          
        }
      }
    }
  }

  /*
  void playlist_highlighted(int num){
  playlist_itemPressed(NULL);
  }
  */
  
  void playlist_itemPressed(QListWidgetItem *){
    //printf("pressed 2: %d\n",(int)QApplication::mouseButtons());
    
    if(num_visitors>0) // event created internally
      return;

    if (QApplication::mouseButtons()==Qt::RightButton){
      
      if (shiftPressed()){
        remove_from_playlist();
      }

    } else {

      int num = playlist.currentItem();
      
      struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
      
      if (num>=playlist.count()-1) {
        if (is_playing_song())
          PlaySong(SEQTRACK_get_length(seqtrack)*MIXER_get_sample_rate()); // why was this line added here?
        return;
      }
      
      PlaylistElement pe = get_playlist_element(num);
      if (pe.is_illegal())
        return;
      
      int seqblocknum = pe.seqblocknum;

      int64_t abstime, seqtime;
      
      //SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);

      struct SeqBlock *seqblock = (struct SeqBlock*)seqtrack->seqblocks.elements[seqblocknum];

      if (pe.is_pause()) {
        if (seqblocknum==0) {
          abstime = 0;
          seqtime = 0;
        } else {
          struct SeqBlock *prev_seqblock = (struct SeqBlock*)seqtrack->seqblocks.elements[seqblocknum-1];
          abstime = prev_seqblock->t.time2;
          seqtime = prev_seqblock->t.time2;
        }
      } else {
        abstime = seqblock->t.time;
        seqtime = seqblock->t.time;
      }      

      if (is_playing_song()==false) {

        if (seqblock->block!=NULL) {
        
          struct Tracker_Windows *window=getWindowFromNum(-1);
          struct WBlocks *wblock=getWBlockFromNum(-1,seqblock->block->l.num);
          if(wblock->curr_realline == wblock->num_reallines-1)
            wblock->curr_realline = 0;
          
          PC_PauseNoMessage();{
            ATOMIC_DOUBLE_SET(pc->song_abstime, abstime);
            //ATOMIC_DOUBLE_SET(seqtrack->start_time_f, seqtime);
            printf("seqtime: %d\n",(int)seqtime);
            DO_GFX(SelectWBlock(window,wblock));
          }PC_StopPause_ForcePlayBlock(NULL);
          
        } else {
          
          ATOMIC_DOUBLE_SET(pc->song_abstime, abstime);
          
        }
          
        SEQUENCER_update(SEQUPDATE_TIME);
              
      } else {
        
        PlaySong(abstime);
        
      }
        
    }
  }
  
  void playlist_selected(int num){
    remove_from_playlist();
  }

  void playlist_doubleclicked(QListWidgetItem *item){
    if (playlist._last_button==Qt::LeftButton)
      remove_from_playlist();
  }
};
}


static BlockSelector *g_bs = NULL;
static bool g_is_hidden = false;


QWidget *create_blockselector(void){
  ScopedVisitors v;

  g_bs = new BlockSelector (NULL);

  return g_bs;
}

void PLAYLIST_insert(void){
  ScopedVisitors v;
  g_bs->add_to_playlist();
}

void PLAYLIST_remove(void){
  ScopedVisitors v;
  g_bs->remove_from_playlist();
}

void BS_resizewindow(void){
  ScopedVisitors v;
}

void BS_UpdateBlockList(void){
  ScopedVisitors v;

  if (g_is_hidden)
    return;
  
  while(g_bs->blocklist.count()>0){
    delete g_bs->blocklist.takeItem(0);
  }

  struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();

  if (seqtrack->for_audiofiles){

    vector_t filenames = SAMPLEREADER_get_all_filenames();
    int justify = log10(filenames.num_elements) + 1;

    VECTOR_FOR_EACH(const wchar_t*, filename, &filenames){

      QFileInfo info(STRING_get_qstring(filename));
      g_bs->blocklist.insertItem(QString::number(iterator666).rightJustified(justify, ' ')+": "+info.fileName());
      
    }END_VECTOR_FOR_EACH;

    if (filenames.num_elements > 0){
      g_bs->_last_selected_audiofilenum = R_MIN(filenames.num_elements-1, g_bs->_last_selected_audiofilenum);
      g_bs->blocklist.setCurrentRow(g_bs->_last_selected_audiofilenum);
    }
    
  } else {
    
    int justify = log10(root->song->num_blocks) + 1;
    //printf("justify: %d\n", justify);
    
    struct Blocks *block=root->song->blocks;
    while(block!=NULL){
      g_bs->blocklist.insertItem(QString::number(block->l.num).rightJustified(justify, ' ')+": "+QString(block->name));
      block=NextBlock(block);
    }

    //PyRun_SimpleString(temp);

    if(root != NULL
       && root->song != NULL
       && root->song->tracker_windows != NULL
       && root->song->tracker_windows->wblock != NULL
       && root->song->tracker_windows->wblock->block != NULL
       )
      BS_SelectBlock(root->song->tracker_windows->wblock->block);
  }
}

void BS_UpdatePlayList(void){
  ScopedVisitors v;

  //printf("  updateplaylist start\n");
  if (g_is_hidden)
    return;
  
  if (g_bs==NULL)
    return;
  
  if (root->song->seqtracks.num_elements==0)
    return;

  int orgpos = g_bs->playlist.currentItem();

  struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
  
  int justify_playlist = log10(gfx_seqblocks(seqtrack)->num_elements) + 1;
  
  QVector<PlaylistElement> elements = get_playlist_elements();
  {
    PlaylistElement pe = PlaylistElement::last();
    elements.push_back(pe);
  }
  
  int num_elements = elements.size();
  int num_items = g_bs->playlist.count();
  int num_iterations = R_MAX(num_elements, num_items);

  bool has_modified = false;
  
  QVector<QListWidgetItem*> to_remove;
  
  int pos = 0;
  for(int i=0;i<num_iterations;i++){
    const PlaylistElement *element = i < num_elements ? &elements.at(i) : NULL;
    QListWidgetItem *item = i < num_items ? g_bs->playlist.item(i) : NULL;

    if (element==NULL) {

      to_remove.push_back(item);
      has_modified = true;
      
    } else {

      QString new_text;
      if (element->is_last) {
        new_text = " "; // Make it possible to put a block at the end of the playlist.
      } else if (element->is_pause()){
        new_text = " pause: "+radium::get_time_string(element->get_pause());
      } else {
        QString seqblockname = get_seqblock_name(seqtrack, element->seqblock, "/", true);
        new_text = QString::number(pos).rightJustified(justify_playlist, ' ') + ": " + seqblockname;
        pos++;
      }
      
      if (item==NULL) {
        g_bs->playlist.insertItem(new_text);
        has_modified = true;
      } else if (new_text != item->text()) {
        item->setText(new_text);
        has_modified = true;
      }
    }

  }

  if (has_modified==false)
    return;
  
  for(auto *item : to_remove)
    delete item;
  
  BS_SelectPlaylistPos(orgpos);
}

/*
void BS_UpdatePlayList_old(void){
  ScopedVisitors v;

  int pos = bs->playlist.currentItem();//root->curr_playlist;

  while(bs->playlist.count()>0)
    bs->playlist.removeItem(0);

  int lokke=0;
  struct Blocks *block=BL_GetBlockFromPos(lokke);

  int justify_playlist = log10(root->song->length) + 1;
  int justify_blocklist = log10(root->song->num_blocks) + 1;
    
  while(block!=NULL){    
    bs->playlist.insertItem(QString::number(lokke).rightJustified(justify_playlist, ' ')+": "+QString::number(block->l.num).rightJustified(justify_blocklist, ' ')+"/"+QString(block->name));

    lokke++;
    block=BL_GetBlockFromPos(lokke);
  }

  bs->playlist.insertItem(" "); // Make it possible to put a block at the end of the playlist.

  BS_SelectPlaylistPos(pos);
}
*/

void BS_SelectBlock(struct Blocks *block){
  ScopedVisitors v;
  g_bs->blocklist.setSelected(block->l.num, true);
}

void BS_SelectBlocklistPos(int pos){
  {
    ScopedVisitors v;
    //printf("selectblocklistpos %d, %d. Length: %d\n",pos, pos % (g_bs->blocklist.count()), g_bs->blocklist.count());

    pos = pos % int(g_bs->blocklist.count());

    int safe = 1000;
    while(pos < 0 && safe > 0){
      pos += g_bs->blocklist.count();
      safe--;
    }

    EVENTLOG_add_event("BS_SelectBlocklistPos 1");
    
    g_bs->blocklist.setSelected(pos, true);
  }

  EVENTLOG_add_event("BS_SelectBlocklistPos 2");
  
  g_bs->blocklist_highlighted(pos);
  //selectBlock(pos, -1);
  //g_bs->blocklist.setSelected(pos, true);
  
  //printf("selectblocklistpos. Curr pos: %d", g_bs->blocklist.currentRow());
}

void BS_SelectPlaylistPos(int pos){
  {
    ScopedVisitors v;
    //printf("selectplaylistpos %d, %d. Length: %d\n",pos, pos % (g_bs->playlist.count()), g_bs->playlist.count());
    pos = pos % int(g_bs->playlist.count());

    int safe = 1000;
    while(pos < 0 && safe > 0){
      pos += g_bs->playlist.count();
      safe--;
    }
    
    g_bs->playlist.setSelected(pos, true);
  }
  
  g_bs->playlist_itemPressed(NULL);
}

struct SeqBlock *BS_GetPrevPlaylistBlock(void){
  ScopedVisitors v;
  return g_bs->get_prev_playlist_block();
}
  
struct SeqBlock *BS_GetNextPlaylistBlock(void){
  ScopedVisitors v;
  return g_bs->get_next_playlist_block();
}

struct SeqBlock *BS_GetSeqBlockFromPos(int pos){
  ScopedVisitors v;
  return g_bs->get_seqblock_from_pos(pos);
}

/*
struct Blocks *BS_GetBlockFromPos(int pos){
  ScopedVisitors v;
  return g_bs->get_block_from_pos(pos);
}
*/

int BS_GetCurrBlocklistPos(void){
  ScopedVisitors v;
  return g_bs->blocklist.currentRow();
}

int BS_GetCurrPlaylistPos(void){
  ScopedVisitors v;
  return g_bs->playlist.currentRow();
}

void BS_call_very_often(void){
  ScopedVisitors v;
  if (is_called_every_ms(70))
    return g_bs->call_very_often();
}

#if 0
static void set_widget_width(int width){
  QWidget *main_window = static_cast<QWidget*>(root->song->tracker_windows->os_visual.main_window);
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  radium::Splitter *splitter = editor->xsplitter;

  QValueList<int> currentSizes = splitter->sizes();
  g_bs->last_shown_width = currentSizes[1];
  currentSizes[0] = main_window->width()-width;
  currentSizes[1] = width;
  splitter->setSizes(currentSizes);
}
#endif

bool GFX_PlaylistWindowIsVisible(void){
  return !g_is_hidden;
}

void GFX_PlayListWindowToFront(void){
  ScopedVisitors v;

  //set_widget_width(g_bs->last_shown_width > 30 ? g_bs->last_shown_width : 200);
  g_bs->show();
  
#if 0
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  radium::Splitter *splitter = editor->xsplitter;
  splitter->addWidget(g_bs);
#endif

  g_is_hidden = false;
}

void GFX_PlayListWindowToBack(void){
  ScopedVisitors v;

#if 0
  // Perhaps a value in percentage of screen would work. But the default size (200) is best anyway.
  if(g_bs->width() > 30)
    SETTINGS_write_int((char*)"blocklist_width",g_bs->width());
#endif

  //set_widget_width(0);
  g_bs->hide();
  g_is_hidden = true;
}

void GFX_showHidePlaylist(struct Tracker_Windows *window){
  ScopedVisitors v;

#if 0
  if(g_bs->width() < 10)
    GFX_PlayListWindowToFront();
  else
    GFX_PlayListWindowToBack();
#endif
  if(g_is_hidden)
    GFX_PlayListWindowToFront();
  else
    GFX_PlayListWindowToBack();
}

#include "mQt_Bs_edit.cpp"
