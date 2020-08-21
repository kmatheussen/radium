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

#include <math.h>

#include <QFileInfo>

#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/visual_proc.h"
#include "../api/api_common_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/settings_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/undo_sequencer_proc.h"

#include "../audio/SampleReader_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_sequencer_proc.h"
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



//static const int xborder = 0;
//static const int yborder = 0;

//static int button_height = 30;
//static int button_width = 50;

namespace{

  // Note: Can only be used temporarily since 'seqblock' is not gc protected.
  //
  struct PlaylistElement{

    struct SeqBlock *seqblock = NULL;
    int seqblocknum = -1;

    bool is_current = false;

    bool is_last = false;
    
  private:
    
    int64_t _pause = 0;

  public:

    //auto operator<=>(const Point&) const = default; 
    bool equal_to(const PlaylistElement &b) const {
      return
        seqblock==b.seqblock &&
        seqblocknum==b.seqblocknum &&
        is_current==b.is_current &&
        is_last==b.is_last &&
        _pause==b._pause;
    }
    
    static PlaylistElement last(bool is_current){
      PlaylistElement pe;
      pe.is_last = true;
      pe.is_current = is_current;
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

static inline bool operator==(const PlaylistElement &a, const PlaylistElement &b){
  return a.equal_to(b);
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

  bool is_current = current_seq_time >= last_end_seq_time;
  PlaylistElement pe = PlaylistElement::last(is_current);
  ret.push_back(pe);

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
  

static int g_num_visitors = 0;

namespace{
struct ScopedVisitors{
  ScopedVisitors(){
    g_num_visitors++;
  }
  ~ScopedVisitors(){
    g_num_visitors--;
  }
};
}


int g_default_slider_height = 20;

static QFont bs_font;
void set_default_slider_height(void){
  bs_font.fromString("Cousine,11,-1,5,75,0,0,0,0,0");
  bs_font.setStyleName("Bold");
  bs_font.setPointSize(QApplication::font().pointSize());
  
  g_default_slider_height = bs_font.pointSize()+4;
}



//static BlockSelector *g_bs = NULL;
static bool g_is_hidden = false;

static QWidget  *g_block_and_playlist = NULL;


QWidget *BS_get(void){
  ScopedVisitors v;

  /*
  if(g_bs==NULL)
    g_bs = new BlockSelector (NULL);

    return g_bs;
  */
  
  if (g_block_and_playlist==NULL){
#if 0
    radium::KeyboardFocusFrame *frame = new radium::KeyboardFocusFrame(g_main_window, radium::KeyboardFocusFrameType::EDITOR, true);
    
    int64_t guinum = S7CALL2(int_void, "FROM_C-create-bock-and-playlist-gui");
    
    QWidget *bs = API_gui_get_widget(guinum);

    frame->layout()->addWidget(bs);
    
    g_block_and_playlist = frame;
#else
    int64_t guinum = S7CALL2(int_void, "FROM_C-create-bock-and-playlist-gui");
    
    g_block_and_playlist = API_gui_get_widget(guinum);
#endif
  }

  return g_block_and_playlist;
}

void BS_resizewindow(void){
  ScopedVisitors v;
}

void BS_UpdateBlockList(void){
  ScopedVisitors v;

  S7CALL2(void_void, "FROM_C-recreate-block/audio-list-guis");

}

static void update_playlist(void){

  //struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
  
  static QVector<PlaylistElement> prev_elements;
  
  QVector<PlaylistElement> elements = get_playlist_elements();

  if (prev_elements.size() != elements.size())
    goto update_it;
  
  for(int i=0;i<elements.size();i++){    
    const PlaylistElement &element_a = prev_elements.at(i);
    const PlaylistElement &element_b = elements.at(i);

    if (!(element_a==element_b))
      goto update_it;
  }

  goto finish;

 update_it:
  S7CALL2(void_void, "FROM_C-recreate-playlist-area");

 finish:
  prev_elements = elements;
}
  
void BS_UpdatePlayList(void){
  ScopedVisitors v;

  //printf("  updateplaylist start\n");

  // Must update if hidden because of BS_GetCurrPlaylistklistPos()
  //if (g_is_hidden)
  //  return;
  
  if (g_block_and_playlist==NULL)
    return;
  
  if (root->song->seqtracks.num_elements==0)
    return;

  update_playlist();
}

void BS_SelectBlock(struct Blocks *block){
  ScopedVisitors v;
  //g_bs->blocklist.setSelected(block->l.num, true);
  R_ASSERT_NON_RELEASE(currentBlock(-1)==block->l.num);
  BS_UpdateBlockList();
  S7CALL2(void_void, "FROM_C-ensure-curr-block-is-visible-in-blocklist");
}

void BS_SelectPlaylistPos(int pos, bool change_song_pos_too){
  {
    ScopedVisitors v;
    //printf("selectplaylistpos %d, %d. Length: %d\n",pos, pos % (g_bs->playlist.count()), g_bs->playlist.count());

    //int orgpos = pos;

    //update_playlist();
      
    if(change_song_pos_too || !is_playing_song())
      setCurrPlaylistPos(pos, true, change_song_pos_too);
  }
}

struct SeqBlock *BS_GetSeqBlockFromPos(int pos){ 
  ScopedVisitors v;
  const PlaylistElement pe = get_playlist_element(pos);
  
  if (pe.is_illegal())
    return NULL;
  
  return pe.seqblock;
}

void BS_call_very_often(void){
  // Must call if hidden because of BS_GetCurrPlaylistklistPos()
  //if (g_is_hidden)
  //  return;
  
  ScopedVisitors v;
  
  if (is_called_every_ms(70)){

    if (is_playing() && pc->playtype==PLAYSONG)
      API_setCurrPlaylistPos_while_playing();
  }
}

bool GFX_PlaylistWindowIsVisible(void){
  return !g_is_hidden;
}

void GFX_PlayListWindowToFront(void){
  ScopedVisitors v;

  if (sequencerInFullMode() && !sequencerInWindow()){
    setSequencerInFullMode(false);    
  }

  g_block_and_playlist->show();
  
  g_is_hidden = false;
}

void GFX_PlayListWindowToBack(void){
  ScopedVisitors v;

  g_block_and_playlist->hide();
  g_is_hidden = true;
}

void GFX_showHidePlaylist(struct Tracker_Windows *window){
  ScopedVisitors v;

  bool must_show = false;
  
  if (sequencerInFullMode() && !sequencerInWindow()){
    setSequencerInFullMode(false);
    must_show = true;
  }

  if(g_is_hidden)
    GFX_PlayListWindowToFront();
  else
    if (!must_show)
      GFX_PlayListWindowToBack();
}
