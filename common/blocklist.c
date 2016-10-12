/* Copyright 2000 Kjetil S. Matheussen

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


#include <string.h>
#include "nsmtracker.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "time_proc.h"
#include "list_proc.h"
#include "undo_playlist_proc.h"
#include "OS_Bs_edit_proc.h"
#include "visual_proc.h"
#include "scheduler_proc.h"

#include "blocklist_proc.h"


// Note: talloc_atomic could be used instead of talloc, since the blocks are stored elsewhere.
// However, in case of bugs, using talloc_atomic here could lead to crashes that would be very hard to find the origin of.


static struct SeqBlock *get_seqblock(struct Blocks *block){
  struct SeqBlock *seqblock = talloc(sizeof(struct SeqBlock));
  seqblock->block = block;
  seqblock->time = -1;
  return seqblock;
}

static void update_seqtrack_timing(struct SeqTrack *seqtrack){
  R_ASSERT(PLAYER_current_thread_has_lock());
    
  int64_t time = 0;
  
  VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){
    seqblock->time = time;
    time += getBlockSTimeLength(seqblock->block);
  }END_VECTOR_FOR_EACH;
}

static void update_seqtrack_timing2(void){
  update_seqtrack_timing(root->song->seqtracks.elements[0]);
}
  
void BL_init(void){
#ifdef TRACKER_DEBUG
	int lokke;
#endif

        // Playlist
        {
          root->song->playlist=talloc(sizeof(struct Blocks *));
          root->song->playlist[0]=root->song->blocks;
          root->song->length=1;
        }

        // Seqtracks
        {

          struct SeqTrack *seqtrack = talloc(sizeof(struct SeqTrack));
          seqtrack->scheduler = SCHEDULER_create();
          
          VECTOR_ensure_space_for_one_more_element(&seqtrack->seqblocks);

          VECTOR_ensure_space_for_one_more_element(&root->song->seqtracks);

          struct SeqBlock *seqblock = get_seqblock(root->song->blocks);
          
          PLAYER_lock();{
            
            VECTOR_clean(&root->song->seqtracks);

            VECTOR_push_back(&root->song->seqtracks, seqtrack);
            
            VECTOR_push_back(&seqtrack->seqblocks, seqblock);

            update_seqtrack_timing2();

          }PLAYER_unlock();
          
        }
                
	/*
#ifdef TRACKER_DEBUG
	debug("playlen: %d\n",root->song->length);
	for(lokke=0;lokke<root->song->length;lokke++){
		debug("pos: %d, block: %d\n",lokke,root->song->playlist[lokke]->l.num);
	}
#endif
	*/
}

int *BL_copy(void){
  int *playlist = talloc_atomic(sizeof(int)*(root->song->length+1)); // Note, this playlist is a list of block numbers, not a list of blocks. (that's why we use talloc_atomic)
  int pos;
  playlist[0] = root->song->length;
  for(pos=0;pos<root->song->length;pos++)
    playlist[pos+1]=root->song->playlist[pos]->l.num;
  return playlist;
}

void BL_paste2(struct Song *song, int *playlist){
  int len = playlist[0];

  // Playlist
  {
    struct Blocks **new=talloc(len*sizeof(struct Blocks *));
    for(int pos=0;pos<len;pos++)
      new[pos]=(struct Blocks *)ListFindElement1(&song->blocks->l,playlist[pos+1]);
    
    song->length = len;
    song->playlist = new;
  }
  
  // Seqtracks
  {
    R_ASSERT_RETURN_IF_FALSE(song!=NULL);

    vector_t seqblocks = {0};

    for(int pos=0;pos<len;pos++)
      VECTOR_push_back(&seqblocks, get_seqblock((struct Blocks *)ListFindElement1(&song->blocks->l,playlist[pos+1])));

    struct SeqTrack *seqtrack = (struct SeqTrack*)song->seqtracks.elements[0];
    
    PLAYER_lock();{
      
      seqtrack->seqblocks = seqblocks;

      update_seqtrack_timing2();
      
    }PLAYER_unlock();
  }

}

void BL_paste(int *playlist){
  BL_paste2(root->song, playlist);
}

void BL_insert(int pos,struct Blocks *block){
  PC_Pause();{
      
    // Playlist
    {
      struct Blocks **temp;
      struct Blocks **playlist=root->song->playlist;
    
      root->song->length++;
    
      temp=talloc(root->song->length*sizeof(struct Blocks *));
      memcpy(temp,playlist,(root->song->length-1)*sizeof(struct Blocks *));
    
      temp[pos]=block;
    
      for(int lokke=pos;lokke<root->song->length-1;lokke++){
        temp[lokke+1]=playlist[lokke];
      }
      root->song->playlist=temp;

#if 1
      printf("playlen: %d\n",root->song->length);
      for(int lokke=0;lokke<root->song->length;lokke++){
        printf("pos: %d, block: %d\n",lokke,temp[lokke]->l.num);
      }

#endif
    }
  
  
    // Seqtracks
    {
      VECTOR_ensure_space_for_one_more_element(root->song->seqtracks.elements[0]);
      struct SeqBlock *seqblock = get_seqblock(block);

      PLAYER_lock();{
        VECTOR_insert(root->song->seqtracks.elements[0], seqblock, pos);
        update_seqtrack_timing2();
      }PLAYER_unlock();
    }
  
  }PC_StopPause(NULL);              
}

void BL_insertCurrPos(int pos,struct Blocks *block){
  ADD_UNDO(Playlist());
  BL_insert(pos,block);
  BS_UpdatePlayList();
}

void BL_delete(int pos){
  PC_Pause();{
      
  // Playlist
  {
    int lokke;
    
    struct Blocks **playlist=root->song->playlist;
    
    for(lokke=pos ; lokke<root->song->length-1 ; lokke++){
      playlist[lokke]=playlist[lokke+1];
    }
    
    root->song->length--;
    
    if(root->song->length==0) { // Illegal with length 0.
      playlist[0] = root->song->blocks;
      root->song->length = 1;
    }

#if 1
    printf("||||||||||| playlen: %d\n",root->song->length);
    for(lokke=0;lokke<root->song->length;lokke++){
      printf("pos: %d, block: %d\n",lokke,playlist[lokke]->l.num);
    }
    printf("||||||||||| playlen: %d\n",root->song->length);
#endif

  }

  // Seqtracks
  {
    vector_t *seqblocks = root->song->seqtracks.elements[0];
    struct SeqBlock *seqblock = get_seqblock(root->song->blocks);

    PLAYER_lock();{
      
      VECTOR_delete(seqblocks, pos);
    
      if (seqblocks->num_elements==0)
        VECTOR_push_back(seqblocks, seqblock);
    
      update_seqtrack_timing2();
      
    }PLAYER_unlock();
  }

  }PC_StopPause(NULL);
}

void BL_deleteCurrPos(int pos){
  ADD_UNDO(Playlist());
  BL_delete(pos);
  BS_UpdatePlayList();
}

struct Blocks *BL_GetBlockFromPos(int pos){
	if(pos>=root->song->length || pos<0) return NULL;
	return root->song->playlist[pos];
}

static int get_first_block_pos(struct Blocks *block){
  int pos;

  for(pos=0;pos<root->song->length;pos++)
    if(root->song->playlist[pos]==block)
      return pos;

  return -1;
}

void BL_removeBlockFromPlaylist(struct Blocks *block){
  while(get_first_block_pos(block)!=-1){
    if(block==root->song->blocks && root->song->length==1){ // special case
      printf("what?\n");
      break;
    }
    BL_delete(get_first_block_pos(block));
  }
}

void BL_setLength(int length){
  ADD_UNDO(Playlist());

  if (length < 1) {
    GFX_Message(NULL, "Can not set playlist length to %d",length);
    return;
  }

  PC_Pause();{
    
    if (length > root->song->length) {
      
      struct Blocks **playlist = talloc(sizeof(struct Blocks*)*length);
      
      int pos;
      
      for(pos=0;pos<root->song->length;pos++)
        playlist[pos] = root->song->playlist[pos];
      
      for(;pos<length;pos++)
        playlist[pos] = playlist[root->song->length-1];
      
      root->song->playlist = playlist;

      vector_t *seqblocks = root->song->seqtracks.elements[0];

      for(int i=root->song->length;i<length;i++)
        VECTOR_ensure_space_for_one_more_element(seqblocks);

      struct SeqBlock *new_seqblocks[length];
      for(int i=root->song->length;i<length;i++)
        new_seqblocks[i] = get_seqblock(playlist[i]);
      
      PLAYER_lock();{
        
        for(int i=root->song->length;i<length;i++){
          //printf("  PUSH PUSH %d. len: %d\n",i,seqblocks->num_elements);
          VECTOR_push_back(seqblocks, new_seqblocks[i]);
          //printf(" Got it. Lnegth now: %d\n",seqblocks->num_elements);
        }

        update_seqtrack_timing2();
        
      }PLAYER_unlock();            
      
    } else {

      PLAYER_lock();{
        
        struct SeqTrack *seqtrack = root->song->seqtracks.elements[0];
        while(seqtrack->seqblocks.num_elements > length)
          VECTOR_delete_last(&seqtrack->seqblocks);

        update_seqtrack_timing2();
        
      }PLAYER_unlock();

    }


    root->song->length = length;
               
  }PC_StopPause(NULL);
  
  BS_UpdatePlayList();
}

void BL_setBlock(int pos, struct Blocks *block){
  if (pos < 0 || pos >= root->song->length){
    GFX_Message(NULL, "No such playlist pos: %d (pos must be between %d and %d)\n",pos, 0, root->song->length-1);
    return;
  }
    
  ADD_UNDO(Playlist());

  PC_Pause();{
    root->song->playlist[pos] = block;
    
    struct SeqBlock *seqblock = get_seqblock(block);
    
    PLAYER_lock();{
      
      VECTOR_set(root->song->seqtracks.elements[0], pos, seqblock);
      update_seqtrack_timing2();
      
    }PLAYER_unlock();
    
  }PC_StopPause(NULL);
  
  BS_UpdatePlayList();
}

void BL_moveDown(int pos){
  if (pos < 0 || pos >= root->song->length){
    GFX_Message(NULL, "No such playlist pos: %d (pos must be between %d and %d)\n",pos, 0, root->song->length-1);
    return;
  }

  if (pos==root->song->length-1)
    return;
  
  ADD_UNDO(Playlist());

  struct Blocks *old = root->song->playlist[pos+1];

  struct SeqBlock *seq1 = get_seqblock(root->song->playlist[pos]);
  struct SeqBlock *seq2 = get_seqblock(root->song->playlist[pos+1]);
  
  PC_Pause();{

    PLAYER_lock();{
      
      root->song->playlist[pos+1] = root->song->playlist[pos];
      VECTOR_set(root->song->seqtracks.elements[0], pos+1, seq1);
      
      root->song->playlist[pos] = old;
      VECTOR_set(root->song->seqtracks.elements[0], pos, seq2);
      
      update_seqtrack_timing2();

    }PLAYER_unlock();
    
  }PC_StopPause(NULL);
  
  BS_UpdatePlayList();
}

void BL_moveUp(int pos){
  if (pos < 0 || pos >= root->song->length){
    GFX_Message(NULL, "No such playlist pos: %d (pos must be between %d and %d)\n",pos, 0, root->song->length-1);
    return;
  }

  if (pos==0)
    return;

  ADD_UNDO(Playlist());

  struct Blocks *old = root->song->playlist[pos-1];
  struct SeqBlock *seq1 = get_seqblock(root->song->playlist[pos]);
  struct SeqBlock *seq2 = get_seqblock(root->song->playlist[pos-1]);

  PC_Pause();{

    PLAYER_lock();{
          
      root->song->playlist[pos-1] = root->song->playlist[pos];
      VECTOR_set(root->song->seqtracks.elements[0], pos-1, seq1);
      
      root->song->playlist[pos] = old;
      VECTOR_set(root->song->seqtracks.elements[0], pos, seq2);

      update_seqtrack_timing2();

    }PLAYER_unlock();
    
  }PC_StopPause(NULL);
  
  BS_UpdatePlayList();
}
