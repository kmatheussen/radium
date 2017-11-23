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


// Wav format: RIFFNEW.pdf

static char *get_current_wav_chunk_id(disk_t *file){
  char *val=(char*)talloc(5);
  
  if(DISK_read_binary(file, val, 4) != 4)
    fprintf(stderr,"Reading file failed\n");
  
  DISK_spool(file,-4);
  
  //printf("current_wav_chunk_id: \"%s\"\n",val);
  
  return val;
}

static bool spool_to_next_wav_chunk(disk_t *file, int64_t endpos){
  if(DISK_spool(file,4)==false) // chunk id
    return false; // end of file

  int size=read_le32int(file);
  if(size%2) // chunks are aligned by two bytes, but the size doesn't have to be.
    size++;

  if(DISK_pos(file)>=endpos-1)
    return false; // end of file


  if(size<4 || DISK_spool(file,size)==false){
    //RError("Broken wave file? Chunk size: %d\n",size);
    //GFX_Message(NULL, "Warning: Wav file seems to be a little bit broken. If you think this might be a bug in Radium, please send the wave file to k.s.matheussen@notam02.no");
    return false;
  }

  return true;
}

static bool spool_to_wav_chunk(disk_t *file, const char *chunk_id, int num){ // chunk_id is a 4 byte string
  DISK_set_pos(file,0);

  if(strcmp(get_current_wav_chunk_id(file),"RIFF"))
    return false;

  DISK_set_pos(file,4);

  int filesize=read_le32int(file);

  if(strcmp(get_current_wav_chunk_id(file),"WAVE"))
    return false;

  DISK_spool(file,4);

  int i=0;

  while(true){
    if(!strcmp(chunk_id,get_current_wav_chunk_id(file))){
      if(i==num)
        return true;
      else
        i++;
    }

    if(spool_to_next_wav_chunk(file,filesize)==false){
      //printf("wav chunk \"%s\" not found\n",chunk_id);
      return false;
    }
  }

  return false;
}

// seek must be set to 0x0c into the cue chunk (start of the list). It will not change this seek when returning.
static int find_loop_cue_pos(disk_t *file, int cue_id, int num_cues){
  //fseek(file,0x18,SEEK_CUR);
  long startpos = DISK_pos(file);
  int cue_pos=0;
  int ret = -1;

  while(read_le32int(file) != cue_id){   
    DISK_spool(file,0x14);
    cue_pos++;
    if(cue_pos==num_cues)
      goto exit;
  }

  DISK_spool(file,0x10);
  ret = read_le32int(file);
  
 exit:
  DISK_set_pos(file,startpos);
  return ret;
}

static int find_cue_id_for_label2(disk_t *file, const char *label){

  int64_t startpos=DISK_pos(file);

  DISK_spool(file,4); // "LIST"

  int list_size=read_le32int(file);
  if(list_size%2)
    list_size++;

  DISK_spool(file,4); // "adtl"

  int64_t end_pos=startpos+list_size;

  while(true){
    //printf("ftell(file): %d. end_pos: %d. startpos: %d, list_size: %d\n",(int)DISK_pos(file), (int)end_pos, (int)startpos, list_size);
    if(DISK_pos(file)>=end_pos)
      return -1;

    //printf("  LIST:  \"%s\"\n",get_current_wav_chunk_id(file));

    if(!strcmp("labl",get_current_wav_chunk_id(file))){
      DISK_spool(file,4);
      int size=read_le32int(file);
      if(size%2)
        size++;

      int id=read_le32int(file);

      char *name=(char*)talloc(size);
      if(DISK_read_binary(file, name, size-4) != (size-4)){
        //fprintf(stderr,"Reading file failed\n");
        return -1;
      }
      //printf("******** labl %d: \"%s\". size: %d\n",id,name,size);

      if(!strcmp(name,label))
        return id;

    }else{

      if(spool_to_next_wav_chunk(file,end_pos)==false){
        //printf("label \"%s\" not found.\n",label);
        return -1;
      }
    }

  }
  return -1;
}

static int find_cue_id_for_label(disk_t *file, const char *label){
  int i=0;
  while(true){
    if(spool_to_wav_chunk(file, "LIST", i)==false)
      return -1;

    int ret = find_cue_id_for_label2(file,label);
    if(ret!=-1)
      return ret;
    else
      i++;
  }

  return -1;
}

static bool set_wav_loop_points_using_cues(Sample &sample, disk_t *file, bool set_loop_on_off){
  int cue_id_loop_start=find_cue_id_for_label(file, "Loop Start");
  int cue_id_loop_end=find_cue_id_for_label(file, "Loop End");

  if(cue_id_loop_start==-1 || cue_id_loop_end==-1)
    return false;

  if(spool_to_wav_chunk(file, "cue ", 0)==false)
    return false;

  DISK_spool(file,8);
  int num_cues = read_le32int(file);

  int64_t loop_start = find_loop_cue_pos(file,cue_id_loop_start,num_cues);
  int64_t loop_end = find_loop_cue_pos(file,cue_id_loop_end,num_cues);

  set_legal_loop_points(sample, loop_start, loop_end, set_loop_on_off);

  //printf("*************** num_cues: %d. loop_start: %d, loop_end: %d\n",num_cues,(int)loop_start,(int)loop_end);
  return true;
}

#if 0
static int get_bytes_per_frame_in_wav(disk_t *file){
  if(spool_to_wav_chunk(file, "fmt ", 0)==-1)
    return -1;

  DISK_spool(file,0x0a);
  int num_channels = read_le16int(file);

  DISK_spool(file,0x16-0xa-2);
  int bits = read_le16int(file);
  if(bits%8!=0){
    float b=bits/8;
    b=ceil(b);
    bits = b*8;
  }
  
  return num_channels * bits / 8;
}
#endif

static bool set_wav_loop_points_using_smpl_chunk(Sample &sample, disk_t *file, bool set_loop_on_off){
  if(spool_to_wav_chunk(file, "smpl", 0)==false)
    return false;

  DISK_spool(file,0x24);
  int num_loops = read_le32int(file);

  if(num_loops==0)
    return false;

  DISK_spool(file,4 + 8);
  int loop_start = read_le32int(file);
  int loop_end = read_le32int(file);

#if 0
  int bytes_per_frame = get_bytes_per_frame_in_wav(file);
  if(bytes_per_frame<=0)
    return false;

  /*
  printf("HEPP: loop_start: %d (%d), loop_end: %d(%d), bytes_per_frame: %d\n",
         loop_start,loop_start/bytes_per_frame,
         loop_end,loop_end/bytes_per_frame,
         bytes_per_frame);
  */
  
  set_legal_loop_points(sample, loop_start/bytes_per_frame, loop_end/bytes_per_frame, set_loop_on_off);
#endif

  set_legal_loop_points(sample, loop_start, loop_end, set_loop_on_off);
  
  return true;
}

static void set_wav_loop_points(Sample &sample, const wchar_t *filename, bool set_loop_on_off){
  
  disk_t *file = DISK_open_binary_for_reading(filename);
  
  if(file==NULL){
    GFX_Message(NULL, "Could not open file \"%s\". libsndfile could open the file though. Something might be wrong with your disk.", STRING_get_chars(filename));
    return;
  }

  if(set_wav_loop_points_using_smpl_chunk(sample,file, set_loop_on_off)==false)
    set_wav_loop_points_using_cues(sample,file, set_loop_on_off);

  DISK_close_and_delete(file);
}

static double get_wav_middle_note_from_sampl_chunk(disk_t *file){
  
  if(spool_to_wav_chunk(file, "smpl", 0)==false)
    return -1;
  
  DISK_spool(file, 20);
  uint32_t midi_unity_note = read_le32uint(file);
  uint32_t midi_pitch_fraction = read_le32uint(file);
  int64_t u32maxplusone = 1;
  u32maxplusone = u32maxplusone<<32;
  double fraction = scale_double(midi_pitch_fraction, 0, u32maxplusone, 0, 1);
    
  double middle_note = midi_unity_note + fraction - 12;

  if (middle_note <= 0)
    middle_note += 12;
    

  if (middle_note < 0 || middle_note > 127)
    return -1;
  
  // ody-ld1.wav
  
  //printf("       UNITY: %u (%x)\n", midi_unity_note, midi_unity_note);
  //printf("       FRACT: %u (%x) (%f)\n", midi_pitch_fraction, midi_pitch_fraction, fraction);
  //getchar();

  return middle_note;
}

static double get_wav_middle_note_from_instr_chunk(disk_t *file){
  
  if(spool_to_wav_chunk(file, "inst", 0)==false)
    return -1;

  int unshifted_note = read_8int(file);
  int finetune = read_8int_signed(file);

  double middle_note = (double)(unshifted_note - 12) + scale_double(finetune, -50, 50, -0.5, 0.5);

  if (middle_note <= 0)
    middle_note += 12;

  if (middle_note <= 0 || middle_note > 127)
    return -1;

  return middle_note;
}

static double get_wav_middle_note(const wchar_t *filename, double default_middle_note){
  disk_t *file = DISK_open_binary_for_reading(filename);
  
  if(file==NULL){
    GFX_Message(NULL, "Could not open file \"%s\". libsndfile could open the file though, which is very strange.", STRING_get_chars(filename));
    return default_middle_note;
  }

  double middle_note = get_wav_middle_note_from_sampl_chunk(file);

  if (middle_note < 0)
    middle_note = get_wav_middle_note_from_instr_chunk(file);

  if (middle_note < 0)
    middle_note = default_middle_note;
  
  
  DISK_close_and_delete(file);
  
  return middle_note;
}
