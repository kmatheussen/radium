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


static char *get_current_wav_chunk_id(FILE *file){
  char *val=talloc(5);
  if(fread(val,4,1,file)!=1)
    fprintf(stderr,"Reading file failed\n");
  fseek(file,-4,SEEK_CUR);
  printf("current_wav_chunk_id: \"%s\"\n",val);
  return val;
}

static bool spool_to_next_wav_chunk(FILE *file, int endpos){
  if(fseek(file,4,SEEK_CUR) != 0) // chunk id
    return false; // end of file

  int size=read_le32int(file);
  if(size%2) // chunks are aligned by two bytes, but the size doesn't have to be.
    size++;

  if(ftell(file)>=endpos-1)
    return false; // end of file


  if(size<4 || fseek(file,size,SEEK_CUR) !=0 ){
    RError("Broken wave file? Chunk size: %d\n",size);
    return false;
  }

  return true;
}

static bool spool_to_wav_chunk(FILE *file, char *chunk_id, int num){ // chunk_id is a 4 byte string
  fseek(file,0,SEEK_SET);

  if(strcmp(get_current_wav_chunk_id(file),"RIFF"))
    return false;

  fseek(file,4,SEEK_SET);

  int filesize=read_le32int(file);

  if(strcmp(get_current_wav_chunk_id(file),"WAVE"))
    return false;

  fseek(file,4,SEEK_CUR);

  int i=0;

  while(true){
    if(!strcmp(chunk_id,get_current_wav_chunk_id(file))){
      if(i==num)
        return true;
      else
        i++;
    }

    if(spool_to_next_wav_chunk(file,filesize)==false){
      printf("wav chunk \"%s\" not found\n",chunk_id);
      return false;
    }
  }

  return false;
}

// seek must be set to 0x0c into the cue chunk (start of the list). It will not change this seek when returning.
static int find_loop_cue_pos(FILE *file, int cue_id, int num_cues){
  //fseek(file,0x18,SEEK_CUR);
  long startpos = ftell(file);
  int cue_pos=0;
  int ret = -1;

  while(read_le32int(file) != cue_id){   
    fseek(file,0x14,SEEK_CUR);
    cue_pos++;
    if(cue_pos==num_cues)
      goto exit;
  }

  fseek(file,0x10,SEEK_CUR);
  ret = read_le32int(file);
  
 exit:
  fseek(file,startpos,SEEK_SET);
  return ret;
}

static int find_cue_id_for_label2(FILE *file, char *label){

  int startpos=ftell(file);

  fseek(file,4,SEEK_CUR); // "LIST"

  int list_size=read_le32int(file);
  if(list_size%2)
    list_size++;

  fseek(file,4,SEEK_CUR); // "adtl"

  int end_pos=startpos+list_size;

  while(true){
    printf("ftell(file): %d. end_pos: %d. startpos: %d, list_size: %d\n",(int)ftell(file), end_pos,startpos,list_size);
    if(ftell(file)>=end_pos)
      return -1;

    printf("  LIST:  \"%s\"\n",get_current_wav_chunk_id(file));

    if(!strcmp("labl",get_current_wav_chunk_id(file))){
      fseek(file,4,SEEK_CUR);
      int size=read_le32int(file);
      if(size%2)
        size++;

      int id=read_le32int(file);

      char *name=talloc(size);
      if(fread(name,size-4,1,file)!=1){
        fprintf(stderr,"Reading file failed\n");
        return -1;
      }
      printf("******** labl %d: \"%s\". size: %d\n",id,name,size);

      if(!strcmp(name,label))
        return id;

    }else{

      if(spool_to_next_wav_chunk(file,end_pos)==false){
        printf("label \"%s\" not found.\n",label);
        return -1;
      }
    }

  }
  return -1;
}

static int find_cue_id_for_label(FILE *file, char *label){
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

static void set_wav_loop_points(Sample *sample, const char *filename){
  FILE *file=fopen(filename,"r");
  if(file==NULL){
    RError("Could not open file \"%s\". libsndfile could though, which is very strange",filename);
    return;
  }

  int cue_id_loop_start=find_cue_id_for_label(file, "Loop Start");
  int cue_id_loop_end=find_cue_id_for_label(file, "Loop End");

  if(cue_id_loop_start==-1 || cue_id_loop_end==-1)
    goto exit;

  if(spool_to_wav_chunk(file, "cue ", 0)==-1)
    goto exit;

  fseek(file,8,SEEK_CUR);
  int num_cues = read_le32int(file);

  int loop_start = find_loop_cue_pos(file,cue_id_loop_start,num_cues);
  int loop_end = find_loop_cue_pos(file,cue_id_loop_end,num_cues);

  set_legal_loop_points(sample, loop_start, loop_end);

  printf("*************** num_cues: %d. loop_start: %d, loop_end: %d\n",num_cues,loop_start,loop_end);

 exit:
  fclose(file);
}
