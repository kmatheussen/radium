


/*
XI loading made by looking at the documents "xi.txt" and "xm.txt", which was included
with the source code of SoundTracker 0.6.8. 

xi.txt says: "reverse engineered by KB / The Obsessed Maniacs / Reflex"
xm.txt says: "By Mr.H of Triton in 1994."
*/






static int xi_get_sample_number_for_note(disk_t *file, int note_num){
  if(note_num>95)
    note_num=95;
  DISK_set_pos(file, 0x42+note_num);
  return read_8int(file);
}

static int xi_get_num_samples(disk_t *file){ //i.e. the number of sounds in the file. Not number of frames.
  DISK_set_pos(file,0x128);
  return read_le16int(file);
}

static int xi_get_loop_type(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 14);
  return read_8int(file) & 3;
}

static int xi_get_bits_per_frame(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 14);
  if (read_8int(file) & 16)
    return 16;
  else
    return 8; // I assume. Documentation doesn't say anything.
}

static int xi_get_bytes_per_frame(disk_t *file, int sample_num){
  return xi_get_bits_per_frame(file,sample_num) / 8;
}

static int xi_get_num_frames(disk_t *file, int sample_num){
  /*
  char size_chars[4] = {0};
  
  DISK_set_pos(file,0x12a + (0x28*sample_num));

  if(DISK_read_binary(file, size_chars, 4) != 4)
    fprintf(stderr,"Reading file failed\n");
  */
  
  DISK_set_pos(file,0x12a + (0x28*sample_num));
    
  int numbytes = read_le32int(file);
  int bytes_per_frame = xi_get_bytes_per_frame(file, sample_num);
  int num_frames = numbytes / bytes_per_frame;

  /*
  printf("%x %x %x %x: %d\n",(unsigned int)size_chars[0],(unsigned int)size_chars[1],(unsigned int)size_chars[2],(unsigned int)size_chars[3], get_le_32(size_chars));
  printf("numbytes: %d, bytes_per_frame: %d, num_frames: %d\n", numbytes, bytes_per_frame, num_frames);
  //getchar();
  */
  
  return num_frames;
}

static int xi_get_loop_start(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 4);
  int num_bytes = read_le32int(file);
  return num_bytes / xi_get_bytes_per_frame(file, sample_num);
}

static int xi_get_loop_end(disk_t *file, int sample_num){
  int start = xi_get_loop_start(file,sample_num);
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 8);
  int num_bytes = read_le32int(file);
  return start + (num_bytes / xi_get_bytes_per_frame(file, sample_num));
}

static float xi_get_sample_volume(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 12);
  return (float)read_8int(file) / (float)0x40;
}


static float xi_get_sample_finetune(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 13);
  return read_8int_signed(file); // -128 -> 128
}

#if 0
static int xi_get_middle_note(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 16);
  int transpose = read_8int_signed(file);
  printf("transpose for %d: %d\n",sample_num,transpose);
  return 48+transpose; // 48 = "C-4". (And from xm.txt: 0 == "C-0")
}
#endif

static int xi_get_relnote(disk_t *file, int sample_num){
  DISK_set_pos(file,0x12a + (0x28*sample_num) + 16);
  return read_8int_signed(file);
}

static void xi_seek_to_sample(disk_t *file, int sample_num){
  int num_samples=xi_get_num_samples(file);

  int64_t pos = 0x12a + (0x28*num_samples);

  int i;
  for(i=0;i<sample_num;i++){
    int num_frames = xi_get_num_frames(file,i);
    int bytes_per_frame = xi_get_bytes_per_frame(file,i);
    pos += (num_frames*bytes_per_frame);
  }

  DISK_set_pos(file, pos);
}

#if 0
typedef struct{
  disk_t *file;
  int8_t prev_sample_value_8;  // must be same type as data since it wraps around
  int16_t prev_sample_value_16;  // must be same type as data since it wraps around
  int bits_per_sample;
} xi_sample_reader_t;

static float xi_read_next_sample(xi_sample_reader_t *sample_reader){
  if(sample_reader->bits_per_sample==16){

    int16_t delta = read_le16int(sample_reader->file);
    sample_reader->prev_sample_value_16 += delta;
    return sample_reader->prev_sample_value_16 / 32768.0f;

  }else{ // I assume bits_per_sample is 8 now

    int8_t delta = read_8int_signed(sample_reader->file);
    sample_reader->prev_sample_value_8 += delta;
    return sample_reader->prev_sample_value_8 / 128.0f;

  }
}
#endif

static float *xi_get_sample(disk_t *file, int sample_num){
  int    num_frames     = xi_get_num_frames(file,sample_num);
  float *sample         = (float*)V_calloc(sizeof(float),num_frames);
  int    bits_per_frame = xi_get_bits_per_frame(file,sample_num);

  if(sample==NULL){
    GFX_Message(NULL, "Out of memory? Failed to allocate %d bytes\n",num_frames*sizeof(float));
    return NULL;
  }

  xi_seek_to_sample(file, sample_num);

  if(bits_per_frame==16){
    int num_bytes = sizeof(int16_t) * num_frames;
    int16_t *s16=(int16_t*)V_calloc(1, num_bytes);
    if(s16==NULL){
      GFX_Message(NULL, "Out of memory? Failed to allocate %d bytes\n",num_bytes);
      return sample;
    }
    int64_t num_bytes_read = DISK_read_binary(file, s16, num_bytes);
    if(num_bytes_read != num_bytes){
      GFX_addMessage(talloc_format("Reading file failed. Expected %d bytes, read %d bytes.\n", num_bytes, (int)num_bytes_read));
    }
    
    convert_16_bit_little_endian_to_native(s16,num_frames);
    
    int16_t value=0; // must be same type as data since it wraps around
    int i;
    for(i=0;i<num_frames;i++){
      value     += s16[i];
      sample[i]  = value / 32768.0f;
    }
    V_free(s16);
  }else{
    int8_t *s8=(int8_t*)V_calloc(sizeof(int8_t),num_frames);
    if(s8==NULL){
      GFX_Message(NULL, "Out of memory? Failed to allocate %d bytes\n",num_frames);
      return sample;
    }
    if(DISK_read_binary(file, s8, num_frames) != num_frames)
      fprintf(stderr,"Reading file failed\n");

    int8_t value=0;  // must be same type as data since it wraps around
    int i;
    for(i=0;i<num_frames;i++){
      value     += s8[i];
      sample[i]  = value / 128.0f;
    }
    V_free(s8);
  }

#if 0
  // This could be slower since we are reading one and one sample. Haven't tested though, it's probably only insignificantly slower.
  // Hower, loading quickly is important when searching for sounds.
  xi_sample_reader_t sample_reader={file, 0, 0, bits_per_frame};

  int i;
  for(i=0;i<num_frames;i++)
    sample[i] = xi_read_next_sample(&sample_reader);
#endif

  return sample;
}

static double xi_get_frequency(disk_t *file, int note, int sample_num){
  double relnote = xi_get_relnote(file,sample_num);
  double finetune = xi_get_sample_finetune(file,sample_num);

  // this calculation is copied from xm.txt
  double period = 10*12*16*4 - (note+relnote)*16*4 - finetune/2;
  return 8363 * pow(2,((6*12*16*4 - period) / (12*16*4)));
}

// libsndfile supports xi. But:
//
// There's a lot of instruments which libsndfile doesn't open, for some reason. sndfile-info complains about "*** Sample count is less than 16 but more than 1.",
// whatever that means. However, I still want to use the files even if they have an error of some kind.
//
// Also, the interface for loading xi intruments with libsndfile is currently too limited, so we have to parse the files manually anyway.

bool load_xi_instrument(Data *data,const wchar_t *filename, bool set_loop_on_off){
  
  EVENTLOG_add_event(talloc_format("load_xi_instrument -%s-", STRING_get_chars(filename)));
  bool ret=false;

  disk_t *file=DISK_open_binary_for_reading(filename);
  if(file==NULL){
    fprintf(stderr,"Could not open file\n");
    return ret;
  }

  {
    char header_id[200]={0};
    if(DISK_read_binary(file, header_id,15)!=15){
      printf("File not big enough to be xi instrument.\n");
      goto exit;
    }
    
    if(strcmp(header_id,"Extended Instru")){
      printf("Header id says it is not an xi instrument: \"%s\".\n",header_id);
      goto exit;
    }
  }

  {
    int num_samples = xi_get_num_samples(file);

    data->num_different_samples = R_MIN(num_samples,MAX_NUM_SAMPLES);
  }

  {
    int sample_num;

    for(sample_num=0;sample_num<data->num_different_samples;sample_num++){
      Sample *sample = (Sample *)&data->samples[sample_num];

      sample->ch = -1;
      sample->volume = xi_get_sample_volume(file,sample_num);

      sample->num_frames   = xi_get_num_frames(file,sample_num);

      set_legal_loop_points(sample,-1,-1, set_loop_on_off); // By default, loop all.

      if(xi_get_loop_type(file,sample_num)!=0){ // TODO: Implement those other types of loops.
        set_legal_loop_points(sample,
                              xi_get_loop_start(file,sample_num),
                              xi_get_loop_end(file,sample_num),
                              set_loop_on_off
                              );
      }

      sample->sound = xi_get_sample(file, sample_num);
      
      if(sample->sound==NULL)
        goto exit;

      int note;
      for(note=0;note<128;note++){
        sample->frequency_table[note] = xi_get_frequency(file, note, sample_num);
      }      
    }
  }
  
  {
    int note_num;
    for(note_num=0;note_num<128;note_num++){
      int sample_num = xi_get_sample_number_for_note(file,note_num);
      Note *note = (Note*)&data->notes[note_num];
      
      note->num_samples = 1;
      note->samples[0]  = &data->samples[sample_num];
    }
  }

  ret=true;
  
 exit:

  if (DISK_close_and_delete(file)==false)
    return false;

  return ret;
}

