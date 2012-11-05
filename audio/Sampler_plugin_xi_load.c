


/*
XI loading made by looking at the documents "xi.txt" and "xm.txt", which was included
with the source code of SoundTracker 0.6.8. 

xi.txt says: "reverse engineered by KB / The Obsessed Maniacs / Reflex"
xm.txt says: "By Mr.H of Triton in 1994."
*/






static int xi_get_sample_number_for_note(FILE *file, int note_num){
  if(note_num>95)
    note_num=95;
  fseek(file,0x42+note_num,SEEK_SET);
  return read_8int(file);
}

static int xi_get_num_samples(FILE *file){ //i.e. the number of sounds in the file. Not number of frames.
  fseek(file,0x128,SEEK_SET);
  return read_le16int(file);
}

static int xi_get_loop_type(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 14,SEEK_SET);
  return read_8int(file) & 3;
}

static int xi_get_bits_per_frame(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 14,SEEK_SET);
  if (read_8int(file) & 16)
    return 16;
  else
    return 8; // I assume. Documentation doesn't say anything.
}

static int xi_get_bytes_per_frame(FILE *file, int sample_num){
  return xi_get_bits_per_frame(file,sample_num) / 8;
}

static int xi_get_num_frames(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num),SEEK_SET);
  return read_le32int(file) / xi_get_bytes_per_frame(file, sample_num);
}

static int xi_get_loop_start(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 4,SEEK_SET);
  return read_le32int(file) / xi_get_bytes_per_frame(file, sample_num);
}

static int xi_get_loop_end(FILE *file, int sample_num){
  int start = xi_get_loop_start(file,sample_num);
  fseek(file,0x12a + (0x28*sample_num) + 8,SEEK_SET);
  return start + (read_le32int(file) / xi_get_bytes_per_frame(file, sample_num));
}

static float xi_get_sample_volume(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 12,SEEK_SET);
  return read_8int(file) / (float)0x40;
}


static float xi_get_sample_finetune(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 13,SEEK_SET);
  return read_8int_signed(file); // -128 -> 128
}

#if 0
static int xi_get_middle_note(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 16,SEEK_SET);
  int transpose = read_8int_signed(file);
  printf("transpose for %d: %d\n",sample_num,transpose);
  return 48+transpose; // 48 = "C-4". (And from xm.txt: 0 == "C-0")
}
#endif

static int xi_get_relnote(FILE *file, int sample_num){
  fseek(file,0x12a + (0x28*sample_num) + 16,SEEK_SET);
  return read_8int_signed(file);
}

static void xi_seek_to_sample(FILE *file, int sample_num){
  int num_samples=xi_get_num_samples(file);

  fseek(file,0x12a + (0x28*num_samples),SEEK_SET);

  int i;
  for(i=0;i<sample_num;i++){
    long pos = ftell(file);
    int num_frames = xi_get_num_frames(file,i);
    int bytes_per_frame = xi_get_bytes_per_frame(file,i);
    fseek(file,pos + (num_frames*bytes_per_frame),SEEK_SET);
  }
}

#if 0
typedef struct{
  FILE *file;
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

static float *xi_get_sample(FILE *file, int sample_num){
  int    num_frames     = xi_get_num_frames(file,sample_num);
  float *sample         = calloc(sizeof(float),num_frames);
  int    bits_per_frame = xi_get_bits_per_frame(file,sample_num);

  if(sample==NULL){
    RError("Out of memory? Failed to allocate %d bytes\n",num_frames*sizeof(float));
    return NULL;
  }

  xi_seek_to_sample(file, sample_num);

  if(bits_per_frame==16){
    int16_t *s16=calloc(sizeof(int16_t),num_frames);
    if(s16==NULL){
      RError("Out of memory? Failed to allocate %d bytes\n",num_frames*2);
      return sample;
    }
    fread(s16, 1, 2*num_frames, file);
    convert_16_bit_little_endian_to_native(s16,num_frames);

    int16_t value=0; // must be same type as data since it wraps around
    int i;
    for(i=0;i<num_frames;i++){
      value     += s16[i];
      sample[i]  = value / 32768.0f;
    }
  }else{
    int8_t *s8=calloc(sizeof(int8_t),num_frames);
    if(s8==NULL){
      RError("Out of memory? Failed to allocate %d bytes\n",num_frames);
      return sample;
    }
    fread(s8, 1, num_frames, file);

    int8_t value=0;  // must be same type as data since it wraps around
    int i;
    for(i=0;i<num_frames;i++){
      value     += s8[i];
      sample[i]  = value / 128.0f;
    }
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

static double xi_get_frequency(FILE *file, int note, int sample_num){
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

bool load_xi_instrument(Data *data,const char *filename){
  bool ret=false;

  FILE *file=fopen(filename,"r");
  if(file==NULL){
    fprintf(stderr,"Could not open file\n");
    return ret;
  }

  {
    char header_id[200]={0};
    if(fread(header_id,1,15,file)!=15){
      printf("File not big enough to be xi instrument.\n");
      goto exit;
    }
    
    if(strcmp(header_id,"Extended Instru")){
      printf("Header id says it is not an xi instrument: \"%s\".\n",header_id);
      goto exit;
    }
  }

  int num_samples = xi_get_num_samples(file);

  data->num_different_samples = R_MIN(num_samples,MAX_NUM_SAMPLES);

  int sample_num;

  for(sample_num=0;sample_num<data->num_different_samples;sample_num++){
    Sample *sample = (Sample *)&data->samples[sample_num];

    sample->ch = -1;
    sample->volume = xi_get_sample_volume(file,sample_num);

    sample->num_frames   = xi_get_num_frames(file,sample_num);

    if(xi_get_loop_type(file,sample_num)!=0){ // TODO: Implement those other types of loops.
      set_legal_loop_points(sample,
                            xi_get_loop_start(file,sample_num),
                            xi_get_loop_end(file,sample_num));
    }

    sample->data = xi_get_sample(file, sample_num);
    if(sample->data==NULL)
      goto exit;

    int note;
    for(note=0;note<128;note++){
      sample->frequency_table[note] = xi_get_frequency(file, note, sample_num);
    }
  }

  int note_num;
  for(note_num=0;note_num<128;note_num++){
    int sample_num = xi_get_sample_number_for_note(file,note_num);
    Note *note = (Note*)&data->notes[note_num];

    note->num_samples = 1;
    note->samples[0]  = &data->samples[sample_num];
  }

  ret=true;
 exit:
  fclose(file);
  return ret;
}

