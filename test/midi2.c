
#include <alsa/asoundlib.h>
#include <alsa/seq_event.h>



int main(){
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;
  int  client;
  int myport;
  int ret;
  int queue;
  snd_seq_t *seq;
  int lokke;

  snd_seq_client_info_alloca(&cinfo);
  snd_seq_client_info_set_client(cinfo, -1);

  snd_seq_open(&seq,"default",SND_SEQ_OPEN_DUPLEX,0);
  //snd_seq_open(&toseq,"hw",SND_SEQ_OPEN_DUPLEX,0);

  queue = snd_seq_alloc_queue(seq);

  myport = snd_seq_create_simple_port(seq, 
				      "radium out",
				      SND_SEQ_PORT_CAP_NO_EXPORT |
				      SND_SEQ_PORT_CAP_READ,
				      SND_SEQ_PORT_TYPE_MIDI_GENERIC |
				      SND_SEQ_PORT_TYPE_APPLICATION );


  snd_seq_start_queue( seq, queue, NULL );

  while (snd_seq_query_next_client(seq, cinfo) >= 0){
    client = snd_seq_client_info_get_client(cinfo);
    snd_seq_port_info_alloca(&pinfo);
    snd_seq_port_info_set_client(pinfo, client);
    snd_seq_port_info_set_port(pinfo, -1);
    if(client==0) continue;
    //if (client == m_client || client == 0) continue;
    printf("Name: %s\n",snd_seq_client_info_get_name(cinfo));
    if(!strcmp("Hydrogen",snd_seq_client_info_get_name(cinfo))){
      while (snd_seq_query_next_port(seq, pinfo) >= 0 ){
	int cap =  snd_seq_port_info_get_capability(pinfo);

	if ( snd_seq_port_info_get_client(pinfo) != 0 ){
	  if ( (cap & SND_SEQ_PORT_CAP_SUBS_WRITE) != 0){
	    printf("gakk %s\n", snd_seq_port_info_get_name(pinfo));
	    break;
	  }
	}
      }
      break;
    }
  }

  ret = snd_seq_connect_to(
			   seq, 
			   myport,
			   snd_seq_port_info_get_client(pinfo),
			   snd_seq_port_info_get_port(pinfo)
			   );


  printf("myport: %d, connectret: %d\n",myport,ret);

  for(lokke=0;lokke<0x7f;lokke++){
    unsigned char buffer[3]={0x90,lokke,0x70};
    snd_seq_event_t ev;
    snd_midi_event_t *midi_ev;
    snd_midi_event_new( 10, &midi_ev );

    snd_seq_ev_clear( &ev );
    snd_midi_event_encode(midi_ev,
			  buffer,
			  3,
			  &ev); 
    snd_midi_event_free( midi_ev );

    snd_seq_ev_set_source(&ev,myport);
    snd_seq_ev_set_subs(&ev);

    snd_seq_ev_schedule_tick( &ev, queue, 1, 1);
    
    snd_seq_event_output(seq, &ev);

    snd_seq_drain_output(seq);
  }

  {
    char temp[500];
    gets(&temp);
  }

  return 0;

}
