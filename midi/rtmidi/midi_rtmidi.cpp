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


#include <assert.h>
#include <pthread.h>

#include "rtmidi-2.0.0/RtMidi.h"

#include "../../common/nsmtracker.h"
#include "../../common/memory_proc.h"
#include "../../common/OS_visual_input.h"
#include "../midi_i_input_proc.h"

#include "../OS_midi_proc.h"

struct MyMidiPortOs{
  RtMidiOut *midiout;
  double last_time;
};

// (The *PutMidi* API needs to be cleaned up)
static int midi_msg_len(int m1){
  if((m1 & 0xf0) == 0xc0)
    return 2;

  if((m1 & 0xf0) == 0xd0)
    return 2;

  if(m1>=0xf0){
    switch(m1){
    case 0xf0:
      return 0;
    case 0xf1:
      return 2;
    case 0xf2:
      return 3;
    case 0xf3:
      return 2;
    case 0xf4:
      return 0;
    case 0xf5:
      return 2;
    case 0xf6:
      return 1;
    case 0xf7:
      return 0;
    default:
      return 1;
    }
  }

  return 3;
}

#ifdef __linux__
static RtMidiIn *inport_jack;
static RtMidiIn *inport_alsa;
#endif

#ifdef FOR_WINDOWS
static RtMidiIn *inport_winmm;
static RtMidiIn *inport_winks;
#endif

static std::vector<unsigned char> message1;
static std::vector<unsigned char> message2;
static std::vector<unsigned char> message3;

// A scoped Lock class that protects midiout->sendmessage
class ScopedPutMidiLock{
public:
  ScopedPutMidiLock() : is_locked( false ) {
    lock();
  }
  ~ScopedPutMidiLock() {
    unlock();
  }
  void lock() {
    if ( is_locked == false ) {
      pthread_mutex_lock( &putmidi_lock );
      is_locked = true;
    }
  }
  void unlock() {
    if ( is_locked == true ) {
      pthread_mutex_unlock( &putmidi_lock );
      is_locked = false;
    }
  }

private:
  bool is_locked;
  static pthread_mutex_t putmidi_lock;
};

pthread_mutex_t ScopedPutMidiLock::putmidi_lock = PTHREAD_MUTEX_INITIALIZER;

static double startup_time = -1.0;

#include "../../common/playerclass.h"
extern PlayerClass *pc;


void OS_PlayFromStart(MidiPortOs port){
#if 0
  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
  startup_time = RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi());
#endif
}


extern LANGSPEC void OS_InitMidiTiming();

// quick hack, called from player.c (TODO: find a better API without making spaghetti) (well, maybe this is the cleanest way...)
void OS_InitMidiTiming(void){
  startup_time = RtMidiOut::getCurrentTime(RtMidi::UNIX_JACK);
}

void OS_PutMidi(MidiPortOs port,
                int cc,
                int data1,
                int data2,
                STime time
                )
{
  if(port==NULL)
    return;

  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);

#if 0
  if(true || time<0)
    printf("got midi. time: %f. startup_time: %f, jack time: %f. %x/%x/%x (%d/%d/%d)\n",
           ((double)time/(double)PFREQ),(float)startup_time,(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()),
           cc,data1,data2,cc,data1,data2
           );
#endif

  //printf("current time: %f\n",(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()));

  double rtmidi_time;

  if(time<0)
    rtmidi_time=myport->last_time; // Necessary to ensure midi messages are sent out in the same order as they got in to this function.
  else{
    rtmidi_time=startup_time + ((double)(time+LATENCY)/(double)PFREQ);
    if(rtmidi_time<myport->last_time)
      rtmidi_time=myport->last_time;  // Necessary to ensure all midi messages are sent in order.
  }

  myport->last_time = rtmidi_time;

  //printf("got midi: %x,%x,%x at time %f (rtmidi_time: %f) (current_time: %f)\n",cc,data1,data2,(float)time/(double)PFREQ,rtmidi_time,(float)RtMidiOut::getCurrentTime(midiout->getCurrentApi()));

  int len = midi_msg_len(cc);
  if(len==0)
    return;

  {
    ScopedPutMidiLock lock; // Sometimes, the GUI wants to send midi signals, and that's why we need a lock here.
                            // Don't think any effect caused by priority inheritance should be an issue when the user drags sliders, etc.

    if(len==1){
      message1[0]=cc;
      myport->midiout->sendMessage(&message1,rtmidi_time);
      return;
    }

    if(len==2){
      message2[0]=cc;
      message2[1]=data1;
      myport->midiout->sendMessage(&message2,rtmidi_time);
      return;
    }

    message3[0]=cc;
    message3[1]=data1;
    message3[2]=data2;
    myport->midiout->sendMessage(&message3,rtmidi_time);
  }
}


void OS_GoodPutMidi(MidiPortOs port,
                    int cc,
                    int data1,
                    int data2,
                    STime time,
                    uint32_t maxbuff
                 )
{
  OS_PutMidi(port,cc,data1,data2,time);
}


static const std::vector<std::string> get_port_names(RtMidi &rtmidi){
  std::vector<std::string> ret;
  unsigned int nPorts = rtmidi.getPortCount();


  for (unsigned int i=0; i<nPorts; i++ ) {
    std::string portName;
    try {
      portName = rtmidi.getPortName(i);
    }
    catch (RtError &error) {
      RError(error.what());
      continue;
    }
    std::cout << "  Output Port #" << i+1 << ": " << portName << '\n';
    ret.push_back(portName);
  }

  return ret;
}


static char **vector_to_cstring_array(const std::vector<std::string> &strings, int *num_ports){
  *num_ports = 0;
  char **ret = (char**)talloc(sizeof(char*)*strings.size());

  for(unsigned int i=0;i<strings.size();i++)
    if(strings[i] != "Radium:in" && strings[i] != "Radium:0"){
      ret[*num_ports] = talloc_strdup((char*)strings[i].c_str());
      *num_ports = *num_ports + 1;
    }
  return ret;
}

static char **get_port_names(bool use_input_ports, int *num_ports){
  std::vector<std::string> ret;

  std::vector<RtMidi::Api> apis;
  RtMidi::getCompiledApi(apis);

  for(unsigned int i=0;i<apis.size();i++){
    try{
      std::vector<std::string> port_names;
      if(use_input_ports==true){
        RtMidiIn rtmidi(apis[i]);
        port_names = get_port_names(rtmidi);
      }else{
        RtMidiOut rtmidi(apis[i]);
        port_names = get_port_names(rtmidi);
      }
      ret.insert(ret.end(), port_names.begin(), port_names.end());
    }catch ( RtError &error ) {
      RError(error.what());
    }
  }

  return vector_to_cstring_array(ret, num_ports);
}


char **MIDI_getInputPortOsNames(int *retsize){
  return get_port_names(true, retsize);
}


char **MIDI_getOutputPortOsNames(int *retsize){
  return get_port_names(false, retsize);
}


void MIDI_closeMidiPortOs(MidiPortOs port){
  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
  delete myport->midiout;
}

MidiPortOs MIDI_getMidiPortOs(struct Tracker_Windows *window, ReqType reqtype,char *name){
  std::vector<RtMidi::Api> apis;
  RtMidi::getCompiledApi(apis);
  RtMidi::Api api;
  int portnum = -1;

  for(unsigned int i1=0;i1<apis.size();i1++){
    try{
      RtMidiOut rtmidi(apis[i1]);
      std::vector<std::string> port_names = get_port_names(rtmidi);

      for(unsigned int i2=0;i2<port_names.size();i2++)
        if(port_names[i2] == std::string(name)){
          api = apis[i1];
          portnum = i2;
          goto got_portnum;
        }
    }catch ( RtError &error ) {
      RError(error.what());
    }
  }

  {
    MyMidiPortOs *ret = (MyMidiPortOs*)calloc(1, sizeof(MyMidiPortOs));
    if(apis.size()==1)
      api = apis[0];
    else{
      const char **menu = (const char**)talloc(apis.size()*sizeof(char*));
      for(unsigned int i=0;i<apis.size();i++)
        menu[i] = apis[i]==RtMidi::LINUX_ALSA ? "Alsa"
                : apis[i]==RtMidi::UNIX_JACK  ? "Jack"
                : apis[i]==RtMidi::WINDOWS_MM ? "Multimedia Library"
                : apis[i]==RtMidi::WINDOWS_KS ? "Kernel Streaming"
                : "Unknown type";
      int sel = -1;
      while(sel==-1){
        char temp[500];
        sprintf(temp,"Select midi API for '%s': ",name);
        sel = GFX_ReqTypeMenu(window, reqtype, temp,apis.size(),(char**)menu);
      }
      api = apis[sel];
    }

    try{
      ret->midiout = new RtMidiOut(api, "Radium");
#ifdef __linux__
      ret->midiout->openVirtualPort(name);
#endif
#ifdef FOR_WINDOWS
      if(ret->midiout->getPortCount()>0)
        ret->midiout->openPort(0);
      else{
        RWarning("No ports available.");
        return NULL;
      }
#endif
    }catch ( RtError &error ) {
      RError(error.what());
      return NULL;
    }
    return ret;
  }

 got_portnum:
  MyMidiPortOs *ret = (MyMidiPortOs*)calloc(1, sizeof(MyMidiPortOs));
  try{
    ret->midiout = new RtMidiOut(api, "Radium");
    ret->midiout->openPort(portnum, name);
  }catch ( RtError &error ) {
    RError(error.what()); // Can't get this exception to work if provocing wrong arguments above. (tried -fexceptions)
    return NULL;
  }
  printf("opened. api %p: %d, portnum: %d\n",ret,(int)api,(int)portnum);

  return ret;
}

static void mycallback( double deltatime, unsigned int length, unsigned char *message, void *userData ){
  //printf("Got data: %d (%x)\n",length,message[0]);
  if(length==1)
    MIDI_InputMessageHasBeenReceived(message[0],0,0);
  else if(length==2)
    MIDI_InputMessageHasBeenReceived(message[0],message[1],0);
  else if(length==3)
    MIDI_InputMessageHasBeenReceived(message[0],message[1],message[2]);
}

bool MIDI_New(struct Instruments *instrument){
  static bool globals_are_initialized = false;

  if(globals_are_initialized==false){
    message1.push_back(0);

    message2.push_back(0);
    message2.push_back(0);

    message3.push_back(0);
    message3.push_back(0);
    message3.push_back(0);

#ifdef __linux__
    {
      inport_jack = new RtMidiIn(RtMidi::UNIX_JACK,std::string("Radium"));
      if(inport_jack!=NULL){
        inport_jack->openVirtualPort("in");
        inport_jack->setCallback(mycallback,NULL);
      }
    }

    {
      inport_alsa = new RtMidiIn(RtMidi::LINUX_ALSA,std::string("Radium"));
      if(inport_alsa!=NULL){
        inport_alsa->openVirtualPort("in");
        inport_alsa->setCallback(mycallback,NULL);
      }
    }
#endif

#ifdef FOR_WINDOWS
    try{
      inport_winmm = new RtMidiIn(RtMidi::WINDOWS_MM,std::string("Radium"));
    }catch ( RtError &error ) {
      RError(error.what());
    }
    if(inport_winmm!=NULL){
      //inport_winmm->openVirtualPort("in");
      try{
        if(inport_winmm->getPortCount()>0)
          inport_winmm->openPort(0);
        else{
          printf("No Windows Multimedia Library MIDI Input ports.\n");
          fflush(stdout);
        }
      }catch ( RtError &error ) {
        RError(error.what());
      }
      inport_winmm->setCallback(mycallback,NULL);
    }

    try{
      inport_winks = new RtMidiIn(RtMidi::WINDOWS_KS,std::string("Radium"));
    }catch ( RtError &error ) {
      RError(error.what());
    }
    if(inport_winks!=NULL){
      //inport_winks->openVirtualPort("in");
      try{
        if(inport_winks->getPortCount()>0)
          inport_winks->openPort(0);
        else{
          printf("No Windows Kernel Streaming MIDI Input ports.\n");
          fflush(stdout);
        }
      }catch ( RtError &error ) {
        RError(error.what());
      }
      inport_winks->setCallback(mycallback,NULL);
    }
#endif

    globals_are_initialized = true;
  }

  return true;
}


void MIDI_Delete(void){
  printf("Ending MIDI instrument\n");

#ifdef __linux__
  delete inport_jack;
  delete inport_alsa;
#endif

#ifdef FOR_WINDOWS
  delete inport_winmm;
  delete inport_winks;
#endif

}

