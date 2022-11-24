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


#include <pthread.h>
#include <string.h>

#include <vector>

#include "rtmidi-2.0.0/RtMidi.h"

#include "../../common/nsmtracker.h"
#include "../../common/playerclass.h"
#include "../../common/Vector.hpp"
#include "../../common/vector_proc.h"
#include "../../common/memory_proc.h"
#include "../../common/OS_visual_input.h"
#include "../../common/settings_proc.h"
#include "../../common/visual_proc.h"

#include "../midi_i_input_proc.h"
#include "../midi_menues_proc.h"

#include "../OS_midi_proc.h"

extern PlayerClass *pc;

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

#if 0
#ifdef __linux__
static RtMidiIn *inport_jack;
static RtMidiIn *inport_alsa;
#endif

#ifdef FOR_WINDOWS
static RtMidiIn *inport_winmm;
//static RtMidiIn *inport_winks;
#endif

#ifdef FOR_MACOSX
static RtMidiIn *inport_coremidi;
#endif
#endif

static radium::Vector<RtMidiIn*> g_inports;

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


extern LANGSPEC void OS_InitMidiTiming(void);

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
  int len = midi_msg_len(cc);
  if(len==0)
    return;

  if(port==NULL)
    return;

  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);

#if 0
  if(true || time<0)
    printf("got midi. time: %f. startup_time: %f, jack time: %f. %x/%x/%x (%d/%d/%d)\n",
           ((double)time/(double)pc->pfreq),(float)startup_time,(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()),
           cc,data1,data2,cc,data1,data2
           );
#endif

  //printf("current time: %f\n",(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()));

  {
    ScopedPutMidiLock lock; // Sometimes, the GUI wants to send midi signals, and that's why we need a lock here.
                            // Don't think any effect caused by priority inheritance should be an issue when the user drags sliders, etc.

    double rtmidi_time;

    if(time<0)
      rtmidi_time=myport->last_time; // Necessary to ensure midi messages are sent out in the same order as they got in to this function.
    else{
      rtmidi_time=startup_time + ((double)(time+LATENCY)/(double)pc->pfreq);
      if(rtmidi_time<myport->last_time)
        rtmidi_time=myport->last_time;  // Necessary to ensure all midi messages are sent in order.
    }
    
    myport->last_time = rtmidi_time;
    
    //printf("got midi: %x,%x,%x at time %f (rtmidi_time: %f) (current_time: %f)\n",cc,data1,data2,(float)time/(double)PFREQ,rtmidi_time,(float)RtMidiOut::getCurrentTime(midiout->getCurrentApi()));
    
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
      GFX_Message(NULL, error.what());
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
    if(strings[i].find("Radium") != 0){
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
      GFX_Message(NULL, error.what());
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
#ifndef FOR_WINDOWS
  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
  delete myport->midiout; // TODO: Find out why this crashes on windows.
#endif
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
      GFX_Message(NULL, error.what());
    }
  }

  {
    MyMidiPortOs *ret = (MyMidiPortOs*)calloc(1, sizeof(MyMidiPortOs));
    if(apis.size()==1)
      api = apis[0];
    else{
      vector_t v={};  // c++ way of zero-initialization without getting missing-field-initializers warning.
      for(unsigned int i=0;i<apis.size();i++)
        VECTOR_push_back(&v,  apis[i]==RtMidi::LINUX_ALSA ? "Alsa"
                            : apis[i]==RtMidi::UNIX_JACK  ? "Jack"
                            : apis[i]==RtMidi::WINDOWS_MM ? "Multimedia Library"
                            : apis[i]==RtMidi::WINDOWS_KS ? "Kernel Streaming"
                            : apis[i]==RtMidi::MACOSX_CORE ? "MAC OS X Core MIDI"
                            : "Unknown type"
                         );
      int sel = -1;
      while(sel==-1){
        char temp[500];
        sprintf(temp,"Select midi API for '%s': ",name);
        sel = GFX_ReqTypeMenu(window, reqtype, temp, &v);
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
        GFX_Message(NULL, "No ports available.");
        return NULL;
      }
#endif
#ifdef FOR_MACOSX
      ret->midiout->openVirtualPort(name);
#endif
    }catch ( RtError &error ) {
      GFX_Message(NULL, error.what());
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
    GFX_Message(NULL, error.what()); // Can't get this exception to work if provocing wrong arguments above. (tried -fexceptions)
    return NULL;
  }
  printf("opened. api %p: %d, portnum: %d\n",ret,(int)api,(int)portnum);

  return ret;
}

static void mycallback( double deltatime, unsigned int length, unsigned char *message, void *userData ){
  printf("mycallback %s %d\n",(char*)userData,(int)pthread_self());
  
  //printf("Got data: %d (%x). time: %f\n",length,message[0],deltatime);
  if(length==1)
    MIDI_InputMessageHasBeenReceived(message[0],0,0);
  else if(length==2)
    MIDI_InputMessageHasBeenReceived(message[0],message[1],0);
  else if(length==3)
    MIDI_InputMessageHasBeenReceived(message[0],message[1],message[2]);
}

static int get_portnum(const char *portname){
  int num_ports;
  char **portnames = MIDI_getInputPortOsNames(&num_ports);
  for(int i=0;i<num_ports;i++)
    if(!strcmp(portname,portnames[i]))
      return i;

  return -1;
}

/*
static bool input_port_exists(const char *portname){
  return get_portnum(portname) >= 0;
}
*/

void MIDI_OS_AddInputPortIfNotAlreadyAdded(const char *portname){
  for (auto port : g_inports)
    if (port->getPortName() == std::string(portname))
      return;

#ifdef FOR_LINUX
  enum RtMidi::Api api = RtMidi::LINUX_ALSA;
#elifdef FOR_WINDOWS
  enum RtMidi::Api api = RtMidi::WINDOWS_MM;
#elifdef FOR_MACOSX
  enum RtMidi::Api api = RtMidi::MACOSX_CORE;
#else
# error "unknwond archihtilher"
#endif
    
  int portnum = get_portnum(portname);
  bool exists = portnum >= 0;

#ifdef FOR_WINDOWS // No virtual ports in windows
  if (!exists){
    GFX_Message(NULL,"Unknown port %s",portname);
    return;
  }
#endif

  static int num = 0;
  RtMidiIn *inport = new RtMidiIn(api,std::string(talloc_format("Radium%d",num++)));

  try{
    if(exists)
      inport->openPort(portnum,portname);
    else
      inport->openVirtualPort(portname);

    inport->setCallback(mycallback,talloc_strdup(portname));

  }catch ( RtError &error ) {
    GFX_Message(NULL, "Couldn't open %s. (%s)", portname, error.what());
    // delete inport; // <-- Don't bother. Need to catch errors, but it's too messy to do that here, or on the outside, to be worth it.
    return;
  }

  g_inports.add(inport);
}


void MIDI_OS_RemoveInputPort(const char *portname){
  for (auto port : g_inports)
    if (port->getPortName() == std::string(portname)) {
      try{
        delete port;
      }catch ( RtError &error ) {
        GFX_Message(NULL, "Unable to delete MIDI port %s (%s)", portname, error.what());
        return;
      }
      g_inports.remove(port);
      return;
    }

  GFX_Message(NULL, "No MIDI port \"%s\" found", portname);
}

vector_t *MIDI_OS_get_input_ports(void){
  vector_t *ret = (vector_t*)talloc(sizeof(vector_t));

  for (auto port : g_inports) {
    int num_ports = port->getPortCount();
    for(int portnum=0;portnum<num_ports;portnum++){
      const char *portname = talloc_strdup(port->getPortName(portnum).c_str());
      printf("AIAI. %d/%d: %s\n",portnum,num_ports,portname);
      VECTOR_push_back(ret, portname);
    }
  }

  return ret;
}

#if 0
void MIDI_OS_SetInputPort(const char *portname){
  int portnum = get_portnum(portname);

#ifdef __linux__

    {

      if (inport_alsa != NULL) {
        fprintf(stderr, "deleting old\n");
        try{
          delete inport_alsa;
        }catch ( RtError &error ) {
          GFX_Message(NULL, "Unable to delete old MIDI Port. (%s)", error.what());
          inport_alsa = NULL;
          return;
        }
      }

      try{
        
        fprintf(stderr,"trying to open\n");
        inport_alsa = new RtMidiIn(RtMidi::LINUX_ALSA,std::string("Radium"));
        fprintf(stderr,"Got. trying to open\n");
        if(inport_alsa!=NULL){
          inport_alsa->setCallback(mycallback,NULL);
          if(portnum==-1)
            inport_alsa->openVirtualPort(portname);
          else
            inport_alsa->openPort(portnum,portname);
        }
      
      }catch ( RtError &error ) {
        GFX_Message(NULL, "Couldn't open %s. (%s)", portname, error.what());
        inport_alsa = NULL;
      }

    }

#elifdef FOR_WINDOWS

    if(portnum==-1)
      GFX_Message(NULL,"Unknown port %s",portname);

    else{

      if (inport_winmm != NULL) {
        fprintf(stderr, "deleting old\n");
        try{
          delete inport_winmm;
        }catch ( RtError &error ) {
          GFX_Message(NULL, "Unable to delete old MIDI Port. (%s)", error.what());
          inport_winmm = NULL;
          return;
        }
      }

      try{
        inport_winmm = new RtMidiIn(RtMidi::WINDOWS_MM,std::string("Radium"));
        if(inport_winmm!=NULL){ 
          inport_winmm->setCallback(mycallback,NULL);          
          inport_winmm->openPort(portnum,"in");
        }
      }catch ( RtError &error ) {
        GFX_Message(NULL, "Couldn't open %s. (%s)", portname, error.what());
        inport_winmm = NULL;
      }
    }

#elifdef FOR_MACOSX
    
    {

      if (inport_coremidi != NULL) {
        fprintf(stderr, "deleting old\n");
        try{
          delete inport_coremidi;
        }catch ( RtError &error ) {
          GFX_Message(NULL, "Unable to delete old MIDI Port. (%s)", error.what());
          inport_coremidi = NULL;
          return;
        }
      }

      try{
        inport_coremidi = new RtMidiIn(RtMidi::MACOSX_CORE,std::string("Radium"));
        if(inport_coremidi!=NULL){
          inport_coremidi->setCallback(mycallback,NULL);
          if(portnum==-1)
            inport_coremidi->openVirtualPort("in");
          else
            inport_coremidi->openPort(portnum,"in");
        }
      }catch ( RtError &error ) {
        GFX_Message(NULL, "Couldn't open %s. (%s)", portname, error.what());
        inport_coremidi = NULL;
      }
    }

#else
    
    #error "unknown arghithceh;rture"
    
#endif

}
#endif


bool MIDI_New(struct Instruments *instrument){
  static bool globals_are_initialized = false;

  if(globals_are_initialized==false){
    message1.push_back(0);

    message2.push_back(0);
    message2.push_back(0);

    message3.push_back(0);
    message3.push_back(0);
    message3.push_back(0);

#if 0
#if defined(FOR_WINDOWS)
      try{
        if(inport_winmm->getPortCount()>0)
          inport_winmm->openPort(0);
        else{
          printf("No Windows Multimedia Library MIDI Input ports.\n");
          fflush(stdout);
        }
      }catch ( RtError &error ) {
        GFX_Message(NULL, error.what());
      }
  
#endif
#endif

      const char *inport = SETTINGS_read_string("midi_input_port",NULL); //MIDI_get_input_port();
      if(inport!=NULL)
        MIDI_OS_AddInputPortIfNotAlreadyAdded(inport);


    globals_are_initialized = true;
  }

  return true;
}


void MIDI_Delete(void){
  printf("Ending MIDI instrument\n");

  while(g_inports.size() > 0)
    MIDI_OS_RemoveInputPort(g_inports[0]->getPortName().c_str());
    
  
#if 0
#ifdef __linux__
  delete inport_jack;
  delete inport_alsa;
#endif

#ifdef FOR_WINDOWS
  delete inport_winmm;
#if 0
  delete inport_winks;
#endif
#endif
#endif
}

