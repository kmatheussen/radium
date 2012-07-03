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

#include "rtmidi-2.0.0/RtMidi.h"

#include "../../common/nsmtracker.h"
#include "../../common/memory_proc.h"
#include "../../common/OS_visual_input.h"

#include "../OS_midi_proc.h"

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


void PutMidi(MidiPortOs port,
             uint32_t msg
             )
{
  if(port==NULL)
    return;

  RtMidiOut *midiout = static_cast<RtMidiOut*>(port);

  unsigned int d3=(msg>>8)&0xff;
  unsigned int d2=(msg>>16)&0xff;
  unsigned int d1=(msg>>24)&0xff;

  int len = midi_msg_len(d1);
  if(len==0)
    return;

  {
    ScopedPutMidiLock lock; // Sometimes, the GUI wants to send midi signals, and that's why we need a lock here.
                            // Don't think any effect caused by priority inheritance should be an issue when the user drags sliders, etc.

    if(len==1){
      message1[0]=d1;
      midiout->sendMessage(&message1);
      return;
    }

    if(len==2){
      message2[0]=d1;
      message2[1]=d2;
      midiout->sendMessage(&message2);
      return;
    }

    message3[0]=d1;
    message3[1]=d2;
    message3[2]=d3;
    midiout->sendMessage(&message3);
  }
}


void GoodPutMidi(MidiPortOs port,
                 uint32_t msg,
                 uint32_t maxbuff
                 )
{
  PutMidi(port,msg);
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


static char **vector_to_cstring_array(const std::vector<std::string> &strings, int *retval){
  *retval = strings.size();
  char **ret = (char**)talloc(sizeof(char*)*(*retval));

  for(int i=0;i<*retval;i++)
    ret[i] = talloc_strdup((char*)strings[i].c_str());

  return ret;
}


static char **get_port_names(bool use_input_ports, int *retval){
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

  return vector_to_cstring_array(ret, retval);
}


char **MIDI_getInputPortOsNames(int *retsize){
  return get_port_names(true, retsize);
}


char **MIDI_getOutputPortOsNames(int *retsize){
  return get_port_names(false, retsize);
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
          goto gotit;
        }
    }catch ( RtError &error ) {
      RError(error.what());
    }
  }

  {
    RtMidiOut *ret;
    if(apis.size()==1)
      api = apis[0];
    else{
      const char **menu = (const char**)talloc(apis.size()*sizeof(char*));
      for(unsigned int i=0;i<apis.size();i++)
        menu[i] = apis[i]==RtMidi::LINUX_ALSA?"Alsa":"Jack";
      int sel = -1;
      while(sel==-1){
        char temp[500];
        sprintf(temp,"Select midi API for '%s': ",name);
        sel = GFX_ReqTypeMenu(window, reqtype, temp,apis.size(),(char**)menu);
      }
      api = apis[sel];
    }

    try{
      ret = new RtMidiOut(api, "Radium");
      ret->openVirtualPort(name);
    }catch ( RtError &error ) {
      RError(error.what());
      return NULL;
    }
    return ret;
  }

 gotit:
  RtMidiOut *ret;
  try{
    ret = new RtMidiOut(api, "Radium");
    ret->openPort(portnum, name);
  }catch ( RtError &error ) {
    RError(error.what()); // Can't get this exception to work if provocing wrong arguments above. (tried -fexceptions)
    return NULL;
  }
  printf("opened. api %p: %d, portnum: %d\n",ret,(int)api,(int)portnum);

  return ret;
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
    globals_are_initialized = true;
  }

  return true;
}


void MIDI_Delete(void){
}

