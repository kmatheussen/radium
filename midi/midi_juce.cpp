/* Copyright 2016 Kjetil S. Matheussen

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


/* This file is #include-d from ../audio/Juce_plugins.cpp */
//
// Note that compilation settings are a bit different here. FOR_LINUX/etc. are not defined, etc. */



#include "midi_i_input_proc.h"
#include "midi_menues_proc.h"

#include "OS_midi_proc.h"

#include "../common/Vector.hpp"
#include "../common/Mutex.hpp"
#include "../common/settings_proc.h"

namespace{
struct MyMidiPortOs{
  juce::MidiOutput *midiout;
};
}

static radium::Mutex g_midi_out_mutex;


int MIDI_msg_len(uint32_t msg){

  int byte1 = MIDI_msg_byte1(msg);
  
  R_ASSERT(byte1!=0xf0);
  R_ASSERT(byte1!=0xf7);
  
  if (byte1<0x80 || byte1>0xff){
    RT_message("Illegal msg: %x",msg);
    return 0;
  }
  
  return juce::MidiMessage::getMessageLengthFromFirstByte(byte1);
}


namespace{
  struct MyMidiInputCallback : juce::MidiInputCallback{
    juce::MidiInput *midi_input;
    const symbol_t *port_name;

    MyMidiInputCallback()
      : midi_input(NULL)
      , port_name(NULL)
    {}

    ~MyMidiInputCallback(){  // todo: check if this function is ever called.
      // free(port_name); // <- The string is used in queues in midi_i_input, so we get memory corruption if the port is deleted while the name lives in one of the buffers. The memory leak caused by not freing here doesn't really matter. The string is also used in midi_learn, where it can be kept for the remaining time of the program.
    }
      
    void handleIncomingMidiMessage(juce::MidiInput *source,
                                   const juce::MidiMessage &message 
                                   )
      override
    {

      /*
      int64_t message_ms = message.timeStamp;
      int64_t now_ms = Time::getMillisecondCounter();
      */

      const juce::uint8* raw = message.getRawData();
      int length = message.getRawDataSize();
      
      if(length==1)
        MIDI_InputMessageHasBeenReceived(port_name, raw[0],0,0);
      else if(length==2)
        MIDI_InputMessageHasBeenReceived(port_name, raw[0],raw[1],0);
      else if(length==3)
        MIDI_InputMessageHasBeenReceived(port_name, raw[0],raw[1],raw[2]);

      //printf("got message to %s (%d %d %d)\n",(const char*)midi_input->getName().toUTF8(),(int)raw[0],(int)raw[1],(int)raw[2]);
    }
  };
}

static radium::Vector<MyMidiInputCallback*> g_inports;


void OS_PlayFromStart(MidiPortOs port){
}


void OS_InitMidiTiming(void){
}


void OS_PutMidi(MidiPortOs port,
                int cc,
                int data1,
                int data2,
                STime time
                )
{
  int len = juce::MidiMessage::getMessageLengthFromFirstByte(cc);

  /*
  {
    MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
    printf("Sending to %s. len: %d, (%d %d %d)\n",port==NULL?"Null":strdup(myport->midiout->getName().toUTF8()), len, cc, data1, data2);
  }
  */
  
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

  juce::MidiOutput *output = myport->midiout;

  if (output==NULL) // I.e. the "dummy" driver. (necessary on windows)
    return;


  
  //printf("current time: %f\n",(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()));

  {
    radium::ScopedMutex lock(g_midi_out_mutex); // Sometimes, the GUI wants to send midi signals, and that's why we need a lock here.
    //                                              Don't think priority inheritance should be a big issue when the user drags sliders, etc. (why not use the player lock?)


    //printf("got midi: %x,%x,%x at time %f (rtmidi_time: %f) (current_time: %f)\n",cc,data1,data2,(float)time/(double)PFREQ,rtmidi_time,(float)RtMidiOut::getCurrentTime(midiout->getCurrentApi()));

    
    if(len==1)
      output->sendMessageNow(juce::MidiMessage(cc));
    else if(len==2)
      output->sendMessageNow(juce::MidiMessage(cc, data1));
    else
      output->sendMessageNow(juce::MidiMessage(cc, data1, data2));
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


static const char** string_array_to_char_array(const juce::StringArray &devices, int *retsize){
  *retsize = devices.size();
  
  const char **ret = (const char**)talloc(sizeof(const char*)*devices.size());
  
  for(int i=0 ; i < devices.size() ; i++)
    ret[i] = talloc_strdup(devices[i].toUTF8());

  return ret;
}

const char **MIDI_OS_get_connected_input_ports(int *retsize){ // returns ports we are connected to
  *retsize = g_inports.size();
  
  const char **ret = (const char**)talloc(sizeof(const char*)*g_inports.size());

  for (int i = 0 ; i < g_inports.size() ; i++)
    ret[i] = talloc_strdup(g_inports[i]->midi_input->getName().toUTF8());

  return ret;
}

const char **MIDI_getInputPortOsNames(int *retsize){ // returns all ports that's possible to connect to (whether we are connected or not)
  juce::StringArray devices = juce::MidiInput::getDevices();
  return string_array_to_char_array(devices, retsize);
}


const char **MIDI_getOutputPortOsNames(int *retsize){ // returns all ports that's possible to connect to (whether we are connected or not)
  juce::StringArray devices = juce::MidiOutput::getDevices();
  return string_array_to_char_array(devices, retsize);
}

const char *MIDI_getDefaultOutputPort(void){
  juce::StringArray devices = juce::MidiOutput::getDevices();
  if (devices.size()==0)
    return NULL;
  
  return talloc_strdup(devices[juce::MidiOutput::getDefaultDeviceIndex()].toUTF8());
}

void MIDI_closeMidiPortOs(MidiPortOs port){
  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
  delete myport->midiout;
  V_free(myport);
}

MidiPortOs MIDI_getMidiPortOs(struct Tracker_Windows *window, ReqType reqtype,const char *name_c){

  MyMidiPortOs *ret = (MyMidiPortOs*)V_calloc(1, sizeof(MyMidiPortOs));
  
  juce::String name(name_c);

#if defined(FOR_LINUX)
  if (name.startsWith("Radium: "))
    name = name.substring(juce::String("Radium: ").length());
#endif

  juce::StringArray devices = juce::MidiOutput::getDevices();

  int device_id = devices.indexOf(name);

  /*
  for(int i=0;i<devices.size();i++){
    printf("AIAIAIAI. %d: %s (%d: %s)\n", i, talloc_strdup(devices[i].toUTF8()), device_id, name_c);
  }
  */

  //RT_message(NULL); // <-- No backtrace in windows32
      
  if (device_id == -1 ) {

#if JUCE_WINDOWS
    if (devices.size() > 0) {
      device_id = juce::MidiOutput::getDefaultDeviceIndex();
      GFX_Message(NULL, "MIDI output device \"%s\" not found.\n\nAs a workaround, all usage of this device is replaced with the device \"%s\".", name_c, devices[device_id].toRawUTF8());
      ret->midiout = juce::MidiOutput::openDevice(device_id);
    }
#else

    ret->midiout = juce::MidiOutput::createNewDevice(name);
#endif

  } else {

    ret->midiout = juce::MidiOutput::openDevice(device_id);

  }

  if (ret->midiout == NULL)
    RT_message("Error. Unable to open MIDI output device %s.\nUsing dummy device.\nYou need to restart Radium after making the device work in order to use it again.", name_c);

  printf("midi output device opened. name: %s, device id: %d\n",name_c, device_id);

  return ret;
}

static void update_settings(void){
  SETTINGS_write_int("midi_num_inports", g_inports.size());
  
  for(int i = 0 ; i < g_inports.size() ; i++)
    SETTINGS_write_string(talloc_format("midi_input_port_%d",i), g_inports[i]->midi_input->getName().toUTF8());
}

static bool is_connected_to_input_port(juce::String name){

  for (auto port : g_inports)
    if (port->midi_input->getName() == name)
      return true;

  return false;
}

static void add_input_port(juce::String name, bool do_update_settings, bool should_be_there){
  
#if defined(FOR_LINUX)
  if (name.startsWith("Radium: "))
    name = name.substring(juce::String("Radium: ").length());
#endif


  if (is_connected_to_input_port(name))
    return;


  juce::StringArray devices = juce::MidiInput::getDevices();

  int device_id = devices.indexOf(name);

#if JUCE_WINDOWS
  if (device_id==-1){
    if (should_be_there)
      RError("Device %s not found", name.toRawUTF8());
    else
      GFX_Message(NULL, "Device %s not found", name.toRawUTF8());
    return;
  }
#endif

  /*
  for(int i=0;i<devices.size();i++){
    printf("%d: -%s- (%d: -%s- -%s-)\n", i, talloc_strdup(devices[i].toUTF8()), device_id, name_c, talloc_strdup(name.toUTF8()));
  }
  */
  
  auto *midi_input_callback = new MyMidiInputCallback();

  juce::MidiInput *midi_input = NULL;

  if (device_id>=0)
    midi_input = juce::MidiInput::openDevice(device_id, midi_input_callback);
#if !JUCE_WINDOWS
  else
    midi_input = juce::MidiInput::createNewDevice(name, midi_input_callback);
#endif
  
  if (midi_input==NULL){
    GFX_Message(NULL, "Error. Unable to open MIDI input device %s.\n", (const char*)name.toUTF8());
    delete midi_input_callback;
    return;
  }

  midi_input_callback->midi_input = midi_input;
  midi_input_callback->port_name = get_symbol(midi_input->getName().toUTF8());
  
  g_inports.push_back(midi_input_callback);
  
  if (do_update_settings)
    update_settings();
  
  midi_input->start();
}

void MIDI_OS_AddInputPortIfNotAlreadyAdded(const char *name_c){
  juce::String name(name_c);
  add_input_port(name, true, true);
}


static void remove_input_port(juce::String name, bool do_update_settings){

  for (auto port : g_inports)
    if (port->midi_input->getName() == name) {
      port->midi_input->stop();
      g_inports.remove(port);
      if (do_update_settings)
        update_settings();
      delete port->midi_input;
      delete port;
      return;
    }

  GFX_Message(NULL, "%s does not seem to be used",(const char*)name.toUTF8());
}

void MIDI_OS_RemoveInputPort(const char *portname){
  remove_input_port(juce::String(portname), true);
}


bool MIDI_New(struct Instruments *instrument){
  static bool globals_are_initialized = false;

  if(globals_are_initialized==false){

    int num_inports = SETTINGS_read_int("midi_num_inports",0);
    for(int i = 0 ; i < num_inports ; i++){
      const char *inport = SETTINGS_read_string(talloc_format("midi_input_port_%d",i),NULL);
      if(inport!=NULL)
        add_input_port(inport, false, false);
    }
    
    globals_are_initialized = true;
  }

  return true;
}


void MIDI_Delete(void){
  printf("Ending MIDI instrument\n");

  while(g_inports.size() > 0)
    remove_input_port(g_inports[0]->midi_input->getName(), false);
}
