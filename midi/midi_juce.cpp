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


#include "midi_i_input_proc.h"
#include "midi_menues_proc.h"

#include "OS_midi_proc.h"

#include "../common/Vector.hpp"
#include "../common/Mutex.hpp"
#include "../common/settings_proc.h"


struct MyMidiPortOs{
  MidiOutput *midiout;
};


static radium::Vector<MidiInput*> g_inports;

static radium::Mutex g_midi_out_mutex;


int MIDI_msg_len(uint32_t msg){

  int byte1 = MIDI_msg_byte1(msg);
  
  R_ASSERT(byte1!=0xf0);
  R_ASSERT(byte1!=0xf7);
  
  if (byte1<0x80 || byte1>0xff){
    RT_message("Illegal msg: %x",msg);
    return 0;
  }
  
  return MidiMessage::getMessageLengthFromFirstByte(byte1);
}



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
  int len = MIDI_msg_len(MIDI_msg_pack1(cc));
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

  MidiOutput *output = myport->midiout;
  
  if (output==NULL)
    return;
  
  //printf("current time: %f\n",(float)RtMidiOut::getCurrentTime(myport->midiout->getCurrentApi()));

  {
    radium::ScopedMutex lock(&g_midi_out_mutex); // Sometimes, the GUI wants to send midi signals, and that's why we need a lock here.
    //                                                Don't think any effect caused by priority inheritance should be an issue when the user drags sliders, etc.

    //printf("got midi: %x,%x,%x at time %f (rtmidi_time: %f) (current_time: %f)\n",cc,data1,data2,(float)time/(double)PFREQ,rtmidi_time,(float)RtMidiOut::getCurrentTime(midiout->getCurrentApi()));

    
    if(len==1)
      output->sendMessageNow(MidiMessage(cc));
    else if(len==2)
      output->sendMessageNow(MidiMessage(cc, data1));
    else
      output->sendMessageNow(MidiMessage(cc, data1, data2));
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


static char** string_array_to_char_array(const StringArray &devices, int *retsize){
  *retsize = devices.size();
  
  char **ret = (char**)talloc(sizeof(char*)*devices.size());
  
  for(int i=0 ; i < devices.size() ; i++)
    ret[i] = talloc_strdup(devices[i].toUTF8());

  return ret;
}

char **MIDI_getInputPortOsNames(int *retsize){
  StringArray devices = MidiInput::getDevices();
  return string_array_to_char_array(devices, retsize);
}


char **MIDI_getOutputPortOsNames(int *retsize){
  StringArray devices = MidiOutput::getDevices();
  return string_array_to_char_array(devices, retsize);
}


void MIDI_closeMidiPortOs(MidiPortOs port){
  MyMidiPortOs *myport = static_cast<MyMidiPortOs*>(port);
  delete myport->midiout;
  free(myport);
}

MidiPortOs MIDI_getMidiPortOs(struct Tracker_Windows *window, ReqType reqtype,char *name){

  MyMidiPortOs *ret = (MyMidiPortOs*)calloc(1, sizeof(MyMidiPortOs));
    
  String string(name);

  StringArray devices = MidiOutput::getDevices();

  int device_id = devices.indexOf(string);


  if (device_id == -1 ){
#if defined(FOR_WINDOWS)
    if (devices.size() > 0) {
      RT_Message(NULL, "MIDI output device %s not found, replacing with device %s", name, devices[0].toUtf8());
      device_id = 0;
    } else {
      RT_Message(NULL, "MIDI output device %s not found, replacing with dummy device", name);
    }
#else
    ret->midiout = MidiOutput::createNewDevice(string);
#endif
  } else {
    ret->midiout = MidiOutput::openDevice(device_id);
  }

  if (ret->midiout == NULL){
    GFX_Message(NULL, "Unable to open MIDI output device %s", name);
    free(ret);
    return NULL;
  }

  printf("midi output device opened. name: %s, device id: %d\n",name, device_id);

  return ret;
}

/*
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
*/


void MIDI_OS_AddInputPortIfNotAlreadyAdded(const char *portname){
}


void MIDI_OS_RemoveInputPort(const char *portname){
  //GFX_Message(NULL, "No MIDI port \"%s\" found", portname);
}




bool MIDI_New(struct Instruments *instrument){
  static bool globals_are_initialized = false;

  if(globals_are_initialized==false){

    const char *inport = SETTINGS_read_string("midi_input_port",NULL); //MIDI_get_input_port();
    if(inport!=NULL)
      MIDI_OS_AddInputPortIfNotAlreadyAdded(inport);

    globals_are_initialized = true;
  }

  return true;
}


void MIDI_Delete(void){
  printf("Ending MIDI instrument\n");

  while(g_inports.size() > 0){
    MidiInput *inport = g_inports[0];
    g_inports.remove(inport);
    delete inport;
  }
}

