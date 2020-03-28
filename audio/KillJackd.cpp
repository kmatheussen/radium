
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>



enum{
  JACK_ALIVE_AND_FINE = 0,
  JACK_NOT_RUNNING,
  COULD_NOT_CREATE_CLIENT,
  COULD_NOT_SET_PROCESS_CALLBACK,
  COULD_NOT_CREATE_PORTS,
  COULD_NOT_ACTIVATE_CLIENT,
  COULD_NOT_CONNECT_PORT,
  COULD_NOT_CLOSE_CLIENT
};

#ifdef COMPILE_EXECUTABLE

#include "../weakjack/weak_libjack.c"


// This executable starts, runs, and stops a simple jack client. We do this before starting radium to
// make sure jack is working fine. If it isn't, we ask the user if he wants to kill jackd.


// Lots of code here based on "simple_client.c" from jack.

#include "../common/atomic.h"

static DEFINE_ATOMIC(bool, g_has_called_process) = false;

static jack_port_t *input_port1, *input_port2;
static jack_port_t *output_port1, *output_port2;

static int process (jack_nframes_t nframes, void *arg){
  jack_default_audio_sample_t *in1, *in2, *out1, *out2;
  
  in1 = (jack_default_audio_sample_t*)jack_port_get_buffer (input_port1, nframes);
  in2 = (jack_default_audio_sample_t*)jack_port_get_buffer (input_port2, nframes);
  out1 = (jack_default_audio_sample_t*)jack_port_get_buffer (output_port1, nframes);
  out2 = (jack_default_audio_sample_t*)jack_port_get_buffer (output_port2, nframes);

  (void) in1;
  (void) in2;
  
  memset(out1, 0, sizeof(jack_default_audio_sample_t)*nframes);
  memset(out2, 0, sizeof(jack_default_audio_sample_t)*nframes);

  ATOMIC_SET(g_has_called_process, true);

  return 0;      
}

#include <QCoreApplication>

//extern void init_weak_jack(void);

int main(int argc, char **argv){
  /*
  usleep(14*1000*1000);
  usleep(14*100*1000);
  getchar();
  */

  /*
  int *ai2=NULL;
  ai2[0] = 50;
  */
  
  QCoreApplication app(argc, argv);
  init_weak_jack();
    
  jack_status_t status;
  
  jack_client_t *client = jack_client_open("radium_check_jack_working",JackNoStartServer,&status,NULL);
  //  abort();
  if (client == NULL) {
    fprintf (stderr, "KillJackd.cpp: jack_client_open() failed, "
             "status = 0x%2.0x\n", status);

    if (status & JackServerFailed)
      return JACK_NOT_RUNNING;
    else
      return COULD_NOT_CREATE_CLIENT;      
  }

  if (jack_set_process_callback (client, process, NULL) != 0){
    fprintf (stderr, "KillJackd.cpp: Could not set process callbac\n");
    return COULD_NOT_SET_PROCESS_CALLBACK;
  }

  input_port1 = jack_port_register (client, "input1",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsInput, 0);
  
  input_port2 = jack_port_register (client, "input2",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsInput, 0);
  
  if ((input_port1 == NULL) || (input_port2 == NULL)) {
    fprintf (stderr, "KillJackd.cpp: Could not create input ports\n");
    return COULD_NOT_CREATE_PORTS;
  }

  output_port1 = jack_port_register (client, "output1",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsOutput, 0);
  
  output_port2 = jack_port_register (client, "output2",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsOutput, 0);
  
  if ((output_port1 == NULL) || (output_port2 == NULL)) {
    fprintf (stderr, "KillJackd.cpp: Could not create output ports\n");
    return COULD_NOT_CREATE_PORTS;
  }

  if (jack_activate (client)) {
    fprintf (stderr, "KillJackd.cpp: Could not activate client\n");
    return COULD_NOT_ACTIVATE_CLIENT;
  }

  // Connect input ports
  {
    const char **portnames = jack_get_ports (client, NULL, NULL,
                                             JackPortIsPhysical|JackPortIsOutput);
    
    if (portnames != NULL) {
      
      const char *portname1 = portnames[0];
      
      if (portname1!=NULL){
        if (jack_connect (client, portname1, jack_port_name (input_port1))){
          fprintf (stderr, "KillJackd.cpp: Could not connect input port 1\n");
          return COULD_NOT_CONNECT_PORT;
        }
        
        const char *portname2 = portname1==NULL ? NULL : portnames[1];
        
        if (portname2!=NULL){
          if (jack_connect (client, portname2, jack_port_name (input_port2))){
            fprintf (stderr, "KillJackd.cpp: Could not connect input port 2\n");
            return COULD_NOT_CONNECT_PORT;
          }
        }
      }
      
      jack_free (portnames);
    }
  }

  // Connect output ports
  {
    const char **portnames = jack_get_ports (client, NULL, NULL,
                                             JackPortIsPhysical|JackPortIsInput);
    
    if (portnames != NULL) {
      
      const char *portname1 = portnames[0];
      
      if (portname1!=NULL){
        if (jack_connect (client, jack_port_name (output_port1), portname1)){
          fprintf (stderr, "KillJackd.cpp: Could not connect output port 1\n");
          return COULD_NOT_CONNECT_PORT;
        }
        
        const char *portname2 = portname1==NULL ? NULL : portnames[1];
        
        if (portname2!=NULL){
          if (jack_connect (client, jack_port_name (output_port2), portname2)){
            fprintf (stderr, "KillJackd.cpp: Could not connect output port 2\n");
            return COULD_NOT_CONNECT_PORT;
          }
        }
      }
      
      jack_free (portnames);
    }
  }


  while(ATOMIC_GET(g_has_called_process)==false)
    usleep(900*1000);

  if (jack_client_close(client) != 0){
    return COULD_NOT_CLOSE_CLIENT;
  }

#if 0 // Set to 1 to test jackd killer
  for(int i=0;i<10*1000;i++)
    usleep(1000); // usleep only works in the range 0->1.000.000
#endif
  
  return JACK_ALIVE_AND_FINE;
}


#else // COMPILE_EXECUTABLE -> !COMPILE_EXECUTABLE

#include <QProcess>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/Process.hpp"

#include "KillJackd_proc.h"

static void kill_jackd(void){
  
  bool killed_check_program = false;
  
  for(int i=0;i<20;i++){
  
#if defined(FOR_LINUX) || defined(FOR_MACOSX)
    const char *command1 = "killall -9 radium_check_jack_status";
    const char *command2 = "killall -9 jackd";
    const char *command3 = "killall -9 jackdmp";    
#elif defined(FOR_WINDOWS)
    const char *command1 = "taskkill /F /T /IM radium_check_jack_status.exe";
    const char *command2 = "taskkill /F /T /IM jackd.exe";
    const char *command3 = "taskkill /F /T /IM jackdmp.exe";
#endif
    
    if (killed_check_program==false){
      GFX_ShowProgressMessage(talloc_format("Killing jack %d/%d: \"%s\"", i+1, 20, command1), true);
      if (system(command1)==0)
        killed_check_program = true;
      
      msleep(80);
    }
    
    GFX_ShowProgressMessage(talloc_format("Killing jack %d/%d: \"%s\"", i+1, 20, command2), true);
    if (system(command2)==0){
      //return;
    }

    msleep(80);
    
    GFX_ShowProgressMessage(talloc_format("Killing jack %d/%d: \"%s\"", i+1, 20, command3), true);
    if (system(command3)==0){
      //return;
    }

    msleep(80);
    
#if defined(FOR_MACOSX)
    const char *command4 = "killall -9 JackPilot"; // On OSX, jack pilot needs to be restarted after jackd has been killed. (if not, it shows a message about needing to restart the system)
    GFX_ShowProgressMessage(talloc_format("Killing jack %d/%d: \"%s\"", i+1, 20, command4), true);
    system(command4);
    msleep(80);
#endif
    
  }

}


// Returns true if unresponsive. (probably been killed too)
bool KILLJACKD_kill_jackd_if_unresponsive(void){
  radium::Process process;

#if FOR_WINDOWS  
  process.start(STRING_get_qstring(OS_get_full_program_file_path("radium_check_jack_status.exe").id));
#else
  process.start(STRING_get_qstring(OS_get_full_program_file_path("radium_check_jack_status").id));
#endif
  
#if defined(RELEASE)
  int msecs = 10000;
#else
  int msecs = 5000;
#endif
  
  process.wait_for_finished(msecs);

  QString message;
  
  if (process.error_has_occured()){

    if (process.get_status()==radium::Process::Status::CRASHED)      
      message = "Crash while testing jack.";

    else if (process.get_status()==radium::Process::Status::TIMED_OUT)
      message = "Jack is unresponsive.";

    else
      message = "Jack test program " + process.get_status_string() + ".";
    
  } else {

    int status = process.get_exit_code();

    switch(status){
    case JACK_ALIVE_AND_FINE:
      break;
    case JACK_NOT_RUNNING: // We handle this situation in Mixer.cpp. Also, there's no need to kill it if it isn't running.
      break;
    case COULD_NOT_CREATE_CLIENT:
      message = "Could not create jack client";
      break;
    case COULD_NOT_SET_PROCESS_CALLBACK:
      message = "Could not set process callback";
      break;
    case COULD_NOT_CREATE_PORTS:
      message = "Could not create jack ports";
      break;
    case COULD_NOT_ACTIVATE_CLIENT:
      message = "Could not activate jack client";
      break;
    case COULD_NOT_CONNECT_PORT:
      message = "Could not connect port";
      break;
    case COULD_NOT_CLOSE_CLIENT:
      message = "Could not close client";
      break;
    default:
      message = "Unknown error (?) " + QString::number(status);
      break;      
    }

  }
  
  if (message != ""){
    vector_t v = {};
    
    int ignore = VECTOR_push_back(&v, "Try to run anyway");
    int kill = VECTOR_push_back(&v, "Stop the jack process! (Strongly recommended)");
    (void)kill;
    
    message = "There is a problem with the jack server: " + message;
    int hmmm = GFX_Message(&v, "%s", message.toUtf8().constData());
    
    if (hmmm!=ignore){
      kill_jackd();
      
      GFX_CloseProgress();

      GFX_Message(NULL, "We have now run several commands that should have stopped the jack process.<p>Now you need to start Jack one more time, and after that start Radium again.");
      return true;
    }
  }

  return false;
}


#endif // !COMPILE_EXECUTABLE
