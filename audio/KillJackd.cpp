
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

int process (jack_nframes_t nframes, void *arg){
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

  QCoreApplication app(argc, argv);
  init_weak_jack();
    
  jack_status_t status;
  
  jack_client_t *client = jack_client_open("radium_check_jack_working",JackNoStartServer,&status,NULL);

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
    
    fprintf(stderr, "Executing\n   \"%s\"\n   \"%s\"\n   \"%s\"\n", command1, command2, command3);

    if (killed_check_program==false)
      if (system(command1)==0)
        killed_check_program = true;
    
    msleep(1);
    
    if (system(command2)==0){
      //return;
    }
    
    if (system(command3)==0){
      //return;
    }

#if defined(FOR_MACOSX)
    const char *command4 = "killall -9 JackPilot"; // On OSX, jack pilot needs to be restarted after jackd has been killed. (if not, it shows a message about needing to restart the system)
    system(command4);
#endif
    
    msleep(250);
  }
}


// Returns true if unresponsive. (probably been killed too)
bool KILLJACKD_kill_jackd_if_unresponsive(void){

  QProcess *myProcess = new QProcess();

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  myProcess->setProcessEnvironment(env);
#endif

#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_check_jack_status.exe");
#else
  QString program = OS_get_full_program_file_path("radium_check_jack_status");
#endif

  myProcess->start(program);

#if defined(RELEASE)
  int msecs = 10000;
#else
  int msecs = 5000;
#endif

  int status = 0;

  bool timed_out = (myProcess->waitForFinished(msecs) == false);

  QString message;
  
  if (timed_out==true) {
    message = "Jack is unresponsive.";
    
  } else if (myProcess->exitStatus()==QProcess::CrashExit) {

    message = "Crash while testing jack.";
    
  } else {
    
    status = myProcess->exitCode();
      
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
    vector_t v = {0};
    
    int ignore = VECTOR_push_back(&v, "Try to run anyway");
    int kill = VECTOR_push_back(&v, "Stop the jack process! (Strongly recommended)");
    (void)kill;
    
    message = "There is a problem with the jack server: " + message;
    int hmmm = GFX_Message(&v, "%s", message.toUtf8().constData());
    
    if (hmmm!=ignore){
      kill_jackd();
      GFX_Message(NULL, "We have now run several commands that should have stopped the jack process.<p>Now you need to start Jack one more time, and after that start Radium again.");
      return true;
    }
  }

  myProcess->connect(myProcess, SIGNAL(finished(int)), myProcess, SLOT(deleteLater()));

  msleep(1000); // Give jack some time to rest

  return false;
}


#endif // !COMPILE_EXECUTABLE
