
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

static jack_port_t *output_port1, *output_port2;

int process (jack_nframes_t nframes, void *arg){
  jack_default_audio_sample_t *out1, *out2;
  
  out1 = (jack_default_audio_sample_t*)jack_port_get_buffer (output_port1, nframes);
  out2 = (jack_default_audio_sample_t*)jack_port_get_buffer (output_port2, nframes);

  memset(out1, 0, sizeof(jack_default_audio_sample_t)*nframes);
  memset(out2, 0, sizeof(jack_default_audio_sample_t)*nframes);

  ATOMIC_SET(g_has_called_process, true);

  return 0;      
}


int main(void){

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

  output_port1 = jack_port_register (client, "output1",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsOutput, 0);
  
  output_port2 = jack_port_register (client, "output2",
                                     JACK_DEFAULT_AUDIO_TYPE,
                                     JackPortIsOutput, 0);
  
  if ((output_port1 == NULL) || (output_port2 == NULL)) {
    fprintf (stderr, "KillJackd.cpp: Could not create ports\n");
    return COULD_NOT_CREATE_PORTS;
  }

  if (jack_activate (client)) {
    fprintf (stderr, "KillJackd.cpp: Could not activate client\n");
    return COULD_NOT_ACTIVATE_CLIENT;
  }

  const char **portnames = jack_get_ports (client, NULL, NULL,
                                           JackPortIsInput);

  if (portnames != NULL) {
    
    const char *portname1 = portnames[0];

    if (portname1!=NULL){
      if (jack_connect (client, jack_port_name (output_port1), portname1)){
        fprintf (stderr, "KillJackd.cpp: Could not connect port 1\n");
        return COULD_NOT_CONNECT_PORT;
      }
      
      const char *portname2 = portname1==NULL ? NULL : portnames[1];

      if (portname2!=NULL){
        if (jack_connect (client, jack_port_name (output_port2), portname2)){
          fprintf (stderr, "KillJackd.cpp: Could not connect port 2\n");
          return COULD_NOT_CONNECT_PORT;
        }
      }
    }
    
    jack_free (portnames);
  }


  while(ATOMIC_GET(g_has_called_process)==false)
    usleep(1000*1000);

  if (jack_client_close(client) != 0){
    return COULD_NOT_CLOSE_CLIENT;
  }

#if 0 // Set to 1 to test jackd killer
  usleep(10*1000*1000);
#endif
  
  return JACK_ALIVE_AND_FINE;
}


#else // COMPILE_EXECUTABLE -> !COMPILE_EXECUTABLE

#include <QProcess>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "KillJackd_proc.h"

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
static void kill_jackd(void){
  for(int i=0;i<20;i++){
    const char *command = "killall -9 radium_check_jack_status ; usleep 1000 ; killall -9 jackd ; killall -9 jackdmp";
    fprintf(stderr, "Executing \"%s\"\n", command);
    system(command);
    usleep(250*1000);
  }
}
#endif

#if defined(FOR_WINDOWS)
static void kill_jackd(void){
  for(int i=0;i<20;i++){
    const char *command1 = "taskkill /F /T /IM radium_check_jack_status.exe";
    const char *command2 = "taskkill /F /T /IM jackd.exe";
    const char *command3 = "taskkill /F /T /IM jackdmp.exe";
    fprintf(stderr, "Executing\n   \"%s\"\n   \"%s\"\n   \"%s\"\n", command1, command2, command3);
    
    system(command1);
    usleep(1000);
    system(command2);
    system(command3);
    
    usleep(250*1000);
  }
}
#endif


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
    
  } else {
    
    status = myProcess->exitStatus();
      
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
    int kill = VECTOR_push_back(&v, "Stop the jack process? (recommended)");
    
    message = "There is a problem with the jack server: " + message;
    int hmmm = GFX_Message(&v, message.toUtf8().constData());
    (void)ignore;
    
    if (hmmm==kill){
      kill_jackd();
      GFX_Message(NULL, "We have now run several commands that should have stopped the jack process.<p>Now you need to start Jack one more time, and after that you can start Radium again.");
      return true;
    }
  }

  myProcess->connect(myProcess, SIGNAL(finished(int)), myProcess, SLOT(deleteLater()));
  
  return false;
}


#endif // !COMPILE_EXECUTABLE
