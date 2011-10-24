
#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"


int radium_main(char *arg){

  printf("\nDummy-Radium starting.\n\n");


  /* Here: Set up threads, gfx init, etc. */


  if(InitProgram()==true){

    /* Here: More inits, receive events, other things. */

    printf("\nDummy-radium succesfully initialized.\n\n");
  }


  EndProgram();

  printf("\nDummy-Radium ending.\n");
  return 0;
}

