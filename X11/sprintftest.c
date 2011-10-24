
#include <stdio.h>
#include <stdlib.h>


void sprintfsak(char *enstreng){
  char filename[]="/tmp/tmpXXXXXX";
  char temp[500];

  mkstemp(filename);

  sprintf(temp,enstreng,filename);

  printf("filename: \"%s\", temp: \"%s\"\n",filename,temp);
}

char *GFX_ReadStringFromPythonCommand(char *pythoncommand){
  FILE *file;
  char filename[50];
  char temp[500];
  char *ret=malloc(500);

  sprintf(filename,"/tmp/radiumipctempfileXXXXXX");
  mkstemp(filename);
  printf("1 -%s- -%s-\n",filename,pythoncommand);
  sprintf(temp,pythoncommand,filename);
  printf("2 -%s- -%s-\n",filename,temp);

  //  PyRun_SimpleString(temp);
  printf("3 -%s-\n",filename);

  file=fopen(filename,"r");
  if(file==NULL) printf("Oops file is null -%s-\n",filename);
  fgets(ret,499,file);
  fclose(file);

  printf("Tried to read -%s-\n",ret);
  sprintf(temp,"rm %s",filename);
  system(temp);
  
  return ret;
}


int main(){
  char enstreng[]="X11.GFX_Write(\"%s\",\"jabbadabba\")";
  //  sprintfsak(enstreng);
  GFX_ReadStringFromPythonCommand(enstreng);
  return 0;
}
