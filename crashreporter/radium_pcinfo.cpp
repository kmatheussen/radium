#include <stdio.h>
#include <stdlib.h>

#include <cxxabi.h>

//#include <QHash>

#include <backtrace.h>



static FILE *g_file = NULL;

static const char *g_modulename = NULL;
static int64_t g_pc;

static int full_callback(void * /*data*/, uintptr_t pc, const char *filename, int lineno, const char *function){
  
  int demangle_status;

  const char *realname = abi::__cxa_demangle(function, 0, 0, &demangle_status);

  if (demangle_status != 0){
    realname = function;
  }

  if (realname==NULL && filename==NULL){
    fprintf(g_file, "0x%llx : %s\n", g_pc, g_modulename);
  }else{
    fprintf(g_file, "%s:%d : %s : 0x%llx : %s\n",
            filename == nullptr ? "???" : filename, lineno,
            realname == nullptr ? "???" : realname,
            (unsigned long long) pc, 
            g_modulename);
  }
  
  return 0;
}

static void error_callback(void * /*data*/, const char *msg, int errnum){
  fprintf(g_file, "0x%llx : %s : (%d: %s)\n", g_pc, g_modulename, errnum, msg);
}


static struct backtrace_state *get_lbstate(const char *modulename){
#if 1
  return backtrace_create_state(modulename, 0, error_callback, nullptr);
#else
  static QHash<const char*, struct backtrace_state*> s_states;

  if (!s_states.contains(modulename))
    s_states.insert(modulename, backtrace_create_state(modulename, 0, error_callback, nullptr));
    
  return s_states.value(modulename);
#endif
}

int main(int argc, const char **argv){

  printf("radium_pcinfo: argv[0]: %s\n", argv[0]);
  
  const char *filename = argv[argc-1];
  
  g_file = fopen(filename, "w");

  if (g_file==NULL)
  {
    fprintf(stderr, "radium_pcinfo: Couldn't open file \"%s\"", filename);
    return -1;
  }
  
  int i = 1;
  
  while(i < argc-1){
    g_modulename = argv[i++];

    if (i==argc-1)
      break;
    
    sscanf(argv[i++], "%llx", &g_pc);

    //struct backtrace_state *lbstate = get_lbstate(argv[0]);//modulename);
    struct backtrace_state *lbstate = get_lbstate(g_modulename);

    if (lbstate==NULL){
      
      fprintf(g_file, "%d: %s\t %lld\n", i-2, g_modulename, g_pc);
      
    }else{

      printf("Modulename: \"%s\"\n", g_modulename);
      backtrace_pcinfo(lbstate, g_pc, full_callback, error_callback, NULL);
    }
  }

  fclose(g_file);
  
  return 0;
}
