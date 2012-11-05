#define _WIN32_WINNT 0x0400
#define _WIN32_DCOM

#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include <wbemidl.h>

// modifies input.
char *strip_whitespace(char *s){
  char *ret=s;

  // strip before
  while(isspace(ret[0]))
    ret++;


  // strip after
  int pos=strlen(ret)-1;
  while(isspace(ret[pos])){
    ret[pos]=0;
    pos--;
  }


  return ret;
}

bool check_commandline(const char *command_line)
  char temp[strlen(command_line)+100];

sprintf(temp,"ra.check_jackd_commandline(\"%s\"",command_line);

  char *s=strdup(command_line);
  PyRun_SimpleString("execfile(os.path.join(sys.g_program_path,\"start.py\"))"); // keybindings.conf start.sh\")");

  s = strip_whitespace(s);
  if(s[0]=='j' && s[1]=='a' && s[2]=='c' && s[3]=='d' && s[4]==' '){
    s = &s[4];
    s = strip_whitespace(s);
    if(strlen(s)
    if(s[0]=='-' 
  }

}

vector_t *get_windows_command_lines(){

  vector_t *ret = talloc(sizeof(vector_t));

  HRESULT hr = 0;
  IWbemLocator         *WbemLocator  = NULL;
  IWbemServices        *WbemServices = NULL;
  IEnumWbemClassObject *EnumWbem  = NULL;

  //initializate the Windows security
  hr = CoInitializeEx(0, COINIT_MULTITHREADED);
  hr = CoInitializeSecurity(NULL, -1, NULL, NULL, RPC_C_AUTHN_LEVEL_DEFAULT, RPC_C_IMP_LEVEL_IMPERSONATE, NULL, EOAC_NONE, NULL);

  hr = CoCreateInstance(&CLSID_WbemLocator, 0, CLSCTX_INPROC_SERVER, &IID_IWbemLocator, (LPVOID *) &WbemLocator);
  //connect to the WMI
  hr = WbemLocator->lpVtbl->ConnectServer(WbemLocator, L"ROOT\\CIMV2", NULL, NULL, NULL, 0, NULL, NULL, &WbemServices);   
  //Run the WQL Query
  hr = WbemServices->lpVtbl->ExecQuery(WbemServices, L"WQL", L"SELECT ProcessId,CommandLine FROM Win32_Process", WBEM_FLAG_FORWARD_ONLY, NULL, &EnumWbem);

  // Iterate over the enumerator
  if (EnumWbem != NULL) {
    IWbemClassObject *result = NULL;
    ULONG returnedCount = 0;

    while((hr = EnumWbem->lpVtbl->Next(EnumWbem, WBEM_INFINITE, 1, &result, &returnedCount)) == S_OK) {
      VARIANT ProcessId;
      VARIANT CommandLine;

      // access the properties
      hr = result->lpVtbl->Get(result, L"ProcessId", 0, &ProcessId, 0, 0);
      hr = result->lpVtbl->Get(result, L"CommandLine", 0, &CommandLine, 0, 0);            
      if (!(CommandLine.vt==VT_NULL)){
        VECTOR_push_back(ret, talloc_strdup(CommandLine.bstrVal));
        wprintf(L"%u  %s \r\n", ProcessId.uintVal, CommandLine.bstrVal);
      }

      result->lpVtbl->Release(result);
    }
  }

  // Release the resources
  EnumWbem->lpVtbl->Release(EnumWbem);
  WbemServices->lpVtbl->Release(WbemServices);
  WbemLocator->lpVtbl->Release(WbemLocator);

  CoUninitialize();    
  //getchar();

  return ret;
}
