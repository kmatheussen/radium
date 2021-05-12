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



#if defined(FOR_WINDOWS)

#define _WIN32_WINNT 0x0400
#define _WIN32_DCOM

#include <stdio.h>
#include <tchar.h>
#include <windows.h>
#include <wbemidl.h>

#include "../common/nsmtracker.h"
#include "../common/vector_proc.h"

#include "get_windows_commandlines_proc.h"

static int bstr_length(BSTR bstr){
  WCHAR *c=(WCHAR*)bstr;
  int len=0;
  while(c[len]!=0)
    len++;
  return len+1;
}

static const char *bstr_to_str(BSTR bstr){
  int len = bstr_length(bstr);
  char *str=talloc(len+10);

  WCHAR *c=(WCHAR*)bstr;

  int i=0;

  while(c[i]!=0){
    str[i] = c[i];
    i++;
  }

  return str;
}


// Code to get list of processses copied from http://stackoverflow.com/questions/9589431/getting-the-command-line-arguments-of-another-process-in-windows

vector_t *get_windows_command_lines(void){

  vector_t *ret = talloc(sizeof(vector_t));

  HRESULT hr = 0;
  IWbemLocator         *WbemLocator  = NULL;
  IWbemServices        *WbemServices = NULL;
  IEnumWbemClassObject *EnumWbem  = NULL;

  //initializate the Windows security
  hr = CoInitializeEx(0, COINIT_MULTITHREADED);
  hr = CoInitializeSecurity(NULL, -1, NULL, NULL, RPC_C_AUTHN_LEVEL_DEFAULT, RPC_C_IMP_LEVEL_IMPERSONATE, NULL, EOAC_NONE, NULL);

  hr = CoCreateInstance(&CLSID_WbemLocator, 0, CLSCTX_INPROC_SERVER, &IID_IWbemLocator, (LPVOID *) &WbemLocator);
  if (WbemLocator == NULL)
    goto exit;

  //connect to the WMI
  hr = WbemLocator->lpVtbl->ConnectServer(WbemLocator, SysAllocString(L"ROOT\\CIMV2"), NULL, NULL, NULL, 0, NULL, NULL, &WbemServices);   
  if (WbemServices == NULL)
    goto exit;

  //Run the WQL Query
  hr = WbemServices->lpVtbl->ExecQuery(WbemServices, SysAllocString(L"WQL"), SysAllocString(L"SELECT ProcessId,CommandLine FROM Win32_Process"), WBEM_FLAG_FORWARD_ONLY, NULL, &EnumWbem);
  if (EnumWbem == NULL)
    goto exit;

  // Iterate over the enumerator
  IWbemClassObject *result = NULL;
  ULONG returnedCount = 0;

  while((hr = EnumWbem->lpVtbl->Next(EnumWbem, WBEM_INFINITE, 1, &result, &returnedCount)) == S_OK) {
    VARIANT ProcessId;
    VARIANT CommandLine;
    
    // access the properties
    hr = result->lpVtbl->Get(result, L"ProcessId", 0, &ProcessId, 0, 0);
    hr = result->lpVtbl->Get(result, L"CommandLine", 0, &CommandLine, 0, 0);            
    if (!(CommandLine.vt==VT_NULL)){
      wprintf(L"%u  %s \r\n", ProcessId.uintVal, CommandLine.bstrVal);
      
      VECTOR_push_back(ret, bstr_to_str(CommandLine.bstrVal));
    }
    
    result->lpVtbl->Release(result);
  }


 exit:
  if (EnumWbem != NULL)
    EnumWbem->lpVtbl->Release(EnumWbem);
  if (WbemServices != NULL)
    WbemServices->lpVtbl->Release(WbemServices);
  if (WbemLocator != NULL)
    WbemLocator->lpVtbl->Release(WbemLocator);

  CoUninitialize();    
  //getchar();

  return ret;
}

#endif // defined(FOR_WINDOWS)

