#ifdef FOR_WINDOWS

#include <windows.h>
#include <excpt.h>
#include <imagehlp.h>
#include <psapi.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

// cpuid.cpp 
// processor: x86, x64
// Use the __cpuid intrinsic to get information about a CPU

#include <stdio.h>
#include <string.h>
#include <intrin.h>

#include "crashreporter_proc.h"
#include "../common/nsmtracker_time.h"
#include "../common/OS_disk2_proc.h"

#define NUM_BACKTRACE 62 // More than 63, and capturestackbacktrace won't work on xp and 2003.

struct output_buffer;
static void output_print(struct output_buffer *ob, const char * format, ...);

#define printf_s output_print

/*
  Code for extracting CPU info is written by microsoft: http://msdn.microsoft.com/en-us/library/hskdteyh(v=vs.80).aspx
  Code for backtracing is written by Cloud Wu: http://code.google.com/p/backtrace-mingw/source/browse/trunk/backtrace.c?r=2
*/



static const char* szFeatures[] =
{
    "x87 FPU On Chip",
    "Virtual-8086 Mode Enhancement",
    "Debugging Extensions",
    "Page Size Extensions",
    "Time Stamp Counter",
    "RDMSR and WRMSR Support",
    "Physical Address Extensions",
    "Machine Check Exception",
    "CMPXCHG8B Instruction",
    "APIC On Chip",
    "Unknown1",
    "SYSENTER and SYSEXIT",
    "Memory Type Range Registers",
    "PTE Global Bit",
    "Machine Check Architecture",
    "Conditional Move/Compare Instruction",
    "Page Attribute Table",
    "Page Size Extension",
    "Processor Serial Number",
    "CFLUSH Extension",
    "Unknown2",
    "Debug Store",
    "Thermal Monitor and Clock Ctrl",
    "MMX Technology",
    "FXSAVE/FXRSTOR",
    "SSE Extensions",
    "SSE2 Extensions",
    "Self Snoop",
    "Hyper-threading Technology",
    "Thermal Monitor",
    "Unknown4",
    "Pend. Brk. EN."
};

static void print_cpuinfo(struct output_buffer *ob){
  {
    char CPUString[0x20];
    char CPUBrandString[0x40];
    int CPUInfo[4] = {-1};
    int nSteppingID = 0;
    int nModel = 0;
    int nFamily = 0;
    int nProcessorType = 0;
    int nExtendedmodel = 0;
    int nExtendedfamily = 0;
    int nBrandIndex = 0;
    int nCLFLUSHcachelinesize = 0;
    int nAPICPhysicalID = 0;
    int nFeatureInfo = 0;
    int nCacheLineSize = 0;
    int nL2Associativity = 0;
    int nCacheSizeK = 0;
    //int nRet = 0;
    unsigned    nIds, nExIds, i;
    bool    bSSE3NewInstructions = false;
    bool    bMONITOR_MWAIT = false;
    bool    bCPLQualifiedDebugStore = false;
    bool    bThermalMonitor2 = false;


    // __cpuid with an InfoType argument of 0 returns the number of
    // valid Ids in CPUInfo[0] and the CPU identification string in
    // the other three array elements. The CPU identification string is
    // not in linear order. The code below arranges the information 
    // in a human readable form.
    __cpuid(CPUInfo, 0);
    nIds = CPUInfo[0];
    memset(CPUString, 0, sizeof(CPUString));

    char *gakk1 = (char*)&CPUInfo[1];
    char *gakk2 = (char*)&CPUInfo[2];
    char *gakk3 = (char*)&CPUInfo[3];

    CPUString[0]=gakk1[0];
    CPUString[1]=gakk1[1];
    CPUString[2]=gakk1[2];
    CPUString[3]=gakk1[3];
    CPUString[4]=gakk2[0];
    CPUString[5]=gakk2[1];
    CPUString[6]=gakk2[2];
    CPUString[7]=gakk2[3];
    CPUString[8]=gakk3[0];
    CPUString[9]=gakk3[1];
    CPUString[10]=gakk3[2];
    CPUString[11]=gakk3[3];

#if 0
    *((int*)CPUString) = CPUInfo[1];
    *((int*)(CPUString+4)) = CPUInfo[3];
    *((int*)(CPUString+8)) = CPUInfo[2];
#endif
    
    // Get the information associated with each valid Id
    for (i=0; i<=nIds; ++i)
      {
        __cpuid(CPUInfo, i);
        printf_s(ob,"\nFor InfoType %d\n", i); 
        printf_s(ob,"CPUInfo[0] = 0x%x\n", CPUInfo[0]);
        printf_s(ob,"CPUInfo[1] = 0x%x\n", CPUInfo[1]);
        printf_s(ob,"CPUInfo[2] = 0x%x\n", CPUInfo[2]);
        printf_s(ob,"CPUInfo[3] = 0x%x\n", CPUInfo[3]);

        // Interpret CPU feature information.
        if  (i == 1)
          {
            nSteppingID = CPUInfo[0] & 0xf;
            nModel = (CPUInfo[0] >> 4) & 0xf;
            nFamily = (CPUInfo[0] >> 8) & 0xf;
            nProcessorType = (CPUInfo[0] >> 12) & 0x3;
            nExtendedmodel = (CPUInfo[0] >> 16) & 0xf;
            nExtendedfamily = (CPUInfo[0] >> 20) & 0xff;
            nBrandIndex = CPUInfo[1] & 0xff;
            nCLFLUSHcachelinesize = ((CPUInfo[1] >> 8) & 0xff) * 8;
            nAPICPhysicalID = (CPUInfo[1] >> 24) & 0xff;
            bSSE3NewInstructions = (CPUInfo[2] & 0x1) || false;
            bMONITOR_MWAIT = (CPUInfo[2] & 0x8) || false;
            bCPLQualifiedDebugStore = (CPUInfo[2] & 0x10) || false;
            bThermalMonitor2 = (CPUInfo[2] & 0x100) || false;
            nFeatureInfo = CPUInfo[3];
          }
      }

    // Calling __cpuid with 0x80000000 as the InfoType argument
    // gets the number of valid extended IDs.
    __cpuid(CPUInfo, 0x80000000);
    nExIds = CPUInfo[0];
    memset(CPUBrandString, 0, sizeof(CPUBrandString));

    // Get the information associated with each extended ID.
    for (i=0x80000000; i<=nExIds; ++i)
      {
        __cpuid(CPUInfo, i);
        printf_s(ob,"\nFor InfoType %x\n", i); 
        printf_s(ob,"CPUInfo[0] = 0x%x\n", CPUInfo[0]);
        printf_s(ob,"CPUInfo[1] = 0x%x\n", CPUInfo[1]);
        printf_s(ob,"CPUInfo[2] = 0x%x\n", CPUInfo[2]);
        printf_s(ob,"CPUInfo[3] = 0x%x\n", CPUInfo[3]);

        // Interpret CPU brand string and cache information.
        if  (i == 0x80000002)
          memcpy(CPUBrandString, CPUInfo, sizeof(CPUInfo));
        else if  (i == 0x80000003)
          memcpy(CPUBrandString + 16, CPUInfo, sizeof(CPUInfo));
        else if  (i == 0x80000004)
          memcpy(CPUBrandString + 32, CPUInfo, sizeof(CPUInfo));
        else if  (i == 0x80000006)
          {
            nCacheLineSize = CPUInfo[2] & 0xff;
            nL2Associativity = (CPUInfo[2] >> 12) & 0xf;
            nCacheSizeK = (CPUInfo[2] >> 16) & 0xffff;
          }
      }

    // Display all the information in user-friendly format.

    printf_s(ob,"\nCPU String: %s\n", CPUString);

    if  (nIds >= 1)
      {
        if  (nSteppingID)
          printf_s(ob,"Stepping ID = %d\n", nSteppingID);
        if  (nModel)
          printf_s(ob,"Model = %d\n", nModel);
        if  (nFamily)
          printf_s(ob,"Family = %d\n", nFamily);
        if  (nProcessorType)
          printf_s(ob,"Processor Type = %d\n", nProcessorType);
        if  (nExtendedmodel)
          printf_s(ob,"Extended model = %d\n", nExtendedmodel);
        if  (nExtendedfamily)
          printf_s(ob,"Extended family = %d\n", nExtendedfamily);
        if  (nBrandIndex)
          printf_s(ob,"Brand Index = %d\n", nBrandIndex);
        if  (nCLFLUSHcachelinesize)
          printf_s(ob,"CLFLUSH cache line size = %d\n",
                   nCLFLUSHcachelinesize);
        if  (nAPICPhysicalID)
          printf_s(ob,"APIC Physical ID = %d\n", nAPICPhysicalID);

        if  (nFeatureInfo || bSSE3NewInstructions ||
             bMONITOR_MWAIT || bCPLQualifiedDebugStore ||
             bThermalMonitor2)
          {
            printf_s(ob,"\nThe following features are supported:\n");

            if  (bSSE3NewInstructions)
              printf_s(ob,"\tSSE3 New Instructions\n");
            if  (bMONITOR_MWAIT)
              printf_s(ob,"\tMONITOR/MWAIT\n");
            if  (bCPLQualifiedDebugStore)
              printf_s(ob,"\tCPL Qualified Debug Store\n");
            if  (bThermalMonitor2)
              printf_s(ob,"\tThermal Monitor 2\n");

            i = 0;
            nIds = 1;
            while (i < (sizeof(szFeatures)/sizeof(const char*)))
              {
                if  (nFeatureInfo & nIds)
                  {
                    printf_s(ob,"\t\n");
                    printf_s(ob,szFeatures[i]);
                    printf_s(ob,"\n\n");
                  }

                nIds <<= 1;
                ++i;
              }
          }
      }

    if  (nExIds >= 0x80000004)
      printf_s(ob,"\nCPU Brand String: %s\n", CPUBrandString);

    if  (nExIds >= 0x80000006)
      {
        printf_s(ob,"Cache Line Size = %d\n", nCacheLineSize);
        printf_s(ob,"L2 Associativity = %d\n", nL2Associativity);
        printf_s(ob,"Cache Size = %dK\n", nCacheSizeK);
      }

  }
}

// Picked up from http://msdn.microsoft.com/en-us/library/windows/desktop/ms724439(v=vs.85).aspx
static void print_osinfo(struct output_buffer *ob){
    DWORD dwVersion = 0; 
    DWORD dwMajorVersion = 0;
    DWORD dwMinorVersion = 0; 
    DWORD dwBuild = 0;

    dwVersion = GetVersion();
 
    // Get the Windows version.

    dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
    dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));

    // Get the build number.

    if (dwVersion < 0x80000000)              
        dwBuild = (DWORD)(HIWORD(dwVersion));
    
    printf_s(ob, "Windows version of this OS is %d.%d (%d)\nVersion names:\n%s\n", 
           dwMajorVersion,
           dwMinorVersion,
           dwBuild,
             "");
    /*
           "Windows 8	6.2\n"
           "Windows Server 2012	6.2\n"
           "Windows 7	6.1\n"
           "Windows Server 2008 R2	6.1\n"
           "Windows Server 2008	6.0\n"
           "Windows Vista	6.0\n"
           "Windows Server 2003 R2	5.2\n"
           "Windows Server 2003	5.2\n"
           "Windows XP 64-Bit Edition	5.2\n"
           "Windows XP	5.1\n"
           "Windows 2000	5.0\n");
    */

}


#define BUFFER_MAX (16*1024)


// output_* stuff is code left from old backtrace written by Cloud Wu, that doesn't work anymore, for some reason. Kept the output_* stuff though.


struct output_buffer {
	char *buf;
	size_t sz;
	size_t ptr;
};

static void
output_init(struct output_buffer *ob, size_t sz)
{
  ob->sz = sz;
  ob->ptr = 0;
  ob->buf[0] = '\0';
}

static void
output_print(struct output_buffer *ob, const char * format, ...)
{
	if (ob->sz == ob->ptr)
		return;
	ob->buf[ob->ptr] = '\0';
	va_list ap;
	va_start(ap,format);
        char *rapport = ob->buf+ob->ptr;
	vsnprintf(rapport , ob->sz - ob->ptr , format, ap);
	va_end(ap);

	ob->ptr = strlen(ob->buf + ob->ptr) + ob->ptr;
}

#define MAX_MODULE_NAME (1024*8)

static wchar_t *g_module_name;

// Returns NULL if module is radium.bin.exe.
// Returns module name if it isn't.
static wchar_t *get_module_name_unless_radum_bin_exe(HINSTANCE module_base){

  if (!GetModuleFileNameW(module_base, g_module_name, MAX_MODULE_NAME-10))
    return NULL; // Shouldn't happen, but if it does, return NULL so that backtrace_pcinfo can try instead.
  
  const int len = wcslen(g_module_name);

  const int radium_bin_exe_len = 15;

  if (len <= radium_bin_exe_len)
    return g_module_name;
  
  wchar_t *maybe_radium_bin_exe = g_module_name + len - radium_bin_exe_len + 1;
  
  printf("       MAYBE: \"%S\"\n", maybe_radium_bin_exe);

  if (!wcscmp(maybe_radium_bin_exe, L"radium.bin.exe"))
    return NULL;
  
  return g_module_name;
}


#if defined(_WIN64)
  #define GET_PC_OFFSET(Context) (Context)->Rip
#else
  #define GET_PC_OFFSET(Context) (Context)->Eip
#endif

static void do_the_backtrace(CONTEXT *context, struct output_buffer *ob){

  printf("do_the1\n");

  HANDLE process = GetCurrentProcess();
  
  printf("do_the2\n");
  
  STACKFRAME64 stack_frame;
  memset(&stack_frame, 0, sizeof(stack_frame));
  
#if defined(_WIN64)
  int machine_type = IMAGE_FILE_MACHINE_AMD64;
  stack_frame.AddrFrame.Offset = context->Rbp;
  stack_frame.AddrStack.Offset = context->Rsp;
#else
  int machine_type = IMAGE_FILE_MACHINE_I386;
  stack_frame.AddrFrame.Offset = context->Ebp;
  stack_frame.AddrStack.Offset = context->Esp;
#endif
  
  stack_frame.AddrPC.Offset = GET_PC_OFFSET(context);
  stack_frame.AddrPC.Mode = AddrModeFlat;
  stack_frame.AddrFrame.Mode = AddrModeFlat;
  stack_frame.AddrStack.Mode = AddrModeFlat;

  printf("do_the3\n");

  int num_pc_offsets = 0;
  int64_t pc_offsets[NUM_BACKTRACE+5];

  {
    while (num_pc_offsets < NUM_BACKTRACE && GET_PC_OFFSET(context) > 0) {
      
      pc_offsets[num_pc_offsets++] = GET_PC_OFFSET(context);
      
      if (!StackWalk64(machine_type,
                       process,
                       GetCurrentThread(),
                       &stack_frame,
                       context,
                       NULL,
                       &SymFunctionTableAccess64,
                       &SymGetModuleBase64,
                       NULL))
        {
          printf("do_the6: %d\n", num_pc_offsets);
          
          break;
        }
    }
  }

  for(int i = 0 ; i < num_pc_offsets ; i++) {

    int64_t pc_offset = pc_offsets[i];
    
    DWORD64 module_base = SymGetModuleBase64(process, pc_offset);

    wchar_t *module_name = get_module_name_unless_radum_bin_exe((HINSTANCE)module_base);

    if (i > 0)
      output_print(ob, "%d: ", i);

    printf("%d: %S : %llx\n", i, module_name, pc_offset);
    
    wchar_t temp[1024];
    swprintf(temp, 1022, L"%llx", pc_offset);
    
    const wchar_t *stuff = DISK_run_program_that_writes_to_temp_file(L"radium_pcinfo.exe", module_name == NULL ? L"radium.bin.exe" : module_name, temp, L"");

    //const wchar_t *stuff = L"aiai";
      
    output_print(ob, "%S\n", stuff);
    
    printf("   stuff: -%S-\n", stuff);
  }
}

static char *crash_buffer = NULL;

static void send_message_with_backtrace(LPCONTEXT c, const char *additional_information, enum Crash_Type crash_type, double time){
  struct output_buffer ob;
  
  ob.buf = crash_type==CT_CRASH ? crash_buffer : (char*)calloc(1, BUFFER_MAX);
  
  output_init(&ob, BUFFER_MAX);

  if (!SymInitialize(GetCurrentProcess(), 0, TRUE)) {
    
    output_print(&ob,"Failed to init symbol context\n");

  } else {
    
    do_the_backtrace(c, &ob);

    SymCleanup(GetCurrentProcess());
  }

  print_cpuinfo(&ob);
  print_osinfo(&ob);
  
  const char *messages[1] = {ob.buf};
  CRASHREPORTER_send_message(additional_information, messages, 1, crash_type, time);

  fputs(ob.buf , stderr);

  if (crash_type!=CT_CRASH)
    free(ob.buf);
}

void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, enum Crash_Type crash_type, double time){
  CONTEXT context; 
  LPCONTEXT c = &context;

#if defined(_WIN64)
  
  RtlCaptureContext(&context);
  
#else


  // Found this (very) dirty trick here: http://jpassing.com/2008/03/12/walking-the-stack-of-the-current-thread/
  

  ZeroMemory( &context, sizeof( CONTEXT ) );

  context.ContextFlags = CONTEXT_CONTROL;
    

 label:
  __asm__ ("mov %%ebp, %[a1] ;\n"
           "mov %%esp, %[a2] ;\n"
           : [a1] "=m" (context.Ebp), [a2] "=m" (context.Esp)
           );
  
  context.Eip = (int)&&label;
  
  /*
    The above gnu inline assembly is supposed to do this:

    __asm
    {
    Label:
      mov [Context.Ebp], ebp;
      mov [Context.Esp], esp;
      mov eax, [Label];
      mov [Context.Eip], eax;
    }

    (the original ms-asm code from http://jpassing.com/2008/03/12/walking-the-stack-of-the-current-thread/)

    I think it does the same, but the gnu asm syntax is complicated, so it's hard to know unless you spend a long time studying the syntax.
    However, the generated assembler code looks correct:

L2:
	mov %ebp, 184(%esp) ;
        mov %esp, 200(%esp) ;
	movl	$L2, %eax
	movl	%eax, 188(%esp)

  */
  
#endif
  
  send_message_with_backtrace(c, additional_information, crash_type, time);  
}


static LONG WINAPI 
exception_filter(LPEXCEPTION_POINTERS info)
{
        double time = TIME_get_ms();
  
        CONTEXT c_holder;

        LPCONTEXT c = &c_holder;

        memcpy(c,info->ContextRecord,sizeof(CONTEXT));

        //c->ContextFlags = CONTEXT_FULL;

        Sleep(5);
        send_message_with_backtrace(c, "", CT_CRASH, time);
        fprintf(stderr, "E 5\n");fflush(stderr);
	//exit(1);

	return EXCEPTION_EXECUTE_HANDLER;
}

#define USE_VECTORED_EXCEPTION_HANDLER 0

#if USE_VECTORED_EXCEPTION_HANDLER
static LONG WINAPI VectoredExceptionHandler(PEXCEPTION_POINTERS info)
{
  //fprintf(stderr, "\n\n\n ************* VectoredExceptionHandler ************** E 1\n");fflush(stderr);

        CONTEXT c_holder;

        LPCONTEXT c = &c_holder;

        memcpy(c,info->ContextRecord,sizeof(CONTEXT));

        //c->ContextFlags = CONTEXT_FULL;

        //Sleep(5);
        send_message_with_backtrace(c, "", CT_CRASH);
        
	//exit(1);

	return EXCEPTION_EXECUTE_HANDLER;

}
static PVOID vectored_exception_handler = NULL;
#endif

static LPTOP_LEVEL_EXCEPTION_FILTER g_prev = NULL;

static void
backtrace_register(void)
{
  if (crash_buffer==NULL){
    crash_buffer = (char*)calloc(1,BUFFER_MAX);
    g_prev = SetUnhandledExceptionFilter(exception_filter);
#if USE_VECTORED_EXCEPTION_HANDLER
    vectored_exception_handler = AddVectoredExceptionHandler(0, VectoredExceptionHandler);
#endif
  }
}

static void
backtrace_unregister(void)
{
#if USE_VECTORED_EXCEPTION_HANDLER
  if (vectored_exception_handler != NULL){
    RemoveVectoredExceptionHandler(vectored_exception_handler);
    vectored_exception_handler = NULL;
  }
#endif
  if (crash_buffer!=NULL) {
          
    SetUnhandledExceptionFilter(g_prev);                
    g_prev = NULL;
          
    free(crash_buffer);
    crash_buffer=NULL;
  }
}

#if 0
BOOL WINAPI 
DllMain(HANDLE hinstDLL, DWORD dwReason, LPVOID lpvReserved)
{
	switch (dwReason) {
	case DLL_PROCESS_ATTACH:
		backtrace_register();
		break;
	case DLL_PROCESS_DETACH:
		backtrace_unregister();
		break;
	}
	return TRUE;
}
#endif


void CRASHREPORTER_windows_init(void){
  g_module_name = calloc(sizeof(wchar_t), MAX_MODULE_NAME);
  backtrace_register();
}

void CRASHREPORTER_windows_close(void){
  backtrace_unregister();
}

#if 0
static void foo(void)
{
  int *f=NULL;
        *f = 0;
}

static void
bar(void)
{
  foo();
}

int main(int argc, char **argv){
  printf("STASRTUP PU\n");
  CRASHREPORTER_init();
  bar();
  return 0;
}
#endif


#endif //FOR_WINDOWS
