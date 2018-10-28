#ifdef FOR_WINDOWS

/*
Links to check out:
https://bitbucket.org/edd/dbg/commits/all
https://github.com/rainers/cv2pdb
https://github.com/jrfonseca/drmingw

Blog post from another guy doing backtraces and catching exceptions in lin/mingw/osx: https://spin.atomicobject.com/2013/01/13/exceptions-stack-traces-c/
*/

#include <windows.h>
#include <excpt.h>
#include <imagehlp.h>
#include <bfd.h>
#include <psapi.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>

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

}


/*

  Backtrace code mostly written by Cloud Wu. Original copyright message:
 */


/* 
 	Copyright (c) 2010 ,
 		Cloud Wu . All rights reserved.
 
 		http://www.codingnow.com
 
 	Use, modification and distribution are subject to the "New BSD License"
 	as listed at <url: http://www.opensource.org/licenses/bsd-license.php >.
 
   filename: backtrace.c

   compiler: gcc 3.4.5 (mingw-win32)

   build command: gcc -O2 -shared -Wall -o backtrace.dll backtrace.c -lbfd -liberty -limagehlp 

   how to use: Call LoadLibraryA("backtrace.dll"); at beginning of your program .

  */


#define BUFFER_MAX (16*1024)

struct bfd_ctx {
	bfd * handle;
	asymbol ** symbol;
};

struct bfd_set {
	char * name;
	struct bfd_ctx * bc;
	struct bfd_set *next;
};

struct find_info {
	asymbol **symbol;
	bfd_vma counter;
	const char *file;
	const char *func;
	unsigned line;
};

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

static void 
lookup_section(bfd *abfd, asection *sec, void *opaque_data)
{
	struct find_info *data = opaque_data;

	if (data->func)
		return;

	if (!(bfd_get_section_flags(abfd, sec) & SEC_ALLOC)) 
		return;

	bfd_vma vma = bfd_get_section_vma(abfd, sec);
	if (data->counter < vma || vma + bfd_get_section_size(sec) <= data->counter) 
		return;

	bfd_find_nearest_line(abfd, sec, data->symbol, data->counter - vma, &(data->file), &(data->func), &(data->line));
}

static void
find(struct bfd_ctx * b, DWORD offset, const char **file, const char **func, unsigned *line)
{
	struct find_info data;
	data.func = NULL;
	data.symbol = b->symbol;
	data.counter = offset;
	data.file = NULL;
	data.func = NULL;
	data.line = 0;

	bfd_map_over_sections(b->handle, &lookup_section, &data);
	if (file) {
		*file = data.file;
	}
	if (func) {
		*func = data.func;
	}
	if (line) {
		*line = data.line;
	}
}
#if 0
static int
init_bfd_ctx(struct bfd_ctx *bc, const char * procname, struct output_buffer *ob)
{
	bc->handle = NULL;
	bc->symbol = NULL;

	bfd *b = bfd_openr(procname, 0);
	if (!b) {
          output_print(ob,"Failed to open bfd from (%s) (%s)\n" , procname, strerror(errno));
          return 1;
	}

        // from addr2line. Don't know what it does. Didn't fix the problem.
        b->flags |= BFD_DECOMPRESS;
        
	int r1 = bfd_check_format(b, bfd_object);
	int r2 = bfd_check_format_matches(b, bfd_object, NULL);
	int r3 = bfd_get_file_flags(b) & HAS_SYMS;

	if (!(r1 && r2 && r3)) {
		bfd_close(b);
		output_print(ob,"Failed to init bfd from (%s) %d,%d,%d.\n", procname,r1,r2,r3);
		return 1;
	}
        
	void *symbol_table;

	unsigned dummy = 0;
	if (bfd_read_minisymbols(b, FALSE, &symbol_table, &dummy) == 0) {
		if (bfd_read_minisymbols(b, TRUE, &symbol_table, &dummy) < 0) {
			free(symbol_table);
			bfd_close(b);
			output_print(ob,"Failed to read symbols from (%s)\n", procname);
			return 1;
		}
	}

	bc->handle = b;
	bc->symbol = symbol_table;

	return 0;
}
#endif

static void
close_bfd_ctx(struct bfd_ctx *bc)
{
	if (bc) {
		if (bc->symbol) {
			free(bc->symbol);
		}
		if (bc->handle) {
			bfd_close(bc->handle);
		}
	}
}

#if 0
// Using addr2line instead.
static struct bfd_ctx *
get_bc(struct output_buffer *ob , struct bfd_set *set , const wchar_t *procname)
{
	while(set->name) {
		if (strcmp(set->name , procname) == 0) {
			return set->bc;
		}
		set = set->next;
	}
	struct bfd_ctx bc;
	if (init_bfd_ctx(&bc, procname , ob)) {
		return NULL;
	}
	set->next = calloc(1, sizeof(*set));
	set->bc = malloc(sizeof(struct bfd_ctx));
	memcpy(set->bc, &bc, sizeof(bc));
	set->name = strdup(procname);

	return set->bc;
}
#endif

static void
release_set(struct bfd_set *set)
{
	while(set) {
		struct bfd_set * temp = set->next;
		free(set->name);
		close_bfd_ctx(set->bc);
		free(set);
		set = temp;
	}
}


// Can not get more than one trace back in Windows XP. 
//
// This function is based on code from http://src.chromium.org/svn/trunk/src/base/debug/stack_trace_win.cc
static int stacktrace(LPCONTEXT context, DWORD *trace_, int num_traces){
  // When walking an exception stack, we need to use StackWalk64().
  int count_ = 0;
  // Initialize stack walking.
  STACKFRAME64 stack_frame;
  memset(&stack_frame, 0, sizeof(stack_frame));
#if defined(_WIN64)
  int machine_type = IMAGE_FILE_MACHINE_AMD64;
  stack_frame.AddrPC.Offset = context->Rip;
  stack_frame.AddrFrame.Offset = context->Rbp;
  stack_frame.AddrStack.Offset = context->Rsp;
#else
  int machine_type = IMAGE_FILE_MACHINE_I386;
  stack_frame.AddrPC.Offset = context->Eip;
  stack_frame.AddrFrame.Offset = context->Ebp;
  stack_frame.AddrStack.Offset = context->Esp;
#endif
  stack_frame.AddrPC.Mode = AddrModeFlat;
  stack_frame.AddrFrame.Mode = AddrModeFlat;
  stack_frame.AddrStack.Mode = AddrModeFlat;


  while (StackWalk64(machine_type,
                     GetCurrentProcess(),
                     GetCurrentThread(),
                     &stack_frame,
                     context,
                     NULL,
                     &SymFunctionTableAccess64,
                     &SymGetModuleBase64,
                     NULL) &&
         count_ < num_traces) {
    trace_[count_++] = stack_frame.AddrPC.Offset;
  }

  int i;
  for (i = count_; i < num_traces; ++i)
    trace_[i] = 0;

  return count_;
}


static void
_backtrace(struct output_buffer *ob, struct bfd_set *set, int depth , LPCONTEXT context)
{
	char procname[MAX_PATH];
	GetModuleFileNameA(NULL, procname, sizeof procname);
	HANDLE process = GetCurrentProcess();

        DWORD stack[NUM_BACKTRACE];
        int num_frames = stacktrace(context, stack, NUM_BACKTRACE);

	char symbol_buffer[sizeof(IMAGEHLP_SYMBOL) + 255];
	wchar_t module_name_raw[MAX_PATH];

        int i = 0;

        int lokke;
        for(lokke=0;lokke<num_frames;lokke++){
          
                DWORD offset = stack[lokke];

		--depth;
		if (depth < 0)
			break;

		IMAGEHLP_SYMBOL *symbol = (IMAGEHLP_SYMBOL *)symbol_buffer;
		symbol->SizeOfStruct = (sizeof *symbol) + 255;
		symbol->MaxNameLength = 254;
#ifdef _WIN64
		DWORD64 module_base = SymGetModuleBase64(process, offset);
#else
		DWORD module_base = SymGetModuleBase(process, offset);
#endif                
                struct bfd_ctx *bc = NULL;

		const wchar_t * module_name = L"[unknown module]";
		if (module_base && 
                    GetModuleFileNameW((HINSTANCE)module_base, module_name_raw, MAX_PATH)
                    ) 
                  {
                    module_name = module_name_raw;
#if 0
                    if(false) // Using addr2line instead. Don't know exactly why addr2line works, but this file doesn't, but there's some indication that bfd doesn't work if the executable is too big.
                      bc = get_bc(ob, set, module_name);
#endif
                  }

		const char * file = NULL;
		const char * func = NULL;
		unsigned line = 0;

		if (bc) {
			find(bc,offset,&file,&func,&line);
		}

		if (file == NULL) {
#ifdef _WIN64
                  DWORD64 dummy = 0;
#else
                  DWORD dummy = 0;
#endif
			if (
#ifdef _WIN64
                            SymGetSymFromAddr64(process, offset, &dummy, symbol)
#else
                            SymGetSymFromAddr(process, offset, &dummy, symbol)
#endif                       
                            ) {
                          
				file = symbol->Name;
			}
			else {
				file = "[unknown file]";
			}
		}
                
                {
                  wchar_t temp[1024];
                  /*
                    sprintf(temp,"addr2line.exe -e %s 0x%x\n",module_name,(unsigned int)offset);
                    fprintf(stderr,"Running -%s-\n",temp);
                    system(temp);
                  */
                  swprintf(temp, 1022, L"0x%x", (unsigned int)offset);
                  
                  const wchar_t *stuff = DISK_run_program_that_writes_to_temp_file(L"radium_addr2line.exe", L"-e", module_name, temp);
                  output_print(ob, "%d: %S. 0x%x : %S\n", i, stuff, (unsigned int)offset, module_name);
                  printf("   stuff: -%S-\n", stuff);
                }
/*
		if (func == NULL) {
			output_print(ob,"%d: 0x%x : %s : %s\n", 
                                     i,
                                     offset,
                                     module_name,
                                     file);
		}
		else {
			output_print(ob,"%d: 0x%x : %s : %s (%d) : in function (%s)\n", 
                                     i,
                                     offset,
                                     module_name,
                                     file,
                                     line,
                                     func);
		}
*/
                i++;
	}
}

static char *crash_buffer = NULL;

static void send_message_with_backtrace(LPCONTEXT c, const char *additional_information, enum Crash_Type crash_type, double time){
  struct output_buffer ob;
  ob.buf = crash_type==CT_CRASH ? crash_buffer : calloc(1, BUFFER_MAX);

  output_init(&ob, BUFFER_MAX);

  if (!SymInitialize(GetCurrentProcess(), 0, TRUE)) {
    output_print(&ob,"Failed to init symbol context\n");

  } else {

    bfd_init();

    struct bfd_set *set = calloc(1,sizeof(*set));
    _backtrace(&ob , set , 128 , c);

    release_set(set);

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
    crash_buffer = calloc(1,BUFFER_MAX);
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
