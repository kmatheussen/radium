
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>

#include <QMutex>
#include <QThread>
#include <QTime>

#include "nsmtracker.h"


#ifndef BUF_BEFORE
# define BUF_BEFORE 128
#endif

#ifndef BUF_AFTER
# define BUF_AFTER 128
#endif

#define CLEAR_BYTE 0x4f
#define CLEAR_32BYTE 0x4f4f4f4f


#define ALIGN_UP(value) (((uintptr_t)value + sizeof(int32_t) - 1) & -sizeof(int32_t))


static QMutex *mutex = NULL; // must be allocated manually since it can be used before all static variables have been initialized.

namespace{
  
  struct Memlink{
    Memlink *next;
    Memlink *prev;
    char *mem;
    int size;
    const char *filename;
    int linenumber;
    MemoryAllocator allocator;
    bool can_be_freed;
  };

  struct LinkToMemlink{
    Memlink *link;
  };
 
}

static Memlink *g_root;
static Memlink *g_root_end;

static int g_num_elements = 0;

/*
function remove(List list, Node node)
   if node.prev == null
       list.firstNode  := node.next
   else
       node.prev.next  := node.next
   if node.next == null
       list.lastNode  := node.prev
   else
       node.next.prev  := node.prev
*/

static void remove_memlink(Memlink *link){
    
  if (link->prev == NULL)
    g_root = link->next;
  else
    link->prev->next = link->next;

  if (link->next == NULL)
    g_root_end = link->prev;
  else
    link->next->prev = link->prev;

  free(link);
  
  g_num_elements--;
}


/*
function insertBeginning(List list, Node newNode)
     if list.firstNode == null
         list.firstNode  := newNode
         list.lastNode   := newNode
         newNode.prev  := null
         newNode.next  := null
     else
         insertBefore(list, list.firstNode, newNode)

function insertBefore(List list, Node node, Node newNode)
     newNode.prev  := node.prev
     newNode.next  := node
     if node.prev == null
         list.firstNode  := newNode
     else
         node.prev.next  := newNode
     node.prev  := newNode
*/

static void add_memlink(void *mem, int size, MemoryAllocator allocator, const char *filename, int linenumber){   
  QMutexLocker locker(mutex);

  Memlink *link = (Memlink*)calloc(1, sizeof(Memlink));
  LinkToMemlink *linktomemlink = (LinkToMemlink*)mem;
  linktomemlink->link = link;

  link->mem = (char*)mem;

  if (g_root == NULL) {
    
    g_root = link;
    g_root_end = link;
    link->prev = NULL;
    link->next = NULL;
    
  } else {

    link->prev = NULL;
    link->next = g_root;
    g_root->prev = link;
    g_root = link;

  }

  link->size = size;
  link->allocator = allocator;
  link->filename = filename;
  link->linenumber = linenumber;

  g_num_elements++;
}

static void print_error(Memlink *link, bool is_after, bool is32, int pos, int32_t value){
  fprintf(stderr,
          "MEMORY CORRUPTION. Memory %s allocated memory overwritten. Pos %d (%s). Value: %d (0x%x '%c'). %s:%d\n",
          is_after?"after":"before",
          pos,
          is32 ? "int32" : "char",
          value,
          value,
          value,
          link->filename,
          link->linenumber
          );
  abort();  
}


static void validate_link(Memlink *link){
  int32_t *mem_start1 = (int32_t*) (((char*)link->mem) + sizeof(LinkToMemlink));
  int32_t *mem_end1 = mem_start1 + BUF_BEFORE/4;
  
  for(int32_t *mem = mem_start1; mem<mem_end1 ; mem++)
    if (mem[0] != CLEAR_32BYTE)
      print_error(link, false, true, (int)(mem-mem_end1),(int)mem[0]);
  
  char *end_pos = ((char*)mem_end1) + link->size;
  
  int32_t *mem_start2 = (int32_t*)ALIGN_UP(end_pos);
  int32_t *mem_end2 = mem_start2 + BUF_AFTER/4;
  
  for(char *mem = end_pos ; mem<(char*)mem_start2; mem++)
    if (mem[0] != CLEAR_BYTE)
      print_error(link, true, false, (int)(mem-end_pos), (int)mem[0]);
  
  for(int32_t *mem = mem_start2; mem<mem_end2 ; mem++)
    if (mem[0] != CLEAR_32BYTE)
      print_error(link, true, true, (int)(mem-mem_start2),(int)mem[0]);
}

void V_validate(void *mem){
  LinkToMemlink *linktomemlink = (LinkToMemlink *) V_allocated_mem_real_start(mem);
  Memlink *link = linktomemlink->link;
  validate_link(link);
}

void V_validate_all(void){
  QMutexLocker locker(mutex);
  
  Memlink *link = g_root;//get_next_link(NULL);

  fprintf(stderr, " STARTING TO VALIDATE %d \n",g_num_elements);

  int num = 0;
  
  while(link!=NULL){

    validate_link(link);

    num++;

    //if ((num % 3000) == 0)
    //  printf("   At %d (out of %d) (%d left)\n", num, g_num_elements, g_num_elements-num);

    if (num > g_num_elements+10){
      fprintf(stderr, "Validation failed. More links than elements.\n");
      abort();
    }

    link=link->next;//get_next_link(link);
  }

  //  fprintf(stderr, " FINISHED VALIDATING \n");
}


static QTime *timer; // Same here. Can be accessed before it's initialized.

static double get_ms(void){
  return timer->elapsed();
}


static Memlink *validate_a_little(double max_time, Memlink *link){
    
  double start_time = get_ms();

  static int num_removed = 0;
  static int num_scanned = 0;

  if (link==NULL)
    link = g_root;

  while(link!=NULL){
    {
      QMutexLocker locker(mutex);

      Memlink *next = link->next;
      
      if (link->can_be_freed) {
        
        remove_memlink(link);
        num_removed++;
      } else {
        validate_link(link);
        num_scanned++;
      }
      
      link = next;
    }

    if ( (num_scanned%512) == 0 && (get_ms() - start_time) > max_time)
      break;

    QThread::yieldCurrentThread();  // <- Try to give other threads a chance to run. (the 'mutex' mutex blocks all other threads from allocating, so it would usually block the rest of the program)
  }

  if (link==NULL) {
    //printf("   MEMORY VALIDATION CYCLE Finished. Scanned %d links, removed %d links. Total right now: %d\n",num_scanned,num_removed,g_num_elements);
    num_removed = 0;
    num_scanned = 0;
  }


  return link;
}


namespace{
  
struct ValidationThread : public QThread {

public:
  ValidationThread()
  {
    start(QThread::LowestPriority);
  }

  void run(){
    Memlink *link = NULL;
    while(true){
      link = validate_a_little(3, link);  // work 3ms
      QThread::msleep(2);                 // sleep 2ms
    }
  }
};

}


void V_run_validation_thread(void){
  static int num=0;

  num++;

  if (num != 5000) // Must wait a little to start the thread. This function is called VERY early in the program.
    return;
  
  static bool is_running=false;
  
  if (is_running==true)
    return;
  
  is_running=true;
  new ValidationThread();
}

static void memset32(char *char_pos, int num_chars){
  int size = num_chars / 4;
  int32_t *pos = (int32_t*)char_pos;
  for(int a=0 ; a<size ; a++)
    pos[a] = CLEAR_32BYTE;
}

void *V_alloc(MemoryAllocator allocator, int size, const char *filename, int linenumber){

  if (ALIGN_UP(sizeof(LinkToMemlink)) != sizeof(LinkToMemlink))
    abort();

  if (BUF_BEFORE % 4 != 0) {
    fprintf(stderr, "BUF_BEFORE must be dividable by 4\n");
    abort();
  }
  
  if (BUF_AFTER % 4 != 0) {
    fprintf(stderr, "BUF_AFTER must be dividable by 4\n");
    abort();
  }

  
  static bool has_inited = false;
  if (has_inited==false){
    has_inited=true;
    
    timer = new QTime;
    timer->start();
    
    mutex = new QMutex;
  }

#if !defined(DONT_RUN_VALIDATION_THREAD)
  V_run_validation_thread();
#endif

  int aligned_size = ALIGN_UP(size);
  int aligned_diff = aligned_size - size;
  assert(aligned_diff>=0);

  int alloc_size = sizeof(LinkToMemlink) + BUF_BEFORE + aligned_size + BUF_AFTER;
  
  char *mem = (char*)allocator(alloc_size);
  if(mem==NULL)
    return NULL;
  
  memset32(mem + sizeof(LinkToMemlink), BUF_BEFORE);

  char *end_pos = mem + sizeof(LinkToMemlink) + BUF_BEFORE + size;

  memset(end_pos, CLEAR_BYTE, aligned_diff);
  //fprintf(stderr,"endpos: %p, up: %p\n",end_pos, (char*)ALIGN_UP(end_pos));

  memset32(end_pos + aligned_diff, BUF_AFTER);

  add_memlink(mem, size, allocator, filename, linenumber);  

  return (void*)(mem + sizeof(LinkToMemlink) + BUF_BEFORE);
}

void *V_allocated_mem_real_start(void *allocated_mem){
  return (void*) ( ((char*)allocated_mem) - sizeof(LinkToMemlink) - BUF_BEFORE );
}

static void V_free_it2(MemoryFreeer freeer, void *actual_mem_real_start){

  QMutexLocker locker(mutex); // May be moved to remove_memlink

  LinkToMemlink *linktomemlink = (LinkToMemlink *) actual_mem_real_start;
  Memlink *link = linktomemlink->link;
  
  /*
  if(link->is_being_validated){
    // Wait a little and try again.
    usleep(50);
    return V_free_it2(freeer, actual_mem_real_start);
  }
  */

  assert(link->mem == actual_mem_real_start);
  
  validate_link(link);

  freeer(actual_mem_real_start);

  link->can_be_freed = true;
  //remove_memlink(link);
}

void V_free_actual_mem_real_start(MemoryFreeer freeer, void *actual_mem_real_start){
  V_free_it2(freeer,
             actual_mem_real_start);
}

void V_free_it(MemoryFreeer freeer, void *allocated_mem){
  V_free_it2(freeer,
             V_allocated_mem_real_start(allocated_mem)
             );
}

MemoryAllocator V_get_MemoryAllocator(void *mem){
  LinkToMemlink *linktomemlink = (LinkToMemlink *) V_allocated_mem_real_start(mem);
  Memlink *link = linktomemlink->link;

  return link->allocator;
}

int V_get_size(void *mem){
  LinkToMemlink *linktomemlink = (LinkToMemlink *) V_allocated_mem_real_start(mem);
  Memlink *link = linktomemlink->link;

  return link->size;
}

static void *V_realloc_it(MemoryFreeer freeer, void *ptr, size_t size, const char *filename, int linenumber){
  assert(ptr!=NULL);

  LinkToMemlink *linktomemlink = (LinkToMemlink *) V_allocated_mem_real_start(ptr);
  Memlink *link = linktomemlink->link;
  
  void *new_mem = V_alloc(link->allocator, size, filename, linenumber);
  memcpy(new_mem, ptr, R_MIN((int)size, link->size));
  
  V_free_it(freeer, ptr);

  return new_mem;
}

void *V_malloc__(size_t size, const char *filename, int linenumber){
  return V_alloc(malloc ,size, filename, linenumber);
}

char *V_strdup__(const char *s, const char *filename, int linenumber){
  int len = strlen(s) + 1;
  char *ret = (char*)V_malloc__(len,filename,linenumber);

  if (ret==NULL)
    return NULL;

  memcpy(ret, s, len);
  
  return ret;
}

static void *calloc_one_arg(size_t size){
  return calloc(1, size);
}

void *V_calloc__(size_t n, size_t size, const char *filename, int linenumber){
  return V_alloc(calloc_one_arg, n*size, filename, linenumber);
}

void V_free__(void *ptr){
  if (ptr==NULL)
    return;
  V_free_it(free, ptr);
}

void *V_realloc__(void *ptr, size_t size, const char *filename, int linenumber){
  if (ptr==NULL)
    return V_malloc__(size, filename, linenumber);

  return V_realloc_it(free, ptr, size, filename, linenumber);
}

// Note, buggy system libraries may cause the memory validator to crash.
// In that case, just change "#if defined(RELEASE)" to "#if 0" below.
//
//#if 0
#if !defined(RELEASE)

void* operator new(size_t size){
  static int num=1;
  //printf("new: %d. size: %d\n",num++,(int)size);
  //return malloc(size);
  if(true || num>100)
    return V_malloc(size);
  else
    return malloc(size);
}

void operator delete (void* mem){
  V_free(mem);
}

void* operator new[](size_t size){
  return V_malloc(size);
}

void operator delete[](void* mem){
  V_free(mem);
}

#endif // !RELEASE



#ifdef TEST_MAIN
int main(int argc, char **argv){
  const int num = 1000000;
  int32_t *a[num];
  
  for(int i=0 ; i<num ; i++)
    a[i] = (int32_t*)V_alloc(dasmalloc,513,__FILE__,__LINE__);

  char *endpos = ((char*)a[4] ) + 513;
                   
  endpos[1] = 23;

  //a[5][516/4 + 14] = 7;
  
  //V_validate();
  
  for(int i = num-1 ; i >= 0 ; i--)
    V_free__(dasfree, (void*)a[i]);

  printf("g_root: %p, g_root_end: %p\n",g_root,g_root_end);
  
  return 0;
}
#endif


                
