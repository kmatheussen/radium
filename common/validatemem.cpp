
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

#include "nsmtracker.h"


#ifndef BUF_BEFORE
# define BUF_BEFORE 512
#endif

#ifndef BUF_AFTER
# define BUF_AFTER 512
#endif

#define CLEAR_BYTE 0x4f
#define CLEAR_32BYTE 0x4f4f4f4f


#define ALIGN_UP(value) (((uintptr_t)value + sizeof(int32_t) - 1) & -sizeof(int32_t))



typedef void *(*MemoryAllocator)(int size);
typedef void (*MemoryFreeer)(void* mem);

namespace{
struct Memlink{
  Memlink *next;
  Memlink *prev;
  int size;
  const char *filename;
  int linenumber;
};
}

// TODO: Keep these out of GC.
static Memlink *g_root;
static Memlink *g_root_end;

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

static void add_memlink(Memlink *link, int size, const char *filename, int linenumber){
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
  link->filename = filename;
  link->linenumber = linenumber;
}

static void print_error(Memlink *link, bool is_after, bool is32, int pos, int32_t value){
  fprintf(stderr,
          "MEMORY CORRUPTION. Memory %s allocated memory overwritten. Pos %d (%s bit aligned). Value: %d (0x%x). %s:%d\n",
          is_after?"after":"before",
          pos,
          is32 ? "32" : "8",
          value,
          value,
          link->filename,
          link->linenumber
          );
  abort();  
}

static void validate_link(Memlink *link){
  int32_t *mem_start1 = (int32_t*) (((char*)link) + sizeof(Memlink));
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

void V_validate(void){
  Memlink *link = g_root;

  while(link!=NULL){

    validate_link(link);
    
    link = link->next;
  }
}

static void mymemset(char *char_pos, int num_chars){
  int size = num_chars / 4;
  int32_t *pos = (int32_t*)char_pos;
  for(int a=0 ; a<size ; a++)
    pos[a] = CLEAR_32BYTE;
}

void *V_alloc(MemoryAllocator allocator, int size, const char *filename, int linenumber){

  if (BUF_BEFORE % 4 != 0) {
    fprintf(stderr, "BUF_BEFORE must be dividable by 4\n");
    abort();
  }
  
  if (BUF_AFTER % 4 != 0) {
    fprintf(stderr, "BUF_AFTER must be dividable by 4\n");
    abort();
  }

  int aligned_size = ALIGN_UP(size);
  
  int32_t *mem = (int32_t*)allocator(aligned_size+BUF_BEFORE+BUF_AFTER+sizeof(Memlink));
  if(mem==NULL)
    abort();
  
  //memset(mem + sizeof(Memlink), CLEAR_32BYTE, BUF_BEFORE);
  //memset(mem + sizeof(Memlink) + BUF_BEFORE + size, CLEAR_32BYTE, BUF_AFTER);
  
  mymemset(((char*)mem) + sizeof(Memlink), BUF_BEFORE);
  char *end_pos = ((char*)mem) + sizeof(Memlink) + BUF_BEFORE + size;
  memset(end_pos, CLEAR_BYTE, aligned_size - size);
  mymemset((char*)ALIGN_UP(end_pos), BUF_AFTER);
  
  Memlink *link = (Memlink *)mem;

  add_memlink(link, size, filename, linenumber);
  
  return (void*)( ((char*)mem) + sizeof(Memlink) + BUF_BEFORE);
}

void V_free_it(MemoryFreeer freeer, void *allocated_mem){
  int32_t *start = (int32_t*) ( ((char*)allocated_mem) - sizeof(Memlink) - BUF_BEFORE );

  Memlink *link = (Memlink *)start;

  validate_link(link);
  
  remove_memlink(link);
                 
  freeer((void*)start);
}



static void *dasmalloc(int size){
  return malloc(size);
}

static void *dascalloc(int size){
  return calloc(1, size);
}

static void dasfree(void *mem){
  free(mem);
}

void *V_malloc__(size_t size, const char *filename, int linenumber){
  return V_alloc(dasmalloc ,size, filename, linenumber);
}

void *V_calloc__(size_t n, size_t size, const char *filename, int linenumber){
  return V_alloc(dascalloc, n*size, filename, linenumber);
}

void V_free__(void *ptr){
  V_free_it(dasfree, ptr);
}

void *V_realloc__(void *ptr, size_t size, const char *filename, int linenumber){
  Memlink *link = (Memlink *) ( ((char*)ptr) - sizeof(Memlink) - BUF_BEFORE );

  void *new_mem = V_alloc(dasmalloc, size, filename, linenumber);
  memcpy(new_mem, ptr, R_MIN((int)size, link->size));
  
  V_free__(ptr);

  return new_mem;
}

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


                
