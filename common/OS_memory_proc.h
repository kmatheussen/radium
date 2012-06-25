
#ifdef __cplusplus
extern "C" {
#endif

void uinit_memory(void);

void OS_tfree(void *element);
void *OS_getmem(size_t size);

#ifdef __cplusplus
}
#endif


