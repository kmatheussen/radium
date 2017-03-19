// 'program' must be placed in the program bin path.
// The returned value must be manually freed.
// Can be called from any thread.
// Leaks a little bit of memory.
#if __cplusplus
extern "C" {
#endif
  char *DISK_run_program_that_writes_to_temp_file(const char *program, const char *arg1, const char *arg2, const char *arg3);
#if __cplusplus
}
#endif

  

