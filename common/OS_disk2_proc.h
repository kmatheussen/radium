// 'program' must be placed in the program bin path.
// The returned value must be manually freed.
// Can be called from any thread.
// Leaks a little bit of memory.
#ifdef __cplusplus
extern "C" {
#endif
  const wchar_t *DISK_run_program_that_writes_to_temp_file(const wchar_t *program, const wchar_t *arg1, const wchar_t *arg2, const wchar_t *arg3);
#ifdef __cplusplus
}
#endif

  

