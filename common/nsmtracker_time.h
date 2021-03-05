#ifdef __cplusplus
extern "C"{
#endif

  // Both of these should be very efficient.
  extern double TIME_get_ms(void); // Calls gettimeofday.
  extern double monotonic_seconds(void); // Calls a monotonic high resolution monotonic timer.
  extern double RT_TIME_get_ms(void); // Calls a monotonic high resolution monotonic timer.

  extern void TIME_init(void);
  
#ifdef __cplusplus
}
#endif
