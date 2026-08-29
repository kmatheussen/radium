#pragma once

#if defined(FOR_MACOSX)

#include <sys/sysctl.h>

static inline bool OS_running_under_rosetta(void)
{
	int translated = 0;
	
	size_t size = sizeof(translated);
	
	if (sysctlbyname("sysctl.proc_translated", &translated, &size, NULL, 0) != 0)
		return false;
	
	return translated == 1;
}

#else

static inline bool OS_running_under_rosetta(void)
{
	return false;
}

#endif
