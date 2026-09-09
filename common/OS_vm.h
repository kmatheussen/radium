#pragma once

#if defined(FOR_MACOSX)

#include <sys/sysctl.h>
#include <strings.h>

static inline bool OS_running_under_vm(void)
{
	int vmm_present = 0;
	size_t size = sizeof(vmm_present);

	if (sysctlbyname("kern.hv_vmm_present", &vmm_present, &size, NULL, 0) == 0 && vmm_present == 1)
		return true;

	char brand_string[128] = {};
	size = sizeof(brand_string);

	if (sysctlbyname("machdep.cpu.brand_string", brand_string, &size, NULL, 0) == 0)
		if (strcasestr(brand_string, "QEMU") != NULL)
			return true;

	char model[128] = {};
	size = sizeof(model);

	if (sysctlbyname("hw.model", model, &size, NULL, 0) == 0)
	{
		if (strcasestr(model, "VMware") != NULL)
			return true;
		if (strcasestr(model, "VirtualBox") != NULL)
			return true;
		if (strcasestr(model, "BHYVE") != NULL)
			return true;
	}

	return false;
}

#else

static inline bool OS_running_under_vm(void)
{
	return false;
}

#endif
