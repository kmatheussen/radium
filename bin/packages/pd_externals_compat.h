#ifndef PD_EXTERNALS_COMPAT_H
#define PD_EXTERNALS_COMPAT_H

// Compatibility header for the dynamically loaded Pd externals built by
// build_pd_externals() in build.sh.
//
// The iemnet sources target Pd <= 0.51, which still had the global error()
// function. It was removed later; pd_error(0, ...) is the replacement.

#include "m_pd.h"

#ifndef error
#define error(...) pd_error(0, __VA_ARGS__)
#endif

#endif
