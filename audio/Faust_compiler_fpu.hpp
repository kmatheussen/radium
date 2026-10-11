#pragma once

#if defined(__SSE__)
#  include <xmmintrin.h>
#endif

namespace radium
{

class ScopedDenormalsDisabled
{
#if defined(__SSE__)
	unsigned int _saved;
#endif

public:
	ScopedDenormalsDisabled(void)
#if defined(__SSE__)
		: _saved(_mm_getcsr())
	{
		_mm_setcsr(_saved & ~0x8040u);
	}
#else
	{
	}
#endif

	~ScopedDenormalsDisabled(void)
	{
#if defined(__SSE__)
		_mm_setcsr(_saved);
#endif
	}

	ScopedDenormalsDisabled(const ScopedDenormalsDisabled &) = delete;
	ScopedDenormalsDisabled &operator=(const ScopedDenormalsDisabled &) = delete;
};

} // namespace radium
