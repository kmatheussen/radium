/* Copyright 2026 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cwchar>
#include <deque>
#include <string>

#include <QString>

#ifndef LANGSPEC
#  define LANGSPEC
#endif

#ifndef USE_QT4
#  define USE_QT4 1
#endif

typedef struct vector_t_ vector_t;

struct filepath_t
{
	const wchar_t *id;
};

static inline filepath_t make_filepath(const wchar_t *id)
{
	filepath_t ret = { id };
	return ret;
}

static inline wchar_t *talloc_wcsdup(const wchar_t *s)
{
	const size_t len = wcslen(s);
	wchar_t *ret = (wchar_t*)malloc((len + 1) * sizeof(wchar_t));
	if (ret != NULL)
	  memcpy(ret, s, (len + 1) * sizeof(wchar_t));
	return ret;
}

static inline wchar_t *V_wcsdup(const wchar_t *s)
{
	return talloc_wcsdup(s);
}

static inline void V_free(void *p)
{
	free(p);
}

static inline bool THREADING_is_main_thread(void)
{
	return true;
}

#define R_ASSERT(x) do {} while(0)
#define R_ASSERT_NON_RELEASE(x) do {} while(0)

extern const wchar_t *STRING_create(const char *s);
extern const wchar_t *STRING_create2(const QString s);

#include "../common/OS_string_proc.h"

QString FAUST_LLM_TEST_get_program_dir(void);
