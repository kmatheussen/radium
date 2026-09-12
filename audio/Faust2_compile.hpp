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

#include <QString>

#include <map>
#include <sndfile.h>
#include <string>
#include <vector>

#ifndef R_ASSERT_RETURN_IF_FALSE2
#  define R_ASSERT_RETURN_IF_FALSE2(a, b) do { if (!(a)) return (b); } while(0)
#endif

#include "../Qt/MyQTemporaryDir.hpp"

class dsp;
class dsp_factory;
class interpreter_dsp_factory;
class llvm_dsp_factory;
struct Soundfile;

#define MAX_CHANNELS 16

struct FaustDev2SoundfileData
{
	std::map<std::string, Soundfile*> url2soundfile;
	std::vector<Soundfile*> owned;

	~FaustDev2SoundfileData();
};

struct FaustDev2CompileConfig
{
	QString code;
	QString options;
	bool use_interpreter_backend = true;
	QString radium_path;
};

struct Faust2CompileCheckResult
{
	bool ok = false;
	QString error_message;
	int num_inputs = 0;
	int num_outputs = 0;
	bool is_instrument = false;
};

void delete_factory(
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
	llvm_dsp_factory *llvm_factory,
#endif
	interpreter_dsp_factory *interp_factory);

dsp_factory *create_factory(const FaustDev2CompileConfig &config,
                            int optlevel,
                            QString &error_message,
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
                            llvm_dsp_factory **out_llvm_factory,
#endif
                            interpreter_dsp_factory **out_interp_factory,
                            MyQTemporaryDir **out_svg_dir,
                            FaustDev2SoundfileData **out_soundfile_data);

bool FAUST2_compile_check(const FaustDev2CompileConfig &config,
                          int optlevel,
                          Faust2CompileCheckResult *result);

typedef SNDFILE *(*Faust2SndfileOpenFunc)(const char *path, int mode, SF_INFO *snd_info);
void FAUST2_set_sndfile_open_func(Faust2SndfileOpenFunc func);

typedef void (*Faust2SoundfileWarningFunc)(const QString &filename);
void FAUST2_set_soundfile_warning_func(Faust2SoundfileWarningFunc func);
