ROOT := ..

PKGCONFIG ?= pkg-config
QT_PKGCONFIG_PATH ?= /opt/local/libexec/qt6/lib/pkgconfig

QT_CFLAGS := $(shell PKG_CONFIG_PATH=$(QT_PKGCONFIG_PATH) $(PKGCONFIG) --cflags Qt6Core Qt6Network Qt6Concurrent 2>/dev/null)
QT_LIBS := $(shell PKG_CONFIG_PATH=$(QT_PKGCONFIG_PATH) $(PKGCONFIG) --libs Qt6Core Qt6Network Qt6Concurrent 2>/dev/null)

FAUSTDIR ?= $(ROOT)/bin/packages/faust
FAUSTLIBDIR ?= $(FAUSTDIR)/build/lib
FAUSTLIBDIR_ABS := $(abspath $(FAUSTLIBDIR))

SNDFILE_CFLAGS := $(shell $(PKGCONFIG) --cflags sndfile)
SNDFILE_LIBS := $(shell $(PKGCONFIG) --libs sndfile)

CXX ?= clang++
OPTFLAGS ?= -g -O1

CPPFLAGS := -std=gnu++20 -fPIC -D__STDC_FORMAT_MACROS=1 -DUSE_QT4 -DUSE_QT5 -DFOR_MACOSX \
	-DWITHOUT_LLVM_IN_FAUST_DEV \
	-I$(ROOT) -I$(ROOT)/Qt -I$(FAUSTDIR)/architecture -I$(FAUSTDIR)/compiler \
	$(QT_CFLAGS) $(SNDFILE_CFLAGS) \
	-Wno-deprecated-declarations -Wno-unused-parameter

LDFLAGS := -L$(FAUSTLIBDIR) -lfaust $(SNDFILE_LIBS) $(QT_LIBS) -Wl,-rpath,$(FAUSTLIBDIR_ABS)

OBJECTS := main.o radium_env.o Faust2_compile.o Faust2_llm_lint.o

all: faust_llm_test

faust_llm_test: $(OBJECTS)
	$(CXX) $(OPTFLAGS) $(OBJECTS) $(LDFLAGS) -o $@

%.o: %.cpp
	$(CXX) $(CPPFLAGS) $(OPTFLAGS) -MMD -MP -c $< -o $@

Faust2_compile.o: $(ROOT)/audio/Faust2_compile.cpp
	$(CXX) $(CPPFLAGS) $(OPTFLAGS) -MMD -MP -c $< -o $@

Faust2_llm_lint.o: $(ROOT)/audio/Faust2_llm_lint.cpp
	$(CXX) $(CPPFLAGS) $(OPTFLAGS) -MMD -MP -c $< -o $@

-include $(OBJECTS:.o=.d)

clean:
	rm -f $(OBJECTS) $(OBJECTS:.o=.d) faust_llm_test
