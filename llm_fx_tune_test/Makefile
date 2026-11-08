ROOT := ..

PKGCONFIG ?= pkg-config
QT_PKGCONFIG_PATH ?= /opt/local/libexec/qt6/lib/pkgconfig

# Only override the pkg-config search path when the directory exists (on
# systems where Qt6 is found through the default search path, setting a
# non-existent PKG_CONFIG_PATH would hide it).
ifeq ($(wildcard $(QT_PKGCONFIG_PATH)),)
QT_ENV :=
else
QT_ENV := PKG_CONFIG_PATH=$(QT_PKGCONFIG_PATH)
endif

QT_CFLAGS := $(shell $(QT_ENV) $(PKGCONFIG) --cflags Qt6Core Qt6Network Qt6Concurrent 2>/dev/null)
QT_LIBS := $(shell $(QT_ENV) $(PKGCONFIG) --libs Qt6Core Qt6Network Qt6Concurrent 2>/dev/null)

CXX ?= clang++
OPTFLAGS ?= -g -O1

CPPFLAGS := -std=gnu++20 -fPIC -D__STDC_FORMAT_MACROS=1 -DUSE_QT4 -DUSE_QT5 \
	-I$(ROOT) -I$(ROOT)/Qt \
	$(QT_CFLAGS) \
	-Wno-deprecated-declarations -Wno-unused-parameter

# AudioAnalyzer.cpp links libfftw3f directly (the app does the same through
# build_linux_common.sh).
FFTW_CFLAGS := $(shell $(PKGCONFIG) --cflags fftw3f 2>/dev/null)
FFTW_LIBS := $(shell $(PKGCONFIG) --libs fftw3f 2>/dev/null)

LDFLAGS := $(QT_LIBS) $(FFTW_LIBS)

OBJECTS := main.o ../audio/AudioAnalyzer.o

all: llm_fx_tune_test

../audio/AudioAnalyzer.o: ../audio/AudioAnalyzer.cpp ../audio/AudioAnalyzer.h
	$(CXX) $(CPPFLAGS) $(FFTW_CFLAGS) $(OPTFLAGS) -c $< -o $@

llm_fx_tune_test: $(OBJECTS)
	$(CXX) $(OPTFLAGS) $(OBJECTS) $(LDFLAGS) -o $@

%.o: %.cpp
	$(CXX) $(CPPFLAGS) $(OPTFLAGS) -MMD -MP -c $< -o $@

-include $(OBJECTS:.o=.d)

clean:
	rm -f $(OBJECTS) $(OBJECTS:.o=.d) llm_fx_tune_test
