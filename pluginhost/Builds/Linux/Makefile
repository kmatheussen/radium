# Automatically generated makefile, created by the Introjucer
# Don't edit this file! Your changes will be overwritten when you re-save the Introjucer project!

# (this disables dependency generation if multiple architectures are set)
DEPFLAGS := $(if $(word 2, $(TARGET_ARCH)), , -MMD)

ifndef CONFIG
  CONFIG=Debug
endif

DAS_ARCH:=$(shell arch)
DAS_OS:=$(shell uname -s)


AVAILABLES=
#"-DJUCE_MODULE_AVAILABLE_juce_audio_basics=1" "-DJUCE_MODULE_AVAILABLE_juce_audio_devices=1" "-DJUCE_MODULE_AVAILABLE_juce_audio_formats=1" "-DJUCE_MODULE_AVAILABLE_juce_audio_processors=1" "-DJUCE_MODULE_AVAILABLE_juce_audio_utils=1" "-DJUCE_MODULE_AVAILABLE_juce_core=1 -DJUCE_MODULE_AVAILABLE_juce_data_structures=1" "-DJUCE_MODULE_AVAILABLE_juce_dsp=1" "-DJUCE_MODULE_AVAILABLE_juce_events=1" "-DJUCE_MODULE_AVAILABLE_juce_graphics=1" "-DJUCE_MODULE_AVAILABLE_juce_gui_basics=1" "-DJUCE_MODULE_AVAILABLE_juce_gui_extra=1"

PWD=$(shell pwd)

JUCE_MODULES=$(PWD)/../../JuceLibraryCode/modules/

LV2FLAGS=-I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/lv2  -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/lilv/src -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/lilv -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/sratom -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/sord/src -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/sord -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/serd -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK/lv2 -I$(JUCE_MODULES)juce_audio_processors/format_types/LV2_SDK

RADIUM_VST2SDK_PATH ?= ${HOME}/SDKs/vstsdk2.4

COMMONCPPFLAGS := $(DEPFLAGS) -D "JUCE_CHECK_MEMORY_LEAKS=0" -D "JUCER_LINUX_MAKE_7346DA2A=1" -D "JUCE_APP_VERSION=1.0.0" -D "JUCE_APP_VERSION_HEX=0x10000" $(AVAILABLES) -I /usr/include -I /usr/include/freetype2 -I$(JUCE_MODULES)/.. -I$(JUCE_MODULES) -I$(JUCE_MODULES)juce_audio_processors/format_types/VST3_SDK -I $(JUCE_MODULES)JuceLibraryCode -I $(JUCE_MODULES)JuceLibraryCode/modules -I$(RADIUM_VST2SDK_PATH) -DJUCE_APP_CONFIG_HEADER="\"AppConfig.h\"" -DPLUGINHOST_BUILD_64BIT -fPIC $(LV2FLAGS)

ifeq ($(DAS_ARCH),x86_64)
	COMMONCPPFLAGS += -msse2 -mfpmath=sse
endif

ifeq ($(DAS_OS),Linux)
	COMMONCPPFLAGS += -D "LINUX=1"
endif

ifeq ($(DAS_OS),Darwin)
	COMMONCPPFLAGS += -mmacosx-version-min=${MACOSX_DEPLOYMENT_TARGET}
endif

# -I~/SDKs/VST_SDK/VST3_SDK -I ~/SDKs/VST_SDK/VST2_SDK/ -I ~/SDKs/VST3\ SDK

ifeq (,$(findstring clang,$(CXX)))
	COMMONCPPFLAGS += -Wno-class-memaccess 
	COMMONCPPFLAGS += -Wno-deprecated-copy
endif


COMMON_SUFFIX=cpp
ifeq ($(DAS_OS),Darwin)
	COMMON_SUFFIX=mm
endif



# DEBUG
ifeq ($(CONFIG),Debug)
  BINDIR := build
  LIBDIR := build
  OBJDIR := build/intermediate/Debug
  OUTDIR := build

  ifeq ($(TARGET_ARCH),)
    TARGET_ARCH := 
  endif

  CPPFLAGS := $(COMMONCPPFLAGS) -D_DEBUG=1

  CFLAGS += $(CPPFLAGS) $(TARGET_ARCH) -g -ggdb -O0 -I../../../Qt `cat ../../../flagopts.opt`
  CXXFLAGS += $(CFLAGS) -std=gnu++17
  LDFLAGS += $(TARGET_ARCH) -L$(BINDIR) -L$(LIBDIR) -L/usr/X11R6/lib/ -lX11 -lXext -lXinerama -lasound -ldl -lfreetype -lpthread -lrt 

  TARGET ?= libMyPluginHost.a
  BLDCMD = ar -rcs $(OUTDIR)/$(TARGET) $(OBJECTS)
  CLEANCMD = rm -rf $(OUTDIR)/$(TARGET) $(OBJDIR)
endif

# RELEASE
ifeq ($(CONFIG),Release)
  BINDIR := build
  LIBDIR := build
  OBJDIR := build/intermediate/Release
  OUTDIR := build

  ifeq ($(TARGET_ARCH),)
    TARGET_ARCH := 
  endif

  CPPFLAGS := $(COMMONCPPFLAGS) -DDEB1UG=1 -DNDEBUG=1

  CFLAGS += $(CPPFLAGS) $(TARGET_ARCH) -g -ggdb -I../../../Qt `cat ../../../flagopts.opt` -O3 
  CXXFLAGS += $(CFLAGS) -std=gnu++17
  LDFLAGS += $(TARGET_ARCH) -L$(BINDIR) -L$(LIBDIR) -L/usr/X11R6/lib/ -lX11 -lXext -lXinerama -lasound -ldl -lfreetype -lpthread -lrt 

  TARGET ?= libMyPluginHost.a
  BLDCMD = ar -rcs $(OUTDIR)/$(TARGET) $(OBJECTS)
  CLEANCMD = rm -rf $(OUTDIR)/$(TARGET) $(OBJDIR)
endif

OBJECTS := \
  $(OBJDIR)/Juce_plugins_6c083ccb.o \
  $(OBJDIR)/Juce_plugin_scanner.o \
  $(OBJDIR)/juce_audio_basics_2442e4ea.o \
  $(OBJDIR)/juce_audio_utils_2442e4ea.o \
  $(OBJDIR)/juce_audio_devices_a4c8a728.o \
  $(OBJDIR)/juce_audio_formats_d349f0c8.o \
  $(OBJDIR)/juce_audio_processors_44a134a2.o \
  $(OBJDIR)/juce_core_aff681cc.o \
  $(OBJDIR)/juce_data_structures_bdd6d488.o \
  $(OBJDIR)/juce_events_79b2840.o \
  $(OBJDIR)/juce_graphics_c8f1e7a4.o \
  $(OBJDIR)/juce_gui_basics_a630dd20.o \
  $(OBJDIR)/juce_gui_extra_7767d6a8.o \
  $(OBJDIR)/include_juce_audio_processors_lv2_libs_12bdca08.o \

.PHONY: clean

$(OUTDIR)/$(TARGET): $(OBJECTS) $(RESOURCES)
	@echo Linking $(OUTDIR)/$(TARGET)
	-@mkdir -p $(BINDIR)
	-@mkdir -p $(LIBDIR)
	-@mkdir -p $(OUTDIR)
	@$(BLDCMD)

clean:
	@echo Cleaning MyPluginHost
	@$(CLEANCMD)

strip:
	@echo Stripping MyPluginHost
	-@strip --strip-unneeded $(OUTDIR)/$(TARGET)

R_OPTS=-Wall -Wextra -Wno-unused-parameter -Wsuggest-override

$(OBJDIR)/Juce_plugins_6c083ccb.o: ../../../audio/Juce_plugins.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling Juce_plugins.cpp"
	$(CXX) $(CXXFLAGS) $(R_OPTS) -o "$@" -c "$<"

$(OBJDIR)/Juce_plugin_scanner.o: ../../../audio/Juce_plugin_scanner.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling Juce_plugins.cpp"
	@$(CXX) $(CXXFLAGS) $(R_OPTS) -o "$@" -c "$<"
#-Werror

$(OBJDIR)/juce_audio_basics_2442e4ea.o: ../../JuceLibraryCode/modules/juce_audio_basics/juce_audio_basics.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_audio_basics.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_audio_utils_2442e4ea.o: ../../JuceLibraryCode/modules/juce_audio_utils/juce_audio_utils.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_audio_utils.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_audio_devices_a4c8a728.o: ../../JuceLibraryCode/modules/juce_audio_devices/juce_audio_devices.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_audio_devices.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_audio_formats_d349f0c8.o: ../../JuceLibraryCode/modules/juce_audio_formats/juce_audio_formats.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_audio_formats.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_audio_processors_44a134a2.o: ../../JuceLibraryCode/modules/juce_audio_processors/juce_audio_processors.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_audio_processors.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/include_juce_audio_processors_lv2_libs_12bdca08.o: ../../JuceLibraryCode/include_juce_audio_processors_lv2_libs.cpp
	-@mkdir -p $(JUCE_OBJDIR)
	@echo "Compiling include_juce_audio_processors_lv2_libs.cpp"
	$(CXX) $(CXXFLAGS) -I../../JuceLibraryCode/modules/ $(JUCE_CPPFLAGS_APP) $(JUCE_CFLAGS_APP) -o "$@" -c "$<"

$(OBJDIR)/juce_core_aff681cc.o: ../../JuceLibraryCode/modules/juce_core/juce_core.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_core.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_data_structures_bdd6d488.o: ../../JuceLibraryCode/modules/juce_data_structures/juce_data_structures.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_data_structures.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_events_79b2840.o: ../../JuceLibraryCode/modules/juce_events/juce_events.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_events.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_graphics_c8f1e7a4.o: ../../JuceLibraryCode/modules/juce_graphics/juce_graphics.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_graphics.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_gui_basics_a630dd20.o: ../../JuceLibraryCode/modules/juce_gui_basics/juce_gui_basics.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_gui_basics.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_gui_extra_7767d6a8.o: ../../JuceLibraryCode/modules/juce_gui_extra/juce_gui_extra.$(COMMON_SUFFIX)
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_gui_extra.$(COMMON_SUFFIX)"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

-include $(OBJECTS:%.o=%.d)
