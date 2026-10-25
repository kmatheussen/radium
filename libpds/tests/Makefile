# Makefile for the libpds test programs.
#
# Run from this directory (or with make -C libpds/tests):
#
#   make        - build libpds_test and libpds_gui_test
#   make run    - build and run libpds_test (runs audio for 10 seconds)
#   make run-gui - build and run libpds_gui_test (shows two patch windows for
#                 15 seconds)
#   make run-externals
#               - build and run both Pd externals tests: lazy loading and
#                 exhaustive loading/instantiation (requires bin/pd/externals
#                 to be built)
#   make run-externals-load
#               - build and run only the exhaustive externals load test
#   make tsan   - build and run libpds_test with ThreadSanitizer
#   make clean  - remove build artifacts

LIBPD_DIR = ../../bin/packages/libpd
LIBPDS_DIR = ..
LIBPD = $(LIBPD_DIR)/libs/libpd.a
ifeq ($(shell uname -s),Darwin)
LIBPD_SHARED = $(LIBPD_DIR)/libs/libpd.dylib
else
LIBPD_SHARED = $(LIBPD_DIR)/libs/libpd.so
endif
TARGET = libpds_test
GUI_TARGET = libpds_gui_test
EXTERNALS_TARGET = test_pd_externals
EXTERNALS_LOAD_TARGET = test_pd_externals_load
TSAN_TARGET = libpds_test_tsan

CC ?= cc
CXX ?= g++

CPPFLAGS = -I$(LIBPD_DIR)/libpd_wrapper -I$(LIBPD_DIR)/pure-data/src
CFLAGS = -O2 -Wall -Wextra
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
LDLIBS = -lpthread -ldl -lm

TSAN_FLAGS = -fsanitize=thread -g -fno-omit-frame-pointer

.PHONY: all run run-gui run-externals run-externals-load tsan clean

all: $(TARGET) $(GUI_TARGET) $(EXTERNALS_TARGET) $(EXTERNALS_LOAD_TARGET)

$(TARGET): libpds.o libpds_test.o $(LIBPD)
	$(CXX) -o $@ $^ $(LDLIBS)

$(GUI_TARGET): libpds.o libpds_gui_test.o $(LIBPD)
	$(CXX) -o $@ $^ $(LDLIBS)

$(EXTERNALS_TARGET) $(EXTERNALS_LOAD_TARGET): %: libpds.o %.o $(LIBPD_SHARED)
	$(CXX) -o $@ $^ $(LDLIBS) -Wl,-rpath,$(abspath $(LIBPD_DIR)/libs)
ifeq ($(shell uname -s),Darwin)
	# libpd.dylib is built for Radium, with an @executable_path install name.
	# Point the test binary at the real file instead.
	install_name_tool -change @executable_path/packages/libpd/libs/libpd.dylib $(abspath $(LIBPD_SHARED)) $@
else
	@true
endif

libpds.o: $(LIBPDS_DIR)/libpds.c $(LIBPDS_DIR)/libpds.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

libpds_test.o: libpds_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

libpds_gui_test.o: libpds_gui_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

test_pd_externals.o: test_pd_externals.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

test_pd_externals_load.o: test_pd_externals_load.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

$(TSAN_TARGET): libpds_tsan.o libpds_test_tsan.o $(LIBPD)
	$(CXX) $(TSAN_FLAGS) -o $@ $^ $(LDLIBS)

libpds_tsan.o: $(LIBPDS_DIR)/libpds.c $(LIBPDS_DIR)/libpds.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(TSAN_FLAGS) -c -o $@ $<

libpds_test_tsan.o: libpds_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(TSAN_FLAGS) -c -o $@ $<

$(LIBPD):
	cd $(LIBPD_DIR) && make MULTI=true UTIL=true EXTRA=true libs/libpd.a ADDITIONAL_CFLAGS="-g -fno-omit-frame-pointer"

$(LIBPD_SHARED):
	cd $(LIBPD_DIR) && make MULTI=true UTIL=true EXTRA=true libpd ADDITIONAL_CFLAGS="-g -fno-omit-frame-pointer"

run: $(TARGET)
	./$(TARGET) $(LIBPD_DIR)/pure-data

run-gui: $(GUI_TARGET)
	./$(GUI_TARGET) $(LIBPD_DIR)/pure-data

run-externals: $(EXTERNALS_TARGET) $(EXTERNALS_LOAD_TARGET)
	./$(EXTERNALS_TARGET) ../../bin/pd/externals $(LIBPD_DIR)/pure-data
	./$(EXTERNALS_LOAD_TARGET) ../../bin/pd/externals $(LIBPD_DIR)/pure-data

run-externals-load: $(EXTERNALS_LOAD_TARGET)
	./$(EXTERNALS_LOAD_TARGET) ../../bin/pd/externals $(LIBPD_DIR)/pure-data

tsan: $(TSAN_TARGET)
	./$(TSAN_TARGET) $(LIBPD_DIR)/pure-data

clean:
	rm -f *.o $(TARGET) $(GUI_TARGET) $(EXTERNALS_TARGET) $(EXTERNALS_LOAD_TARGET) $(TSAN_TARGET)
