# Makefile for the libpds test programs.
#
# Run from this directory (or with make -C libpds/tests):
#
#   make        - build libpds_test and libpds_gui_test
#   make run    - build and run libpds_test (runs audio for 10 seconds)
#   make run-gui - build and run libpds_gui_test (shows two patch windows for
#                 15 seconds)
#   make tsan   - build and run libpds_test with ThreadSanitizer
#   make clean  - remove build artifacts

LIBPD_DIR = ../../bin/packages/libpd
LIBPDS_DIR = ..
LIBPD = $(LIBPD_DIR)/libs/libpd.a
TARGET = libpds_test
GUI_TARGET = libpds_gui_test
TSAN_TARGET = libpds_test_tsan

CC ?= cc
CXX ?= g++

CPPFLAGS = -I$(LIBPD_DIR)/libpd_wrapper -I$(LIBPD_DIR)/pure-data/src
CFLAGS = -O2 -Wall -Wextra
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
LDLIBS = -lpthread -ldl -lm

TSAN_FLAGS = -fsanitize=thread -g -fno-omit-frame-pointer

.PHONY: all run run-gui tsan clean

all: $(TARGET) $(GUI_TARGET)

$(TARGET): libpds.o libpds_test.o $(LIBPD)
	$(CXX) -o $@ $^ $(LDLIBS)

$(GUI_TARGET): libpds.o libpds_gui_test.o $(LIBPD)
	$(CXX) -o $@ $^ $(LDLIBS)

libpds.o: $(LIBPDS_DIR)/libpds.c $(LIBPDS_DIR)/libpds.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

libpds_test.o: libpds_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

libpds_gui_test.o: libpds_gui_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

$(TSAN_TARGET): libpds_tsan.o libpds_test_tsan.o $(LIBPD)
	$(CXX) $(TSAN_FLAGS) -o $@ $^ $(LDLIBS)

libpds_tsan.o: $(LIBPDS_DIR)/libpds.c $(LIBPDS_DIR)/libpds.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(TSAN_FLAGS) -c -o $@ $<

libpds_test_tsan.o: libpds_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(TSAN_FLAGS) -c -o $@ $<

$(LIBPD):
	cd $(LIBPD_DIR) && make MULTI=true UTIL=true EXTRA=true libs/libpd.a ADDITIONAL_CFLAGS="-g -fno-omit-frame-pointer"

run: $(TARGET)
	./$(TARGET) $(LIBPD_DIR)/pure-data

run-gui: $(GUI_TARGET)
	./$(GUI_TARGET) $(LIBPD_DIR)/pure-data

tsan: $(TSAN_TARGET)
	./$(TSAN_TARGET) $(LIBPD_DIR)/pure-data

clean:
	rm -f *.o $(TARGET) $(GUI_TARGET) $(TSAN_TARGET)
