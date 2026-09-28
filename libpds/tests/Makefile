# Makefile for the libpds test program.
#
# Run from this directory (or with make -C libpds/tests):
#
#   make        - build libpds_test
#   make run    - build and run libpds_test (runs audio for 10 seconds)
#   make clean  - remove build artifacts

LIBPD_DIR = ../../bin/packages/libpd
LIBPDS_DIR = ..
LIBPD = $(LIBPD_DIR)/libs/libpd.a
TARGET = libpds_test

CC ?= cc
CXX ?= g++

CPPFLAGS = -I$(LIBPD_DIR)/libpd_wrapper -I$(LIBPD_DIR)/pure-data/src
CFLAGS = -O2 -Wall -Wextra
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
LDLIBS = -lpthread -ldl -lm

.PHONY: all run clean

all: $(TARGET)

$(TARGET): libpds.o libpds_test.o $(LIBPD)
	$(CXX) -o $@ $^ $(LDLIBS)

libpds.o: $(LIBPDS_DIR)/libpds.c $(LIBPDS_DIR)/libpds.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

libpds_test.o: libpds_test.cpp $(LIBPDS_DIR)/libpds.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

$(LIBPD):
	cd $(LIBPD_DIR) && make MULTI=true UTIL=true EXTRA=true libs/libpd.a ADDITIONAL_CFLAGS="-g -fno-omit-frame-pointer"

run: $(TARGET)
	./$(TARGET) $(LIBPD_DIR)/pure-data

clean:
	rm -f *.o $(TARGET)
