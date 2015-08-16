CC ?= gcc
CCC ?= g++
PKG ?= pkg-config
MOC ?= moc-qt4
QMAKE ?= qmake-qt4

all: s7webserver

clean:
	rm -fr *.o mus-config.h master.tar.gz qhttpserver-master.tar.gz qhttpserver-master s7webserver moc_s7webserver.cpp

s7webserver: s7webserver.o qhttpserver-master/lib/libqhttpserver.a s7.o mus-config.h
	$(CCC) s7webserver.o `$(PKG) --libs QtNetwork` qhttpserver-master/lib/libqhttpserver.a s7.o -ldl -o s7webserver

s7webserver.o: s7webserver.h s7webserver.cpp moc_s7webserver.cpp qhttpserver-master/lib/libqhttpserver.a mus-config.h
	$(CCC) -c s7webserver.cpp -DWITH_MAIN `$(PKG) --cflags QtNetwork` -Iqhttpserver-master/src -I. -I.. -Wall -Werror -Wno-error=unused-variable -Wno-error=unused-function

moc_s7webserver.cpp: s7webserver.h qhttpserver-master/lib/libqhttpserver.a mus-config.h
	$(MOC) -DCOMPILING_S7WEBSERVER s7webserver.h -o moc_s7webserver.cpp

s7.o: ../s7.c ../s7.h mus-config.h
	$(CC) -c ../s7.c -I. -I..

mus-config.h:
	touch mus-config.h

qhttpserver-master/lib/libqhttpserver.a:
	rm -fr qhttpserver-master.tar.gz qhttpserver-master
	wget https://github.com/kmatheussen/qhttpserver/archive/master.tar.gz
	mv master.tar.gz qhttpserver-master.tar.gz
	tar xvzf qhttpserver-master.tar.gz
	echo "CONFIG += staticlib" >> qhttpserver-master/src/src.pro
	cd qhttpserver-master && $(QMAKE)
	cd qhttpserver-master && make
