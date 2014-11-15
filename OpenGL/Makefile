#HOME=${HOME}

VL_PATH=$(HOME)/Visualization-Library

VL_CFLAGS=-DVL_STATIC_LINKING -Wall -I$(VL_PATH)/src -I$(VL_PATH)/src/3rdparty/Khronos -I$(VL_PATH)/src/examples

VL_LDFLAGS=$(VL_PATH)/src/examples/Applets/lib/libVLApplets.a $(VL_PATH)/src/vlQt4/lib/libVLQt4.a $(VL_PATH)/src/vlMolecule/lib/libVLMolecule.a $(VL_PATH)/src/vlVG/lib/libVLVG.a $(VL_PATH)/src/vlVolume/lib/libVLVolume.a $(VL_PATH)/src/vlGraphics/lib/libVLGraphics.a $(VL_PATH)/src/vlCore/lib/libVLCore.a -lGLU -lGL $(VL_PATH)/src/vlCore/plugins/libpng/lib/libPNG.a $(VL_PATH)/src/vlGraphics/plugins/freetype/lib/libFreeType.a $(VL_PATH)/src/vlCore/plugins/libtiff/lib/libTIFF.a $(VL_PATH)/src/vlCore/plugins/jpeg/lib/libJPG.a $(VL_PATH)/src/vlCore/plugins/zlib/lib/libZLib.a

OPT=-Wno-error=unused-function -O3 -mtune=native

run: a.out
	./a.out

run-gdb: a.out
	gdb ./a.out

clean:
	rm -f a.out *.o

a.out: GfxElements.o test.o  $(VL_LDFLAGS)
	g++ GfxElements.o test.o ../memory.o ../bin/packages/gc-7.2/.libs/libgc.a -lpthread `pkg-config --libs QtOpenGL` $(VL_LDFLAGS)

#GfxElements.o: GfxElements.c
#	gcc GfxElements.c -c -Wall -g $(OPT) -Werror -I../Qt -DDEBUG -DGUIISQT -DFOR_LINUX -DWITH_PD -DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0 -DUSE_QT_REQTYPE=1 -DUSE_GTK_REQTYPE=0 -DUSE_QT_MENU=1 -DUSE_GTK_MENU=0 -DUSE_VESTIGE=1 -DFULL_VERSION=1 -DIS_LITTLE_ENDIAN=1 -I../bin/packages/gc-7.2/include

test.o: test.cpp GfxElements.h
	g++ test.cpp $(VL_CFLAGS) -c -Wall -g $(OPT) -Werror -I../Qt -DDEBUG -DGUIISQT -DFOR_LINUX -DWITH_PD -DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0 -DUSE_QT_REQTYPE=1 -DUSE_GTK_REQTYPE=0 -DUSE_QT_MENU=1 -DUSE_GTK_MENU=0 -DUSE_VESTIGE=1 -DFULL_VERSION=1 -DIS_LITTLE_ENDIAN=1 -I../bin/packages/gc-7.2/include `pkg-config --cflags QtOpenGL` `pkg-config --cflags Qt3Support` -DUSE_QT4=1 -DUSE_OPENGL=1 -DUSE_QIMAGE_BUFFER

GfxElements.o: GfxElements.cpp TextBitmaps.hpp GfxElements.h
	g++ GfxElements.cpp $(VL_CFLAGS) -c -Wall -g $(OPT) -Werror -I../Qt -DDEBUG -DGUIISQT -DFOR_LINUX -DWITH_PD -DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0 -DUSE_QT_REQTYPE=1 -DUSE_GTK_REQTYPE=0 -DUSE_QT_MENU=1 -DUSE_GTK_MENU=0 -DUSE_VESTIGE=1 -DFULL_VERSION=1 -DIS_LITTLE_ENDIAN=1 -I../bin/packages/gc-7.2/include `pkg-config --cflags QtOpenGL` `pkg-config --cflags Qt3Support` -DUSE_QT4=1 -DUSE_OPENGL=1 -DUSE_QIMAGE_BUFFER

