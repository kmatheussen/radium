#!/bin/sh

export VISUAL="-DUSE_QT_VISUAL=0 -DUSE_GTK_VISUAL=1"

export GTK_CFLAGS="-mms-bitfields -I/home/kjetil/radium-qt4/temp/include/gtk-2.0 -I/home/kjetil/radium-qt4/temp/lib/gtk-2.0/include -I/home/kjetil/radium-qt4/temp/include/atk-1.0 -I/home/kjetil/radium-qt4/temp/include/cairo -I/home/kjetil/radium-qt4/temp/include/gdk-pixbuf-2.0 -I/home/kjetil/radium-qt4/temp/include/pango-1.0 -I/home/kjetil/radium-qt4/temp/include/glib-2.0 -I/home/kjetil/radium-qt4/temp/lib/glib-2.0/include -I/home/kjetil/radium-qt4/temp/include -I/home/kjetil/radium-qt4/temp/include/freetype2 -I/home/kjetil/radium-qt4/temp/include/libpng14"
export GTK_LDFLAGS="-L/home/kjetil/radium-qt4/temp/lib -lgtk-win32-2.0 -lgdk-win32-2.0 -latk-1.0 -lgio-2.0 -lpangowin32-1.0 -lgdi32 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lpango-1.0 -lcairo -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl"

./build_mingw_common.sh $@
