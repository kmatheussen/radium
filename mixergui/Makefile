
OPT=-g -DTEST_MAIN -DQT_NO_OPENGL -DDEBUG -Wall
# -O3

CFLAGS=$(OPT)
CPPFLAGS=$(OPT)  -I../Qt `pkg-config --cflags QtGui`

MIXER_OBJS = QM_main.o QM_MixerWidget.o QM_view.o QM_chip.o SoundProducer.o Jack_plugin.o Sine_plugin.o SoundPluginRegistry.o Mixer.o images.o

all: $(MIXER_OBJS)
	g++ $(OPTS) $(MIXER_OBJS) -Wall `pkg-config --libs --cflags QtGui` -ljack

run-gdb: all
	gdb ./a.out

run: all
	./a.out

clean:
	rm -f *.o mQM_*.cpp

QM_view.cpp: mQM_view.cpp
QM_MixerWidget.cpp: mQM_MixerWidget.cpp

mQM_view.cpp: QM_view.h
	moc-qt4 QM_view.h >mQM_view.cpp

mQM_MixerWidget.cpp: QM_MixerWidget.h
	moc-qt4 QM_MixerWidget.h >mQM_MixerWidget.cpp

images.o: images.qrc
	rcc images.qrc -name images -o images.cpp
	g++ images.cpp  `pkg-config --libs --cflags QtGui` -c

Jack_plugin.o: Jack_plugin.c
	gcc $(CFLAGS) Jack_plugin.c -c

Sine_plugin.o: Sine_plugin.c
	gcc $(CFLAGS) Sine_plugin.c -c

