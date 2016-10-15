/* Copyright 2016 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#include "Qt_MyQCheckBox.h"


#include "Qt_seqtrack_widget.h"

class Seqblock_widget : public QWidget {
public:
  SeqBlock *_seqblock;

  Seqblock_widget(QWidget *parent, SeqBlock *seqblock)
    : QWidget(parent)
    , _seqblock(add_gc_root(seqblock))
  {
  }

  ~Seqblock_widget(){
    remove_gc_root(_seqblock);
  }
  
  void call_very_often(void){
  }
  
  void paintEvent ( QPaintEvent * ev ) override {
    QPainter p(this);

    printf("Painting seqblock %d\n", _seqblock->block->l.num);

    p.fillRect(1,1,width()-2,height()-1,QColor(qrand() % 255,qrand() % 255,qrand() % 255));
  }
};

class Seqblocks_widget : public QWidget {
public:

  QVector<Seqblock_widget*> seqblock_widgets;
  
  SeqTrack *_seqtrack;

  double _pixels_per_second = 10;
  
  Seqblocks_widget(QWidget *parent, SeqTrack *seqtrack)
    : QWidget(parent)
    , _seqtrack(add_gc_root(seqtrack))
  {
    create_seqblock_widgets();
  }

  ~Seqblocks_widget(){
    remove_gc_root(_seqtrack);
  }

  void position_seqblocks(void){
    for (auto *seqblock_widget : seqblock_widgets){
      auto *seqblock = seqblock_widget->_seqblock;

      float samplerate = MIXER_get_sample_rate();
        
      float x1 = scale(seqblock->time,                                        0, samplerate, 0, _pixels_per_second);
      float x2 = scale(seqblock->time + getBlockSTimeLength(seqblock->block), 0, samplerate, 0, _pixels_per_second);

      seqblock_widget->move(x1, 0);
      seqblock_widget->resize(x2-x1, height());
                            
    }
  }
  
  void create_seqblock_widgets(void){
    for (auto *seqblock_widget : seqblock_widgets){
      delete seqblock_widget;
    }

    seqblock_widgets.clear();    

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &_seqtrack->seqblocks){
      Seqblock_widget *seqblock_widget = new Seqblock_widget(this, seqblock);
      seqblock_widgets.push_back(seqblock_widget);
      seqblock_widget->show();
    }END_VECTOR_FOR_EACH;

    position_seqblocks();
  }
  
  void call_very_often(void){

    if (seqblock_widgets.size() != _seqtrack->seqblocks.num_elements) {
      create_seqblock_widgets();
      return;
    }

    int i = 0;
    for (auto *seqblock_widget : seqblock_widgets){
      auto *seqblock = seqblock_widget->_seqblock;
      if (seqblock != (struct SeqBlock*) _seqtrack->seqblocks.elements[i]) {
        create_seqblock_widgets();
        return;
      }
      i++;
    }

    position_seqblocks();
    
    for(auto *seqblock_widget : seqblock_widgets)
      seqblock_widget->call_very_often();
  }

  void paintEvent ( QPaintEvent * ev ) override {
    QPainter p(this);

    printf("PAINTING seqblocks. gakk\n");

    /*
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    }END_VECTOR_FOR_EACH;
    */
    
    p.fillRect(1,1,width()-2,height()-1,QColor(255, 0, 0));

    p.fillRect(10,10,width()-22,height()-21,QColor(0, 255, 0));
  }
};


class Seqtrack_widget : public QWidget, public Ui::Seqtrack_widget {
  Q_OBJECT

 public:

  Seqblocks_widget *_seqblocks_widget;  
  SeqTrack *_seqtrack;

  Seqtrack_widget(QWidget *parent, SeqTrack *seqtrack)
    : QWidget(parent)
    , _seqblocks_widget(new Seqblocks_widget(this, seqtrack))
    , _seqtrack(add_gc_root(seqtrack))
  {
    setupUi(this);

    main_layout->addWidget(_seqblocks_widget);
  }

  ~Seqtrack_widget(){
    remove_gc_root(_seqtrack);
  }
    
  void updateWidgets(void){
  }

  void call_very_often(void){
    _seqblocks_widget->call_very_often();
  }

public slots:

};


namespace{

class Sequencer_widget : public radium::VerticalScroll {
public:

  QVector<Seqtrack_widget*> seqtracks;
  
  Sequencer_widget(QWidget *parent)
    :radium::VerticalScroll(parent)
  {
    update_seqtracks();
  }

  void update_seqtracks(void){
    printf("  Updating seqtracks\n");
    
    for(auto *seqtrack : seqtracks)
      removeWidget(seqtrack);
      
    seqtracks.clear();
    
    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
      Seqtrack_widget *seqtrack_widget = new Seqtrack_widget(this, seqtrack);
      seqtracks.push_back(seqtrack_widget);
      addWidget(seqtrack_widget);
    }END_VECTOR_FOR_EACH;
  }

  void call_very_often(void){
    if (seqtracks.size() != root->song->seqtracks.num_elements) {
      update_seqtracks();
      return;
    }

    int i = 0;
    for(auto *seqtrack : seqtracks){
      if (root->song->seqtracks.elements[i] != seqtrack->_seqtrack){
        update_seqtracks();
        return;
      }
      i++;

      seqtrack->call_very_often();
    }
      
  }
  
};


}
