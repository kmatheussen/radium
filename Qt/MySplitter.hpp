
#ifndef RADIUM_QT_MYSPLITTER_HPP
#define RADIUM_QT_MYSPLITTER_HPP

#include <QSplitter>

namespace radium {

#if 1
  
class Splitter : public QSplitter {
  //  Q_OBJECT;
  
public:
  Splitter(Qt::Orientation orientation, QWidget *parent = Q_NULLPTR)
    : QSplitter(orientation, parent)
  {
  }
};
  
#else
  
class Splitter : public QWidget {
  QVector<QWidget*> _children;
  
public:
  Splitter(Qt::Orientation orientation, QWidget *parent = Q_NULLPTR)
    : QWidget(parent)
  {
  }

  void setChildrenCollapsible(bool){
  }

  int count() const{
    return _children.size();
  }
};
  
#endif
}

#endif
