#include <assert.h>

#include <vlCore/Image.hpp>
#include <vlCore/IMutex.hpp>
#include <vlGraphics/ImagePBO.hpp>

#include <QFont>
#include <QFontMetrics>
#include <QHash>
#include <QPainter>
#include <QGLWidget>
#include <QMutex>


static const char *chars="CDEFGABcdefgabhlo0123456789.,-MULR# m";

namespace{

  struct MyMutex : public vl::IMutex{
    QMutex mutex;
    int is_locked;

    MyMutex() : is_locked(0) {}

    virtual void 	lock () {
      mutex.lock();
      is_locked = 1;
    }

    virtual void 	unlock () {
      is_locked = 0;
      mutex.unlock();
    }

    virtual int isLocked () const {
      //Returns 1 if locked, 0 if non locked, -1 if unknown. 
      return is_locked;
    }

  };

  struct ImageHolder {
    vl::ref<vl::ImagePBO> image;
    int width;
  };


static QHash<char,ImageHolder> g_imageholders;

static MyMutex image_mutex;


static inline void GE_set_new_font(const QFont &font){
  QFontMetrics metrics(font);

  int real_width = metrics.width("#");
  int height = metrics.height();
  int width=height;
  //int width=real_width;
  
  QImage qt_image(width, height, QImage::Format_ARGB32);
  QPainter p(&qt_image);
  p.setPen(QColor(255, 255, 255, 255));

  p.setFont(font);
  
  for(int i=0;i<(int)strlen(chars)-1;i++){
    char c = chars[i];
    //p.drawText(20,12,"hello");//QString(c));
    
    qt_image.fill(QColor(0.0, 0.0, 0.0, 0));
    
    QRect rect(0,0,width,height);
    p.drawText(rect, Qt::AlignVCenter, QString(QChar(c)));
    
    QImage qtgl_image = qt_image;
    //QImage qtgl_image = QGLWidget::convertToGLFormat(qt_image);
    
    //vl::ImagePBO *vl_image = new vl::ImagePBO;
    vl::ImagePBO *vl_image = new vl::ImagePBO;
    vl_image->setRefCountMutex(&image_mutex);

    vl_image->allocate2D(width,height,4,vl::IF_RGBA, vl::IT_UNSIGNED_BYTE);
    
    unsigned char *qt_bits = qtgl_image.bits();
    unsigned char *vl_bits = vl_image->pixels();
    
    int s = width*height*4;
    memcpy(vl_bits,qt_bits,s);
    
    ImageHolder holder;
    holder.image = vl_image;
    holder.width = real_width;
    
    g_imageholders[c] = holder;
  }
}


struct TextBitmaps{
  QHash<char, std::vector<vl::dvec2> > points;

  void clearCharBoxes(){
    for(int i=0;i<(int)strlen(chars)-1;i++){
      char c = chars[i];
      points[c].clear();
    }
  }

  float addCharBox(char c, float x, float y){
    //fprintf(stderr,"adding %c\n",c);

    x = (int)x;
    y = (int)y;

    assert(g_imageholders.contains(c));

    ImageHolder holder = g_imageholders[c];

    if (c != ' '){
      vl::ImagePBO *image = holder.image.get();
      //float x2 = x + image->width()-1;
      //float y2 = y + image->height()-1;
      
      points[c].push_back(vl::dvec2(x+holder.width,y-image->height()/2.0));//(y+y2)/2));
    }

    return x + holder.width;
  }

  void addCharBoxes(const char *text, float x, float y){
    for(int i=0;i<(int)strlen(text);i++)
      x = addCharBox(text[i], x, y);
  }

  void drawAllCharBoxes(vl::VectorGraphics *vg, vl::Transform *transform){
    for(int i=0;i<(int)strlen(chars)-1;i++){
      char c = chars[i];
      ImageHolder holder = g_imageholders[c];

      if(points[c].size()>0) {
        //printf("char: %c, size: %d\n",c,(int)points[c].size());

        //vg->setPoint(holder.image.get());
        vg->setPoint(holder.image.get());
        //vg->setColor(vl::fvec4(0.1,0.05,0.1,0.8));
        if(transform)
          vg->drawPoints(points[c])->setTransform(transform);
        else
          vg->drawPoints(points[c]);
      }
    }

    vg->setImage(NULL);
  }

  //ImageHolder get_image_holder(char c){
  //  return image_holders[c];
  // }
};


} // namespace
