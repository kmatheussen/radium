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

#include "../common/nsmtracker.h"
#include "../common/OS_error_proc.h"


namespace{

struct MyMutex : public vl::IMutex{
  QMutex mutex;
  int num_visitors;
  
  MyMutex() 
    : mutex(QMutex::Recursive)
    , num_visitors(0) 
  {}
  
  virtual void 	lock () {
    mutex.lock();
    num_visitors++;
  }
  
  virtual void 	unlock () {
    num_visitors--;
    mutex.unlock();
  }
  
  virtual int isLocked () const {
    //Returns 1 if locked, 0 if non locked, -1 if unknown. 
    return num_visitors>0;
  }
  
};

struct ImageHolder {
  vl::ref<vl::ImagePBO> image;
  int width;
};


static QFont g_qfont;
static QHash<char,ImageHolder> *g_imageholders; // It's a pointer to avoid auto-desctruction at program exit.

static MyMutex image_mutex;

static inline void GE_add_imageholder(const char *chars){
  QFontMetrics metrics(g_qfont);

  int real_width = metrics.width("#");
  int height = metrics.height();
  int width=height;
  //int width=real_width;
  
  QImage qt_image(width, height, QImage::Format_ARGB32);
  QPainter p(&qt_image);
  p.setPen(QColor(255, 255, 255, 255));

  p.setFont(g_qfont);

  for(int i=0;i<(int)strlen(chars);i++){
    char c = chars[i];
    
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
    
    g_imageholders->insert(c, holder);
    //printf("Added '%c' (%d) to g_imageholders. image: %p\n",c,c,vl_image);
  }
}
  
static inline void GE_set_new_font(const QFont &font){
  g_qfont = font;
  
  const char *chars="CDEFGABcdefgabhlo0123456789.,-MULR# m";
  //const char *chars="";

  if (g_imageholders==NULL)
    g_imageholders = new QHash<char,ImageHolder>;

  GE_add_imageholder(chars);
}


struct TextBitmaps{
  QHash<char, std::vector<vl::dvec2> > points;

  void clearCharBoxes(){
    QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i)
      i.value().clear();
  }
  
  float addCharBox(char c, float x, float y){
    //fprintf(stderr,"adding %c\n",c);

    x = (int)x;
    y = (int)y;

    if(!g_imageholders->contains(c)) {
      RWarning("TextBitmaps.hpp: '%c' was not precomputed\n",c);
      char chars[2];
      chars[0] = c;
      chars[1] = 0;
      GE_add_imageholder(chars);
    }

    ImageHolder holder = g_imageholders->value(c);

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
    QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i) {
      char c = i.key();
      std::vector<vl::dvec2> pointspoints = i.value();
      ImageHolder holder = (*g_imageholders)[c];

      if(pointspoints.size()>0) {
        //printf("char: %c, size: %d\n",c,(int)points[c].size());

        //vg->setPoint(holder.image.get());
        vg->setPoint(holder.image.get());
        //vg->setColor(vl::fvec4(0.1,0.05,0.1,0.8));
        if(transform)
          vg->drawPoints(pointspoints)->setTransform(transform);
        else
          vg->drawPoints(pointspoints);
      }
    }

    vg->setImage(NULL);
  }

  //ImageHolder get_image_holder(char c){
  //  return image_holders[c];
  // }
};


} // namespace
