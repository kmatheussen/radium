
#include <vlCore/Image.hpp>
#include <vlCore/IMutex.hpp>
#include <vlGraphics/ImagePBO.hpp>

#include <QFont>
#include <QFontMetrics>
#include <QHash>
#include <QPainter>
#include <QGLWidget>
#include <QMutex>
#include <QSet>
#include <QCoreApplication>
#include <QDir>

#include "../common/nsmtracker.h"
#include "../common/OS_error_proc.h"
#include "../Qt/Qt_colors_proc.h"


#if USE_FREETYPE
#include "FreeType.hpp"
#endif


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
  vl::ImagePBO *image;
  int width;
};

static QFont g_qfont;
static QHash<char,ImageHolder> *g_imageholders; // It's a pointer to avoid auto-desctruction at program exit.

static QFont g_qfont_halfsize;
static QHash<char,ImageHolder> *g_imageholders_halfsize; // It's a pointer to avoid auto-desctruction at program exit.

static QHash<QString, QHash<char,ImageHolder>* > g_imageholderss;

static QSet<QString> g_imageholder_fonts;
   

static MyMutex image_mutex;

static inline void GE_add_imageholder(QFont qfont, QHash<char,ImageHolder> *imageholders, const char *chars){
  QFontMetrics metrics(qfont);

  int real_width = metrics.width("#");
  int height = metrics.height();
  int width=height;
  //int width=real_width;
  
  QImage qt_image(width, height, QImage::Format_ARGB32);
  QPainter p(&qt_image);
  p.setPen(QColor(255, 255, 255, 255));

  qfont.setHintingPreference(QFont::PreferFullHinting); // full hinting should look better

  //QFontInfo info(qfont);
  //qfont.setPixelSize(info.pixelSize()); // probably a no-op. Try to avoid non-integer pixel size. Don't know if it's any point though.
                     
  p.setFont(qfont);

#if USE_FREETYPE
  FreeType freeType((QCoreApplication::applicationDirPath() + QDir::separator() + "fonts/Cousine-Bold.ttf").toUtf8().constData(), qfont.pointSize());
#endif
  
  for(int i=0;i<(int)strlen(chars);i++){
    char c = chars[i];

#if USE_FREETYPE
    QImage qtgl_image = freeType.draw(c,width,height);
#else
    //QColor qcol=get_qcolor(NULL, 15);
    //QColor qcol("#888888");
    QColor qcol("#fefefe");
    //QColor qcol("#c0c0c0c0");
    qcol.setAlpha(0);
    qt_image.fill(qcol);//QColor(0.0, 0.0, 0.0, 0));
    //qt_image.fill(QColor(0.0, 0.0, 0.0, 0));

  
    QRect rect(0,0,width,height);
    p.drawText(rect, Qt::AlignVCenter, QString(QChar(c)));

    QImage qtgl_image = qt_image;
#endif

    //QImage qtgl_image = QGLWidget::convertToGLFormat(qt_image);
    
    vl::ImagePBO *vl_image = new vl::ImagePBO;
    vl_image->setRefCountMutex(&image_mutex);
    vl_image->setAutomaticDelete(false);

    vl_image->allocate2D(width,height,4,vl::IF_RGBA, vl::IT_UNSIGNED_BYTE);
    
    unsigned char *qt_bits = qtgl_image.bits();
    unsigned char *vl_bits = vl_image->pixels();
    
    int s = width*height*4;
    memcpy(vl_bits,qt_bits,s);
    
    ImageHolder holder;
    holder.image = vl_image;
    holder.width = real_width;
    
    imageholders->insert(c, holder);
    //printf("Added '%c' (%d) to g_imageholders. image: %p\n",c,c,vl_image);
  }
}
  
static const QString qfont_key(const QFont &font){ // From the qt documentation, it seems like it should work using a qfont as key, but I wasn't able to make it compile then.
  return font.toString()+"#"+font.styleName().toUtf8().constData();
}

static inline QHash<char,ImageHolder> *add_new_font(const QFont &font){
  const char *chars="CDEFGABcdefgabhlo0123456789.,-MULR# m/";
  //const char *chars="";

  const QString key = qfont_key(font);
  if (!g_imageholderss.contains(key)) {
    QHash<char,ImageHolder> *imageholders = new QHash<char,ImageHolder>;
    g_imageholderss[key] = imageholders;
    GE_add_imageholder(font, imageholders, chars);
  }

  return g_imageholderss[key];
}

static void cache_font_family(const QFont &font){
  if (g_imageholder_fonts.contains(font.family()))
    return;
  
  for(int size=3 ; size < 20 ; size++) {
    QFont font2(font);
    font2.setPointSize(size);
    add_new_font(font2);
  }

  g_imageholder_fonts.insert(font.family());
}

static inline void set_halfsize_font(void){
  int full_size = g_qfont.pointSize();
  int half_size = full_size / 2;

  for(int size = half_size ; size>1 ; size++){
    QFont font2(g_qfont);
    font2.setPointSize(size);
    if (font2.pointSize() != full_size) {
      g_qfont_halfsize = font2;
      g_imageholders_halfsize = add_new_font(font2);
      return;
    }
  }
  
  g_qfont_halfsize = g_qfont;
  g_imageholders_halfsize = g_imageholders;
}
 
static inline void GE_set_new_font(const QFont &font){
  g_qfont = font;

  g_imageholders = add_new_font(font);
  R_ASSERT(g_imageholders != NULL);

  cache_font_family(font);

  set_halfsize_font();
}


struct TextBitmaps{
  QHash<char, std::vector<vl::dvec2> > points;

  QFont qfont;
  QHash<char,ImageHolder> *imageholders;

  TextBitmaps(bool halfsize)
    : qfont(halfsize ? g_qfont_halfsize : g_qfont)
    , imageholders(halfsize ? g_imageholders_halfsize : g_imageholders)
  {}

  // called from Main thread (not used)
  void clearCharBoxes(){
    QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i)
      i.value().clear();
  }

  // called from Main thread  
  float addCharBox(char c, float x, float y){
    //fprintf(stderr,"adding %c\n",c);

    x = (int)x;
    y = (int)y;

    if(!imageholders->contains(c)) {
      RWarning("TextBitmaps.hpp: '%c' was not precomputed\n",c);
      char chars[2];
      chars[0] = c;
      chars[1] = 0;
      GE_add_imageholder(qfont, imageholders, chars);
    }

    ImageHolder holder = imageholders->value(c);

    if (c != ' '){
      vl::ImagePBO *image = holder.image;
      //float x2 = x + image->width()-1;
      //float y2 = y + image->height()-1;
      
      points[c].push_back(vl::dvec2(x+holder.width,y-image->height()/2.0));//(y+y2)/2));
    }

    return x + holder.width;
  }

  // called from Main thread
  void addCharBoxes(const char *text, float x, float y){
    for(int i=0;i<(int)strlen(text);i++)
      x = addCharBox(text[i], x, y);
  }


#if RADIUM_DRAW_FONTS_DIRECTLY

  // Called from OpenGL thread.
  // This one looks slightly better than the "ImageHolder" version below, and the code is a billion times simpler too (approx.), but it uses too much CPU. (needs more work though, can't just be enabled. By far, the biggest problem is to get a font file name from a QFont.)
  void drawAllCharBoxes(vl::VectorGraphics *vg, vl::Transform *transform){
    QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i) {
      char c = i.key();
      std::vector<vl::dvec2> pointspoints = i.value();
      //ImageHolder holder = (*imageholders)[c];

      for (std::vector< vl::dvec2 >::iterator it = pointspoints.begin(); it != pointspoints.end(); ++it) {
        vl::dvec2 points = *it;
        //printf("drawing %c at %f,%f\n",c,points.x(),points.y());
        //vl::ref<vl::Text> text = new vl::Text();
        //text->setClampY(false);
        
        if(transform)
          vg->drawText(points.x()-5, points.y()-5, vl::String(c))->setTransform(transform);
        else
          vg->drawText(points.x()-5, points.y()-5, vl::String(c));
      }
    }
  }
  
#else
  
  // Called from OpenGL thread
  void drawAllCharBoxes(vl::VectorGraphics *vg, vl::Transform *transform){
    QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i) {
      char c = i.key();
      std::vector<vl::dvec2> pointspoints = i.value();
      ImageHolder holder = (*imageholders)[c];

      if(pointspoints.size()>0) {
        //printf("char: %c, size: %d\n",c,(int)points[c].size());

        //vg->setPoint(holder.image.get());
        vg->setPoint(holder.image);
        //vg->setColor(vl::fvec4(0.1,0.05,0.1,0.8));
        if(transform)
          vg->drawPoints(pointspoints)->setTransform(transform);
        else
          vg->drawPoints(pointspoints);
      }
    }

    vg->setImage(NULL);
  }
#endif

  //ImageHolder get_image_holder(char c){
  //  return image_holders[c];
  // }
};


} // namespace

