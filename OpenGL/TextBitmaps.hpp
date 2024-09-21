
#include <vlCore/Image.hpp>
#include <vlCore/IMutex.hpp>
#include <vlGraphics/ImagePBO.hpp>

#include <QtGlobal>
#include <QFont>
#include <QFontMetrics>
#include <QHash>
#include <QPainter>
#include <QGLWidget>
#include <QSet>
#include <QCoreApplication>
#include <QDir>

#include "../common/nsmtracker.h"
#include "../common/Mutex.hpp"
#include "../common/OS_error_proc.h"

#include "../api/api_proc.h"
#include "../Qt/Qt_colors_proc.h"


#if USE_FREETYPE
#include "FreeType.hpp"
#endif


namespace{

struct MyIMutex : public vl::IMutex{
  radium::Mutex _lock;

  bool is_obtained = false;

  void lock () override {
    _lock.lock();
    is_obtained = true;
  }
  
  void 	unlock () override {
    is_obtained = false;
    _lock.unlock();
  }
  
  int isLocked () const override {
    //Returns 1 if locked, 0 if non locked, -1 if unknown. 
    return is_obtained ? 1 : 0;
  }
  
};

struct ImageHolder {
  vl::ImagePBO *image;
  int width;
};

static QFont g_qfont;
static QHash<QChar,ImageHolder> *g_imageholders; // It's a pointer to avoid auto-desctruction at program exit.

static QFont g_qfont_halfsize;
static QHash<QChar,ImageHolder> *g_imageholders_halfsize; // It's a pointer to avoid auto-desctruction at program exit.

static QHash<QString, QHash<QChar,ImageHolder>* > g_imageholderss;

static QSet<QString> g_imageholder_fonts;
   

static MyIMutex image_mutex;

static inline void GE_add_imageholder(QFont qfont, QHash<QChar,ImageHolder> *imageholders, QString chars){
  QFontMetrics metrics(qfont);

#if QT_VERSION >= QT_VERSION_CHECK(5, 11, 0)
  int real_width = metrics.horizontalAdvance("#");
#else
  int real_width = metrics.width("#");
#endif
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
  char *font_path = strdup(OS_get_full_program_file_path("fonts/Cousine-Bold.ttf").toUtf8().constData());
  FreeType freeType(font_path, qfont.pointSize());
#endif
  
  for(QChar c : chars){ //for(int i=0;i<chars.length();i++){
    //QString c = chars.mid(i,1); //char c = chars[i];

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
    p.drawText(rect, Qt::AlignVCenter, c);

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

#if USE_FREETYPE
  free(font_path);
#endif
}
  
static const QString qfont_key(const QFont &font){ // From the qt documentation, it seems like it should work using a qfont as key, but I wasn't able to make it compile then.
  return font.toString()+"#"+font.styleName();
}

static inline QHash<QChar,ImageHolder> *add_new_font(const QFont &font){
  const QString chars="CDEFGABcdefgabhlo0123456789.,-MULR# m/|xPlsWtSvkupwinry()T:qH"; // Prerender characters we know could be used.
  //const char *chars="";

  const QString key = qfont_key(font);
  if (!g_imageholderss.contains(key)) {
    QHash<QChar,ImageHolder> *imageholders = new QHash<QChar,ImageHolder>;
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

static inline void GE_set_new_font(const QFont &nonscaled_font){
  QFont font(nonscaled_font);

  const double scale_ratio = safe_double_read(&g_opengl_scale_ratio);
	  
  if(!equal_doubles(scale_ratio, 1.0))
    font.setPointSize(font.pointSize() * scale_ratio);

  g_qfont = font;

  g_imageholders = add_new_font(font);
  R_ASSERT(g_imageholders != NULL);

  cache_font_family(font);

  set_halfsize_font();
}


struct TextBitmaps{
  QHash<QChar, std::vector<vl::dvec2> > points;

  QFont qfont;
  QHash<QChar,ImageHolder> *imageholders;

  TextBitmaps(bool halfsize)
    : qfont(halfsize ? g_qfont_halfsize : g_qfont)
    , imageholders(halfsize ? g_imageholders_halfsize : g_imageholders)
  {}

  // called from Main thread (not used)
  void clearCharBoxes(){
    QHash<QChar, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i)
      i.value().clear();
  }

  // called from Main thread  
  float addCharBox(QChar c, float x, float y){
    //fprintf(stderr,"adding %c\n",c);

    x = (int)x;
    y = (int)y;

    if(!imageholders->contains(c)) {
#if !defined(RELEASE)
      // Soundfile names are displayed now.
      //RWarning("TextBitmaps.hpp: '%c' was not precomputed\n",c);
      printf("\n\n ******** TextBitmaps.hpp: '%c' was not precomputed ******** \n\n\n",c.toLatin1());
#endif
      GE_add_imageholder(qfont, imageholders, QString(c));
    }

    ImageHolder holder = imageholders->value(c);

    if (c != ' '){
      vl::ImagePBO *image = holder.image;
      //float x2 = x + image->width()-1;
      //float y2 = y + image->height()-1;

      const double scale_ratio = safe_double_read(&g_opengl_scale_ratio);
	
      points[c].push_back(vl::dvec2(x + holder.width/scale_ratio,
                                    y - image->height()/scale_ratio/2.0
                                    )
                          );
    }

    // return x + holder.width;
    return x + holder.width/g_opengl_scale_ratio;
  }

  // called from Main thread
  void addCharBoxes(const char *text, float x, float y){
	  const int num_chars = strlen(text);
	  for(int i=0;i<num_chars;i++)
		  x = addCharBox(text[i], x, y);
  }

  // called from Main thread
  void addCharBoxes(QString text, float x, float y){
    for(QChar c : text)
      x = addCharBox(c, x, y);
  }
  

#if RADIUM_DRAW_FONTS_DIRECTLY

  // Called from OpenGL thread.
  // This one looks slightly better than the "ImageHolder" version below, and the code is a billion times simpler too (approx.), but it uses too much CPU. (needs more work though, can't just be enabled. By far, the biggest problem is to get a font file name from a QFont.)
  void drawAllCharBoxes(vl::VectorGraphics *vg, vl::Transform *transform, bool set_mask, PaintingData *painting_data, enum UseScissors use_scissors) const {
      
    QHash<QChar, std::vector<vl::dvec2> >::iterator i;
    for (i = points.begin(); i != points.end(); ++i) {
      char c = i.key();
      std::vector<vl::dvec2> pointspoints = i.value();
      //ImageHolder holder = (*imageholders)[c];

      for (std::vector< vl::dvec2 >::iterator it = pointspoints.begin(); it != pointspoints.end(); ++it) {
        vl::dvec2 points = *it;
        //printf("drawing %c at %f,%f\n",c,points.x(),points.y());
        //vl::ref<vl::Text> text = new vl::Text();
        //text->setClampY(false);

        Actor *actor = vg->drawText(points.x()-5, points.y()-5, vl::String(c));

        if (use_scissors==USE_SCISSORS)
          actor->setScissor(new vl::Scissor(100, 100, 500, 500));

        actor->computeBounds();
        if (set_mask)
          setActorEnableMask(actor,painting_data);
        
        actor->setTransform(transform);
        //actor->setScissor(new vl::Scissor(100, 100, 500, 500));
      }
    }
  }
  
#else
  
  // Called from OpenGL thread
  void drawAllCharBoxes(vl::VectorGraphics *vg, vl::Transform *transform, bool set_mask, PaintingData *painting_data, vl::Scissor *scissor) const {
    
    //QHash<char, std::vector<vl::dvec2> >::iterator i;
    for (auto i = points.begin(); i != points.end(); ++i) {
      QChar c = i.key();
      std::vector<vl::dvec2> pointspoints = i.value();
      ImageHolder holder = (*imageholders)[c];

      if(pointspoints.size()>0) {
        //printf("char: %c, size: %d\n",c,(int)points[c].size());

        //vg->setPoint(holder.image.get());
        vg->setPoint(holder.image);
        //vg->setColor(vl::fvec4(0.1,0.05,0.1,0.8));
        
        vl::Actor *actor = vg->drawPoints(pointspoints);

        if (scissor != NULL)
          actor->setScissor(scissor);

        actor->computeBounds();

        if (set_mask)
          setActorEnableMask(actor,painting_data);
        
        //actor->setScissor(new vl::Scissor(100, 100, 500, 500));
        actor->setTransform(transform);
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

