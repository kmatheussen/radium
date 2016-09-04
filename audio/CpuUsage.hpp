
#ifdef __cplusplus

struct CpuUsage{
  DEFINE_ATOMIC(int, max_cpu_usage);
  DEFINE_ATOMIC(int, min_cpu_usage);
  DEFINE_ATOMIC(int, num_cpu_usage);
  DEFINE_ATOMIC(int, total_cpu_usage);

  CpuUsage(){
    reset();
  }

  void reset(){
    ATOMIC_SET(max_cpu_usage, 0);
    ATOMIC_SET(min_cpu_usage, 10000000);
    ATOMIC_SET(num_cpu_usage, 0);
    ATOMIC_SET(total_cpu_usage, 0);
  }

  void addUsage(float usage){
    int i_new_cpu_usage = 1000.0 * usage;

    if (ATOMIC_GET(num_cpu_usage)==0) {
      
      ATOMIC_SET(total_cpu_usage, i_new_cpu_usage);
      ATOMIC_SET(max_cpu_usage, i_new_cpu_usage);
      ATOMIC_SET(min_cpu_usage, i_new_cpu_usage);
      
    } else {
      ATOMIC_ADD(total_cpu_usage, i_new_cpu_usage);
      
      if (i_new_cpu_usage > ATOMIC_GET(max_cpu_usage))
        ATOMIC_SET(max_cpu_usage, i_new_cpu_usage);
      
      if (i_new_cpu_usage < ATOMIC_GET(min_cpu_usage))
        ATOMIC_SET(min_cpu_usage, i_new_cpu_usage);
    }
    
    ATOMIC_ADD(num_cpu_usage, 1);
  }
  
  float min(void){
    if (ATOMIC_GET(num_cpu_usage)==0)
      return 0.0;
    else
      return ATOMIC_GET(min_cpu_usage) / 1000.0;
  }

  float avg(void){
    if (ATOMIC_GET(num_cpu_usage)==0)
      return 0.0;
    else
      return (ATOMIC_GET(total_cpu_usage) / ATOMIC_GET(num_cpu_usage)) / 1000.0;
  }
  
  float max(void){
    return ATOMIC_GET(max_cpu_usage) / 1000.0;
  }
};

#if USE_QT4

#include <QFont>
#include <QWidget>
#include <QApplication>
#define AUTOSUSPENDING_STRING " Auto-suspended"

static inline void set_cpu_usage_font_and_width(QWidget *widget, bool shows_integers, bool might_autosuspend){
  QFont sansFont;
      
  sansFont.setFamily("Bitstream Vera Sans Mono");
  sansFont.setStyleName("Bold");
  sansFont.setPointSize(QApplication::font().pointSize()-1.0);
  
  widget->setFont(sansFont);
  
  QFontMetrics fm(sansFont); //QApplication::font());
  //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
  int width;

  if (shows_integers)
    width = fm.width("55 / 55 / 55");
  else
    width = fm.width("50.0 / 90.5 / 00.5");// + 5;

  if (might_autosuspend)
    width = R_MAX(fm.width(AUTOSUSPENDING_STRING), width);
    
  widget->setMinimumWidth(width);
  widget->setMaximumWidth(width);
}
#endif

#endif

extern LANGSPEC void CpuUsage_delete(void *cpu_usage);

