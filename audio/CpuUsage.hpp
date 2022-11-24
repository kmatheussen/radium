
#ifdef USE_QT4

#include <QString>
#include <QFont>
#include <QWidget>
#include <QApplication>
#define AUTOSUSPENDING_STRING " Auto-susp."


struct CpuUsage{
  DEFINE_ATOMIC(int, max_cpu_usage) = 0;
  DEFINE_ATOMIC(int, min_cpu_usage) = 10000000;
  DEFINE_ATOMIC(int, num_cpu_usage) = 0;
  DEFINE_ATOMIC(int, total_cpu_usage) = 0;

  int64_t _last_cpu_update_time = -1;
  QString _last_cpu_text;
  
  CpuUsage(){
  }

  void reset(){
    //ATOMIC_SET_RELAXED(max_cpu_usage, 0);
    //ATOMIC_SET_RELAXED(min_cpu_usage, 10000000);
    ATOMIC_SET_RELAXED(num_cpu_usage, 0);
    //ATOMIC_SET_RELAXED(total_cpu_usage, 0);
  }

  void addUsage(float usage){
    int i_new_cpu_usage = 1000.0 * usage;

    if (ATOMIC_GET_RELAXED(num_cpu_usage)==0) {
      
      ATOMIC_SET_RELAXED(total_cpu_usage, i_new_cpu_usage);
      ATOMIC_SET_RELAXED(max_cpu_usage, i_new_cpu_usage);
      ATOMIC_SET_RELAXED(min_cpu_usage, i_new_cpu_usage);
      
    } else {

      ATOMIC_ADD(total_cpu_usage, i_new_cpu_usage);
      
      if (i_new_cpu_usage > ATOMIC_GET_RELAXED(max_cpu_usage))
        ATOMIC_SET_RELAXED(max_cpu_usage, i_new_cpu_usage);
      
      if (i_new_cpu_usage < ATOMIC_GET_RELAXED(min_cpu_usage))
        ATOMIC_SET_RELAXED(min_cpu_usage, i_new_cpu_usage);
    }
    
    ATOMIC_ADD(num_cpu_usage, 1);
  }

  
private:
  
  float min(void){
    if (ATOMIC_GET_RELAXED(num_cpu_usage)==0)
      return 0;
    else
      return ATOMIC_GET_RELAXED(min_cpu_usage) / 1000.0;
  }

  float avg(void){
    int num = ATOMIC_GET_RELAXED(num_cpu_usage);
    if (num==0)
      return 0.0; // prevent theoretical division by zero.
    else
      return (ATOMIC_GET_RELAXED(total_cpu_usage) / num) / 1000.0;
  }
  
  float max(void){
    return ATOMIC_GET_RELAXED(max_cpu_usage) / 1000.0;
  }

  QString get_string(void){
    int mincpu = round(min());
    int maxcpu = round(max());
    int avgcpu = round(avg());

    reset();

    //QString ret;

    bool a = mincpu<10 && maxcpu<10 && avgcpu<10;

    char temp[100];
    snprintf(temp,98,"%s%s%d/%s%d/%s%d%s",
             a ? "  " : "",
             mincpu < 10 && !a? " " : "", mincpu,
             avgcpu < 10 && !a ? " " : "", avgcpu,
             maxcpu < 10 && !a ? " " : "", maxcpu,
             a ? " " : ""
             );

    return QString(temp);
  }

public:

  std::tuple<float,float,float> get_cpu_usage_since_last_time(void){
    std::tuple<float,float,float> ret = {min(), avg(), max()};
    reset();
    _last_cpu_update_time = TIME_get_ms();
    return ret;
  }
  
  bool should_update(int64_t time = TIME_get_ms()){
    
    if (_last_cpu_text=="" || time > (_last_cpu_update_time + 1000))
      return true;
    else
      return false;
  }
  
  bool maybe_update(QString &string){
    int64_t time = TIME_get_ms();
    
    if (should_update(time)){
      
      _last_cpu_text = get_string();
          
      _last_cpu_update_time = time;

      string =_last_cpu_text;

      return true;

    } else {

      string =_last_cpu_text;

      return false;
    }
  }
};

static inline void set_cpu_usage_font_and_width(QWidget *widget, bool shows_integers, bool might_autosuspend){
  QFont sansFont;
      
  sansFont.setFamily("Bitstream Vera Sans Mono");
  sansFont.setStyleName("Bold");
  sansFont.setPointSize(QApplication::font().pointSize()-1.0);
  
  widget->setFont(sansFont);

  /*
  QPalette pal = widget->palette();
  pal.setColor(QPalette::Background, QColor("green"));
  pal.setColor(QPalette::Base, QColor("green"));
  widget->setAutoFillBackground(true);
  widget->setPalette(pal);
  */
  
  QFontMetrics fm(sansFont); //QApplication::font());
  //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
  int width;

  if (shows_integers)
    width = fm.boundingRect("55/55/55").width();
  else
    width = fm.boundingRect("50.0 / 90.5 / 00.5").width();// + 5;

  if (might_autosuspend)
    width = R_MAX(fm.boundingRect(AUTOSUSPENDING_STRING).width(), width);
    
  widget->setMinimumWidth(width);
  widget->setMaximumWidth(width);
}
#endif

extern LANGSPEC void CpuUsage_delete(void *cpu_usage);

extern CpuUsage g_cpu_usage;
