
#include <QMap>
#include <QThread>
#include <QPair>

#if QT_VERSION >= 0x050600
#include <QTemporaryDir>
#else
namespace{

  static bool try_temp_path(QString &path, QString &error_string){
    QDir base(QDir::root());
    
    if (base.mkdir(path)){
      
      base.cd(path);
      
      if (QDir::toNativeSeparators(path) != QDir::toNativeSeparators(base.absolutePath())){
        error_string = "Created a temporary directory, but the path does not have expected value. Expected: " + QDir::toNativeSeparators(path) + ". Actual: " + QDir::toNativeSeparators(base.absolutePath());

        base.cdUp();
        base.rmdir(path);
        
        return false;
      }
      
      return true;
      
    } else {

      error_string = "Unable to create temporary directory " + path;
      return false;
      
    }
  }
  
  static bool get_new_temp_path(const QString &templatePath, QString &path, QString &error_string){

    if (templatePath.endsWith("XXXXXX")){

      QString error_string2;
      
      for(int i = 100000 ; i < 110000 ; i++){
        path = templatePath.left(templatePath.length()-6) + QString::number(i);
        if (try_temp_path(path, error_string2))
          return true;
      }
      
      error_string = error_string2;
      return false;
      
    } else {

      path = templatePath;
      return try_temp_path(path, error_string);
    }
    
  }

  // http://stackoverflow.com/questions/11050977/removing-a-non-empty-folder-in-qt
  static bool removeDir(const QString & dirName)
  {
    bool result = true;
    QDir dir(dirName);
    
    if (dir.exists()) {
      Q_FOREACH(QFileInfo info, dir.entryInfoList(QDir::NoDotAndDotDot | QDir::System | QDir::Hidden  | QDir::AllDirs | QDir::Files, QDir::DirsFirst)) {
        if (info.isDir()) {
          result = removeDir(info.absoluteFilePath());
        }
        else {
          result = QFile::remove(info.absoluteFilePath());
        }
        
        if (!result) {
          return result;
        }
      }
      result = dir.rmdir(dirName);
    }
    return result;
  }
 
  // In qt5, but not in qt4  
  struct QTemporaryDir{

    QString _error_string;
    QString _path;
    bool _is_valid;
    
    QTemporaryDir(const QString &templatePath){
      _path = templatePath;
      _is_valid = get_new_temp_path(templatePath, _path, _error_string);
    }
    
    ~QTemporaryDir(){
      remove();
    }

    QString errorString() const {
      return _error_string;
    }
                                               
    bool isValid() const {
      return _is_valid;
    }
  
    QString path() const {
      return _path;
    }
  
    bool remove() {
      if (_is_valid){
        R_ASSERT_RETURN_IF_FALSE2(_path.startsWith(QDir::tempPath()), false);
        return removeDir(_path);
      } else
        return false;
    }
    
  };
}
#endif


#include "../common/Mutex.hpp"
#include "../common/Queue.hpp"

// radium::Mutex fff_mutex; // Must be obtained when using the factory. (Not necessary, faust has its own lock)

namespace{
  struct FFF_Reply {
    
    Data *data;
    QTemporaryDir *svg_dir;
    QString error_message;
    bool is_instrument;
    llvm_dsp_factory *factory;
    
    bool is_empty(void){
      if (data==NULL && error_message=="") {
        if (svg_dir!=NULL)
          RError("svg_dir!=NUL: %p\n",svg_dir);
        return true;
      } else
        return false;
    }


    FFF_Reply()
      : data(NULL)
      , svg_dir(NULL)
      , factory(NULL)
    {}
  };

  struct Request{
    int64_t id;
    QString code;
    QString options;
    int optlevel;
    FFF_Reply reply;
    bool please_stop;
    
    Request(int64_t id)
      : id(id)
      , please_stop(false)
    {}
  };

  class ArgsCreator{
    QStringList args;

    int argc;
    const char **argv;

    bool is_dirty;
    
    void free_argv(){
      for(int i=0;i<argc;i++)
        free((void*)argv[i]);
      free((void*)argv);      
    }
    
    void create(){
      free_argv();
      argc = args.size();
      argv = (const char**)calloc(sizeof(char*), argc);
      for(int i=0;i<argc;i++)
        argv[i] = strdup(args[i].toUtf8().constData());
      is_dirty = false;
    }
    
  public:
    void push_back(QString arg){
      args.push_back(arg.replace("%radium_path%",QCoreApplication::applicationDirPath()));
      is_dirty = true;
    }
    void push_back(QStringList args2){
      for(auto arg : args2)
        push_back(arg);
    }
    
    int get_argc(void){
      return args.size();
    }

    const char** get_argv(void){
      if (is_dirty)
        create();
      return argv;
    }
    
    ArgsCreator()
      : argc(0)
      , argv(NULL)
      , is_dirty(true)
    {}
    
    ~ArgsCreator(){
      free_argv();
    }
  };
}

static radium::Queue<Request*,1024> g_queue;

static radium::Mutex g_reply_mutex;
static QMap<int64_t, FFF_Reply> g_ready; // Access must be protected by the g_reply_mutex

namespace{
  class FFF_Thread : public QThread {
    Q_OBJECT;

  public:

    void startit(void){
      start();
    }

  private:
    
    bool create_svg(QString code, QString options, FFF_Reply &reply){
      QTemporaryDir *svg_dir = new QTemporaryDir(QDir::tempPath() + QDir::separator() + "radium_faust_svg_XXXXXX");

      if (svg_dir->isValid()==false) {
        reply.error_message = svg_dir->errorString();
        if (reply.error_message==""){
          R_ASSERT(false);
          reply.error_message="Unable to create temporary directory";
        }
        delete svg_dir;
        return false;
      }

      ArgsCreator args;
      args.push_back("-svg");
      args.push_back("-O");
      args.push_back(svg_dir->path());
      args.push_back("-o");
      args.push_back("cppsource.cpp");
      args.push_back(options.split("\n", QString::SkipEmptyParts));
      
      std::string error_message;
      printf("\n\n   Starting to create aux\n");
      double time = TIME_get_ms();
      if (generateAuxFilesFromString(
                                     "FaustDev",
                                     code.toUtf8().constData(),
                                     args.get_argc(),
                                     args.get_argv(),
                                     error_message
                                     )
          ==false
          )
        {          
          reply.error_message = QString("Unable to create svg: %1").arg(error_message.c_str());
          delete svg_dir;
          return false;
        }

      printf("   Aux created %f\n\n\n\n", (TIME_get_ms() - time) / 1000.0);
      
      //fprintf(stderr,"   SVG tempdir: -%s-. Error: %s\n", args.argv[2], error_message.c_str());

      reply.svg_dir = svg_dir;
      
      return true;
    }

    bool create_reply_data(FFF_Reply &reply){
      llvm_dsp *dsp_ = createDSPInstance(reply.factory);
      
      if (dsp_ == NULL){
        reply.error_message = "createDSPInstance returned NULL 5";
        return false;
      }

      dsp_->init(MIXER_get_sample_rate());
      
      int num_inputs = dsp_->getNumInputs();
      int num_outputs = dsp_->getNumInputs();
      
      if (num_inputs > MAX_CHANNELS){
        reply.error_message = QString("Maximum %1 input channels supported (%2)").arg(QString::number(MAX_CHANNELS), QString::number(num_inputs));
        delete dsp_;
        return false;
      }
      
      if (num_outputs > MAX_CHANNELS){
        reply.error_message = QString("Maximum %1 output channels supported (%2)").arg(QString::number(MAX_CHANNELS), QString::number(num_outputs));
        delete dsp_;
        return false;
      }
      
      reply.data = create_effect_plugin_data2(MIXER_get_sample_rate(), dsp_);
      
      reply.is_instrument = reply.data->voices[0].myUI.is_instrument();
      
      if (reply.is_instrument){
        dsp *dsps[MAX_POLYPHONY];
        dsps[0] = dsp_;
        
        {
          //radium::ScopedMutex lock(&fff_mutex);
          for(int i=1;i<MAX_POLYPHONY;i++)
            dsps[i] = createDSPInstance(reply.factory);
        }
        
        for(int i=1;i<MAX_POLYPHONY;i++)
          dsps[i]->instanceInit(MIXER_get_sample_rate());
        
        convert_effect_data_to_instrument_data(reply.data, dsps);
      }

      return true;
    }

    // Check atan2! Opt seems to not work.
    
  public:

    bool create_reply(QString code, QString options, int optlevel, FFF_Reply &reply){

      ArgsCreator args;
      args.push_back(options.split("\n", QString::SkipEmptyParts));
#if 0 // __WIN32 && !_WIN64
      args.push_back("-l");
      args.push_back(OS_get_full_program_file_path("llvm_math.ll"));
#endif
      
      std::string error_message;

      //R_ASSERT(OPT_LEVEL==0);
      
      printf("\n\n   Starting to create factory %s.  \n\n Optlevel: %d\n\n",code.toUtf8().constData(),optlevel);
      double time = TIME_get_ms();
      reply.factory = createDSPFactoryFromString(
                                                 //"/home/kjetil/radium/bin/packages/faust2/examples/graphic_eq.dsp",
                                                 "FaustDev",
                                                 code.toUtf8().constData(),
                                                 args.get_argc(),
                                                 args.get_argv(),
                                                 "",
                                                 error_message,
                                                 optlevel);
      printf("   Factory created %f\n\n\n\n", (TIME_get_ms() - time) / 1000.0);
            
      if (reply.factory==NULL) {
        reply.error_message = error_message.c_str();
        printf("    Error,das: -%s-\n", reply.error_message.toUtf8().constData());
        return false;
      }

      bool got_data = create_reply_data(reply);

      if (got_data==false)
        return false;

      if (create_svg(code, options, reply)==false) {
        delete_dsps_and_data2(reply.data);
        reply.data = NULL;
        return false;
      }
      
      return true;
    }

    void free_reply_data(FFF_Reply &reply){
      delete reply.svg_dir;
      delete_dsps_and_data2(reply.data);
      deleteDSPFactory(reply.factory);
    }
    
  private:
    
    void run(){
      
      while(true){

        QMap<int64_t, QPair<QString, QPair<QString, int> > > code_requests;

        // 1. First handle delete requests and collect compilation requests. Here we only keep the newest compilation requests for each plugin.
        do{
          Request *request = g_queue.get();
          if (request->please_stop==true)   // Program exit.
            return;
         
          if (request->reply.data != NULL) {
            free_reply_data(request->reply);
          } else
            code_requests[request->id] = qMakePair(request->code, qMakePair(request->options, request->optlevel));
          
          delete request;
        }while(g_queue.size() > 0);

        
        // 2. Then do the compilation
        
        QMapIterator<int64_t, QPair<QString, QPair<QString, int> > > iterator(code_requests);
        
        while (iterator.hasNext()) {
          
          iterator.next();

          int64_t id = iterator.key();
          QString code    = iterator.value().first;
          QString options = iterator.value().second.first;
          int optlevel    = iterator.value().second.second;

          FFF_Reply reply;
          create_reply(code, options, optlevel, reply);
          
          {
            radium::ScopedMutex lock(&g_reply_mutex);
            g_ready[id] = reply;
          }
        }

      }
    }
  };
  #include "mFaust_factory_factory.cpp"
}

static FFF_Thread g_fff_thread;

static void init_fff(void){
  static bool has_started = false;

  if (has_started==false){
    startMTDSPFactories(); // Make faust llvm is thread safe
    g_fff_thread.startit();
    has_started = true;
  }
}

void FFF_shut_down(void){
  if (g_fff_thread.isRunning()){
    
    Request *request = new Request(0);
    request->please_stop = true;
    
    g_queue.put(request);
    
    g_fff_thread.wait();

  }
}

FFF_Reply FFF_get_reply_now(QString code, QString options){
  init_fff();

  FFF_Reply reply;
  g_fff_thread.create_reply(code, options, getFaustOptimizationLevel(), reply);
  
  return reply;
}

/*
llvm_dsp *FFF_get_dsp(const FFF_Reply &reply){
  //radium::ScopedMutex lock(&fff_mutex);
  return createDSPInstance(reply.factory);
}
*/

void FFF_request_reply(int64_t id, QString code, QString options){
  init_fff();
  
  Request *request = new Request(id);
  request->code = code;
  request->options = options;
  request->optlevel = getFaustOptimizationLevel(); // getFaustOptimizationLevel is not thread safe, and it's simpler to just send it over rather than making getFaustOpimizationLevel thread safe.
    
  g_queue.put(request);
}

FFF_Reply fff_empty_reply;

// Lightweight function made to be called very often from a timer.
// .reply->data==NULL and .reply->error_message=="" if there was nothing to get.
FFF_Reply FFF_get_reply(int64_t id){
  radium::ScopedMutex lock(&g_reply_mutex);
  
  if (g_ready.contains(id)==false)
    return fff_empty_reply;

  FFF_Reply reply = g_ready[id];
  
  g_ready.remove(id);

  return reply;
}

void FFF_request_free(int64_t id, const FFF_Reply &reply){
  if (reply.data==NULL)
    return;

  delete_dsps_and_data1(reply.data);

  Request *request = new Request(id);
  request->reply = reply;
  
  g_queue.put(request);
}

// We don't call delete_dsps_and_data1 here since this function is called when cleaning up, and then the widgets have already been deleted.
void FFF_free_now(FFF_Reply &reply){
  if (reply.data==NULL)
    return;

  g_fff_thread.free_reply_data(reply);
}

