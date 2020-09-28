#include <QMap>
#include <QThread>
#include <QtConcurrent>
#include <QPair>



#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"

#define U_MUTEX false
static radium::Mutex g_faust_mutex; // Calling startMTDSPFactories() is not enough since that one will only protect llvm factories.
//radium::Mutex fff_mutex; // Must be obtained when using the factory. (Not necessary, faust has its own lock)

#define COMPILE_SVG_IN_PARALLEL 0 // Setting this one to 1 is currently a lot slower. The reason is that Faust uses a common lock around all API functions (so it's not running in parallell anyway), plus that generating C++ code takes a lot of time.


/*
To run lambda on main thread:
        QMetaObject::invokeMethod(qApp, [line, this]
                                {
                                printf("hello main thread\n");
                                });
*/


namespace{
  struct CompileOptions{
    QString code;
    QString options;
    int optlevel;
    bool use_interpreter;

    CompileOptions(QString code,
                   QString options,
                   int optlevel,
                   bool use_interpreter)
      : code(code)
      , options(options)
      , optlevel(optlevel)
#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
      , use_interpreter(true)
#else
      , use_interpreter(use_interpreter)
#endif
    {}

    CompileOptions(const Devdata *devdata)
      : CompileOptions(devdata->code, devdata->options, getFaustOptimizationLevel(), devdata->use_interpreter_backend)
    {}
    /*
    CompileOptions(){
    }
    */
  };

  struct Request{
    instrument_t patch_id;
    CompileOptions opts;

    bool please_stop;
    
    Request(instrument_t patch_id, const CompileOptions &opts)
      : patch_id(patch_id)
      , opts(opts)
      , please_stop(false)
    {}
    /*
    Request(){}
    */
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

/*
namespace{

class SetDefaultValuesUI : public UI
{

 public:

  SetDefaultValuesUI(dsp *dsp)
  {
    dsp->buildUserInterface(this);
    for(float *control_port : control_ports){
      *control_port = 0.1;
    }
    //printf("   SetDefault %s: Before: %f. Now: %f\n", name, bef, *control_port);
  }

  ~SetDefaultValuesUI() {	}
  
  // -- widget's layouts
  
  //void openFrameBox(const char* label) override {_curr_box_name = label;}
  void openTabBox(const char* label) override {}
  void openHorizontalBox(const char* label) override {}
  void openVerticalBox(const char* label) override {}
  void closeBox() override {}
  
  // -- active widgets

  std::vector<float*> control_ports;

private:

  void addEffect(const char *name, float* control_port, int type, float min_value, float default_value, float max_value) {
    control_ports.push_back(control_port);
  }

protected:

  void addButton(const char* label, float* zone) override {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addCheckButton(const char* label, float* zone) override {
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step) override {
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) override {
    addEffect(label, zone,  step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) override {
    addEffect(label, zone, step==1.0f ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max); // The INT effect format might not work. Need to go through the code first.
  }
  
  // -- passive widgets

  void addHorizontalBargraph(const char* label, float* zone, float min, float max) override {
  }
  void addVerticalBargraph(const char* label, float* zone, float min, float max) override {
  }
  
  // -- soundfiles
  
#if 1 //HEPP
  void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) override {}
#endif
};
}
*/


static void FAUST_handle_new_svg_dir(struct SoundPlugin *plugin, MyQTemporaryDir *svg_dir, QString error_message);
static bool FAUST_handle_fff_reply(struct SoundPlugin *plugin, const FFF_Reply &reply, bool is_initializing);


static radium::Queue<Request*,1024> g_queue;

static void add_factory_ready(Devdata *devdata, bool success){
  R_ASSERT(THREADING_is_main_thread());

  radium::FAUST_calledRegularlyByParentReply &ready = devdata->ready;
  ready.has_new_data = true;

  ready.factory_is_ready = true;
  ready.factory_succeeded = success;
}

static void add_svg_ready(Devdata *devdata, bool success){
  R_ASSERT(THREADING_is_main_thread());

  radium::FAUST_calledRegularlyByParentReply &ready = devdata->ready;
  ready.has_new_data = true;

  ready.svg_is_ready = true;
  ready.svg_succeeded = success;
}

namespace{

    struct v_calloc_memory_manager : public dsp_memory_manager {
    
      void* allocate(size_t size) override
      {
#if defined(RELEASE)
        void* res = V_calloc(1, size);
#else
        void* res = V_malloc(size); // makes it possible (probably) for asan to check reading unassigned memory.
#endif
        return res;
      }
      
      void destroy(void* ptr) override
      {
        //cout << "free_manager" << endl;
        V_free(ptr);
      }    
    };

    static v_calloc_memory_manager g_v_calloc_memory_manager;

  class FFF_Thread : public QThread {
    Q_OBJECT;

  public:

    void startit(void){
      start();
    }

  private:

    MyQTemporaryDir *create_svg_dir(ArgsCreator &args, QString &error_message) const {
      MyQTemporaryDir *svg_dir = new MyQTemporaryDir(QDir::tempPath() + QDir::separator() + "radium_faust_svg_XXXXXX");

      if (svg_dir->isValid()==false) {
        error_message = "Unable to create temporary directory \"" + svg_dir->_path + "\": " + svg_dir->errorString();
        delete svg_dir;
        return NULL;
      }

      args.push_back("-svg");
      args.push_back("-O");
      args.push_back(svg_dir->path());

      return svg_dir;
    }

#if COMPILE_SVG_IN_PARALLEL
    MyQTemporaryDir *create_svg(const CompileOptions &opts, QString &error_message) const {

      ArgsCreator args;
      args.push_back("-o");
      args.push_back("cppsource.cpp");
      args.push_back(options.split("\n", QString::SkipEmptyParts));

      MyQTemporaryDir *svg_dir = create_svg_dir(args, error_message);
      if (svg_dir==NULL)
        return NULL;

      std::string error_message2;
      printf("\n\n   Starting to create aux\n");
      double time = TIME_get_ms();
      if (generateAuxFilesFromString(
                                     "FaustDev",
                                     opts.code.toUtf8().constData(),
                                     args.get_argc(),
                                     args.get_argv(),
                                     error_message2
                                     )
          ==false
          )
        {          
          error_message = QString("Unable to create svg: %1").arg(error_message2.c_str());
          delete svg_dir;
          return NULL;
        }

      printf("   Aux created %f\n\n\n\n", (TIME_get_ms() - time) / 1000.0);
      
      //fprintf(stderr,"   SVG tempdir: -%s-. Error: %s\n", args.argv[2], error_message.c_str());

      return svg_dir;
    }
#endif

    QString create_reply_data(FFF_Reply &reply) const {
      R_ASSERT(U_MUTEX==0 || g_faust_mutex.is_locked());

      dsp *dsp_ = reply.factory->createDSPInstance();
      
      if (dsp_ == NULL){
        return "createDSPInstance returned NULL 5";
      }

      dsp_->init(MIXER_get_sample_rate());
      
      int num_inputs = dsp_->getNumInputs();
      int num_outputs = dsp_->getNumInputs();
      
      if (num_inputs > MAX_CHANNELS){
        QString ret = QString("Maximum %1 input channels supported (%2)").arg(QString::number(MAX_CHANNELS), QString::number(num_inputs));
        delete dsp_;
        return ret;
      }
      
      if (num_outputs > MAX_CHANNELS){
        QString ret = QString("Maximum %1 output channels supported (%2)").arg(QString::number(MAX_CHANNELS), QString::number(num_outputs));
        delete dsp_;
        return ret;
      }

      {
        
      }

      //SetDefaultValuesUI doit(dsp_);

      reply.data = create_effect_plugin_data2(MIXER_get_sample_rate(), dsp_);
      //set_effect_value2(reply.data, 0, 0.75, EFFECT_FORMAT_NATIVE, FX_single);

      reply.is_instrument = reply.data->voices[0].myUI.is_instrument();
      
      if (reply.is_instrument){
        dsp *dsps[MAX_POLYPHONY];
        dsps[0] = dsp_;
        
        {
          //radium::ScopedMutex lock(fff_mutex);
          for(int i=1;i<MAX_POLYPHONY;i++)
            dsps[i] = reply.factory->createDSPInstance(); //reply.factory);
        }
        
        for(int i=1;i<MAX_POLYPHONY;i++)          
          dsps[i]->instanceInit(MIXER_get_sample_rate());
        
        convert_effect_data_to_instrument_data(reply.data, dsps);
      }

      return "";
    }

    // Check atan2! Opt seems to not work.

    QString create_reply_factory(const CompileOptions &opts, FFF_Reply &reply, MyQTemporaryDir* &svg_dir) const {
      ArgsCreator args;
      args.push_back(opts.options.split("\n", QString::SkipEmptyParts));
#if 0 // __WIN32 && !_WIN64
      args.push_back("-l");
      args.push_back(OS_get_full_program_file_path("llvm_math.ll"));
#endif

#if !COMPILE_SVG_IN_PARALLEL
      {
        QString error_message;
        
        svg_dir = create_svg_dir(args, error_message);
        if (svg_dir==NULL){
          R_ASSERT(error_message != "");
          return error_message;
        }

        
      }
#endif
 
      std::string error_message;

      //R_ASSERT(OPT_LEVEL==0);
      
      //printf("\n\n   Starting to create factory %s.  \n\n Optlevel: %d\n\n",code.toUtf8().constData(),optlevel);
      double time = TIME_get_ms();
      reply.factory = NULL;
      /*
      create_svg(code, options, reply);
      reply.svg_dir = NULL;
      return false;
      */
      if (opts.use_interpreter) {
        /*
        for(int i=0;i<args.get_argc();i++)
          printf("%d: \"%s\"\n",i, args.get_argv()[i]);
        */
        reply.interpreter_factory =
          createInterpreterDSPFactoryFromString(
                                                "FaustDev",
                                                opts.code.toUtf8().constData(),
                                                args.get_argc(),
                                                args.get_argv(),
                                                error_message
                                                );
        reply.factory = reply.interpreter_factory;

      } else {
#if defined(WITHOUT_LLVM_IN_FAUST_DEV)
        R_ASSERT(false);
        return "Compilation error";
#else
        /*
        for(int i=0;i<args.get_argc();i++)
          printf("%d: %s\n",i, args.get_argv()[i]);
        */
        
        reply.llvm_factory =
          createDSPFactoryFromString(
                                     "FaustDev",
                                     opts.code.toUtf8().constData(),
                                     args.get_argc(),
                                     args.get_argv(),
#if FOR_WINDOWS
  #if _WIN64
                                     "x86_64-w64-windows-gnu", // clang -target $P -v 2>&1 | grep Target
  #else
                                     "i686-w64-windows-gnu",
  #endif
#elif FOR_LINUX
                                     "x86_64-pc-linux-gnu",
#elif FOR_MACOSX
                                     "x86_64-apple-darwin17",
#else
#error "error"
#endif
                                     error_message,
                                     opts.optlevel
                                     );
        reply.factory = reply.llvm_factory;
#endif

      }

      printf("   Factory created %f\n\n\n\n", (TIME_get_ms() - time) / 1000.0);
      if (reply.factory==NULL) {

        printf("    Error,das: -%s-\n", reply.error_message.toUtf8().constData());

        if (error_message==""){
          printf("            returning unknown error\n");
          return "Unknown error";
        }

        return QString(error_message.c_str());
      }

      reply.factory->setMemoryManager(&g_v_calloc_memory_manager);

      return create_reply_data(reply);
    }
    
  public:

#if COMPILE_SVG_IN_PARALLEL
    void run_svg_job(instrument_t patch_id, const CompileOptions &opts, bool is_initializing) const {
      QString error_message;
      MyQTemporaryDir *svg_dir = create_svg(opts, error_message);
      //printf("    SVG error: -%s-\n", error_message.toUtf8().constData());

      THREADING_run_on_main_thread_async([patch_id, error_message, svg_dir]{ // THREADING_run_on_main_thread_async runs in order.

          struct SoundPlugin *plugin = NULL;

          struct Patch *patch = PATCH_get_from_id(patch_id); // if NULL, it means that the instrument has been deleted.

          if (patch!=NULL)
            plugin = (struct SoundPlugin*)patch->patchdata;

          if (plugin != NULL)
            FAUST_handle_new_svg_dir(plugin, svg_dir, error_message);
        },
        is_initializing
        );
    }
#endif

    void run_create_factory_job(struct SoundPlugin *maybe_plugin, instrument_t patch_id, const CompileOptions &opts, bool is_initializing) const {
      FFF_Reply reply;

      MyQTemporaryDir *svg_dir = NULL; // only used if !COMPILE_SVG_IN_PARALLEL

      QString error_string = create_reply_factory(opts, reply, svg_dir);
      reply.error_message = error_string;

      // Don't want to run invokeMethod since we could be called from anywhere where Qt::exec() is called. (invokeMethod callbacks may not be run in order either, but that particular problem can be fixed easily)
      //QMetaObject::invokeMethod(qApp, [patch_id, reply]{

#if COMPILE_SVG_IN_PARALLEL
      R_ASSERT(svg_dir==NULL);
#endif

      auto doit = [maybe_plugin, patch_id, reply, svg_dir, is_initializing]{

        bool has_added_error_message = false;
        
        if (is_initializing && reply.error_message != ""){
          has_added_error_message = true;
          GFX_addMessage(reply.error_message.toUtf8().constData());
        }

        struct SoundPlugin *plugin = maybe_plugin;

        if (plugin==NULL){
          R_ASSERT(is_initializing==false);

          struct Patch *patch = PATCH_get_from_id(patch_id);
          if(patch==NULL)
            return; // Instrument has been deleted.          

          plugin = (struct SoundPlugin*)patch->patchdata;
          if (plugin==NULL){
            R_ASSERT_NON_RELEASE(false);
            return;
          }
          
        } else {
          
          R_ASSERT(is_initializing==true);

        }

        Devdata *devdata = (Devdata*)plugin->data;
        devdata->is_compiling = false;
            
        bool success;
            
        if (Undo_num_undos()==0) {
          
          UNDO_OPEN();{
            success = FAUST_handle_fff_reply(plugin, reply, is_initializing);
          }UNDO_CLOSE();
          
        } else if (is_initializing || Undo_Is_Open()==true) {
          
          success = FAUST_handle_fff_reply(plugin, reply, is_initializing);
          
        } else {
          
          UNDO_REOPEN_LAST();{ // Simply add any undos create here into the last undo point. Adding a new undo point here would be confusing for the user since this function is triggered by a timer and not a user interaction. (and we need to add undo to avoid inconsitencies, i.e. we can't just call Undo_StartIgnoringUndo()/Undo_StopIgnoringUndo().)
            success = FAUST_handle_fff_reply(plugin, reply, is_initializing);
          }UNDO_CLOSE();
          
        }
        
        if (is_initializing && has_added_error_message==false && devdata->reply.error_message != ""){
          GFX_addMessage(devdata->reply.error_message.toUtf8().constData());
        }
        
        add_factory_ready(devdata, success);
        
#if !COMPILE_SVG_IN_PARALLEL
        {
          QString error_message;
          FAUST_handle_new_svg_dir(plugin, svg_dir, error_message);
        }
#endif
      };

      if(is_initializing) {

        R_ASSERT(THREADING_is_main_thread());
        R_ASSERT(maybe_plugin!=NULL);

        doit();

      } else {

        R_ASSERT(!THREADING_is_main_thread());
        R_ASSERT(maybe_plugin==NULL);

        // THREADING_run_on_main_thread_async should be safe to use instead of invokeMethod. (these callbacks are also guaranteed to be run in order)
        THREADING_run_on_main_thread_async(doit);

      }

    }

    void start_new_job(instrument_t patch_id, const CompileOptions &opts) const {

      //printf("           start_new_job. is_init: %d\n", false);

#if COMPILE_SVG_IN_PARALLEL
      QtConcurrent::run([patch_id, this, opts]{
          run_svg_job(patch_id, opts, false);
        });
#endif

      run_create_factory_job(NULL, patch_id, opts, false);
    }


    void start_new_job_now(struct SoundPlugin *plugin, const CompileOptions &opts, bool is_initializing) const {
      R_ASSERT(is_initializing==true);
      R_ASSERT(plugin!=NULL);

#if COMPILE_SVG_IN_PARALLEL
      run_svg_job(plugin->patch->id, opts, is_initializing);
#endif

      printf("         start_new_job_now. is_init: %d\n", is_initializing);

      run_create_factory_job(plugin, make_instrument(-1), opts, is_initializing);
    }


    void free_reply_data(FFF_Reply &reply, bool free_now) const {
      R_ASSERT(U_MUTEX==0 || g_faust_mutex.is_locked());

      if (reply.data==NULL && reply.interpreter_factory==NULL){
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
        if(reply.llvm_factory==NULL)
#endif
        return;
      }
      
      auto doit = [reply]{

        if (reply.data != NULL)
          delete_dsps_and_data2(reply.data);
        
        if(reply.interpreter_factory != NULL)
          deleteInterpreterDSPFactory(reply.interpreter_factory);
#if !defined(WITHOUT_LLVM_IN_FAUST_DEV)
        if(reply.llvm_factory != NULL)
          deleteDSPFactory(reply.llvm_factory);
#endif
      };

      if (free_now)
        doit();
      else
        QtConcurrent::run(doit);
    }
    
  private:
    
    void run(){

#if COMPILE_SVG_IN_PARALLEL
      setPriority(QThread::HighPriority); // To make this thread (which creates the dsp factories) run faster than the threads creating svg/c++ source.
#endif

      while(true){

        QMap<instrument_t, Request*> code_requests;

        // 1. First collect compilation requests. Here we only keep the newest compilation requests for each plugin, i.e. ensure that we only compile latest version.
        do{
          Request *request = g_queue.get();
          if (request->please_stop==true)   // Program exit.
            return;

          if (code_requests.contains(request->patch_id))
            delete code_requests[request->patch_id];
          
          code_requests[request->patch_id] = request;
          
        }while(g_queue.size() > 0);

        
        // 2. Then do the compilation
        
        QMapIterator<instrument_t, Request*> iterator(code_requests);

        {
#if U_MUTEX
          radium::ScopedMutex lock(g_faust_mutex);
#endif
          while (iterator.hasNext()) {
            
            iterator.next();

            const Request *request = iterator.value();

            start_new_job(request->patch_id, request->opts);

            delete request;
          }
        }

      }
    }
  };
  #include "mFaust_factory_factory.cpp"
}

static FFF_Thread g_fff_thread;
//static FFF_Thread g_fff_thread2;

static void init_fff(void){
  static bool has_started = false;

  if (has_started==false){
          //#if !RADIUM_FAUST_USE_INTERPRETER
    startMTDSPFactories(); // Make faust is thread safe
    //#endif
    g_fff_thread.startit();
    //g_fff_thread2.startit();
    has_started = true;
  }
}
  
void FFF_shut_down(void){
  if (g_fff_thread.isRunning()){
    
    Request *request = new Request(make_instrument(-1), CompileOptions("", "", 0, false));
    request->please_stop = true;
    
    g_queue.put(request);
    
    g_fff_thread.wait();
  }
}

static void FFF_run_now(struct SoundPlugin *plugin, const CompileOptions &opts){
#if U_MUTEX
  radium::ScopedMutex lock(g_faust_mutex);
#endif
  init_fff();

  printf("    Calling FFF_run_now\n");
  g_fff_thread.start_new_job_now(plugin, opts, true);
}

/*
llvm_dsp *FFF_get_dsp(const FFF_Reply &reply){
  //radium::ScopedMutex lock(fff_mutex);
  return createDSPInstance(reply.factory);
}
*/

static void FFF_request_reply(instrument_t patch_id, const CompileOptions &opts){
  init_fff();
  
  Request *request = new Request(patch_id, opts);
    
  g_queue.put(request);
}

// We don't call delete_dsps_and_data1 here since this function is called when cleaning up, and then the widgets have already been deleted.
void FFF_free_now(FFF_Reply &reply){
  if (reply.data==NULL)
    return;
#if U_MUTEX
  radium::ScopedMutex lock(g_faust_mutex);
#endif
  g_fff_thread.free_reply_data(reply, true);
}

