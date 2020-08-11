/* Copyright 2017 Kjetil S. Matheussen

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








#define __STDC_FORMAT_MACROS

#include "../common/includepython.h"

#include <inttypes.h>

#include <lo/lo.h>
#include <lo/lo_serverthread.h>

#include <QHash>
#include <QSet>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector>
#pragma clang diagnostic pop

#include "radium_proc.h"

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/settings_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


static int64_t g_curr_oscservernum = 0;
static int64_t g_curr_oscmethodnum = 0;

static int osc_callback(const char *path, const char *types, lo_arg **argv, int argc, lo_message msg, void *user_data);
  
namespace{

  struct OscMethod;
  struct OscServer;
  
  static QHash<int64_t,OscMethod*> g_osc_methods;
  static QHash<int64_t,OscServer*> g_osc_servers;


  struct OscMethod{
    int64_t _id = g_curr_oscmethodnum++;

    int64_t _osc_server_id;

    int _num_fails = 0;
    
    lo_server_thread _server_thread;
    
    radium::ProtectedS7Extra<func_t*> _callback = radium::ProtectedS7Extra<func_t*>("osc_method_callback");
    //radium::GcHolder<const char> _typespec;
    lo_method _method;
    
    OscMethod(int64_t osc_server_id, lo_server_thread server_thread, const_char *path, const char *typespec, func_t *callback)
      : _osc_server_id(osc_server_id)
      , _server_thread(server_thread)
      , _callback(callback)
        //, _typespec(talloc_strdup(typespec))
      , _method(lo_server_thread_add_method(_server_thread, path, typespec, osc_callback, this))
    {
      g_osc_methods[_id] = this;
    }
    
    ~OscMethod(){
      if (_method != NULL){
        auto ret = lo_server_thread_del_lo_method(_server_thread, _method);
        R_ASSERT(ret==0);
      }

      g_osc_methods.remove(_id);
    }
  };
  
  struct OscServer{
    int64_t _id = g_curr_oscservernum++;

  private:
    
    lo_server _server = NULL;
    
    lo_server_thread _server_thread = NULL;
    bool _server_thread_started = false;
    
    QVector<OscMethod*> _methods;
    
  public:
    
    bool _is_valid = false;
    
    OscServer(const_char* port, int protocol){
      g_osc_servers[_id] = this;
      
      _server_thread = lo_server_thread_new_with_proto(!strcmp(port, "") ? NULL : port, protocol, NULL );
      
      if (_server_thread==NULL){
        handleError("createOscServer: Unable to create OSC server thread");
        return;
      }
      
      if (lo_server_thread_start(_server_thread) < 0) {
        handleError("createOscServer: Unable to create OSC server thread");
        return;
      }
      
      _server_thread_started = true;
      
      _server = lo_server_thread_get_server(_server_thread);
      
      if (_server==NULL){
        handleError("createOscServer: Unable to create OSC server");
        return;
      }

      _is_valid = true;
    }

    ~OscServer(){
      for(auto *method : _methods)
        delete method;

      if (_server_thread) {
        
        if (_server_thread_started)
          lo_server_thread_stop(_server_thread);
        
        lo_server_thread_free(_server_thread);
      }

      // Why on earth can't I call lo_server_free? (it crashes, and the nsm example code doesn't call it either). It's not even documented that this function should not be called. Should probably not have used liblo...
      //if (_server)
      //  lo_server_free(_server);

      g_osc_servers.remove(_id);
    }

    int64_t add_method(const_char *path, const_char* typespec, func_t* callback){
      R_ASSERT(_is_valid);
      
      auto method = new OscMethod(_id, _server_thread, path, typespec, callback);
      if (method->_method==NULL){
        delete method;
        return -1;
      } else {
        _methods.push_back(method);
        return method->_id;
      }
    }

    void remove_method(int64_t method_id){
      OscMethod *method = g_osc_methods[method_id];
      R_ASSERT(_methods.removeAll(method)==1);
      delete method;
    }
    
    int send_message(lo_address address, const_char* path, lo_message message){
      return lo_send_message_from(address, _server, path, message);
    }

  };
}


int64_t createOscServer(const_char* port, int protocol){

  OscServer *o = new OscServer(port, protocol);

  if (o->_is_valid) {
    
    return o->_id;
    
  } else {

    delete o;
    return -1;
    
  }

}


static bool assertServerId(const char *funcname, int64_t server_id){
  if (g_osc_servers.contains(server_id))
    return true;
  
  if (server_id >= g_curr_oscservernum || server_id < 0)
    handleError("%s: Server #%d not found", funcname, (int)server_id);
  else
    handleError("%s: Server #%d has been closed", funcname, (int)server_id);

  return false;    
}

static bool assertMethodId(const char *funcname, int64_t method_id){
  if (g_osc_methods.contains(method_id))
    return true;
  
  if (method_id >= g_curr_oscmethodnum || method_id < 0)
    handleError("%s: Method #%d not found", funcname, (int)method_id);
  else
    handleError("%s: Method #%d has been closed", funcname, (int)method_id);

  return false;    
}

void closeOscServer(int64_t server_id){
  if (!assertServerId(__FUNCTION__, server_id))
    return;

  delete g_osc_servers[server_id];
}

int getOscProtocolFromUrl(const_char* url){
  lo_address addr = lo_address_new_from_url(url);
  int proto = lo_address_get_protocol(addr);
  lo_address_free(addr);
  return proto;
}

static int osc_callback(const char *path, const char *typespec, lo_arg **argv, int argc, lo_message msg, void *user_data){
#if !defined(RELEASE) 
  printf("OSC_CALLBACK called. typespec: -%s-. Url: %s. Port: %s. Hostname: %s. Protocol: %d\n",
         typespec,
         lo_address_get_url( lo_message_get_source(msg)),
         lo_address_get_port( lo_message_get_source(msg)),
         lo_address_get_hostname( lo_message_get_source(msg)),
         lo_address_get_protocol( lo_message_get_source(msg))
         );
#endif
  
  int result;
  
  THREADING_run_on_main_thread_and_wait([typespec, argv, argc, msg, user_data, &result](){
  
      OscMethod *method = static_cast<OscMethod*>(user_data);
      
      static dynvec_t args = {}; // static to avoid allocating bdwgc-memory.
      
      DYNVEC_light_clean(args);
      
      {
        const_char* url = lo_address_get_url(lo_message_get_source(msg));
        DYNVEC_push_back(args, DYN_create_string(url));
        free((void*)url);
      }
      
      for(int i=0;i<argc;i++){
        switch(typespec[i]){
          
          case 'i':
            DYNVEC_push_back(args, DYN_create_int(argv[i]->i));
            break;
            
          case 'f':
            DYNVEC_push_back(args, DYN_create_float(argv[i]->f));
            break;
            
          case 's':
            DYNVEC_push_back(args, DYN_create_string(&argv[i]->s));
            break;

          default:
            R_ASSERT(false);
        }
      }

      bool docancel = false;
      
      dyn_t result2 = s7extra_applyFunc_dyn(method->_callback.get(), args);
      
      if (result2.type == SYMBOL_TYPE) {
        
        if (!strcmp(result2.symbol, "grab"))
          result = 0;
        else if (!strcmp(result2.symbol, "pass"))
          result = 1;
        else if (!strcmp(result2.symbol, "grab-and-close"))
          docancel = true;
        else if (!strcmp(result2.symbol, "pass-and-close")){
          result = 1;
          docancel = true;
        } else {
          
          printf("    OSC method: Wrong return type from callback. Expected 'grab, 'pass, 'grab-and-close, or 'pass-and-close, got '%s. Fails: %d / 1000", result2.symbol, method->_num_fails++);
        }
        
      } else {
        
        printf("    OSC method: Wrong return type from callback. Expected symbol, got %s. Fails: %d / 1000", DYN_type_name(result2), method->_num_fails++);
        
      }

      if (method->_num_fails > 1000){
        printf("    OSC method: Failed 1000 times. Removing");
        docancel = true;
      }
      
      if (docancel)
        evalScheme(talloc_format("(ra:schedule 0 (lambda () (ra:close-osc-method %" PRId64 ") #f))", method->_id)); // might be safer to close server after method callback has returned.
    });
    
  return result;
}
    
int64_t addOscMethod(int64_t server_id, const_char* path, const_char* typespec, func_t* callback){
  if (!assertServerId(__FUNCTION__, server_id))
    return -1;

  int64_t ret = g_osc_servers[server_id]->add_method(path, typespec, callback);
  if (ret==-1)
    handleError("addOscMethod: Creating method failed.");

  return ret;
}

void closeOscMethod(int64_t method_id){
  if (!assertMethodId(__FUNCTION__, method_id))
    return;

  int64_t server_id = g_osc_methods[method_id]->_osc_server_id;

  if (!assertServerId(__FUNCTION__, server_id)){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  g_osc_servers[server_id]->remove_method(method_id);
}

static lo_message create_lo_message_from_dynvec(const_char* typespec, dynvec_t args){
  if ((int)strlen(typespec) != args.num_elements){
    handleError("sendOscMessage(From): Typespec/args mismatch. Wrong number of arguments. Length of typespec is %d while length of args is %d", (int)strlen(typespec), args.num_elements);
    return NULL;
  }
  
  lo_message message = lo_message_new();

  int i = 0;
  
  for(const dyn_t dyn : args){
    const char t = typespec[i];
    switch(t){
      case 'i':
        if (!DYN_is_number(dyn)){
          handleError("sendOscMessage(From): Typespec/args mismatch. Expected integer for argument #%d, found %s", i, DYN_type_name(dyn));
          goto failed;
        }
        lo_message_add_int32(message, (int)DYN_get_int64_from_number(dyn));
        break;
      case 'f':
        if (!DYN_is_number(dyn)){
          handleError("sendOscMessage(From): Typespec/args mismatch. Expected float for argument #%d, found %s", i, DYN_type_name(dyn));
          goto failed;
        }
        lo_message_add_float(message, (int)DYN_get_double_from_number(dyn));
        break;
      case 's':
        if (dyn.type == STRING_TYPE) {
          lo_message_add_string(message, STRING_get_chars(dyn.string));
        } else if (dyn.type == SYMBOL_TYPE){
          lo_message_add_string(message, dyn.symbol);
        } else {
          handleError("sendOscMessage(From): Typespec/args mismatch. Expected string for argument #%d, found %s", i, DYN_type_name(dyn));
          goto failed;
        }
        break;
      default:
        handleError("sendOscMessage(From): Type '%c' is not supported", t);
        goto failed;
    }
    i++;
  }

  return message;

 failed:
  lo_message_free(message);
  return NULL;
}
 
void sendOscMessage(const_char* url, const_char* path, const_char* typespec, dynvec_t args){
  lo_message message = create_lo_message_from_dynvec(typespec, args);
  if (message==NULL)
    return;
  
  lo_address address = lo_address_new_from_url(url);
  if (address==NULL){
    handleError("sendOscMessage: Unable to create address from URL \"%s\"", url);
    goto exit;
  }

  {
    int ret = lo_send_message(address, path, message);
    if (ret < 0)
      handleError("sendOscMessage: Could not send message to \"%s\" / \"%s\"", url, path);
  }
  
 exit:
  if (message)
    lo_message_free(message);

  if (address)
    lo_address_free(address);
}

void sendOscMessageFrom(const_char* url, int64_t server_id, const_char* path, const_char* typespec, dynvec_t args){
  if (!assertServerId(__FUNCTION__, server_id))
    return;

  lo_address address = NULL;
  
  OscServer *o = g_osc_servers[server_id];

  lo_message message = create_lo_message_from_dynvec(typespec, args);
  if (message==NULL)
    goto exit;
  
  address = lo_address_new_from_url(url);
  if (address==NULL){
    handleError("sendOscMessageFrom: Unable to create address from URL \"%s\"", url);
    goto exit;
  }

  {
    int ret = o->send_message(address, path, message);
    if (ret < 0)
      handleError("sendOscMessage: Could not send message to \"%s\" / \"%s\"", url, path);
  }
  
 exit:
  if (message)
    lo_message_free(message);

  if (address)
    lo_address_free(address);
}



/* NSM */


bool nsmHasInited(void){
  static bool s_has_inited = false;

  if (s_has_inited==true)
    return true;
  
  dyn_t nsm_has_inited = evalSchemeWithReturn("*nsm-has-inited*");
  
  if (nsm_has_inited.type != BOOL_TYPE){

    R_ASSERT(false);
    return false;
    
  } else {

    if (nsm_has_inited.bool_number)
      s_has_inited = true;
    
    return nsm_has_inited.bool_number;
    
  }
}

void waitUntilNsmHasInited(void){
  int safety = 0;
  while(!nsmHasInited() && safety < 200){
    msleep(25);
    THREADING_call_very_often();
    safety++;
  }

  if (safety > 198)
    GFX_addMessage("NSM took more than 5 seconds to start.");    
}

bool nsmIsActive(void){
  if (!nsmHasInited()){
    handleError("nsmIsActive: NSM has not inited yet");
    return false;
  }
    
  R_ASSERT(nsmHasInited());
  
  dyn_t nsm_is_active = evalSchemeWithReturn("*nsm-is-active*");
  
  if (nsm_is_active.type != BOOL_TYPE){

    R_ASSERT(false);
    return false;
    
  } else {
    
    return nsm_is_active.bool_number;
    
  }
}

const_char* getNsmClientId(void){
  if (!nsmHasInited()){
    handleError("getNsmClientId: NSM has not inited yet");
    return "";
  }
  
  static const_char* s_client_id = "";
  
  static bool s_has_inited = false;

  if (!nsmIsActive())
    return "";
    
  if (s_has_inited==false){

    for(int i = 0 ; i < 200 ; i++) {
      dyn_t client_id = evalSchemeWithReturn("*nsm-client-id*");

      if (client_id.type!=STRING_TYPE){

        msleep(25);
        THREADING_call_very_often();
        
      } else {
        
        s_client_id = strdup(STRING_get_chars(client_id.string));
        break;
        
      }

      if (i > 198)
        GFX_addMessage("NSM took more than 5 seconds to call /nsm/client/open. Giving up.");    

    }
    
    s_has_inited = true;
  }

  return s_client_id;
  
}

static bool g_supportsSwitchNsmCapability = true;

bool supportsSwitchNsmCapability(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_supportsSwitchNsmCapability = SETTINGS_read_bool("supports_switch_nsm_capability", g_supportsSwitchNsmCapability);
    has_inited = true;
  }

  return g_supportsSwitchNsmCapability;
}

void setSupportsSwitchNsmCapability(bool val){
  g_supportsSwitchNsmCapability = val;
  SETTINGS_write_bool("supports_switch_nsm_capability", val);
}

void nsmNewSong(void){
  if (!nsmIsActive()){
    handleError("nsmNewSong: NSM is not active");
    return;
  }
  
  if(Undo_NSM_are_you_sure_questionmark()==false)
    return;

  evalScheme("(FROM_C-nsm-new-song)");
}

void nsmSave(void){
  if (!nsmIsActive()){
    handleError("nsmNewSong: NSM is not active");
    return;
  }
  
  evalScheme("(FROM_C-nsm-save)");
}

void nsmDuplicate(void){
  if (!nsmIsActive()){
    handleError("nsmNewSong: NSM is not active");
    return;
  }
  
  if(Undo_NSM_are_you_sure_questionmark()==false)
    return;

  evalScheme("(FROM_C-nsm-save-as)");
}

bool nsmOpen(void){
  if (!nsmIsActive()){
    handleError("nsmNewSong: NSM is not active");
    return false;
  }
  
  if(Undo_NSM_are_you_sure_questionmark()==false)
    return false;
  
  evalScheme("(FROM_C-nsm-open)");
  return true;
}

void nsmQuit(void){
  if (!nsmIsActive()){
    handleError("nsmNewSong: NSM is not active");
    return;
  }
  
  if(Undo_are_you_sure_questionmark()==false)
    return;

  evalScheme("(FROM_C-nsm-quit)");
}
