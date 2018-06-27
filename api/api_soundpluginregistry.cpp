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








#include "../common/includepython.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop



#include <QFileInfo>
#include <QFile>
#include <QDateTime>
#include <QDir>
#include <QStack>
#include <QSet>

#include "../common/nsmtracker.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_disk_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/visual_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"
#include "api_instruments_proc.h"




static QString extend_path(const QString a, const QString b){
  if (a=="")
    return b;
  else
    return a + " / " + b;
}

static QString get_path(const QStack<QString> &dir){
  QString ret;
  for(QString s : dir)
    ret = extend_path(ret, s);
  return ret;
}

static hash_t *get_entry_from_type(const SoundPluginType *type, const QString path){
  hash_t *hash = HASH_create(5);
  HASH_put_string(hash, ":type", PluginMenuEntry::type_to_string(PluginMenuEntry::IS_NORMAL));
  HASH_put_string(hash, ":path", path);
  HASH_put_string(hash, ":container-name", type->container==NULL ? "" : type->container->name);
  HASH_put_string(hash, ":type-name", type->type_name);
  HASH_put_string(hash, ":name", type->name);
  HASH_put_int(hash, ":num-inputs", type->num_inputs);
  HASH_put_int(hash, ":num-outputs", type->num_outputs);
  HASH_put_chars(hash, ":category", type->category==NULL ? "" : type->category);
  HASH_put_chars(hash, ":creator", type->creator==NULL ? "" : type->creator);
  HASH_put_int(hash, ":num-uses", type->num_uses);
  return hash;
}

static hash_t *get_container_entry(const SoundPluginTypeContainer *container, const QString path, bool is_blacklisted){
  hash_t *hash = HASH_create(10);
  HASH_put_string(hash, ":filename", container->filename);
  HASH_put_string(hash, ":type-name", container->type_name);
  HASH_put_string(hash, ":name", container->name);
  HASH_put_string(hash, ":path",  path);
  HASH_put_int(hash, ":num-uses", container->num_uses);
  if (is_blacklisted){
    HASH_put_bool(hash, ":is-blacklisted", true);
    HASH_put_string(hash, ":category", "Unstable");
  }else{
    HASH_put_string(hash, ":category", "");
  }
  return hash;
}


/***************************
       Blacklist
 **************************/

#include <QHash>

enum BlacklistCached{
  NOT_IN_CACHE = 0,
  NOT_BLACKLISTED,
  BLACKLISTED
};

static QHash<QString, enum BlacklistCached> g_blacklisted_cache;

static void update_blacklist_cache(const SoundPluginTypeContainer *container, bool is_blacklisted){
  g_blacklisted_cache[STRING_get_qstring(container->filename)] = is_blacklisted ? BLACKLISTED : NOT_BLACKLISTED;
}
  
static QString get_blacklist_filename(const SoundPluginTypeContainer *plugin_type_container){
  QString encoded = STRING_get_qstring(STRING_get_sha1(plugin_type_container->filename));

  return OS_get_dot_radium_path() + QDir::separator() + SCANNED_PLUGINS_DIRNAME + QDir::separator() + "blacklisted_" + encoded;
}

void API_blacklist_container(const SoundPluginTypeContainer *container){
  update_blacklist_cache(container, true);
  
  QString disk_blacklist_filename = get_blacklist_filename(container);
  disk_t *file = DISK_open_for_writing(disk_blacklist_filename);
  hash_t *hash = HASH_create(1);

  HASH_put_string(hash, "filename", container->filename);
  
  HASH_save(hash, file);
  
  DISK_close_and_delete(file); // Shows error message if something goes wrong.
}

void API_unblacklist_container(const SoundPluginTypeContainer *container){
  update_blacklist_cache(container, false);
  
  QString disk_blacklist_filename = get_blacklist_filename(container);
  
  if (QFile::remove(disk_blacklist_filename)==false)
    GFX_Message(NULL, "Error: Unable to delete file \"%s\"", disk_blacklist_filename.toUtf8().constData());
}

bool API_container_is_blacklisted(const SoundPluginTypeContainer *container){
  const enum BlacklistCached cached = g_blacklisted_cache[STRING_get_qstring(container->filename)];

#define Vars()                                                          \
  QString disk_blacklist_filename = get_blacklist_filename(container);  \
  bool is_blacklisted = QFile::exists(disk_blacklist_filename);
  
#if !defined(RELEASE)
  Vars();
  if (cached != NOT_IN_CACHE){
    if (is_blacklisted)
      R_ASSERT(cached==BLACKLISTED);
    if (!is_blacklisted)
      R_ASSERT(cached==NOT_BLACKLISTED);
  }
#endif
  
  if (cached==NOT_BLACKLISTED)
    return false;
  
  if (cached==BLACKLISTED)
    return true;
  
#if defined(RELEASE)
  Vars();
#endif

#undef Vars
  
  update_blacklist_cache(container, is_blacklisted);
  
  return is_blacklisted;
}



/***************************
       Entries
 **************************/

static QString get_disk_entries_dir(void){
  return OS_get_dot_radium_path() + QDir::separator() + SCANNED_PLUGINS_DIRNAME + QDir::separator();
}

static hash_t *g_disk_entries_cache = NULL;
static QSet<QString> g_known_noncached_entries;


static QString get_disk_entries_filename(const SoundPluginTypeContainer *plugin_type_container){
  QString encoded = STRING_get_qstring(STRING_get_sha1(plugin_type_container->filename));
  return get_disk_entries_dir() + "v2_" + encoded;
}

static void cleanup_old_disk_files(void){
  static bool has_done = false;
  if (has_done==true)
    return;

  QDir dir(get_disk_entries_dir());
  QFileInfoList list = dir.entryInfoList(QDir::Files);

  {
    bool has_deleted = false;
    for(auto info : list){
      QString name = info.fileName();
      if (!name.startsWith("v2_") && !name.startsWith("blacklisted_")){
        printf("   Deleting obsolete file %s\n", name.toUtf8().constData());
        QFile::remove(info.absoluteFilePath());
        has_deleted = true;
      }
    }
    if (has_deleted==true){
      GFX_addMessage("Plugin registry data has been deleted. You might want to rescan all "
                     "plugins. This was necessary since the file format has changed. Really sorry for the inconvenience");
    }
  }

  has_done = true;
}


static hash_t *get_container_disk_hash(const SoundPluginTypeContainer *container, const QString path){
  hash_t *hash = HASH_create(container->num_types + 10);

  HASH_put_int(hash, "version", 1);

  {
    QFileInfo info(STRING_get_qstring(container->filename));
    if (info.exists()==false){
      GFX_Message(NULL, "Error: Plugin file %S does not seem to exist anymore.", container->filename);
      return NULL;
    }

    int64_t filesize = info.size();
    if (filesize==0){
      //GFX_addMessage("Error: Plugin file %s seems to have size 0.", STRING_get_chars(container->filename));
      printf("Error: Plugin file %S seems to have size 0.", container->filename);
      //return NULL;  // No need to fail.
    }
    
    QDateTime datetime = info.lastModified();
    if (datetime.isValid()==false)
      printf("Warning: plugin %S does not have a valid write time", container->filename); // Could perhaps happen on some filesystems.
    
    int64_t writetime = datetime.isValid() ? datetime.toUTC().toMSecsSinceEpoch() : 0;
    
    HASH_put_string(hash, "filename", container->filename);
    HASH_put_int(hash, "filesize", filesize);
    HASH_put_int(hash, "writetime", writetime);
  }

  return hash;
}

static DiskOpReturn save_disk_hash(const SoundPluginTypeContainer *container, const QString path, hash_t *hash){
  QString disk_entries_filename = get_disk_entries_filename(container);
    
  disk_t *file = DISK_open_for_writing(disk_entries_filename);
  if (file==NULL){
    GFX_Message(NULL, "Error: Unable to write to file \"%s\"", disk_entries_filename.toUtf8().constData());
    return DiskOpReturn::ALL_MAY_FAIL;
  }
  
  HASH_save(hash, file);
  
  if (DISK_close_and_delete(file)==false) // Shows error message if something goes wrong.
    return DiskOpReturn::ALL_MAY_FAIL;
  
  
  {
    const char *key = talloc_strdup(disk_entries_filename.toUtf8().constData());
    
    if (HASH_has_key(g_disk_entries_cache, key))
      HASH_remove(g_disk_entries_cache, key);
    
    //printf("         CACHE: Adding %s to cache\n", key);
    HASH_put_hash(g_disk_entries_cache, key, hash);
    
    //printf("         CACHE: Removing %s to known noncached\n", key);
    g_known_noncached_entries.remove(disk_entries_filename);
  }

  return DiskOpReturn::SUCCEEDED;
}


static DiskOpReturn save_entries_to_disk(const QVector<hash_t*> &entries, const SoundPluginTypeContainer *container, const QString path){
  R_ASSERT_RETURN_IF_FALSE2(container->is_populated, DiskOpReturn::THIS_ONE_FAILED);  
                                       
  hash_t *hash = get_container_disk_hash(container, path);
  if (hash==NULL)
    return DiskOpReturn::THIS_ONE_FAILED;
  
  dynvec_t dynvec = {};
    
  for(hash_t *hash : entries)
    DYNVEC_push_back(dynvec, DYN_create_hash(hash));
    
  HASH_put_array(hash, "entries", dynvec);
  
  return save_disk_hash(container, path, hash);
}


// Returns true if entries was found in diskcache (i.e. cache stored on disk) and put into 'ret'.
static bool load_entries_from_diskcache(dynvec_t &ret, const SoundPluginTypeContainer *container, const QString path){

  QString disk_entries_filename = get_disk_entries_filename(container);

  if (g_known_noncached_entries.contains(disk_entries_filename)){
    //printf("         CACHE: Getting %s from knwon noncached\n", disk_entries_filename.toUtf8().constData());
    return false;
  }
    
  hash_t *hash;

  const char *key = talloc_strdup(disk_entries_filename.toUtf8().constData());

  if (HASH_has_key(g_disk_entries_cache, key)){

    //printf("         CACHE: Getting %s from cache\n", key);
    hash = HASH_get_hash(g_disk_entries_cache, key);

    QString loaded_filename = HASH_get_qstring(hash, "filename");
    if (loaded_filename != STRING_get_qstring(container->filename)){
      // Oops. sha1 crash. How likely is that?
      printf("             NOT SAME FILENAME: -%s-",loaded_filename.toUtf8().constData());
      printf("                                -%S-",container->filename);
      return false;
    }

  } else {
    
    if (QFile(disk_entries_filename).exists()==false){
      printf("         CACHE: Adding %s to known noncached\n", key);
      g_known_noncached_entries.insert(disk_entries_filename);
      return false;
    }
  
    disk_t *file = DISK_open_for_reading(disk_entries_filename);
    if (file==NULL){
      GFX_Message(NULL, "Error: Unable to read file \"%s\"", disk_entries_filename.toUtf8().constData());
      return false;
    }
    
    hash = HASH_load(file); // HASH_load shows error message

    DISK_close_and_delete(file); // Shows error message if something goes wrong.
  
    if (hash == NULL) {
      
      // File contains errors. Delete it so we don't get the same problem next time.
      if (QFile::remove(disk_entries_filename)==false)
        GFX_Message(NULL, "Error: Unable to delete file \"%s\"", disk_entries_filename.toUtf8().constData());
      
      return false;
    }

    printf("         CACHE: Adding %s to cache\n", key);
    HASH_put_hash(g_disk_entries_cache, key, hash);
  }

  dynvec_t entries = HASH_get_array(hash, "entries");
  
  for(int i = 0 ; i < entries.num_elements ; i++)
    R_ASSERT_RETURN_IF_FALSE2(entries.elements[i].type==HASH_TYPE, false);
  
  for(int i = 0 ; i < entries.num_elements ; i++) {

    // Check that path is correct. It might not be if the same container file exists in several different paths.
    hash_t *hash = entries.elements[i].hash;

    QString disk_path = HASH_get_qstring(hash, ":path");
    QString correct_path = extend_path(path, HASH_get_qstring(hash, ":name"));

    if (disk_path != correct_path){
      HASH_remove(hash, ":path");
      HASH_put_string(hash, ":path", correct_path);
      DYNVEC_push_back(ret, DYN_create_hash(hash));
    } else {
      DYNVEC_push_back(ret, entries.elements[i]);
    }
  }
  
  return true;
}

int API_get_num_entries_in_disk_container(SoundPluginTypeContainer *container){
  dynvec_t ret = {};
  if (load_entries_from_diskcache(ret, container, "")==false)
    return -1;
  return ret.num_elements;
}

static void get_entries_from_populated_container(dynvec_t &ret, SoundPluginTypeContainer *container, const QString path){

  R_ASSERT_RETURN_IF_FALSE(container->is_populated);
  
  QVector<hash_t*> entries;

  for(int i=0 ; i < container->num_types ; i++){
    const SoundPluginType *type = container->plugin_types[i];
    hash_t *new_hash = get_entry_from_type(type, extend_path(path, type->name));
    entries.push_back(new_hash);
  }

  if (container->has_saved_disk_entry==false) {
    save_entries_to_disk(entries, container, path);
    container->has_saved_disk_entry=true; // We might not have succeded writing disk entry, but we don't want to try again later.
  }
  
  for (hash_t *hash : entries)
    DYNVEC_push_back(ret, DYN_create_hash(hash));
}


static void get_entry(dynvec_t &ret, const PluginMenuEntry &entry, const QString path){
  cleanup_old_disk_files();

  hash_t *hash = NULL;

  if (entry.type==PluginMenuEntry::IS_CONTAINER && entry.plugin_type_container->is_populated){

    get_entries_from_populated_container(ret, entry.plugin_type_container, path);
        
  } else if (entry.type==PluginMenuEntry::IS_CONTAINER){

    QString new_path = extend_path(path, entry.plugin_type_container->name);

    bool is_blacklisted = API_container_is_blacklisted(entry.plugin_type_container);
    
    if (is_blacklisted || load_entries_from_diskcache(ret, entry.plugin_type_container, new_path) == false)
      hash = get_container_entry(entry.plugin_type_container, new_path, is_blacklisted);
    
  } else if (entry.type==PluginMenuEntry::IS_LEVEL_UP){
    
    hash = HASH_create(2);
    HASH_put_string(hash, ":name", entry.level_up_name);
  
  } else if (entry.type==PluginMenuEntry::IS_NUM_USED_PLUGIN){
    
    hash = HASH_create(5);
    HASH_put_string(hash, ":container-name", entry.hepp.container_name);
    HASH_put_string(hash, ":type-name", entry.hepp.type_name);
    HASH_put_string(hash, ":name", entry.hepp.name);
    HASH_put_int(hash, ":num-uses", entry.hepp.num_uses);
    
  } else if (entry.type==PluginMenuEntry::IS_NORMAL){
        
    if (entry.plugin_type!=NULL)
      DYNVEC_push_back(ret, DYN_create_hash(get_entry_from_type(entry.plugin_type, extend_path(path, entry.plugin_type->name))));
    else
      R_ASSERT(false);
        
  } else {
    hash = HASH_create(1);
  }

  if (hash!=NULL){
    HASH_put_string(hash, ":type", PluginMenuEntry::type_to_string(entry));
    DYNVEC_push_back(ret, DYN_create_hash(hash));
    //printf("Pushing back %p\n", hash);
  }
}

static int g_spr_generation = 0;
void API_incSoundPluginRegistryGeneration(void){
  g_spr_generation++;
}

int getSoundPluginRegistryGeneration(void){
  return g_spr_generation;
}

// Note, NOT the same as clearSoundPluginRegistry.
void API_clearSoundPluginRegistryCache(void){
  g_disk_entries_cache=HASH_create(1000);
  g_known_noncached_entries.clear();
}

dyn_t getSoundPluginRegistry(bool only_normal_and_containers){
  const QVector<PluginMenuEntry> entries = PR_get_menu_entries();
  
  dynvec_t ret = {};
  
  QStack<QString> dir;
  
  for(const PluginMenuEntry &entry : entries){
    
    switch(entry.type){
    case PluginMenuEntry::IS_LEVEL_UP:
      dir.push(entry.level_up_name);
      break;
    case PluginMenuEntry::IS_LEVEL_DOWN:
      dir.pop();
      break;
    default:
      break;
    }
    
    if (!only_normal_and_containers || entry.type==PluginMenuEntry::IS_NORMAL || entry.type==PluginMenuEntry::IS_CONTAINER)
      get_entry(ret, entry, get_path(dir));
    }
  
  return DYN_create_array(ret);
}

dyn_t populatePluginContainer(dyn_t entry){
  dynvec_t ret = {};

  if (entry.type!=HASH_TYPE){
    handleError("openPluginContainer: argument is not a hash table");
    goto exit;
  }

  {
    hash_t *hash=entry.hash;
    const char *name = HASH_get_chars(hash, ":name");
    QString type = HASH_get_chars(hash, ":type");
    const char *type_name = HASH_get_chars(hash, ":type-name");
    const char *path = HASH_get_chars(hash, ":path");
  
    if (PluginMenuEntry::type_to_string(PluginMenuEntry::IS_CONTAINER) != type){
      handleError("openPluginContainer: Excpected %s, found %s", PluginMenuEntry::type_to_string(PluginMenuEntry::IS_CONTAINER).toUtf8().constData(), HASH_get_chars(hash, ":type"));
      goto exit;
    }
  
    if(name==NULL){
      handleError("Missing :name");
      goto exit;
    }
  
    if(type_name==NULL){
      handleError("Missing :type-name");
      goto exit;
    }
  
    if(path==NULL){
      handleError("Missing :path");
      goto exit;
    }

    {
      SoundPluginTypeContainer *container = PR_get_populated_container(name, type_name); // PR_get_populated_container will populate the container if it isn't already populated
      if (container==NULL){
        //handleError("Could not find container %s / %s", name, type_name); // Screws up scanning since handleError casts an exception. Besides, we already get error messages from PR_get_container.
        goto exit;
      }

      get_entries_from_populated_container(ret, container, path);
    }
  }
  
 exit:
  return DYN_create_array(ret);
}

// Note, NOT the same as API_clearSoundPluginRegistryCache.
void clearSoundPluginRegistry(void){
  QDir dir(get_disk_entries_dir());
  QFileInfoList list = dir.entryInfoList(QDir::AllEntries|QDir::NoDotAndDotDot);

  for(auto info : list){
    QString name = info.fileName();
    if (!name.startsWith("blacklisted_")){
      printf("   Deleting file %s\n", name.toUtf8().constData());
      QFile::remove(info.absoluteFilePath());
    }
  }

  PR_init_plugin_types();
}
