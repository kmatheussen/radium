
#if 0 //QT_VERSION >= 0x050600

#include <QTemporaryDir> // Spends too much time deleting files, and gives error messages as well.

#else
namespace{

  static inline bool try_temp_path(QString &path, QString &error_string){
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
  
  static inline bool get_new_temp_path(const QString &templatePath, QString &path, QString &error_string){

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
  static inline bool removeDir(const QString & dirName)
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
  struct MyQTemporaryDir{

    QString _error_string;
    QString _path;
    bool _is_valid;
    
    MyQTemporaryDir(const QString &templatePath){
      _path = templatePath;
      _is_valid = get_new_temp_path(templatePath, _path, _error_string);
    }
    
    ~MyQTemporaryDir(){
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

