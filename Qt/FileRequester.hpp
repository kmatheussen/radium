#include <QByteArray>
#include <QFileDialog>
#include <QSortFilterProxyModel>
#include <QFileSystemModel>
#include <QComboBox>

#include "FocusSniffers.h"


extern QByteArray g_filedialog_geometry;

extern bool g_has_resized_filedialog;

namespace radium{

  static inline void fixqfiledialog(QWidget *widget){
    //return; // checking.
    if (widget != NULL){
      
      QComboBox *box = dynamic_cast<QComboBox*>(widget);
      if (box != NULL)
        box->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);

      QSplitter *splitter = dynamic_cast<QSplitter*>(widget);
      if (splitter != NULL){
        if (splitter->orientation()==Qt::Vertical){
          splitter->setStretchFactor(0,1);
          splitter->setStretchFactor(1,4);
        }
      }
      
      for(auto *c : widget->children())
        fixqfiledialog(dynamic_cast<QWidget*>(c));
    }
  }

  struct FileRequester : public QFileDialog {

    class FileFilterProxyModel : public QSortFilterProxyModel
    {
    protected:
      bool filterAcceptsRow(int source_row, const QModelIndex& source_parent) const override{
        QFileSystemModel* fileModel = qobject_cast<QFileSystemModel*>(sourceModel());
        fileModel->setNameFilterDisables(false);
        return QSortFilterProxyModel::filterAcceptsRow(source_row, source_parent);
      }

#if 1 // !defined(FOR_WINDOWS) (seems to fail in Wine only)
      void sort(int column, Qt::SortOrder order = Qt::AscendingOrder) override{
        QFileSystemModel* fileModel = qobject_cast<QFileSystemModel*>(sourceModel());
        fileModel->sort(column, order);
      }
#endif
    };
    
  public:
    
    FileRequester(QWidget *parent, QString header_text, QString dir, QString filetypename, QString postfixes, bool for_loading, QString default_suffix = "", bool several_files = false)
      : QFileDialog(parent, header_text, dir, FileRequester::get_postfixes_filter(filetypename, postfixes))
    {
      /*
      setWindowTitle(header_text);
      setDirectory(dir);
      setSupportedSchemes(FileRequester::get_postfixes_filter(filetypename, postfixes).split(";;"));
      */

      if (default_suffix != "")
        setDefaultSuffix(default_suffix);
      
      setOption(QFileDialog::DontUseCustomDirectoryIcons, true); // Sometimes make windows crawl if I remember correctly.
    
#if FOR_MACOSX
      // Reasons for never using native dialog on OSX:
      //
      // 1. There doesn't seem to be a way to remove filtered-out files. They are drawn in a grayer color though, but that's just messy.
      // 2. GUI Freezes when calling show(). (exec() works though, strangely enough)
      //
      setOption(QFileDialog::DontUseNativeDialog, true);
#else
      setOption(QFileDialog::DontUseNativeDialog, useNativeFileRequesters());
#endif

      if (several_files)
        setFileMode(QFileDialog::ExistingFiles);
      
      if (for_loading)
        setAcceptMode(QFileDialog::AcceptOpen);
      else
        setAcceptMode(QFileDialog::AcceptSave);

      setProxyModel(new FileFilterProxyModel());

      if (!g_filedialog_geometry.isEmpty()){
        restoreGeometry(g_filedialog_geometry);
      }
      
      fixqfiledialog(this);

      if (!g_has_resized_filedialog){
        g_has_resized_filedialog = true;
        resize(width()*g_gfx_scale*0.72, height()*g_gfx_scale*0.72);
      }
    }

    ~FileRequester(){
      g_filedialog_geometry = saveGeometry();
    }
    
    static QString get_postfixes_filter(QString type, QString postfixes){
      QString postfixes2 = postfixes==NULL ? "*.rad *.mmd *.mmd2 *.mmd3 *.MMD *.MMD2 *.MMD3" : QString(postfixes);
      
#if defined(FOR_WINDOWS)
      return postfixes2 + " ;; All files (*)";
#else
      type = type==NULL ? "Song files" : type;
      return QString(type) + " (" + postfixes2 + ") ;; All files (*)";
#endif
    }
    
    filepath_t get_filename(bool program_state_is_valid){
      QString ret;
      connect(this, &QFileDialog::fileSelected, this, [&ret](const QString &filename)
              {
                ret = filename; // There is a function in QFileDialog called selectedFiles(), but the documentation for it is cryptic and contains no examples. This is easier.
              }
              );
      safeExec(this, program_state_is_valid);
      return make_filepath(ret);
    }

  };
  
}
