#include <QByteArray>
#include <QFileDialog>
#include <QSortFilterProxyModel>
#include <QFileSystemModel>


extern QByteArray g_filedialog_geometry;
  
namespace radium{

  struct FileRequester : public QFileDialog {

    class FileFilterProxyModel : public QSortFilterProxyModel
    {
    protected:
      bool filterAcceptsRow(int source_row, const QModelIndex& source_parent) const override{
        QFileSystemModel* fileModel = qobject_cast<QFileSystemModel*>(sourceModel());
        fileModel->setNameFilterDisables(false);
        return QSortFilterProxyModel::filterAcceptsRow(source_row, source_parent);
      }

      void sort(int column, Qt::SortOrder order = Qt::AscendingOrder) override{
        QFileSystemModel* fileModel = qobject_cast<QFileSystemModel*>(sourceModel());
        fileModel->sort(column, order);
      }
    };
    
  public:
    
    FileRequester(QWidget *parent, QString header_text, QString dir, QString filetypename, QString postfixes, bool for_loading)
      : QFileDialog(parent, header_text, dir, FileRequester::get_postfixes_filter(filetypename, postfixes))
    {
      /*
      setWindowTitle(header_text);
      setDirectory(dir);
      setSupportedSchemes(FileRequester::get_postfixes_filter(filetypename, postfixes).split(";;"));
      */

      setOption(QFileDialog::DontUseCustomDirectoryIcons, true); // Sometimes make windows crawl if I remember correctly.
    
#if FOR_MACOSX
      printf("          WORKAROUND: Always set DontUseNativeDialog on OSX to avoid partly GUI freeze.\n");
      setOption(QFileDialog::DontUseNativeDialog, true);
#else
      setOption(QFileDialog::DontUseNativeDialog, useNativeFileRequesters());
#endif

      if (for_loading)
        setAcceptMode(QFileDialog::AcceptOpen);
      else
        setAcceptMode(QFileDialog::AcceptSave);

      setProxyModel(new FileFilterProxyModel());

      if (!g_filedialog_geometry.isEmpty()){
        restoreGeometry(g_filedialog_geometry);
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
    
    QString get_filename(bool program_state_is_valid){
      QString ret;
      connect(this, &QFileDialog::fileSelected, this, [this, &ret](const QString &filename)
              {
                ret = filename; // There is a function in QFileDialog called selectedFiles(), but the documentation for it is cryptic and contains no examples. This is easier.
              }
              );
      safeExec(this, program_state_is_valid);
      return ret;
    }

  };
  
}