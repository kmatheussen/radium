/* Copyright 2016 Kjetil S. Matheussen

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


#include <QDateTime>


#define g_diagnostic_reports_path (QString(getenv("HOME")) + "/Library/Logs/DiagnosticReports")
static const QString g_starts_with = "Radium_";
static const QString g_ends_with = ".crash";

/*
todo, also check here:
/Libraries/Logs/DiagnosticReports
/Libraries/Logs/CrashReporter
*/

static QString get_latest_diagnostic_report(void){

  QDir dir(g_diagnostic_reports_path);
  dir.setSorting(QDir::Time);

  QString ret="";
  
  QDateTime start_time = QDateTime::currentDateTime();
  QDateTime start_time_plus_xx_seconds = QDateTime(start_time).addSecs(5); // we only wait 5 seconds for the file to be created

  QDateTime start_time_minus_xx_seconds = QDateTime(start_time).addSecs(-60);

  while(QDateTime::currentDateTime() < start_time_plus_xx_seconds) {

    QFileInfoList list = dir.entryInfoList(QDir::AllEntries|QDir::NoDotAndDotDot);

    if (list.count() == 0)
      return "(Diagnostic dir empty or not found: "+g_diagnostic_reports_path+". Exists: " + (dir.exists()?"Yes":"No") + ", Readable: " + (dir.isReadable()?"Yes":"No") + ")";

    QString ret = " ";
    
    for (int i = 0 ; i<list.count();i++){
      QFileInfo file_info = list[i];

      //ret += file_info.absoluteFilePath() + "\n";
      
      if (file_info.isDir())
        continue;

      //ret += "1\n";
      
      if (file_info.size() < 100)
        continue;

      //ret += "2\n";
      
      QString file_name = file_info.fileName();

      if (!file_name.startsWith(g_starts_with))
        continue;

      //ret += "3\n";
      
      if (!file_name.endsWith(g_ends_with))
        continue;

      //ret += "4\n";

      QDateTime file_time = file_info.lastModified();

      //printf("file_name: %s. Time: %s. Minus60: %s\n", file_name.toUtf8().constData(), file_time.toString("hh:mm:ss.zzz").toUtf8().constData(), start_time_minus_60_seconds.toString("hh:mm:ss.zzz").toUtf8().constData());

      if (file_time < start_time_minus_xx_seconds)
        continue;

      //ret += "5\n";
      
      //if (file_time > now)
      //  continue;

      QString file_path = file_info.absoluteFilePath();
      
      return file_to_string(file_path);    
    }

    QThread::msleep(5000);
  }

  return "(Unable to find diagnostics)"+ret;
}

