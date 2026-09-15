/* Copyright 2026 Kjetil S. Matheussen

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

#include "radium_env.hpp"

#include "../common/settings_proc.h"
#include "../common/OS_settings_proc.h"

#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QTextStream>
#include <QVector>

#include "../bin/packages/faust/architecture/faust/gui/GUI.h"

std::list<GUI*> GUI::fGuiList;

static QString g_program_dir;

QString FAUST_LLM_TEST_get_program_dir(void)
{
	if (!g_program_dir.isEmpty())
	  return g_program_dir;

	const char *env_dir = getenv("RADIUM_PROGRAM_DIR");
	if (env_dir != NULL && env_dir[0] != '\0')
	{
		g_program_dir = QString::fromUtf8(env_dir);
		return g_program_dir;
	}

	const QString app_dir = QCoreApplication::applicationDirPath();
	const QString bin_dir = QDir::cleanPath(app_dir + QString::fromUtf8("/../bin"));
	if (QFileInfo::exists(bin_dir + QString::fromUtf8("/packages/faust/libraries/stdfaust.lib")))
	  g_program_dir = bin_dir;
	else
	  g_program_dir = app_dir;

	return g_program_dir;
}

QString OS_get_full_program_file_path2(QString filename)
{
	const QFileInfo info(QDir(FAUST_LLM_TEST_get_program_dir()), filename);
	if (!info.exists())
	  fprintf(stderr, "faust_llm_test: warning: Radium program file not found: %s\n",
	          info.absoluteFilePath().toUtf8().constData());
	return info.absoluteFilePath();
}

static QString conf_filename(const char *filename)
{
	const QString dot_radium = QDir::homePath() + QString::fromUtf8("/.radium/") + QString::fromUtf8(filename);
	if (QFileInfo::exists(dot_radium))
	  return dot_radium;
	return FAUST_LLM_TEST_get_program_dir() + QString::fromUtf8("/") + QString::fromUtf8(filename);
}

bool OS_has_conf_filename2(const char *filename)
{
	return QFileInfo::exists(conf_filename(filename));
}

filepath_t OS_get_conf_filename2(const char *filename)
{
	static std::deque<std::wstring> storage;
	storage.push_back(conf_filename(filename).toStdWString());
	return make_filepath(storage.back().c_str());
}

const wchar_t *STRING_create(const char *s)
{
	static std::deque<std::wstring> storage;
	storage.push_back(QString::fromUtf8(s).toStdWString());
	return storage.back().c_str();
}

const wchar_t *STRING_create(const QString s, bool use_gc_alloc)
{
	static std::deque<std::wstring> storage;
	storage.push_back(s.toStdWString());
	return storage.back().c_str();
}

const wchar_t *STRING_create2(const QString s)
{
	return STRING_create(s, false);
}

static QString settings_path(void)
{
	return QDir::homePath() + QString::fromUtf8("/.radium/config");
}

static QStringList read_settings_lines(void)
{
	QStringList ret;

	QFile file(settings_path());
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	  return ret;

	QTextStream in(&file);
	while (!in.atEnd())
	{
		QString line = in.readLine();
		if (!line.contains("#") && !line.contains("="))
		  line = "";
		else if (line.contains("#") && !line.contains("="))
		  line.remove(0, line.indexOf("#"));
		ret.push_back(line);
	}

	return ret;
}

static bool line_has_key(const QString &key, const QString &line)
{
	QString string = line.trimmed();
	if (!string.startsWith(key))
	  return false;
	string = string.remove(0, key.length()).trimmed();
	return string.startsWith("=");
}

static QString get_value(const QString &line)
{
	QString l = line;
	if (l.contains("#") && !l.contains("color"))
	  l.truncate(l.indexOf("#"));
	const int pos = l.indexOf("=");
	if (pos == -1)
	  return QString();
	return l.remove(0, pos + 1).trimmed();
}

static bool settings_find(const char *key, QString *out)
{
	const QString qkey = QString::fromUtf8(key);
	const QStringList lines = read_settings_lines();
	for (const QString &line : lines)
	{
		if (line_has_key(qkey, line))
		{
			*out = get_value(line);
			return true;
		}
	}
	return false;
}

bool SETTINGS_has_key(const char *key)
{
	QString value;
	return settings_find(key, &value);
}

QString SETTINGS_read_qstring(const char *key, QString def)
{
	QString value;
	if (settings_find(key, &value))
	  return value;
	return def;
}

int64_t SETTINGS_read_int(const char *key, int64_t def)
{
	QString value;
	if (settings_find(key, &value))
	  return (int64_t)value.toLongLong();
	return def;
}

bool SETTINGS_read_bool(const char *key, bool def)
{
	QString value;
	if (settings_find(key, &value))
	  return value.startsWith('t');
	return def;
}

void SETTINGS_write_string(const char *key, QString val)
{
	QStringList lines = read_settings_lines();
	if (lines.isEmpty())
	  return;

	const QString qkey = QString::fromUtf8(key);
	const QString new_line = qkey + QString::fromUtf8(" = ")
	  + val.replace("\r\n", QString::fromUtf8("<____________LS____________>"))
	       .replace("\n", QString::fromUtf8("<____________LS____________>"));

	bool found = false;
	for (QString &line : lines)
	{
		if (line_has_key(qkey, line))
		{
			line = new_line;
			found = true;
			break;
		}
	}
	if (!found)
	  lines.push_back(new_line);

	QFile file(settings_path());
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate))
	  return;
	QTextStream out(&file);
	for (const QString &line : lines)
	  out << line << "\n";
}
