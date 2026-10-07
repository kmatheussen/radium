#pragma once

#include <mutex>

#include <QByteArray>
#include <QCryptographicHash>
#include <QDir>
#include <QFile>
#include <QHash>
#include <QList>
#include <QString>
#include <QStringList>

namespace radium
{

inline QString faust_find_platform_lib(const QStringList &options, const QString &radium_path)
{
	for (int i = options.size() - 2 ; i >= 0 ; i--)
	{
		if (options[i] != "-I")
			continue;

		QString dir = options[i + 1];
		dir.replace("%radium_path%", radium_path);
		QString candidate = QDir(dir).filePath("platform.lib");
		if (QFile::exists(candidate))
			return candidate;
	}

	if (!radium_path.isEmpty())
	{
		QString candidate = QDir(radium_path + "/packages/faust/libraries").filePath("platform.lib");
		if (QFile::exists(candidate))
			return candidate;
	}

	return QString();
}

inline QString faust_get_sample_rate_overlay_dir(
	const QStringList &options,
	const QString &radium_path,
	int sample_rate,
	QString &error)
{
	if (sample_rate <= 0)
		return QString();

	static std::mutex mutex;
	static QHash<QString, QString> cache;

	std::lock_guard<std::mutex> lock(mutex);

	QString key = QString("%1|%2|%3").arg(sample_rate).arg(radium_path, options.join(QChar('\n')));
	QHash<QString, QString>::const_iterator cached = cache.find(key);
	if (cached != cache.end())
		return cached.value();

	QString source = faust_find_platform_lib(options, radium_path);
	if (source.isEmpty())
	{
		error = "Could not find platform.lib";
		return QString();
	}

	QFile in(source);
	if (!in.open(QIODevice::ReadOnly))
	{
		error = QString("Could not open %1").arg(source);
		return QString();
	}
	const QByteArray content = in.readAll();
	in.close();

	QList<QByteArray> lines = content.split('\n');
	bool replaced = false;
	for (QByteArray &line : lines)
	{
		if (!replaced && line.startsWith("SR = "))
		{
			line = QString("SR = %1;").arg(sample_rate).toUtf8();
			replaced = true;
		}
	}

	if (!replaced)
	{
		error = QString("Could not find an 'SR = ' line in %1").arg(source);
		return QString();
	}

	QString hash = QString::fromUtf8(QCryptographicHash::hash(content, QCryptographicHash::Sha1).toHex().left(8));
	QString overlay_dir = QDir(QDir::tempPath()).filePath(QString("radium_faust_sr_%1_%2").arg(sample_rate).arg(hash));
	if (!QDir().mkpath(overlay_dir))
	{
		error = QString("Could not create %1").arg(overlay_dir);
		return QString();
	}

	QString target = QDir(overlay_dir).filePath("platform.lib");
	QString temp = target + QString::fromUtf8(".tmp");
	QFile out(temp);
	if (!out.open(QIODevice::WriteOnly | QIODevice::Truncate))
	{
		error = QString("Could not write %1").arg(temp);
		return QString();
	}
	out.write(lines.join('\n'));
	out.close();

	QFile::remove(target);
	if (!QFile::rename(temp, target))
	{
		QFile::remove(temp);
		error = QString("Could not rename %1 to %2").arg(temp).arg(target);
		return QString();
	}

	cache[key] = overlay_dir;
	return overlay_dir;
}

} // namespace radium
