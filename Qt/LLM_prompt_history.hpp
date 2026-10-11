#pragma once
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

#include <QJsonArray>
#include <QJsonDocument>
#include <QString>
#include <QStringList>

#include "../common/settings_proc.h"
#include "../common/vector_proc.h"


namespace radium
{
namespace llm
{

// How many prompts a prompt history (persisted across sessions) remembers.
static constexpr int LLM_PROMPT_HISTORY_MAX_SIZE = 50;

// The config file parser truncates every line at the first '#' (unless the
// line contains "color"), and drops lines longer than 16 KB, so both limit
// how much of the serialized history can be stored in the settings.
static constexpr int LLM_PROMPT_HISTORY_MAX_SETTINGS_LINE_SIZE = 15*1024;

// Parses the JSON array format used to store a history. An empty list is
// returned if the string is not a valid JSON array.
static inline QStringList parse_prompt_history(const QString &raw)
{
	QStringList history;

	if (!raw.isEmpty())
	{
		const QJsonDocument doc = QJsonDocument::fromJson(raw.toUtf8());
		if (doc.isArray())
		{
			const QJsonArray array = doc.array();
			for (const QJsonValue &value : array)
				if (value.isString())
					history.append(value.toString());
		}
	}

	return history;
}

// The prompt history is stored in the settings as a compact JSON array of
// strings so that it survives across sessions.
static inline QStringList load_prompt_history(const char *key)
{
	QStringList history = parse_prompt_history(SETTINGS_read_string(key, ""));

	if (history.isEmpty())
	{
		// Older versions did not escape '#' when saving, and the config
		// reader truncates a line at the first '#', so such histories come
		// back as broken JSON. Read the raw config line to recover them.
		const QString key_string = QString::fromUtf8(key);
		vector_t *lines = SETTINGS_get_all_lines_starting_with(key);

		VECTOR_FOR_EACH(const char *, line_c, lines){
			const QString line = QString(line_c).trimmed();
			if (line.startsWith(key_string)) {
				QString value = line.mid(key_string.length()).trimmed();
				if (value.startsWith("=")) { // [NO_IF_=_WARNING]
					history = parse_prompt_history(value.remove(0, 1).trimmed());
					break;
				}
			}
		}END_VECTOR_FOR_EACH;
	}

	while (history.size() > LLM_PROMPT_HISTORY_MAX_SIZE)
		history.removeFirst();

	return history;
}

static inline void save_prompt_history(const char *key, const QStringList &history)
{
	QStringList bounded_history = history;
	while (bounded_history.size() > LLM_PROMPT_HISTORY_MAX_SIZE)
		bounded_history.removeFirst();

	// SETTINGS_get_value() truncates every config line at the first '#'
	// (unless the line contains "color"), which would corrupt the JSON and
	// make the whole history unreadable. Escape '#' as its JSON unicode
	// escape; the JSON parser decodes it back to '#' on load.
	auto serialize = [](const QStringList &list) -> QString
	{
		QString json = QString(QJsonDocument(QJsonArray::fromStringList(list)).toJson(QJsonDocument::Compact));
		json.replace("#", "\\u0023");
		return json;
	};

	QString json = serialize(bounded_history);

	// get_lines2() discards config lines longer than 16 KB, which would
	// silently lose the whole history, so drop the oldest entries until the
	// serialized history fits.
	while (!bounded_history.isEmpty() && json.length() > LLM_PROMPT_HISTORY_MAX_SETTINGS_LINE_SIZE)
	{
		bounded_history.removeFirst();
		json = serialize(bounded_history);
	}

	SETTINGS_write_string(key, json.toUtf8().constData());
}

} // namespace llm
} // namespace radium
