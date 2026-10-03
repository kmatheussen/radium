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

#pragma once

#include <QString>
#include <QVector>

namespace radium
{

QString fuzzy_search_fold(const QString &text);

class FuzzySearchQuery
{
public:

	explicit FuzzySearchQuery(const QString &query);

	bool empty(void) const;

	bool matches(const QString &folded_text) const;

	int score(const QString &folded_text) const;

private:

	QVector<QString> _tokens;
};

bool fuzzy_search_match(const QString &text, const QString &query);

int fuzzy_search_score(const QString &text, const QString &query);

} // namespace radium
