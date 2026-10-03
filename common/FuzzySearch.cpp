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

#include "FuzzySearch.h"

namespace radium{

static bool is_word_boundary_char(QChar c)
{
	switch(c.unicode())
	{
		case ' ':
		case '\t':
		case '-':
		case '_':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		case '/':
		case '\\':
		case '.':
		case ',':
		case ':':
		case ';':
			return true;
		default:
			return false;
	}
}

static int fold_latin_extended_a(int codepoint)
{
	if (codepoint == 0x130)
		return 'i';

	if (codepoint == 0x178)
		return 0xFF;

	if ((codepoint >= 0x100 && codepoint <= 0x137 && (codepoint & 1) == 0)
			|| (codepoint >= 0x139 && codepoint <= 0x147 && (codepoint & 1) == 1)
			|| (codepoint >= 0x14A && codepoint <= 0x177 && (codepoint & 1) == 0)
			|| (codepoint >= 0x179 && codepoint <= 0x17D && (codepoint & 1) == 1))
		return codepoint + 1;

	return codepoint;
}

QString fuzzy_search_fold(const QString &text)
{
	QString ret;
	ret.reserve(text.size());

	for (QChar c : text)
	{
		const int codepoint = c.unicode();

		if (codepoint >= 'A' && codepoint <= 'Z')
		{
			ret.append(QChar(codepoint + ('a' - 'A')));
			continue;
		}

		if ((codepoint >= 0xC0 && codepoint <= 0xD6) || (codepoint >= 0xD8 && codepoint <= 0xDE))
		{
			ret.append(QChar(codepoint + 0x20));
			continue;
		}

		ret.append(QChar(fold_latin_extended_a(codepoint)));
	}

	return ret;
}

static bool match_token(const QString &text, const QString &token)
{
	qsizetype text_pos = 0;

	for (qsizetype token_pos = 0 ; token_pos < token.size() ; token_pos++)
	{
		while (text_pos < text.size() && text[text_pos] != token[token_pos])
			text_pos++;

		if (text_pos == text.size())
			return false;

		text_pos++;
	}

	return true;
}

static int score_token(const QString &text, const QString &token)
{
	if (token.isEmpty())
		return 0;

	int best = -1;

	for (qsizetype start = 0 ; start + token.size() <= text.size() ; start++)
	{
		if (text[start] != token[0])
			continue;

		int score = 0;
		qsizetype last_match = -2;
		qsizetype text_pos = start;
		qsizetype token_pos = 0;

		while (token_pos < token.size())
		{
			while (text_pos < text.size() && text[text_pos] != token[token_pos])
				text_pos++;

			if (text_pos == text.size())
				break;

			score += 1;

			if (last_match == text_pos - 1)
				score += 4;

			if (text_pos == 0 || is_word_boundary_char(text[text_pos-1]))
				score += 8;

			last_match = text_pos;
			text_pos++;
			token_pos++;
		}

		if (token_pos == token.size())
		{
			if (start == 0)
				score += 16;

			if (score > best)
				best = score;
		}
	}

	return best;
}

FuzzySearchQuery::FuzzySearchQuery(const QString &query)
{
	const QString folded = fuzzy_search_fold(query);

	qsizetype pos = 0;

	while (pos < folded.size())
	{
		while (pos < folded.size() && folded[pos].isSpace())
			pos++;

		const qsizetype start = pos;

		while (pos < folded.size() && !folded[pos].isSpace())
			pos++;

		if (pos > start)
			_tokens.push_back(folded.mid(start, pos-start));
	}
}

bool FuzzySearchQuery::empty(void) const
{
	return _tokens.isEmpty();
}

bool FuzzySearchQuery::matches(const QString &folded_text) const
{
	for (const QString &token : _tokens)
		if (!match_token(folded_text, token))
			return false;

	return true;
}

int FuzzySearchQuery::score(const QString &folded_text) const
{
	int ret = 0;

	for (const QString &token : _tokens)
	{
		const int token_score = score_token(folded_text, token);

		if (token_score < 0)
			return -1;

		ret += token_score;
	}

	return ret;
}

bool fuzzy_search_match(const QString &text, const QString &query)
{
	return FuzzySearchQuery(query).matches(fuzzy_search_fold(text));
}

int fuzzy_search_score(const QString &text, const QString &query)
{
	return FuzzySearchQuery(query).score(fuzzy_search_fold(text));
}

}
