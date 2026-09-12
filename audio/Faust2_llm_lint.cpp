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

#include <math.h>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#if defined(__clang__)
#  pragma GCC diagnostic ignored "-Wcast-function-type-mismatch"
#else
#  pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"

#include "../bin/packages/faust/architecture/faust/dsp/dsp.h"
#include "../bin/packages/faust/architecture/faust/dsp/interpreter-dsp.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust/compiler/generator/libfaust.h"
#undef uchar
#undef uint

#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#pragma GCC diagnostic pop

#include <QCoreApplication>
#include <QHash>
#include <QList>
#include <QPair>
#include <QRegularExpression>
#include <QSet>
#include <QString>
#include <QStringList>

#include "../common/ArgsCreator.hpp"

#include "Faust2_llm_lint.hpp"

// Static analysis used by the LLM auto-fix loop.
//
// When a generated program fails to compile, the widget asks the LLM to fix
// it. Faust's compiler error for arity/composition mistakes is a multi-KB
// dump of the inlined signal graph without source locations, so the model
// cannot localize the bug (it has failed whole sessions rewriting the wrong
// code). To give it exact lines instead, each top-level definition of the
// failing program is compiled in isolation here: the expression is wrapped
// in its own tiny program, compiled with the fast interpreter backend, and
// the real compiler's type system then tells us whether the expression
// itself is well-formed and whether it has unbound audio inputs (the
// signature of "a filter/smoother was used as a plain value instead of
// being applied to a signal with ':'").
//=====================================================

namespace
{

struct Faust2LintDef
{
	QString name;
	QString rhs;
	int line;
	QStringList args; // parameter names when the definition is a function: 'name(a, b) = ...'
};

// Masks strings and comments in one line so delimiter counting below never
// sees them. 'in_block_comment' carries a /* */ comment across lines.
QString faust2_lint_mask_line(const QString &line, bool &in_block_comment)
{
	QString masked = line;
	const int len = masked.size();

	for (int i = 0; i < len; i++)
	{
		const QChar c = masked.at(i);

		if (in_block_comment)
		{
			if (c == '*' && i + 1 < len && masked.at(i + 1) == '/')
			{
				masked[i] = masked[i + 1] = ' ';
				i++;
				in_block_comment = false;
			}
			else
				masked[i] = ' ';
		}
		else if (c == '/' && i + 1 < len && masked.at(i + 1) == '/')
		{
			while (i < len)
			{
				masked[i] = ' ';
				i++;
			}
		}
		else if (c == '/' && i + 1 < len && masked.at(i + 1) == '*')
		{
			masked[i] = masked[i + 1] = ' ';
			i++;
			in_block_comment = true;
		}
		else if (c == '"')
		{
			masked[i] = ' ';
			i++;
			while (i < len && masked.at(i) != '"')
			{
				if (masked.at(i) == '\\')
				{
					masked[i] = ' ';
					i++;
				}
				masked[i] = ' ';
				i++;
			}
		}
	}

	return masked;
}

// Collects the top-level "name = ...;" definitions of 'code'. Function-style
// definitions ("name(a, b) = ...;") are collected with their parameter names,
// and their entire 'with { }' block belongs to their RHS (so with-block
// locals are NOT collected as separate definitions - they are not top-level
// and compiling them standalone can only produce bogus 'undefined' findings).
// The RHS may span several lines and ends at the first ';' outside of any
// bracket. Assumes at most one definition per line.
QList<Faust2LintDef> faust2_lint_collect_defs(const QString &code)
{
	QList<Faust2LintDef> defs;
	const QStringList lines = code.split('\n');
	const QRegularExpression def_re(QStringLiteral("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*(\\(\\s*([^)]*)\\s*\\))?\\s*=(.*)$"));

	bool in_block_comment = false;
	bool collecting = false;
	QString def_name;
	QString def_rhs;
	QStringList def_args;
	int def_line = 0;
	int depth = 0; // delimiters inside the statement being collected

	for (int i = 0; i < lines.size(); i++)
	{
		const QString line_text = lines.at(i);
		const QString masked = faust2_lint_mask_line(line_text, in_block_comment);

		if (!collecting)
		{
			// Match on the ORIGINAL line (the RHS must keep its string
			// literals and comments intact), but require that the matched
			// name is real code: the masked line has everything inside
			// strings/comments blanked, so if the name position was blanked
			// the "definition" is only text inside a string or comment.
			const QRegularExpressionMatch m = def_re.match(line_text);
			if (m.hasMatch()
			    && m.captured(1) != "declare"
			    && m.captured(1) != "import"
			    && m.capturedStart(1) < masked.size()
			    && masked.at(m.capturedStart(1)) != ' ')
			{
				def_name = m.captured(1);
				def_rhs = m.captured(4);
				def_args.clear();
				for (const QString &arg : m.captured(3).split(",", Qt::SkipEmptyParts))
				{
					const QString trimmed = arg.trimmed();
					if (!trimmed.isEmpty())
					  def_args.append(trimmed);
				}
				def_line = i + 1;
				depth = 0;
				collecting = true;
			}
		}
		else
			def_rhs += "\n" + line_text;

		if (collecting)
		{
			bool terminated = false;
			int terminator_pos = -1; // index of the terminating ';' in the current (masked) line
			for (int pos = 0; pos < masked.size(); pos++)
			{
				const QChar ch = masked.at(pos);
				if (ch == '(' || ch == '{' || ch == '[')
				  depth++;
				else if (ch == ')' || ch == '}' || ch == ']')
				  depth--;
				else if (ch == ';' && depth <= 0)
				{
					terminated = true;
					terminator_pos = pos;
					break;
				}
			}

			if (terminated)
			{
				// Cut the RHS at the terminating ';' (the same position in
				// the original text; anything after it on that line belongs
				// to the next statement and is ignored, since one definition
				// per line is assumed). The synthetic program adds its own
				// ';' after the RHS.
				const int cut = def_rhs.size() - (line_text.size() - terminator_pos);
				if (cut > 0 && cut < def_rhs.size())
				  def_rhs = def_rhs.left(cut);

				Faust2LintDef d;
				d.name = def_name;
				d.rhs = def_rhs.trimmed();
				d.line = def_line;
				d.args = def_args;
				defs.append(d);

				def_name.clear();
				def_rhs.clear();
				def_args.clear();
				collecting = false;
				depth = 0;
			}
		}
	}

	return defs;
}

// Replaces the content of every string literal with an empty string, so the
// name substitution below can never match (and thereby corrupt) text inside
// strings. String contents are irrelevant for the arity check. The
// placeholder must NOT be an identifier: an earlier version replaced string
// contents with "x", and when the program defined 'x = _,' (the effect
// input binding), the \bx\b substitution matched INSIDE the sanitized
// string literals and injected 'hslider("_l0", ...)' into them, which made
// every innocent slider def fail its isolated compile with
// 'syntax error, unexpected IDENT, expecting PAR'.
QString faust2_lint_sanitize_strings(const QString &rhs)
{
	QString out;
	out.reserve(rhs.size());
	const int len = rhs.size();

	for (int i = 0; i < len; i++)
	{
		const QChar c = rhs.at(i);
		if (c != '"')
		{
			out.append(c);
			continue;
		}

		i++;
		while (i < len && rhs.at(i) != '"')
		{
			if (rhs.at(i) == '\\')
			  i++;
			i++;
		}

		out.append("\"\"");
	}

	return out;
}

// Returns the index of the '(' that opens the parenthesized group whose
// ')' is the last character before 'before_pos' (the group's content may
// itself contain parenthesized sub-expressions, e.g.
// '(dry : par(i, 2, ...), wet : par(...))'). '*close_pos' receives the
// index of the group's closing ')' (the first ')' met in the backward
// scan). Returns -1 (with *close_pos = -1) when 'before_pos' is not
// preceded by a closing ')'.
static int faust2_lint_find_open_paren(const QString &text, int before_pos, int *close_pos)
{
	*close_pos = -1;
	int depth = 0;
	for (int i = before_pos - 1; i >= 0; i--)
	{
		const QChar ch = text.at(i);
		if (ch == ')')
		{
			if (depth == 0)
			  *close_pos = i;
			depth++;
		}
		else if (ch == '(')
		{
			depth--;
			if (depth == 0)
			  return i;
		}
	}
	*close_pos = -1;
	return -1;
}

// Splits a parenthesized group's content on top-level commas only: commas
// inside nested parentheses (function-call arguments, sub-expressions) do
// not split.
static QStringList faust2_lint_split_tuple(const QString &content)
{
	QStringList members;
	int depth = 0;
	int start = 0;
	for (int i = 0; i < content.size(); i++)
	{
		const QChar ch = content.at(i);
		if (ch == '(')
		  depth++;
		else if (ch == ')')
		  depth--;
		else if (ch == ',' && depth == 0)
		{
			members.append(content.mid(start, i - start));
			start = i + 1;
		}
	}
	members.append(content.mid(start));
	return members;
}

// Returns 'text' with the argument lists of function calls replaced by
// spaces (the call name and the parentheses are kept). A 'name, name'
// inside a call - function ARGUMENTS, e.g.
// 'fi.resonlp(filter_freq, filter_q, 1)' - is then invisible to the
// tuple heuristics, while a real parallel-composition pair like
// '(dry, wet)' still matches (its parentheses are not preceded by an
// identifier). Without this, any definition using a multi-argument
// function call is mistaken for a tuple and wrongly excluded from the
// mono classification (observed: 'dry' was classified non-mono because
// of 'fi.resonlp(filter_freq, filter_q, 1)', hiding the mono-into-par
// bug from the lint checks). Only parentheses whose previous non-space
// character ends an identifier (optionally 'mod.name') count as calls.
static QString faust2_lint_mask_call_args(const QString &text)
{
	QString out = text;
	const int len = out.size();

	for (int i = 0; i < len; i++)
	{
		if (out.at(i) != '(')
		  continue;

		// A function-call paren is preceded by an identifier (optionally
		// 'mod.name').
		bool is_call = false;
		int j = i - 1;
		while (j >= 0 && out.at(j).isSpace())
		  j--;
		if (j >= 0 && (out.at(j).isLetterOrNumber() || out.at(j) == '_'))
		{
			const int name_end = j;
			while (j >= 0 && (out.at(j).isLetterOrNumber() || out.at(j) == '_' || out.at(j) == '.'))
			  j--;
			const QString name = out.mid(j + 1, name_end - j);
			if (QRegularExpression(QStringLiteral("^[a-zA-Z_][a-zA-Z0-9_]*\\.[a-zA-Z_][a-zA-Z0-9_]*$|^[a-zA-Z_][a-zA-Z0-9_]*$")).match(name).hasMatch()) // [NO_IF_=_WARNING]
			  is_call = true;
		}

		if (!is_call)
		  continue;

		int depth = 1;
		int k = i + 1;
		for (; k < len && depth > 0; k++)
		{
			if (out.at(k) == '(')
			  depth++;
			else if (out.at(k) == ')')
			  depth--;
		}
		if (depth != 0)
		  continue;

		for (int m = i + 1; m < k - 1; m++)
		  out[m] = ' ';
	}

	return out;
}

// Returns the output channel count if 'rhs' is a 'soundfile("...", N)'
// declaration (N is parsed from the trailing numeric argument of the
// sanitized rhs), else -1. Used to substitute references to soundfile
// definitions with type-correct placeholders.
int faust2_lint_soundfile_channels(const QString &rhs)
{
	const QString sanitized = faust2_lint_sanitize_strings(rhs);
	const QRegularExpression re(QStringLiteral("\\bsoundfile\\s*\\([^()]*,\\s*(\\d+)\\s*\\)"));
	const QRegularExpressionMatch m = re.match(sanitized);
	if (!m.hasMatch())
	  return -1;
	return m.captured(1).toInt();
}

// True if the (sanitized) rhs is a soundfile declaration.
bool faust2_lint_is_soundfile_decl(const QString &rhs)
{
	return faust2_lint_sanitize_strings(rhs).contains(QStringLiteral("soundfile"));
}

// Replaces every reference to another top-level definition in 'rhs' with a
// 0-input hslider placeholder. All replaced names refer to 0-input
// definitions (in a valid instrument every definition is 0-input, and a
// definition with an unbound input is flagged when its own line is
// checked), so the substitution preserves the input arity of the
// expression. Names in 'soundfile_channels' are replaced by a real
// soundfile placeholder with the same channel count instead: so.sound()
// and friends require an actual soundfile signal, and the channel count
// matters for arity (a stereo file produces 2 outputs, a mono hslider
// only 1). Names in 'def_channels' are multi-channel definitions (input
// bindings like 'x = _,_' and heuristic-stereo names like
// 'dry = x : par(i, 2, ...)') and are replaced by a TUPLE of sliders with
// the same number of channels - replacing a 2-channel name with a mono
// hslider made every par(i, 2, ...)/interleave consumer fail its isolated
// compile with a bogus arity error.
QString faust2_lint_substitute(const QString &rhs,
                               const QString &self_name,
                               const QSet<QString> &all_names,
                               const QHash<QString, int> &soundfile_channels,
                               const QHash<QString, int> &arg_counts,
                               const QHash<QString, int> &def_channels)
{
	QString out = faust2_lint_sanitize_strings(rhs);
	int placeholder = 0;

	for (const QString &name : all_names)
	{
		if (name == self_name)
		  continue; // recursive references are left as-is

		const int nargs = arg_counts.value(name, 0);

		// Word-bounded and not adjacent to '.', so module-qualified names
		// like 'ma.SR' or 'si.smooth' and partial identifiers like 'freq'
		// inside 'vib_freq' are never touched. The leading-dot guard
		// matters as much as the trailing one: a definition named
		// 'smooth' must NOT replace the 'smooth' inside 'si.smooth'
		// (observed: it produced 'si.hslider(...)' and bogus 'does not
		// compile' findings for every slider def).
		const QRegularExpression re(QStringLiteral("(?<!\\.)\\b%1\\b(?!\\.)").arg(name));

		if (nargs > 0)
		{
			// 'name' is a FUNCTION ('name(a, b) = ...'). A call
			// 'name(expr)' is equivalent to 'expr : name', so with 'name'
			// replaced by an identity box it is equivalent to 'expr' -
			// arity-safe whatever the call looks like. A bare point-free
			// reference gets an identity lambda with one parameter per
			// formal argument.
			const QRegularExpression call_re(QStringLiteral("(?<!\\.)\\b%1\\b\\s*\\(([^()]*)\\)").arg(name));
			out.replace(call_re, QStringLiteral("\\1"));

			QStringList params;
			for (int i = 1; i <= nargs; i++)
			  params.append(QStringLiteral("a%1").arg(i));
			out.replace(re, QStringLiteral("\\(%1).(%2)").arg(params.join(", ")).arg(params[0]));
		}
		else if (soundfile_channels.contains(name))
		{
			// A soundfile-derived name is either passed to so.sound()/so.loop()
			// (which expect the raw soundfile signal with its auxiliary
			// length/rate outputs), or used as a plain N-channel audio signal
			// (e.g. 'wet = dry : pf.flanger_stereo(...)'). In the first case
			// the raw placeholder is correct; in the second the auxiliary
			// outputs must be stripped ((0, 0) binds the implicit part/index
			// inputs so the compiler's interval analysis accepts it).
			const QRegularExpression so_arg_re(QStringLiteral("\\bso\\.[a-zA-Z_]*\\s*\\(\\s*%1\\s*,").arg(name));
			if (so_arg_re.match(out).hasMatch())
			  out.replace(re, QString("soundfile(\"_lint[url:{'_lint.wav'}]\", %1)").arg(soundfile_channels.value(name)));
			else
			  // Parenthesized: the trailing 'si.block(2), si.bus(N)' contains a
			  // top-level comma, which would leak into an enclosing tuple.
			  out.replace(re, QString("((0, 0) : soundfile(\"_lint[url:{'_lint.wav'}]\", %1) : si.block(2), si.bus(%1))").arg(soundfile_channels.value(name)));
		}
		else if (def_channels.contains(name))
		{
			const int channels = def_channels.value(name);
			QStringList sliders;
			for (int i = 0; i < channels; i++)
			  sliders.append(QStringLiteral("hslider(\"_l%1\", 0, 0, 1, 0.01)").arg(placeholder++));
			out.replace(re, channels == 1
			            ? sliders[0]
			            : QStringLiteral("(") + sliders.join(", ") + QStringLiteral(")"));
		}
		else
		  out.replace(re, QString("hslider(\"_l%1\", 0, 0, 1, 0.01)").arg(placeholder++));
	}

	return out;
}

}

// Returns true when 'name' is referenced inside a '<: ... :>' fan-out
// (the canonical filter-bank idiom: '_ <: band1, band2, ... :> _') or as
// the body of an iteration construct ('par(i, 2, name)'). Both are CORRECT
// usages of a value def that has an unbound audio input, so the
// 'plain value' and 'referenced bare' findings must not fire for them
// (observed: an autotune effect whose band1..band8 filters were fanned out
// exactly like the EQ idiom in the system prompt - the lint flagged all
// eight as bugs and the model, told that its correct code was broken,
// refused to fix the real error).
static bool faust2_lint_is_fan_or_iter_body(const QList<Faust2LintDef> &defs,
                                            const QString &name)
{
	for (const Faust2LintDef &user : defs)
	{
		if (user.name == name)
		  continue;
		const QString sanitized = faust2_lint_sanitize_strings(user.rhs);

		// Name inside a '<: ... :>' region of this definition.
		{
			int from = 0;
			while ((from = sanitized.indexOf(QStringLiteral("<:"), from)) >= 0)
			{
				const int end = sanitized.indexOf(QStringLiteral(":>"), from + 2);
				const QString region = end >= 0
				  ? sanitized.mid(from + 2, end - from - 2)
				  : sanitized.mid(from + 2);
				if (QRegularExpression(QStringLiteral("\\b%1\\b").arg(name)).match(region).hasMatch())
				  return true;
				if (end < 0)
				  break;
				from = end + 2;
			}
		}

		// Name as the body of an iteration construct: 'par(i, 2, name)'.
		if (QRegularExpression(QStringLiteral("\\b(?:par|sum|prod|seq)\\s*\\([^()]*(?<!\\.)\\b%1\\b[^()]*\\)").arg(name)).match(sanitized).hasMatch())
		  return true;
	}
	return false;
}

// The LLM auto-fix lint: returns "Line N: ..." findings for the definitions
// of 'code' that are themselves ill-formed, carry unbound audio inputs, or
// reference an undefined symbol.
// Called synchronously on the GUI thread, only after a compile has failed
// (no plugin compile is in flight, and libfaust serializes its factory
// creation through its own global lock anyway). Each check is a small
// interpreter-backend compile, ~10-50 ms.
QStringList FAUST2_lint_faust_code(const QString &code, bool compile_check_safe, const QString &radium_path)
{
	const QList<Faust2LintDef> defs = faust2_lint_collect_defs(code);
	if (defs.isEmpty())
	  return QStringList();

	QSet<QString> all_names;
	for (const Faust2LintDef &def : defs)
	  all_names.insert(def.name);

	// Number of formal parameters per function-style definition
	// ('name(a, b) = ...'); absent = constant definition. The substitution
	// replaces references to function definitions with identity lambdas of
	// matching arity (a bare reference to a 2-parameter function takes 2
	// inputs).
	QHash<QString, int> arg_counts;
	for (const Faust2LintDef &def : defs)
		if (!def.args.isEmpty())
		  arg_counts.insert(def.name, def.args.size());

	// Channel count of input bindings ('x = _;' = 1, 'x = _,_;' = 2):
	// the substitution replaces them with a matching-arity tuple of
	// sliders, because replacing a 2-channel binding with a mono hslider
	// makes every par(i, 2, ...) consumer fail its isolated compile with a
	// bogus 'outputs [1] vs inputs [2]' error.
	QHash<QString, int> binding_channels;
	{
		const QRegularExpression binding_re(QStringLiteral("^\\s*_\\s*(,\\s*_\\s*)*$"));
		for (const Faust2LintDef &def : defs)
		{
			const QRegularExpressionMatch m = binding_re.match(faust2_lint_sanitize_strings(def.rhs).trimmed());
			if (m.hasMatch())
			{
				int count = 0;
				for (const QChar &ch : m.captured(0))
					if (ch == QChar('_'))
					  count++;
				binding_channels.insert(def.name, count);
			}
		}
	}

	// Names that (transitively) compute a signal from the program input:
	// the input bindings themselves, every def with a bare '_' in its RHS
	// (a chain fed directly by the input), and everything referencing one
	// of those. Used to detect delay parameters derived from the input
	// signal (see below).
	QSet<QString> input_derived;
	{
		const QRegularExpression bare_re(QStringLiteral("(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9_])"));
		for (const Faust2LintDef &def : defs)
			if (bare_re.match(faust2_lint_sanitize_strings(def.rhs)).hasMatch())
			  input_derived.insert(def.name);
		for (bool changed = true; changed; )
		{
			changed = false;
			for (const Faust2LintDef &def : defs)
			{
				if (input_derived.contains(def.name))
				  continue;
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
				for (auto it = input_derived.constBegin(); it != input_derived.constEnd(); ++it)
				{
					if (QRegularExpression(QStringLiteral("(?<!\\.)\\b%1\\b(?!\\.)").arg(*it)).match(sanitized).hasMatch())
					{
						input_derived.insert(def.name);
						changed = true;
						break;
					}
				}
			}
		}
	}

	// Soundfile definitions and the definitions derived from them (e.g.
	// 'dry = so.sound(mysf, 0).play_interp(...)') produce N-channel signals.
	// Track them so the substitution uses a type-correct, N-channel
	// placeholder instead of a mono hslider.
	QHash<QString, int> soundfile_channels;
	for (const Faust2LintDef &def : defs)
	{
		const int channels = faust2_lint_soundfile_channels(def.rhs);
		if (channels > 0)
		  soundfile_channels.insert(def.name, channels);
	}
	// Propagate to definitions that reference a soundfile-derived name.
	for (bool changed = true; changed; )
	{
		changed = false;
		for (const Faust2LintDef &def : defs)
		{
			if (soundfile_channels.contains(def.name))
			  continue;
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
			{
				const QRegularExpression re(QStringLiteral("(?<!\\.)\\b%1\\b(?!\\.)").arg(it.key()));
				if (re.match(sanitized).hasMatch())
				{
					soundfile_channels.insert(def.name, it.value());
					changed = true;
					break;
				}
			}
		}
	}

	const QString radium_path_used = radium_path.isEmpty() ? QCoreApplication::applicationDirPath() : radium_path;
	radium::ArgsCreator args;
	args.push_back("-I");
	args.push_back(radium_path_used + "/packages/faust/libraries");

	QStringList findings;

	// The per-definition compile check below creates an interpreter factory
	// per line, and libfaust serializes ALL factory creation through one
	// global lock. The caller decides whether it is safe to run: it must be
	// skipped while any compile is in flight (a pathological program can
	// hold that lock essentially forever - observed: an autotune effect
	// with an.pitchTracker(2048, 512) sent the interpreter evaluator into
	// an unbounded tree expansion, and the GUI froze on the lock while the
	// compile churned).

	// Mask comments and strings before the analyzer test: a COMMENT like
	// "no an.* functions used" must not trigger the guard (observed: the
	// model wrote exactly that comment, the guard matched the comment
	// text, reported a bogus 'uses an.*' finding, and skipped the
	// compile-based lint that would have localized the real error).
	QString masked_code;
	{
		bool in_block_comment = false;
		for (const QString &line : code.split('\n'))
		  masked_code += faust2_lint_mask_line(line, in_block_comment) + "\n";
	}

	if (compile_check_safe && masked_code.contains(QRegularExpression(QStringLiteral("\\ban\\."))))
	{
		compile_check_safe = false;
		findings.append("The program uses an.* analysis functions (an.pitchTracker, an.fft, an.rfft, an.rtocv, ...) which build enormous signal graphs and can make the Faust compiler run for minutes or hang. The per-line compile check is skipped for this program.");
	}

	// Mono effects applied directly to stereo (soundfile-derived) signals:
	// e.g. 'delay = dry : ef.echo(2.0, 0.25, 0.5)' or
	// 'left = dry : de.delay(0.1, mod1)'. The compiler error says
	// "outputs [2] ... must be equal to the number of inputs [3]" and the
	// per-line check flags the line, but nothing says WHY - so name the
	// pattern and give the recipe here. Note: no map-membership skip - a
	// def that references a stereo signal can still apply a mono effect to
	// it wrongly (that is exactly how 'left'/'right' above are broken).
	{
		static const QStringList mono_effects =
		{
			QStringLiteral("ef\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("de\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("fi\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("en\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("co\\.[a-zA-Z_][a-zA-Z0-9_]*"),
			QStringLiteral("pf\\.flanger_mono"),
			QStringLiteral("pf\\.vibrato2_mono"),
			QStringLiteral("pf\\.phaser2_mono"),
		};

		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
			{
				const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*:\\s*(%2)\\s*\\(").arg(it.key()).arg(mono_effects.join("|")));
				const QRegularExpressionMatch m = re.match(sanitized);
				if (m.hasMatch())
				{
					// The de. module takes its signal as the LAST argument
					// (de.delay(n, d, x)); the other mono effects are
					// 1-input filters applied with par.
					if (m.captured(1).startsWith("de."))
					  findings.append(QString("Line %1: '%2' takes its signal as its last argument - write %2(..., %3) instead of '%3 : %2(...)'. Example: de.delay(0.1, mod1, dry).").arg(def.line).arg(m.captured(1)).arg(it.key()));
					else
					  findings.append(QString("Line %1: '%2' is a mono effect applied to a stereo signal - this gives an arity error. Apply it per channel: sig : par(i, 2, %2).").arg(def.line).arg(m.captured(1)));
					break;
				}
			}
		}
	}

	// A parallel composition mixing mono signals with stereo signals before
	// ro.interleave(2, 2): e.g. 'mix1 = (piano, chorus) : ro.interleave(2, 2)'
	// where piano is mono (1 channel) and chorus stereo (2 channels): 3
	// channels into a 4-input interleave is an arity error. The compiler
	// error and the per-line check flag the line, but nothing says WHY -
	// name the pattern and give the recipe. Also covers tuples whose
	// members are all mono with fewer than 4 members (2 channels into the
	// 4-input interleave), and mono signals applied to stereo effects.
	// (Declared outside the block: the substitution below also uses it to
	// give stereo names 2-channel slider placeholders.)
	QSet<QString> stereo_names;
	{
		// Which names are stereo: soundfile-derived 2-channel signals (the
		// propagation above), outputs of known stereo effects, the results
		// of a pairwise (2,2) interleave mix, and - propagated to a fixed
		// point - names whose definition applies a 2-channel par to a
		// known-stereo name ('out = mixed : par(i, 2, *(1 - mix))' is
		// 2-in/2-out, so 'out' is stereo too). Without that last rule,
		// 'process = out * gain' (a stereo signal multiplied by a mono
		// coefficient) hides behind 'out' and only the generic per-def
		// compile finding is reported, which cost two extra fix rounds
		// (observed).
		for (auto it = soundfile_channels.constBegin(); it != soundfile_channels.constEnd(); ++it)
		  if (it.value() == 2)
		    stereo_names.insert(it.key());
		static const QStringList stereo_effects =
		{
			QStringLiteral("pf\\.flanger_stereo"),
			QStringLiteral("re\\.stereo_freeverb"),
			QStringLiteral("re\\.dattorro_rev_default"),
			QStringLiteral("re\\.satrev"),
			QStringLiteral("co\\.compressor_stereo"),
		};
		for (bool changed = true; changed; )
		{
			changed = false;
			for (const Faust2LintDef &def : defs)
			{
				if (stereo_names.contains(def.name))
				  continue;
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);

				bool stereo = sanitized.contains(QStringLiteral("ro.interleave(2, 2)"))
				           || QRegularExpression(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|"))).match(sanitized).hasMatch();

				// Channel-count-preserving application of a stereo name:
				// par(i, 2, f) runs f over each of its 2 input channels, so
				// a 2-channel input gives a 2-channel output. The input
				// must be fed directly with ':' ('mixed * gain : par(...)'
				// is still the multiplication bug and must NOT match).
				if (!stereo)
				{
					for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
					{
						const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*:\\s*par\\s*\\(\\s*i\\s*,\\s*2\\s*,").arg(*it));
						if (re.match(sanitized).hasMatch())
						{
							stereo = true;
							break;
						}
					}
				}

				if (stereo)
				{
					stereo_names.insert(def.name);
					changed = true;
				}
			}
		}
		// A name is (heuristically) mono when its definition uses only mono
		// primitives (oscillators, envelopes, filters, noises, UI controls)
		// and no stereo construct, propagated through definitions that only
		// reference known-mono names ('dry = piano_sound * gain' is mono
		// because piano_sound and the gain slider are). A 'name, name'
		// tuple makes a definition non-mono (parallel composition), and so
		// does any stereo construct.
		static const QStringList mono_primitives =
		{
			QStringLiteral("\\bos\\."),
			QStringLiteral("\\ben\\."),
			QStringLiteral("\\bfi\\."),
			QStringLiteral("\\bno\\."),
			QStringLiteral("\\bhslider\\s*\\("),
			QStringLiteral("\\bvslider\\s*\\("),
			QStringLiteral("\\bnentry\\s*\\("),
			QStringLiteral("\\bbutton\\s*\\("),
			QStringLiteral("\\bcheckbox\\s*\\("),
			QStringLiteral("\\bhbargraph\\s*\\("),
			QStringLiteral("\\bvbargraph\\s*\\("),
		};
		const QRegularExpression mono_primitive_re(mono_primitives.join("|"));
		const QRegularExpression name_pair_re(QStringLiteral("\\b[a-zA-Z_][a-zA-Z0-9_]*\\s*,\\s*[a-zA-Z_][a-zA-Z0-9_]*\\b"));
		const auto has_stereo_construct = [&](const QString &sanitized) -> bool
		{
			return sanitized.contains(QStringLiteral("ro.interleave"))
			    || sanitized.contains(QStringLiteral("soundfile"))
			    || sanitized.contains(QStringLiteral("so.sound"))
			    || QRegularExpression(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|"))).match(sanitized).hasMatch();
		};
		QSet<QString> mono_names;
		for (bool changed = true; changed; )
		{
			changed = false;
			for (const Faust2LintDef &def : defs)
			{
				if (mono_names.contains(def.name) || stereo_names.contains(def.name))
				  continue;
			// Function-call argument lists are masked out so a 'name, name'
			// inside a call (arguments) is not mistaken for a parallel-
			// composition tuple.
			const QString masked = faust2_lint_mask_call_args(faust2_lint_sanitize_strings(def.rhs));
			if (name_pair_re.match(masked).hasMatch())
			  continue;
			if (has_stereo_construct(masked))
			  continue;
			// 'x = _;' binds the input as a plain identity: 1 channel.
			bool mono = mono_primitive_re.match(masked).hasMatch()
			         || masked.trimmed() == QStringLiteral("_");
				if (!mono)
				{
					// No mono primitive of its own: mono only if it
					// references at least one known-mono name and nothing
					// else (all references are known-mono names).
					mono = false;
					bool refs_any = false;
					for (const Faust2LintDef &other : defs)
					{
						if (other.name == def.name)
						  continue;
						const QRegularExpression ref(QStringLiteral("\\b%1\\b(?!\\.)").arg(other.name));
						if (ref.match(masked).hasMatch())
						{
							refs_any = true;
							if (!mono_names.contains(other.name))
							{
								mono = false;
								break;
							}
							mono = true;
						}
					}
					mono = mono && refs_any;
				}
			if (mono)
			{
				mono_names.insert(def.name);
				changed = true;
			}
		}
	}
		// A mono signal duplicated into a stereo pair with the split
		// ('dry = bell <: _,_;'): mono input, 2 outputs, so the def is
		// stereo. This makes the operator checks below see named
		// split-duplicates ('process = dry : *(mix)') as stereo. Only
		// the mono-name-anchored form is matched: '<: _,_' fanned over
		// a stereo input would give 4 channels, which is a different
		// bug and must not be classified as stereo.
		for (bool changed = true; changed; )
		{
			changed = false;
			for (const Faust2LintDef &def : defs)
			{
				if (stereo_names.contains(def.name))
				  continue;
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
				for (auto it = mono_names.constBegin(); it != mono_names.constEnd(); ++it)
				{
					const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*<:\\s*_,\\s*_").arg(*it));
					if (re.match(sanitized).hasMatch())
					{
						stereo_names.insert(def.name);
						changed = true;
						break;
					}
				}
			}
		}
		// A mono signal applied to a stereo effect: e.g. the synthesized
		// piano's 'chorus = dry : pf.flanger_stereo(...)' (1 channel into a
		// 2-input effect). The reverse direction (stereo into a mono
		// effect) has its own finding above. ALL occurrences are checked:
		// the first ':'-composition in the RHS may be legal while a later
		// one is broken (observed with dry/wet pairs).
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*:\\s*(%1)\\s*\\(").arg(stereo_effects.join("|")));
			QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				if (mono_names.contains(m.captured(1)))
				{
					findings.append(QString("Line %1: '%2' is a mono signal applied to the stereo effect %3 - this gives an arity error. Duplicate it first: (%2, %2) : %3(...).").arg(def.line).arg(m.captured(1)).arg(m.captured(2)));
					break; // one finding per definition
				}
			}
		}
		// A mono signal applied to a 2-channel par: 'dry : par(i, 2,
		// *(1 - mix))' where dry is mono - 1 channel into a 2-way par is
		// an arity error, and the compiler reports it as "sequential
		// composition dry:B" (outputs [1] vs inputs [2]) with no recipe.
		// The model then applies the per-channel idiom to a mono signal
		// and the fix round only carries the generic per-def finding
		// (observed). Duplicate the mono signal first. ALL occurrences
		// are checked: '((dryStereo : par(...)), (wet : par(...)))' with
		// mono wet must flag wet even though the first par is legal.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*:\\s*par\\s*\\(\\s*i\\s*,\\s*2\\s*,"));
			QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				if (mono_names.contains(m.captured(1)))
				{
					findings.append(QString("Line %1: '%2' is a mono signal applied to par(i, 2, ...) - this gives an arity error (1 channel into a 2-channel par). Duplicate it to stereo first: %2Stereo = %2 <: _,_; and use %2Stereo instead.").arg(def.line).arg(m.captured(1)));
					break; // one finding per definition
				}
			}
		}
		// A parenthesized pair of mono signals fed into a MONO effect:
		// '(sound, sound) : de.delay(0.1 * ma.SR, 0.02 * ma.SR)' is 2
		// channels into a 1-input effect. (Observed: the fix loop
		// exhausted all three rounds on it - the soundfile-keyed check
		// above only sees soundfile-derived stereo names, never anonymous
		// pairs, so the fix prompts carried only the generic finding.)
		// Only pairs of known-mono names are flagged: the pair is then
		// exactly 2 channels and the 1-input effect breaks the arity. A
		// stereo pair into a 2-input effect (pf.flanger_stereo,
		// re.stereo_freeverb, ...) is legal and never matches here.
		{
			static const QStringList mono_effects =
			{
				QStringLiteral("ef\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("de\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("fi\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("en\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("co\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("pf\\.flanger_mono"),
				QStringLiteral("pf\\.vibrato2_mono"),
				QStringLiteral("pf\\.phaser2_mono"),
			};
			for (const Faust2LintDef &def : defs)
			{
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
				const QRegularExpression re(QStringLiteral("\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*,\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)\\s*:\\s*(%1)\\s*\\(").arg(mono_effects.join("|")));
				QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
				while (it.hasNext())
				{
					const QRegularExpressionMatch m = it.next();
					if (m.captured(3) == QStringLiteral("ef.dryWetMixer")
					    || m.captured(3) == QStringLiteral("ef.dryWetMixerConstantPower"))
					  continue; // N-in/N-out bus mixer: a stereo pair in is legal when FX is stereo
					if (!mono_names.contains(m.captured(1)) || !mono_names.contains(m.captured(2)))
					  continue;
					findings.append(QString("Line %1: '(%2, %3)' is a 2-channel signal applied to the mono effect %4 - this gives an arity error. Apply the effect per channel: (%2, %3) : par(i, 2, %4).").arg(def.line).arg(m.captured(1)).arg(m.captured(2)).arg(m.captured(3)));
					break; // one finding per definition
				}
			}
		}
		// A mono effect called with a stereo argument: e.g. the chorus
		// effect's 'process = x : ef.dryWetMixer(wet, chorus)' where chorus
		// is a stereo signal. (The de. module is excluded: it takes its
		// signal as the last argument and has its own finding.)
		// ef.dryWetMixer is special-cased below: it is an N-in/N-out bus
		// mixer whose second argument must be an EFFECT FUNCTION, not a
		// signal, so the generic per-channel recipe does not apply. (The
		// model repeatedly passes the named 'wet' signal there, and then
		// "fixes" it with par(i, 2, ...), which still does not compile -
		// observed.)
		{
			static const QStringList mono_arg_effects =
			{
				QStringLiteral("ef\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("fi\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("en\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("co\\.[a-zA-Z_][a-zA-Z0-9_]*"),
				QStringLiteral("pf\\.flanger_mono"),
				QStringLiteral("pf\\.vibrato2_mono"),
				QStringLiteral("pf\\.phaser2_mono"),
			};
			for (const Faust2LintDef &def : defs)
			{
				const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				{
					const QRegularExpression re(QStringLiteral("\\b(%1)\\s*\\([^()]*\\b%2\\b[^()]*\\)").arg(mono_arg_effects.join("|")).arg(*it));
					const QRegularExpressionMatch m = re.match(sanitized);
					if (m.hasMatch())
					{
						if (m.captured(1) == QStringLiteral("ef.dryWetMixer")
						    || m.captured(1) == QStringLiteral("ef.dryWetMixerConstantPower"))
						{
							// Only a problem when the argument is a SIGNAL
							// (a definition with no audio inputs). A named
							// effect (its definition contains a bare '_'
							// input binding, e.g. 'chorus = _,_ : ...') is
							// exactly what FX must be: then the call is
							// legal and nothing is reported.
							QString arg_rhs;
							for (const Faust2LintDef &other : defs)
							  if (other.name == *it)
							  {
							    arg_rhs = faust2_lint_sanitize_strings(other.rhs);
							    break;
							  }
							const QRegularExpression input_re(QStringLiteral("(^|[^a-zA-Z0-9_])_([^a-zA-Z0-9_]|$)"));
							if (!arg_rhs.isEmpty() && !input_re.match(arg_rhs).hasMatch())
							  findings.append(QString("Line %1: %2(wetAmount, FX) expects an EFFECT (a function) as its second argument, not a signal like '%3' (a signal has 0 inputs, so the compiler reports the arity mismatch). Pass the effect itself instead: %2(wetAmount, re.stereo_freeverb(0.8, 0.8, 0.3, 0.5)) - and DELETE the separate '%3' definition: %2 already passes the input through as the dry signal (do not add a dry path). Do NOT wrap it in par(i, 2, ...): that does not help here.").arg(def.line).arg(m.captured(1)).arg(*it));
						}
						else
						{
							findings.append(QString("Line %1: '%2' is a mono effect, but its argument '%3' is a stereo signal - this gives an arity error. Apply the effect per channel: sig : par(i, 2, %2(...)).").arg(def.line).arg(m.captured(1)).arg(*it));
						}
						break;
					}
				}
			}
		}
		// Tuples before ro.interleave(2, 2): the interleave needs 4 input
		// channels. Flag tuples that mix stereo and mono members (3 or
		// fewer channels), all-mono tuples with fewer than 4 members, a
		// single 2-channel member, and members that are ':'-compositions
		// not individually parenthesized (the comma binds tighter than ':',
		// so '(a : f, b : g)' parses as 'a : (f, b) : g' - a different
		// arity error, observed with '(dry : par(...), wet : par(...)) :
		// ro.interleave(2, 2)'). The tuple is parsed paren-aware: its
		// members may contain nested parentheses (function calls).
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);

			const QRegularExpression interleave_re(QStringLiteral(":\\s*ro\\.interleave\\(2\\s*,\\s*2\\)"));
			QRegularExpressionMatchIterator it = interleave_re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				int close = -1;
				const int open = faust2_lint_find_open_paren(sanitized, m.capturedStart(), &close);
				if (open < 0 || close <= open)
				  continue;
				const QString content = sanitized.mid(open + 1, close - open - 1);
				const QStringList members = faust2_lint_split_tuple(content);

				if (members.size() == 1)
				{
					// A single parenthesized 2-channel expression into a
					// 4-input interleave (observed: '(wet : par(i, 2,
					// *(mix))) : ro.interleave(2, 2)' with stereo wet).
					// A lone member whose channel count is unknown is
					// left to the generic per-def finding.
					const QString member = members[0].trimmed();
					bool stereo = false;
					for (auto it2 = stereo_names.constBegin(); it2 != stereo_names.constEnd() && !stereo; ++it2)
					  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*it2)).match(member).hasMatch())
					    stereo = true;
					if (stereo)
					  findings.append(QString("Line %1: '%2' is a 2-channel signal, but ro.interleave(2, 2) needs 4 input channels - this gives an arity error. Mix two stereo signals pairwise instead: (sigA, sigB) : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(member));
					break; // one finding per definition
				}
				if (members.size() < 2)
				  continue;

				// A member that is a ':'-composition but NOT wrapped in its
				// own parentheses is misparsed: the comma binds tighter
				// than ':'.
				bool unparenthesized_composition = false;
				for (const QString &member : members)
				{
					const QString trimmed = member.trimmed();
					if (!trimmed.startsWith('(') && trimmed.contains(':'))
					{
						unparenthesized_composition = true;
						break;
					}
				}
				if (unparenthesized_composition)
				{
					QString dup = "(";
					for (int i = 0; i < members.size(); i++)
					{
						if (i > 0)
						  dup += ", ";
						dup += QString("(%1)").arg(members[i].trimmed());
					}
					dup += ")";
					findings.append(QString("Line %1: a tuple member before ro.interleave(2, 2) is a ':'-composition that is not individually parenthesized, and the comma binds tighter than ':' - '(a : f, b : g)' means 'a : (f, b) : g', which gives this arity error. Parenthesize each tuple element: %2 : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(dup));
					break; // one finding per definition
				}

				QStringList mono_members;
				bool has_stereo_member = false;
				bool has_unknown_member = false;
				for (const QString &member : members)
				{
					const QString name = member.trimmed();
					if (stereo_names.contains(name))
					  has_stereo_member = true;
					else if (mono_names.contains(name))
					  mono_members << name;
					else
					  has_unknown_member = true; // e.g. a parenthesized sub-composition - its channel count is unknown
				}
				if (has_unknown_member)
				  continue;

				if (has_stereo_member && !mono_members.isEmpty())
				{
					QString dup = "(";
					for (int i = 0; i < members.size(); i++)
					{
						const QString name = members[i].trimmed();
						if (i > 0)
						  dup += ", ";
						dup += mono_members.contains(name) ? QString("(%1, %1)").arg(name) : name;
					}
					dup += ")";
					findings.append(QString("Line %1: the parallel composition (%2) mixes mono signal(s) %3 with stereo signals before ro.interleave(2, 2) - this gives an arity error. Duplicate the mono signal(s): %4 : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(content).arg(mono_members.join(", ")).arg(dup));
				}
				else if (!has_stereo_member && members.size() < 4 && mono_members.size() == members.size())
				{
					QString dup = "(";
					for (int i = 0; i < members.size(); i++)
					{
						const QString name = members[i].trimmed();
						if (i > 0)
						  dup += ", ";
						dup += QString("(%1, %1)").arg(name);
					}
					dup += ")";
					findings.append(QString("Line %1: every signal in (%2) is mono, so the tuple has only %3 channels - ro.interleave(2, 2) needs 4. Duplicate each one: %4 : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(content).arg(mono_members.size()).arg(dup));
				}
				break; // one finding per definition
			}
		}
		// A tuple containing a stereo signal applied to a stereo effect:
		// '(dry, dry) : pf.flanger_stereo(...)' with stereo dry is 4 channels
		// into a 2-input effect (the model over-applies the duplicate-mono
		// recipe). Name it and undo the duplication.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression effect_re(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|")));
			QRegularExpressionMatchIterator it = effect_re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				int close = -1;
				const int open = faust2_lint_find_open_paren(sanitized, m.capturedStart(), &close);
				if (open < 0 || close <= open)
				  continue;
				const QString content = sanitized.mid(open + 1, close - open - 1);
				const QStringList members = faust2_lint_split_tuple(content);
				for (const QString &member : members)
				{
					const QString name = member.trimmed();
					if (stereo_names.contains(name))
					{
						findings.append(QString("Line %1: (%2) has at least 4 channels but %3 takes only 2 inputs - '%4' is already stereo. Write '%4 : %3(...)' without duplicating.").arg(def.line).arg(content).arg(m.captured(1)).arg(name));
						break;
					}
				}
			}
		}
		// A parenthesized parallel pair combined with an INFIX operator:
		// '(left, right) * gain' feeds the 2-channel pair plus the
		// coefficient to the operator (3 inputs), so the arity breaks. The
		// checks below only see operators adjacent to a NAMED stereo
		// signal; here the pair is anonymous. (Observed: '(left, right) *
		// gain' gave "outputs [3] vs inputs [2]" and the fix prompt only
		// carried the generic per-def finding.) A call paren is excluded by
		// the lookbehind: 'en.adsr(attack, decay, 0.0, release, gate) *
		// gain' is legal.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpression re(QStringLiteral("(?<![a-zA-Z0-9_.])\\(\\s*[a-zA-Z_][a-zA-Z0-9_]*\\s*(?:,\\s*[a-zA-Z_][a-zA-Z0-9_]*)+\\s*\\)\\s*([+*/-])"));
			QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				const QString op = m.captured(1);
				if (op == "*" || op == "/")
				  findings.append(QString("Line %1: '%2' is applied to a parenthesized parallel pair - the pair is a multi-channel signal, so the operator receives too many inputs (this gives the arity error). Apply it per channel: sig : par(i, 2, %2(x)).").arg(def.line).arg(op));
				else
				  findings.append(QString("Line %1: applying '%2' to a parenthesized parallel pair gives an arity error (the operator receives too many inputs). Apply it per channel: sig : par(i, 2, %2(x)).").arg(def.line).arg(op));
				break; // one finding per definition
			}
		}
		// Operators do not distribute over multi-channel signals. For every
		// operator touching a stereo-tracked name, name the line and give a
		// recipe: '*'/'/' need the per-channel scaling, '+' between two
		// stereo signals needs the pairwise mix, and '+'/'-' against a
		// constant needs the per-channel operator. The '+' inside the
		// pairwise-mix idiom itself (par(i, 2, +)) is masked out first, and
		// the two-stereo case does not require the operator to be adjacent
		// to a name: '(a : par(...)) + (b : par(...))' has the stereo names
		// behind parentheses and is just as broken.
		for (const Faust2LintDef &def : defs)
		{
			QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			sanitized.replace(QRegularExpression(QStringLiteral("par\\(i\\s*,\\s*\\d+\\s*,\\s*[+\\-]\\)")), QStringLiteral("PARSUM"));

			QString stereo_name;
			QString op;
			bool found = false;
			for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd() && !found; ++it)
			{
				const QRegularExpression re(QStringLiteral("\\b%1\\b\\s*([+\\-*/])|([+\\-*/])\\s*\\b%1\\b").arg(*it));
				const QRegularExpressionMatch m = re.match(sanitized);
				if (m.hasMatch())
				{
					stereo_name = *it;
					op = !m.captured(1).isEmpty() ? m.captured(1) : m.captured(2);
					found = true;
				}
			}
			if (!found)
			{
				// Operator not adjacent to a stereo name: still the two-stereo
				// case when a remaining +/- operator exists anywhere and at
				// least two stereo names appear (e.g. parenthesized sums).
				if (!sanitized.contains("+") && !sanitized.contains("-"))
				  continue;
				int stereo_count = 0;
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*it)).match(sanitized).hasMatch())
				    stereo_count++;
				if (stereo_count < 2)
				  continue;
				findings.append(QString("Line %1: '+'/'-' does not distribute over stereo signals - this gives an arity error. Mix stereo signals pairwise: (a, b) : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line));
				continue;
			}
			if (op == "*" || op == "/")
			  findings.append(QString("Line %1: '%2' is a stereo signal %3 by a mono coefficient - this gives an arity error. Scale each channel instead: %2 : par(i, 2, %4).").arg(def.line).arg(stereo_name).arg(op == "*" ? "multiplied" : "divided").arg(op == "*" ? "*(x)" : "/(x)"));
			else
			{
				int stereo_count = 0;
				for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
				  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*it)).match(sanitized).hasMatch())
				    stereo_count++;
				if (stereo_count >= 2)
				  findings.append(QString("Line %1: '%2' does not distribute over stereo signals - this gives an arity error. Mix stereo signals pairwise: (a, b) : ro.interleave(2, 2) : par(i, 2, +).").arg(def.line).arg(op));
				else
				  findings.append(QString("Line %1: applying '%2' to the stereo signal '%3' gives an arity error. Apply it per channel: %3 : par(i, 2, %2(x)).").arg(def.line).arg(op).arg(stereo_name));
			}
		}

		// Postfix operator application to a (possibly anonymous) stereo
		// signal: '... : ro.interleave(2, 2) : par(i, 2, +) : *(gain)'.
		// The operator consumes the stereo signal as one input, so the
		// arity breaks. The checks above only see operators ADJACENT to
		// a named stereo signal; here the stereo side is an anonymous
		// composition and the operator is a postfix ':' application.
		// Observed: the fix loop exhausted all three rounds on
		// '... : par(i, 2, +) : *(gain)' because no finding localized
		// the line. Only matches at paren depth 0 are candidates: the
		// same textual pattern inside 'par(i, 2, *(1 - mix))' is the
		// legal per-channel recipe.
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);

			const QRegularExpression postfix_re(QStringLiteral(":\\s*([+*/-])\\s*\\("));
			QRegularExpressionMatchIterator it = postfix_re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();

				int depth = 0;
				for (int i = 0; i < m.capturedStart(); i++)
				{
					if (sanitized.at(i) == '(')
					  depth++;
					else if (sanitized.at(i) == ')')
					  depth--;
				}
				if (depth > 0)
				  continue; // inside a par(...) body or another group

				// The signal the operator is applied to must actually be
				// stereo: the prefix either builds one anonymously
				// (interleave mix, par, a stereo effect) or references a
				// stereo-tracked name.
				const QString prefix = sanitized.left(m.capturedStart());
				bool prefix_stereo =
					prefix.contains(QStringLiteral("ro.interleave(2, 2)"))
					|| prefix.contains(QStringLiteral("par(i, 2,"))
					|| QRegularExpression(QStringLiteral(":\\s*(%1)\\s*\\(").arg(stereo_effects.join("|"))).match(prefix).hasMatch()
					|| QRegularExpression(QStringLiteral("(?<![a-zA-Z0-9_.])\\([a-zA-Z_][a-zA-Z0-9_]*\\s*,\\s*[a-zA-Z_][a-zA-Z0-9_]*\\)\\s*$")).match(prefix).hasMatch();
				if (!prefix_stereo)
				{
					for (auto sn = stereo_names.constBegin(); sn != stereo_names.constEnd() && !prefix_stereo; ++sn)
					  if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(*sn)).match(prefix).hasMatch())
					    prefix_stereo = true;
				}
				if (!prefix_stereo)
				  continue;

				const QString op = m.captured(1);
				if (op == "*" || op == "/")
				  findings.append(QString("Line %1: '%2(x)' is applied to a stereo signal with ':' - the operator consumes it as one input, so this gives an arity error. Scale each channel instead: sig : par(i, 2, %2(x)) (e.g. write ... : par(i, 2, *(gain)), not ... : *(gain)).").arg(def.line).arg(op));
				else
				  findings.append(QString("Line %1: applying '%2' to a stereo signal gives an arity error. Apply it per channel: sig : par(i, 2, %2(x)).").arg(def.line).arg(op));

				break; // one finding per definition
			}
		}
		// The max-length argument of a delay line must be a provably
		// CONSTANT bound: deriving it from a slider (or any non-constant
		// expression) makes the compiler unable to prove that the delay
		// time stays within it, giving 'invalid delay parameter range:
		// interval(0, INT_MAX, 0)' with no line number. Observed:
		// 'delay_samps = delay_time * ma.SR; de.delay(delay_samps,
		// delay_samps)' - the fix was 'de.delay(1.0 * ma.SR, delay_samps)'.
		// A max length built from constants is fine
		// ('de.delay(chorus_delay, ...)' with chorus_delay = 0.03 * ma.SR).
		// Smoothed sliders INSIDE the delay-time argument are NOT flagged:
		// e.g. a chorus delay 'const * (1 + smoothed_depth * os.osc(rate))'
		// compiles (observed).
		QSet<QString> constant_names;
		for (const Faust2LintDef &def : defs)
		{
			QString stripped = faust2_lint_sanitize_strings(def.rhs);
			stripped.replace(QStringLiteral("ma.SR"), QStringLiteral(""));
			stripped.replace(QStringLiteral("_"), QStringLiteral(""));
			if (QRegularExpression(QStringLiteral("^[\\d\\s.eE+\\-*/()]+$")).match(stripped.trimmed()).hasMatch())
			  constant_names.insert(def.name);
		}
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			QRegularExpressionMatchIterator it = QRegularExpression(QStringLiteral("\\b(de\\.(?:delay|sdelay|fdelay|ffdelay|fbdelay)|pf\\.flanger_(?:mono|stereo)|ef\\.echo[a-zA-Z0-9_]*)\\s*\\(([^()]*)\\)")).globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				const QStringList args = m.captured(2).split(",", Qt::SkipEmptyParts);
				if (args.isEmpty())
				  continue;
				const QString max_arg = args[0].trimmed();
				for (const Faust2LintDef &other : defs)
				{
					if (constant_names.contains(other.name))
					  continue;
					if (QRegularExpression(QStringLiteral("\\b%1\\b(?!\\.)").arg(other.name)).match(max_arg).hasMatch())
					  findings.append(QString("Line %1: the max-length argument of %2 ('%3') is derived from a slider or a signal - the compiler cannot prove the delay time stays within it, which gives the 'invalid delay parameter range' error. Use a generous CONSTANT max length instead: e.g. %2(1.0 * ma.SR, %3).").arg(def.line).arg(m.captured(1)).arg(max_arg));
				}
			}
		}
	}

	// A sum of applied filter terms, e.g.
	// 'process = _,_ : par(i, 2, (_ : lp) + (_ : bp1) + ...)'. Each
	// '(_ : band)' term consumes its OWN input channel (the '+' operator
	// distributes the composition's inputs across its branches), so the
	// sum has one input per term and the arity is wrong. Fan the input to
	// all bands with the split instead: '_ <: lp, bp1, ... :> _'.
	{
		const QRegularExpression re(QStringLiteral("\\(_\\s*:\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\)\\s*\\+[^;\\n]*\\(_\\s*:\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\)"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpressionMatch m = re.match(sanitized);
			if (m.hasMatch())
			  findings.append(QString("Line %1: each '(_ : band)' term in the '+' sum consumes its own input channel - this gives an arity error. Fan one signal to all bands with the split instead: _ <: %2, %3, ... :> _.").arg(def.line).arg(m.captured(1)).arg(m.captured(2)));
		}
	}

	// re.mono_freeverb / re.stereo_freeverb derive their internal delay
	// lengths from the 'spread' argument (reverbs.lib:
	// lbcf(combtuningL(i) + spread, ...)). Passing a SMOOTHED slider there
	// hides its range from the compiler (the smoothing recursion breaks
	// range analysis) and gives an 'invalid delay parameter range' error
	// with no line number - name the line and give the recipe.
	{
		const QRegularExpression freeverb_re(QStringLiteral("re\\.(mono_freeverb|stereo_freeverb)\\s*\\(([^()]*)\\)"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			const QRegularExpressionMatch m = freeverb_re.match(sanitized);
			if (!m.hasMatch())
			  continue;
			const QStringList args = m.captured(2).split(",", Qt::SkipEmptyParts);
			if (args.size() != 4)
			  continue;
			const QString spread_name = args[3].trimmed();
			if (!QRegularExpression(QStringLiteral("^[a-zA-Z_][a-zA-Z0-9_]*$")).match(spread_name).hasMatch())
			  continue;
			for (const Faust2LintDef &other : defs)
			{
				if (other.name != spread_name)
				  continue;
				const QString rhs = faust2_lint_sanitize_strings(other.rhs);
				if (rhs.contains(QStringLiteral("si.smooth")) || rhs.contains(QStringLiteral("ba.tau2pole")) || rhs.contains(QStringLiteral("si.smoo")))
				  findings.append(QString("Line %1: '%2' is a smoothed slider passed as the 'spread' argument of %3, and its delay lengths depend on spread - the smoothing hides the range from the compiler and gives an 'invalid delay parameter range' error. Leave that slider unsmoothed ('%2 = hslider(...);' without si.smooth).").arg(def.line).arg(spread_name).arg(m.captured(1)));
				break;
			}
		}
	}

	// Passing a bare input binding as the FX argument of a dry/wet mixer:
	// the second argument of ef.dryWetMixer (and friends) must be an
	// EFFECT (a function applied to a signal). 'ef.dryWetMixer(mix, _,_)'
	// passes a 2-input identity instead, which makes the mixer's internal
	// split see 2 outputs where 3 inputs are expected - the line-less
	// 'split composition A<:B' error. The compiler names no line for it,
	// so name it here.
	{
		const QRegularExpression dwm_re(QStringLiteral("\\b(ef\\.(?:dryWetMixer|dryWetMixerConstantPower|dwmEnv))\\s*\\(\\s*([^,()]*)\\s*,\\s*([^()]*)\\)"));
		const QRegularExpression bare_re(QStringLiteral("(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9_])"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			QRegularExpressionMatchIterator it = dwm_re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				const QString fx_arg = m.captured(3).trimmed();
				if (bare_re.match(fx_arg).hasMatch())
				  findings.append(QString("Line %1: the second argument of %2 is a bare input ('%3'), but it must be an EFFECT - a function applied to a signal. Passing '_' or '_,_' makes the mixer's internal split see the wrong number of channels (the 'split composition A<:B' error). Use %2(mix, effect), or drop the second argument entirely: 'sig : %2(mix)' already passes the input through as the dry signal.").arg(def.line).arg(m.captured(1)).arg(fx_arg));
			}
		}
	}

	// '... + _,_ : ...': the comma binds tighter than '+', so this parses
	// as '((... + _), _) : ...' - a 2-channel signal followed by a 2-input
	// binding after '+' becomes a 3-tuple, giving the line-less
	// 'sequential composition A:B - outputs [3] of A vs inputs [2] of B'
	// error. The compiler names no line, so name it here.
	{
		const QRegularExpression plus_binding_re(QStringLiteral("\\+\\s*_,\\s*_"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			if (plus_binding_re.match(sanitized).hasMatch())
			  findings.append(QString("Line %1: '%2' writes '+ _,_' - the comma binds tighter than '+', so this parses as '((... + _), _)' with 3 output channels (the 'outputs [3] vs inputs [2]' error). Never add a stereo input binding after '+': mix stereo signals with '(a, b) : ro.interleave(2, 2) : par(i, 2, +)', or crossfade with 'sig : ef.dryWetMixer(mix, effect)'.").arg(def.line).arg(def.name));
		}
	}

	// A delay PARAMETER derived from the input signal: when the delay time
	// of de.* / ef.echo depends (transitively) on a signal computed from
	// the program input, the faust compiler inlines the input's channels
	// into the delay-time scalar expression and the arity explodes into a
	// line-less 'sequential composition x:B - outputs [2] of x vs inputs
	// [N] of B' error with a huge N (observed: [34], [65], [130] - the
	// size of the inlined analysis graph). The compiler names no line, so
	// name the delay parameter here. The SIGNAL argument of the delay is
	// of course allowed to be input-derived - only the delay PARAMETERS
	// (n/d/maxDuration/duration) are checked.
	{
		// Splits 'a, b, c(...)' on top-level commas (parens respected).
		const auto split_args = [](const QString &text) -> QStringList
		{
			QStringList result;
			int depth = 0;
			QString current;
			for (const QChar &ch : text)
			{
				if (ch == QChar('('))
				  depth++;
				else if (ch == QChar(')'))
				  depth--;
				if (ch == QChar(',') && depth == 0)
				{
					result.append(current.trimmed());
					current.clear();
				}
				else
					current.append(ch);
			}
			if (!current.trimmed().isEmpty())
			  result.append(current.trimmed());
			return result;
		};

		// Finds every 'de.<name>(' / 'ef.echo<...>(' call in 'text' and
		// appends the full call (balanced parens) to 'calls'.
		const auto collect_delay_calls = [](const QString &text, QStringList &calls)
		{
			const QRegularExpression call_re(QStringLiteral("\\b(de\\.[a-zA-Z_][a-zA-Z0-9_]*|ef\\.echo[a-zA-Z0-9_]*)\\s*\\("));
			int from = 0;
			QRegularExpressionMatch m;
			while ((m = call_re.match(text, from)).hasMatch())
			{
				int depth = 0;
				int i = m.capturedEnd();
				for (; i < text.size(); i++)
				{
					const QChar ch = text.at(i);
					if (ch == QChar('('))
					  depth++;
					else if (ch == QChar(')'))
					{
						if (depth == 0)
						{
							i++;
							break;
						}
						depth--;
					}
				}
				calls.append(text.mid(m.capturedStart(), i - m.capturedStart()));
				from = i;
			}
		};

		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			QStringList calls;
			collect_delay_calls(sanitized, calls);
			for (const QString &call : calls)
			{
				const int open = call.indexOf(QChar('('));
				const QString callee = call.left(open).trimmed();
				const QString args_text = call.mid(open + 1, call.size() - open - 2);
				const QStringList args_list = split_args(args_text);

				// Which arguments are delay parameters (not the signal).
				QList<int> delay_param_indexes;
				if (callee.contains(QStringLiteral("sdelay")))
				  delay_param_indexes.append(args_list.size() - 1); // (n, it, d) - d is the audio-rate delay
				else if (callee.startsWith(QStringLiteral("ef.")))
				  delay_param_indexes << 0 << 1; // echo(maxDuration, duration, feedback)
				else
				  delay_param_indexes.append(qMin(1, args_list.size() - 1)); // delay/fdelay/... (n, d, x) - d is the delay

				for (const int idx : delay_param_indexes)
				{
					if (idx < 0 || idx >= args_list.size())
					  continue;
					const QString &arg = args_list.at(idx);
					for (auto it = input_derived.constBegin(); it != input_derived.constEnd(); ++it)
					{
						if (QRegularExpression(QStringLiteral("(?<!\\.)\\b%1\\b(?!\\.)").arg(*it)).match(arg).hasMatch())
						{
							findings.append(QString("Line %1: the delay parameter '%2' of %3 is derived from the program input ('%4' is computed from the input signal) - the faust compiler inlines the input's channels into the delay-time expression and reports a meaningless 'outputs [2] vs inputs [N]' arity error. Derive delay parameters from sliders/constants/LFOs only, never from a signal computed from the program input.")
							                .arg(def.line).arg(arg).arg(callee).arg(*it));
							goto next_def; // one finding per definition
						}
					}
				}
			}
			next_def:;
		}
	}

	// Output channel counts for the substitution placeholders: bindings by
	// their exact count ('x = _,_' = 2), heuristic-stereo names by 2, and
	// everything else 1 (the substitution default). Without this, names
	// like 'dry = x : par(i, 2, *(1 - mix))' (2 channels) were replaced by
	// a MONO hslider, making every tuple/interleave consumer fail its
	// isolated compile with a bogus arity error.
	QHash<QString, int> def_channels = binding_channels;
	for (auto it = stereo_names.constBegin(); it != stereo_names.constEnd(); ++it)
		if (!def_channels.contains(*it))
		  def_channels.insert(*it, 2);

	// A filter bank built with par(i, N, ...) for a LARGE N (observed:
	// par(i, 37, env(i)) in an autotune effect): wide banks quickly hit
	// faust's arity limits, produce line-less 'outputs [37] vs inputs
	// [38]'-style errors, and invite channel-indexing bugs (see the next
	// check). Give the recipe: a handful of bands with the fan idiom.
	{
		const QRegularExpression wide_par_re(QStringLiteral("\\bpar\\s*\\(\\s*[a-zA-Z_]\\s*,\\s*(\\d+)\\s*,"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			QRegularExpressionMatchIterator it = wide_par_re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QRegularExpressionMatch m = it.next();
				const int n = m.captured(1).toInt();
				if (n > 8)
				{
					findings.append(QString("Line %1: '%2' builds a filter bank with par(i, %3, ...) - a bank this wide is very hard to get right in faust (it gives line-less arity errors like 'outputs [%3] vs inputs [%4]', and multi-channel signals cannot be indexed per channel). Use a handful of bands (2-4) with the fan idiom: process = _ <: band1, band2, ... :> _ - and sum their results with a chain of pairwise '+' (mix1 = (a, b) : ro.interleave(2, 2) : par(i, 2, +); mix2 = (mix1, c) : ...).").arg(def.line).arg(def.name).arg(n).arg(n + 1));
					break;
				}
			}
		}
	}

	// Indexing a multi-channel signal like an array: 'normw(i)' where
	// 'normw' is a multi-channel VALUE (not a function) is not valid faust
	// - signals cannot be indexed per channel. (A function def CAN be
	// called normally, so only value defs are flagged.)
	{
		for (const Faust2LintDef &def : defs)
		{
			if (arg_counts.contains(def.name) || def_channels.value(def.name, 1) <= 1)
			  continue;
			for (const Faust2LintDef &user : defs)
			{
				if (user.name == def.name)
				  continue;
				const QString sanitized = faust2_lint_sanitize_strings(user.rhs);
				if (QRegularExpression(QStringLiteral("(?<!\\.)\\b%1\\b\\s*\\(").arg(def.name)).match(sanitized).hasMatch())
				{
					findings.append(QString("Line %1: '%2' is a multi-channel signal, but it is called like a function ('%2(...)') - faust signals cannot be indexed per channel. Apply the whole signal with ':' or restructure with par(i, N, ...) instead.").arg(user.line).arg(def.name));
					break;
				}
			}
		}
	}

	for (const Faust2LintDef &def : defs)
	{
		if (!compile_check_safe)
		  break; // guarded at the top of this function: never wait on libfaust's global factory lock here (this runs on the GUI thread)

		// A soundfile declaration cannot be checked standalone: a bare
		// 'process = soundfile(...)' is not a valid program (the compiler
		// rejects the unbound part number), so it would always be flagged
		// even though the line is fine in context.
		if (faust2_lint_is_soundfile_decl(def.rhs))
		  continue;

		const QString substituted = faust2_lint_substitute(def.rhs, def.name, all_names, soundfile_channels, arg_counts, def_channels);

		// A function definition ('name(a, b) = ...') must have its formal
		// parameters bound in the synthetic program, otherwise every use of
		// a parameter gives a bogus 'undefined symbol' finding.
		const QString lambda_open = def.args.isEmpty()
		  ? QString()
		  : QStringLiteral("\\(") + def.args.join(", ") + QStringLiteral(").(");
		const QString lambda_close = def.args.isEmpty() ? QString() : QStringLiteral(")");

		const QString synthetic =
		  "import(\"stdfaust.lib\");\n"
		  "process = " + lambda_open + substituted + lambda_close + ";\n";

		std::string error_msg;
		interpreter_dsp_factory *factory =
		  createInterpreterDSPFactoryFromString("FaustDev2Lint",
		                                        synthetic.toUtf8().constData(),
		                                        args.get_argc(),
		                                        args.get_argv(),
		                                        error_msg);

		if (factory == NULL)
		{
			printf("LLM lint: line %d (%s): expression does not compile on its own: %s\n",
			       def.line, def.name.toUtf8().constData(), error_msg.c_str());
			findings.append(QString("Line %1: the definition '%2' does not compile on its own, so it is part of the compile error.")
			                .arg(def.line).arg(def.name));

			// Sequential composition of a signal with a constant: e.g.
			// 'zeroCrossRate(x) = x : signChange : ma.SR / max(1, count)'
			// fails with 'outputs [1] ... inputs [0]' and names no line
			// (the error only says 'inlined expression omitted'). Attach
			// the recipe to this definition, which is where the fix goes.
			const QString error_qstr = QString::fromUtf8(error_msg.c_str());
			if (error_qstr.contains(QStringLiteral("inputs [0]")))
			  findings.append(QString("Line %1: the definition '%2' composes a signal with a constant via ':' (the compiler says the last expression has no input). Multiply instead: e.g. write 'x : signChange * (ma.SR / max(1, count))', not 'x : signChange : ma.SR / max(1, count)'.").arg(def.line).arg(def.name));

			// Arity explosion: 'outputs [N] ... inputs [M]' with an
			// implausibly large M is the signature of a delay parameter
			// derived from the input signal (the compiler inlines the
			// input's channels into the delay-time scalar expression).
			{
				const QRegularExpression huge_re(QStringLiteral("inputs \\[(\\d+)\\]"));
				const QRegularExpressionMatch huge_m = huge_re.match(error_qstr);
				if (huge_m.hasMatch()
				    && huge_m.captured(1).toInt() >= 4
				    && error_qstr.contains(QStringLiteral("outputs [")))
				  findings.append(QString("Line %1: the definition '%2' fails with 'outputs [N] vs inputs [%3]' - this huge-arity shape usually means a delay parameter is derived from the program input (the compiler inlines the input's channels into the delay-time expression). Derive delay parameters from sliders/constants/LFOs only, never from a signal computed from the program input.").arg(def.line).arg(def.name).arg(huge_m.captured(1)));
			}

			// An 'undefined symbol' failure is the one case where the
			// compiler names the exact symbol (and this isolated compile
			// proves it belongs to this definition). Report it precisely:
			// the generic finding above does not say what to fix, and the
			// model often copies the name from an example (observed with
			// 'bend').
			if (error_qstr.contains(QStringLiteral("undefined symbol")))
			{
				const QRegularExpression sym_re(QStringLiteral("undefined symbol\\s*:?\\s*'?([a-zA-Z_][a-zA-Z0-9_]*)"));
				const QRegularExpressionMatch sym_m = sym_re.match(error_qstr);
				if (sym_m.hasMatch())
				{
					const QString symbol = sym_m.captured(1);

					// The substitution deliberately leaves self-references
					// ('bell = bell : fi.lowpass(...)') as-is, so a def that
					// redefines an already-defined name fails the isolated
					// compile with 'undefined symbol : bell' even though
					// 'bell' IS defined - the real error is the duplicate
					// definition, which the textual check reports with its
					// own (contradicting this one) message. Do not add a
					// 'never defined' finding on top of it.
					bool duplicate_of_self = false;
					if (symbol == def.name)
					{
						for (const Faust2LintDef &other : defs)
						  if (other.name == def.name && other.line != def.line)
						  {
						    duplicate_of_self = true;
						    break;
						  }
					}

					if (!duplicate_of_self)
					{
						if (symbol == def.name)
						  // Pure self-reference without another definition:
						  // the recursion itself is the bug, and the generic
						  // 'define it as a slider' recipe does not apply.
						  findings.append(QString("Line %1: the definition '%2' references itself, but there is no other definition of '%2' - the self-reference is the undefined symbol. Either add the missing base definition, or remove the reference to itself.")
						                  .arg(def.line).arg(def.name));
						else
						  findings.append(QString("Line %1: the definition '%2' uses '%3', which is never defined anywhere in the program - this gives the 'undefined symbol' error. It is probably a leftover from an example or a previous revision: either define it (e.g. %3 = hslider(\"%3\", 0.5, 0, 1, 0.01);) or remove every use of it.")
						                  .arg(def.line).arg(def.name).arg(symbol));
					}
				}
			}
		}
		else
		{
			dsp *dsp = factory->createDSPInstance();
			const int num_inputs = (dsp != NULL) ? dsp->getNumInputs() : 0;
			delete dsp;
			deleteInterpreterDSPFactory(factory);

			if (num_inputs > 0)
			{
				// Classify WHY the isolated compile has unbound inputs by
				// comparing the measured input count with the bare '_'s in
				// the RHS text:
				//  - A chain/binding def ('x = _;', 'zcr = _ : abs : ...')
				//    has exactly its leading '_'s as inputs: intentional.
				//  - EXTRA bare '_'s deeper in the expression (e.g.
				//    'ba.if(_, 1, 0)' inside a chain) consume additional
				//    hidden process inputs: the real bug, report the count.
				//  - A def with NO bare '_' but unbound inputs is the
				//    value-def case ('lp = fi.lowpass(2, fc)'): a filter/
				//    smoother used as a plain value - the classic bug.
				// For a FUNCTION def ('band(i) = ...') the synthetic binds
				// the formal parameters in a lambda, which counts them as
				// signal inputs - subtract them so the reported count is
				// the REAL unbound input count.
				const int effective_inputs = num_inputs - def.args.size();
				const QString sanitized_rhs = faust2_lint_sanitize_strings(def.rhs);

				// Bare '_' tokens: underscores that are not part of an
				// identifier (identifiers like 'foo_bar' may contain '_').
				// '_'s inside a '<: ... :>' fan are bound by the split and
				// are not process inputs, so blank those regions first.
				QString bare_count_text = sanitized_rhs;
				{
					int from = 0;
					while ((from = bare_count_text.indexOf(QStringLiteral("<:"), from)) >= 0)
					{
						const int end = bare_count_text.indexOf(QStringLiteral(":>"), from + 2);
						if (end < 0)
						  break;
						for (int i = from; i < end + 2; i++)
						  bare_count_text[i] = ' ';
						from = end + 2;
					}
				}
				int bare_count = 0;
				{
					const QRegularExpression bare_re(QStringLiteral("(?<![a-zA-Z0-9_])_(?![a-zA-Z0-9_])"));
					QRegularExpressionMatchIterator it = bare_re.globalMatch(bare_count_text);
					while (it.hasNext())
					{
						it.next();
						bare_count++;
					}
				}

				// Leading '_' tuple before the first ':' (the chain input
				// binding): '_ : ...' or '_,_ : ...'.
				int leading_count = 0;
				{
					const QRegularExpression leading_re(QStringLiteral("^\\s*_\\s*(,\\s*_\\s*)*:"));
					const QRegularExpressionMatch m = leading_re.match(sanitized_rhs);
					if (m.hasMatch())
					{
						for (const QChar &ch : m.captured(0))
							if (ch == QChar('_'))
							  leading_count++;
					}
				}

				const QString trimmed_rhs = sanitized_rhs.trimmed();
				const bool pure_binding = trimmed_rhs == QStringLiteral("_") || trimmed_rhs == QStringLiteral("_,_");
				const bool pure_chain = leading_count > 0 && leading_count == bare_count;
				if (pure_binding || pure_chain)
				  continue; // intentional inputs - not a bug

				if (bare_count > leading_count)
				{
					// Hidden extra '_'s inside the expression: every one
					// consumes a process input channel (observed:
					// 'ba.if(_, 1, 0)' inside a chain).
					findings.append(QString("Line %1: the definition '%2' has %3 unbound audio input channel(s) but only %4 come from its own input - it contains %5 extra bare '_' inside the expression (e.g. a 'ba.if(_, 1, 0)' call), and every bare '_' consumes a process input channel. Apply the function to the signal with ':' instead.")
					                .arg(def.line).arg(def.name).arg(effective_inputs).arg(leading_count).arg(bare_count - leading_count));
					continue;
				}

				// A value def whose input is fed by a '<: ... :>' fan-out
				// (or used as a par body) is the filter-bank EQ idiom - its
				// unbound input is intentional, not a bug.
				if (faust2_lint_is_fan_or_iter_body(defs, def.name))
				  continue;

				// Cap the repetitive 'unbound audio input' findings: a
				// program with N broken filters needs one recipe, not N
				// copies of it eating the whole findings budget.
				{
					int plain_value_count = 0;
					for (const QString &f : findings)
						if (f.contains(QStringLiteral("unbound audio input channel(s)")))
						  plain_value_count++;
					if (plain_value_count >= 3)
					  continue;
				}

				printf("LLM lint: line %d (%s): expression has %d unbound audio input(s)\n",
				       def.line, def.name.toUtf8().constData(), effective_inputs);
				findings.append(QString("Line %1: the definition '%2' has %3 unbound audio input channel(s): a filter/smoother is probably used as a plain value instead of being applied to a signal with ':'.")
				                .arg(def.line).arg(def.name).arg(effective_inputs));

				// If another definition references this unbound-input
				// definition BARE (not applied with ':' and not called
				// with '('), name that line too and give the recipe: the
				// model writes 'lp = fi.lowpass(2, fc) : *(...)' and then
				// sums it bare ('process = _,_ : par(i, 2, lp + bp1 +
				// ...)'), which reproduces exactly this unbound input.
				for (const Faust2LintDef &user : defs)
				{
					if (user.name == def.name)
					  continue;
					const QString sanitized = faust2_lint_sanitize_strings(user.rhs);
					const QRegularExpression re(QStringLiteral("(?<!:\\s)\\b%1\\b(?!\\s*\\()").arg(def.name));
					if (re.match(sanitized).hasMatch())
					{
						if (effective_inputs == 1)
						  findings.append(QString("Line %1: '%2' has an audio input but is referenced as a plain value - apply it to the signal with ':': (_ : %2), or, when summing several filters, fan the input to them with the split: _ <: f1, f2, ... :> _.").arg(user.line).arg(def.name));
						else
						  findings.append(QString("Line %1: '%2' has %3 unbound audio input channels and is referenced as a plain value - a bare reference consumes %3 of the process's input channels; the total over ALL bare references must equal the process input count.").arg(user.line).arg(def.name).arg(effective_inputs));
						break;
					}
				}
			}
		}

		if (findings.size() >= 8)
		  break;
	}

	return findings;
}


// (name, start_line, end_line) of every top-level definition, 0-based
// inclusive lines. Same parsing rules as faust2_lint_collect_defs:
// multi-line RHS, delimiters tracked, strings/comments masked. Shared by
// the splice flow and the duplicate-definition auto-fix.
struct Faust2DefSpan { QString name; int start; int end; };
static QList<Faust2DefSpan> faust2_collect_def_spans(const QString &code)
{
	QList<Faust2DefSpan> spans;
	const QStringList lines = code.split('\n');
	const QRegularExpression def_re(QStringLiteral("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*=(.*)$"));
	bool in_block_comment = false;
	int depth = 0;
	int start_line = -1;
	QString name;
	for (int i = 0; i < lines.size(); i++)
	{
		const QString line_text = lines.at(i);
		const QString masked = faust2_lint_mask_line(line_text, in_block_comment);

		if (start_line < 0)
		{
			const QRegularExpressionMatch m = def_re.match(line_text);
			if (m.hasMatch()
			    && m.captured(1) != "declare"
			    && m.captured(1) != "import"
			    && m.capturedStart(1) < masked.size()
			    && masked.at(m.capturedStart(1)) != ' ')
			{
				name = m.captured(1);
				start_line = i;
				depth = 0;
			}
		}

		if (start_line >= 0)
		{
			bool terminated = false;
			for (const QChar &ch : masked)
			{
				if (ch == '(' || ch == '{' || ch == '[')
				  depth++;
				else if (ch == ')' || ch == '}' || ch == ']')
				  depth--;
				else if (ch == ';' && depth <= 0)
				{
					terminated = true;
					break;
				}
			}
			if (terminated)
			{
				spans.append({name, start_line, i});
				start_line = -1;
			}
		}
	}
	return spans;
}

// Splices the top-level definitions of 'fragment' into 'current_code': a
// definition whose name already exists replaces the existing definition,
// and a new name is inserted before 'process'. Used by the partial-fix
// flow: for long programs the fix prompt asks for only the corrected
// definition(s), so the response no longer needs to fit the whole program
// into the completion token limit. When the fragment contains a 'process'
// definition it is a complete program and is returned as-is. 'import'
// lines in the fragment are dropped (the current program already imports
// stdfaust.lib).
QString FAUST2_splice_faust_definitions(const QString &current_code, const QString &fragment)
{
	const QList<Faust2DefSpan> current_spans = faust2_collect_def_spans(current_code);
	const QList<Faust2DefSpan> fragment_spans = faust2_collect_def_spans(fragment);

	// A complete program: use it wholesale.
	for (const Faust2DefSpan &span : fragment_spans)
	  if (span.name == QStringLiteral("process"))
	    return fragment;

	const QStringList current_lines = current_code.split('\n');
	const QStringList fragment_lines = fragment.split('\n');

	// Which current definitions exist, and their span by start line.
	QHash<QString, int> current_index;
	QHash<int, QPair<QString, int>> span_at_start; // start_line -> (name, end_line)
	for (int i = 0; i < current_spans.size(); i++)
	{
		current_index.insert(current_spans[i].name, i);
		span_at_start.insert(current_spans[i].start, {current_spans[i].name, current_spans[i].end});
	}

	// Fragment definition texts: by current def name (for replacement) and
	// as a list of new-name texts (for insertion before 'process').
	QHash<QString, QStringList> replacement_text;
	QList<QStringList> insert_texts;
	for (const Faust2DefSpan &fspan : fragment_spans)
	{
		QStringList text;
		for (int i = fspan.start; i <= fspan.end; i++)
		  text.append(fragment_lines.at(i));
		if (current_index.contains(fspan.name))
		  replacement_text.insert(fspan.name, text); // last one wins on duplicates
		else
		  insert_texts.append(text);
	}

	// Rebuild the current program line by line, swapping replaced spans.
	QStringList result;
	for (int i = 0; i < current_lines.size(); )
	{
		if (span_at_start.contains(i))
		{
			const QPair<QString, int> span = span_at_start.value(i);
			if (replacement_text.contains(span.first))
			{
				result.append(replacement_text.value(span.first));
				i = span.second + 1;
				continue;
			}
		}
		result.append(current_lines.at(i));
		i++;
	}

	// Insert new definitions before 'process' (or at the end).
	if (!insert_texts.isEmpty())
	{
		int process_idx = result.size();
		for (int i = 0; i < result.size(); i++)
		  if (QRegularExpression(QStringLiteral("^\\s*process\\s*=")).match(result.at(i)).hasMatch()) // [NO_IF_=_WARNING]
		  {
		    process_idx = i;
		    break;
		  }
		int insert_at = process_idx;
		for (const QStringList &text : insert_texts)
		{
			for (const QString &line : text)
			  result.insert(insert_at++, line);
		}
	}

	return result.join('\n');
}


// Local auto-fix for the "multiple definitions of symbol 'name'" error.
// Faust has NO assignment, so a second definition of the same symbol is
// usually the model's imperative attempt to UPDATE a value
// ('x = ...; x = x : f;'). Translate that intent into legal faust:
//  - When the SECOND definition references the name itself
//    ('x = x : fi.lowpass(...)'), the FIRST definition gets a fresh name
//    ('x_raw = ...') and the second one computes from it. The second
//    keeps the original name, so every other reference sees the final
//    value - exactly what the imperative reading means.
//  - Otherwise (identical or different RHS), the LAST definition is the
//    effective one in an imperative reading: remove the first.
// Returns false (leaving *fixed_code untouched) when no safe fix exists
// (no duplicate found, a reserved name like 'process' or a note control,
// or the definitions are not plain value defs).
bool FAUST2_try_fix_duplicate_definition(const QString &code,
                                         const QString &name,
                                         QString *fixed_code,
                                         QString *note)
{
	if (name.isEmpty() || name == QStringLiteral("process")
	    || name == QStringLiteral("declare") || name == QStringLiteral("import")
	    || name == QStringLiteral("freq") || name == QStringLiteral("gain")
	    || name == QStringLiteral("gate") || name == QStringLiteral("velocity"))
	  return false;

	const QList<Faust2DefSpan> spans = faust2_collect_def_spans(code);

	QList<Faust2DefSpan> dups;
	for (const Faust2DefSpan &span : spans)
	  if (span.name == name)
	    dups.append(span);

	if (dups.size() < 2)
	  return false;

	const Faust2DefSpan first = dups[0];
	const Faust2DefSpan second = dups[1];

	const QStringList lines = code.split('\n');

	// Text of the SECOND definition's RHS (everything after its header's
	// '=' on the first line, plus the following lines).
	QStringList second_rhs_lines;
	{
		QString second_rhs = lines.at(second.start);
		const int eq = second_rhs.indexOf(QChar('='));
		second_rhs_lines.append(eq >= 0 ? second_rhs.mid(eq + 1) : second_rhs);
		for (int i = second.start + 1; i <= second.end; i++)
		  second_rhs_lines.append(lines.at(i));
	}

	const QRegularExpression name_re(QStringLiteral("(?<!\\.)\\b%1\\b(?!\\.)").arg(name));
	const bool self_referential = name_re.match(faust2_lint_sanitize_strings(second_rhs_lines.join("\n"))).hasMatch();

	// The fresh name for the FIRST definition (self-referential case).
	QString fresh;
	if (self_referential)
	{
		QSet<QString> taken;
		for (const Faust2DefSpan &span : spans)
		  taken.insert(span.name);
		fresh = name + QStringLiteral("_raw");
		for (int n = 2; taken.contains(fresh); n++)
		  fresh = name + QStringLiteral("_raw%1").arg(n);
	}

	QStringList result;
	for (int i = 0; i < lines.size(); i++)
	{
		if (i >= first.start && i <= first.end && i >= second.start && i <= second.end)
		{
			// overlapping spans (same-line dup) - not expected; give up
			return false;
		}

		if (i >= first.start && i <= first.end)
		{
			if (self_referential)
			{
				// Give the FIRST definition a fresh name.
				QString line = lines.at(i);
				if (i == first.start)
				  line.replace(QRegularExpression(QStringLiteral("^(\\s*)%1\\b").arg(QRegularExpression::escape(name))), QStringLiteral("\\1") + fresh);
				result.append(line);
			}
			else
			{
				// Remove the first definition (the last one wins).
			}
		}
		else if (i >= second.start && i <= second.end)
		{
			if (self_referential)
			{
				QString line = lines.at(i);
				if (i == second.start)
				{
					const int eq = line.indexOf(QChar('='));
					const QString header = eq >= 0 ? line.left(eq + 1) : QString();
					const QString rhs = eq >= 0 ? line.mid(eq + 1) : QString();
					line = header + QString(rhs).replace(name_re, fresh);
				}
				else
					line.replace(name_re, fresh);
				result.append(line);
			}
			else
				result.append(lines.at(i));
		}
		else
			result.append(lines.at(i));
	}

	*fixed_code = result.join('\n');
	*note = self_referential
	  ? QString("Auto-fixed duplicate definition: Faust has no assignment, so '%1 = ...; %1 = %1 : ...' was translated into '%2 = ...; %1 = %2 : ...' (the second definition keeps the name; every other reference sees the final value).").arg(name).arg(name + QStringLiteral("_raw"))
	  : QString("Auto-fixed duplicate definition: removed the first of the two definitions of '%1' (Faust cannot reassign a symbol; the last definition is the effective one).").arg(name);
	return true;
}


// Local auto-fix for the 'invalid delay parameter range' error: when a
// SMOOTHED slider is passed as a delay parameter (freeverb spread, or the
// max-length argument of de.*/pf.*/ef.echo), the smoothing hides the
// range from the compiler (interval(0, INT_MAX, 0)). The fix is
// mechanical: strip the smoothing from the slider definition, exactly
// like the lint recipe says. Returns false when no such pattern is found
// or the definition is not a single line.
bool FAUST2_try_fix_smoothed_delay_param(const QString &code,
                                         QString *fixed_code,
                                         QString *note)
{
	const QList<Faust2LintDef> defs = faust2_lint_collect_defs(code);
	if (defs.isEmpty())
	  return false;

	// Smoothed UI-control definitions: 'name = hslider(...) : si.smooth(...);'
	QSet<QString> smoothed_names;
	{
		const QRegularExpression smooth_re(QStringLiteral(":\\s*si\\.(?:smooth\\s*\\(|smoo\\b)"));
		for (const Faust2LintDef &def : defs)
		{
			const QString sanitized = faust2_lint_sanitize_strings(def.rhs);
			if (QRegularExpression(QStringLiteral("^\\s*(hslider|vslider|nentry)\\s*\\(")).match(sanitized).hasMatch()
			    && smooth_re.match(sanitized).hasMatch())
			  smoothed_names.insert(def.name);
		}
	}
	if (smoothed_names.isEmpty())
	  return false;

	// Which smoothed name is used as a delay parameter?
	QString culprit;
	for (const Faust2LintDef &def : defs)
	{
		const QString sanitized = faust2_lint_sanitize_strings(def.rhs);

		// Freeverb spread (4th argument).
		{
			const QRegularExpression re(QStringLiteral("\\bre\\.(?:mono_freeverb|stereo_freeverb)\\s*\\(([^()]*)\\)"));
			QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QStringList args = it.next().captured(1).split(",", Qt::SkipEmptyParts);
				if (args.size() == 4)
				{
					const QString spread = args[3].trimmed();
					if (smoothed_names.contains(spread))
					{
						culprit = spread;
						break;
					}
				}
			}
		}
		if (!culprit.isEmpty())
		  break;

		// Max-length argument of de.*/pf.*/ef.echo.
		{
			const QRegularExpression re(QStringLiteral("\\b(de\\.(?:delay|sdelay|fdelay|ffdelay|fbdelay)|pf\\.flanger_(?:mono|stereo)|ef\\.echo[a-zA-Z0-9_]*)\\s*\\(([^()]*)\\)"));
			QRegularExpressionMatchIterator it = re.globalMatch(sanitized);
			while (it.hasNext())
			{
				const QStringList args = it.next().captured(2).split(",", Qt::SkipEmptyParts);
				if (!args.isEmpty())
				{
					const QString max_arg = args[0].trimmed();
					if (smoothed_names.contains(max_arg))
					{
						culprit = max_arg;
						break;
					}
				}
			}
		}
		if (!culprit.isEmpty())
		  break;
	}
	if (culprit.isEmpty())
	  return false;

	// Find the single-line definition of the culprit and strip the
	// smoothing suffix (' : si.smooth(...)' or ' : si.smoo').
	const QList<Faust2DefSpan> spans = faust2_collect_def_spans(code);
	const Faust2DefSpan *span = NULL;
	for (const Faust2DefSpan &s : spans)
	  if (s.name == culprit)
	  {
		span = &s;
		break;
	  }
	if (span == NULL || span->start != span->end)
	  return false;

	const QStringList lines = code.split('\n');
	QString line = lines.at(span->start);

	const int smooth_pos = line.lastIndexOf(QStringLiteral("si.smooth"));
	const int smoo_pos = line.lastIndexOf(QStringLiteral("si.smoo"));
	int pos = -1;
	{
		const int a = smooth_pos >= 0 ? smooth_pos : -1;
		const int b = smoo_pos >= 0 ? smoo_pos : -1;
		pos = qMax(a, b);
	}
	if (pos < 0)
	  return false;

	// Backward to the ':' that introduces the smoothing.
	int colon = pos;
	while (colon > 0 && line.at(colon) != QChar(':'))
	  colon--;
	if (line.at(colon) != QChar(':'))
	  return false;

	// Forward to the end of the smoothing: the matching ')' of si.smooth(...),
	// or the end of the 'si.smoo' token when there are no parentheses.
	int end = pos;
	if (smooth_pos >= 0 && smooth_pos == pos && line.indexOf(QChar('('), pos) >= 0)
	{
		int depth = 0;
		int i = line.indexOf(QChar('('), pos);
		for (; i < line.size(); i++)
		{
			if (line.at(i) == QChar('('))
			  depth++;
			else if (line.at(i) == QChar(')'))
			{
				depth--;
				if (depth == 0)
				{
					end = i;
					break;
				}
			}
		}
	}
	else
	{
		// si.smoo without parentheses: consume the token.
		end = pos;
		while (end < line.size() && (line.at(end).isLetterOrNumber() || line.at(end) == QChar('_')))
		  end++;
		end--; // last char of the token
	}

	line.remove(colon, end - colon + 1);

	QStringList result = lines;
	result[span->start] = line;
	*fixed_code = result.join('\n');
	*note = QString("Auto-fixed: removed smoothing from '%1' - delay parameters must stay unsmoothed (the smoothing hides the range from the compiler and gives the 'invalid delay parameter range' error).").arg(culprit);
	return true;
}
