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
#include <QRegularExpression>
#include <QString>
#include <QStringList>

#include <atomic>
#include <functional>
#include <memory>
#include <utility>

#include "LLM_client.hpp"
#include "../audio/Faust2_llm_lint.hpp"


namespace radium
{
namespace llm
{

// The GUI/plugin operations the session needs. Radium's Faust Dev 2 widget
// implements them with the editor and the plugin; faust_llm_test implements
// them with an in-memory program and a headless compiler.
struct FaustSessionHost
{
	std::function<QString()> get_code;
	std::function<void(const QString &code)> set_code;
	std::function<QString()> compile_error;             // the currently displayed compiler error, or empty
	std::function<bool()> error_is_shown;               // optional; true when the host currently displays a compile error
	std::function<bool()> is_compiling;
	std::function<bool()> compile_check_is_safe;
	std::function<bool()> is_effect;
	std::function<bool()> effect_is_mono_override; // optional; used by faust_llm_test's --mono
	std::function<QString()> radium_path;
	std::function<QString()> default_code;
	std::function<void(const QString &text)> status;
	std::function<void(const QString &message)> show_error;
	std::function<void(bool)> set_generate_enabled;
	std::function<void(bool)> set_cancel_enabled;
	std::function<void()> prompt_accepted; // optional; clears the prompt entry after a successful generation
};

// The Faust Dev 2 LLM session state machine: generate -> compile -> static
// checks -> local auto-fixes -> compile-error fix rounds -> lint cleanup
// rounds. Shared by the Radium widget and faust_llm_test so both run the
// exact same loop and produce the same ~/.radium/llm.log story.
class FaustSession
{
public:
	explicit FaustSession(FaustSessionHost host)
		: _host(std::move(host))
		, _alive(std::make_shared<std::atomic_bool>(true))
	{
	}

	// Stops all pending callbacks from touching this session. Must be called
	// before the session is destroyed.
	void shutdown(void)
	{
		*_alive = false;
		if (_llm_cancel)
			*_llm_cancel = true;
	}

	// Arms the creation context: the next generate request is treated as
	// creation whatever the prompt says.
	void set_new_session(void)
	{
		_llm_new_session = true;
	}

	bool is_busy(void) const
	{
		return _request_in_flight || _awaiting_compile;
	}

	// True when no request, compile or auto-fix round is pending.
	bool is_idle(void) const
	{
		return !is_busy() && !_llm_fixing_error;
	}

	bool fixing_error(void) const
	{
		return _llm_fixing_error;
	}

	bool new_session_armed(void) const
	{
		return _llm_new_session;
	}

	QString last_applied_code(void) const
	{
		return _llm_last_applied_code;
	}

	void generate(const QString &prompt_to_send)
	{
		_host.status(QString::fromUtf8("\u2318 Generating..."));

		QString current_code = _host.get_code();

		const LLMConfig config = get_config();

		const bool is_effect = _host.is_effect();
		const bool effect_is_mono = effect_is_mono_for(prompt_to_send);

		const bool creation = _llm_new_session || is_creation_request(prompt_to_send);
		const QString code_for_request = creation ? QString() : current_code;

		const bool skip_examples = !code_for_request.isEmpty();

		QString compile_error;
		if (!code_for_request.isEmpty()
		    && !_host.compile_error().isEmpty()
		    && !_host.is_compiling())
		{
			const QString error_message = _host.compile_error();
			const bool arity_class = is_arity_error(error_message);
			const QStringList findings = lint_findings(current_code, arity_class);
			compile_error = summarize_faust_error(error_message);
			if (!findings.isEmpty())
			  compile_error += "\n\nA local static check of the code above found these suspicious lines:\n"
			                   + findings.join("\n");
		}

		const QJsonArray history = _llm_history;
		const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();
		_request_in_flight = true;
		const std::shared_ptr<std::atomic_bool> alive = _alive;

		send_prompt(config, code_for_request, prompt_to_send,
		            [this, alive, config, prompt_to_send, code_for_request, compile_error, current_code, is_effect, effect_is_mono, skip_examples](bool ok, QString result_or_error)
		            {
			            if (!*alive)
			              return;

			            _request_in_flight = false;
			            end_llm_request();
			            _host.set_generate_enabled(true);

			            if (ok && !current_code.trimmed().isEmpty()
			                && result_or_error.simplified() == current_code.simplified())
			            {
				            printf("LLM: The model returned the current program unchanged. Discarding.\n");
				            _host.status("The model returned the current program unchanged - please rephrase the request.");
				            return;
			            }

			            if (ok)
			            {
				            _llm_history.append(QJsonObject{
					            {QStringLiteral("role"), QStringLiteral("user")},
					            {QStringLiteral("content"), build_full_user_content(code_for_request, prompt_to_send, config.library_context, skip_examples, compile_error, is_effect, effect_is_mono)},
				            });
				            _llm_history.append(QJsonObject{
					            {QStringLiteral("role"), QStringLiteral("assistant")},
					            {QStringLiteral("content"), result_or_error},
				            });
				            trim_llm_history(result_or_error.size());

				            _llm_new_session = false;

				            if (_host.prompt_accepted)
				              _host.prompt_accepted();

				            _llm_original_prompt = prompt_to_send;
				            _llm_max_fixes = config.max_fixes < 0 ? 0 : config.max_fixes;
				            _llm_compile_attempts = 0;
				            _llm_last_fix_error.clear();
				            _llm_same_error_count = 0;
				            _llm_auto_fix_count = 0;
				            _llm_fixing_error = true;
				            _host.status("Generated. Compiling...");
				            apply_code(result_or_error);
			            }
			            else
			            {
				            llm_log_note("Generate request failed: " + result_or_error);
				            _llm_fixing_error = false;
				            _host.show_error(result_or_error);
			            }
		            },
		            history, cancel, 0.2,
		            [this, alive](int reasoning_chars, int content_chars)
		            {
			            if (*alive)
			              update_progress(reasoning_chars, content_chars);
		            },
		            skip_examples,
		            compile_error,
		            is_effect,
		            effect_is_mono);
	}

	void cancel(void)
	{
		if (_llm_cancel)
		  *_llm_cancel = true;
		_llm_cancel.reset();
		_host.set_cancel_enabled(false);
		_host.set_generate_enabled(true);
		_llm_fixing_error = false;
		_host.status("Cancelled.");
	}

	// Resets the session to the state of a newly created instrument: cancels
	// any in-flight request, clears the conversation history and all
	// generation state, and replaces the code with the default program.
	void clear_history(void)
	{
		if (_llm_cancel)
		  *_llm_cancel = true;
		_llm_cancel.reset();
		_host.set_cancel_enabled(false);
		_host.set_generate_enabled(true);

		_request_in_flight = false;
		_llm_history = QJsonArray();
		_llm_fixing_error = false;
		_llm_compile_attempts = 0;
		_llm_last_fix_error.clear();
		_llm_same_error_count = 0;
		_llm_last_applied_code.clear();
		_llm_original_prompt.clear();
		_llm_lint_cache_code.clear();
		_llm_lint_cache_findings.clear();
		_llm_last_progress_total = -1;
		_llm_last_progress_thinking = false;

		apply_code(_host.default_code());

		_llm_new_session = true;
	}

	// A manual edit cancels auto-fixing of LLM code and invalidates the
	// conversation history.
	void user_edited_code(void)
	{
		_llm_fixing_error = false;
		_llm_history = QJsonArray();
		_llm_new_session = false;
	}

	// Static-analysis findings for 'code': the textual checks (duplicate
	// definitions, JS arrow syntax), plus (when compile_check is true) the
	// exact per-definition compile check. Cached per code and check type so
	// the error pane and the LLM fix prompt don't run the compile-based
	// check twice for the same failing code.
	QStringList lint_findings(const QString &code, bool compile_check)
	{
		if (compile_check && !_host.compile_check_is_safe())
		  compile_check = false;

		if (_llm_lint_cache_code == code && _llm_lint_cache_compile_check == compile_check)
		  return _llm_lint_cache_findings;

		QStringList findings = radium::llm::lint_faust_code(code).split('\n', Qt::SkipEmptyParts);
		if (compile_check)
		  findings += FAUST2_lint_faust_code(code, true, _host.radium_path());

		// Correct 'undefined symbol' findings that name a Faust LIBRARY
		// function: the fix is the module-qualified form (ma.log2), never a
		// self-made definition (defining the bare name collides with the
		// library definition - observed with 'log2', which the model then
		// defined itself and got "BoxIdent[log2] is defined here"). Both
		// phrasings of the finding are matched: the audio-side compile check
		// ("uses 'X', which is never defined") and the textual check
		// ("'X' is used but never defined anywhere in the program").
		static const QRegularExpression undef_re(QStringLiteral("(?:uses '([a-zA-Z_][a-zA-Z0-9_]*)', which is never defined|'([a-zA-Z_][a-zA-Z0-9_]*)' is used but never defined)")); // [NO_STATIC_ARRAY_WARNING]
		for (QString &finding : findings)
		{
			const QRegularExpressionMatch m = undef_re.match(finding);
			if (!m.hasMatch())
			  continue;
			const QString symbol = m.captured(1).isEmpty() ? m.captured(2) : m.captured(1);
			const QString qualified = llm_library_qualified_name(symbol);
			if (!qualified.isEmpty())
			  finding += QString(" Note: '%1' is a function in the Faust standard library - use the module-qualified form '%2' instead of defining it yourself (defining the bare name collides with the library definition).").arg(symbol).arg(qualified);
		}

		_llm_lint_cache_code = code;
		_llm_lint_cache_compile_check = compile_check;
		_llm_lint_cache_findings = findings;
		return findings;
	}

	// Called by the host when a compilation of the current program finished
	// successfully.
	void on_compile_succeeded(int num_inputs)
	{
		_awaiting_compile = false;

		const bool llm_compile_done = _llm_fixing_error;
		const bool llm_compile_was_fix = _llm_compile_attempts > 0;
		_llm_fixing_error = false;

		if (!llm_compile_done)
		  return;

		QStringList lint_warnings = lint_findings(_host.get_code(), false);

		if (_host.is_effect())
		{
			const int expected_inputs = _llm_original_prompt.toLower().contains("mono") ? 1 : 2;
			llm_log_note(QString("effect-input-check: ready.num_inputs=%1 expected=%2").arg(num_inputs).arg(expected_inputs));
			if (num_inputs != expected_inputs)
			  lint_warnings.append(QString("The compiled effect has %1 input channel(s), but it must have exactly %2. Bind the input ONCE ('process = %3 : ...') and derive every other signal from that single binding - a bare '_' reference outside the input binding consumes an extra input channel, and references inside par(i, 2, ...) bodies belong to the lambda, not the input.").arg(num_inputs).arg(expected_inputs).arg(expected_inputs == 2 ? "_,_" : "_"));
		}

		if (!lint_warnings.isEmpty())
		{
			llm_log_note("LLM code compiled, static check findings:\n" + lint_warnings.join("\n"));

			const bool free_mode = get_config().mode == "free";
			if (free_mode)
			{
				llm_log_note("Free mode: cleanup skipped - the program compiles, so it is kept as-is.");
				if (llm_compile_was_fix)
				  llm_log_note("Auto-fix/cleanup done - no static-check findings remain.");
				reset_loop_state();
				_host.status(llm_compile_was_fix ? "Fixed." : "Generated.");
			}
			else
			{
				request_llm_lint_cleanup(lint_warnings, llm_compile_was_fix);
			}
		}
		else
		{
			if (llm_compile_was_fix)
			  llm_log_note("Auto-fix/cleanup done - no static-check findings remain.");
			reset_loop_state();
			_host.status(llm_compile_was_fix ? "Fixed." : "Generated.");
		}
	}

	// Called by the host when a compilation of the current program failed.
	// 'factory_failed' is false when only SVG generation failed: the program
	// itself compiled, so no fix request is sent.
	void on_compile_failed(bool factory_failed)
	{
		_awaiting_compile = false;

		if (!factory_failed)
		{
			_llm_fixing_error = false;
			return;
		}

		const QString code = _host.get_code();

		if (_llm_fixing_error
		    && _llm_compile_attempts < _llm_max_fixes
		    && _llm_last_applied_code == code)
		{
			const QString error_message = _host.compile_error();

			// Local auto-fix for "multiple definitions of symbol 'name'":
			// the model writes imperative faust ('x = ...; x = x : f;') and
			// the translation into legal faust is mechanical (rename the
			// first definition / remove it), so it is fixed here without
			// spending an LLM round or a fix attempt.
			{
				static const QRegularExpression dup_re(QStringLiteral("multiple definitions of symbol\\s+'?([a-zA-Z_][a-zA-Z0-9_]*)")); // [NO_STATIC_ARRAY_WARNING]
				const QRegularExpressionMatch dup_m = dup_re.match(error_message);
				if (dup_m.hasMatch() && _llm_auto_fix_count < 3)
				{
					QString fixed_code, note;
					if (FAUST2_try_fix_duplicate_definition(code, dup_m.captured(1), &fixed_code, &note))
					{
						_llm_auto_fix_count++;
						llm_log_note(note);
						_host.status("Auto-fixed duplicate definition. Compiling...");
						apply_code(fixed_code);
						return;
					}
				}
			}

			// Local auto-fix for 'invalid delay parameter range': the model
			// keeps smoothing sliders that are passed as delay parameters.
			if (error_message.contains("invalid delay parameter range")
			    && _llm_auto_fix_count < 3)
			{
				QString fixed_code, note;
				if (FAUST2_try_fix_smoothed_delay_param(code, &fixed_code, &note))
				{
					_llm_auto_fix_count++;
					llm_log_note(note);
					_host.status("Auto-fixed delay parameter. Compiling...");
					apply_code(fixed_code);
					return;
				}
			}

			// If the error text is unchanged since the previous fix round,
			// the fix did not touch the failing expression. One identical
			// repeat is tolerated; after two the loop is stopped instead of
			// burning the remaining fix budget.
			if (error_message == _llm_last_fix_error)
			  _llm_same_error_count++;
			else
			  _llm_same_error_count = 0;

			if (_llm_same_error_count >= 2)
			{
				_llm_fixing_error = false;
				llm_log_note("Compile-error fix loop stopped - same error twice:\n" + error_message);
				_host.status("LLM fix attempts keep producing the same error. Giving up - edit the code manually.");
			}
			else
			{
				_llm_last_fix_error = error_message;
				_llm_compile_attempts++;
				request_llm_fix(error_message);
			}
		}
		else
		{
			_llm_fixing_error = false;
		}
	}

private:
	bool effect_is_mono_for(const QString &prompt) const
	{
		return _host.is_effect()
		    && (prompt.toLower().contains("mono")
		        || (_host.effect_is_mono_override && _host.effect_is_mono_override()));
	}

	// Live-updates the status line with how much the LLM has produced so
	// far: "Thinking..." while the model reasons, "Generating..." once it
	// streams code. Throttled to only report actual changes.
	void update_progress(int reasoning_chars, int content_chars)
	{
		const bool thinking = (reasoning_chars > 0 && content_chars == 0);
		const int total = reasoning_chars + content_chars;
		if (total == _llm_last_progress_total && thinking == _llm_last_progress_thinking)
		  return;
		_llm_last_progress_total = total;
		_llm_last_progress_thinking = thinking;
		if (thinking)
		  _host.status(QString("Thinking... (%1 chars)").arg(reasoning_chars));
		else if (content_chars > 0)
		  _host.status(QString("Generating... (%1 chars)").arg(content_chars));
	}

	void reset_loop_state(void)
	{
		_llm_compile_attempts = 0;
		_llm_last_fix_error.clear();
		_llm_same_error_count = 0;
		_llm_auto_fix_count = 0;
	}

	void apply_code(const QString &code)
	{
		_awaiting_compile = true;
		_host.set_code(code);
		_llm_last_applied_code = _host.get_code();
	}

	void apply_code_splice(const QString &fragment)
	{
		apply_code(FAUST2_splice_faust_definitions(_host.get_code(), fragment));
	}

	// Marks a new in-flight LLM request; cancels any previous one. The epoch
	// lets fix/cleanup callbacks detect that a NEWER request (e.g. a user
	// prompt) has taken over, so their automatic retries never cancel or
	// clobber a user request.
	std::shared_ptr<std::atomic_bool> start_llm_request(void)
	{
		if (_llm_cancel)
		  *_llm_cancel = true;
		_llm_cancel = std::make_shared<std::atomic_bool>(false);
		_llm_request_epoch++;
		_host.set_cancel_enabled(true);
		_llm_last_progress_total = -1;
		_llm_last_progress_thinking = false;
		return _llm_cancel;
	}

	void end_llm_request(void)
	{
		_llm_cancel.reset();
		_host.set_cancel_enabled(false);
	}

	// Keeps history bounded: at most ~6 user/assistant pairs and a character
	// budget that scales with the current program size. Trimming is atomic:
	// complete user/assistant pairs are dropped from the front.
	void trim_llm_history(int current_code_size)
	{
		const int max_pairs = 6;
		const int base_budget = 20000;
		const int max_budget = 60000;

		int char_budget = base_budget + 2 * current_code_size;
		if (char_budget > max_budget)
		  char_budget = max_budget;

		while (_llm_history.size() > max_pairs * 2)
		{
			_llm_history.removeFirst();
			if (!_llm_history.isEmpty() && _llm_history.at(0).toObject().value("role").toString() == QStringLiteral("assistant"))
			  _llm_history.removeFirst();
		}

		int total = 0;
		for (const QJsonValue &message : _llm_history)
		  total += message.toObject().value("content").toString().size();
		while (total > char_budget && _llm_history.size() > 2)
		{
			total -= _llm_history.at(0).toObject().value("content").toString().size();
			_llm_history.removeFirst();
			if (!_llm_history.isEmpty() && _llm_history.at(0).toObject().value("role").toString() == QStringLiteral("assistant"))
			{
				total -= _llm_history.at(0).toObject().value("content").toString().size();
				_llm_history.removeFirst();
			}
		}
	}

	// Sends the compiler error back to the LLM so it can fix the code.
	void request_llm_fix(const QString &error_message)
	{
		const QString current_code = _host.get_code();

		const QStringList lint_findings_list = lint_findings(current_code, true);
		const QString lint_findings = lint_findings_list.join("\n");

		QString verification_instruction;
		if (is_arity_error(error_message))
		  verification_instruction =
		    "Before writing the fix, verify that every function call in the program "
		    "has the exact number of arguments given in the library list. An arity "
		    "error means one call has too many or too few arguments; fix that call "
		    "and change nothing else.";
		else if (error_message.contains("undefined symbol"))
		  verification_instruction =
		    "Before writing the fix, verify that every identifier the program uses "
		    "is defined in the program itself - names that appear only in an "
		    "example are not defined here. Define each missing name (e.g. as a "
		    "slider) or remove every use of it, and change nothing else.";

		const QString fix_prompt =
		  "The Faust compiler reported this error for the code above:\n"
		  + summarize_faust_error(error_message) + "\n\n"
		  + (lint_findings.isEmpty()
		     ? QString()
		     : QString("A local static check of the code above found these suspicious lines:\n")
		       + lint_findings + "\n\n")
		  + "The original request was: " + _llm_original_prompt + "\n\n"
		  + "Reply with ONLY the corrected top-level definition(s), each written as 'name = ...;' (a definition may span several lines and must end with ';'). The rest of the program is kept unchanged. Do NOT re-emit the whole program - change only the definition(s) the error points at.\n\n"
		  + verification_instruction;

		const LLMConfig config = get_config();

		if (config.api_key.isEmpty() && config.mode != "free")
		{
			_llm_fixing_error = false;
			_host.status("No API key set. Cannot fix compile error.");
			return;
		}

		_host.status(QString("Compile error. Asking the LLM to fix it (%1/%2)...")
		             .arg(_llm_compile_attempts).arg(_llm_max_fixes));

		llm_log_note(QString("Sending compile-error fix round %1/%2:\n").arg(_llm_compile_attempts).arg(_llm_max_fixes) + truncate_faust_error(error_message));

		const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();
		_request_in_flight = true;
		const int epoch = _llm_request_epoch;
		const std::shared_ptr<std::atomic_bool> alive = _alive;

		const bool is_effect = _host.is_effect();
		const bool effect_is_mono = effect_is_mono_for(_llm_original_prompt);

		const bool thinking_enabled = is_deepseek(config) && config.reasoning_effort != "off";
		std::shared_ptr<bool> used_fallback = std::make_shared<bool>(false);

		auto fix_progress = [this, alive](int reasoning_chars, int content_chars)
		{
			if (*alive)
			  update_progress(reasoning_chars, content_chars);
		};

		auto fix_callback = std::make_shared<std::function<void(bool, QString)>>();
		*fix_callback = [this, alive, cancel, epoch, current_code, used_fallback, thinking_enabled, config, fix_prompt, fix_progress, fix_callback, is_effect, effect_is_mono](bool ok, QString result_or_error)
		{
			if (!*alive)
			  return;

			if (_llm_request_epoch != epoch)
			  return; // a newer request (e.g. a user prompt) took over - never apply or retry over it

			if (ok
			    && faust_lint_mask(FAUST2_splice_faust_definitions(current_code, result_or_error)).simplified()
			       != faust_lint_mask(current_code).simplified())
			{
				_request_in_flight = false;
				end_llm_request();
				_host.status("LLM fix received. Compiling...");
				apply_code_splice(result_or_error);
				return;
			}

			const QString failure_reason = ok
			  ? "The LLM returned the failing program unchanged."
			  : result_or_error;

			if (!*used_fallback && !thinking_enabled)
			{
				*used_fallback = true;
				_host.status("First fix attempt failed. Trying once more...");
				llm_log_note("Fix attempt failed (" + failure_reason + "), retrying hotter.");
				const QString retry_prompt =
				  fix_prompt
				  + "\n\n"
				  + (ok
				     ? QString("Your previous fix attempt returned the failing program UNCHANGED, "
				               "so the compile error is still present. Do NOT repeat the program "
				               "above: it does not compile.\n\n")
				     : QString("The previous fix attempt failed. The compile error is still present. "
				               "Do NOT repeat the program above: it does not compile.\n\n"))
				  + "Locate the exact expression the error message points at and change it. "
				    "If the cause is unclear, replace the failing line(s) with a simpler "
				    "equivalent construction - for example, drop the dry/wet helper and mix "
				    "the dry and wet signals explicitly - and remove any definition that "
				    "becomes unused.\n\n"
				    "Respond with ONLY the corrected definition(s) (each 'name = ...;').";
				send_prompt(config, current_code, retry_prompt,
				            *fix_callback,
				            QJsonArray(), cancel, 0.7,
				            fix_progress,
				            true, // skip the example section
				            QString(),
				            is_effect,
				            effect_is_mono);
				return;
			}

			_request_in_flight = false;
			end_llm_request();
			_llm_fixing_error = false;
			llm_log_note("Fix failed: " + failure_reason);
			if (failure_reason.contains("429"))
			  _host.show_error("LLM quota exhausted (HTTP 429). Giving up on fixing the compile error.");
			else
			  _host.show_error("LLM could not fix the compile error: " + failure_reason);
		};

		send_prompt(config, current_code, fix_prompt,
		            *fix_callback,
		            QJsonArray(), cancel, 0.2,
		            fix_progress,
		            true, // skip the example section
		            QString(),
		            is_effect,
		            effect_is_mono);
	}

	// Sends the static-check findings back to the LLM when LLM-generated code
	// COMPILES but has suspicious lines (dead sliders, cancelling mix math).
	void request_llm_lint_cleanup(const QStringList &findings, bool llm_compile_was_fix)
	{
		const QString current_code = _host.get_code();
		const QString findings_text = findings.join("\n");

		// Unused sliders are cosmetic and on large programs the cleanup
		// rounds degenerate: cap the rounds at 2 when every finding is an
		// unused-slider finding.
		bool only_unused_sliders = true;
		for (const QString &finding : findings)
		  if (!finding.contains(QStringLiteral("is declared but never used")))
		  {
			  only_unused_sliders = false;
			  break;
		  }
		const int cleanup_max = only_unused_sliders ? 2 : _llm_max_fixes;

		if (findings_text == _llm_last_fix_error)
		  _llm_same_error_count++;
		else
		  _llm_same_error_count = 0;

		_llm_last_fix_error = findings_text;

		if (_llm_same_error_count >= 2)
		{
			llm_log_note("Cleanup gave up - same findings twice:\n" + findings_text);
			_llm_compile_attempts = 0;
			_llm_last_fix_error.clear();
			_llm_same_error_count = 0;
			_host.status(llm_compile_was_fix ? "Fixed." : "Generated.");
			return;
		}

		if (_llm_compile_attempts >= cleanup_max)
		{
			llm_log_note(QString("Cleanup gave up - round budget exhausted (%1/%2):\n").arg(_llm_compile_attempts).arg(cleanup_max) + findings_text);
			_llm_compile_attempts = 0;
			_llm_last_fix_error.clear();
			_llm_same_error_count = 0;
			_host.status(llm_compile_was_fix ? "Fixed." : "Generated.");
			return;
		}

		_llm_compile_attempts++;
		_llm_fixing_error = true;

		const LLMConfig config = get_config();

		if (config.api_key.isEmpty() && config.mode != "free")
		{
			_llm_fixing_error = false;
			_host.status("No API key set. Cannot clean up the code.");
			return;
		}

		_host.status(QString("%1. Cleaning up (%2/%3)...")
		             .arg(llm_compile_was_fix ? "Fixed" : "Generated")
		             .arg(_llm_compile_attempts).arg(cleanup_max));

		llm_log_note(QString("Sending lint cleanup round %1/%2:\n").arg(_llm_compile_attempts).arg(cleanup_max) + findings_text);

		const QString cleanup_prompt =
		  "The program compiles successfully, but a static check of the code above found these suspicious lines:\n"
		  + findings_text + "\n\n"
		  + "Fix ONLY the issues listed above and respond with ONLY the complete "
		    "corrected Faust program. For example: remove a UI control that is "
		    "declared but never used, or replace dry/wet mix math that cancels "
		    "out. Do not change anything else.\n\n"
		  + "The original request was: " + _llm_original_prompt;

		const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();
		_request_in_flight = true;
		const int epoch = _llm_request_epoch;
		const std::shared_ptr<std::atomic_bool> alive = _alive;

		const bool is_effect = _host.is_effect();
		const bool effect_is_mono = effect_is_mono_for(_llm_original_prompt);

		const bool thinking_enabled = is_deepseek(config) && config.reasoning_effort != "off";
		std::shared_ptr<bool> used_fallback = std::make_shared<bool>(false);

		auto cleanup_progress = [this, alive](int reasoning_chars, int content_chars)
		{
			if (*alive)
			  update_progress(reasoning_chars, content_chars);
		};

		auto cleanup_callback = std::make_shared<std::function<void(bool, QString)>>();
		*cleanup_callback = [this, alive, cancel, epoch, current_code, used_fallback, thinking_enabled, config, cleanup_prompt, findings_text, cleanup_progress, cleanup_callback, is_effect, effect_is_mono](bool ok, QString result_or_error)
		{
			if (!*alive)
			  return;

			if (_llm_request_epoch != epoch)
			  return; // a newer request took over

			if (ok
			    && faust_lint_mask(result_or_error).simplified()
			       != faust_lint_mask(current_code).simplified())
			{
				_request_in_flight = false;
				end_llm_request();
				_host.status("Cleanup received. Compiling...");
				apply_code(result_or_error);
				return;
			}

			const QString failure_reason = ok
			  ? "The LLM returned the program unchanged."
			  : result_or_error;

			if (!*used_fallback && !thinking_enabled)
			{
				*used_fallback = true;
				_host.status("Cleanup attempt failed. Trying once more...");
				llm_log_note("Cleanup attempt failed (" + failure_reason + "), retrying hotter.");

				const QString retry_prompt =
				  cleanup_prompt
				  + "\n\n"
				  + (ok
				     ? QString("Your previous cleanup attempt returned the program UNCHANGED, "
				               "so the issues listed above are still present. Do NOT repeat "
				               "the program above.\n\n")
				     : QString("The previous cleanup attempt failed. The issues listed above "
				               "are still present. Do NOT repeat the program above.\n\n"))
				  + "Remove or fix the exact suspicious lines listed above, and respond "
				    "with a DIFFERENT complete Faust program.";

				send_prompt(config, current_code, retry_prompt,
				            *cleanup_callback,
				            QJsonArray(), cancel, 0.7,
				            cleanup_progress,
				            true, // skip the example section
				            QString(),
				            is_effect,
				            effect_is_mono);
				return;
			}

			_request_in_flight = false;
			end_llm_request();
			_llm_fixing_error = false;
			llm_log_note("Cleanup failed: " + failure_reason);
			if (failure_reason.contains("429"))
			  _host.show_error("LLM quota exhausted (HTTP 429). Giving up on cleaning up the code.");
			else
			{
				llm_log_note("Cleanup failed - remaining findings:\n" + findings_text);
				_host.status((_host.error_is_shown && _host.error_is_shown()) ? "Cleaning failed" : "Finished");
			}
		};

		send_prompt(config, current_code, cleanup_prompt,
		            *cleanup_callback,
		            QJsonArray(), cancel, 0.2,
		            cleanup_progress,
		            true, // skip the example section
		            QString(),
		            is_effect,
		            effect_is_mono);
	}

	FaustSessionHost _host;

	std::shared_ptr<std::atomic_bool> _alive;

	bool _request_in_flight = false;
	bool _awaiting_compile = false;

	// Auto-fixing of LLM-generated code that fails to compile.
	bool _llm_fixing_error = false;
	int _llm_compile_attempts = 0;
	int _llm_max_fixes = 3;
	QString _llm_original_prompt;
	QString _llm_last_applied_code;
	QString _llm_last_fix_error;
	int _llm_same_error_count = 0;
	int _llm_auto_fix_count = 0;
	QString _llm_lint_cache_code;
	bool _llm_lint_cache_compile_check = false;
	QStringList _llm_lint_cache_findings;
	int _llm_last_progress_total = -1;
	bool _llm_last_progress_thinking = false;

	QJsonArray _llm_history;
	std::shared_ptr<std::atomic_bool> _llm_cancel;

	bool _llm_new_session = false;

	int _llm_request_epoch = 0;
};

} // namespace llm
} // namespace radium
