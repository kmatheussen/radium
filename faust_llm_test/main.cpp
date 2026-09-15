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

#include <QCoreApplication>
#include <QDateTime>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QFutureWatcher>
#include <QProcess>
#include <QRegularExpression>
#include <QStringList>
#include <QTextStream>
#include <QTimer>
#include <QtConcurrent/QtConcurrentRun>

#include <memory>

extern "C" bool startMTDSPFactories(void);

#include "../Qt/LLM_client.hpp"
#include "../Qt/LLM_faust_session.hpp"
#include "../audio/Faust_dev2_default.hpp"
#include "../audio/Faust2_compile.hpp"

static QString read_file(const QString &path)
{
	QFile file(path);
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
	  return QString();
	return QString::fromUtf8(file.readAll());
}

static void write_file(const QString &path, const QString &text)
{
	QFile file(path);
	if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate))
	{
		fprintf(stderr, "faust_llm_test: could not write %s\n", path.toUtf8().constData());
		return;
	}
	file.write(text.toUtf8());
}

// The compile (and the compile-based lint) run in a CHILD process: the tool
// re-execs itself with --compile-worker / --lint-compile-worker and the
// parent watches the child's memory and wall time. A pathological program -
// observed: a SLIDER used as the filter order of fi.lowpass made libfaust
// allocate 21 GB and run for 26 minutes - is killed instead of taking the
// machine down, and the caller gets a synthetic, actionable error.
static constexpr int WORKER_MAX_RSS_MB = 4096;
static constexpr qint64 WORKER_TIMEOUT_MS = 180000;

static QString worker_kill_reason(void)
{
	return QString(
	  "The Faust compiler used more than %1 MB of memory or ran longer than %2 seconds while compiling this program, so it was killed. "
	  "This is a PATHOLOGICAL PROGRAM: the most common cause is a filter ORDER that is a SLIDER or SIGNAL instead of a constant integer "
	  "(e.g. 'string_lp_order = hslider(...); ... fi.lowpass(string_lp_order, fc)' makes the compiler try to build a variable-order filter). "
	  "Use a constant order ('fi.lowpass(2, fc)'); if the order should be adjustable, select between a few constant-order filters with select2. "
	  "Other causes are a huge unrolled par/loop or a giant expression - split it into smaller named definitions.")
	  .arg(WORKER_MAX_RSS_MB).arg(WORKER_TIMEOUT_MS / 1000);
}

// Runs 'worker_args' (a worker mode of this executable) and waits for it,
// killing it when it exceeds the memory/time budget. On success the worker
// has written 'result_file'; on a kill the reason is written there instead.
static void run_worker_guarded(const QStringList &worker_args, const QString &result_file)
{
	QFile::remove(result_file);

	QProcess process;
	process.setProcessChannelMode(QProcess::MergedChannels);
	process.start(QCoreApplication::applicationFilePath(), worker_args);

	const qint64 start = QDateTime::currentMSecsSinceEpoch();
	for (;;)
	{
		if (process.state() == QProcess::NotRunning)
		  break;
		process.waitForFinished(500);
		if (process.state() == QProcess::NotRunning)
		  break;

		double rss_mb = 0;
		{
			QProcess ps;
			ps.start(QStringLiteral("ps"), QStringList() << QStringLiteral("-o") << QStringLiteral("rss=")
			                                             << QStringLiteral("-p") << QString::number(process.processId()));
			if (ps.waitForFinished(300))
			{
				const QString out = QString::fromUtf8(ps.readAllStandardOutput()).trimmed();
				if (!out.isEmpty())
				  rss_mb = out.split(QRegularExpression(QStringLiteral("\\s+"))).first().toDouble() / 1024.0;
			}
		}

		const qint64 elapsed = QDateTime::currentMSecsSinceEpoch() - start;
		if (rss_mb > WORKER_MAX_RSS_MB || elapsed > WORKER_TIMEOUT_MS)
		{
			process.kill();
			process.waitForFinished(3000);
			const QString reason = worker_kill_reason();
			radium::llm::llm_log_note_append(reason);
			write_file(result_file, reason);
			return;
		}
	}
}

// Compiles 'code' in a worker process with the watchdog.
static Faust2CompileCheckResult guarded_compile(const QString &code, int optlevel)
{
	const qint64 pid = QCoreApplication::applicationPid();
	const QString code_file = QDir::tempPath() + QStringLiteral("/faust_llm_test_%1.faust").arg(pid);
	const QString result_file = QDir::tempPath() + QStringLiteral("/faust_llm_test_%1.result").arg(pid);
	write_file(code_file, code);

	run_worker_guarded(QStringList() << QStringLiteral("--compile-worker") << code_file
	                                 << result_file << QString::number(optlevel), result_file);

	Faust2CompileCheckResult result;
	const QString text = read_file(result_file);
	if (text.isEmpty())
	{
		result.ok = false;
		result.error_message = QStringLiteral("The compiler process crashed without producing a result.");
		return result;
	}
	const int err_pos = text.indexOf(QStringLiteral("\nerror="));
	if (err_pos < 0)
	{
		// The killed-worker reason (no 'error=' marker).
		result.ok = false;
		result.error_message = text;
		return result;
	}
	result.error_message = text.mid(err_pos + 7);
	for (const QString &line : text.left(err_pos).split('\n'))
	{
		if (line.startsWith(QStringLiteral("ok=")))
		  result.ok = line.mid(3).trimmed() == QStringLiteral("1");
		else if (line.startsWith(QStringLiteral("inputs=")))
		  result.num_inputs = line.mid(7).toInt();
		else if (line.startsWith(QStringLiteral("outputs=")))
		  result.num_outputs = line.mid(8).toInt();
		else if (line.startsWith(QStringLiteral("instrument=")))
		  result.is_instrument = line.mid(11).trimmed() == QStringLiteral("1");
	}
	return result;
}

struct Runner
{
	QString code;
	QString displayed_error;
	bool compiling = false;
	bool last_compile_ok = false;
	bool quiet = false;
	bool is_effect = false;
	bool effect_is_mono = false;
	int expected_effect_inputs = -1; // -1 = infer from the prompt
	QString output_file;
	QStringList prompts;
	int next_prompt = 0;
	bool prompt_in_flight = false;
	bool finished = false;
	int exit_code = 1;

	std::unique_ptr<radium::llm::FaustSession> session;
	QFutureWatcher<Faust2CompileCheckResult> watcher;
	QTimer idle_timer;

	void log_status(const QString &text)
	{
		if (!quiet)
		  fprintf(stderr, "faust_llm_test: %s\n", text.toUtf8().constData());
	}

	void start_compile(const QString &new_code)
	{
		code = new_code;
		compiling = true;

		const int optlevel = (int)SETTINGS_read_int("faust_optimization_level", 4);

		watcher.setFuture(QtConcurrent::run([new_code, optlevel]()
		{
			return guarded_compile(new_code, optlevel);
		}));
	}

	void on_compile_done(void)
	{
		const Faust2CompileCheckResult result = watcher.result();
		compiling = false;
		last_compile_ok = result.ok;

		if (result.ok)
		{
			displayed_error.clear();
			radium::llm::llm_log_note_append(QString("compile-result: %1 in / %2 out; is_instrument=%3. code:\n%4")
			                                 .arg(result.num_inputs)
			                                 .arg(result.num_outputs)
			                                 .arg(result.is_instrument)
			                                 .arg(code.left(400)));
			session->on_compile_succeeded(result.num_inputs, result.num_outputs);
		}
		else
		{
			displayed_error = result.error_message;
			session->on_compile_failed(true);
		}
	}

	void start_next_prompt(void)
	{
		if (next_prompt >= prompts.size())
		{
			finish();
			return;
		}

		const radium::llm::LLMConfig config = radium::llm::get_config();
		if (config.api_key.isEmpty() && config.mode != "free")
		{
			log_status("no API key set and not in free mode - configure ~/.radium/config");
			exit_code = 3;
			finish();
			return;
		}

		prompt_in_flight = true;
		last_compile_ok = false;
		session->generate(prompts.at(next_prompt));
		next_prompt++;
	}

	void check_idle(void)
	{
		if (finished)
		  return;

		if (compiling || !session->is_idle())
		  return;

		if (prompt_in_flight)
		{
			prompt_in_flight = false;
			exit_code = last_compile_ok ? 0 : 1;
			log_status(QString("prompt done (compiles: %1)").arg(last_compile_ok ? "yes" : "no"));
		}

		start_next_prompt();
	}

	void finish(void)
	{
		if (finished)
		  return;
		finished = true;

		if (!output_file.isEmpty())
		  write_file(output_file, code);
		else
		  printf("%s\n", code.toUtf8().constData());

		QCoreApplication::exit(exit_code);
	}
};

int main(int argc, char **argv)
{
	QCoreApplication app(argc, argv);

	// Activate libfaust's internal global lock, like create_faust_dev2_plugin
	// does in Radium.
	startMTDSPFactories();

	// Internal worker modes: do one compile (or compile-based lint) and write
	// the result to a file, then exit. Used by run_worker_guarded() so the
	// parent can kill a pathological compile (memory/time watchdog).
	// Compile format: ok=0|1\ninputs=N\noutputs=M\ninstrument=0|1\nerror=<text>
	{
		const QStringList a = QCoreApplication::arguments();
		const int idx_lint = a.indexOf(QStringLiteral("--lint-compile-worker"));
		if (idx_lint >= 0 && idx_lint + 2 < a.size())
		{
			const QString code_file = a.at(idx_lint + 1);
			const QString result_file = a.at(idx_lint + 2);
			const QStringList lint_findings = FAUST2_lint_faust_code(read_file(code_file), true, FAUST_LLM_TEST_get_program_dir());
			write_file(result_file, lint_findings.join('\n'));
			return 0;
		}
		const int idx = a.indexOf(QStringLiteral("--compile-worker"));
		if (idx >= 0 && idx + 3 < a.size())
		{
			const QString code_file = a.at(idx + 1);
			const QString result_file = a.at(idx + 2);
			const int optlevel = a.at(idx + 3).toInt();

			FaustDev2CompileConfig config;
			config.code = read_file(code_file);
			config.options = QString::fromUtf8("-I\n%radium_path%/packages/faust/libraries");
			config.use_interpreter_backend = true;
			config.radium_path = FAUST_LLM_TEST_get_program_dir();

			Faust2CompileCheckResult result;
			FAUST2_compile_check(config, optlevel, &result);

			QString out = QStringLiteral("ok=%1\ninputs=%2\noutputs=%3\ninstrument=%4\nerror=")
			  .arg(result.ok ? 1 : 0).arg(result.num_inputs).arg(result.num_outputs).arg(result.is_instrument ? 1 : 0);
			out += result.error_message;
			write_file(result_file, out);
			return 0;
		}
	}

	Runner runner;
	QString code_file;
	QString compile_error_file;
	QString lint_only_file;
	QString lint_compile_file;
	QString compile_only_file;
	bool new_session = false;

	QStringList args = QCoreApplication::arguments();
	args.removeFirst();

	auto next_arg = [&args](int &i) -> QString
	{
		if (i + 1 < args.size())
		  return args.at(++i);
		return QString();
	};

	for (int i = 0; i < args.size(); i++)
	{
		const QString arg = args.at(i);

		if (arg == "--code")
		  code_file = next_arg(i);
		else if (arg == "--effect")
		  runner.is_effect = true;
		else if (arg == "--mono")
		{
			runner.is_effect = true;
			runner.effect_is_mono = true;
		}
		else if (arg == "--inputs")
		{
			const QString n = next_arg(i);
			runner.is_effect = true;
			runner.expected_effect_inputs = n.toInt();
			if (runner.expected_effect_inputs < 1)
			  runner.expected_effect_inputs = -1;
		}
		else if (arg == "--new-session")
		  new_session = true;
		else if (arg == "--compile-error")
		  compile_error_file = next_arg(i);
		else if (arg == "--output")
		  runner.output_file = next_arg(i);
		else if (arg == "--program-dir")
		{
			const QString dir = next_arg(i);
			if (!dir.isEmpty())
			  setenv("RADIUM_PROGRAM_DIR", dir.toUtf8().constData(), 1);
		}
		else if (arg == "--log")
		{
			const QString filename = next_arg(i);
			if (!filename.isEmpty())
			  setenv("RADIUM_LLM_LOG", filename.toUtf8().constData(), 1);
		}
		else if (arg == "--prompt")
		{
			const QString prompt = next_arg(i);
			if (!prompt.isEmpty())
			  runner.prompts.append(prompt);
		}
		else if (arg == "--prompt-file")
		{
			const QString filename = next_arg(i);
			QFile file(filename);
			if (file.open(QIODevice::ReadOnly | QIODevice::Text))
			{
				QTextStream in(&file);
				while (!in.atEnd())
				{
					const QString line = in.readLine().trimmed();
					if (!line.isEmpty())
					  runner.prompts.append(line);
				}
			}
			else
			{
				fprintf(stderr, "faust_llm_test: could not read prompt file %s\n", filename.toUtf8().constData());
				return 2;
			}
		}
		else if (arg == "--quiet")
		  runner.quiet = true;
		else if (arg == "--lint-only")
		  lint_only_file = next_arg(i);
		else if (arg == "--lint-compile")
		  lint_compile_file = next_arg(i);
		else if (arg == "--compile-only")
		  compile_only_file = next_arg(i);
		else if (arg == "--help" || arg == "-h")
		{
			printf("Usage: faust_llm_test [options] <prompt> [<prompt> ...]\n"
			       "  --code <file>          current program (default: FaustDev2 default program)\n"
			       "  --effect / --mono      effect conventions (mono implies effect)\n"
			       "  --inputs <n>           expected effect input count (-1 = infer from prompt)\n"
			       "  --new-session          first prompt is creation (like \"Clear history\")\n"
			       "  --compile-error <file> seed the first request with this compiler error\n"
			       "  --prompt <text>        add a prompt (same as a positional argument)\n"
			       "  --prompt-file <file>   one prompt per line, appended to positional prompts\n"
			       "  --output <file>        final program (default: stdout)\n"
			       "  --lint-only <file>     print the textual static-analysis findings for a program and exit\n"
			       "  --lint-compile <file>  print the compile-based static-analysis findings for a program and exit\n"
			       "  --compile-only <file>  compile a program and print its channel counts and exit\n"
			       "  --program-dir <dir>    Radium program dir (default <exe>/../bin)\n"
			       "  --log <file>           LLM log path (default $RADIUM_LLM_LOG or ~/.radium/llm.log)\n"
			       "  --quiet\n");
			return 0;
		}
		else if (arg.startsWith("--"))
		{
			fprintf(stderr, "faust_llm_test: unknown option %s\n", arg.toUtf8().constData());
			return 2;
		}
		else
		  runner.prompts.append(arg);
	}

	if (!lint_only_file.isEmpty())
	{
		const QString lint_code = read_file(lint_only_file);
		if (lint_code.isEmpty())
		{
			fprintf(stderr, "faust_llm_test: could not read code file %s\n", lint_only_file.toUtf8().constData());
			return 2;
		}
		const QString findings = radium::llm::lint_faust_code(lint_code);
		if (!findings.isEmpty())
		  printf("%s", findings.toUtf8().constData());
		return findings.isEmpty() ? 0 : 1;
	}

	if (!lint_compile_file.isEmpty())
	{
		const QString lint_code = read_file(lint_compile_file);
		if (lint_code.isEmpty())
		{
			fprintf(stderr, "faust_llm_test: could not read code file %s\n", lint_compile_file.toUtf8().constData());
			return 2;
		}
		const qint64 pid = QCoreApplication::applicationPid();
		const QString lint_code_file = QDir::tempPath() + QStringLiteral("/faust_llm_test_%1_lint.faust").arg(pid);
		const QString lint_result_file = QDir::tempPath() + QStringLiteral("/faust_llm_test_%1_lint.result").arg(pid);
		write_file(lint_code_file, lint_code);
		run_worker_guarded(QStringList() << QStringLiteral("--lint-compile-worker") << lint_code_file << lint_result_file,
		                   lint_result_file);
		const QString findings_text = read_file(lint_result_file);
		if (findings_text.startsWith(QStringLiteral("The Faust compiler used more than")))
		{
			fprintf(stderr, "ERROR: %s\n", findings_text.toUtf8().constData());
			return 2;
		}
		if (!findings_text.isEmpty())
		  printf("%s", findings_text.toUtf8().constData());
		return findings_text.isEmpty() ? 0 : 1;
	}

	if (!compile_only_file.isEmpty())
	{
		const QString compile_code = read_file(compile_only_file);
		if (compile_code.isEmpty())
		{
			fprintf(stderr, "faust_llm_test: could not read code file %s\n", compile_only_file.toUtf8().constData());
			return 2;
		}
		Faust2CompileCheckResult result = guarded_compile(compile_code, (int)SETTINGS_read_int("faust_optimization_level", 4));
		if (result.ok)
		{
			printf("OK: %d in / %d out; is_instrument=%d\n", result.num_inputs, result.num_outputs, result.is_instrument);
			return 0;
		}
		fprintf(stderr, "ERROR: %s\n", result.error_message.toUtf8().constData());
		return 1;
	}

	if (runner.prompts.isEmpty())
	{
		fprintf(stderr, "faust_llm_test: no prompt given. See --help.\n");
		return 2;
	}

	if (!QFileInfo::exists(FAUST_LLM_TEST_get_program_dir() + QString::fromUtf8("/packages/faust/libraries/stdfaust.lib")))
	{
		fprintf(stderr, "faust_llm_test: Faust libraries not found in %s/packages/faust/libraries. Use --program-dir.\n",
		        FAUST_LLM_TEST_get_program_dir().toUtf8().constData());
		return 2;
	}

	runner.code = code_file.isEmpty() ? FAUST2_get_default_code() : read_file(code_file);
	if (!code_file.isEmpty() && runner.code.isEmpty())
	{
		fprintf(stderr, "faust_llm_test: could not read code file %s\n", code_file.toUtf8().constData());
		return 2;
	}

	if (!compile_error_file.isEmpty())
	  runner.displayed_error = read_file(compile_error_file);

	radium::llm::FaustSessionHost host;

	host.get_code = [&runner]() -> QString
	{
		return runner.code;
	};

	host.set_code = [&runner](const QString &new_code)
	{
		runner.start_compile(new_code);
	};

	host.compile_error = [&runner]() -> QString
	{
		return runner.displayed_error;
	};

	host.is_compiling = [&runner]() -> bool
	{
		return runner.compiling;
	};

	host.compile_check_is_safe = [&runner]() -> bool
	{
		return !runner.compiling;
	};

	host.is_effect = [&runner]() -> bool
	{
		return runner.is_effect;
	};

	host.effect_is_mono_override = [&runner]() -> bool
	{
		return runner.effect_is_mono;
	};

	host.expected_effect_inputs_override = [&runner]() -> int
	{
		return runner.expected_effect_inputs;
	};

	host.radium_path = []() -> QString
	{
		return FAUST_LLM_TEST_get_program_dir();
	};

	host.default_code = []() -> QString
	{
		return FAUST2_get_default_code();
	};

	host.status = [&runner](const QString &text)
	{
		runner.log_status(text);
	};

	host.show_error = [&runner](const QString &message)
	{
		fprintf(stderr, "faust_llm_test: error: %s\n", message.toUtf8().constData());
	};

	host.set_generate_enabled = [](bool enabled) {};
	host.set_cancel_enabled = [](bool enabled) {};

	runner.session = std::unique_ptr<radium::llm::FaustSession>(new radium::llm::FaustSession(host));

	if (new_session)
	  runner.session->set_new_session();

	QObject::connect(&runner.watcher, &QFutureWatcherBase::finished, [&runner]()
	{
		runner.on_compile_done();
	});

	QObject::connect(&runner.idle_timer, &QTimer::timeout, [&runner]()
	{
		runner.check_idle();
	});
	runner.idle_timer.start(50);

	QTimer::singleShot(0, [&runner]()
	{
		runner.check_idle();
	});

	const int ret = app.exec();
	runner.session->shutdown();
	return ret;
}
