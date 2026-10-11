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

#include <QEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QPushButton>
#include <QToolButton>
#include <QVBoxLayout>
#include <QWidget>

#include <functional>
#include <utility>

#include "../api/radium_proc.h"

#include "FocusSniffers.h"
#include "Qt_MyQButton.h"

#include "LLM_fx_tune.hpp"
#include "LLM_prompt_history.hpp"
#include "LLM_settings_dialog.hpp"


// Settings key for the instrument effect tuning prompt history.
static const char *LLM_FX_PROMPT_HISTORY_SETTINGS_KEY = "instrument_llm_prompt_history";


// The LLM effect tuning prompt bar shown at the bottom of the audio
// instrument widget: a menu button, a prompt entry, Tune/Cancel buttons and
// a status line. Owns the FxTuneSession; the catalog reading and change
// applying is supplied by the instrument widget.
class LLM_fx_tune_widget : public QWidget
{
public:
	LLM_fx_tune_widget(QWidget *parent,
	                   std::function<QVector<radium::llm::FxParamState>()> read_catalog,
	                   std::function<bool(QVector<radium::llm::FxChange> &changes, const QVector<radium::llm::FxModulation> &modulations)> apply_reply)
		: QWidget(parent)
		, _session(radium::llm::FxTuneSessionHost{
			std::move(read_catalog),
			std::move(apply_reply),
			[this](const QString &text){ set_status(text); },
			[this](const QString &message){ show_error(message); },
			[this](bool busy){ set_busy(busy); },
		})
	{
		_prompt_history = radium::llm::load_prompt_history(LLM_FX_PROMPT_HISTORY_SETTINGS_KEY);

		QVBoxLayout *main_layout = new QVBoxLayout(this);
		main_layout->setContentsMargins(2, 1, 2, 1);
		main_layout->setSpacing(0);

		QHBoxLayout *controls_layout = new QHBoxLayout;
		controls_layout->setContentsMargins(0, 0, 0, 0);
		controls_layout->setSpacing(2);

		_menu_button = new MyQButton(this);
		_menu_button->setText(QString::fromUtf8("\xe2\x98\xb0")); // ☰
		_menu_button->setToolTip("Menu");
		_menu_button->setPopupMode(QToolButton::InstantPopup);
		{
			QMenu *menu = new QMenu(_menu_button);
			menu->addAction("Clear conversation", this, [this]()
			{
				_session.clear_history();
				set_status("Cleared the conversation.");
			});
			menu->addAction("LLM settings", this, [this]()
			{
				radium::llm::show_settings_dialog(this);
			});
			_menu_button->setMenu(menu);
		}
		controls_layout->addWidget(_menu_button);

		_prompt_edit = new FocusSnifferQLineEdit(this);
		_prompt_edit->setPlaceholderText("Tune effects. E.g. \"make it sound harsher\" / \"wider\" / \"add an octave layer\" / \"more reverb\"");
		_prompt_edit->setToolTip("Describe the sound you want, and the LLM adjusts the instrument's system effects (EQ, filters, compressor, pitch, pan/width, reverb/chorus sends and the note duplicator).\n"
		                         "The changes are applied immediately; one undo step restores them all. Follow-up requests refine the result.");
		_prompt_edit->installEventFilter(this);
		controls_layout->addWidget(_prompt_edit, 1);

		_tune_button = new QPushButton("Tune", this);
		_tune_button->setToolTip("Ask the LLM to tune the effects");
		controls_layout->addWidget(_tune_button);

		_cancel_button = new QPushButton("Cancel", this);
		_cancel_button->setEnabled(false);
		controls_layout->addWidget(_cancel_button);

		main_layout->addLayout(controls_layout);

		_status_label = new QLabel(this);
		_status_label->setWordWrap(false);
		_status_label->setTextInteractionFlags(Qt::TextSelectableByMouse);
		main_layout->addWidget(_status_label);

		QObject::connect(_tune_button, &QPushButton::clicked, [this](){ submit_prompt(); });
		QObject::connect(_cancel_button, &QPushButton::clicked, [this](){ _session.cancel(); });
		QObject::connect(_prompt_edit, &QLineEdit::returnPressed, [this](){ submit_prompt(); });
	}

	~LLM_fx_tune_widget()
	{
		_session.shutdown();
	}

	// Must be called before the widget is deleted (from the owning
	// instrument widget) so no pending LLM callback touches anything.
	void prepare_for_deletion(void)
	{
		_session.shutdown();
	}

	bool is_busy(void) const
	{
		return _session.is_busy();
	}

protected:
	bool eventFilter(QObject *watched, QEvent *event) override
	{
		if (watched == _prompt_edit && event->type() == QEvent::KeyPress)
		{
			QKeyEvent *key_event = static_cast<QKeyEvent*>(event);
			if (key_event->key() == Qt::Key_Up)
			{
				navigate_prompt_history(true);
				return true;
			}
			if (key_event->key() == Qt::Key_Down)
			{
				navigate_prompt_history(false);
				return true;
			}
		}

		return QWidget::eventFilter(watched, event);
	}

private:
	void submit_prompt(void)
	{
		const QString prompt = _prompt_edit->text().trimmed();
		if (prompt.isEmpty() || _session.is_busy())
			return;

		remember_prompt(prompt);
		_prompt_edit->clear();
		_prompt_history_index = -1;
		_session.tune(prompt);
	}

	void remember_prompt(const QString &prompt)
	{
		// Reload the stored list and merge, since several instrument widgets
		// share the same history; otherwise submitting here would overwrite
		// prompts submitted from another instrument.
		QStringList history = radium::llm::load_prompt_history(LLM_FX_PROMPT_HISTORY_SETTINGS_KEY);
		for (const QString &entry : _prompt_history)
			if (!history.contains(entry))
				history.append(entry);
		if (history.isEmpty() || history.last() != prompt)
			history.append(prompt);
		while (history.size() > radium::llm::LLM_PROMPT_HISTORY_MAX_SIZE)
			history.removeFirst();
		radium::llm::save_prompt_history(LLM_FX_PROMPT_HISTORY_SETTINGS_KEY, history);
		_prompt_history = history;
		_prompt_history_index = -1;
	}

	void navigate_prompt_history(bool up)
	{
		if (_prompt_history.isEmpty())
			return;

		if (up)
		{
			if (_prompt_history_index == -1)
				_prompt_history_index = _prompt_history.size() - 1;
			else if (_prompt_history_index > 0)
				_prompt_history_index--;
		}
		else
		{
			if (_prompt_history_index == -1)
				return;
			_prompt_history_index++;
			if (_prompt_history_index >= _prompt_history.size())
				_prompt_history_index = -1;
		}

		if (_prompt_history_index == -1)
			_prompt_edit->setText("");
		else
			_prompt_edit->setText(_prompt_history.at(_prompt_history_index));
	}

	void set_status(const QString &text)
	{
		_status_label->setText(text);
		_status_label->setToolTip(text);
	}

	void show_error(const QString &message)
	{
		set_status("");
		showAsyncMessage(message.toUtf8().constData());
	}

	void set_busy(bool busy)
	{
		_tune_button->setEnabled(!busy);
		_prompt_edit->setEnabled(!busy);
		_menu_button->setEnabled(!busy);
		_cancel_button->setEnabled(busy);
	}

	radium::llm::FxTuneSession _session;

	MyQButton *_menu_button;
	FocusSnifferQLineEdit *_prompt_edit;
	QPushButton *_tune_button;
	QPushButton *_cancel_button;
	QLabel *_status_label;

	QStringList _prompt_history;
	int _prompt_history_index = -1;
};
