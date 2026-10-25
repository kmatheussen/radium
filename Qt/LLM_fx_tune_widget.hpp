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
#include <QColor>
#include <QFontMetrics>
#include <QFrame>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QTextCharFormat>
#include <QTextCursor>
#include <QTextDocument>
#include <QTextOption>
#include <QTimer>
#include <QToolButton>
#include <QVBoxLayout>
#include <QWidget>

#include <functional>
#include <utility>

#include "../api/radium_proc.h"

#include "FocusSniffers.h"
#include "Qt_MyQButton.h"
#include "Qt_MyQCheckBox.h"

#include "LLM_fx_tune.hpp"
#include "LLM_prompt_history.hpp"
#include "LLM_settings_dialog.hpp"


// Settings key for the instrument effect tuning prompt history.
static const char *LLM_FX_PROMPT_HISTORY_SETTINGS_KEY = "instrument_llm_prompt_history";


// The LLM effect tuning terminal shown at the bottom of the audio instrument
// widget. It has two modes: compact (one control row with a prompt) and
// transcript (the control row plus a scrollable 10-line transcript). The
// "Transcript" checkbox switches between them. Owns the FxTuneSession; the
// catalog reading and change applying is supplied by the instrument widget.
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

		_prompt_format.setFontWeight(QFont::Bold);
		_error_format.setForeground(QColor(200, 60, 60));

		QVBoxLayout *main_layout = new QVBoxLayout(this);
		main_layout->setContentsMargins(2, 1, 2, 1);
		main_layout->setSpacing(0);

		QFrame *frame = new QFrame(this);
		frame->setFrameShape(QFrame::Panel);
		frame->setFrameShadow(QFrame::Sunken);
		frame->setLineWidth(1);
		main_layout->addWidget(frame);

		QVBoxLayout *frame_layout = new QVBoxLayout(frame);
		frame_layout->setContentsMargins(2, 1, 2, 1);
		frame_layout->setSpacing(0);

		_transcript = new QPlainTextEdit(frame);
		_transcript->setReadOnly(true);
		_transcript->setFrameShape(QFrame::NoFrame);
		_transcript->setLineWrapMode(QPlainTextEdit::WidgetWidth);
		_transcript->setWordWrapMode(QTextOption::WrapAtWordBoundaryOrAnywhere);
		_transcript->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
		_transcript->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
		_transcript->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
		_transcript->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
		_transcript->setMinimumWidth(0);
		_transcript->setFixedHeight(terminal_output_height());
		_transcript->setToolTip("Conversation with the LLM. Scroll to see previous replies; click and drag to select text.");
		_transcript->installEventFilter(this);
		frame_layout->addWidget(_transcript);

		QHBoxLayout *controls_layout = new QHBoxLayout;
		controls_layout->setContentsMargins(0, 0, 0, 0);
		controls_layout->setSpacing(2);

		_menu_button = new MyQButton(frame);
		_menu_button->setText(QString::fromUtf8("\xe2\x98\xb0")); // ☰
		_menu_button->setToolTip("Menu");
		_menu_button->setPopupMode(QToolButton::InstantPopup);
		{
			QMenu *menu = new QMenu(_menu_button);
			menu->addAction("Clear conversation", this, [this]()
			{
				_session.clear_history();
				_transcript->clear();
				set_status("Cleared the conversation.");
			});
			menu->addAction("LLM settings", this, [this]()
			{
				radium::llm::show_settings_dialog(this);
			});
			_menu_button->setMenu(menu);
		}

		_transcript_checkbox = new MyQCheckBox("Transcript", frame);
		_transcript_checkbox->setToolTip("Show the 10-line LLM conversation. Unchecked = compact one-line mode.");
		controls_layout->addWidget(_transcript_checkbox);

		QLabel *prompt_marker = new QLabel(">", frame);
		prompt_marker->setToolTip("Prompt");
		{
			QFont marker_font = prompt_marker->font();
			marker_font.setBold(true);
			prompt_marker->setFont(marker_font);

			// Blend the prompt marker with the transcript/input background.
			prompt_marker->setAutoFillBackground(true);
			QPalette marker_palette = prompt_marker->palette();
			marker_palette.setColor(QPalette::Window, marker_palette.color(QPalette::Base));
			prompt_marker->setPalette(marker_palette);
		}
		controls_layout->addWidget(prompt_marker);

		_prompt_edit = new FocusSnifferQLineEdit(frame);
		_prompt_edit->setFrame(false);
		_prompt_edit->setPlaceholderText("Tune effects. E.g. \"make it sound harsher\" / \"wider\" / \"add an octave layer\" / \"more reverb\"");
		_prompt_edit->setToolTip("Describe the sound you want, and the LLM adjusts the instrument's system effects (EQ, filters, compressor, pitch, pan/width, reverb/chorus sends and the note duplicator).\n"
		                         "The changes are applied immediately; one undo step restores them all. Follow-up requests refine the result.");
		_prompt_edit->installEventFilter(this);
		controls_layout->addWidget(_prompt_edit, 1);

		_tune_button = new QPushButton("Tune", frame);
		_tune_button->setToolTip("Ask the LLM to tune the effects");
		controls_layout->addWidget(_tune_button);

		_cancel_button = new QPushButton("Cancel", frame);
		_cancel_button->setEnabled(false);
		controls_layout->addWidget(_cancel_button);

		controls_layout->addWidget(_menu_button);

		frame_layout->addLayout(controls_layout);

		QObject::connect(_tune_button, &QPushButton::clicked, [this](){ submit_prompt(); });
		QObject::connect(_cancel_button, &QPushButton::clicked, [this](){ _session.cancel(); });
		QObject::connect(_prompt_edit, &QLineEdit::returnPressed, [this](){ submit_prompt(); });
		QObject::connect(_transcript_checkbox, &QCheckBox::toggled, [this](bool visible){ set_transcript_visible(visible); });

		// Start in compact mode.
		_transcript_checkbox->setChecked(false);
		set_transcript_visible(false);
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
		if (event->type() == QEvent::KeyPress)
		{
			QKeyEvent *key_event = static_cast<QKeyEvent*>(event);

			if (watched == _prompt_edit)
			{
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

			// Clicking the transcript is useful for selecting/copying output,
			// but typing there does nothing. Escape returns to the input line.
			if (watched == _transcript && key_event->key() == Qt::Key_Escape)
			{
				_prompt_edit->setFocus();
				return true;
			}
		}

		return QWidget::eventFilter(watched, event);
	}

	void changeEvent(QEvent *event) override
	{
		if (event->type() == QEvent::FontChange && _transcript != NULL)
			_transcript->setFixedHeight(terminal_output_height());

		QWidget::changeEvent(event);
	}

private:
	void submit_prompt(void)
	{
		const QString prompt = _prompt_edit->text().trimmed();
		if (prompt.isEmpty() || _session.is_busy())
			return;

		append_terminal_text("> " + prompt, _prompt_format);
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

	int terminal_output_height(void) const
	{
		const QFontMetrics fm(_transcript->font());
		return fm.lineSpacing() * LLM_FX_TERMINAL_OUTPUT_LINES
		     + (int)(_transcript->document()->documentMargin() * 2)
		     + _transcript->frameWidth() * 2
		     + 2;
	}

	// Appends one conversation entry to the terminal. The entries wrap at
	// the widget width, so neither the text nor the widget can widen the
	// instrument window.
	void append_terminal_text(const QString &text, const QTextCharFormat &format)
	{
		QScrollBar *scroll_bar = _transcript->verticalScrollBar();
		const bool was_at_bottom = scroll_bar->value() >= scroll_bar->maximum() - 2;

		QTextCursor cursor(_transcript->document());
		cursor.movePosition(QTextCursor::End);
		if (!_transcript->document()->isEmpty())
			cursor.insertBlock();

		const QStringList lines = text.split('\n');
		for (int i = 0; i < lines.size(); i++)
		{
			if (i > 0)
				cursor.insertBlock();
			cursor.insertText(lines.at(i), format);
		}

		if (was_at_bottom)
			QTimer::singleShot(0, this, [this]()
			{
				QScrollBar *bar = _transcript->verticalScrollBar();
				bar->setValue(bar->maximum());
			});
	}

	void set_transcript_visible(bool visible)
	{
		_transcript->setVisible(visible);
	}

	void set_status(const QString &text)
	{
		if (!text.isEmpty())
			append_terminal_text(text, _status_format);
	}

	void show_error(const QString &message)
	{
		append_terminal_text("Error: " + message, _error_format);
	}

	void set_busy(bool busy)
	{
		_tune_button->setEnabled(!busy);
		_prompt_edit->setEnabled(!busy);
		_menu_button->setEnabled(!busy);
		_cancel_button->setEnabled(busy);
	}

	// The transcript shows 9 text rows; the prompt entry row below counts as
	// the 10th row. Compact mode shows only that row.
	static constexpr int LLM_FX_TERMINAL_OUTPUT_LINES = 9;

	radium::llm::FxTuneSession _session;

	MyQButton *_menu_button = NULL;
	MyQCheckBox *_transcript_checkbox = NULL;
	QPlainTextEdit *_transcript = NULL;
	FocusSnifferQLineEdit *_prompt_edit = NULL;
	QPushButton *_tune_button = NULL;
	QPushButton *_cancel_button = NULL;

	QTextCharFormat _prompt_format;
	QTextCharFormat _status_format;
	QTextCharFormat _error_format;

	QStringList _prompt_history;
	int _prompt_history_index = -1;
};
