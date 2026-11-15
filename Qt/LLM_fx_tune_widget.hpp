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
#include <QFontDatabase>
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
#include <QShortcut>
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
#include "Qt_colors_proc.h"
#include "Qt_MyQButton.h"
#include "Qt_MyQCheckBox.h"

#include "LLM_fx_tune.hpp"
#include "LLM_prompt_history.hpp"
#include "LLM_settings_dialog.hpp"


// Settings key for the instrument effect tuning prompt history.
static const char *LLM_FX_PROMPT_HISTORY_SETTINGS_KEY = "instrument_llm_prompt_history";


// The LLM effect tuning terminal shown at the bottom of the audio instrument
// widget: a scrollable 10-line transcript above a control row with a prompt.
// The widget is shown/hidden by the "LLM" checkbox in the instrument header.
// Owns the FxTuneSession; the catalog reading and change applying is supplied
// by the instrument widget.
class LLM_fx_tune_widget : public QWidget
{
public:
	LLM_fx_tune_widget(QWidget *parent,
	                   std::function<QVector<radium::llm::FxParamState>()> read_catalog,
	                   std::function<radium::llm::FxInstrumentContext()> read_instrument_context,
	                   std::function<QVector<radium::llm::FxPlugin>()> read_plugin_catalog,
	                   std::function<QVector<radium::llm::FxPlugin>()> read_inserted_plugins,
	                   std::function<bool(QVector<radium::llm::FxChange> &changes,
	                                      QVector<radium::llm::FxInstrumentChange> &instrument_changes,
	                                      const QVector<radium::llm::FxModulation> &modulations,
	                                      const QVector<radium::llm::FxPlugin> &add_plugins,
	                                      const QVector<radium::llm::FxPlugin> &remove_plugins)> apply_reply,
	                   std::function<bool(const QString &description, const QString &name)> create_faust_effect,
	                   std::function<bool(const QString &name)> switch_to_instrument)
		: QWidget(parent)
		, _session(radium::llm::FxTuneSessionHost{
			std::move(read_catalog),
			std::move(read_instrument_context),
			std::move(read_plugin_catalog),
			std::move(read_inserted_plugins),
			std::move(apply_reply),
			std::move(create_faust_effect),
			std::move(switch_to_instrument),
			[this](const QString &text){ set_status(text); },
			[this](const QString &message){ show_error(message); },
			[this](bool busy){ set_busy(busy); },
		})
	{
		setObjectName("llm_fx_tune_widget");
		setAutoFillBackground(true);

		_prompt_history = radium::llm::load_prompt_history(LLM_FX_PROMPT_HISTORY_SETTINGS_KEY);

		_prompt_format.setFontWeight(QFont::Bold);
		_question_format.setFontWeight(QFont::Bold);
		_error_format.setForeground(QColor(200, 60, 60));

		// No margins and no frame: the terminal uses the full width of the
		// instrument widget.
		QVBoxLayout *main_layout = new QVBoxLayout(this);
		main_layout->setContentsMargins(0, 0, 0, 0);
		main_layout->setSpacing(0);

		QFrame *frame = new QFrame(this);
		frame->setFrameShape(QFrame::NoFrame);
		frame->setFrameShadow(QFrame::Plain);
		frame->setLineWidth(0);
		frame->setAutoFillBackground(true);
		main_layout->addWidget(frame);

		QVBoxLayout *frame_layout = new QVBoxLayout(frame);
		frame_layout->setContentsMargins(0, 0, 0, 0);
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

		// Terminal-style answer row directly below the transcript. Flat
		// monospace buttons make it read as part of the terminal instead of
		// as a form next to the prompt. The row is only shown while a
		// confirmation question is pending.
		QHBoxLayout *confirmation_layout = new QHBoxLayout;
		confirmation_layout->setContentsMargins(0, 0, 0, 0);
		confirmation_layout->setSpacing(2);

		const QFont answer_font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
		_confirm_yes_button = new QPushButton("[X] Yes", frame);
		_confirm_yes_button->setFont(answer_font);
		_confirm_yes_button->setFlat(true);
		_confirm_yes_button->setAutoDefault(false);
		_confirm_yes_button->setFocusPolicy(Qt::StrongFocus);
		_confirm_yes_button->setToolTip("Confirm the question above. Use Left/Right to switch, Return to select.");
		_confirm_yes_button->installEventFilter(this);
		confirmation_layout->addWidget(_confirm_yes_button);

		_confirm_no_button = new QPushButton("[ ] No", frame);
		_confirm_no_button->setFont(answer_font);
		_confirm_no_button->setFlat(true);
		_confirm_no_button->setAutoDefault(false);
		_confirm_no_button->setFocusPolicy(Qt::StrongFocus);
		_confirm_no_button->setToolTip("Decline the question above. Use Left/Right to switch, Return to select.");
		_confirm_no_button->installEventFilter(this);
		confirmation_layout->addWidget(_confirm_no_button);

		confirmation_layout->addStretch();
		frame_layout->addLayout(confirmation_layout);
		_confirm_yes_button->hide();
		_confirm_no_button->hide();

		// The normal prompt row. It is hidden while a confirmation is pending,
		// so the keyboard question row replaces it instead of sitting next to
		// it.
		_controls_widget = new QWidget(frame);
		QHBoxLayout *controls_layout = new QHBoxLayout(_controls_widget);
		controls_layout->setContentsMargins(0, 0, 0, 0);
		controls_layout->setSpacing(2);

		_menu_button = new MyQButton(frame);
		_menu_button->setText(QString::fromUtf8("\xe2\x98\xb0")); // ☰
		_menu_button->setToolTip("Menu");
		_menu_button->setPopupMode(QToolButton::InstantPopup);
		// MyQButton draws its box one pixel inside the widget by default, which
		// would make the menu button look smaller than the Tune/Cancel buttons
		// next to it. Let it fill the whole widget instead.
		_menu_button->x1_border = 0;
		_menu_button->x2_border = 0;
		_menu_button->y1_border = 0;
		_menu_button->y2_border = 0;
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

		_prompt_marker = new QLabel(">", frame);
		_prompt_marker->setToolTip("Prompt");
		{
			QFont marker_font = _prompt_marker->font();
			marker_font.setBold(true);
			_prompt_marker->setFont(marker_font);

			// Blend the prompt marker with the transcript/input background.
			_prompt_marker->setAutoFillBackground(true);
			QPalette marker_palette = _prompt_marker->palette();
			marker_palette.setColor(QPalette::Window, marker_palette.color(QPalette::Base));
			_prompt_marker->setPalette(marker_palette);
		}
		controls_layout->addWidget(_prompt_marker);

		_prompt_edit = new FocusSnifferQLineEdit(frame);
		_prompt_edit->setFrame(false);
		// Rounded corners, like the buttons next to it. palette(base) keeps the
		// background the same color as the rest of the input, so it follows
		// palette/theme changes instead of using a fixed color.
		_prompt_edit->setStyleSheet("QLineEdit { border: 0; border-radius: 4px; background-color: palette(base); }");
		_prompt_edit->setPlaceholderText("Tune instrument controls/effects. E.g. \"boost the 2nd harmonic\" / \"make it harsher\" / \"wider\"");
		_prompt_edit->setToolTip("Describe the sound you want, and the LLM adjusts the current instrument's own plugin controls as well as its system effects (EQ, filters, compressor, pitch, pan/width, reverb/chorus sends and the note duplicator).\n"
		                         "For effects the instrument and system controls cannot provide (distortion, wah, phaser, delay, ...), it can insert an available effect plugin after the instrument, and remove it again on request.\n"
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

		frame_layout->addWidget(_controls_widget);

		QObject::connect(_tune_button, &QPushButton::clicked, [this](){ submit_prompt(); });
		QObject::connect(_cancel_button, &QPushButton::clicked, [this](){ _session.cancel(); });
		QObject::connect(_prompt_edit, &QLineEdit::returnPressed, [this](){ submit_prompt(); });
		QObject::connect(_confirm_yes_button, &QPushButton::clicked, [this](){ answer_confirmation(true); });
		QObject::connect(_confirm_no_button, &QPushButton::clicked, [this](){ answer_confirmation(false); });

		// The confirmation is keyboard-only. Window shortcuts make Left,
		// Right, Return and Escape work no matter which control in the window
		// has focus; they are enabled only while a question is pending.
		_left_shortcut = new QShortcut(QKeySequence(Qt::Key_Left), this);
		_right_shortcut = new QShortcut(QKeySequence(Qt::Key_Right), this);
		_return_shortcut = new QShortcut(QKeySequence(Qt::Key_Return), this);
		_enter_shortcut = new QShortcut(QKeySequence(Qt::Key_Enter), this);
		_escape_shortcut = new QShortcut(QKeySequence(Qt::Key_Escape), this);
		QShortcut *confirmation_shortcuts[] = {_left_shortcut, _right_shortcut, _return_shortcut, _enter_shortcut, _escape_shortcut};
		for (QShortcut *shortcut : confirmation_shortcuts)
		{
			shortcut->setContext(Qt::WindowShortcut);
			shortcut->setEnabled(false);
		}
		QObject::connect(_left_shortcut, &QShortcut::activated, [this](){ if (_confirmation_pending) set_confirmation_choice(0); });
		QObject::connect(_right_shortcut, &QShortcut::activated, [this](){ if (_confirmation_pending) set_confirmation_choice(1); });
		QObject::connect(_return_shortcut, &QShortcut::activated, [this](){ if (_confirmation_pending) answer_confirmation(_confirmation_choice == 0); });
		QObject::connect(_enter_shortcut, &QShortcut::activated, [this](){ if (_confirmation_pending) answer_confirmation(_confirmation_choice == 0); });
		QObject::connect(_escape_shortcut, &QShortcut::activated, [this](){ if (_confirmation_pending) answer_confirmation(false); });

		// Focus changes (fullscreen toggling, window activation, an editor
		// focus update scheduled by Radium) can silently release the keyboard
		// focus we obtained for the question. Re-assert it while pending.
		_confirmation_timer = new QTimer(this);
		_confirmation_timer->setInterval(50);
		QObject::connect(_confirmation_timer, &QTimer::timeout, [this](){ ensure_confirmation_keyboard_focus(); });

		// Applies the current palette to the terminal. updatePalette() gives
		// widgets inside this widget the configured LLM background color, so a
		// later global updateAll() updates it without using a stylesheet.
		setWidgetColors(this);

		update_control_row_heights();
	}

	~LLM_fx_tune_widget()
	{
		if (measurement_active_callback)
		  measurement_active_callback(false);
		if (_confirmation_timer != NULL)
			_confirmation_timer->stop();
		set_confirmation_keyboard_focus(false);
		_session.shutdown();
	}

	// Called with true when the terminal becomes visible and with false when it
	// is hidden (checkbox off, instrument widget closed, or deleted). The host
	// uses it to start/stop capturing the instrument's output for the
	// measurements, so nothing is done in the audio thread unless the user is
	// actually looking at the tuner.
	std::function<void(bool)> measurement_active_callback;

protected:
	void showEvent(QShowEvent *event) override
	{
		QWidget::showEvent(event);
		if (measurement_active_callback)
		  measurement_active_callback(true);

		// The question may have arrived while this widget (or its instrument
		// tab) was hidden. Take the keyboard only once it can be seen.
		if (_confirmation_pending) {
			ensure_confirmation_keyboard_focus();
			_confirmation_timer->start();
		} else {
			// Opening the LLM terminal should be enough to start typing.
			focus_prompt_if_visible();
		}
	}

	void hideEvent(QHideEvent *event) override
	{
		QWidget::hideEvent(event);
		if (measurement_active_callback)
		  measurement_active_callback(false);
		if (_confirmation_timer != NULL)
			_confirmation_timer->stop();
		set_confirmation_keyboard_focus(false);
	}

public:

	// Must be called before the widget is deleted (from the owning
	// instrument widget) so no pending LLM callback touches anything.
	void prepare_for_deletion(void)
	{
		if (measurement_active_callback)
		  measurement_active_callback(false);
		if (_confirmation_timer != NULL)
			_confirmation_timer->stop();
		set_confirmation_keyboard_focus(false);
		_confirmation_pending = false;
		_confirmation_callback = nullptr;
		_session.shutdown();
	}

	// Asks a yes/no question in the terminal instead of opening a modal
	// dialog. The callback runs after the user chooses Yes or No. While the
	// question is pending, the prompt row is replaced by the two keyboard
	// buttons and Radium's keyboard focus is held by this widget, so the
	// editor cannot consume the arrow keys. Returns false if a question is
	// already pending.
	bool ask_confirmation(const QString &question, std::function<void(bool)> callback)
	{
		if (_confirmation_pending)
			return false;

		append_terminal_text(question, _question_format);
		_confirmation_callback = std::move(callback);
		_confirmation_pending = true;
		_confirmation_choice = 0;
		update_controls();
		return true;
	}

	// Appends a status line to the transcript. Used by the host for messages
	// that happen after the user answered a confirmation (for example when
	// the Faust effect generation starts), so they stay in the conversation
	// instead of appearing in a separate message window.
	void add_terminal_status(const QString &text)
	{
		set_status(text);
	}

	// Called by the host once a new instrument has actually been created, so
	// later turns know about it.
	void note_instrument_created(const QString &name)
	{
		_session.note_created_instrument(name);
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

			if (_confirmation_pending && (watched == _confirm_yes_button || watched == _confirm_no_button))
			{
				if (key_event->key() == Qt::Key_Left || key_event->key() == Qt::Key_Right)
				{
					set_confirmation_choice(key_event->key() == Qt::Key_Left ? 0 : 1);
					return true;
				}
				if (key_event->key() == Qt::Key_Return || key_event->key() == Qt::Key_Enter)
				{
					answer_confirmation(_confirmation_choice == 0);
					return true;
				}
				if (key_event->key() == Qt::Key_Escape)
				{
					answer_confirmation(false);
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
		if (event->type() == QEvent::FontChange)
		{
			if (_transcript != NULL)
				_transcript->setFixedHeight(terminal_output_height());
			update_control_row_heights();
		}

		QWidget::changeEvent(event);
	}

private:
	void submit_prompt(void)
	{
		const QString prompt = _prompt_edit->text().trimmed();
		if (prompt.isEmpty() || _session.is_busy())
			return;

		if (_confirmation_pending)
		{
			// A typed "Yes"/"No" answers the question. It must not be sent to
			// the LLM: the model would only propose the effect again and the
			// host would ask a second time.
			const int answer = radium::llm::fx_parse_confirmation_answer(prompt);
			if (answer >= 0)
			{
				append_terminal_text("> " + prompt, _prompt_format);
				_prompt_edit->clear();
				_prompt_history_index = -1;
				answer_confirmation(answer == 1);
				return;
			}

			// Typing a new request instead of answering the pending
			// confirmation means the user does not want the effect. Drop the
			// question so the conversation can continue.
			answer_confirmation(false);
		}

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

	// Every widget in the prompt and confirmation rows must have the same
	// top and bottom. Qt's frameless QLineEdit and the QToolButton-based menu
	// button get a smaller height hint than a QPushButton, so the tallest hint
	// is applied to all of them. Doing it on FontChange keeps the rows aligned
	// when the font (and with it the button height) changes.
	void update_control_row_heights(void)
	{
		QWidget *const widgets[] = {
			_prompt_marker,
			_prompt_edit,
			_tune_button,
			_cancel_button,
			_menu_button,
			_confirm_yes_button,
			_confirm_no_button,
		};

		int height = 0;
		for (QWidget *widget : widgets)
			if (widget != NULL && widget->sizeHint().height() > height)
				height = widget->sizeHint().height();

		if (height <= 0)
			return;

		for (QWidget *widget : widgets)
			if (widget != NULL)
				widget->setFixedHeight(height);
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
		_busy = busy;
		update_controls();

		// Disabling the prompt while a request runs makes it lose keyboard
		// focus. Give it back when the reply has arrived, so the user can
		// continue typing immediately (the confirmation selector keeps focus
		// when a question is pending).
		if (!busy)
			focus_prompt_if_visible();
	}

	void update_controls(void)
	{
		const bool pending = _confirmation_pending && !_busy;

		// While the question is pending, the prompt row is replaced by the
		// keyboard Yes/No row.
		_controls_widget->setVisible(!pending);
		_confirm_yes_button->setVisible(pending);
		_confirm_no_button->setVisible(pending);

		_left_shortcut->setEnabled(pending);
		_right_shortcut->setEnabled(pending);
		_return_shortcut->setEnabled(pending);
		_enter_shortcut->setEnabled(pending);
		_escape_shortcut->setEnabled(pending);

		_tune_button->setEnabled(!_busy);
		_prompt_edit->setEnabled(!_busy);
		_menu_button->setEnabled(!_busy);
		_cancel_button->setEnabled(_busy);

		if (pending) {
			ensure_confirmation_keyboard_focus();
			_confirmation_timer->start();
		} else {
			_confirmation_timer->stop();
			set_confirmation_keyboard_focus(false);
		}
	}

	// Gives the prompt Qt focus and Radium's keyboard focus. setFocus alone is
	// not enough when some other code (creating a Faust effect, for example)
	// released Radium's keyboard focus while the prompt already had Qt focus:
	// then no focusIn event arrives and the cursor blinks without receiving
	// key events.
	void focus_prompt_if_visible(void)
	{
		if (_prompt_edit == NULL || !isVisible() || _confirmation_pending || _busy)
			return;

		_controls_widget->setVisible(true);
		_prompt_edit->setFocus(Qt::OtherFocusReason);
		obtain_keyboard_focus();
	}

	// The prompt row's focus sniffers release Radium's keyboard focus when
	// they are hidden. Without holding it again, Radium's native key filter
	// sends the arrow keys to the editor and the confirmation buttons never
	// see them. Fullscreen toggling and Radium's own editor focus updates can
	// release it again, hence the repeating timer in update_controls().
	void ensure_confirmation_keyboard_focus(void)
	{
		if (!_confirmation_pending || !isVisible())
			return;

		_confirmation_keyboard_focus_obtained = true;
		obtain_keyboard_focus();
		update_confirmation_choice();
	}

	void set_confirmation_keyboard_focus(bool active)
	{
		if (active == _confirmation_keyboard_focus_obtained)
			return;

		_confirmation_keyboard_focus_obtained = active;
		if (active)
			obtain_keyboard_focus();
		else
			release_keyboard_focus();
	}

	void set_confirmation_choice(int choice)
	{
		_confirmation_choice = choice == 0 ? 0 : 1;
		update_confirmation_choice();
	}

	void update_confirmation_choice(void)
	{
		if (_confirm_yes_button == NULL || _confirm_no_button == NULL)
			return;

		_confirm_yes_button->setText(_confirmation_choice == 0 ? "[X] Yes" : "[ ] Yes");
		_confirm_no_button->setText(_confirmation_choice == 1 ? "[X] No" : "[ ] No");

		QPushButton *selected = _confirmation_choice == 0 ? _confirm_yes_button : _confirm_no_button;
		if (!selected->hasFocus())
			selected->setFocus();
	}

	void answer_confirmation(bool yes)
	{
		if (!_confirmation_pending)
			return;

		_confirmation_pending = false;
		std::function<void(bool)> callback = std::move(_confirmation_callback);
		_confirmation_callback = nullptr;

		if (!yes)
			append_terminal_text("Not creating the effect.", _status_format);

		update_controls();

		// Run the callback first: creating the Faust Dev 2 instrument can
		// release Radium's keyboard focus. Re-assert the prompt focus and
		// keyboard ownership afterwards, otherwise the cursor blinks but
		// typing goes to the editor.
		if (callback)
			callback(yes);

		update_controls();
		focus_prompt_if_visible();
	}

	// The transcript shows 9 text rows; the prompt entry row below counts as
	// the 10th row.
	static constexpr int LLM_FX_TERMINAL_OUTPUT_LINES = 9;

	radium::llm::FxTuneSession _session;

	MyQButton *_menu_button = NULL;
	QWidget *_controls_widget = NULL;
	QLabel *_prompt_marker = NULL;
	QShortcut *_left_shortcut = NULL;
	QShortcut *_right_shortcut = NULL;
	QShortcut *_return_shortcut = NULL;
	QShortcut *_enter_shortcut = NULL;
	QShortcut *_escape_shortcut = NULL;
	QTimer *_confirmation_timer = NULL;
	QPlainTextEdit *_transcript = NULL;
	FocusSnifferQLineEdit *_prompt_edit = NULL;
	QPushButton *_tune_button = NULL;
	QPushButton *_cancel_button = NULL;
	QPushButton *_confirm_yes_button = NULL;
	QPushButton *_confirm_no_button = NULL;

	QTextCharFormat _prompt_format;
	QTextCharFormat _question_format;
	QTextCharFormat _status_format;
	QTextCharFormat _error_format;

	QStringList _prompt_history;
	int _prompt_history_index = -1;
	bool _busy = false;
	bool _confirmation_pending = false;
	int _confirmation_choice = 0;
	bool _confirmation_keyboard_focus_obtained = false;
	std::function<void(bool)> _confirmation_callback;
};
