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

#include <QButtonGroup>
#include <QColor>
#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPushButton>
#include <QRadioButton>
#include <QSignalBlocker>
#include <QSpinBox>
#include <QVBoxLayout>

#include "helpers.h"
#include "FocusSniffers.h"
#include "Qt_mix_colors.h"
#include "LLM_client.hpp"
#include "../common/settings_proc.h"


namespace radium
{
namespace llm
{

// The shared LLM settings dialog (Free/Custom provider, base URL, model, API
// key, thinking effort, reasoning cut-offs, max compile-fix attempts). Every
// change is saved immediately. Used by the Faust Dev 2 prompt bar and by the
// instrument effect tuning prompt bar.
static inline void show_settings_dialog(QWidget *parent)
{
    const radium::llm::LLMConfig config = radium::llm::get_dialog_config();

    RememberGeometryQDialog *dialog = new RememberGeometryQDialog(parent, radium::NOT_MODAL);
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->setWindowTitle("LLM settings");

    QVBoxLayout *layout = new QVBoxLayout(dialog);

    auto add_row = [layout](const char *label, QWidget *edit)
    {
      layout->addWidget(new QLabel(label));
      layout->addWidget(edit);
    };

    QRadioButton *free_radio = new QRadioButton("Free");
    QRadioButton *custom_radio = new QRadioButton("Custom");
    QButtonGroup *mode_group = new QButtonGroup(dialog);
    mode_group->addButton(free_radio);
    mode_group->addButton(custom_radio);

    FocusSnifferQLineEdit *url_edit = new FocusSnifferQLineEdit;
    url_edit->setText(config.base_url);
    FocusSnifferQLineEdit *model_edit = new FocusSnifferQLineEdit;
    model_edit->setText(config.model);
    FocusSnifferQLineEdit *key_edit = new FocusSnifferQLineEdit;
    key_edit->setText(config.api_key);
    key_edit->setEchoMode(QLineEdit::Password);
    key_edit->setToolTip("The API key for your LLM provider. Required for a custom provider; Free mode uses the hosted relay and needs no key.");

    FocusSnifferQComboBox *effort_combo = new FocusSnifferQComboBox(NULL);
    effort_combo->addItem("Off (fastest)", QString("off"));
    effort_combo->addItem("Low", QString("low"));
    effort_combo->addItem("High (most careful)", QString("high"));
    const int effort_index = effort_combo->findData(config.reasoning_effort);
    effort_combo->setCurrentIndex(effort_index >= 0 ? effort_index : 0);
    effort_combo->setToolTip("How much the model 'thinks' before generating code. "
                             "Off is fastest. High is most careful (the reasoning is printed to stdout). "
                             "Applies to DeepSeek and to OpenAI reasoning models (gpt-5 / o-series); "
                             "other providers always respond directly and ignore this setting.");

    FocusSnifferQComboBox *library_context_combo = new FocusSnifferQComboBox(NULL);
    library_context_combo->addItem("Off", QString("off"));
    library_context_combo->addItem("Compact (recommended)", QString("compact"));
    library_context_combo->addItem("Full", QString("full"));
    const int library_index = library_context_combo->findData(config.library_context);
    library_context_combo->setCurrentIndex(library_index >= 0 ? library_index : 1);
    library_context_combo->setToolTip("Include the Faust library symbol table in the prompt. "
                                      "Compact lists only the commonly used modules (~half the size, faster/cheaper). "
                                      "Full lists everything. Either way the exact definitions of the functions in your code are added.");

    FocusSnifferQSpinBox *max_fixes_spinbox = new FocusSnifferQSpinBox;
    max_fixes_spinbox->setRange(0, 10);
    max_fixes_spinbox->setValue(config.max_fixes);
    max_fixes_spinbox->setToolTip("How many times to ask the LLM to fix a compile error before giving up (0 = never).");

    FocusSnifferQSpinBox *cutoff_high_spinbox = new FocusSnifferQSpinBox;
    cutoff_high_spinbox->setRange(1000, 100000);
    cutoff_high_spinbox->setValue(config.reasoning_cutoff_high);
    cutoff_high_spinbox->setToolTip("High thinking effort: a request that streams more reasoning characters than this without producing any code is aborted and retried at a lower thinking effort.");

    FocusSnifferQSpinBox *cutoff_low_spinbox = new FocusSnifferQSpinBox;
    cutoff_low_spinbox->setRange(1000, 100000);
    cutoff_low_spinbox->setValue(config.reasoning_cutoff_low);
    cutoff_low_spinbox->setToolTip("Low thinking effort: a request that streams more reasoning characters than this without producing any code is aborted and retried with thinking off.");

    QHBoxLayout *mode_row = new QHBoxLayout;
    mode_row->addWidget(free_radio);
    mode_row->addWidget(custom_radio);
    layout->addLayout(mode_row);

    add_row("Base URL:", url_edit);
    add_row("Model:", model_edit);
    add_row("API key:", key_edit);
    add_row("Thinking effort:", effort_combo);

    QLabel *cutoff_high_label = new QLabel("Reasoning cut-off, high effort (chars):");
    layout->addWidget(cutoff_high_label);
    layout->addWidget(cutoff_high_spinbox);

    QLabel *cutoff_low_label = new QLabel("Reasoning cut-off, low effort (chars):");
    layout->addWidget(cutoff_low_label);
    layout->addWidget(cutoff_low_spinbox);

#if 0 // Don't think we need to set this. The default probably always works best.
    add_row("Library context:", library_context_combo);
#endif
    add_row("Max compile fix attempts:", max_fixes_spinbox);

#if 0
    QPushButton *advanced_button = new QPushButton("Advanced");
    QObject::connect(advanced_button, &QPushButton::clicked, [dialog]()
    {
      // When llm.conf does not exist yet, seed it with the commented-out
      // example settings before opening the editor. NOTE: the conf path
      // must be built directly - OS_get_conf_filename2() exits the program
      // when the file is missing.
      const QString conf_filename = QDir(OS_get_dot_radium_path()).filePath(QStringLiteral("llm.conf"));
      if (!QFile::exists(conf_filename))
      {
        const QString default_filename = QCoreApplication::applicationDirPath() + "/default_llm.conf";
        if (!QFile::copy(default_filename, conf_filename))
          printf("LLM: Warning: could not copy -%s- to -%s-\n",
                 default_filename.toUtf8().constData(),
                 conf_filename.toUtf8().constData());
      }
      if (QFile::exists(conf_filename))
      {
        evalScheme("(FROM_C-show-llm-conf-editor)");
        dialog->close();
      }
      else
      {
        showAsyncMessage(QString("Could not open the llm.conf editor: -%1- does not exist and could not be created.").arg(conf_filename).toUtf8().constData());
      }
    });
    layout->addWidget(advanced_button);
#endif

    QPushButton *ok_button = new QPushButton("OK");
    layout->addWidget(ok_button);

    // The reasoning cut-offs only do anything for DeepSeek, so the rows are
    // hidden for OpenAI and other providers. Visibility follows the base URL
    // field (and the Free/Custom mode): Free mode always uses the DeepSeek
    // relay, so the rows are shown there regardless of the forced URL text.
    auto update_cutoff_rows_visibility = [cutoff_high_label, cutoff_high_spinbox, cutoff_low_label, cutoff_low_spinbox, url_edit, free_radio]()
    {
      const bool deepseek = free_radio->isChecked()
                            || url_edit->text().contains("deepseek", Qt::CaseInsensitive);

      cutoff_high_label->setVisible(deepseek);
      cutoff_high_spinbox->setVisible(deepseek);
      cutoff_low_label->setVisible(deepseek);
      cutoff_low_spinbox->setVisible(deepseek);
    };
    QObject::connect(url_edit, &QLineEdit::textChanged, update_cutoff_rows_visibility);
    update_cutoff_rows_visibility();

    // Every widget change is saved to settings immediately (no OK needed),
    // but the forced Free-mode displays ("<Radium server>", the model,
    // empty API key, "Off (fastest)") are display only and never saved: the
    // text fields use textEdited (fires only for user edits) and the effort
    // combo's forced/restored selections are blocked below.
    int last_effort_index = effort_combo->currentIndex();
    QString last_base_url = url_edit->text();
    QString last_model = model_edit->text();
    QString last_api_key = key_edit->text();
    QObject::connect(free_radio, &QRadioButton::toggled, [model_edit, key_edit, effort_combo, url_edit, last_effort_index, last_base_url, last_model, last_api_key](bool free_selected) mutable
    {
      model_edit->setEnabled(!free_selected);
      key_edit->setEnabled(!free_selected);
      url_edit->setEnabled(!free_selected);
      effort_combo->setEnabled(!free_selected);
      if (free_selected)
      {
        last_effort_index = effort_combo->currentIndex();
        {
          QSignalBlocker blocker(effort_combo);
          effort_combo->setCurrentIndex(0);
        }
        last_base_url = url_edit->text();
        url_edit->setText("<Radium server>");
        last_model = model_edit->text();
        model_edit->setText("deepseek-v4-flash");
        last_api_key = key_edit->text();
        key_edit->setText("");
      }
      else
      {
        if (last_effort_index >= 0)
        {
          QSignalBlocker blocker(effort_combo);
          effort_combo->setCurrentIndex(last_effort_index);
        }
        url_edit->setText(last_base_url);
        model_edit->setText(last_model);
        key_edit->setText(last_api_key);
      }
    });
    free_radio->setChecked(config.mode == "free");
    if (config.mode != "free")
      custom_radio->setChecked(true);

    // In Free mode several widgets are locked. The app palette's Disabled
    // group is clobbered when the global app stylesheet is set (it becomes
    // identical to the Active group), so a disabled widget would not look
    // any different. Give these widgets a clearly grayed-out Disabled group,
    // derived from the current theme colors, like updatePalette() does for
    // the widgets that existed when the palette was last applied.
    {
      const QColor active_text = dialog->palette().color(QPalette::Active, QPalette::Text);
      const QColor active_base = dialog->palette().color(QPalette::Active, QPalette::Base);
      const QColor active_window = dialog->palette().color(QPalette::Active, QPalette::Window);

      const QColor disabled_text = mix_colors(active_text, active_base, 0.5);
      const QColor disabled_base = mix_colors(active_base, active_window, 0.5);

      auto set_disabled_look = [disabled_text, disabled_base](QWidget *w)
      {
        QPalette pal = w->palette();
        pal.setColor(QPalette::Disabled, QPalette::Text, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::WindowText, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::ButtonText, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::Base, disabled_base);
        pal.setColor(QPalette::Disabled, QPalette::Button, disabled_base);
        pal.setColor(QPalette::Disabled, QPalette::Window, disabled_base);
        w->setPalette(pal);
      };

      set_disabled_look(url_edit);
      set_disabled_look(model_edit);
      set_disabled_look(key_edit);
      set_disabled_look(effort_combo);
    }

    QObject::connect(free_radio, &QRadioButton::toggled, [](bool free_selected)
    {
      SETTINGS_write_string("llm_mode", free_selected ? QString("free") : QString("custom"));
    });
    QObject::connect(url_edit, &QLineEdit::textEdited, [](const QString &text)
    {
      SETTINGS_write_string("llm_base_url", text.trimmed());
    });
    QObject::connect(model_edit, &QLineEdit::textEdited, [](const QString &text)
    {
      SETTINGS_write_string("llm_model", text.trimmed());
    });
	QObject::connect(key_edit, &QLineEdit::textEdited, [](const QString &text)
	{
		SETTINGS_make_config_file_private("llm_api_key");
		SETTINGS_write_string("llm_api_key", text.trimmed());
	});
    QObject::connect(effort_combo, &QComboBox::currentIndexChanged, [effort_combo](int index)
    {
      SETTINGS_write_string("llm_reasoning_effort", effort_combo->itemData(index).toString());
    });
    QObject::connect(library_context_combo, &QComboBox::currentIndexChanged, [library_context_combo](int index)
    {
      SETTINGS_write_string("llm_library_context", library_context_combo->itemData(index).toString());
    });
    QObject::connect(max_fixes_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_max_fixes", value);
    });
    QObject::connect(cutoff_high_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_reasoning_cutoff_high", value);
    });
    QObject::connect(cutoff_low_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_reasoning_cutoff_low", value);
    });

    QObject::connect(ok_button, &QPushButton::released, [dialog]()
    {
      dialog->close();
    });

    dialog->show();
    dialog->raise();
    dialog->activateWindow();
}

} // namespace llm
} // namespace radium
