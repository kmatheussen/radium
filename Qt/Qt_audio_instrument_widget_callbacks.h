#pragma once
/* Copyright 2012 Kjetil S. Matheussen

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


#include "../common/sequencer_proc.h"
#include "../common/dyn_type.h"
#include "../common/hashmap_proc.h"

#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundPluginRegistry_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../audio/Modulator_plugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/AudioMeasurements.h"
#include "../common/sequencer_timing_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include "Qt_MyQScrollBar.hpp"

#include "LLM_fx_tune.hpp"
#include "LLM_fx_tune_widget.hpp"
#include "Qt_instruments_proc.h"

/*
class Pd_Plugin_widget;
class Audio_instrument_wigdet;
static Pd_Plugin_widget *AUDIOWIDGET_get_pd_plugin_widget(Audio_instrument_widget *audio_instrument_widget);
*/

#include "Qt_audio_instrument_widget.h"

#include "Qt_instruments_proc.h"

#include "mQt_sample_requester_widget_callbacks.h"
#include "mQt_plugin_widget_callbacks.h"
#include "mQt_compressor_widget_callbacks.h"

#include "../mixergui/QM_chip.h"
#include "../mixergui/QM_MixerWidget.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "../audio/Bus_plugins_proc.h"

#include <algorithm>
#include <cmath>


//extern void BottomBar_set_system_audio_instrument_widget_and_patch(Ui::Audio_instrument_widget *system_audio_instrument_widget, struct Patch *patch);


namespace{

// A plugin type that makes sense as an insert effect: it processes audio and
// is not an instrument, router or internal system plugin.
static bool is_instrument_category(const char *category)
{
  if (category == NULL)
    return false;

  const QString lower = QString::fromUtf8(category).toLower();
  return lower.contains("instrument") || lower.contains("synth");
}

static bool is_excluded_effect_type(const QString &type_name, const QString &name)
{
  // Internal routing/system plugins are not useful as insert effects.
  if (type_name == "Bus" || type_name == "Send" || type_name == "Receive"
      || type_name == "Patchbay" || type_name == "Jack" || type_name == "Seqtrack"
      || type_name == "MIDI Messages" || type_name == "Timeskew")
    return true;
  if (name.startsWith("System "))
    return true;
  return false;
}

static bool is_insertable_effect_type(const SoundPluginType *type)
{
  if (type == NULL || type->type_name == NULL || type->name == NULL)
    return false;

  // For direct/built-in plugins 'is_instrument' is reliable. For container
  // plugins (VST/VST3/AU/LV2) it is initially true and later becomes
  // 'acceptsMidi()', which is wrong for MIDI-controlled effects such as
  // autotune. Use audio I/O and the category for those instead.
  if (type->container == NULL && type->is_instrument)
    return false;

  if (type->num_inputs <= 0 || type->num_outputs <= 0)
    return false;

  const QString type_name = QString::fromUtf8(type->type_name);
  const QString name = QString::fromUtf8(type->name);

  if (is_excluded_effect_type(type_name, name))
    return false;

  if (type->category != NULL && is_instrument_category(type->category))
    return false;

  return true;
}

static void add_catalog_plugin(QVector<radium::llm::FxPlugin> &plugins,
                               QSet<QString> &seen,
                               const QString &type_name,
                               const QString &name,
                               const QString &container_name)
{
  const QString key = radium::llm::fx_normalize_plugin_key(container_name)
                    + "\n" + radium::llm::fx_normalize_plugin_key(type_name)
                    + "\n" + radium::llm::fx_normalize_plugin_key(name);
  if (seen.contains(key))
    return;
  seen.insert(key);
  plugins.append(radium::llm::FxPlugin{type_name, name, container_name});
}

struct InsertedEffectPlugin{
  radium::llm::FxPlugin fx;
  instrument_t instrument_id;
};

// Finds the effect plugins inserted after 'patch', in signal-chain order.
// Traversal stops at the main pipe, buses, instruments and routing plugins,
// so only plugins that are actually part of this instrument's insert chain
// are returned.
static void find_inserted_effect_plugins_recursive(struct Patch *patch,
                                                   QVector<InsertedEffectPlugin> &ret,
                                                   QSet<instrument_t> &visited)
{
  if (patch == NULL || visited.contains(patch->id))
    return;
  visited.insert(patch->id);

  const int num_out_connections = CHIP_get_num_out_connections(patch);
  for (int i = 0; i < num_out_connections; i++){
    struct Patch *dest = CHIP_get_dest(patch, i);
    if (dest == NULL || AUDIO_is_permanent_patch(dest))
      continue;

    SoundPlugin *dest_plugin = (SoundPlugin*)dest->patchdata;
    if (dest_plugin == NULL || !is_insertable_effect_type(dest_plugin->type))
      continue;

    InsertedEffectPlugin inserted;
    inserted.fx.type_name = QString::fromUtf8(dest_plugin->type->type_name);
    inserted.fx.name = QString::fromUtf8(dest_plugin->type->name);
    inserted.fx.container_name = dest_plugin->type->container == NULL
                               ? QString()
                               : QString::fromUtf8(dest_plugin->type->container->name);
    inserted.instrument_id = dest->id;
    ret.append(inserted);

    find_inserted_effect_plugins_recursive(dest, ret, visited);
  }
}

static QVector<InsertedEffectPlugin> find_inserted_effect_plugins(struct Patch *patch)
{
  QVector<InsertedEffectPlugin> ret;
  QSet<instrument_t> visited;
  find_inserted_effect_plugins_recursive(patch, ret, visited);
  return ret;
}

static QVector<radium::llm::FxPlugin> get_inserted_effect_plugin_catalog(struct Patch *patch)
{
  QVector<radium::llm::FxPlugin> ret;

  for (const InsertedEffectPlugin &inserted : find_inserted_effect_plugins(patch)){
    bool already_added = false;
    for (const radium::llm::FxPlugin &existing : ret)
      if (radium::llm::fx_plugin_matches(existing, inserted.fx)){
        already_added = true;
        break;
      }
    if (!already_added)
      ret.append(inserted.fx);
  }

  return ret;
}

// Inserts 'new_patch' directly after 'insert_after': all of insert_after's
// current destinations are rewired through the new patch. Used for both
// regular effect plugins and the auto-created Faust Dev 2 effect.
static struct Patch *insert_effect_patch_after(struct Patch *insert_after, struct Patch *new_patch)
{
  if (insert_after == NULL || new_patch == NULL)
    return NULL;

  struct Patch *main_pipe_patch = get_main_pipe_patch();
  R_ASSERT_RETURN_IF_FALSE2(main_pipe_patch != NULL, NULL);

  QVector<struct Patch*> destinations;
  const int num_out_connections = CHIP_get_num_out_connections(insert_after);
  for (int i = 0; i < num_out_connections; i++){
    struct Patch *dest = CHIP_get_dest(insert_after, i);
    if (dest != NULL)
      destinations.append(dest);
  }

  ADD_UNDO(MixerConnections_CurrPos());

  MW_connect(insert_after, new_patch, ConnectionType::IS_SEND);

  if (destinations.isEmpty()){
    MW_connect(new_patch, main_pipe_patch, ConnectionType::IS_SEND);
  } else {
    for (struct Patch *dest : destinations){
      MW_disconnect(insert_after, dest);
      MW_connect(new_patch, dest, ConnectionType::IS_SEND);
    }
  }

  return new_patch;
}

  
class Audio_instrument_widget : public QWidget, public Ui::Audio_instrument_widget{
  Q_OBJECT;

public:

  int _is_updating = 0;
  
  radium::GcHolder<struct Patch> _patch;

  Patch_widget *_patch_widget;
  Plugin_widget *_plugin_widget;
  
  Sample_requester_widget *_sample_requester_widget;
  Compressor_widget *_comp_widget;
  LLM_fx_tune_widget *_fx_tune_widget;
  MyQCheckBox *_llm_checkbox;

  int64_t _mixerstrip_guinum = -1;

  SizeType _size_type;
  SizeType _size_type_before_hidden;
  //bool _was_large_before_hidden;

  static void set_arrow_style(QWidget *arrow, bool set_size_policy=true){
    QPalette palette2;

    QBrush brush1(QColor(106, 104, 100, 255));
    brush1.setStyle(Qt::SolidPattern);

    QBrush brush3(QColor(85, 85, 0, 255));
    //QBrush brush3(QColor(0, 107, 156, 255));
    brush3.setStyle(Qt::SolidPattern);
    palette2.setBrush(QPalette::Active, QPalette::WindowText, brush3);
    palette2.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
    palette2.setBrush(QPalette::Disabled, QPalette::WindowText, brush1);
    palette2.setBrush(QPalette::Active, QPalette::Text, brush3);
    palette2.setBrush(QPalette::Inactive, QPalette::Text, brush3);
    palette2.setBrush(QPalette::Disabled, QPalette::Text, brush1);
    arrow->setPalette(palette2);
    //static_cast<QLabel*>(arrow)->setText("gakk");

#if 1
    if(set_size_policy){
      QSizePolicy sizePolicy4(QSizePolicy::Fixed, QSizePolicy::Fixed);
      sizePolicy4.setHorizontalStretch(0);
      sizePolicy4.setVerticalStretch(0);
      sizePolicy4.setHeightForWidth(arrow->sizePolicy().hasHeightForWidth());
      arrow->setSizePolicy(sizePolicy4);

      //arrow->setFrameShape(QFrame::Box);
    }
#endif
  }


 Audio_instrument_widget(QWidget *parent,struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _plugin_widget(NULL)
    , _sample_requester_widget(NULL)
    , _fx_tune_widget(NULL)
    , _llm_checkbox(NULL)
      //, _is_large(false)
    , _size_type(SIZETYPE_NORMAL) //patch->widget_height_type)
    , _size_type_before_hidden(patch->widget_height_type)
  {
    radium::ScopedGeneration scoped_update(_is_updating);

    _ab_checkbox_width=gui_textWidth("12", -1) * 3 / 2;
    
    setupUi(this);    

    scrollArea->setHorizontalScrollBar(new Qt_MyQScrollBar(Qt::Horizontal));
    scrollArea->setVerticalScrollBar(new Qt_MyQScrollBar(Qt::Vertical));

    time_of_last_minheight_inc.start();

    solo_button->_add_undo_when_clicked = false;
    bypass_button->_add_undo_when_clicked = false;
    
    /*
    {
      //QFontMetrics fm(font());
      _ab_checkbox_width = ab_reset_button->width(); //fm.width("A") * 4;
      ab_reset_button->setMinimumWidth(_ab_checkbox_width);
      ab_reset_button->setMaximumWidth(_ab_checkbox_width);
    }
    */

    ab_reset_button->setText("reset.svg");
    
    /*
    right_part_layout->setStretch(0,2);
    right_part_layout->setStretch(1,9);
    */
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    _patch_widget = new Patch_widget(this,patch);
    voiceBox_layout->insertWidget(0,_patch_widget, 1);
    //if(false && plugin->type->play_note==NULL)
    //  _patch_widget->voices_widget->setDisabled(true);

    SLIDERPAINTER_set_string(input_volume_slider->_painter, "In");
    SLIDERPAINTER_set_string(volume_slider->_painter, "Volume");
    SLIDERPAINTER_set_string(output_volume_slider->_painter, "Out");
    SLIDERPAINTER_set_string(panning_slider->_painter, "Panning");
    //velocity_slider->_painter, "Edit vel.";

    SLIDERPAINTER_set_string(drywet_slider->_painter, "Dry/Wet");
    SLIDERPAINTER_set_string(bus1_slider->_painter, "Reverb");
    SLIDERPAINTER_set_string(bus2_slider->_painter, "Chorus");
    SLIDERPAINTER_set_string(bus3_slider->_painter, "Aux 1");
    SLIDERPAINTER_set_string(bus4_slider->_painter, "Aux 2");
    SLIDERPAINTER_set_string(bus5_slider->_painter, "Aux 3");

    /*
    if(!strcmp(plugin->type->type_name,"Jack") && !strcmp(plugin->type->name,"System Out")){
      _i_am_system_out = true;
      BottomBar_set_system_audio_instrument_widget_and_patch(this, _patch.data());
    }
    */
    
#if 0
    if(plugin->type->num_inputs==0){
      drywet_slider->setDisabled(true);
      effects_onoff->setDisabled(true);
    }

    if(plugin->type->num_inputs==0){
      input_volume_slider->setDisabled(true);
      input_volume_onoff->setDisabled(true);
    }
#endif

    if(plugin->type->num_outputs==0){
      filters_widget->setDisabled(true);
      outputs_widget->setDisabled(true);
    }

    if(plugin->type->num_outputs<2){
      panning_onoff->setDisabled(true);
      panning_slider->setDisabled(true);
      rightdelay_onoff->setDisabled(true);
      rightdelay_slider->setDisabled(true);
    }

#if 0
    //instrument->effects_frame->addWidget(PluginWidget_create(NULL, plugin), 0, 3, 2, 1);
    _plugin_widget=PluginWidget_create(NULL, _patch.data());
    if(_plugin_widget->_param_widgets.size() > 0){
      delete(pipe_label);
      pipe_label = NULL;
    }
#else
    _plugin_widget = new Plugin_widget(this,_patch.data());

    delete(pipe_label);
    pipe_label = NULL;
#endif

    effects_layout->insertWidget(4,_plugin_widget);
    _plugin_widget->setVisible(plugin->show_controls_gui);
    spacer_holder->setVisible(!plugin->show_controls_gui);
    

    if(plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player") || plugin->type==PR_get_plugin_type_by_name(NULL, "Sample Player","Click") || plugin->type==PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth")){
      _sample_requester_widget = new Sample_requester_widget(this, _patch_widget->name_widget, _plugin_widget->sample_name_label, _patch.data());
      effects_layout->insertWidget(3,_sample_requester_widget);
      //show_browser->setFixedWidth(browserArrow->width());

      /*
      QLabel *arrow = new QLabel("=>",this);
      arrow->setFrameShape(QFrame::Box);

      set_arrow_style(arrow);

      effects_layout->insertWidget(3, arrow, 0, Qt::AlignTop);
      arrow->show();
      */

      //MyQCheckBox *checkbox = new MyQCheckBox("hello", this);
      //effects_layout->

    } else
      browserWidget->hide();

    // Add compressor
    {
      _comp_widget = new Compressor_widget(patch, this);
      _comp_widget->setMinimumWidth(150 * g_gfx_scale);
      effects_layout->insertWidget(effects_layout->indexOf(equalizerWidget), _comp_widget);

      // these widgets are only used in the standalone version
      _comp_widget->load_button->hide();
      _comp_widget->save_button->hide();
      _comp_widget->radium_url->hide();
      _comp_widget->bypass->hide();

      GL_lock(); {
        _comp_widget->show();
        _comp_widget->setVisible(plugin->show_compressor_gui);
      }GL_unlock();
    }

    
    filters_widget->setVisible(plugin->show_equalizer_gui);


    // LLM effect tuning terminal. Placed in the outer grid (row 1, below
    // the scroll area) so it also stays visible in Half/Full mode, where the
    // effects_layout widgets are hidden.
    {
      auto read_catalog = [this]() -> QVector<radium::llm::FxParamState>
      {
        QVector<radium::llm::FxParamState> states;

        struct Patch *patch = _patch.data();
        if (patch == NULL)
          return states;

        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        if (plugin == NULL)
          return states;

        for (const radium::llm::FxParam &param : radium::llm::fx_catalog()){
          radium::llm::FxParamState state;
          state.param = param;
          state.current_native = PLUGIN_get_effect_from_name(plugin, param.name.toUtf8().constData(), VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);
          // Bus sends are linear gains natively, but the catalog (and the
          // LLM) works in dB.
          if (param.is_gain_db)
            state.current_native = gain2db(state.current_native);
          state.has_modulator = hasModulator(patch->id, param.name.toUtf8().constData());
          states.append(state);
        }

        return states;
      };

      auto read_instrument_context = [this]() -> radium::llm::FxInstrumentContext
      {
        radium::llm::FxInstrumentContext context;

        struct Patch *patch = _patch.data();
        if (patch == NULL)
          return context;

        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        if (plugin == NULL || plugin->type == NULL)
          return context;

        const SoundPluginType *type = plugin->type;
        // Controls whose display text the plugin type cannot produce (a missing
        // case in its get_display_value_string); logged once, never a dialog.
        QStringList undisplayable_control_numbers;

        // Song tempo/signature, so durations can be expressed musically.
        // (g_rt_sequencer_bpm/_signature are updated with the audio blocks,
        // so they describe the position the measurements come from.)
        {
          const double bpm = g_rt_sequencer_bpm;
          if (bpm > 1.0 && bpm < 999.0){
            context.bpm = bpm;
            context.signature_numerator = (int)g_rt_sequencer_signature.numerator;
            context.signature_denominator = (int)g_rt_sequencer_signature.denominator;
          }
        }
        // Why the prompt has no measurements (logged, so "the instrument was
        // not audible" can be told apart from "the feature is broken").
        QString measurement_missing_reason;
        context.type_name = type->type_name == NULL ? QString() : QString::fromUtf8(type->type_name);
        context.plugin_name = type->name == NULL ? QString() : QString::fromUtf8(type->name);
        context.plugin_version = type->version;

        // One short line about the plugin (vendor + category, say), when the
        // plugin type can provide one. Falls back to the plugin's info text,
        // which is mostly metadata ("Version", "Accepts MIDI", ...) the LLM
        // does not need.
        {
          char summary[192] = {0};
          PLUGIN_get_plugin_summary(plugin, summary, sizeof(summary));
          if (summary[0] != 0)
            context.description = QString::fromUtf8(summary).simplified().left(160);
          else if (type->info != NULL)
            context.description = QString::fromUtf8(type->info).simplified().left(160);
        }

        if (!undisplayable_control_numbers.isEmpty())
          radium::llm::llm_log_note(QString("instrument %1 [%2]: controls without a display text (missing case "
                                            "in the plugin type): %3 (of %4 controls)")
                                    .arg(context.plugin_name, context.type_name,
                                         undisplayable_control_numbers.join(", "))
                                    .arg(type->num_effects));

        // (Logged after the block below has computed the reason.)
        // What the instrument currently measures (spectrum/levels), when it
        // has been audible recently. The capture target is set here too, so the
        // audio thread starts collecting this instrument's output.
        {
          SoundProducer *sound_producer = SP_get_sound_producer(plugin);
          MEASUREMENTS_set_target(sound_producer, plugin);

          if (getenv("RADIUM_DEBUG_MEASUREMENTS") != NULL)
            printf("measurements: host: patch=%p plugin=%p sound_producer=%p (type %s)\n",
                   (void*)patch, (void*)plugin, (void*)sound_producer,
                   type->name == NULL ? "?" : type->name);

          AudioAnalysis analysis;
          MeasurementsStatus status = sound_producer == NULL
            ? MEASUREMENTS_STATUS_NO_PRODUCER
            : MEASUREMENTS_get_status(sound_producer, plugin, (int)MIXER_get_sample_rate(), &analysis);

          // Nothing yet? The target may have been selected only a moment ago
          // (or the instrument only just started playing): if it is audible, a
          // block arrives within ~85 ms, so wait briefly instead of sending a
          // prompt without measurements.
          if (status != MEASUREMENTS_STATUS_OK && sound_producer != NULL
              && MEASUREMENTS_wait_for_block(sound_producer, plugin, 200))
            status = MEASUREMENTS_get_status(sound_producer, plugin, (int)MIXER_get_sample_rate(), &analysis);

          const bool have_measurement = status == MEASUREMENTS_STATUS_OK && analysis.valid;

          if (!have_measurement){
            switch(status){
            case MEASUREMENTS_STATUS_NO_PRODUCER:     measurement_missing_reason = "no SoundProducer for this instrument"; break;
            case MEASUREMENTS_STATUS_NOT_TARGET:      measurement_missing_reason = "another instrument is selected for the measurements"; break;
            case MEASUREMENTS_STATUS_NEVER_CAPTURED:  measurement_missing_reason = QString("the audio thread never processed this instrument since it was selected (%1 capture calls, %2 blocks)")
                                                                                     .arg(MEASUREMENTS_get_num_capture_calls())
                                                                                     .arg(MEASUREMENTS_get_num_captured_blocks()); break;
            case MEASUREMENTS_STATUS_SILENT:          measurement_missing_reason = QString("the instrument's output was silent (%1 capture calls, %2 blocks)")
                                                                                     .arg(MEASUREMENTS_get_num_capture_calls())
                                                                                     .arg(MEASUREMENTS_get_num_captured_blocks()); break;
            case MEASUREMENTS_STATUS_NO_ANALYZER:     measurement_missing_reason = "libfftw3f is not available"; break;
            case MEASUREMENTS_STATUS_OK:              measurement_missing_reason = "unexpected status"; break;
            }
          }

          if (have_measurement){
            context.measurement.valid = true;
            context.measurement.sample_rate = analysis.sample_rate;
            context.measurement.peak_db = analysis.peak_db;
            context.measurement.rms_db = analysis.rms_db;
            context.measurement.crest_db = analysis.crest_db;
            context.measurement.loudness_momentary_lufs = analysis.loudness_momentary_lufs;
            context.measurement.loudness_loudest_lufs = analysis.loudness_loudest_lufs;
            context.measurement.loudness_window_lufs = analysis.loudness_window_lufs;
            context.measurement.true_peak_db = analysis.true_peak_db;
            context.measurement.stereo_correlation = analysis.stereo_correlation;
            context.measurement.stereo_side_db = analysis.stereo_side_db;
            context.measurement.stereo_balance_db = analysis.stereo_balance_db;
            context.measurement.centroid_hz = analysis.centroid_hz;
            context.measurement.flatness = analysis.flatness;
            context.measurement.hum_db = analysis.hum_db;
            context.measurement.hum_hz = analysis.hum_hz;
            context.measurement.dc_offset = analysis.dc_offset;
            context.measurement.sub_bass_db = analysis.sub_bass_db;
            context.measurement.attack_db_per_ms = analysis.attack_db_per_ms;
            context.measurement.onsets_per_second = analysis.onsets_per_second;
            context.measurement.decay_time_s = analysis.decay_time_s;
            context.measurement.tail_db = analysis.tail_db;
            context.measurement.age_ms = MEASUREMENTS_get_age_ms(sound_producer, plugin);
            context.measurement.num_frames = analysis.num_frames;
            for (int band = 0; band < AUDIO_ANALYZER_NUM_BANDS; band++){
              const AudioAnalyzerBand edges = AUDIO_ANALYZER_band(band);
              context.measurement.bands.append(QString("%1-%2 Hz %3")
                                               .arg((int)edges.low_hz)
                                               .arg((int)edges.high_hz)
                                               .arg(analysis.band_db[band], 0, 'f', 0));
            }
          }
        }

        if (!measurement_missing_reason.isEmpty())
          radium::llm::llm_log_note("no sound measurements: " + measurement_missing_reason);

        const int max_prompt_params = 256;
        for (int effect_num = 0; effect_num < type->num_effects; effect_num++){
          if (type->effect_is_visible != NULL && !type->effect_is_visible(plugin, effect_num))
            continue;

          const char *effect_name = PLUGIN_get_effect_name(plugin, effect_num);
          if (effect_name == NULL || effect_name[0] == 0)
            continue;

          // Generic MIDI controller controls ("Breath (CH 3)", "ModWheel (CH
          // 12)", ...) cannot shape a mixing change, and for some plugins they
          // are almost the whole control list. Drop them before the size cap
          // so they cannot crowd out the plugin's real controls.
          {
            radium::llm::FxInstrumentParamState midi_probe;
            midi_probe.name = QString::fromUtf8(effect_name);
            if (radium::llm::fx_is_midi_controller_param(midi_probe)){
              context.hidden_midi_controls++;
              continue;
            }
          }

          const float current_scaled = PLUGIN_get_effect_value2(plugin, effect_num, VALUE_FROM_STORAGE, EFFECT_FORMAT_SCALED);
          if (!std::isfinite(current_scaled) || current_scaled < -0.001f || current_scaled > 1.001f){
            context.omitted_params++;
            continue;
          }

          if (context.params.size() >= max_prompt_params){
            context.omitted_params++;
            continue;
          }

          radium::llm::FxInstrumentParamState param;
          param.effect_num = effect_num;
          param.name = QString::fromUtf8(effect_name);
          param.display_name = param.name;
          if (type->get_displayable_effect_name != NULL){
            const char *display_name = type->get_displayable_effect_name(plugin, effect_num);
            if (display_name != NULL && display_name[0] != 0)
              param.display_name = QString::fromUtf8(display_name);
          }
          param.current_scaled = std::max(0.0f, std::min(1.0f, current_scaled));
          param.current_native = PLUGIN_get_effect_value2(plugin, effect_num, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);

          {
            float min_native = 0.0f;
            float max_native = 0.0f;
            char unit[64] = {0};
            if (PLUGIN_get_effect_native_range(plugin, effect_num, &min_native, &max_native, unit, sizeof(unit))
                && std::isfinite(min_native) && std::isfinite(max_native)
                && min_native < max_native && std::isfinite(param.current_native)
                && param.current_native >= min_native - 0.001f
                && param.current_native <= max_native + 0.001f){
              param.has_native_range = true;
              param.min_native = min_native;
              param.max_native = max_native;
              if (unit[0] != 0)
                param.unit = QString::fromUtf8(unit);
            } else {
              // Fall back to the normalized 0..1 range: the real range is
              // unknown, so the LLM must not infer values from the display
              // text (see FxInstrumentParamState::is_clamped).
              param.is_clamped = true;
            }
          }

          const char *description = type->get_effect_description != NULL
                                  ? type->get_effect_description(plugin, effect_num)
                                  : NULL;
          // The prompt sends at most FX_CONTROL_DESCRIPTION_MAX characters of
          // this (it is per-control text, multiplied by the control count).
          if (description != NULL)
            param.description = QString::fromUtf8(description).simplified().left(radium::llm::FX_CONTROL_DESCRIPTION_MAX * 2);

          // A plugin may know the unit without reporting a native range (all
          // JUCE-hosted plugins do: their values are normalized, but "%", "ms"
          // and "dB" are known). Without this the prompt would describe the
          // control as a bare 0..1 position.
          if (param.unit.isEmpty()){
            char effect_unit[32] = {0};
            PLUGIN_get_effect_unit(plugin, effect_num, effect_unit, sizeof(effect_unit));
            if (effect_unit[0] != 0)
              param.unit = QString::fromUtf8(effect_unit).simplified().left(12);
          }

          switch (PLUGIN_get_effect_format(plugin, effect_num)){
            case EFFECT_FORMAT_FLOAT: param.format = radium::llm::FX_INSTRUMENT_PARAM_FLOAT; break;
            case EFFECT_FORMAT_INT: param.format = radium::llm::FX_INSTRUMENT_PARAM_INT; break;
            case EFFECT_FORMAT_BOOL: param.format = radium::llm::FX_INSTRUMENT_PARAM_BOOL; break;
            case EFFECT_FORMAT_RADIO: param.format = radium::llm::FX_INSTRUMENT_PARAM_RADIO; break;
            default: param.format = radium::llm::FX_INSTRUMENT_PARAM_UNKNOWN; break;
          }

          char display_value[256] = {0};
          PLUGIN_get_display_value_string(plugin, effect_num, display_value, sizeof(display_value));
          param.current_display = QString::fromUtf8(display_value).simplified().left(128);

          // A plugin type that forgot a case for one of its effects returns an
          // empty or "?" display text instead of raising an assertion (see
          // Sampler_plugin.cpp). Note it once in the log, where the developer
          // will see it, instead of showing a dialog to the user.
          if (param.current_display.isEmpty() || param.current_display == "?"
              || param.name.endsWith("(error)") || param.name.contains("(Error)")){
            if (undisplayable_control_numbers.size() < 8)
              undisplayable_control_numbers.append(QString::number(effect_num));
          }

          context.params.append(param);
        }

        return context;
      };

      // Available effect plugins (plugins with both audio inputs and outputs
      // that are not instruments). The LLM picks one of these when the user
      // asks for an effect the system effects cannot make (e.g. wah,
      // distortion or delay); the host creates it and inserts it into the
      // signal path.
      auto read_plugin_catalog = []() -> QVector<radium::llm::FxPlugin>
      {
        QVector<radium::llm::FxPlugin> plugins;
        QSet<QString> seen;

        // 1. Plugin types that are already registered directly. This covers
        // built-in effects, LADSPA and compiled-in Faust effects, plus
        // container children that have already been populated.
        const int num_types = PR_get_num_plugin_types();
        for (int i = 0; i < num_types; i++){
          SoundPluginType *type = PR_get_plugin_type(i);
          if (!is_insertable_effect_type(type))
            continue;

          const QString type_name = QString::fromUtf8(type->type_name);
          const QString name = QString::fromUtf8(type->name);

          // These are development instruments. They are created by the
          // dedicated create_faust flow instead of being listed as effects.
          if (type_name == "Faust Dev" || type_name == "Faust Dev 2")
            continue;

          const QString container_name = type->container == NULL
                                       ? QString()
                                       : QString::fromUtf8(type->container->name);
          add_catalog_plugin(plugins, seen, type_name, name, container_name);
        }

        // 2. Container children (VST/VST3/AU/LV2) from the plugin registry
        // cache. These are normally not in g_plugin_types until the user has
        // opened the container's menu or loaded a song using one of them.
        // The cache store has the name, type, container and audio channel
        // counts, so we can list them without instantiating every plugin.
        const dynvec_t registry = getSoundPluginRegistry(false);
        for (int i = 0; i < registry.num_elements; i++){
          const dyn_t &entry = registry.elements[i];
          if (entry.type != HASH_TYPE || entry.hash == NULL)
            continue;

          hash_t *hash = entry.hash;

          const char *entry_type = HASH_get_chars(hash, ":type");
          if (entry_type == NULL || strcmp(entry_type, "NORMAL") != 0)
            continue;

          const char *container_name = HASH_get_chars(hash, ":container-name");
          if (container_name == NULL || container_name[0] == 0)
            continue; // Directly registered plugins were handled above.

          const char *type_name = HASH_get_chars(hash, ":type-name");
          const char *name = HASH_get_chars(hash, ":name");
          if (type_name == NULL || name == NULL || name[0] == 0)
            continue;

          const QString qtype_name = QString::fromUtf8(type_name);
          const QString qname = QString::fromUtf8(name);

          if (is_excluded_effect_type(qtype_name, qname))
            continue;

          const int64_t num_inputs = HASH_has_key(hash, ":num-inputs") ? HASH_get_int(hash, ":num-inputs") : -1;
          const int64_t num_outputs = HASH_has_key(hash, ":num-outputs") ? HASH_get_int(hash, ":num-outputs") : -1;
          if (num_inputs <= 0 || num_outputs <= 0)
            continue;

          const char *category = HASH_get_chars(hash, ":category");
          if (category != NULL && is_instrument_category(category))
            continue;

          add_catalog_plugin(plugins, seen, qtype_name, qname, QString::fromUtf8(container_name));
        }

        return plugins;
      };

      // Effect plugins currently inserted after this instrument, for the
      // remove_plugins part of the prompt.
      auto read_inserted_plugins = [this]() -> QVector<radium::llm::FxPlugin>
      {
        return get_inserted_effect_plugin_catalog(_patch.data());
      };

      auto apply_changes = [this, read_catalog](QVector<radium::llm::FxChange> &changes,
                                                const QVector<radium::llm::FxInstrumentChange> &instrument_changes) -> bool
      {
        struct Patch *patch = _patch.data();
        if (patch == NULL)
          return false;

        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        if (plugin == NULL || plugin->type == NULL)
          return false;

        for (const radium::llm::FxInstrumentChange &change : instrument_changes){
          if (change.type_name != QString::fromUtf8(plugin->type->type_name)
              || change.plugin_name != QString::fromUtf8(plugin->type->name)
              || change.plugin_version != plugin->type->version
              || change.effect_num < 0 || change.effect_num >= plugin->type->num_effects)
            return false;

          const char *current_name = PLUGIN_get_effect_name(plugin, change.effect_num);
          if (current_name == NULL || change.name != QString::fromUtf8(current_name))
            return false;

          if (change.value_is_native){
            const float current_native = PLUGIN_get_effect_value2(plugin, change.effect_num, VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE);
            if (!std::isfinite(current_native) || std::fabs(current_native - change.old_value_native) > 0.0001f)
              return false;
          }else{
            const float current_scaled = PLUGIN_get_effect_value2(plugin, change.effect_num, VALUE_FROM_STORAGE, EFFECT_FORMAT_SCALED);
            if (!std::isfinite(current_scaled) || std::fabs(current_scaled - change.old_value_scaled) > 0.001f)
              return false;
          }
        }

        // Catalog lookups used below.
        QSet<QString> gain_db_names;
        QSet<QString> catalog_names;
        for (const radium::llm::FxParam &param : radium::llm::fx_catalog()){
          catalog_names.insert(param.name.toLower());
          if (param.is_gain_db)
            gain_db_names.insert(param.name.toLower());
        }

        const QString mute_name = QString("System Volume On/Off");
        const QString bypass_name = QString("System Effects On/Off");
        const QString solo_name = QString("System Solo On/Off");

        auto is_mixer_button = [&mute_name, &bypass_name, &solo_name](const QString &name){
          return name == mute_name || name == bypass_name || name == solo_name;
        };

        // Make sure changes are audible: enable the bands that are being
        // changed, and switch the bypass back on if it is off - but only when
        // the reply makes sound-shaping changes and does not change the
        // bypass itself. (These extra changes are appended to 'changes', so
        // the status summary shows them too.)
        {
          const QVector<radium::llm::FxParamState> states = read_catalog();

          for (const radium::llm::FxChange &change : radium::llm::fx_band_enable_changes(states, changes))
            changes.append(change);

          bool changes_an_effect = false;
          bool changes_the_bypass = false;
          for (const radium::llm::FxChange &change : changes){
            if (change.name == bypass_name)
              changes_the_bypass = true;
            else if (!is_mixer_button(change.name))
              changes_an_effect = true;
          }

          if (!changes_the_bypass && changes_an_effect
              && PLUGIN_get_effect_from_name(plugin, "System Effects On/Off", VALUE_FROM_STORAGE, EFFECT_FORMAT_NATIVE) < 0.5f)
            changes.append(radium::llm::FxChange{bypass_name, 1.0f});
        }

        // A bus send only has an audible effect when the matching mixer send
        // connection is enabled. Enable it (via the same path as the bus
        // on/off checkbox) when a send level is changed while it is off. The
        // connection gets its own undo entry; it is added before the effects
        // undo below, so the first undo reverts the effect values and the
        // second the connection.
        {
          const Buses buses = MIXER_get_buses();

          auto bus_for_send_name = [&buses](const QString &name) -> SoundProducer*{
            if (name == "System Reverb") return buses.bus1;
            if (name == "System Chorus") return buses.bus2;
            if (name == "System Aux 1") return buses.bus3;
            if (name == "System Aux 2") return buses.bus4;
            if (name == "System Aux 3") return buses.bus5;
            return NULL;
          };

          QVector<radium::llm::FxChange> connection_extras;

          for (const radium::llm::FxChange &change : changes){
            SoundProducer *bus = bus_for_send_name(change.name);
            if (bus == NULL || change.value <= -39.999f) // -40 dB = no send; no need to connect then.
              continue;

            struct Patch *bus_patch = const_cast<Patch*>(SP_get_plugin(bus)->patch);
            if (bus_patch == NULL)
              continue;

            if (!MW_are_connected(patch, bus_patch)){
              set_bus_value(bus, true);
              connection_extras.append(radium::llm::FxChange{change.name + QString(" On/Off"), 1.0f});
            }
          }

          for (const radium::llm::FxChange &change : connection_extras)
            changes.append(change);
        }

        // Snapshot the complete effect state when plugin-specific controls
        // are part of the batch; otherwise system effects alone are enough.
        // ScopedUndo in apply_reply groups this together with any graph edits.
        if (instrument_changes.isEmpty())
          ADD_UNDO(OnlySystemEffects(patch, AE_NO_FLAGS));
        else
          ADD_UNDO(AudioEffect_CurrPos(patch, -1, AE_NO_FLAGS));

        // Apply the changes. Bus sends are linear gains natively, but the
        // catalog (and the LLM) works in dB; convert those back first.
        // Changes whose names are not in the catalog (the summary-only
        // On/Off entries added above) are not real effects and are skipped.
        bool changed_mixer_button = false;

        for (const radium::llm::FxChange &change : changes){
          if (!catalog_names.contains(change.name.toLower()))
            continue; // Summary-only pseudo entries (bus send connections).

          if (is_mixer_button(change.name))
            changed_mixer_button = true;

          const float native_value = gain_db_names.contains(change.name.toLower())
                                     ? db2gain(change.value)
                                     : change.value;
          PLUGIN_set_effect_from_name(plugin, change.name.toUtf8().constData(), native_value, EFFECT_FORMAT_NATIVE);
        }

        for (const radium::llm::FxInstrumentChange &change : instrument_changes)
          PLUGIN_set_effect_value(plugin, -1, change.effect_num,
                                  change.value_is_native ? change.value_native : change.value_scaled,
                                  STORE_VALUE, FX_single,
                                  change.value_is_native ? EFFECT_FORMAT_NATIVE : EFFECT_FORMAT_SCALED);

        GFX_update_instrument_widget(patch);

        // Update the mixer strip immediately when the mute/bypass/solo
        // buttons changed.
        if (changed_mixer_button)
          CHIP_update(plugin);

        return true;
      };

      // Creates/configures/removes modulators (LFOs) for effect parameters.
      // Uses the API functions, which add their own undo entries (creating
      // the modulator patch and adding/removing the target).
      auto apply_modulations = [this](const QVector<radium::llm::FxModulation> &modulations) -> bool
      {
        struct Patch *patch = _patch.data();
        if (patch == NULL)
          return false;

        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        if (plugin == NULL)
          return false;

        auto set_modulator_parameters = [](instrument_t modulator_instrument_id, const radium::llm::FxModulation &modulation, bool use_defaults)
        {
          const QString type = modulation.type.isEmpty() ? QString("sine") : modulation.type;
          int type_index = radium::llm::fx_modulator_type_index(type);
          if (type_index < 0)
            type_index = 0;

          if (!modulation.type.isEmpty() || use_defaults)
            setNativeInstrumentEffect(modulator_instrument_id, "Type", (float)type_index);
          if (modulation.tempo_numerator > 0 || use_defaults)
            setNativeInstrumentEffect(modulator_instrument_id, "Tempo multiplier", (float)(modulation.tempo_numerator > 0 ? modulation.tempo_numerator : 1));
          if (modulation.tempo_denominator > 0 || use_defaults)
            setNativeInstrumentEffect(modulator_instrument_id, "Tempo divisor", (float)(modulation.tempo_denominator > 0 ? modulation.tempo_denominator : 2));
          if (modulation.min_scaled >= 0.0f || use_defaults)
            setNativeInstrumentEffect(modulator_instrument_id, "Lower bound", modulation.min_scaled >= 0.0f ? modulation.min_scaled : 0.0f);
          if (modulation.max_scaled >= 0.0f || use_defaults)
            setNativeInstrumentEffect(modulator_instrument_id, "Upper bound", modulation.max_scaled >= 0.0f ? modulation.max_scaled : 1.0f);
        };

        for (const radium::llm::FxModulation &modulation : modulations){
          const QByteArray effect_name = modulation.effect.toUtf8();

          if (modulation.action == radium::llm::FxModulation::REMOVE){
            const int effect_num = PLUGIN_get_effect_num_from_name(plugin, effect_name.constData());
            struct Patch *modulator_patch = effect_num >= 0 ? MODULATOR_get_modulator_patch(patch, effect_num) : NULL;

            removeModulator(patch->id, effect_name.constData());

            // Delete the modulator instrument if nothing uses it anymore.
            // (LLM-path only; the UI's "Remove modulator" deliberately leaves
            // the instrument so it can be re-assigned.)
            if (modulator_patch != NULL && MODULATOR_get_modulator_targets(modulator_patch->id).num_elements == 0)
              deleteInstrument(modulator_patch->id);

            continue;
          }

          if (modulation.action == radium::llm::FxModulation::REPLACE){
            const int effect_num = PLUGIN_get_effect_num_from_name(plugin, effect_name.constData());
            struct Patch *modulator_patch = effect_num >= 0 ? MODULATOR_get_modulator_patch(patch, effect_num) : NULL;

            if (modulator_patch != NULL){
              set_modulator_parameters(modulator_patch->id, modulation, false);
              continue;
            }

            // No existing modulator (should not normally happen); fall
            // through and add a new one.
          }

          instrument_t modulator_id = createModulator();
          if (!isLegalInstrument(modulator_id))
            return false;

          set_modulator_parameters(modulator_id, modulation, true);
          addModulator(patch->id, effect_name.constData(), modulator_id);
        }

        return true;
      };

      // Creates the requested effect plugins and inserts them, in order,
      // directly after this instrument. The instrument (and any previously
      // inserted plugin) is disconnected from its old destinations, connected
      // to the new plugin, and the new plugin is connected to those same
      // destinations. This keeps the instrument's existing routing (including
      // its bus sends) but processes it through the new plugin.
      auto apply_plugins = [this](const QVector<radium::llm::FxPlugin> &plugins) -> bool
      {
        struct Patch *insert_after = _patch.data();
        if (insert_after == NULL)
          return false;
        if (plugins.isEmpty())
          return true;

        for (const radium::llm::FxPlugin &request : plugins){
          double x,y;
          MW_set_autopos(&x, &y);

          instrument_t new_id;

          if (request.container_name.isEmpty()){
            new_id = createAudioInstrument(request.type_name.toUtf8().constData(),
                                           request.name.toUtf8().constData(),
                                           NULL, x, y, false, true);
          } else {
            // VST/VST3/AU/LV2 plugins live in containers that might not have
            // been populated yet. The description path populates the
            // container on demand and then creates the plugin.
            const char *description = getAudioInstrumentDescription(request.container_name.toUtf8().constData(),
                                                                    request.type_name.toUtf8().constData(),
                                                                    request.name.toUtf8().constData());
            new_id = createAudioInstrumentFromDescription(description, NULL, x, y, false, true);
          }

          if (!isLegalInstrument(new_id))
            return false;

          struct Patch *new_patch = PATCH_get_from_id(new_id);
          if (new_patch == NULL)
            return false;

          SoundPlugin *new_plugin = (SoundPlugin*)new_patch->patchdata;
          if (new_plugin == NULL)
            return false;

          if (insert_effect_patch_after(insert_after, new_patch) == NULL)
            return false;

          insert_after = new_patch;
        }

        return true;
      };

      // Removes effect plugins that are part of this instrument's insert
      // chain. The plugin is bypassed first (its sources are connected to its
      // destinations), so removing it does not break the signal path, and is
      // then deleted as one undo entry together with the rest of the reply.
      auto apply_remove_plugins = [this](const QVector<radium::llm::FxPlugin> &plugins) -> bool
      {
        if (plugins.isEmpty())
          return true;

        struct Patch *source_patch = _patch.data();
        if (source_patch == NULL)
          return false;

        const QVector<InsertedEffectPlugin> inserted = find_inserted_effect_plugins(source_patch);

        QVector<instrument_t> instrument_ids;
        for (const radium::llm::FxPlugin &request : plugins){
          for (const InsertedEffectPlugin &entry : inserted){
            if (radium::llm::fx_plugin_matches(request, entry.fx)
                && !instrument_ids.contains(entry.instrument_id))
              instrument_ids.append(entry.instrument_id);
          }
        }

        for (instrument_t instrument_id : instrument_ids){
          struct Patch *patch = PATCH_get_from_id(instrument_id);
          if (patch == NULL)
            continue;

          SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
          if (plugin == NULL)
            continue;

          // Never remove the main pipe, buses or other permanent patches.
          if (AUDIO_is_permanent_patch(patch))
            continue;

          QVector<struct Patch*> sources;
          QVector<struct Patch*> destinations;

          const int num_in_connections = CHIP_get_num_in_connections(patch);
          for (int i = 0; i < num_in_connections; i++){
            struct Patch *source = CHIP_get_source(patch, i);
            if (source != NULL)
              sources.append(source);
          }

          const int num_out_connections = CHIP_get_num_out_connections(patch);
          for (int i = 0; i < num_out_connections; i++){
            struct Patch *dest = CHIP_get_dest(patch, i);
            if (dest != NULL)
              destinations.append(dest);
          }

          ADD_UNDO(MixerConnections_CurrPos());

          // Bypass the plugin.
          for (struct Patch *source : sources){
            for (struct Patch *dest : destinations){
              if (!MW_are_connected(source, dest))
                MW_connect(source, dest, ConnectionType::IS_SEND);
            }
          }

          for (struct Patch *source : sources)
            MW_disconnect(source, patch);
          for (struct Patch *dest : destinations)
            MW_disconnect(patch, dest);

          deleteInstrument(instrument_id);
        }

        return true;
      };

      // Applies the whole reply (effect value changes, modulation actions and
      // plugin insertions/removals) as ONE undo entry: radium::ScopedUndo
      // keeps all the nested ADD_UNDO calls (patch creation, chip positions,
      // effect values, mixer connections) inside a single undo record.
      auto apply_reply = [read_catalog, apply_changes, apply_modulations, apply_plugins, apply_remove_plugins](QVector<radium::llm::FxChange> &changes,
                                                                                                                const QVector<radium::llm::FxInstrumentChange> &instrument_changes,
                                                                                                                const QVector<radium::llm::FxModulation> &modulations,
                                                                                                                const QVector<radium::llm::FxPlugin> &add_plugins,
                                                                                                                const QVector<radium::llm::FxPlugin> &remove_plugins) -> bool
      {
        radium::ScopedUndo scoped_undo;

        // Remove first, so a reply can replace an effect plugin by removing
        // the old one and adding the new one.
        if (!remove_plugins.isEmpty() && !apply_remove_plugins(remove_plugins))
          return false;

        // A modulated effect must be enabled to be audible (e.g. the
        // highpass filter, or 'System Pitch type' for the pitch shifter).
        {
          QVector<radium::llm::FxChange> modulation_targets;
          for (const radium::llm::FxModulation &modulation : modulations)
            if (modulation.action != radium::llm::FxModulation::REMOVE)
              modulation_targets.append(radium::llm::FxChange{modulation.effect, 0.0f});

          if (!modulation_targets.isEmpty()){
            const QVector<radium::llm::FxParamState> states = read_catalog();
            for (const radium::llm::FxChange &enable : radium::llm::fx_band_enable_changes(states, modulation_targets))
              changes.append(enable);
          }
        }

        if ((!changes.isEmpty() || !instrument_changes.isEmpty()) && !apply_changes(changes, instrument_changes))
          return false;

        if (!modulations.isEmpty() && !apply_modulations(modulations))
          return false;

        if (!add_plugins.isEmpty() && !apply_plugins(add_plugins))
          return false;

        return true;
      };

      // Asks the user whether to create a Faust Dev 2 effect, and starts the
      // existing Faust Dev 2 LLM generation flow on the new instrument if the
      // user accepts.
      auto create_faust_effect = [this](const QString &description, const QString &requested_name) -> bool
      {
        struct Patch *source_patch = _patch.data();
        if (source_patch == NULL)
          return false;

        const QString instrument_name = radium::llm::fx_make_faust_instrument_name(requested_name.isEmpty() ? description : requested_name);

        {
          MyQMessageBox *msg_box = MyQMessageBox::create(false, this);
          msg_box->setAttribute(Qt::WA_DeleteOnClose);
          msg_box->setWindowTitle("Create Faust Dev 2 effect");
          msg_box->setText(QString("No matching effect plugin is available.\n\n"
                                   "Should I create a Faust Dev 2 effect named \"%1\" for \"%2\"?").arg(instrument_name, description));
          msg_box->setStandardButtons(QMessageBox::Yes | QMessageBox::No);
          msg_box->setDefaultButton(QMessageBox::Yes);
          if (safeExec(msg_box, false) != QMessageBox::Yes)
            return true; // The user declined. This is not an error.
        }

        radium::ScopedUndo scoped_undo;

        double x,y;
        MW_set_autopos(&x, &y);

        instrument_t new_id = createAudioInstrument("Faust Dev 2", "Faust Dev 2", instrument_name.toUtf8().constData(), x, y, false, true);
        if (!isLegalInstrument(new_id))
          return false;

        struct Patch *new_patch = PATCH_get_from_id(new_id);
        if (new_patch == NULL)
          return false;

#if defined(WITH_FAUST_DEV) && defined(WITH_FAUST_DEV2)
        {
          // Start with an inaudible/neutral passthrough until the LLM has
          // generated the real effect. This avoids briefly inserting the
          // default Faust synth into the signal path.
          SoundPlugin *new_plugin = (SoundPlugin*)new_patch->patchdata;
          if (new_plugin != NULL){
            FAUST2_set_code(new_plugin, QString("process = _,_;\n"));
            FAUST2_start_compilation(new_plugin);
          }
        }
#endif

        if (insert_effect_patch_after(source_patch, new_patch) == NULL)
          return false;

#if defined(WITH_FAUST_DEV) && defined(WITH_FAUST_DEV2)
        if (!FAUSTWIDGET_generate_effect(new_id, description)){
          GFX_addMessage("Could not start the Faust Dev 2 generation.");
          return false;
        }

        GFX_addMessage("Creating a Faust Dev 2 effect for \"%s\".", description.toUtf8().constData());
#else
        GFX_addMessage("Faust Dev 2 is not available in this build.");
        return false;
#endif

        return true;
      };

      _fx_tune_widget = new LLM_fx_tune_widget(this, read_catalog, read_instrument_context, read_plugin_catalog, read_inserted_plugins, apply_reply, create_faust_effect);

      // Sound measurements are captured only while this instrument's tuning
      // terminal is on screen (and, because the terminal belongs to one patch,
      // only for that instrument). Nothing is captured otherwise.
      _fx_tune_widget->measurement_active_callback = [this](bool active){
        struct Patch *patch = _patch.data();
        SoundPlugin *plugin = patch != NULL ? (SoundPlugin*)patch->patchdata : NULL;
        if (plugin == NULL){
          MEASUREMENTS_clear_target();
          return;
        }

        if (active)
          MEASUREMENTS_set_target(SP_get_sound_producer(plugin), plugin);
        else
          MEASUREMENTS_clear_target();

        if (getenv("RADIUM_DEBUG_MEASUREMENTS") != NULL)
          printf("measurements: host: tuner %s for patch=%p plugin=%p\n",
                 active ? "shown, capturing" : "hidden, capture stopped", (void*)patch, (void*)plugin);
      };
      gridLayout_3->addWidget(_fx_tune_widget, 1, 0);

      // Checkbox in the instrument header, to the right of the "Configure
      // Auto-suspend" button, which shows/hides the LLM effect tuning
      // terminal. The terminal starts hidden so it does not take up space
      // unless it is used.
      _llm_checkbox = new MyQCheckBox("LLM", _plugin_widget);
      _llm_checkbox->setToolTip("Show the LLM effect tuning terminal.");
      _llm_checkbox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
      // Insert after the fixed spacer that follows the "Configure
      // Auto-suspend" button, so no unused fixed gap is left on the right
      // side of the checkbox when the window is narrowed.
      _plugin_widget->horizontalLayout_2->insertWidget(_plugin_widget->horizontalLayout_2->indexOf(_plugin_widget->auto_bypass_menu_button) + 2,
                                                       _llm_checkbox);
      _llm_checkbox->setChecked(false);
      _fx_tune_widget->setVisible(false);
      QObject::connect(_llm_checkbox, &QCheckBox::toggled, [this](bool visible){
        if (_fx_tune_widget != NULL)
          _fx_tune_widget->setVisible(visible);

        // When hiding the LLM prompt, shrink the instrument widget to its
        // minimum height without changing its size type. Dropping the minimum
        // height is needed because the LLM terminal may have raised it.
        if (!visible) {
          setMinimumHeight(10);
          updateGeometry();
          S7CALL2(void_void,"FROM_C-minimize-lowertab");
        }
      });
    }


    // Adjust output widget widths
    {
      QFontMetrics fm(QApplication::font());
      //QRect r =fm.boundingRect(SLIDERPAINTER_get_string(_painter));
      int width = fm.boundingRect("Dry: 100% Wet: 0% Gak").width() + 10;
      lowpass_freq_slider->setMinimumWidth(width);
      highpass_freq_slider->setMinimumWidth(width);
      drywet_slider->setMinimumWidth(width);
    }


#if 0
    if(plugin->type->num_inputs==0){
      input_volume_layout->setDisabled(true);
    }
#endif


    // set the scroll bar itself to size 10.
    //    scrollArea->horizontalScrollBar()->setFixedHeight(10);
#ifdef USE_QT5
    //scrollArea->verticalScrollBar()->hide();
#endif
    
    updateWidgets();
    setupPeakAndAutomationStuff();


    {
      instrument_t id = _patch->id;
      pitch_type_button->_show_popup_menu = [id](){
        struct Patch *patch = PATCH_get_from_id(id);
        if (patch != NULL){
          SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
          if (plugin != NULL)
            S7CALL2(dyn_dyn_charpointer,"FROM_C-show-effect-popup-menu", DYN_create_instrument(id), PLUGIN_get_effect_name(plugin, plugin->type->num_effects + EFFNUM_PITCH_TYPE));
        }
      };
    }

    ab_reset_button->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-reset-instrument-a/b-popup-menu", 0);
    };
    
    ab0->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 0);
    };
    ab1->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 1);
    };
    ab2->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 2);
    };
    ab3->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 3);
    };
    ab4->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 4);
    };
    ab5->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 5);
    };
    ab6->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 6);
    };
    ab7->_show_popup_menu = [](){
      S7CALL2(void_int,"FROM_C-select-instrument-a/b-popup-menu", 7);
    };

  }

  void enterEvent(QEnterEvent *event) override{
    setCursor(Qt::ArrowCursor);
  }
  
  void prepare_for_deletion(void){
    close_mixer_strip();
    _comp_widget->prepare_for_deletion();
    _plugin_widget->prepare_for_deletion();
    if (_fx_tune_widget != NULL)
      _fx_tune_widget->prepare_for_deletion();
  }

  ~Audio_instrument_widget(){
    if(_size_type != SIZETYPE_NORMAL)
      change_height(SIZETYPE_NORMAL);

    prepare_for_deletion();
  }
     
  MyQSlider *get_system_slider(int system_effect){
    switch(system_effect){

    case EFFNUM_INPUT_VOLUME:
      return input_volume_slider; 
    case EFFNUM_VOLUME:
      return volume_slider; 
    case EFFNUM_OUTPUT_VOLUME:
      return output_volume_slider;

    case EFFNUM_BUS1:
      return bus1_slider; 
    case EFFNUM_BUS2:
      return bus2_slider; 
    case EFFNUM_BUS3:
      return bus3_slider; 
    case EFFNUM_BUS4:
      return bus4_slider; 
    case EFFNUM_BUS5:
      return bus5_slider; 

    case EFFNUM_DRYWET:
      return drywet_slider; 

    case EFFNUM_PAN:
      return panning_slider; 

    case EFFNUM_LOWPASS_FREQ:
      return lowpass_freq_slider; 

    case EFFNUM_HIGHPASS_FREQ:
      return highpass_freq_slider; 

    case EFFNUM_EQ1_FREQ:
      return eq1_freq_slider; 
    case EFFNUM_EQ1_GAIN:
      return eq1_gain_slider; 

    case EFFNUM_EQ2_FREQ:
      return eq2_freq_slider; 
    case EFFNUM_EQ2_GAIN:
      return eq2_gain_slider; 

    case EFFNUM_LOWSHELF_FREQ:
      return lowshelf_freq_slider; 
    case EFFNUM_LOWSHELF_GAIN:
      return lowshelf_gain_slider; 

    case EFFNUM_HIGHSHELF_FREQ:
      return highshelf_freq_slider; 
    case EFFNUM_HIGHSHELF_GAIN:
      return highshelf_gain_slider; 

    case EFFNUM_DELAY_TIME:
      return rightdelay_slider; 

    case EFFNUM_PITCH_PITCH:
      return pitch_slider; 

    default:
      return NULL;
    }
  }

  MyQCheckBox *get_system_checkbox(int system_effect){
    switch(system_effect){
    case EFFNUM_INPUT_VOLUME_ONOFF:
      return input_volume_onoff;
    case EFFNUM_VOLUME_ONOFF:
      return mute_button;
    case EFFNUM_OUTPUT_VOLUME_ONOFF:
      return output_volume_onoff;
    case EFFNUM_PAN_ONOFF:
      return panning_onoff;
    case EFFNUM_EFFECTS_ONOFF:
      return bypass_button;
    case EFFNUM_SOLO_ONOFF:
      return solo_button;
    case EFFNUM_LOWPASS_ONOFF:
      return lowpass_onoff;
    case EFFNUM_HIGHPASS_ONOFF:
      return highpass_onoff;

    case EFFNUM_EQ1_ONOFF:
      return eq1_onoff;
    case EFFNUM_EQ2_ONOFF:
      return eq2_onoff;
    case EFFNUM_LOWSHELF_ONOFF:
      return lowshelf_onoff;
    case EFFNUM_HIGHSHELF_ONOFF:
      return highshelf_onoff;

    case EFFNUM_DELAY_ONOFF:
      return rightdelay_onoff;

    default:
      return NULL;
    }
  }

  void updateSliderString(int system_effect){
    MyQSlider *slider = get_system_slider(system_effect);
    if(slider==NULL)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    SLIDERPAINTER_set_recording_color(slider->_painter, PLUGIN_is_recording_automation(plugin, effect_num));
    
    QString p = get_parameter_prepend_text(_patch.data(), effect_num);
    
    char buf[64]={0};
    PLUGIN_get_display_value_string(plugin, effect_num, buf, 64);
    if(system_effect==EFFNUM_DRYWET)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(buf));
    else if(system_effect==EFFNUM_BUS1)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(0)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS2)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(1)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS3)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(2)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS4)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(3)) + ": " + QString(buf));
    else if(system_effect==EFFNUM_BUS5)
      SLIDERPAINTER_set_string(slider->_painter, p + QString(BUS_get_bus_name(4)) + ": " + QString(buf));
    else {
      SLIDERPAINTER_set_string(slider->_painter, p + QString(PLUGIN_get_effect_name(plugin, effect_num)+strlen("System ")) + ": " + QString(buf));
    }
      
    slider->update();
  }

  void updateSlider(int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    MyQSlider* slider = get_system_slider(system_effect);
    float value = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);
    //printf("            %s, value: %f\n",PLUGIN_get_effect_name(plugin, effect_num), value);
    slider->setValue(value * 10000);

    updateSliderString(system_effect);
  }

  void updateChecked(QAbstractButton* checkwidget, int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;

    int effect_num = type->num_effects + system_effect;

    bool val = PLUGIN_get_effect_value(plugin,effect_num, VALUE_FROM_PLUGIN) >= 0.5f;

    if(system_effect==EFFNUM_VOLUME_ONOFF || system_effect==EFFNUM_EFFECTS_ONOFF)
      checkwidget->setChecked(!val);
    else
      checkwidget->setChecked(val);

    //printf("     Update Checked %d\n", checkwidget==mute_button);
    if (checkwidget==mute_button){
      if (PATCH_get_from_id(_patch->id)==NULL) // happens during initialization.
        mute_button->_is_implicitly_on = false;
      else
        mute_button->_is_implicitly_on = instrumentIsImplicitlyMuted(_patch->id);
    }
    
    if (checkwidget==solo_button){
      if (PATCH_get_from_id(_patch->id)==NULL) // happens during initialization.
        solo_button->_is_implicitly_on = false;
      else
        solo_button->_is_implicitly_on = instrumentIsImplicitlySoloed(_patch->id);
    }
    
    if(system_effect==EFFNUM_INPUT_VOLUME_ONOFF)
      input_volume_slider->setEnabled(val);
    //if(system_effect==EFFNUM_VOLUME_ONOFF)
    //  volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_OUTPUT_VOLUME_ONOFF)
      output_volume_slider->setEnabled(val);
    if(system_effect==EFFNUM_LOWPASS_ONOFF)
      lowpass_freq_slider->setEnabled(val);
    if(system_effect==EFFNUM_HIGHPASS_ONOFF)
      highpass_freq_slider->setEnabled(val);
    if(system_effect==EFFNUM_EQ1_ONOFF){
      eq1_freq_slider->setEnabled(val);
      eq1_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_EQ2_ONOFF){
      eq2_freq_slider->setEnabled(val);
      eq2_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_LOWSHELF_ONOFF){
      lowshelf_freq_slider->setEnabled(val);
      lowshelf_gain_slider->setEnabled(val);
    }
    if(system_effect==EFFNUM_HIGHSHELF_ONOFF){
      highshelf_freq_slider->setEnabled(val);
      highshelf_gain_slider->setEnabled(val);
    }

    if(system_effect==EFFNUM_DELAY_ONOFF){
      rightdelay_slider->setEnabled(val);
    }
  }

  void updateBusChecked(QAbstractButton* checkwidget, QSlider* bus_slider, SoundProducer *bus){
    if(bus==NULL){
      R_ASSERT_NON_RELEASE(g_is_loading);
      return;
    }

    struct Patch *from = _patch.data();
    
    if(instrumentIsOpen(from->id)==false) // happens when creating new instrument.
      return;
    
    struct Patch *to = const_cast<Patch*>(SP_get_plugin(bus)->patch);

    if(instrumentIsOpenAndAudio(from->id)==false){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    if(instrumentIsOpenAndAudio(to->id)==false){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    bool are_connected = MW_are_connected(from, to);
    checkwidget->setChecked(are_connected);
    bus_slider->setEnabled(are_connected);

    if(are_connected)
      checkwidget->setEnabled(true);
    else{
      bool can_connect = CONNECTION_can_connect(from, to);
      //printf("   %s => %s legal: %d\n", from->name, to->name, can_connect);
      checkwidget->setEnabled(can_connect);
    }
  }
    
  QElapsedTimer time_of_last_minheight_inc;
  int number_of_minheight_incs = 0;
  
  void call_regularly_for_painters(void){
    
    //SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (_plugin_widget->isVisible())
      _plugin_widget->calledRegularlyByParent();
    
    callSystemSliderpainterUpdateCallbacks();
    
    if (_comp_widget->isVisible())
      _comp_widget->calledRegularlyByParent();
  }

  void calledRegularlyByParent(void){
    
    //printf("hello %p\n", this);

    if (_size_type == SIZETYPE_HALF && !instrumentInMixer())
      set_half_height();
    
    int curr_minimum_height = minimumHeight();
    
    bool is_visible = scrollArea->verticalScrollBar()->isVisible();
    
    if (is_visible) {

      printf("  **UP** MINIMIZING UP to %d\n", curr_minimum_height+5);
      setMinimumHeight(curr_minimum_height+5);
      time_of_last_minheight_inc.restart();
      number_of_minheight_incs++;
      
    } else if (_size_type != SIZETYPE_HALF && curr_minimum_height > 5){

      if (time_of_last_minheight_inc.elapsed() > 1000 && number_of_minheight_incs < 3) { // Wait at least 1 second before trying to lower it again. If not, it won't stabilize. (might not stabilize now either though, but it won't be that visible now)
        printf("  **DOWN** MINIMIZING DOWN to %d\n", curr_minimum_height-5);
        setMinimumHeight(curr_minimum_height-5);
        number_of_minheight_incs = 0;
      }
    }
  }

  void callSystemSliderpainterUpdateCallbacks(void){
    
    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        slider->calledRegularlyByParent();
    }

  }

  void setupPeakAndAutomationStuff(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;

    // peaks
    
    int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      SLIDERPAINTER_set_peak_value_pointers(volume_slider->_painter, num_outputs, plugin->volume_peaks.decaying_dbs, true);

      SLIDERPAINTER_set_peak_value_pointers(output_volume_slider->_painter, num_outputs, plugin->output_volume_peaks.decaying_dbs, true);

      SLIDERPAINTER_set_peak_value_pointers(bus1_slider->_painter,2, plugin->bus0_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus2_slider->_painter,2, plugin->bus1_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus3_slider->_painter,2, plugin->bus2_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus4_slider->_painter,2, plugin->bus3_volume_peaks.decaying_dbs, true);
      SLIDERPAINTER_set_peak_value_pointers(bus5_slider->_painter,2, plugin->bus4_volume_peaks.decaying_dbs, true);
    }

    if(num_inputs>0)
      SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_inputs, plugin->input_volume_peaks.decaying_dbs, true);
    else if (num_outputs>0)
      SLIDERPAINTER_set_peak_value_pointers(input_volume_slider->_painter, num_outputs, plugin->input_volume_peaks.decaying_dbs, true);

    // automation

    for(int system_effect=0 ; system_effect<NUM_SYSTEM_EFFECTS ; system_effect++){
      int effect_num = type->num_effects + system_effect;
      
      MyQSlider *slider = get_system_slider(system_effect);
      if (slider != NULL) // effect can be a checkbox.
        SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->slider_automation_values[effect_num]);
    }

#if 0
    PluginWidget *plugin_widget = NULL;

    if (_plugin_widget->_plugin_widget != NULL)
      plugin_widget = _plugin_widget->_plugin_widget;

#ifdef WITH_FAUST_DEV
    else if (_plugin_widget->_faust_plugin_widget != NULL)
      plugin_widget = _plugin_widget->_faust_plugin_widget->_plugin_widget;
#endif

    //if (plugin_widget != NULL)
    //  plugin_widget->set_automation_value_pointers(plugin);
#endif
    
    if (_plugin_widget->_pd_plugin_widget != NULL){

      for(unsigned int i=0; i<_plugin_widget->_pd_plugin_widget->_controllers.size(); i++) {
        Pd_Controller_widget *c = _plugin_widget->_pd_plugin_widget->_controllers[i];

        MyQSlider *slider = c->value_slider;
        if (slider != NULL){
          int effect_num = slider->_effect_num;
          SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->slider_automation_values[effect_num]);
        }
      }
    }
  }

  void updateWidgets(void){
    radium::ScopedGeneration scoped_update(_is_updating);
    
    //printf("updateWidgets %s\n", _patch->name);
    set_arrow_style(controlsArrow, false);
    set_arrow_style(arrow2, false);
    set_arrow_style(arrow3);
    set_arrow_style(arrow4);
    set_arrow_style(arrow5);
    set_arrow_style(arrow6);
    set_arrow_style(arrow8);
    set_arrow_style(arrow9);
    set_arrow_style(arrow10);
    set_arrow_style(arrow7, false);
    set_arrow_style(browserArrow, false);
    set_arrow_style(mixerstripArrow, false);
    if(pipe_label!=NULL)
      set_arrow_style(pipe_label,false);

    QFontMetrics fm(QApplication::font());
    int fontheight = fm.height() + 3;

    browserWidget->setFixedWidth(fontheight);
    controlsWidget->setFixedWidth(fontheight);
    compressorWidget->setFixedWidth(fontheight);
    equalizerWidget->setFixedWidth(fontheight);
    mixerstripWidget->setFixedWidth(fontheight);
    
    /*
    show_compressor->setFixedWidth(fontheight);
    show_equalizer->setFixedWidth(fontheight);
    show_browser->setFixedWidth(fontheight);
    show_controls->setFixedWidth(fontheight);
    */
    /*
    show_compressor->setFixedWidth(controlsArrow->width());
    show_equalizer->setFixedWidth(controlsArrow->width());
    show_browser->setFixedWidth(browserArrow->width());
    show_controls->setFixedWidth(controlsArrow->width());
    */
    

    updateSlider(EFFNUM_BUS1);
    updateSlider(EFFNUM_BUS2);
    updateSlider(EFFNUM_BUS3);
    updateSlider(EFFNUM_BUS4);
    updateSlider(EFFNUM_BUS5);
    updateSlider(EFFNUM_DRYWET);
    updateSlider(EFFNUM_INPUT_VOLUME);
    updateSlider(EFFNUM_VOLUME);
    updateSlider(EFFNUM_OUTPUT_VOLUME);
    updateSlider(EFFNUM_PAN);

    updateSlider(EFFNUM_LOWPASS_FREQ);
    updateSlider(EFFNUM_HIGHPASS_FREQ);
    updateSlider(EFFNUM_EQ1_FREQ);
    updateSlider(EFFNUM_EQ1_GAIN);
    updateSlider(EFFNUM_EQ2_FREQ);
    updateSlider(EFFNUM_EQ2_GAIN);
    updateSlider(EFFNUM_LOWSHELF_FREQ);
    updateSlider(EFFNUM_LOWSHELF_GAIN);
    updateSlider(EFFNUM_HIGHSHELF_FREQ);
    updateSlider(EFFNUM_HIGHSHELF_GAIN);
    updateSlider(EFFNUM_DELAY_TIME);
    updateSlider(EFFNUM_PITCH_PITCH);

    updateChecked(input_volume_onoff, EFFNUM_INPUT_VOLUME_ONOFF);
    updateChecked(mute_button, EFFNUM_VOLUME_ONOFF);
    updateChecked(output_volume_onoff, EFFNUM_OUTPUT_VOLUME_ONOFF);

    Buses buses = MIXER_get_buses();
    updateBusChecked(bus1_onoff, bus1_slider, buses.bus1);
    updateBusChecked(bus2_onoff, bus2_slider, buses.bus2);
    updateBusChecked(bus3_onoff, bus3_slider, buses.bus3);
    updateBusChecked(bus4_onoff, bus4_slider, buses.bus4);
    updateBusChecked(bus5_onoff, bus5_slider, buses.bus5);
    
    updateChecked(panning_onoff, EFFNUM_PAN_ONOFF);
    updateChecked(bypass_button, EFFNUM_EFFECTS_ONOFF);
    updateChecked(solo_button, EFFNUM_SOLO_ONOFF);

    updateChecked(lowpass_onoff, EFFNUM_LOWPASS_ONOFF);
    updateChecked(highpass_onoff, EFFNUM_HIGHPASS_ONOFF);
    updateChecked(eq1_onoff, EFFNUM_EQ1_ONOFF);
    updateChecked(eq2_onoff, EFFNUM_EQ2_ONOFF);
    updateChecked(lowshelf_onoff, EFFNUM_LOWSHELF_ONOFF);
    updateChecked(highshelf_onoff, EFFNUM_HIGHSHELF_ONOFF);
    updateChecked(rightdelay_onoff, EFFNUM_DELAY_ONOFF);

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;

    for(int system_effect=EFFNUM_INPUT_VOLUME;system_effect<NUM_SYSTEM_EFFECTS;system_effect++){
      MyQSlider *slider = get_system_slider(system_effect);
      if(slider!=NULL){
        int effect_num = type->num_effects + system_effect;
        slider->_effect_num = effect_num;
        slider->_patch.set(_patch.data());
      }

      MyQCheckBox *checkbox = get_system_checkbox(system_effect);
      if(checkbox!=NULL){
        int effect_num = type->num_effects + system_effect;
        checkbox->_effect_num = effect_num;
        checkbox->_patch.set(_patch.data());
      }

      if(slider!=NULL && checkbox!=NULL)
        RError("weird");
    }

    update_all_ab_buttons();

#if 0 //ndef RELEASE
    static int checknum;
    //    PLAYER_lock();
    fprintf(stderr,"\n\n     STARTING CHECK %d\n",checknum);
        
    const radium::Vector2<SoundProducer*> *org_all_sp = MIXER_get_all_SoundProducers();
    //void *elements1=org_all_sp->elements;
    int size = org_all_sp->size();
    QVector<SoundProducer*> sps;

    for(int n=0;n<size;n++)
      //    for (auto *sp : *all_sp)
      sps.push_back(org_all_sp->at(n));//[n]);//->elements[n]);

    for(int i = 0 ; i < 50000 ; i ++){
      const radium::Vector2<SoundProducer*> *all_sp = MIXER_get_all_SoundProducers();
      //void *elements2=all_sp->elements;
      R_ASSERT(all_sp->size() == size);
      
      for(int n=0;n<size;n++){
        SoundProducer *sp1 = sps[n];
        //SoundProducer *sp2 = all_sp->at(n);//[n];//((SoundProducer**)elements1)[n];//org_all_sp->elements[n];
        SoundProducer *sp2 = all_sp->elements[n];//at(n);//[n];//((SoundProducer**)elements1)[n];//org_all_sp->elements[n];
        if (sp1 != sp2){
          SoundPlugin *p1 = SP_get_plugin(sp1);
          SoundPlugin *p2 = SP_get_plugin(sp2);
          fprintf(stderr,"org_all_sp: %p, all_sp: %p, sp1: %p, sp2: %p. plug1: %s, plug2: %s\n",sp1,sp2,sp1,sp2,p1==NULL?"(null)":p1->patch->name,p2==NULL?"(null)":p2->patch->name);
          abort();
        }
        R_ASSERT(sps[n]==all_sp->at(n));//elements[n]);
      }
      
      //SP_get_SoundProducer(plugin);
    }
    fprintf(stderr,"\n\n     FINISHING CHECK %d\n",checknum++);
    //    PLAYER_unlock();
#endif
    
    //int num_inputs = type->num_inputs;
    int num_outputs = type->num_outputs;

    if(num_outputs>0){
      bool effects_are_on = ATOMIC_GET(plugin->effects_are_on);

      // In Half/Full mode, don't disable any widgets when the instrument
      // is bypassed.
      const bool enable_widgets = effects_are_on || _size_type != SIZETYPE_NORMAL;
      
      input_volume_layout->setEnabled(enable_widgets);
      
      if(plugin->type->num_inputs>0)
        _plugin_widget->setEnabled(enable_widgets);
      
      filters_widget->setEnabled(enable_widgets);
      
      volume_widget->setEnabled(enable_widgets);

      pitch_slider->setEnabled(plugin->pitch_type!=SPT_DISABLED);
      
      if (root->song->include_pan_and_dry_in_wet_signal){
        bool pan_is_on = enable_widgets && PLUGIN_get_effect_value(plugin, type->num_effects + EFFNUM_PAN_ONOFF, VALUE_FROM_PLUGIN) >= 0.5f;
        bool width_is_on = enable_widgets && PLUGIN_get_effect_value(plugin, type->num_effects + EFFNUM_DELAY_ONOFF, VALUE_FROM_PLUGIN) >= 0.5f;
        
        panning_slider->setEnabled(pan_is_on);
        rightdelay_slider->setEnabled(width_is_on);

        panning_onoff->setEnabled(enable_widgets);
        rightdelay_onoff->setEnabled(enable_widgets);
      }
      
      drywet_slider->setEnabled(enable_widgets);

      if (root->song->mute_system_buses_when_bypassed) {
        bus1_widget->setEnabled(enable_widgets);
        bus2_widget->setEnabled(enable_widgets);
        bus3_widget->setEnabled(enable_widgets);
        bus4_widget->setEnabled(enable_widgets);
        bus5_widget->setEnabled(enable_widgets);
      }
    }

    _comp_widget->update_gui();

    show_browser->setChecked(plugin->show_browser_gui);
    show_controls->setChecked(plugin->show_controls_gui);
    show_equalizer->setChecked(plugin->show_equalizer_gui);
    show_compressor->setChecked(plugin->show_compressor_gui);
    show_mixer_strip->setChecked(plugin->show_mixer_strip_gui);

    update_mixer_strip(plugin->show_mixer_strip_gui);

    switch(plugin->pitch_type){
      case SPT_DISABLED:
        pitch_type_button->setText("");
        break;
        
      case SPT_ONLY_LEFT:
        pitch_type_button->setText("L");
        break;
        
      case SPT_ONLY_RIGHT:
        pitch_type_button->setText("R");
        break;
        
      case SPT_ALL_CHANNELS:
        pitch_type_button->setText("A");
        break;
        
      case SPT_INVERTED_CHANNELS:
        pitch_type_button->setText("I");
        break;
    }

    {
      pitch_type_button->setMinimumWidth(rightdelay_onoff->width());
      pitch_type_button->setMaximumWidth(rightdelay_onoff->width());
    }

    {
      QFontMetrics fm(QApplication::font());
      int max_width = fm.boundingRect("   Eq2 F.: 20000.0 Hz").width() * 1.3;
      filters_widget->setMaximumWidth(max_width);
      filters_widget->setMinimumWidth(max_width);
    }
    
    {
      QFontMetrics fm(QApplication::font());
      int max_width = fm.boundingRect("   Dry: 46%. Wet: 54%").width() * 1.3;
      outputs_widget->setMaximumWidth(max_width);
      outputs_widget->setMinimumWidth(max_width);
    }
    
    _patch_widget->updateWidgets();

    if(_sample_requester_widget != NULL){
      _sample_requester_widget->updateWidgets();
    }

    update();
  }

  void set_plugin_value(int sliderval, int system_effect){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + system_effect;

    if (_is_updating==0)
      PLUGIN_set_effect_value(plugin, -1, effect_num, sliderval/10000.0f, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);

    updateSliderString(system_effect);
  }

  void set_bus_value(SoundProducer *bus, bool val){
    if (_is_updating > 0)
      return;

    if (bus==NULL){
      R_ASSERT_NON_RELEASE(g_is_starting_up);
      return;
    }

    ADD_UNDO(MixerConnections_CurrPos());
    
    if (val)
      MW_connect(_patch.data(), const_cast<Patch*>(SP_get_plugin(bus)->patch), ConnectionType::IS_SEND);
    else
      MW_disconnect(_patch.data(), const_cast<Patch*>(SP_get_plugin(bus)->patch));
  }
  
private:

  void close_mixer_strip(void){
    if (_mixerstrip_guinum >= 0){
      int64_t guinum = _mixerstrip_guinum;
      _mixerstrip_guinum = -1;
      if (gui_isOpen(guinum))
        gui_close(guinum);
    }
  }

  void update_mixer_strip(bool should_be_visible){
    if (should_be_visible){
      if (_mixerstrip_guinum < 0 || !gui_isOpen(_mixerstrip_guinum)){
        _mixerstrip_guinum = -1;

        if (instrumentIsOpenAndAudio(_patch->id)){
          int strip_height = height();
          if (strip_height < 100)
            strip_height = 400;

          int64_t guinum = MIXERSTRIP_create_instrument_widget(_patch->id, 100 * g_gfx_scale, strip_height);
          if (guinum >= 0){
            QWidget *mixer_strip_widget = API_gui_get_widget(guinum);
            if (mixer_strip_widget != NULL){
              mixer_strip_layout->addWidget(mixer_strip_widget);
              _mixerstrip_guinum = guinum;
            }else{
              gui_close(guinum);
            }
          }
        }
      }

      if (_mixerstrip_guinum >= 0)
        mixer_strip_container->setVisible(_size_type == SIZETYPE_NORMAL);
      else
        mixer_strip_container->hide();

    }else{
      close_mixer_strip();
      mixer_strip_container->hide();
    }
  }

  QVector<QWidget*> hidden_widgets;
  int height_before_large;

  void set_half_height(void){
    setFixedHeight(g_main_window->height() / 2);
  }

  // enable for faust and multiband
  void show_large(SizeType new_size_type){
    printf("show_large\n");

    R_ASSERT_RETURN_IF_FALSE(new_size_type!=_size_type);
    R_ASSERT_RETURN_IF_FALSE(new_size_type!=SIZETYPE_NORMAL);

    if (_size_type==SIZETYPE_NORMAL){
      hidden_widgets.clear();

      height_before_large = height();
      
      for (int i = 0; i < effects_layout->count(); ++i){
        QWidget *widget = effects_layout->itemAt(i)->widget();
        if (widget != NULL){
          if (widget != _plugin_widget && widget != _sample_requester_widget && widget->isVisible()){
            widget->hide();
            hidden_widgets.push_back(widget);
        }
        }
      }
    }

    if (instrumentInMixer())
      MW_instrument_widget_set_size(this, _size_type, new_size_type);

    if (new_size_type==SIZETYPE_HALF) {

      if (!instrumentInMixer())
        set_half_height();
      
    } else {
      setMinimumHeight(0);
      setMaximumHeight(16777214);//g_main_window->height());
    }

    // Move the mute/solo/bypass buttons into the plugin widget header,
    // right next to the "Random" button, so they remain available in
    // Half/Full mode where the outputs widget (their normal place) is
    // hidden. Inserting in order is idempotent, so this also works when
    // switching between Half and Full mode.
    {
      QHBoxLayout *header_layout = _plugin_widget->horizontalLayout_2;

      // Move the instrument selection button (normally placed in the patch
      // widget header) to the left of the instrument name in the plugin
      // widget header, since the patch widget is hidden in Half/Full mode.
      header_layout->insertWidget(header_layout->indexOf(_plugin_widget->info_button),
                                  _patch_widget->select_instrument);
      _patch_widget->select_instrument->show();

      const int pos = header_layout->indexOf(_plugin_widget->random100_button) + 1;

      header_layout->insertWidget(pos, mute_button);
      header_layout->insertWidget(pos + 1, solo_button);
      header_layout->insertWidget(pos + 2, bypass_button);

      mute_button->show();
      solo_button->show();
      bypass_button->show();
    }

    _size_type = new_size_type;

    // Refresh the enabled states of all widgets: in Half/Full mode no
    // widgets are disabled by bypass, so if the instrument was bypassed
    // while in normal mode, everything must be re-enabled now.
    updateWidgets();
  }

  void show_small(void){
    printf("show_small\n");

    R_ASSERT_RETURN_IF_FALSE(_size_type!=SIZETYPE_NORMAL);

    if (instrumentInMixer())
      MW_instrument_widget_set_size(this, _size_type, SIZETYPE_NORMAL);
    
    _size_type = SIZETYPE_NORMAL;
    
    setMinimumHeight(10);
    setMaximumHeight(16777214);
    //resize(width(), height_before_large);
    
    for (auto *widget : hidden_widgets)
      widget->show();
    
    hidden_widgets.clear();

    // Move the mute/solo/bypass buttons back to their normal place in the
    // outputs widget. (They become visible again when the outputs widget
    // was re-shown by the loop above.)
    {
      // Move the instrument selection button back to the patch widget header.
      _patch_widget->horizontalLayout->insertWidget(0, _patch_widget->select_instrument);
      _patch_widget->select_instrument->show();

      horizontalLayout_8->insertWidget(0, mute_button);
      horizontalLayout_8->insertWidget(1, solo_button);
      horizontalLayout_8->insertWidget(2, bypass_button);
    }

    // Refresh the enabled states of all widgets (in normal mode bypass
    // disables the effect widgets again).
    updateWidgets();
  }

  void hide_non_instrument_widgets(void){
    /*
    GFX_HideEditor();
    GFX_HideMixer();
    GFX_PlayListWindowToBack();
    */
    if (instrumentInMixer()){
      MW_hide_non_instrument_widgets();
    } else {
      hideUpperPartOfMainWindow();
    }

    MW_disable_include_instrument_checkbox();
  }

  void show_non_instrument_widgets(void){

    if (instrumentInMixer()){
      MW_show_non_instrument_widgets();
    } else {
      showUpperPartOfMainWindow();
    }

    MW_enable_include_instrument_checkbox();
    
    /*
    GFX_ShowEditor();
    GFX_ShowMixer();
    GFX_PlayListWindowToFront();
    */
  }

  
public:

  // Note: May be called from the destructor (new_size_type=SIZETYPE_NORMAL when called from the destructor)
  void change_height(SizeType new_size_type)
  {
    if (new_size_type==_size_type)
    {
      // The faust plugin widget keeps its own size state, and its hide/show
      // events are suppressed while loading, so it can get out of sync with
      // this widget. Keep it in sync even when we early-return.
#ifdef WITH_FAUST_DEV
      if (_plugin_widget->_faust_plugin_widget != NULL)
        _plugin_widget->_faust_plugin_widget->change_height(new_size_type);
#endif
      return;
    }

    if(isVisible())
      _patch->widget_height_type = new_size_type;
    
    if (new_size_type==SIZETYPE_FULL)
      hide_non_instrument_widgets();    
    else if (_size_type == SIZETYPE_FULL)
      show_non_instrument_widgets();

    if (new_size_type==SIZETYPE_NORMAL)
      SEQUENCER_show_because_instrument_widget_is_large();
    else {
      if (!instrumentInMixer())
        SEQUENCER_hide_because_instrument_widget_is_large();
    }
    
    if (new_size_type==SIZETYPE_NORMAL)
      show_small();
    else
      show_large(new_size_type);
    
#ifdef WITH_FAUST_DEV
    if (_plugin_widget->_faust_plugin_widget != NULL)
      _plugin_widget->_faust_plugin_widget->change_height(new_size_type);
#endif

    if (new_size_type==SIZETYPE_NORMAL && !instrumentInMixer())
      S7CALL2(void_void,"FROM_C-minimize-lowertab");
  }

  void hideEvent(QHideEvent * event) override
  {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL)
      ATOMIC_SET(plugin->is_visible, false);

    _size_type_before_hidden = _size_type;
    
    if(_size_type != SIZETYPE_NORMAL)
    {
      // Only overwrite the saved size type if the widget was actually in
      // Half or Full mode. If the widget is hidden while still in normal
      // mode (for instance while loading a song), the saved state must be
      // kept so that half/full mode is entered the next time the widget is
      // shown.
      _patch->widget_height_type = _size_type;
      change_height(SIZETYPE_NORMAL);
      //show_small();  // If not, all instrument widgets will have large height.
    }
  }
  
  void showEvent(QShowEvent * event) override
  {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL)
      ATOMIC_SET(plugin->is_visible, true);
    
    // Restore the saved size type. The saved patch state is authoritative
    // since _size_type_before_hidden can be clobbered by a hide event that
    // happens before half/full mode has ever been entered (for instance
    // while loading a song).
    const SizeType restore_type = _patch->widget_height_type != SIZETYPE_NORMAL
                                    ? _patch->widget_height_type
                                    : _size_type_before_hidden;
    
    if (restore_type != SIZETYPE_NORMAL)
      change_height(restore_type);
    else if (!instrumentInMixer())
      S7CALL2(void_void,"FROM_C-minimize-lowertab");

    //      show_large();
  }


  void update_all_ab_buttons(void){
    radium::ScopedGeneration scoped_update(_is_updating);

    ab_reset_button->setMinimumWidth(_ab_checkbox_width);
    ab_reset_button->setMaximumWidth(_ab_checkbox_width);

    update_ab_button(ab0, 0);
    update_ab_button(ab1, 1);
    update_ab_button(ab2, 2);
    update_ab_button(ab3, 3);
    update_ab_button(ab4, 4);
    update_ab_button(ab5, 5);
    update_ab_button(ab6, 6);
    update_ab_button(ab7, 7);
    
    if(_plugin_widget != NULL)
      _plugin_widget->update_ab_buttons();
  }

  int _ab_checkbox_width = -1;

  void update_ab_button(MyQCheckBox *checkbox, int num){
    radium::ScopedGeneration scoped_update(_is_updating);
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    checkbox->_show_enabled_marker = false;
    
    QString c = QString("ABCDEFGH"[num]);

    bool is_selected=plugin->curr_ab_num==num || plugin->ab_is_valid[num];
    
    if (is_selected)
      checkbox->setText(c+"*");
    else
      checkbox->setText(c);

    /*
    if (!is_selected && _ab_checkbox_width==-1){
      checkbox->adjustSize();
      _ab_checkbox_width = checkbox->width();
    }
    */
    
    checkbox->setMinimumWidth(_ab_checkbox_width);
    checkbox->setMaximumWidth(_ab_checkbox_width);
  }

  MyQCheckBox *get_ab_checkbox(int num){
    if (num<0 || num >= 8){
      R_ASSERT(false);
      num=0;
    }
    MyQCheckBox *array[] = {ab0,ab1,ab2,ab3,ab4,ab5,ab6,ab7};
    return array[num];
  }
  
  bool arrgh = false;
  
  void ab_pressed(int num, bool val){
    if (_is_updating > 0)
      return;
    
    MyQCheckBox *checkbox = get_ab_checkbox(num);
    
    if (arrgh==false) {

      arrgh=true;
      
      //printf("   AB_PRESSED. Num: %d. Val: %d\n", num, val);

      if (val==true){

        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        if (plugin==NULL) return;
        
        PLUGIN_change_ab(plugin, num);

        if (_plugin_widget != NULL){
          _plugin_widget->_ignore_checkbox_stateChanged = true; { // Note to Qt developers: 'setChecked' should have an optional arguments whether to send signal to callbacks.
            
            _plugin_widget->ab_a->setChecked(num==0);
            _plugin_widget->ab_b->setChecked(num==1);
            /*          
                        ab0->setChecked(num==0);
                        ab1->setChecked(num==1);
                        ab2->setChecked(num==2);
                        ab3->setChecked(num==3);
                        ab4->setChecked(num==4);
                        ab5->setChecked(num==5);
                        ab6->setChecked(num==6);
                        ab7->setChecked(num==7);
            */
          }_plugin_widget->_ignore_checkbox_stateChanged = false;      
        }
        
      }

      update_ab_button(checkbox, num);

      arrgh=false;
    }
    
  }

public slots:

  void on_ab_reset_button_clicked(){
    if (_is_updating > 0)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    PLUGIN_reset_ab(plugin,-1);
    update_all_ab_buttons();
  }

  void on_ab0_toggled(bool val){ab_pressed(0, val);}
  void on_ab1_toggled(bool val){ab_pressed(1, val);}
  void on_ab2_toggled(bool val){ab_pressed(2, val);}
  void on_ab3_toggled(bool val){ab_pressed(3, val);}
  void on_ab4_toggled(bool val){ab_pressed(4, val);}
  void on_ab5_toggled(bool val){ab_pressed(5, val);}
  void on_ab6_toggled(bool val){ab_pressed(6, val);}
  void on_ab7_toggled(bool val){ab_pressed(7, val);}
  
  
  
#if 0
  void on_arrow1_toggled(bool val){
    _plugin_widget->setVisible(val);
  }

  void on_arrow2_toggled(bool val){
    filters_widget->setVisible(val);
  }

  void on_arrow3_toggled(bool val){
    outputs_widget1->setVisible(val);
    outputs_widget2->setVisible(val);
  }
#endif

  void on_show_browser_toggled(bool val){
    if (_is_updating > 0)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_BROWSER_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    if(_sample_requester_widget!=NULL)
      _sample_requester_widget->setVisible(val);
  }

  void on_show_controls_toggled(bool val){
    if (_is_updating > 0)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_CONTROLS_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    _plugin_widget->setVisible(val);
    spacer_holder->setVisible(!val);
  }

  void on_show_equalizer_toggled(bool val){
    if (_is_updating > 0)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_EQ_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    filters_widget->setVisible(val);
  }

  void on_show_compressor_toggled(bool val){
    if (_is_updating > 0)
      return;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_COMP_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    _comp_widget->setVisible(val);
  }

  void on_show_mixer_strip_toggled(bool val){
    if (_is_updating > 0)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;

    const SoundPluginType *type = plugin->type;
    int effect_num = type->num_effects + EFFNUM_MIXER_STRIP_SHOW_GUI;
    PLUGIN_set_effect_value(plugin, -1, effect_num, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    update_mixer_strip(val);
  }

  void on_input_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_INPUT_VOLUME_ONOFF);
    input_volume_slider->setEnabled(val);
  }

  void on_mute_button_toggled(bool val){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;

    set_plugin_value(val==false ? 10000 : 0, EFFNUM_VOLUME_ONOFF);
    //volume_slider->setEnabled(!val);
    CHIP_update(plugin);
  }

  void on_output_volume_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_OUTPUT_VOLUME_ONOFF);
    output_volume_slider->setEnabled(val);
  }

  
  void on_bus1_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus1, val);
    bus1_slider->setEnabled(val);
  }
  void on_bus2_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus2, val);
    bus2_slider->setEnabled(val);
  }
  void on_bus3_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus3, val);
    bus3_slider->setEnabled(val);
  }
  void on_bus4_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus4, val);
    bus4_slider->setEnabled(val);
  }
  void on_bus5_onoff_toggled(bool val){
    set_bus_value(MIXER_get_buses().bus5, val);
    bus5_slider->setEnabled(val);
  }

  
  void on_bus1_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS1);
  }
  void on_bus2_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS2);
  }
  void on_bus3_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS3);
  }
  void on_bus4_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS4);
  }
  void on_bus5_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_BUS5);
  }

  // effects onoff / dry_wet

  void on_bypass_button_toggled(bool val){
    if(_is_updating==0)
      setInstrumentBypass(val, _patch->id);
  }

  void on_solo_button_toggled(bool val){
    if(_is_updating==0)
      setInstrumentSolo(val, _patch->id);
  }

  void on_drywet_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_DRYWET);
  }

  void on_input_volume_slider_valueChanged(int val){
    if (_is_updating > 0)
      return;
    
    if(_patch->patchdata != NULL) { // temp fix
      set_plugin_value(val, EFFNUM_INPUT_VOLUME);

      if (GFX_OS_patch_is_system_out(_patch.data()))
        OS_GFX_SetVolume(val);

      CHIP_update((SoundPlugin*)_patch->patchdata);
    }
  }

  void on_volume_slider_valueChanged(int val){
    if (_is_updating > 0)
      return;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;
    
    set_plugin_value(val, EFFNUM_VOLUME);
    CHIP_update(plugin);
  }

  void on_output_volume_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_OUTPUT_VOLUME);
  }

  // lowpass 
  void on_lowpass_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_LOWPASS_ONOFF);
    lowpass_freq_slider->setEnabled(val);
  }

  void on_lowpass_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWPASS_FREQ);
  }

  // highpass 
  void on_highpass_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_HIGHPASS_ONOFF);
    highpass_freq_slider->setEnabled(val);
  }

  void on_highpass_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHPASS_FREQ);
  }

  // eq1
  void on_eq1_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_EQ1_ONOFF);
    eq1_freq_slider->setEnabled(val);
    eq1_gain_slider->setEnabled(val);
  }

  void on_eq1_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ1_FREQ);
  }

  void on_eq1_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ1_GAIN);
  }

  // eq2
  void on_eq2_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_EQ2_ONOFF);
    eq2_freq_slider->setEnabled(val);
    eq2_gain_slider->setEnabled(val);
  }

  void on_eq2_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ2_FREQ);
  }

  void on_eq2_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_EQ2_GAIN);
  }

  // lowshelf
  void on_lowshelf_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_LOWSHELF_ONOFF);
    lowshelf_freq_slider->setEnabled(val);
    lowshelf_gain_slider->setEnabled(val);
  }

  void on_lowshelf_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWSHELF_FREQ);
  }

  void on_lowshelf_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_LOWSHELF_GAIN);
  }

  // highshelf
  void on_highshelf_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_HIGHSHELF_ONOFF);
    highshelf_freq_slider->setEnabled(val);
    highshelf_gain_slider->setEnabled(val);
  }

  void on_highshelf_freq_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHSHELF_FREQ);
  }

  void on_highshelf_gain_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_HIGHSHELF_GAIN);
  }

  // rightdelay
  void on_rightdelay_onoff_toggled(bool val){
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_DELAY_ONOFF);
    rightdelay_slider->setEnabled(val);
  }

  void on_rightdelay_slider_valueChanged(int val){
    set_plugin_value(val, EFFNUM_DELAY_TIME);
  }

  void on_pitch_type_button_pressed(void){
    S7CALL2(void_instrument,"FROM_C-pitch_type_button-pressed", _patch->id);
  }
  
  void on_pitch_slider_valueChanged(int val){
    //set_plugin_value(val==0.5 ? 0 : 10000, EFFNUM_PITCH_TYPE);
    set_plugin_value(val, EFFNUM_PITCH_PITCH);
  }

  // Volume

#if 0
  // Velocity

  void on_velocity_slider_valueChanged(int val)
  {
    _patch->standardvel = val*MAX_VELOCITY/10000;
  }
#endif


#if 0
  void on_velocity_onoff_stateChanged( int val)
  {
    if (val==Qt::Checked){
      velocity_slider->setEnabled(true);
      velocity_spin->setEnabled(true);;
    }else if(val==Qt::Unchecked){
      velocity_slider->setEnabled(false);
      velocity_spin->setEnabled(false);
    }
  }
#endif

  // Panning

  void on_panning_slider_valueChanged( int val)
  {
    set_plugin_value(val, EFFNUM_PAN);
  }

  void on_panning_onoff_toggled(bool val)
  {
    set_plugin_value(val==true ? 10000 : 0, EFFNUM_PAN_ONOFF);
    panning_slider->setEnabled(val);
  }
};

}

 
/*
static Pd_Plugin_widget *AUDIOWIDGET_get_pd_plugin_widget(Audio_instrument_widget *audio_instrument_widget){
  return audio_instrument_widget->_plugin_widget->_pd_plugin_widget;
}
*/

#ifdef WITH_FAUST_DEV
static Faust_Plugin_widget *AUDIOWIDGET_get_faust_plugin_widget(Audio_instrument_widget *audio_instrument_widget){
  return audio_instrument_widget->_plugin_widget->_faust_plugin_widget;
}
#endif

Sample_requester_widget *AUDIOWIDGET_get_sample_requester_widget(struct Patch *patch){
  return get_audio_instrument_widget(patch)->_sample_requester_widget;
}

void AUDIOWIDGET_change_height(struct Patch *patch, SizeType type){
  get_audio_instrument_widget(patch)->change_height(type);
}

void AUDIOWIDGET_toggle_size_type(struct Patch *patch, SizeType type){
  Audio_instrument_widget *w = get_audio_instrument_widget(patch);

  if (w==NULL || w->_plugin_widget==NULL)
    return;

  if (type==SIZETYPE_HALF)
    w->_plugin_widget->half_checkbox->setChecked(!w->_plugin_widget->half_checkbox->isChecked());

  else if (type==SIZETYPE_FULL)
    w->_plugin_widget->max_checkbox->setChecked(!w->_plugin_widget->max_checkbox->isChecked());
}

/*
SizeType AUDIOWIDGET_get_size_type(struct Patch *patch){
  return get_audio_instrument_widget(patch)->_size_type;
}
*/

void AUDIOWIDGET_set_ab(struct Patch *patch, int ab_num){
  auto *w = get_audio_instrument_widget(patch);

  switch(ab_num){
    case 0: w->ab0->setChecked(true); break;
    case 1: w->ab1->setChecked(true); break;
    case 2: w->ab2->setChecked(true); break;
    case 3: w->ab3->setChecked(true); break;
    case 4: w->ab4->setChecked(true); break;
    case 5: w->ab5->setChecked(true); break;
    case 6: w->ab6->setChecked(true); break;
    case 7: w->ab7->setChecked(true); break;
  }
}

void AUDIOWIDGET_redraw_ab(struct Patch *patch){
  get_audio_instrument_widget(patch)->update_all_ab_buttons();
}
  
#if 0
void AUDIOWIDGET_show_large(struct Patch *patch){
  get_audio_instrument_widget(patch)->show_large();
}

void AUDIOWIDGET_show_small(struct Patch *patch){
  get_audio_instrument_widget(patch)->show_small();
}
#endif
