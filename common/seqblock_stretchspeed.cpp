
#include <QVector>

#include "nsmtracker.h"
#include "SeqAutomation.hpp"
#include "seqtrack_proc.h"

#include "seqblock_automation_proc.h"



static void apply_to_time_conversion_table_single(int num_elements, int64_t *ret, radium::SeqAutomationIterator<radium::AutomationNode> &iterator, const enum Seqblock_Automation_Type _sat, const double time_inc, double &total_automation_time) {

  total_automation_time = 0;
  double time = 0.0;

  for(int i=0 ; i<num_elements ; i++, time+=time_inc){
      
    ret[i] = total_automation_time;
        
    double value = iterator.get_value(time);
    double stretch_or_speed = _sat==SAT_SPEED ? get_speed_from_automation(value) : get_stretch_from_automation(value);
      
    total_automation_time += RADIUM_BLOCKSIZE / stretch_or_speed;
  }

  ret[num_elements] = total_automation_time;
}

static void apply_to_time_conversion_table_double(int num_elements, int64_t *ret, radium::SeqAutomationIterator<radium::AutomationNode> &stretch_iterator, radium::SeqAutomationIterator<radium::AutomationNode> &speed_iterator, const double time_inc, double &total_stretch, double &total_speed) {

  total_stretch = 0;
  total_speed = 0;
  double total_automation_time = 0;
  double time = 0.0;

  for(int i=0 ; i<num_elements ; i++, time+=time_inc){
      
    ret[i] = total_automation_time;
        
    double stretch_value = stretch_iterator.get_value(time);
    double stretch = get_stretch_from_automation(stretch_value);
    double stretch_duration = RADIUM_BLOCKSIZE / stretch;
    total_stretch += stretch_duration;
      
    double speed_value = speed_iterator.get_value(time);
    double speed = get_speed_from_automation(speed_value); 
    double speed_duration = RADIUM_BLOCKSIZE / speed;
    total_speed += speed_duration;
     
    total_automation_time += RADIUM_BLOCKSIZE / (stretch * speed);
  }

  ret[num_elements] = total_automation_time;
}

// TODO: This function needs to be memoized so that we don't have to calculate for every pixel when moving a seqblock with stretch automation.
static StretchspeedTimeConversionTable get_time_conversion_table(struct SeqBlock *seqblock) {
  StretchspeedTimeConversionTable table = {
    .stretch_automation_compensation = 1.0,
    .speed_automation_compensation = 1.0,
    .stretchspeed_automation_compensation = 1.0,
    .num_elements = 0,
    .array = NULL
  };

  const auto *stretch = seqblock->automations[SAT_STRETCH];
  R_ASSERT_RETURN_IF_FALSE2(stretch!=NULL, table);

  const auto *speed = seqblock->automations[SAT_SPEED];
  R_ASSERT_RETURN_IF_FALSE2(speed!=NULL, table);

  const bool do_stretch = RT_seqblock_automation_is_enabled(stretch);
  const bool do_speed = RT_seqblock_automation_is_enabled(speed);

  if(!do_stretch && !do_speed)
    return table;

  const double total_time = seqblock->t.num_samples; //default_duration / resample_ratio; //2.0;//(s2-s1);
  int num_elements = total_time / RADIUM_BLOCKSIZE;
    
  int64_t *array = (int64_t*)talloc_atomic(sizeof(int64_t)*(num_elements+1));

  //_automation.print();

  double time_inc = (double)seqblock->t.default_duration / (double)num_elements;

  double total_stretch = total_time;
  double total_speed = total_time;

  radium::SeqAutomationIterator<radium::AutomationNode> stretch_iterator(SEQBLOCK_AUTOMATION_get_SeqAutomation(stretch));
  radium::SeqAutomationIterator<radium::AutomationNode> speed_iterator(SEQBLOCK_AUTOMATION_get_SeqAutomation(speed));

  if (do_stretch && do_speed)
    apply_to_time_conversion_table_double(num_elements, array, stretch_iterator, speed_iterator, time_inc, total_stretch, total_speed);
  else if (do_stretch)
    apply_to_time_conversion_table_single(num_elements, array, stretch_iterator, SAT_STRETCH, time_inc, total_stretch);
  else if (do_speed)
    apply_to_time_conversion_table_single(num_elements, array, speed_iterator, SAT_SPEED, time_inc, total_speed);
  else
    R_ASSERT(false);

  table.stretch_automation_compensation = total_stretch / total_time;
  table.speed_automation_compensation = total_speed / total_time;
    
  table.num_elements = num_elements;
  table.array = array;

  //printf("1. stretch c: %f. speed c: %f. total c: %f. total time: %f\n", table.stretch_automation_compensation, table.speed_automation_compensation, g_total, total_time);
    
  if (do_stretch){
    table.stretch_automation_compensation = total_stretch / total_time;
    table.stretchspeed_automation_compensation = table.stretch_automation_compensation;
  } else {
    table.stretch_automation_compensation = 1.0;
  }

  //printf("2. stretch c: %f. speed c: %f. total c: %f. total time: %f\n", table.stretch_automation_compensation, table.speed_automation_compensation, g_total, total_time);
    
  if (do_speed){
    table.speed_automation_compensation = total_speed / total_time;
    table.stretchspeed_automation_compensation = table.speed_automation_compensation;
  } else {
    table.speed_automation_compensation = 1.0;
  }

  //printf("3. stretch c: %f. speed c: %f. total c: %f. total time: %f\n", table.stretch_automation_compensation, table.speed_automation_compensation, g_total, total_time);
    
  if (do_stretch && do_speed){
    double uncompensated_duration = array[num_elements];
    table.stretchspeed_automation_compensation = uncompensated_duration / total_time;
    table.speed_automation_compensation = uncompensated_duration / total_stretch;
      
    //printf("4. stretch c: %f. speed c: %f. total c: %f. uncompensated duration: %f. total_stretch: %f. total_speed: %f. total time: %f\n", table.stretch_automation_compensation, table.speed_automation_compensation, g_total, uncompensated_duration, total_stretch, total_speed, total_time);
  }
    
    
  return table;
}


static void calculate_time_conversion_table(struct SeqBlock *seqblock) {
  StretchspeedTimeConversionTable table = get_time_conversion_table(seqblock);
  {
    radium::PlayerLock lock;
    seqblock->conversion_table = table;
  }
}


void SEQBLOCK_calculate_time_conversion_table(struct SeqBlock *seqblock, bool seqblock_is_live){
  if (seqblock->block!=NULL)
    return;

  calculate_time_conversion_table(seqblock);
}


