/* Copyright 2018 Kjetil S. Matheussen

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



#include <QVector>

#include "nsmtracker.h"
#include "SeqAutomation.hpp"
#include "seqtrack_proc.h"

#include "seqblock_automation_proc.h"
#include "seqblock_stretchspeed_proc.h"

#define D(n)
//#define D(n) n



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

static inline int get_num_elements(const struct SeqBlock *seqblock){
  const double total_time = seqblock->t.num_samples; //default_duration / resample_ratio; //2.0;//(s2-s1);
  int num_elements = total_time / RADIUM_BLOCKSIZE;
  return num_elements;
}

static const StretchspeedTimeConversionTable default_table = {
  .stretch_automation_compensation = 1.0,
  .speed_automation_compensation = 1.0,
  .stretchspeed_automation_compensation = 1.0,
  .num_elements = 0,
  .array = NULL
};

// TODO: This function needs to be memoized so that we don't have to calculate for every pixel when moving a seqblock with stretch automation.
static StretchspeedTimeConversionTable get_time_conversion_table(struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed) {
  StretchspeedTimeConversionTable table = default_table;

  const bool do_stretch = RT_seqblock_automation_is_enabled(stretch);
  const bool do_speed = RT_seqblock_automation_is_enabled(speed);

  R_ASSERT_RETURN_IF_FALSE2(do_stretch||do_speed, table);

  const double total_time = seqblock->t.num_samples; //default_duration / resample_ratio; //2.0;//(s2-s1);
  int num_elements = get_num_elements(seqblock); //total_time / RADIUM_BLOCKSIZE;
    
  int64_t *array = (int64_t*)V_calloc(sizeof(int64_t), (num_elements+1));

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


namespace{

  class CachedTimeConversionTable;
  
  static QVector<CachedTimeConversionTable*> g_cached_tables;

  class CachedTimeConversionTable{
    bool _speed_enabled;
    bool _stretch_enabled;
    
    QVector<radium::AutomationNode> _speed;
    QVector<radium::AutomationNode> _stretch;

    int _num_seqblock_users = 0;

    double _time_when_num_seqblock_users_decreased_to_0 = -1;
    
  public:
    
    StretchspeedTimeConversionTable _table;

    CachedTimeConversionTable(struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed, const StretchspeedTimeConversionTable &table)
      : _speed_enabled(RT_seqblock_automation_is_enabled(speed))
      , _stretch_enabled(RT_seqblock_automation_is_enabled(stretch))
      , _speed(SEQBLOCK_AUTOMATION_get_SeqAutomation(speed).get_qvector())
      , _stretch(SEQBLOCK_AUTOMATION_get_SeqAutomation(stretch).get_qvector())
      , _table(table)
    {
      apply_table_to_seqblock(seqblock);
      g_cached_tables.push_back(this);
    }

    ~CachedTimeConversionTable(){
      R_ASSERT(_num_seqblock_users==0);
      int num_removed = g_cached_tables.removeAll(this);
      R_ASSERT(num_removed==1);

      V_free(_table.array);
      
      D(printf("   Releasing Cached time conversion table. Size: %d\n", (int)g_cached_tables.size()););
    }
    
    void inc_users(void){
      _num_seqblock_users++;
    }
    
    int dec_users(void){
      R_ASSERT_RETURN_IF_FALSE2(_num_seqblock_users>0, 0);
      
      _num_seqblock_users--;
      
      if (_num_seqblock_users==0)
        _time_when_num_seqblock_users_decreased_to_0 = TIME_get_ms();
      
      return _num_seqblock_users;
    }

    bool maybe_release(void){
      if (_num_seqblock_users==0 && (TIME_get_ms()-_time_when_num_seqblock_users_decreased_to_0) > 1000){
        delete this;
        return true;
      }

      return false;
    }
    
  private:
    
    bool automation_vectors_are_equal(const QVector<radium::AutomationNode> &a1, const QVector<radium::AutomationNode> &a2) const {
      if (a1.size() != a2.size())
        return false;

      int i=0;
      for(const radium::AutomationNode &node1 : a1){
        const radium::AutomationNode &node2 = a2.at(i);
        if (nodes_are_not_equal(node1, node2))
          return false;
        i++;
      }
    
      return true;
    }

  public:
  
    void apply_table_to_seqblock(struct SeqBlock *seqblock){
      inc_users();
      {
        seqblock->conversion_table = _table;
        radium::PlayerLock lock;
      }
    }

    bool is_compatible(const struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed) const {
      if(get_num_elements(seqblock) != _table.num_elements)
        return false;

      else if (_speed_enabled != RT_seqblock_automation_is_enabled(speed))
        return false;

      else if (_stretch_enabled != RT_seqblock_automation_is_enabled(stretch))
        return false;
      
      else if (automation_vectors_are_equal(SEQBLOCK_AUTOMATION_get_SeqAutomation(speed).get_qvector(), _speed)==false)
        return false;

      else if (automation_vectors_are_equal(SEQBLOCK_AUTOMATION_get_SeqAutomation(stretch).get_qvector(), _stretch)==false)
        return false;

      else 
        return true;
    }
    
    bool try_to_apply_to_seqblock(struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed){
      R_ASSERT_RETURN_IF_FALSE2(seqblock->block==NULL, false);

      if(is_compatible(seqblock, stretch, speed)==false)
        return false;
      
      apply_table_to_seqblock(seqblock);
      D(printf("        GOTIT. num_users: %d\n", _num_seqblock_users);)
      return true;
    }
  };
}

static bool CACHE_find_time_conversion_table(struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed){
  for(auto *cached : g_cached_tables){
    if (cached->try_to_apply_to_seqblock(seqblock, stretch, speed)==true)
      return true;
  }

  return false;
}

static void CACHE_release(const StretchspeedTimeConversionTable &table){
  if (table.array==NULL)
    return;

  for(auto *cached : g_cached_tables){
    if (cached->_table.array==table.array){      
      int now = cached->dec_users();
      D(printf("  Decced %d -> %d\n", now+1, now););
      break;      
    }
  }
}

static void CACHE_add_and_apply(struct SeqBlock *seqblock, const struct SeqblockAutomation *stretch, const struct SeqblockAutomation *speed, const StretchspeedTimeConversionTable &table){
  new CachedTimeConversionTable(seqblock, stretch, speed, table);
}

void SEQBLOCK_STRETCHSPEED_call_me_very_often(void){
  int i = 0;
  int num_released = 0;
  while(i < g_cached_tables.size() && num_released < 100){
    auto *cached = g_cached_tables.at(i);
    if (cached->maybe_release())
      num_released++;
    else
      i++;
  }
}

void SEQBLOCK_STRETCHSPEED_call_me_when_seqblock_is_released(const struct SeqBlock *seqblock){
  D(printf("SEQBLOCK_STRETCHSPEED_call_me_when_seqblock_is_released called for %p\n", seqblock););
  CACHE_release(seqblock->conversion_table);
}
  
void SEQBLOCK_calculate_time_conversion_table(struct SeqBlock *seqblock, bool seqblock_is_live){
  if (seqblock->block!=NULL)
    return;

  CACHE_release(seqblock->conversion_table);

  const auto *stretch = seqblock->automations[SAT_STRETCH];
  R_ASSERT_RETURN_IF_FALSE(stretch!=NULL);

  const auto *speed = seqblock->automations[SAT_SPEED];
  R_ASSERT_RETURN_IF_FALSE(speed!=NULL);

  const bool do_stretch = RT_seqblock_automation_is_enabled(stretch);
  const bool do_speed = RT_seqblock_automation_is_enabled(speed);

  if(!do_stretch && !do_speed){
    radium::PlayerLock lock;
    seqblock->conversion_table = default_table;
    return;
  }
  
  if (CACHE_find_time_conversion_table(seqblock, stretch, speed)==true)
    return;

  StretchspeedTimeConversionTable table = get_time_conversion_table(seqblock, stretch, speed);

  CACHE_add_and_apply(seqblock, stretch, speed, table);  
}
