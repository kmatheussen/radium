#include <stdio.h>
#include <assert.h>
#include <memory>
#include <mutex>
#include <atomic>

#include "../common/nsmtracker.h"
#include "../common/Vector.hpp"
#include "../common/TimeData.hpp"

#include "test_dummies.c"


// Note: We are also testing TimeData.



int g_num_freed = 0;
int g_num_allocated = 0;
int g_num_timedata_vectors = 0;


namespace{

struct Gakk : public r::TimeDataDataTypeRef<int> {
  Gakk(int val, bool called_from_timedata = false)
    : TimeDataDataTypeRef(make_ratio(val,1), val, 1)
  {
    if (called_from_timedata)
      g_num_allocated++;
  }
};

using GakkPtr = std::shared_ptr<Gakk>;

using GakkTimeData = r::TimeData<r::TimeData_shared_ptr<Gakk>, r::RT_TimeData_Player_Cache<typeof(Gakk::_val)>>;
 
}

static void printit(const radium::Vector<GakkPtr> &radium_vector,
                                 const std::vector<GakkPtr> &std_vector,
                                 GakkTimeData &timedata)
{
  GakkTimeData::Reader reader(&timedata);
  
  for(int i = 0 ; i < radium_vector.size() ; i++){
    printf("%d: %d - %d - %d\n", i, radium_vector.at_ref(i)->_val, std_vector[i]->_val, reader.at_ref(i)->_val);
  }
}

static void assert_equal_vectors(const radium::Vector<GakkPtr> &radium_vector,
                                 const std::vector<GakkPtr> &std_vector,
                                 GakkTimeData &timedata)
{
  assert(radium_vector.size()==(int)std_vector.size());

  GakkTimeData::Reader reader(&timedata);
  
  assert(radium_vector.size()==reader.size());

  for(int i = 0 ; i < radium_vector.size() ; i++){
    
    if (radium_vector.at_ref(i)->_val != std_vector[i]->_val){
      printit(radium_vector, std_vector, timedata);
      
      printf("\n111. Wrong at %d: %d - %d\n", i, radium_vector.at_ref(i)->_val, std_vector[i]->_val);
      assert(radium_vector.at_ref(i)->_val == std_vector[i]->_val);

      assert(radium_vector.at_ref(i).use_count() == std_vector[i].use_count());
    }
    
  }

  std::vector<GakkPtr> sorted = radium_vector.to_std_vector();
  std::stable_sort(sorted.begin(), sorted.end(), [](auto a, auto b){
    return a->_val < b->_val;
  });
  
  for(int i = 0 ; i < (int)sorted.size() ; i++){
    
    if (reader.at_ref(i)->_val != sorted[i]->_val){
      printit(radium_vector, sorted, timedata);
      printf("\n222. Wrong at %d: %d - %d\n", i, reader.at_ref(i)->_val, sorted[i]->_val);
      assert(reader.at_ref(i)->_val == sorted[i]->_val);

      assert(reader.at_ref(i)->_num_references == sorted[i].use_count());
    }
    
  }
}

static void push_back_element(radium::Vector<GakkPtr> &radium_vector,
                              std::vector<GakkPtr> &std_vector,
                              GakkTimeData &timedata)
{
  auto gakk = std::make_shared<Gakk>(rand() % 1000);
  radium_vector.push_back(gakk);
  std_vector.push_back(gakk);

  GakkTimeData::Writer(&timedata).add(new Gakk(gakk->_val, true));
}

static void push_back_ref_ref_element(radium::Vector<GakkPtr> &radium_vector,
                                      std::vector<GakkPtr> &std_vector,
                                      GakkTimeData &timedata)
{
  int val = rand() % 1000;
  
  auto gakk = std::make_shared<Gakk>(val);
  radium_vector.push_back(std::move(gakk));
  
  auto gakk2 = std::make_shared<Gakk>(val);
  std_vector.push_back(std::move(gakk2));

  GakkTimeData::Writer writer(&timedata);
  r::TimeData_shared_ptr<Gakk> gakk3(new Gakk(val, true));
  writer.add(std::move(gakk3));
}

static void remove_element(radium::Vector<GakkPtr> &radium_vector,
                           std::vector<GakkPtr> &std_vector,
                           GakkTimeData &timedata)
{
  int size = radium_vector.size();
  if (size==0)
    return;
  
  const int pos = rand() % size;

  auto &gakk = radium_vector.at_ref(pos);

  const bool keep_order = chance(0.5);

  //printit(radium_vector, std_vector, timedata);
  
  //printf(" Removing element at pos %d (%d)\n", pos, gakk->_val);

  const int timedata_pos = GakkTimeData::Reader(&timedata).find_element_at_ratio(gakk->get_time());
  assert(timedata_pos >= 0);
  
  // 1.
  radium_vector.remove(gakk, keep_order);

  // 2.
  if (keep_order){
    std_vector.erase(std_vector.begin() + pos);
  } else {
    std_vector[pos] = std_vector[size-1];
    std_vector.erase(std_vector.end() - 1);
  }

  // 3.
  GakkTimeData::Writer(&timedata).remove_at_pos(timedata_pos);

  /*
  printf("    >>>REMOVEDF POS %d - %d\n", pos, timedata_pos);
  printit(radium_vector, std_vector, timedata);
  printf("    <<<REMOVEDF POS %d - %d\n", pos, timedata_pos);
  */
}

static void remove_pos_element(radium::Vector<GakkPtr> &radium_vector,
                               std::vector<GakkPtr> &std_vector,
                               GakkTimeData &timedata)
{
  int size = radium_vector.size();
  if (size==0)
    return;
  
  int pos = rand() % size;

  bool keep_order = chance(0.5);
  
  const int timedata_pos = GakkTimeData::Reader(&timedata).find_element_at_ratio(radium_vector.at_ref(pos)->get_time());
  assert(timedata_pos >= 0);
  
  //printf(" Removing pos %d\n", pos);

  // 1.
  radium_vector.remove_pos(pos, keep_order);

  // 2.
  if (keep_order){
    std_vector.erase(std_vector.begin() + pos);
  } else {
    std_vector[pos] = std_vector[size-1];
    std_vector.erase(std_vector.end() - 1);
  }

  // 3.
  GakkTimeData::Writer(&timedata).remove_at_pos(timedata_pos);
}

static void clear_elements(radium::Vector<GakkPtr> &radium_vector,
                           std::vector<GakkPtr> &std_vector,
                           GakkTimeData &timedata)
{
  radium_vector.clear();
  std_vector.clear();
  GakkTimeData::Writer(&timedata).clear();
}

static void sort_elements(radium::Vector<GakkPtr> &radium_vector,
                          std::vector<GakkPtr> &std_vector,
                          GakkTimeData &timedata)
{
  radium_vector.sort([](auto a, auto b){
    return a->_val < b->_val;
  });

  std::stable_sort(std_vector.begin(), std_vector.end(), [](auto a, auto b){
    return a->_val < b->_val;
  });

  // (timedata is always sorted)
}

static void append_elements(radium::Vector<GakkPtr> &radium_vector,
                            std::vector<GakkPtr> &std_vector,
                            GakkTimeData &timedata)
{
  int size = rand() % 10;

  radium::Vector<GakkPtr> v;
  
  for(int i = 0 ; i < size ; i++){
    auto gakk = std::make_shared<Gakk>(rand() % 1000);
    v.push_back(std::move(gakk));
  }

  radium_vector.append(v);

  for(auto gakk : v){
    std_vector.push_back(gakk);
  }

  for(auto gakk : v)
    GakkTimeData::Writer(&timedata).add(new Gakk(gakk->_val, true));
}

static void pop_element(radium::Vector<GakkPtr> &radium_vector,
                        std::vector<GakkPtr> &std_vector,
                        GakkTimeData &timedata)
{
  int size = radium_vector.size();
  if (size==0)
    return;
  
  int pos = rand() % size;

  bool keep_order = chance(0.5);

  const int timedata_pos = GakkTimeData::Reader(&timedata).find_element_at_ratio(radium_vector.at_ref(pos)->get_time());
  assert(timedata_pos >= 0);

  // 1.
  GakkPtr popped = radium_vector.pop(pos, keep_order);
  (void)popped;
  
  //printf(" Popping pos %d. Val: %d\n", pos, popped->_val);

  // 2.
  if (keep_order){
    std_vector.erase(std_vector.begin() + pos);
  } else {
    std_vector[pos] = std_vector[size-1];
    std_vector.erase(std_vector.end() - 1);
  }

  // 3.
  GakkTimeData::Writer(&timedata).remove_at_pos(timedata_pos);
}

static void remove_at_positions(radium::Vector<GakkPtr> &radium_vector,
                                std::vector<GakkPtr> &std_vector,
                                GakkTimeData &timedata)
{
  sort_elements(radium_vector, std_vector, timedata);
  
  std::vector<int> positions;
  
  for(int i = 0 ; i < radium_vector.size() ; i++)
    if (chance(0.5))
      positions.push_back(i);

  int dec = 0;
  for(int pos : positions){
    radium_vector.remove_pos(pos - dec, true);
    std_vector.erase(std_vector.begin() + pos - dec);
    dec++;
  }

  GakkTimeData::Writer(&timedata).remove_at_positions(positions);
}

static void run_tests(void){

  radium::Vector<GakkPtr> radium_vector;
  std::vector<GakkPtr> std_vector;
  GakkTimeData timedata;
  
  double start_time = get_ms();
  double last_print = start_time;
  double duration;

  int64_t num_iterations = 0;
    
  do{
    
    num_iterations++;
    
    switch(rand() % 9){
      case 0:
        if (chance(0.5))
          push_back_element(radium_vector, std_vector, timedata);
        break;
      case 1:
        if (chance(0.5))
          push_back_ref_ref_element(radium_vector, std_vector, timedata);
        break;
      case 2:
        if (chance(0.2))
          remove_element(radium_vector, std_vector, timedata);
        break;
      case 3:
        if (chance(0.2))
          remove_pos_element(radium_vector, std_vector, timedata);
        break;
      case 4:
        if (chance(0.03))
          clear_elements(radium_vector, std_vector, timedata);
        break;
      case 5:
        if (chance(0.8))
          sort_elements(radium_vector, std_vector, timedata);
        break;
      case 6:
        if (chance(0.2))
          append_elements(radium_vector, std_vector, timedata);
        break;
      case 7:
        if (chance(0.1))
          pop_element(radium_vector, std_vector, timedata);
        break;
      case 8:
        if (chance(0.05))
          remove_at_positions(radium_vector, std_vector, timedata);
        break;
      default:
        abort();
        break;
    }

    double now = get_ms();
  
    duration = now - start_time;

    if ((now-last_print) > 100){
      last_print = now;
      printf("%d/%d, ", radium_vector.size(), (int)num_iterations);
      fflush(stdout);
    }

    assert_equal_vectors(radium_vector, std_vector, timedata);

  } while(duration < 14000);

  printf("\n");
}



int main(void){
  int seed = time(NULL);
  printf("Seed: %d\n", (int)seed);
  
  srand(seed);

  {
    auto gakk = std::make_shared<Gakk>(2);
    
    {
      radium::Vector<GakkPtr> gakks;
      gakks.push_back(gakk);
      assert(gakks.size()==1);
      assert(gakk.use_count()==2);
    }

    assert(gakk.use_count()==1);
  }

  run_tests();

  assert(g_num_allocated==g_num_freed);
  assert(g_num_timedata_vectors==0);
  return 0;
}


