#include <QList>


#include "../common/nsmtracker.h"
#include "../common/SeqAutomation.hpp"

struct Root *root = NULL;


// BUILDTYPE=DEBUG ./build_linux.sh test

// (g++ test_seqautomation.cpp -DTEST_MAIN -Wall -std=gnu++11 -g `pkg-config --libs --cflags QtGui` -Wunknown-pragmas && ./a.out)


#include <stdio.h>
#include <assert.h>


namespace test{
struct Node{
  double time; // seqtime format
  double value;
  int logtype;
};
}
  
static test::Node create_node(double time, double value = 2.91){
  test::Node node = {
    .time = time,
    .value = value,
    .logtype = LOGTYPE_LINEAR
  };
  
  return node;
}
  

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}

#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
bool THREADING_has_player_thread_priority(void){
  return false;
}
#endif
#endif


bool THREADING_is_runner_thread(void){
  return false;
}


static void test_get_value(void){
  radium::SeqAutomation<test::Node> nodes;
  
  nodes.add_node(create_node(10,10)); // 0
  nodes.add_node(create_node(20,20)); // 1
  nodes.add_node(create_node(30,30)); // 2
  nodes.add_node(create_node(40,40)); // 3
  nodes.add_node(create_node(40,50)); // 4
  nodes.add_node(create_node(40,60)); // 5
  nodes.add_node(create_node(40,70)); // 6
  nodes.add_node(create_node(40,80)); // 7
  nodes.add_node(create_node(40,90)); // 8
  nodes.add_node(create_node(40,100)); // 9
  nodes.add_node(create_node(40,110)); // 10
  nodes.add_node(create_node(50,120)); // 11
  nodes.add_node(create_node(60,130)); // 12

  bool random_cache;
  
  auto test_value = [&nodes, &random_cache](double time, double expect){

    if (random_cache)
      nodes._RT_last_search_pos = 1 + rand() % nodes.size();
    
    double ret;
    radium::SeqAutomationReturnType rettype = nodes.RT_get_value(time, ret, true);
    
    if (rettype==radium::SeqAutomationReturnType::NO_VALUES)
      abort();
    else if (rettype==radium::SeqAutomationReturnType::NO_VALUES_YET)
      ret = -2;
    else if (rettype==radium::SeqAutomationReturnType::NO_MORE_VALUES){
      if (time > nodes.at(nodes.size()-1).time)
        ret = -1;
    }

    if (!random_cache)
      printf("...test_value. time: %f. expect: %f. ret: %f.\n", time, expect, ret);
    
    if (!equal_doubles(ret, expect))
      abort();
  };

  auto testit = [&test_value](void){
    test_value(9,-2);
    
    test_value(10,10);
    test_value(11,11);
    test_value(15,15);
    test_value(19,19);
    
    test_value(20,20);
    test_value(21,21);
    test_value(25,25);
    test_value(29,29);
    
    test_value(30,30);
    test_value(39,39);
    
    test_value(40,110);
    
    test_value(41,111);
    test_value(50,120);
    test_value(59,129);
    test_value(60,130);
    
    test_value(61,-1);
  };

  random_cache = false;
  testit();

  random_cache = true;
  for(int i=0;i<1000;i++)
    testit();
}

int main(){
  
  test_get_value();
  
  radium::SeqAutomation<test::Node> nodes;

#define ADD(time) nodes.add_node(create_node(time));
  
#define TEST(Time, Type, Has1, Has2) {                                  \
    radium::SeqAutomation<test::Node>::ScopedRtAccess rt_access(nodes); \
    assert(nodes.RT_get_nodes(Time, rt_access)==radium::SeqAutomationReturnType::Type); \
    if(Has1) assert(rt_access.node1!=NULL); else assert(rt_access.node1==NULL); \
    if(Has2) assert(rt_access.node2!=NULL); else assert(rt_access.node2==NULL); \
  }
  
  TEST(0, NO_VALUES, false, false);
  TEST(10, NO_VALUES, false, false);
  
  {
    ADD(0);
    TEST(0, NO_MORE_VALUES, true, false);
    TEST(10, NO_MORE_VALUES, true, false);
  }

  {
    nodes.delete_node(0);

    TEST(0, NO_VALUES, false, false);
    TEST(10, NO_VALUES, false, false);
  }

  {
    ADD(1);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(1, NO_MORE_VALUES, true, false);
    TEST(2, NO_MORE_VALUES, true, false);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(2, NO_MORE_VALUES, true, false);
    TEST(1, NO_MORE_VALUES, true, false);
    TEST(0, NO_VALUES_YET, false, true);
  }
  
  {
    ADD(3);
    TEST(4, NO_MORE_VALUES, true, false);
    TEST(2, VALUE_OK, true, true);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(1, VALUE_OK, true, true);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(3, NO_MORE_VALUES, true, false);
    TEST(4, NO_MORE_VALUES, true, false);
    TEST(1, VALUE_OK, true, true);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(2, VALUE_OK, true, true);
    TEST(1, VALUE_OK, true, true);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(2, VALUE_OK, true, true);
    TEST(0, NO_VALUES_YET, false, true);
    TEST(4, NO_MORE_VALUES, true, false);
    TEST(3, NO_MORE_VALUES, true, false);
  }
  
  printf("Size: %d.\n", nodes.size());
  
  return 0;
}
  
