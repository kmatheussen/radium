


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
  
static test::Node create_node(double time){
  test::Node node = {
    .time = time,
    .value = 2.91,
    .logtype = LOGTYPE_LINEAR
  };
  
  return node;
}
  

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  abort();
}


int main(){
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
  
