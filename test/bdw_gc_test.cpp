#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>

struct Storage
{
	std::vector<void *> allocated;
};

static Storage *g_storage;


static void allocatedfinalizer(void *actual_mem_start, void *user_data)
{
	printf("Finalizing allocated %p\n", actual_mem_start);

	int i = 0;
	for(void *allocated : g_storage->allocated)
	{
		if (allocated == actual_mem_start)
		{
			fprintf(stderr, "\n"
					"Oh no. :(.\n"
					"Allocation #%d should not be finalized now since there should be a pointer to it on the stack.\n\n", i);
			abort();
		}
		i++;
	}
}

extern "C"
{
	extern void __asan_print_accumulated_stats();
	extern void *__asan_get_current_fake_stack();
	extern void *__asan_addr_is_in_fake_stack(void *fake_stack, void *addr,
											  void **beg, void **end);
}

/*
static void afunc(void **allocated)
{
}
*/

void f3()
{
	void *array_f3[10];
	void *ret = __asan_addr_is_in_fake_stack(__asan_get_current_fake_stack(), &array_f3[0], &array_f3[1], &array_f3[2]);
	(void)ret;
	printf("f3. Address of          array: %p. Frame-Start: %p. Frame-End: %p\n", &array_f3[0], array_f3[1], array_f3[2]);
}

void f2()
{
	int a,b,c;

	void *pos1; void *pos2;
	
	void *ret = __asan_addr_is_in_fake_stack(__asan_get_current_fake_stack(), &a, &pos1, &pos2);
	(void)ret;
	printf("f2. Address of non-array vars: %p. Frame-Start: %p. Frame-End: %p\n", &a, pos1, pos2);
	
	f3();
}

void f1() {
	void *array_f2[10];

	void *ret = __asan_addr_is_in_fake_stack(__asan_get_current_fake_stack(), &array_f2[0], &array_f2[1], &array_f2[2]);
	(void)ret;
	printf("f1. Address of          array: %p. Frame-Start: %p. Frame-End: %p\n", &array_f2[0], array_f2[1], array_f2[2]);

	f2();
}

int main(int argc, char **argv){
	f1();
	return 0;
	g_storage = new Storage;

	char largetingonstack[40] = {};
	void *allocated[10];

	//afunc(allocated);
		for(int i = 0 ; i < 10 ; i++)
	{
		allocated[i] = GC_malloc(50);
		printf("%d: Allocated %p\n", i, allocated[i]);
		g_storage->allocated.push_back(allocated[i]);
		GC_register_finalizer(allocated[i], allocatedfinalizer, NULL, NULL, NULL);
	}
	
	__asan_print_accumulated_stats();

	printf("Fake stack: %p\n", __asan_get_current_fake_stack());

	void *beg, *end;
	void *ret = __asan_addr_is_in_fake_stack(__asan_get_current_fake_stack(), &allocated[2], &beg, &end);

	printf("In Fake stack. Ret: %p. Beg: %p. End: %p. &allocated[2]: %p\n", ret, beg, end, &allocated[2]);
		
	GC_gcollect();

	for(int i = 0 ; i < 10 ; i++)
		printf("Survived allocation: %p. %d\n", allocated[i], largetingonstack[999]);
	
	return 0;
}
