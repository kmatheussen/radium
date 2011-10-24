

struct Inputs{
	struct Inputs *next;
	NInt tracknum;
	STime time;

	int type;

	int note;
	int velocity;
};

extern bool Input_Init(int num_elements);
extern void Input_UnInit(void);

#ifndef RADIUM_INPUT_IS_CALLING_NOW
extern RSemaphore *Input_Semaphore;
#endif

