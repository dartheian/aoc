#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define BUFFER_SIZE 1000

int program[BUFFER_SIZE];
int runtime[BUFFER_SIZE];

bool get_intcode(FILE* file, int* token)
{
    return fscanf(file, "%d,", token) != EOF ? true : false;
}

void load_program(FILE* file, int* buffer)
{
    while(get_intcode(file, buffer++));
}

void program_copy(int* destination, int* source)
{
    memcpy(destination, source, BUFFER_SIZE);
}

void run_program(int* program)
{
    int* instruction_pointer = program;
    int  opcode = *instruction_pointer;

    while(opcode != 99)
    {
        int firstArgument  = *++instruction_pointer;
        int secondArgument = *++instruction_pointer;
        int thirdArgument  = *++instruction_pointer;

        switch(opcode)
        {
            case 1: program[thirdArgument] = program[firstArgument] + program[secondArgument]; break;
            case 2: program[thirdArgument] = program[firstArgument] * program[secondArgument]; break;
        }

        opcode = *++instruction_pointer;
    }
}

bool test_input(int* program, int noun, int verb)
{
    program_copy(runtime, program);

    runtime[1] = noun;
    runtime[2] = verb;

    run_program(runtime);

    return runtime[0] == 19690720 ? true : false;
}

int main(const int argc, const char* const argv[])
{
    if(argc != 2) exit(EXIT_FAILURE);

    FILE* file = fopen(argv[1], "r");

    load_program(file, program);

    fclose(file);

    for(int noun = 0; noun < 100; ++noun)
    {
        for(int verb = 0; verb < 100; ++verb)
        {
            if(test_input(program, noun, verb)) printf("%d\n", 100 * noun + verb);
        }
    }
}