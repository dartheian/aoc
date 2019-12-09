#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>
#include <fstream>

using namespace std;

vector<int> read_input(const string& filename)
{
    ifstream file {filename};

    if(!file.is_open()) throw runtime_error {"error while opening file"};

    vector<int> data;
    string token;

    while(getline(file, token, ',')) data.push_back(stoi(token));

    if(file.bad()) throw runtime_error {"error while reading file"};

    return data;
}

void run_intcode_program(vector<int>& program)
{
    auto instructionPointer {program.begin()};
    int  opcode             {*instructionPointer};

    while(opcode != 99)
    {
        int firstParameter  {*++instructionPointer};
        int secondParameter {*++instructionPointer};
        int thirdParameter  {*++instructionPointer};

        switch(opcode)
        {
            case 1:  program[thirdParameter] = program[firstParameter] + program[secondParameter]; break;
            case 2:  program[thirdParameter] = program[firstParameter] * program[secondParameter]; break;
        }

        opcode = *++instructionPointer;
    }
}

bool test_intcode_program(vector<int> program, int noun, int verb)
{
    program[1] = noun;
    program[2] = verb;

    run_intcode_program(program);

    if(program[0] == 19690720) return true;
    else return false;
}

void determine_inputs(vector<int>& program)
{
    for(int noun = 0; noun <= 99; ++noun)
    {
        for(int verb = 0; verb <= 99; ++verb)
        {
            if(test_intcode_program(program, noun, verb)) cout << 100 * noun + verb << endl;
        }
    }
}

int main(const int argc, char const * const argv[])
{
    if(argc != 2) throw invalid_argument {"missing input filename"};

    vector<int> program {read_input(argv[1])};

    determine_inputs(program);
}