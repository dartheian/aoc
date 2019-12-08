#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>
#include <fstream>
#include <iterator>

using namespace std;

vector<int> read_input(const string& filename, const char delimiter)
{
    ifstream file(filename);

    if(!file.is_open()) throw runtime_error("error while opening file");

    vector<int> data;
    string token;

    while(getline(file, token, delimiter)) data.push_back(stoi(token));

    if(file.bad()) throw runtime_error("error while reading file");

    return data;
}

template <typename T> ostream& operator<< (ostream& out, const vector<T>& v)
{
    if(v.empty()) throw runtime_error("empty vector");

    copy(v.begin(), --v.end(), ostream_iterator<T>(out, ","));
    copy(--v.end(), v.end(), ostream_iterator<T>(out));

    return out;
}

vector<int> run_intcode_program(vector<int>& program)
{
    for(size_t i = 0; i < program.size(); i += 4)
    {
        int firstIndex  = program[i+1];
        int secondIndex = program[i+2];
        int resultIndex = program[i+3];

        switch (program[i])
        {
            case 1:

                program[resultIndex] = program[firstIndex] + program[secondIndex];
                break;

            case 2:

                program[resultIndex] = program[firstIndex] * program[secondIndex];
                break;

            case 99:

                return program;
                break;

            default:

                throw runtime_error("invalid intcode opcode");
        }
    }

    return program;
}

int main(const int argc, char const * const argv[])
{
    if(argc != 2) throw invalid_argument("missing argument");

    vector<int> program(read_input(argv[1], ','));

    for(int i = 0; i < 99; ++i)
    {
        for(int j = 0; j < 99; ++j)
        {
            vector<int> programCopy(program);
            programCopy[1] = i;
            programCopy[2] = j;
            run_intcode_program(programCopy);
            if(programCopy[0] == 19690720) cout << 100 * i + j << endl;
        }
    }
}