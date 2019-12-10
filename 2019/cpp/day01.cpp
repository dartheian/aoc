#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <iterator>

using namespace std;

vector<int> load_masses(const string& filename)
{
    ifstream file {filename};

    if(!file.is_open()) throw runtime_error {"error while opening file"};

    vector<int> masses {istream_iterator<int> {file}, istream_iterator<int> {}};

    if(file.bad()) throw runtime_error {"error while reading file"};

    return masses;
}

int compute_fuel(const int mass)
{
    int fuel {mass / 3 - 2};
    return fuel > 0 ? fuel + compute_fuel(fuel) : 0;
}

int compute_total_fuel(vector<int>& masses)
{
    transform(masses.begin(), masses.end(), masses.begin(), compute_fuel);
    return accumulate(masses.begin(), masses.end(), 0);
}

int main(const int argc, char const * const argv[])
{
    if(argc != 2) throw invalid_argument {"missing input filename"};

    vector<int> masses {load_masses(argv[1])};

    cout << compute_total_fuel(masses) << endl;
}