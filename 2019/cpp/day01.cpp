#include <iostream>
#include <stdexcept>
#include <fstream>
#include <numeric>
#include <iterator>

using namespace std;

int compute_fuel(const int mass)
{
    int fuel{ mass / 3 - 2 };
    return fuel > 0 ? fuel + compute_fuel(fuel) : 0;
}

int main(const int argc, char const* const argv[])
{
    if (argc != 2) throw invalid_argument{ "missing input filename" };

    ifstream file{ argv[1] };

    if (!file.is_open()) throw runtime_error{ "error while opening file" };

    int fuel = transform_reduce(istream_iterator<int> {file}, istream_iterator<int> {}, 0, plus<>(), compute_fuel);

    if (file.bad()) throw runtime_error{ "error while reading file" };

    cout << fuel << endl;
}