#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

bool get_mass(FILE* file, int* mass)
{
    return fscanf(file, "%d", mass) != EOF ? true : false;
}

int compute_fuel(const int mass)
{
    int fuel = mass / 3 - 2;
    return fuel > 0 ? fuel + compute_fuel(fuel) : 0;
}

int main(const int argc, const char* const argv[])
{
    if(argc != 2) exit(EXIT_FAILURE);

    FILE* file = fopen(argv[1], "r");

    int mass, fuel = 0;

    while(get_mass(file, &mass)) fuel += compute_fuel(mass);

    fclose(file);

    printf("%d\n", fuel);
}