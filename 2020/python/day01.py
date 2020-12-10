import fileinput
from math import prod
from itertools import combinations

def day01(n):
    with fileinput.input() as file:
        numbers = map(int, file)
        for combination in combinations(numbers, n):
            if sum(combination) == 2020:
                return prod(combination)

print(day01(2))
print(day01(3))
