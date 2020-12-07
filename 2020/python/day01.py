from sys import argv
from math import prod
from itertools import combinations


def day01(path='../input/day01.txt', n=3, m=2020):
    with open(path) as file:
        numbers = map(int, file)
        return (prod(c) for c in combinations(numbers, n) if sum(c) == m)


if __name__ == '__main__':
    print(next(day01(n=2)))
    print(next(day01(n=3)))

