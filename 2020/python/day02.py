from sys import argv
from re import search
from collections import Counter

def parse(line):
    policyL, policyR, character, password = search('(\d+)-(\d+) (\w): (\w+)', line).groups()
    return (int(policyL), int(policyR), character, password) 

checkA = lambda policyL, policyR, character, password : policyL <= Counter(password)[character] <= policyR

def checkB(policyL, policyR, character, password):
    checkL = password[policyL - 1] == character
    checkR = password[policyR - 1] == character
    return checkL != checkR

def day02(check, path='../input/day02.txt'):
    with open(path) as file:
        data = map(parse, file)
        return sum(map(lambda e : check(*e), data))

if __name__ == '__main__':
    print(day02(checkA))
    print(day02(checkB))

