import fileinput, re
from collections import Counter

pattern = re.compile('^(\d+)-(\d+) (\w): (\w+)\n$')

def parse(line):
    low, high, character, password = pattern.match(line).groups()
    return int(low), int(high), character, password

def policyA(low, high, character, password):
    return low <= password.count(character) <= high

def policyB(low, high, character, password):
    low = password[low - 1] == character
    high = password[high - 1] == character
    return low != high

def day02(check_policy):
    with fileinput.input() as file:
        entries = map(parse, file)
        successes = map(lambda e: check_policy(*e), entries)
        return sum(successes)

print(day02(policyA))
print(day02(policyB))
