from sys       import argv
from itertools import tee, islice, zip_longest, product

def instructions(program):

    opcode, firstParameter, secondParameter, thirdParameter = tee(program, 4)

    opcode          = islice(opcode,          0, None, 4)
    firstParameter  = islice(firstParameter,  1, None, 4)
    secondParameter = islice(secondParameter, 2, None, 4)
    thirdParameter  = islice(thirdParameter,  3, None, 4)

    return zip_longest(opcode, firstParameter, secondParameter, thirdParameter)


def run_intcode_program(program):

    for opcode, firstParameter, secondParameter, thirdParameter in instructions(program):

        if   opcode ==  1 : program[thirdParameter] = program[firstParameter] + program[secondParameter]
        elif opcode ==  2 : program[thirdParameter] = program[firstParameter] * program[secondParameter]
        elif opcode == 99 : return


def test_intcode_input(program, noun, verb):

    program[1:3] = [noun, verb]

    run_intcode_program(program)

    return True if(program[0]) == 19690720 else False


with open(argv[1]) as file:

    program = list(map(int, file.readline().split(',')))

for noun, verb in product(range(100), repeat=2):

    if test_intcode_input(program.copy(), noun, verb) : print(100 * noun + verb)