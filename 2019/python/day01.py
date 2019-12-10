from sys import argv

def compute_fuel(mass):

    fuel = mass // 3 - 2
    return fuel + compute_fuel(fuel) if fuel > 0 else 0


with open(argv[1]) as file:

    masses = map(int, file)
    fuel   = sum(map(compute_fuel, masses))

print(fuel)