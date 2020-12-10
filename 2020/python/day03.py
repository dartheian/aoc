import fileinput

hits = 0

with fileinput.input() as file:
    for latitude, line in enumerate(file):
        for longitude, character in enumerate(line):
            if character == '#' and latitude % 1 == 0 and longitude == 3 * latitude % len(line):
                hits += 1

print(hits)