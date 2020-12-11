import fileinput

hits = 0

with fileinput.input() as file:
    for latitude, line in enumerate(file):
        for longitude, character in enumerate(line):
            toboggan_longitude = 3 * latitude % len(line)
            if character == '#' and longitude == toboggan_longitude:
                hits += 1

print(hits)