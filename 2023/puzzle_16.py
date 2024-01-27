from time import time
from collections import namedtuple

Point = namedtuple("Point", "x y")
Beam = namedtuple("Beam", "x y d")

f = open("data16.txt", "r")
data = f.read().strip()
f.close()

lines = data.split("\n")
W = len(lines[0])
H = len(lines)


def rotate_bream(direction, mirror):
    verticals = [(0, -1), (0, 1)]
    horizontals = [(-1, 0), (1, 0)]
    if direction == (1, 0):  # right
        if mirror == "/":
            return [(0, -1)]
        if mirror == "\\":
            return [(0, 1)]
        if mirror == "|":
            return verticals
    if direction == (-1, 0):  # left
        if mirror == "/":
            return [(0, 1)]
        if mirror == "\\":
            return [(0, -1)]
        if mirror == "|":
            return verticals
    if direction == (0, 1):  # down
        if mirror == "/":
            return [(-1, 0)]
        if mirror == "\\":
            return [(1, 0)]
        if mirror == "-":
            return horizontals
    if direction == (0, -1):  # up
        if mirror == "/":
            return [(1, 0)]
        if mirror == "\\":
            return [(-1, 0)]
        if mirror == "-":
            return horizontals
    return []


def move_beam(beam: Beam, mirrors):
    # print("Moving", beam)
    next_p = Point(beam.x + beam.d[0], beam.y + beam.d[1])
    if next_p.x >= W or next_p.x < 0 or next_p.y < 0 or next_p.y >= H:
        return []
    moved = Beam(next_p.x, next_p.y, beam.d)
    if next_p not in mirrors:
        return [moved]
    m = mirrors[next_p]
    if beam.d[0] != 0 and m == "-" or beam.d[1] != 0 and m == "|":
        return [moved]
    next_dirs = rotate_bream(beam.d, m)
    return [Beam(next_p.x, next_p.y, d) for d in next_dirs]


mirrors = {}
for i in range(H):
    for j in range(W):
        if lines[i][j] != ".":
            p = Point(j, i)
            mirrors[p] = lines[i][j]

def get_energized_count(initial_beam):
    beams = [initial_beam]
    beam_cache = set([])
    energized = set([])
    while len(beams) != 0:
        new_beams = []
        # print(len(beams))
        # print(len(energized))
        for beam in beams:
            moved = move_beam(beam, mirrors)
            new_beams.extend((b for b in moved if b not in beam_cache))
        beam_cache.update(new_beams)
        beams = new_beams
        energized.update([Point(x, y) for (x, y, d) in beams])
    return len(energized)


t0 = time()
print(f"Part I  = {get_energized_count(Beam(-1, 0, (1, 0)))} in {time() - t0}s")

t0 = time()
starters = []
for i in range(W):
    starters.append(Beam(i, -1, (0, 1)))
    starters.append(Beam(i, H, (0, -1)))

for i in range(H):
    starters.append(Beam(-1, i, (1, 0)))
    starters.append(Beam(W, i, (-1, 0)))

max_energy = max((get_energized_count(start) for start in starters))
print(f"Part II = {max_energy} in {time() - t0}s")
