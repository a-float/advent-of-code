from time import time
from collections import namedtuple

Point = namedtuple("Point", "x y")

f = open("data21.txt", "r")
data = f.read().strip()
f.close()

lines = data.split("\n")
W = len(lines[0])
H = len(lines)
garden = {
    Point(x, y): c for y, l in enumerate(lines) for x, c in enumerate(l) if c != "#"
}
# print(garden)

def part1(garden, steps, loop=False):
    start = next(k for (k, v) in garden.items() if v == "S")
    current = set([start])
    dists = {start: 0}
    dirs = (Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
    for i in range(steps):
        new_current = set([])
        for p in current:
            for new in [Point(p[0] + d[0], p[1] + d[1]) for d in dirs]:
                m_new = Point(new[0] % W, new[1] % H) if loop else new
                if m_new not in garden or new in dists:
                    continue
                new_current.add(new)
                dists[new] = (dists[p] + 1) % 2
        current = new_current
        if len(current) == 0:
            break
        q = (i + 1) / W
        v = (i + 1) % W
        # for part 2
        if i % 2 == 1 and v == 26501365 % W:
            x = len([d for d, v in dists.items() if v % 2 != i % 2])
            print(f"We {q}, {v} get {x}")
    return len([d for d, v in dists.items() if v % 2 == steps % 2])

def part2(steps):
    x = steps / W
    # 2nd deree polynomial fitting
    return int(x**2 * 15615 + x * 207.198473284 - 6.186644139233)

t0 = time()
print(f"Part I  = { part1(garden, 64)} in {time() - t0}s")
t0 = time()
print(f"Part II = { part2(26501365)} in {time() - t0}s")
