from time import time
from collections import namedtuple
import sympy
from typing import List

Stone = namedtuple("Stone", "px py pz vx vy vz")

f = open("data24.txt", "r")
lines = f.read().strip().split("\n")
f.close()

stones: List[Stone] = []
for line in lines:
    pos, vel = line.split("@")
    p = [int(x) for x in pos.strip().split(", ")]
    v = [int(x) for x in vel.strip().split(", ")]
    stones.append(Stone(*p, *v))


def part1(bounds=(7, 27)):
    ok = 0
    t0 = time()
    visited = 0
    for i, stone1 in enumerate(stones):
        for j in range(i + 1, len(stones)):
            stone2 = stones[j]
            p, q = sympy.symbols("p q")
            visited += 1
            if visited % 2000 == 0:
                print(f"Visited {visited} with {ok} oks after {time() - t0}s")
            system = sympy.Matrix(
                [
                    [stone1.vx, -stone2.vx, stone2.px - stone1.px],
                    [stone1.vy, -stone2.vy, stone2.py - stone1.py],
                ]
            )
            res = sympy.linsolve(system, p, q)
            if res:
                t1, t2 = res.args[0]
                if t1 < 0 or t2 < 0:
                    continue
                x = stone1.px + t1 * stone1.vx
                y = stone1.py + t1 * stone1.vy
                if bounds[0] <= x <= bounds[1] and bounds[0] < y < bounds[1]:
                    ok += 1
    return ok


def part2():
    m = 3
    times = sympy.symbols(f"t0:{m}")
    x, y, z, vx, vy, vz = sympy.symbols("x y z vx vy vz")
    system = []
    for i, stone in enumerate(stones[:m]):
        print(i)
        eqs = [
            times[i] * vx + x - times[i] * stone.vx - stone.px,
            times[i] * vy + y - times[i] * stone.vy - stone.py,
            times[i] * vz + z - times[i] * stone.vz - stone.pz,
        ]
        system.extend(eqs)
    for i, s in enumerate(system):
        print(i, s)
    res = sympy.solve(system, [x, y, z, vx, vy, vz, *times], dict=True)[0]
    return res[x] + res[y] + res[z]


t0 = time()
print(f"Part I  = {part1((2e14, 4e14))} in {time() - t0}s")
# print(f"Part I  = {part1()} in {time() - t0}s")

t0 = time()
print(f"Part II = {part2()} in {time() - t0}s")
