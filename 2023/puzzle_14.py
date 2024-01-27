from time import time
from collections import namedtuple

Point = namedtuple("Point", "x y")

f = open("data14.txt", "r")
data = f.read().strip()
f.close()

plane = data.split("\n")

W = len(plane[0])
H = len(plane)

def part1():
    total = 0
    for col in range(W):
        last_square = -1
        circles = 0
        for row in range(H):
            c = plane[row][col]
            if c == "O":
                circles += 1
            if c == "#":
                x = sum((H - last_square - i - 1 for i in range(circles)))
                total += x
                last_square = row
                circles = 0
        x = sum((H - last_square - i - 1 for i in range(circles)))
        total += x
    return total

def get_load(hmap):
    return sum((H - k.y for k, v in hmap.items() if v == "O"))

def push(hmap, diff: Point):
    other = {}
    for k, v in sorted(hmap.items(), key=lambda t: -t[0].x * diff.x + -t[0].y * diff.y):
        if v == "#":
            other[k] = v
            continue
        p = Point(k.x, k.y)
        last_free = p
        if p in other:
            print("bad from the start", hmap[p])
        while p.x >= 0 and p.x < W and p.y >= 0 and p.y < H:
            if p in hmap and hmap[p] == "#":
                # print("Got rock")
                break
            elif p not in other:
                last_free = p
            p = Point(p.x + diff.x, p.y + diff.y)
        if last_free in other:
            print("WHATT", other[last_free])
        other[last_free] = "O"
    return other


# t0 = time()
# print(f"Part I  = {part1()} in {1000*(time() - t0):.3f}ms")


hmap = {}
for i in range(H):
    for j in range(W):
        if plane[i][j] in ["#", "O"]:
            hmap[Point(j, i)] = plane[i][j]


cache = {}
def cycle(inp):
    cache_key = to_str(inp)
    if cache_key in cache:
        return cache[cache_key]
    x = push(inp, Point(0, -1))
    x = push(x, Point(-1, 0))
    x = push(x, Point(0, 1))
    x = push(x, Point(1, 0))
    cache[cache_key] = x
    return x

def to_str(hmap):
    out = ""
    for y in range(H):
        for x in range(W):
            p = Point(x, y)
            out += hmap[p] if p in hmap else "."
        out += "\n"
    return out

x = hmap
for i in range(10000):
    l = len(x)
    x = cycle(x)
    print(f"{i+1}. {get_load(x)}")


print("PROTOTYPE", get_load(push(hmap, Point(0, -1))))
# t0 = time()
# print(f"Part II = {sum(total)} in {time() - t0:.3f}s")

# [11, 6, 3, 9, 8 ,1]
# 457 - 11
# 463 - 11

# 105209 too high

# not 90598