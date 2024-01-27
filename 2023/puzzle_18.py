import time
import re

f = open("data18.txt", "r")
lines = f.read().strip().split("\n")
f.close()

dirs = {
    "U": (0, -1),
    "R": (1, 0),
    "D": (0, 1),
    "L": (-1, 0)
}

def part1():
    pos = (0, 0)
    route = {pos: "?"}
    for line in lines:
        d, l, color = line.split()
        for i in range(int(l)):
            pos = (pos[0] + dirs[d][0], pos[1] + dirs[d][1])
            route[pos] = color

    xs = [k[0] for k in route]
    ys = [k[1] for k in route]
    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    # naive bfs
    stack = [(min_x - 1, min_y - 1)]
    visited = set([])
    while stack:
        pos = stack.pop()
        if pos in visited or pos in route:
            continue
        if min_x - 1 <= pos[0] <= max_x + 1 and min_y - 1 <= pos[1] <= max_y + 1:
            visited.add(pos)
            for n in [(pos[0] + d[0], pos[1] + d[1]) for d in dirs.values()]:
                stack.append(n)
    return (max_x - min_x + 3) * (max_y - min_y + 3) - len(visited)


def part2():
    route = [(0, 0)]
    keys = ["R", "D", "L", "U"]
    border = 0
    for line in lines:
        pos = route[-1]
        color = line.split()[-1][1:-1]
        l = int(color[1:-1], base=16)
        d = keys[int(color[-1])]
        border += l
        end = (pos[0] + dirs[d][0] * l, pos[1] + dirs[d][1] * l)
        route.append(end)
    route.append(route[1])
    
    # smart shoelace formula
    A = 0
    for i in range(1, len(lines)+1):
        A += 0.5 * (route[i][1] + route[i+1][1]) * (route[i][0] - route[i+1][0])

    # pick's theorem
    inner = A - border / 2 + 1
    return int(inner + border)


t0 = time.time()
print(f"Part I  = {part1()} in {time.time() - t0}s")

t0 = time.time()
print(f"Part II = {part2()} in {time.time() - t0}")