from time import time
from collections import namedtuple
from dataclasses import dataclass
import heapq
from typing import Set

Point = namedtuple("Point", "x y")

f = open("data23.txt", "r")
data = f.read().strip()
f.close()

lines = data.split("\n")
W = len(lines[0])
H = len(lines)
garden = {
    Point(x, y): c for y, l in enumerate(lines) for x, c in enumerate(l) if c != "#"
}


def find_all_routes(garden, can_climb_slopes):
    start = Point(1, 0)
    end = Point(W - 2, H - 1)
    stack = [(start, set([]))]
    routes = []
    max_len = 0
    while stack:
        pos, visited = stack.pop()
        if pos == end:
            if len(visited) > max_len:
                max_len = len(visited)
                routes.append(visited)
                print(f"Found route of length {len(visited)}")
        if len(visited) > 7000:
            return -1
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            new_pos = (pos[0] + dx, pos[1] + dy)
            if new_pos in visited or new_pos not in garden:
                continue
            if not can_climb_slopes:
                ch = garden[new_pos]
                if ch == ">" and dx != 1:
                    continue
                if ch == "<" and dx != -1:
                    continue
                if ch == "^" and dy != -1:
                    continue
                if ch == "v" and dy != 1:
                    continue
            stack.append((new_pos, visited | set([pos])))
    return routes


@dataclass
class Entry:
    pos: Point
    dist: int
    visited: Set[str]

    def __lt__(self, other):
        return self.dist > other.dist


@dataclass
class DistEntry:
    pos: Point
    dist: int

    def __lt__(self, other):
        return self.dist < other.dist


best = -1
start = Point(1, 0)
end = Point(W - 2, H - 1)


def bfs_to_crossroards(a):
    res = []
    h = [DistEntry(a, 0)]
    visited = set([])
    while h:
        e = heapq.heappop(h)
        if e.pos in visited:
            continue
        visited.add(e.pos)
        dist, pos = e.dist, e.pos
        nexts = []
        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            new_pos = (pos[0] + dx, pos[1] + dy)
            if new_pos not in visited and new_pos in garden:
                nexts.append(new_pos)
        # print(len(nexts), pos)
        if len(nexts) == 1 or pos == a:
            for n in nexts:
                heapq.heappush(h, DistEntry(Point(*n), dist + 1))
        elif pos != a:
            res.append((dist, pos))
        if (pos == start or pos == end) and dist > 0:
            res.append((dist, pos))
    return res


def condensed():
    c = {}
    stack = [start]
    while stack:
        z = stack.pop()
        cr = bfs_to_crossroards(z)
        c[z] = cr
        for k in [x[1] for x in cr]:
            if k not in c:
                stack.append(k)
    for k, v in c.items():
        print(k, v)
    return c


t0 = time()
c = condensed()
print(f"generated condensed in {time() - t0}s")


def part2():
    start = Point(1, 0)
    end = Point(W - 2, H - 1)
    heap = [Entry(start, 0, set([]))]
    max_len = 0
    t0 = time()
    while heap:
        entry = heapq.heappop(heap)
        pos, visited = entry.pos, entry.visited
        if pos == end:
            # print(f"{len(visited)} at {time() - t0:.2f}s")
            if entry.dist > max_len:
                max_len = entry.dist
                print(f"Found route of length {max_len} after {time() - t0}s")
            continue
        for d, new_pos in c[pos]:
            if new_pos not in visited:
                heapq.heappush(heap, Entry(new_pos, d + entry.dist, visited | set([pos])))
    return max_len


t0 = time()
print(f"Part II = {part2()} in {time() - t0}s")
