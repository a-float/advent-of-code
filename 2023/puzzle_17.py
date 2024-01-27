from time import time
from collections import namedtuple
from dataclasses import dataclass
import heapq

Point = namedtuple("Point", "x y")

f = open("data17.txt", "r")
data = f.read().strip()
f.close()

lines = data.split("\n")
W = len(lines[0])
H = len(lines)
board = {}
for y in range(H):
    for x in range(W):
        board[Point(x, y)] = int(lines[y][x])


@dataclass
class Entry:
    dist: int
    curr: Point
    dir: Point
    straight: int

    def __lt__(self, other):
        return self.dist < other.dist


def crusible_bfs(start: Point, end: Point, min_straight, max_straight) -> int:
    heap = [
        Entry(0, start, Point(*d), 0) for d in [(1, 0), (-1, 0), (0, 1), (0, -1)]
    ]
    visited = set([])

    while heap:
        e = heapq.heappop(heap)
        dist, curr, d, straight = (e.dist, e.curr, e.dir, e.straight)
        if curr == end and straight >= min_straight:
            return dist

        nexts = []
        if straight >= min_straight:
            for i in range(-1, 2, 2):
                if d.x == 0:
                    nexts.append((Point(curr.x + i, curr.y), Point(i, 0), 1))
                elif d.y == 0:
                    nexts.append((Point(curr.x, curr.y + i), Point(0, i), 1))

        if straight < max_straight:
            nexts.append((Point(curr.x + d.x, curr.y + d.y), d, straight + 1))

        for n in filter(lambda n: n not in visited and n[0] in board, nexts):
            heat_loss = board[Point(n[0].x, n[0].y)]
            entry = Entry(dist + heat_loss, *n)
            visited.add(n)
            heapq.heappush(heap, entry)
    return float("inf"), []

start = Point(0, 0)
end = Point(W - 1, H - 1)
t0 = time()
print(f"Part I  = { crusible_bfs(start, end, 0, 3)} in {time() - t0}s")
t0 = time()
print(f"Part II = { crusible_bfs(start, end, 4, 10)} in {time() - t0}s")
