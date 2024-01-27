from enum import Enum


class Dir(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3


f = open("data10.txt", "r")
pipes = f.read().strip()
f.close()

W = len(pipes.split("\n")[0])
H = len(pipes.split("\n"))


def explore(start_pos: int):
    visited = []
    pos = start_pos
    to = None
    while pos not in visited:
        visited.append(pos)
        if to is None:
            if pos % W < W - 1 and pipes[pos + 1] in "7-J":
                pos = pos + 1
                to = Dir.RIGHT
                continue
            if pos % W > 0 and pipes[pos - 1] in "L-F":
                pos = pos - 1
                to = Dir.LEFT
                continue
            if pos // W > 0 and pipes[pos - W - 1] in "|F7":
                pos = pos - W - 1
                to = Dir.UP
                continue
            if pos // W < H - 1 and pipes[pos + W + 1] in "LJ|":
                pos = pos + W + 1
                to = Dir.DOWN
                continue

        ch = pipes[pos]
        if to == Dir.RIGHT:
            if ch == "J":
                pos = pos - W - 1
                to = Dir.UP
            if ch == "-":
                pos = pos + 1
                to = Dir.RIGHT
            if ch == "7":
                pos = pos + W + 1
                to = Dir.DOWN
            continue

        if to == Dir.LEFT:
            if ch == "L":
                pos = pos - W - 1
                to = Dir.UP
            if ch == "-":
                pos = pos - 1
                to = Dir.LEFT
            if ch == "F":
                pos = pos + W + 1
                to = Dir.DOWN
            continue

        if to == Dir.UP:
            if ch == "7":
                pos = pos - 1
                to = Dir.LEFT
            if ch == "|":
                pos = pos - W - 1
                to = Dir.UP
            if ch == "F":
                pos = pos + 1
                to = Dir.RIGHT
            continue

        if to == Dir.DOWN:
            if ch == "L":
                pos = pos + 1
                to = Dir.RIGHT
            if ch == "|":
                pos = pos + W + 1
                to = Dir.DOWN
            if ch == "J":
                pos = pos - 1
                to = Dir.LEFT
    return visited


route = explore(pipes.find("S"))
# print([(r, pipes[r]) for r in route])
print(f"Part I = {len(route) // 2}")


if abs(route[1] - route[-1]) == 2 * W + 2:
    pipes[route[0]] = "|"

route = set(route)
inside = 0
in_wall = False
last_turn = ""
for i, ch in enumerate(pipes):
    if i in route:
        if ch == "|" or (last_turn == "F" and ch == "J") or (last_turn == "L" and ch == "7"):
            in_wall = not in_wall
        if ch not in "|-":
            last_turn = ch
        print(ch, end="")
    elif i in route:
        print(ch, end="")
    elif ch == "\n":
        in_wall = False
        print()
    else:
        if in_wall:
            inside += 1
            print("I", end="")
        else:
            print(".", end="")
print()
print(f"Part II = {inside}")
