from typing import Dict, List
import time
from collections import namedtuple, deque
from dataclasses import dataclass

Vector3 = namedtuple("Vector3", "x y z")

@dataclass
class Block:
    id: int
    start: Vector3
    end: Vector3

f = open("data22.txt", "r")
lines = f.read().strip().split("\n")
f.close()

def split_block(b):
    s = []
    for x in range(b.start.x, b.end.x + 1):
        for y in range(b.start.y, b.end.y + 1):
            for z in range(b.start.z, b.end.z + 1):
                s.append(Vector3(x, y, z))
    return s

blocks = []
for i, line in enumerate(lines):
    a, b = line.split("~")
    start = Vector3(*(int(x) for x in a.split(",")))
    end = Vector3(*(int(x) for x in b.split(",")))
    blocks.append(Block(i, start, end))

def drop_blocks(blocks):
    distinct_dropped = set([])
    new_blocks = blocks[:]
    # print("START", new_blocks)
    busy = {}
    for i, splits in enumerate([split_block(o) for o in new_blocks]):
        for s in splits:
            busy[s] = i
    for i in range(len(blocks)):
        can_drop = True
        # print("at ", i)
        while can_drop:
            b = new_blocks[i]
            s0 = split_block(b)
            lowered = Block(
                b.id, 
                Vector3(b.start.x, b.start.y, b.start.z - 1),
                Vector3(b.end.x, b.end.y, b.end.z - 1))
            s1 = split_block(lowered)
            # print(f"Dropping {i} to {lowered.start.z}")
            if lowered.start.z < 0 or lowered.end.z < 0:
                can_drop = False
                continue
            for s in s1:
                if s in busy:
                    k = busy[s]
                    if k != i:
                        can_drop = False
                        break
            # if len(s1 + s2) != len(set(s1 + s2)):
                # no collision
                # can_drop = False
            if can_drop:
                new_blocks[i] = lowered 
                for s in s0:
                    del busy[s]
                for s in s1:
                    busy[s] = i
                distinct_dropped.add(i)
    return new_blocks, distinct_dropped

def part1():
    blcks = sorted(blocks[:], key=lambda b: b.start.z)
    while True:
        blcks, dropped = drop_blocks(blcks)
        if len(dropped) == 0:
            break
    # for b in blcks:
        # print(b)
    xs = []
    for i in range(len(blcks)):
        bs = blcks[:]
        bs.pop(i)
        _, dropped = drop_blocks(bs)
        xs.append(len(dropped))
    print(xs)
    return xs

t0 = time.time()
removables = part1()
print(f"Part I  = {len([x for x in removables if x == 0])} in {time.time() - t0}s")
print(f"Part II = {sum(removables)} in {time.time() - t0}s")


# expanded = []
# for b in blocks:
#     for x in range(b.start.x, b.end.x + 1):
#         for y in range(b.start.y, b.end.y + 1):
#             for z in range(b.start.z, b.end.z + 1):
#                 p = (b.id, Vector3(x, y, z))
#                 expanded.append(p)

# print(expanded)
# min_x = min(b.start.x for b in blocks)
# max_x = max(b.end.x for b in blocks)
# min_y = max(b.start.y for b in blocks)
# max_y = max(b.end.y for b in blocks)

# print(min_x, max_x, min_y, max_y)

# b1 = Block(1, Vector3(0,0,0), Vector3(2,0,0))
# b2 = Block(2, Vector3(0,0,0), Vector3(0,2,0))
# print(set(split_block(b1)), set(split_block(b2)))

# print(set(split_block(b1) + split_block(b2)))