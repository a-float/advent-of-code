from time import time
from collections import namedtuple

Lens = namedtuple("Lens", "label f")

f = open("data15.txt", "r")
data = f.read().strip()
f.close()

def hash(s):
    total = 0
    for char in s:
        total += ord(char)
        total = (total * 17) % 256
    return total

def get_focus_power(hmap):
    total = 0
    for i in range(256):
        if i in hmap:
            box = hmap[i]
            for j, lens in enumerate(box):
                total += (i + 1) * (j + 1) * lens.f
    return total

ops = data.split(",")
print(f"Part I  = {sum(hash(o) for o in ops)}")

hmap = {}
for op in ops:
    if "-" in op:
        label = op[:-1]
        h = hash(label)
        # print(f"Removing {label} from {h}")
        if h in hmap:
            box = hmap[h]
            hmap[h] = list(filter(lambda f: f.label != label, box))
    else:
        label, focus = op.split("=")
        focus = int(focus)
        h = hash(label)
        # print(f"Adding {label} {focus} to {h}")
        if h not in hmap:
            hmap[h] = []
        for i, lens in enumerate(hmap[h]):
            if lens.label == label:
                hmap[h][i] = Lens(label, focus)
                break
        else:
            hmap[h].append(Lens(label, focus))
    # print(f"After {op:10} {hmap}")
print(f"Part II = {get_focus_power(hmap)}")
