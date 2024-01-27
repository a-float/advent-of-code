from collections import namedtuple

Point = namedtuple("Point", "x y")

f = open("data11.txt", "r")
data = f.read()
f.close()

galaxies = []
lines = data.split("\n")
H = len(lines)
W = len(lines[0])
print(W, H)

empty_rows = [i for i in range(H) if all([d == "." for d in lines[i]])]
empty_cols = [i for i in range(W) if all([d == "." for d in [l[i] for l in lines]])]

print(empty_rows)
print(empty_cols)

for i, d in enumerate(data.replace("\n", "")):
    if d == "#":
        galaxies.append(Point(i % W, i // W))

# print(galaxies)

def get_dist(a: Point, b: Point, mult: int):
    dx = 0
    dy = 0
    for i in range(min(a.x, b.x), max(a.x, b.x)):
        if i in empty_cols:
            dx += mult
        else:
            dx += 1

    for i in range(min(a.y, b.y), max(a.y, b.y)):
        if i in empty_rows:
            dy += mult
        else:
            dy += 1

    # print("dx=", dx, "dy=", dy)
    return dx + dy


dists1 = []
dists2 = []
for i, g1 in enumerate(galaxies):
    for j, g2 in enumerate(galaxies):
        if j <= i:
            continue
        # d = get_dist(g1, g2)
        # print(i + 1, j + 1, "d=", d)
        dists1.append(get_dist(g1, g2, 2))
        dists2.append(get_dist(g1, g2, 1000000))

# print(dists)
print(len(dists1))
print(f"Part I  = {sum(dists1)}")
print(f"Part II = {sum(dists2)}")
# print(galaxies[0], galaxies[6])
