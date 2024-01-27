from time import time

f = open("data13.txt", "r")
data = f.read().strip()
f.close()

maps = data.split("\n\n")
maps = [m.split("\n") for m in maps]


def evaluate(m, smudges: int):
    W = len(m[0])
    H = len(m)
    res = 0
    # consider split after column i
    for i in range(W - 1):
        radius = min(i + 1, W - i - 1)
        s = 0
        for j in range(H):
            l = m[j][i - radius + 1 : i + 1]
            r = reversed(m[j][i + 1 : i + radius + 1])
            s += sum((1 if a != b else 0 for a, b in zip(l, r)))
        if s == smudges:
            res += i + 1

    # consider split after column i
    for i in range(H - 1):
        radius = min(i + 1, H - i - 1)
        s = 0
        for j in range(W):
            l = [m[k][j] for k in range(i - radius + 1, i + 1)]
            r = reversed([m[k][j] for k in range(i + 1, i + radius + 1)])
            s += sum((1 if a != b else 0 for a, b in zip(l, r)))
        if s == smudges:
            res += 100 * (i + 1)
    return res


t0 = time()
total = [evaluate(m, 0) for m in maps]
print(f"Part I  = {sum(total)} in {time() - t0:.3f}s")

t0 = time()
total = [evaluate(m, 1) for m in maps]
print(f"Part II = {sum(total)} in {time() - t0:.3f}s")
