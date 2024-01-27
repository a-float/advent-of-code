from typing import Dict, List
import time
from collections import namedtuple, deque

Module = namedtuple("Module", "name type targets")
Signal = namedtuple("Signal", "source target value")


f = open("data20.txt", "r")
lines = f.read().strip()
f.close()

modules: Dict[str, Module] = {}

for line in lines.split("\n"):
    a, b = [x.strip() for x in line.split("->")]
    if a != "broadcaster":
        type = a[0]
        name = a[1:]
    else:
        type = "b"
        name = a
    targets = b.split(", ")
    modules[name] = Module(name, type, targets)


conj = {}  # conjugators' inputs
for cm in [m for m in modules.values() if m.type == "&"]:
    for n in [m for m in modules.values() if cm.name in m.targets]:
        if cm.name not in conj:
            conj[cm.name] = []
        conj[cm.name].append(n.name)

# print(conj)

# for m in modules:
#     print(f"{m} -> {modules[m]}")


def simulate(state, step):
    counts = [0, 0]
    q = deque([Signal("button", "broadcaster", 0)])

    while q:
        s = q.popleft()
        counts[s.value] += 1
        if s.target not in modules:
            continue
        m = modules[s.target]
        if m.type == "b":
            for t in m.targets:
                q.append(Signal(m.name, t, s.value))
        elif m.type == "%":
            if s.value == 0:
                flop_state = state[m.name] if m.name in state else 0
                inv = (flop_state + 1) % 2
                state[m.name] = inv
                for t in m.targets:
                    q.append(Signal(m.name, t, inv))
        elif m.type == "&":
            inputs = conj[m.name]
            mem = state[m.name] if m.name in state else {i: 0 for i in inputs}
            for i in inputs:
                mem[i] = mem[i] if i in mem else 0
            mem[s.source] = s.value
            state[m.name] = mem
            all_tru = 0 if all(list(mem.values())) else 1
            if all_tru == 0 and m.name in ["qm", "pm", "nf", "jd"]:
                print(f"CONJ {m.name} SENDS 0 at step {step}")
            # print("all tru", mem, all_tru)
            for t in m.targets:
                q.append(Signal(m.name, t, all_tru))
    return counts


def part1(presses: int):
    state = {}
    counts = [0, 0]
    for i in range(presses):
        c = simulate(state, i+1)
        counts[0] += c[0]
        counts[1] += c[1]
        # for c in ["qm", "pm", "nf", "jd"]:
            # if all(state[c].values()) and len(state[c]) == len(conj[c]):
                # print(f"CONJ {c} IS ON AFTER {i} steps")
    return counts[0] * counts[1]


t0 = time.time()
print(f"Part I  = {part1(10_0000)} in {time.time() - t0}s")


"""
qm: 2^12 - n * 0000 1010 0101 (165) = 3917
pm: 2^12 - n * 0000 1011 0011 (179) = 3931
nf: 2^12 - n * 0000 1001 1001 (153) = 3943
jd: 2^12 - n * 0000 0010 0111 (39)  = 4057

246313604784977
"""