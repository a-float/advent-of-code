from typing import Dict, List
import time
from collections import namedtuple
from dataclasses import dataclass
import re

Part = namedtuple("Part", "x m a s")
Condition = namedtuple("Condition", "trait cmp val next")


@dataclass
class Flow:
    default: str
    conditions: List[Condition]


f = open("data19.txt", "r")
lines = f.read().strip()
f.close()

_flows, parts = [l.split("\n") for l in lines.split("\n\n")]

parts = [Part(*[int(x) for x in re.findall("(\d+)", p)]) for p in parts]

flows = {}
for f in _flows:
    name, rest = f[:-1].split("{")
    steps = rest.split(",")
    rest, default = steps[:-1], steps[-1]
    conditions = [re.findall("(\w+)([\<\>])(\d+):(\w+)", r)[0] for r in rest]
    conditions = [Condition(*cond[:2], int(cond[2]), cond[3]) for cond in conditions]
    flows[name] = Flow(default, conditions)


def check_condition(value: int, cond: Condition) -> bool:
    return (
        cond.cmp == ">"
        and value > int(cond.val)
        or cond.cmp == "<"
        and value < int(cond.val)
    )


def evaluate_part(part: Part, flows: Dict[str, Flow]) -> bool:
    flow_name = "in"
    while flow_name not in "AR":
        flow = flows[flow_name]
        for cond in flow.conditions:
            trait = part.__getattribute__(cond.trait)
            if check_condition(trait, cond):
                flow_name = cond.next
                break
        else:
            flow_name = flow.default
    return flow_name == "A"


def part1():
    total = 0
    for part in parts:
        if evaluate_part(part, flows):
            total += part.x + part.m + part.a + part.s
    return total


t0 = time.time()
print(f"Part I  = {part1()} in {time.time() - t0}s")


def product(lst: List[int]):
    total = 1
    for l in lst:
        total *= l
    return total


def part2(flow_name: str, flows: Dict[str, Flow], domain=None):
    if domain is None:
        d = set(range(1, 4001))
        domain = {"x": d, "m": d.copy(), "a": d.copy(), "s": d.copy()}
    if flow_name == "R":
        return 0
    if flow_name == "A":
        return product([len(s) for s in domain.values()])
    total, flow = 0, flows[flow_name]
    for cond in flow.conditions:
        valid = domain.copy()
        valid[cond.trait] = set([])
        invalids = set([])
        for x in domain[cond.trait]:
            if check_condition(x, cond):
                valid[cond.trait].add(x)
            else:
                invalids.add(x)
        domain[cond.trait] = invalids
        total += part2(cond.next, flows, valid)
    total += part2(flow.default, flows, domain)
    return total


t0 = time.time()
print(f"Part II = {part2('in', flows)} in {time.time() - t0}s")
