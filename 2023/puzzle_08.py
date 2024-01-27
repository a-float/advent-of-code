import math
import re

with open("data08.txt", "r") as f:
    tree = {}
    lines = f.read().split("\n")
    turns = lines[0]
    for line in lines[2:]:
        places = re.findall("\w+", line)
        tree[places[0]] = (places[1], places[2])


    curr = list(filter(lambda x: x.endswith("A"), tree.keys()))
    # i = 0
    # while not all([c.endswith("Z") for c in curr]):
    #     turn = 1 if turns[i % len(turns)] == "R" else 0
    #     for j in range(len(curr)):
    #         curr[j] = tree[curr[j]][turn]
    #     if i % 1e5 == 0:
    #         print(f"Visiting {curr}")
    #     i += 1
    sols = []
    for babajaga in curr:
        i = 0
        curr = babajaga
        while not curr.endswith("Z"):
            turn = 1 if turns[i % len(turns)] == "R" else 0
            curr = tree[curr][turn]
            i += 1
        sols.append(i)
    print(f"Part I  = {sols[curr.find('AAA')]}")
    print(f"Part II = {math.lcm(*sols)}")


