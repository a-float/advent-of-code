from time import time
import networkx as nx
import matplotlib.pyplot as plt

f = open("data25.txt", "r")
lines = f.read().strip().split("\n")
f.close()

G = nx.Graph()
for l in lines:
    src, dsts = l.split(":")
    for d in dsts.strip().split():
        G.add_edge(src, d)

nx.draw_networkx(G, font_size=8, node_color="white", edgecolors="black")

t0 = time()
cut = nx.minimum_edge_cut(G)
print(f"Min-cut: {cut}")
for c in cut:
    G.remove_edge(*c)
comps = nx.connected_components(G)
p1 = 1
for comp in comps:
    p1 *= len(comp)

print(f"Part I = {p1} in {time() - t0}s")