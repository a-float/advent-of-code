const getGraph = (input: string) => {
  const graph = new Map<string, string[]>();
  input.split("\n").forEach((line) => {
    const [a, b] = line.split("-");
    graph.set(a, [...(graph.get(a) ?? []), b]);
    graph.set(b, [...(graph.get(b) ?? []), a]);
  });
  return graph;
};

export const part1 = (input: string) => {
  const graph = getGraph(input);
  const nodes = [...graph.keys()];
  const loops = new Set<string>();
  for (const node of nodes.filter((n) => n.startsWith("t"))) {
    const queue = [{ node, prev: [] as string[] }];
    while (queue.length > 0) {
      const curr = queue.shift()!;
      if (curr.prev.length >= 3) continue;
      for (const neighbor of graph.get(curr.node)!) {
        if (curr.prev.length === 2 && neighbor === node) {
          const loop = [...curr.prev, curr.node];
          loops.add(loop.toSorted().join(","));
          continue;
        }
        queue.push({
          node: neighbor,
          prev: [...curr.prev, curr.node],
        });
      }
    }
  }
  return loops.size;
};

export const part2 = (input: string) => {
  const graph = getGraph(input);

  const maxClique = (nodes: string[]): string[] => {
    if (nodes.length === 0) return [];
    if (nodes.length === 1) return nodes;
    const [first, ...rest] = nodes;
    const fn = graph.get(first)!;
    if (rest.every((n) => fn.includes(n))) {
      return [first, ...maxClique(rest)];
    }
    const a = maxClique(rest);
    const b = [first, ...maxClique(rest.filter((n) => fn.includes(n)))];
    return a.length > b.length ? a : b;
  };

  return [...graph.entries()]
    .reduce((acc, [node, neighbours]) => {
      const clique = maxClique([node, ...neighbours]);
      return acc.length > clique.length ? acc : clique;
    }, [] as string[])
    .toSorted()
    .join(",");
};

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-23.txt").text();
  const time = async (fn: Function) => {
    const start = performance.now();
    const res = await fn();
    const end = performance.now();
    process.stdout.write(`(${(end - start).toFixed(3)}ms) `);
    return res;
  };
  console.log("Part 1:", await time(() => part1(input)));
  console.log("Part 2:", await time(() => part2(input)));
}
