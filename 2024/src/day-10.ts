type TopoMap = Map<string, { height: number; neigh: string[] }>;

const dirs = [
  [0, 1],
  [1, 0],
  [0, -1],
  [-1, 0],
];

const parseInput = (input: string) => {
  const lines = input.trim().split("\n");
  const map: TopoMap = new Map();
  const [height, width] = [lines.length, lines[0].length];

  const inBounds = ([x, y]: readonly [number, number]) =>
    x >= 0 && x < width && y >= 0 && y < height;

  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[0].length; x++) {
      const height = parseInt(lines[y][x]);
      const key = JSON.stringify([x, y]);
      const neigh = dirs
        .map(([dx, dy]) => [x + dx, y + dy] as const)
        .filter((pos) => inBounds(pos))
        .filter((pos) => parseInt(lines[pos[1]][pos[0]]) === height + 1)
        .map((pos) => JSON.stringify(pos));
      map.set(key, { height, neigh });
    }
  }
  return map;
};

const calculateScore = (map: TopoMap, head: string) => {
  const visited = new Set<string>();
  const queue = [head];
  while (queue.length > 0) {
    const current = queue.shift()!;
    if (visited.has(current)) continue;
    visited.add(current);
    const { neigh } = map.get(current)!;
    queue.push(...neigh);
  }
  return [...visited]
    .map((key) => map.get(key)!)
    .filter((node) => node.height === 9).length;
};

function calculateRating(map: TopoMap, head: string): number {
  const visited = new Set<string>();
  let pathCount = 0;

  function dfs(current: string) {
    const node = map.get(current)!;
    if (node.height === 9) {
      pathCount++;
      return;
    }
    visited.add(current);
    node.neigh.filter((neigh) => !visited.has(neigh)).forEach(dfs);
    visited.delete(current);
  }

  dfs(head);
  return pathCount;
}

export function part1(input: string) {
  const map = parseInput(input);
  return [...map.entries()]
    .filter(([key, node]) => node.height === 0)
    .map(([key, node]) => calculateScore(map, key))
    .reduce((a, b) => a + b, 0);
}

export function part2(input: string) {
  const map = parseInput(input);
  return [...map.entries()]
    .filter(([key, node]) => node.height === 0)
    .map(([key, node]) => calculateRating(map, key))
    .reduce((a, b) => a + b, 0);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-10.txt").text();
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
