type GardenMap = Map<string, { flower: string; neigh: string[] }>;

type Region = {
  area: number;
  perimeter: number;
  flower: string;
  keys?: Set<string>;
  bounds: {
    x: [number, number];
    y: [number, number];
  };
};

const dirs = [
  [0, 1],
  [1, 0],
  [0, -1],
  [-1, 0],
];

const parseInput = (input: string) => {
  const lines = input.trim().replaceAll("\r", "").split("\n");
  const graph: GardenMap = new Map();
  const [height, width] = [lines.length, lines[0].length];

  const inBounds = ([x, y]: readonly [number, number]) =>
    x >= 0 && x < width && y >= 0 && y < height;

  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[0].length; x++) {
      const key = JSON.stringify([x, y]);
      const neigh = dirs
        .map(([dx, dy]) => [x + dx, y + dy] as const)
        .filter((pos) => inBounds(pos))
        .map((pos) => JSON.stringify(pos));
      graph.set(key, { flower: lines[y][x], neigh });
    }
  }
  return graph;
};

const updateBounds = (region: Region, [x, y]: [number, number]) => {
  region.bounds.x[0] = Math.min(region.bounds.x[0], x);
  region.bounds.x[1] = Math.max(region.bounds.x[1], x);
  region.bounds.y[0] = Math.min(region.bounds.y[0], y);
  region.bounds.y[1] = Math.max(region.bounds.y[1], y);
};

const getRegions = (graph: GardenMap) => {
  const visited = new Set<string>();
  const regions: Region[] = [];
  for (const [key, node] of graph) {
    if (visited.has(key)) continue;
    const newRegion: Region = {
      area: 0,
      perimeter: 0,
      keys: new Set(),
      flower: node.flower,
      bounds: {
        x: [Infinity, -Infinity],
        y: [Infinity, -Infinity],
      },
    };

    const queue = [key];
    while (queue.length > 0) {
      const key = queue.shift()!;
      const current = graph.get(key)!;
      if (visited.has(key)) continue;
      visited.add(key);
      const [x, y] = JSON.parse(key);
      updateBounds(newRegion, [x, y]);
      newRegion.perimeter += 4;
      newRegion.area++;
      newRegion.keys?.add(key);
      for (const neigh of current.neigh) {
        if (graph.get(neigh)!.flower === newRegion.flower) {
          queue.push(neigh);
          newRegion.perimeter--;
        }
      }
    }
    regions.push(newRegion);
  }
  return regions;
};

export function part1(input: string) {
  const graph = parseInput(input);
  return getRegions(graph).reduce((acc, r) => acc + r.area * r.perimeter, 0);
}

/**
 * Scan each region with a 1 cell padding using a 2x2 kernel and count the corners.
 * There are 3 cases to consider:
 * - 1 cell belongs to the region - gives 1 corner
 * - 3 cells belong to the region - gives 1 corner
 * - 2 cells on the diagonal belong to the region (a hole) - gives 2 corners
 *  */
export function part2(input: string) {
  const graph = parseInput(input);
  const regions = getRegions(graph);

  let answer = 0;
  for (const region of regions) {
    let corners = 0;
    for (let y = region.bounds.y[0] - 1; y < region.bounds.y[1] + 1; y++) {
      for (let x = region.bounds.x[0] - 1; x < region.bounds.x[1] + 1; x++) {
        const square = [
          [x, y],
          [x + 1, y],
          [x, y + 1],
          [x + 1, y + 1],
        ]
          .map((pos) => JSON.stringify(pos))
          .map<number>((key) => (region.keys?.has(key) ? 1 : 0));

        const squareSum = square.reduce((acc, v) => acc + v, 0);
        if (squareSum === 2) {
          if (square[0] && square[3]) corners += 2;
          if (square[1] && square[2]) corners += 2;
        } else if (squareSum === 1 || squareSum === 3) corners++;
      }
    }
    answer += region.area * corners;
  }
  return answer;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-12.txt").text();
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
