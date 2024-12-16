import TinyQueue from "tinyqueue";

type Point = readonly [number, number];
type State = { pos: Point; dir: Point; dist: number; prev: Point[] };
const DIRS = {
  UP: [0, -1],
  RIGHT: [1, 0],
  DOWN: [0, 1],
  LEFT: [-1, 0],
} as const;

const getPos = (grid: string[][], char: string): Point => {
  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      if (grid[y][x] === char) {
        return [x, y];
      }
    }
  }
  throw new Error(`Character ${char} not found in grid`);
};

const getMinPaths = (
  grid: string[][],
  options: { maxPaths: number } = { maxPaths: Infinity }
) => {
  const visited = new Map<string, number>();
  const start: State = {
    pos: getPos(grid, "S"),
    dir: DIRS.RIGHT,
    dist: 0,
    prev: [],
  };
  const queue = new TinyQueue<State>([start], (a, d) => a.dist - d.dist);
  const ends: State[] = [];
  while (queue.length > 0) {
    const curr = queue.pop()!;
    const key = JSON.stringify({ pos: curr.pos, dir: curr.dir });
    if (!visited.has(key)) {
      visited.set(key, curr.dist);
    } else if (visited.get(key)! < curr.dist) continue;
    // prettier-ignore
    const { pos: [x, y], dir: [dx, dy], dist } = curr;
    if (ends.length > 0 && dist > ends[0].dist) continue;
    if (grid[y][x] === "E") {
      ends.push(curr);
      if (ends.length >= options.maxPaths) break;
      continue;
    }
    Object.values(DIRS).forEach(([ddx, ddy]) => {
      if (ddx === -dx && ddy === -dy) return;
      if (grid[y + ddy][x + ddx] === "#") return;
      const cost = ddx === dx && ddy === dy ? 1 : 1001;
      queue.push({
        pos: [x + ddx, y + ddy],
        dir: [ddx, ddy],
        dist: dist + cost,
        prev: [...curr.prev, curr.pos],
      });
    });
  }
  return ends;
};

export function part1(input: string) {
  const grid = input.split("\n").map((line) => line.split(""));
  const paths = getMinPaths(grid, { maxPaths: 1 });
  return paths[0].dist;
}

export function part2(input: string) {
  const grid = input.split("\n").map((line) => line.split(""));
  const paths = getMinPaths(grid);
  return (
    new Set(paths.flatMap((e) => e.prev.map((x) => JSON.stringify(x)))).size + 1
  );
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-16.txt").text();
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
