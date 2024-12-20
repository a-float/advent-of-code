type Point = [number, number];
type State = { pos: Point; dist: number };
//prettier-ignore
const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]] as const;

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

const hash = (pos: Point) => pos[0] * 1000 + pos[1];

const getFlood = (grid: string[][], start: Point) => {
  const queue: State[] = [{ pos: start, dist: 0 }];
  const flood = new Map<ReturnType<typeof hash>, number>();
  while (queue.length > 0) {
    const curr = queue.shift()!;
    const key = hash(curr.pos);
    if (flood.has(key)) continue;
    flood.set(key, curr.dist);
    const [x, y] = curr.pos;
    dirs
      .map(([dx, dy]) => [x + dx, y + dy])
      .filter(([nx, ny]) => grid[ny][nx] !== "#")
      .forEach(([nx, ny]) => {
        queue.push({ pos: [nx, ny], dist: curr.dist + 1 });
      });
  }
  return flood;
};

const countCheats = (
  input: string,
  opts: { maxCheatDuration: number; minImprovement: number }
) => {
  const grid = input
    .trim()
    .split("\n")
    .map((line) => line.split(""));
  const S = getPos(grid, "S");
  const E = getPos(grid, "E");
  const startFlood = getFlood(grid, S);
  const endFlood = getFlood(grid, E);

  const fairDist = startFlood.get(hash(E))!;
  const cheats = [];
  const MCR = opts.maxCheatDuration;
  let checks = 0;

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      for (let dy = -MCR; dy <= MCR; dy++) {
        for (let dx = -MCR; dx <= MCR; dx++) {
          checks++;
          const dist = Math.abs(dx) + Math.abs(dy);
          if (dist > MCR) continue;
          const nx = x + dx;
          const ny = y + dy;
          if (nx < 0 || ny < 0 || nx >= grid[y].length || ny >= grid.length) {
            continue;
          }
          if (grid[ny][nx] === "#") continue;
          const posKey = hash([x, y]);
          const newPosKey = hash([nx, ny]);
          const cheatDist =
            startFlood.get(posKey)! + endFlood.get(newPosKey)! + dist;
          if (cheatDist <= fairDist - opts.minImprovement) {
            cheats.push(fairDist - cheatDist);
          }
        }
      }
    }
  }
  return cheats;
};

export const part1 = (input: string) =>
  countCheats(input, {
    maxCheatDuration: 2,
    minImprovement: 100,
  }).length;

export const part2 = (input: string) =>
  countCheats(input, {
    maxCheatDuration: 20,
    minImprovement: 100,
  }).length;

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-20.txt").text();
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
