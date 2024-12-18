type Point = [number, number];
type State = { pos: Point; dist: number };

const SIZE = 71;
const TAKE = 1024;

const hash = (p: Point) => JSON.stringify(p);

const readBytes = (input: string) =>
  input
    .trim()
    .split("\n")
    .map((line) => line.split(",").map(Number)) as Point[];

const findPath = (corrupted: Set<string>) => {
  const visited = new Set<string>();
  const queue: State[] = [{ pos: [0, 0], dist: 0 }];
  while (queue.length > 0) {
    const { pos, dist } = queue.shift()!;
    const key = JSON.stringify(pos);
    if (pos.every((x) => x === SIZE - 1)) return { pos, dist };
    if (visited.has(key)) continue;
    visited.add(key);
    const [x, y] = pos;

    // prettier-ignore
    [[0, 1], [1, 0], [0, -1], [-1, 0]]
      .map(([dx, dy]) => [x + dx, y + dy] as Point)
      .filter((p) => p.every((x) => x >= 0 && x < SIZE))
      .filter((p) => !corrupted.has(hash(p)))
      .forEach((p) => {
        queue.push({ pos: p, dist: dist + 1 });
      });
  }
  return null;
};

export function part1(input: string) {
  const bytes = readBytes(input);
  const corrupted = new Set<string>(bytes.slice(0, TAKE).map(hash));
  return findPath(corrupted)!.dist;
}

export function part2(input: string) {
  const bytes = readBytes(input);
  const hashedBytes = bytes.map(hash);

  function binSearch(): number {
    let low = 0;
    let high = bytes.length - 1;
    while (low < high) {
      const mid = Math.floor((low + high) / 2);
      const path = findPath(new Set(hashedBytes.slice(0, mid)));
      if (path !== null) {
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    }
    return low;
  }

  return bytes[binSearch()].join(",");
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-18.txt").text();
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
