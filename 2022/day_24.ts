import * as fs from "fs";
import * as os from "os";

function gcd(a: number, b: number): number {
  return !b ? a : gcd(b, a % b);
}

function lcm(a: number, b: number): number {
  return (a * b) / gcd(a, b);
}

type Rotation = 0 | 1 | 2 | 3;
type Pos = { x: number; y: number };
type Wind = { pos: Pos; rot: Rotation };
const Directions: { [k in Rotation]: [number, number] } = {
  0: [1, 0],
  1: [0, 1],
  2: [-1, 0],
  3: [0, -1],
};

type Size = { width: number; height: number };
const winds: Map<string, Wind[]>[] = [];
const rootWinds = new Map<string, Wind[]>();

const lines = fs
  .readFileSync("../data/data24.txt", { encoding: "utf-8" })
  .split(os.EOL);

lines.forEach((row, y) =>
  row.split("").forEach((c, x) => {
    if (["#", "."].includes(c)) return;
    const pos = { x, y };
    const rot = [">", "v", "<", "^"].indexOf(c) as Rotation;
    rootWinds.set(JSON.stringify(pos), [{ pos, rot }]);
  })
);

const size: Size = { width: lines[0].length, height: lines.length } as const;

const inBounds = (x: number, y: number) =>
  (x === 1 && y === 0) || // entrance
  (x === size.width - 2 && y === size.height - 1) || //exit
  (x > 0 && y > 0 && x < size.width - 1 && y < size.height - 1);

const getMoves = (pos: Pos, winds: Map<string, Wind[]>): Pos[] => {
  return [...Object.values(Directions), [0, 0]]
    .map((diff) => ({
      x: pos.x + diff[0],
      y: pos.y + diff[1],
    }))
    .filter((p) => inBounds(p.x, p.y))
    .filter((p) => !winds.has(JSON.stringify(p)));
};

const moveWinds = (winds: Map<string, Wind[]>): Map<string, Wind[]> => {
  const newWinds = new Map<string, Wind[]>();
  for (const windList of winds.values()) {
    for (const wind of windList) {
      const diff = Directions[wind.rot];
      const newPos = { x: wind.pos.x + diff[0], y: wind.pos.y + diff[1] };
      if (newPos.x === 0) newPos.x = size.width - 2;
      if (newPos.x === size.width - 1) newPos.x = 1;
      if (newPos.y === 0) newPos.y = size.height - 2;
      if (newPos.y === size.height - 1) newPos.y = 1;
      const hash = JSON.stringify(newPos);
      const old = newWinds.get(hash);
      newWinds.set(hash, [...(old ?? []), { ...wind, pos: newPos }]);
    }
  }
  return newWinds;
};

winds.push(rootWinds);
for (let i = 1; i < lcm(size.width - 2, size.height - 2); i++) {
  winds.push(moveWinds(winds[i - 1]));
}

const manhattan = (a: Pos, b: Pos) => Math.abs(a.x - b.x) + Math.abs(a.y - b.y);

type Element = { pos: Pos; prev: Pos[]; time: number };

// time === prev.length
const pathfind = (start: Pos, target: Pos, time: number, w: typeof winds) => {
  const visited = new Set<string>();
  const queue: Element[] = [{ pos: start, prev: [], time }];
  while (queue.length > 0) {
    const curr = queue.shift()!;
    const hash = JSON.stringify({ pos: curr.pos, time: curr.time });
    if (visited.has(hash)) continue;
    visited.add(hash);
    if (curr.pos.x === target.x && curr.pos.y === target.y) {
      return { ...curr, time: curr.time + 1 };
    }
    // if (curr.time > 20) return { ...curr, time: 9999 };
    const wind = winds[(1 + curr.time) % winds.length];
    getMoves(curr.pos, wind).forEach((x) => {
      queue.push({
        pos: x,
        time: curr.time + 1,
        prev: [...curr.prev, curr.pos],
      });
    });
    queue.sort(
      (a, d) =>
        a.time - d.time + manhattan(a.pos, target) - manhattan(d.pos, target)
    );
  }
  throw new Error(`Target ${target} unreachable from ${start} at ${time}`);
};

const start = { x: 1, y: 0 };
const end = { x: size.width - 2, y: size.height - 2 };

console.time("Runtime 1");
const a = pathfind(start, end, 0, []).time;
const b = pathfind(end, start, a, []).time;
const c = pathfind(start, end, b, []).time;
console.log(`Part 1: ${a}`);
console.log(`Part 2: ${a} + ${b - a} + ${c - b} = ${c}`);

console.timeEnd("Runtime 1");

// more than 234
