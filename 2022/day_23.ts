import * as fs from "fs";
import * as os from "os";

type Vector = { x: number; y: number };
type Elf = { pos: Vector; dir: number };

const lines = fs
  .readFileSync("../data/data23.txt", { encoding: "utf-8" })
  .split(os.EOL);

const elves = new Map<string, Elf>();
lines.forEach((line, y) =>
  line.split("").forEach((c, x) => {
    const pos = { x, y };
    if (c === "#") elves.set(JSON.stringify(pos), { pos, dir: 0 });
  })
);

const getNeighbours = (pos: Vector, map: Map<string, Elf>): boolean[] =>
  [-1, 0, 1].flatMap((y) =>
    [-1, 0, 1]
      .filter((x) => x !== 0 || y !== 0)
      .map((x) => map.has(JSON.stringify({ x: pos.x + x, y: pos.y + y })))
  );

const doRound = (elves: Map<string, Elf>) => {
  const props = new Map<string, Elf[]>();
  for (const elf of elves.values()) {
    const { pos, dir } = elf;
    const neigh = getNeighbours(pos, elves);
    // console.log(neigh, pos);
    elf.dir++;
    if (neigh.some((x) => x)) {
      let targetPos = undefined;
      for (let d = 0; d < 4; d++) {
        if ((dir + d) % 4 === 0) {
          if (!neigh[0] && !neigh[1] && !neigh[2]) {
            targetPos = { x: pos.x, y: pos.y - 1 };
          }
        } else if ((dir + d) % 4 === 1) {
          if (!neigh[5] && !neigh[6] && !neigh[7]) {
            targetPos = { x: pos.x, y: pos.y + 1 };
          }
        } else if ((dir + d) % 4 === 2) {
          if (!neigh[0] && !neigh[3] && !neigh[5]) {
            targetPos = { x: pos.x - 1, y: pos.y };
          }
        } else if ((dir + d) % 4 === 3) {
          if (!neigh[2] && !neigh[4] && !neigh[7]) {
            targetPos = { x: pos.x + 1, y: pos.y };
          }
        }
        if (targetPos) break;
      }
      if (targetPos) {
        // console.log(JSON.stringify(pos) + " -> " + JSON.stringify(targetPos));
        const hash = JSON.stringify(targetPos);
        const old = props.get(hash) ?? [];
        props.set(hash, [...old, { ...elf }]);
      }
    }
  }
  let someoneMoved = false;
  for (const [hash, queue] of props.entries()) {
    if (queue.length === 1) {
      const newPos = JSON.parse(hash) as Vector;
      someoneMoved = true;
      elves.set(hash, { pos: newPos, dir: queue[0].dir });
      elves.delete(JSON.stringify(queue[0].pos));
    }
  }
  return someoneMoved;
};

for (let i = 0; i < 10; i++) doRound(elves);

const getResult = (elves: Map<string, Elf>): number => {
  const left = Math.min(...[...elves.values()].map((x) => x.pos.x));
  const right = Math.max(...[...elves.values()].map((x) => x.pos.x));
  const top = Math.min(...[...elves.values()].map((x) => x.pos.y));
  const bottom = Math.max(...[...elves.values()].map((x) => x.pos.y));

  return (right - left + 1) * (bottom - top + 1) - elves.size;
};

console.log("Part I:  " + getResult(elves));

let i = 11;
while (doRound(elves)) {
  i++;
}

console.log("Part II: " + i);
