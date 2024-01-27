import * as fs from "fs";
import * as os from "os";
import { nextTick } from "process";

type Wind = "<" | ">";

const wind: Wind[] = fs
  .readFileSync("../data/data17.txt", { encoding: "utf-8" })
  .trim()
  .split("")
  .filter((x): x is Wind => ["<", ">"].includes(x));

const rockType = {
  line: [
    [0, 0],
    [1, 0],
    [2, 0],
    [3, 0],
  ],
  plus: [
    [0, 1],
    [1, 0],
    [1, 1],
    [2, 1],
    [1, 2],
  ],
  corner: [
    [0, 0],
    [1, 0],
    [2, 0],
    [2, 1],
    [2, 2],
  ],
  pole: [
    [0, 0],
    [0, 1],
    [0, 2],
    [0, 3],
  ],
  box: [
    [0, 0],
    [1, 0],
    [1, 1],
    [0, 1],
  ],
};

class Hole {
  width: number;
  set: Set<string>;
  floor: number;
  constructor(width: number, floor: number) {
    this.set = new Set<string>();
    this.width = width;
    this.floor = floor;
  }

  has(pos: number[]) {
    return (
      pos[0] < 0 ||
      pos[0] >= this.width ||
      pos[1] < this.floor ||
      this.set.has(pos[0] + "." + pos[1])
    );
  }

  add(pos: number[]) {
    this.set.add(pos[0] + "." + pos[1]);
  }
}

// function* windGenerator() {
//   let i = 0;
//   while (true) {
//     yield wind[i % wind.length];
//     i++;
//   }
// }

let windTaken = 0;
// const getWind = windGenerator();

const dropRock = (
  [x, y]: number[],
  type: keyof typeof rockType,
  height: number,
  set: typeof rocks
): number => {
  let [cx, cy] = [x, y];
  // const wx = getWind.next().value === "<" ? -1 : 1;
  const wx = wind[windTaken] === "<" ? -1 : 1;
  windTaken = (windTaken + 1) % wind.length;

  const canMoveWind = !rockType[type].some((d) =>
    set.has([cx + d[0] + wx, cy + d[1]])
  );
  if (canMoveWind) cx += wx;

  const hasFallen = rockType[type].some((d) =>
    set.has([cx + d[0], cy + d[1] - 1])
  );
  if (hasFallen) {
    rockType[type].forEach((diff) => set.add([cx + diff[0], cy + diff[1]]));
    const newHeight = rockType[type].reduce(
      (acc, diff) => Math.max(acc, cy + diff[1] + 1),
      -Infinity
    );
    return Math.max(height, newHeight);
  }

  return dropRock([cx, cy - 1], type, height, set);
};

const isTetris = (hole: typeof rocks, height: number) =>
  Array.from({ length: hole.width }).every(
    (_, x) => !hole.has([x, height]) && hole.has([x, height - 1])
  );

const types = Object.keys(rockType) as unknown as (keyof typeof rockType)[];

let height = 0;
const rocks = new Hole(7, height);

const cache = new Map<string, { i: number; height: number }[]>();

const mySkip = {
  heightDiff: 15357,
  iDiff: 9810,
};

console.log("Starting");
console.time("Runtime");
// windTaken += 3028;
let i = 0;
const target = 1e12;
while (i < target) {
  const [iMod, windMod] = [i % types.length, windTaken];
  const hash = iMod + "." + windMod;
  if (isTetris(rocks, height)) {
    // console.log(`${hash} at ${i} height ${height}`);
    if (cache.has(hash)) cache.get(hash)!.push({ i, height });
    else cache.set(hash, [{ i, height }]);
    if (hash === "1.3028") {
      const leftToDrop = target - i;
      const skipApplies = Math.floor(leftToDrop / mySkip.iDiff);
      height += 2750 * skipApplies;
      i += 1745 * skipApplies;
      rocks.floor = height;
    }
  }
  // const skip = cache.get(hash);
  // if (skip) {
  // if (i + skip.skipI < target) {
  // console.log("Skipping from " + i + " by " + skip.skipI);
  //   i += skip.skipI;
  //   height += skip.heightDiff;
  //   rocks.floor = height;
  // }
  // }
  height = dropRock([2, height + 3], types[i % types.length], height, rocks);
  // if (isTetris(rocks, height)) {
  //   if (prevTetris.key.length > 0) {
  //     cache.set(prevTetris.key, {
  //       skipI: i - prevTetris.i,
  //       heightDiff: height - prevTetris.height,
  //       target: (i % types.length) + "." + (i % wind.length),
  //     });
  //   }
  //   rocks.floor = height;
  //   rocks.set.clear();
  // }
  i++;
}
console.timeEnd("Runtime");
console.log(`Part 1: ${height}`);
// console.log(cache);
console.log(wind.length);

// bigger than 1565443425060
// bigger than 1565443425080
// less than   1575931232079
