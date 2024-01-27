import * as fs from "fs";
import * as os from "os";

type Coord = {
  id: number;
  value: number;
};

const nums: Coord[] = fs
  .readFileSync("../data/data20.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL)
  .map((x, idx) => ({ value: parseInt(x), id: idx }));

const mod = (x: number, m: number): number => ((x % m) + m) % m;

const mix = (arr: Coord[]): Coord[] => {
  const copy = [...arr];
  const order = [...arr].sort((a, d) => a.id - d.id);
  for (let coord of order) {
    const { value: val, id } = coord;
    const valMod = val % (copy.length - 1);
    const idx = copy.findIndex((x) => x.id === id);
    const sign = Math.sign(coord.value);

    for (let d = 0; d !== valMod; d += sign) {
      copy[mod(idx + d, copy.length)] = copy[mod(idx + d + sign, copy.length)];
    }
    copy[mod(idx + valMod, copy.length)] = coord;
  }
  return copy;
};

const calculateGrove = (mixed: Coord[]): number => {
  const zeroPos = mixed.findIndex((x) => x.value === 0);
  const ans = [1000, 2000, 3000].map(
    (x) => mixed[mod(zeroPos + x, mixed.length)].value
  );
  return ans.reduce((a, b) => a + b, 0);
};

console.time("Runtime 1")
const p1 = calculateGrove(mix(nums));
console.timeEnd("Runtime 1")
console.log("Part 1: " + p1);

const MULT = 811589153;

console.time("Runtime 2")
let mixed = nums.map((x) => ({ ...x, value: x.value * MULT }));
for (let i = 0; i < 10; i++) mixed = mix(mixed);
const p2 = calculateGrove(mixed);
console.timeEnd("Runtime 2")
console.log("Part 2: " + p2);