import * as fs from "fs";
import * as os from "os";

const text = fs.readFileSync("../data/data4.txt", {
  encoding: "utf8",
  flag: "r",
});

const [s, moves] = text.split(os.EOL + os.EOL).map((x) => x.split(os.EOL));

const cols = s[s.length - 1]
  .trim()
  .split(/\s+/)
  .map((x) => parseInt(x));
const colsCount = cols[cols.length - 1];

const ogStacks: string[][] = [];

for (let x = 0; x < colsCount; x++) {
  for (let y = s.length - 2; y >= 0; y--) {
    const newChar = s[y][1 + 4 * x];
    if (newChar != " ") ogStacks[x] = [...(ogStacks[x] || []), newChar];
  }
}

let stacks = ogStacks.map((x) => x.slice());
// Part 1
for (const move of moves) {
  const [count, from, to] = move
    .match(/\d+/g)!
    .slice(3)
    .map((x) => parseInt(x));
  for (let i = 0; i < count; i++) {
    stacks[to - 1].push(stacks[from - 1].pop()!);
  }
}
const p1 = stacks.map((x) => x[x.length - 1]).join("");

stacks = ogStacks;
// Part 2
for (const move of moves) {
  const [count, from, to] = move
    .match(/\d+/g)!
    .slice(3)
    .map((x) => parseInt(x));
  const toAdd = stacks[from - 1].splice(-count);
  stacks[to - 1].push(...toAdd);
}
const p2 = stacks.map((x) => x[x.length - 1]).join("");

console.log("Part 1:", p1);
console.log("Part 2:", p2);
