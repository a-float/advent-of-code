import * as fs from "fs";
import * as os from "os";

type Forest = readonly number[][];
type Vector2 = [number, number]; // TODO use {x: number, y: number}

console.time("loadData");
const forest = fs
  .readFileSync("../data/data8.txt", { encoding: "utf8", flag: "r" })
  .split(os.EOL)
  .map((row) => row.split("").map((tree) => parseInt(tree)));

console.timeEnd("loadData");
console.time("Part 1 execution time");

const vec2str = (vec: Vector2) => `${vec[0]}:${vec[1]}`;

const checkLine = (
  forest: Forest,
  start: Vector2,
  dir: Vector2
): Set<Vector2> => {
  let pos: Vector2 = [...start];
  let maxTree = -1;
  const res = new Set<Vector2>();
  while (
    pos[0] >= 0 &&
    pos[0] < forest.length &&
    pos[1] >= 0 &&
    pos[1] < forest[0].length
  ) {
    const tree = forest[pos[0]][pos[1]];
    if (tree > maxTree) {
      maxTree = tree;
      res.add(pos);
    }
    pos = [pos[0] + dir[0], pos[1] + dir[1]];
  }
  return res;
};

const megaSet = new Set<string>();
// check left and right
for (let i = 1; i < forest.length - 1; i++) {
  const ltr = checkLine(forest, [i, 0], [0, 1]);
  const rtl = checkLine(forest, [i, forest[0].length - 1], [0, -1]);
  [...ltr, ...rtl].forEach((x) => megaSet.add(vec2str(x)));
}
// check up and bottom
for (let i = 1; i < forest[0].length - 1; i++) {
  const utb = checkLine(forest, [0, i], [1, 0]);
  const btu = checkLine(forest, [forest.length - 1, i], [-1, 0]);
  [...utb, ...btu].forEach((x) => megaSet.add(vec2str(x)));
}

console.timeEnd("Part 1 execution time");
console.log("Part 1:", megaSet.size + 4); // add 4 corners
console.time("Part 2 execution time");

const checkTreeView = (
  forest: Forest,
  treePos: Vector2,
  dir: Vector2
): number => {
  let pos: Vector2 = [...treePos];
  const startTree = forest[pos[0]][pos[1]];
  let range = 0;
  pos = [pos[0] + dir[0], pos[1] + dir[1]];
  while (
    pos[0] >= 0 && // TODO extract to forest class
    pos[0] < forest.length &&
    pos[1] >= 0 &&
    pos[1] < forest[0].length
  ) {
    range++;
    const tree = forest[pos[0]][pos[1]];
    if (tree >= startTree) return range;
    pos = [pos[0] + dir[0], pos[1] + dir[1]];
  }
  return range;
};

let maxViewScore = -1;
// console.log(checkTreeView(forest, [0, 0], [0, -1]));
for (let y = 0; y < forest.length; y++) {
  for (let x = 0; x < forest[0].length; x++) {
    const pos: Vector2 = [y, x];
    const ranges = [
      checkTreeView(forest, pos, [1, 0]),
      checkTreeView(forest, pos, [-1, 0]),
      checkTreeView(forest, pos, [0, 1]),
      checkTreeView(forest, pos, [0, -1]),
    ];
    const mult = ranges.reduce((a, b) => a * b, 1);
    maxViewScore = Math.max(maxViewScore, mult);
  }
}

console.timeEnd("Part 2 execution time");
console.log("Part 2:", maxViewScore);
