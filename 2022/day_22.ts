import * as fs from "fs";
import * as os from "os";

type Rotation = 0 | 1 | 2 | 3;
const Directions: { [k in Rotation]: [number, number] } = {
  3: [0, -1],
  0: [1, 0],
  1: [0, 1],
  2: [-1, 0],
};

const inBounds = (x: number, y: number, tiles: string[]) =>
  x >= 0 && y >= 0 && x < tiles[0].length && y < tiles.length;

const getTile = (
  x: number,
  y: number,
  rotation: Rotation,
  map: string[]
): [number, number, string, Rotation] => {
  const diff = Directions[rotation];
  const [nextX, nextY] = [x + diff[0], y + diff[1]];
  if (inBounds(nextX, nextY, tiles) && map[nextY][nextX] !== " ") {
    return [nextX, nextY, map[nextY][nextX], rotation];
  }
  let last = [x, y];
  let curr = [x - diff[0], y - diff[1]];
  while (inBounds(curr[0], curr[1], tiles) && map[curr[1]][curr[0]] !== " ") {
    last = curr;
    curr = curr.map((x, i) => x - diff[i]);
  }
  return [last[0], last[1], map[last[1]][last[0]], rotation];
};

// const cheat: {
//   [k: string]: (x: number, y: number) => [number, number, Rotation];
// } = {
//   "2,1->3,1": (x, y) => [15 - y, 8, 1],
//   "2,2->2,3": (x, y) => [3 - x, 7, 3],
//   "1,1->1,0": (x, y) => [8, x, 0],
// };

const cheat: {
  [k: string]: (x: number, y: number) => [number, number, Rotation];
} = {
  "1,0->0,0": (x, y) => [0, 149 - y, 0],
  "1,0->1,-1": (x, y) => [0, 150 + x, 0],
  "2,0->2,-1": (x, y) => [x, 199, 3],
  "2,0->3,0": (x, y) => [99, 149 - y, 2],
  "2,0->2,1": (x, y) => [99, 50 + x, 2],
  "1,1->0,1": (x, y) => [y, 100, 1],
  "1,1->2,1": (x, y) => [100 + y, 49, 3],
  "0,2->0,1": (x, y) => [50, 50 + x, 0],
  "0,2->-1,2": (x, y) => [50, 49 - y, 0],
  "1,2->2,2": (x, y) => [149, 49 - y, 2],
  "1,2->1,3": (x, y) => [49, 150 + x, 2],
  "0,3->1,3": (x, y) => [50 + y, 149, 3],
  "0,3->-1,3": (x, y) => [50 + y, 0, 1],
  "0,3->0,4": (x, y) => [100 + x, 0, 1],
};

const getTileOnCube = (
  x: number,
  y: number,
  rotation: Rotation,
  map: string[]
): [number, number, string, Rotation] => {
  const diff = Directions[rotation];
  const [nextX, nextY] = [x + diff[0], y + diff[1]];
  if (inBounds(nextX, nextY, tiles) && map[nextY][nextX] !== " ") {
    return [nextX, nextY, map[nextY][nextX], rotation];
  }
  const D = 50;
  const [blockX, blockY] = [Math.floor(x / D), Math.floor(y / D)];
  const nextBlock = { x: Math.floor(nextX / D), y: Math.floor(nextY / D) };
  const hash = `${blockX},${blockY}->${nextBlock.x},${nextBlock.y}`;
  const cubeDiff = cheat[hash];
  if (!cubeDiff) throw new Error("Unrecognized edge: " + hash);

  const [finX, finY, finRot] = cubeDiff(x % D, y % D);
  if (tiles[finY][finX] === " ")
    throw new Error(
      "Got empty for " + hash + " " + x + " " + y + " " + rotation
    );
  return [finX, finY, tiles[finY][finX], finRot];
};

const lines = fs
  .readFileSync("../data/data22.txt", { encoding: "utf-8" })
  .split(os.EOL + os.EOL);

const unpaddedTiles = lines[0].split(os.EOL);
const maxLen = Math.max(...unpaddedTiles.map((x) => x.length));
const tiles = unpaddedTiles.map((x) => x + " ".repeat(maxLen - x.length));

let [x, y] = [tiles[0].indexOf("."), 0];

let rot: Rotation = 0;

const path = lines[1].match(/[RLUB]+|[0-9]+/g);
if (!path) throw new Error("Invalid path: " + lines[1]);
for (const move of path) {
  if (move === "R") rot = ((rot + 1) % 4) as Rotation;
  else if (move === "L") rot = ((4 + rot - 1) % 4) as Rotation;
  else {
    const steps = parseInt(move);
    for (let i = 0; i < steps; i++) {
      const [newX, newY, tile, newRot] = getTileOnCube(x, y, rot, tiles);
      if (tile === ".") {
        [x, y, rot] = [newX, newY, newRot];
      }
    }
  }
}

console.log("Part 2: " + 1000 * (y + 1) + 4 * (x + 1) + rot);