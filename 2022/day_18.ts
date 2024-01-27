import * as fs from "fs";
import * as os from "os";

type Vector3 = [number, number, number];

const hash = (vec: Vector3) => JSON.stringify(vec);

const getSides = (vec: Vector3): Vector3[] =>
  [-1, 1].flatMap((diff) =>
    [0, 1, 2].map(
      (i) => Array.from({ ...vec, [i]: vec[i] + diff, length: 3 }) as Vector3
    )
  );

const blocks: Vector3[] = fs
  .readFileSync("../data/data18.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL)
  .map((x) => x.split(",").map((y) => parseInt(y)) as Vector3);

const getRockArea = (blocks: Vector3[]): [number, Set<string>] => {
  let exposedSides = 0;
  const blockSet = new Set<string>();
  for (const block of blocks) {
    const empty = getSides(block).filter((x) => !blockSet.has(hash(x)));
    exposedSides += 2 * empty.length - 6;
    blockSet.add(hash(block));
  }
  return [exposedSides, blockSet];
};

console.time("Runtime 1");
const [exposed, blockSet] = getRockArea(blocks);

console.timeEnd("Runtime 1");

console.log("Part 1:", exposed);

console.time("Runtime 2");

let outline = new Set(
  [...blockSet]
    .flatMap((x) => getSides(JSON.parse(x)))
    .filter((x) => !blockSet.has(hash(x)))
    .map(hash)
);

// console.log(outline.size);

[...outline].forEach((x) =>
  getSides(JSON.parse(x))
    .map((y) => hash(y))
    .filter((y) => !blockSet.has(y) && !outline.has(y))
    .forEach((y) => outline.add(y))
);

// console.log(outline.size);

const bfs = (startPos: Vector3, outline: Set<string>): Set<string> => {
  const visited = new Set<string>();
  const queue = [startPos];
  while (queue.length > 0) {
    const pos = queue.shift()!;
    if (visited.has(hash(pos))) continue;
    visited.add(hash(pos));

    for (const side of getSides(pos)) {
      const h = hash(side);
      if (outline.has(h) && !visited.has(h)) {
        queue.push(side);
      }
    }
  }
  return visited;
};

const comps = [];

while (outline.size > 0) {
  const component = bfs(JSON.parse([...outline][0]), outline);
  comps.push(component);
  outline = new Set([...outline].filter((x) => !component.has(x)));
}

const exploreFrom = (startPos: Vector3, rock: Set<string>): number => {
  let insideArea = 0;
  const visited = new Set<string>();
  const queue = [startPos];
  while (queue.length > 0) {
    const pos = queue.shift()!;
    if (visited.has(hash(pos))) continue;
    visited.add(hash(pos));
    const free = getSides(pos).filter((x) => !rock.has(hash(x)));
    insideArea += 6 - free.length;
    free.filter((x) => !visited.has(hash(x))).forEach((x) => queue.push(x));
  }
  return insideArea;
};

// the biggest component might not be the outside one.
// should check by finding an edge along any axis (max|min x|y|z)
comps.sort((a, d) => d.size - a.size);
console.log(comps.map((x, i) => `$Component ${i} of size ${x.size} explored`));

let totalInside = 0;
for (let i = 1; i < comps.length; i++) {
  const p = exploreFrom(JSON.parse([...comps[i]][0]), blockSet);
  totalInside += p;
  console.log(`Inside area of comp ${i} is ${p}`);
}

console.log(`Part 2: ${exposed} - ${totalInside} = ${exposed - totalInside}`);

console.timeEnd("Runtime 2");
