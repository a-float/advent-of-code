import * as fs from "fs";
import * as os from "os";

if (process.argv.length < 3) throw new Error("Time not specified");
const TIME = parseInt(process.argv[2]);
type Stock = [number, number, number, number];

interface Blueprint {
  id: number;
  robotCosts: [Stock, Stock, Stock, Stock];
  maxOreReq: number;
}

interface State {
  materials: Stock;
  robots: Stock;
}

const blueprints: Blueprint[] = fs
  .readFileSync("../data/data19.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL)
  .map(
    (x) =>
      [
        ...x.matchAll(
          /.*costs (\d+).* costs (\d+).*costs (\d+).* and (\d+).*costs (\d+).* and (\d+).*/g
        ),
      ][0]
  )
  .map((x, idx) => ({
    id: idx + 1,
    robotCosts: [
      [parseInt(x[1]), 0, 0, 0],
      [parseInt(x[2]), 0, 0, 0],
      [parseInt(x[3]), parseInt(x[4]), 0, 0],
      [parseInt(x[5]), 0, parseInt(x[6]), 0],
    ],
  }))
  .map(
    (x) =>
      ({
        ...x,
        maxOreReq: Math.max(...x.robotCosts.map((x) => x[0])),
      } as Blueprint)
  );

const hash = (state: State, time: number): string => {
  return JSON.stringify({ ...state, time });
};

const copy = (state: State): State => ({
  materials: [...state.materials],
  robots: [...state.robots],
});

const canBuyRobot = (robotCost: Stock, materials: Stock): boolean =>
  robotCost.every((x, i) => x <= materials[i]);

const buyRobot = (robotCost: Stock, materials: Stock): Stock =>
  robotCost.map((x, i) => materials[i] - x) as Stock;

const getMaxGeodes = (
  blueprint: Blueprint,
  state: State,
  time: number,
  cache: Map<string, number>,
  atLeast: number
): number => {
  if (time === 0) return state.materials[3];

  // min pruning
  // const maxPotentialGeodes =
  //   state.robots[3] * time + ((time - 1) * (time - 2)) / 4;
  // if (maxPotentialGeodes <= atLeast) return 0;

  // caching
  const h = hash(state, time);
  if (cache.has(h)) return cache.get(h)!;
  const moves = [];

  const newMaterials: Stock = state.materials.map(
    (x, i) => x + state.robots[i]
  ) as Stock;
  // state.robots[3] > 0 && console.log(state.robots);

  // try to buy a robot
  let canBuyAllRobots = true;
  for (let r = 3; r >= 0; r--) {
    if (canBuyRobot(blueprint.robotCosts[r], state.materials)) {
      if (r === 0 && state.robots[0] >= blueprint.maxOreReq) continue;
      if (r === 1 && state.robots[1] >= blueprint.robotCosts[2][1]) continue;
      if (r === 2 && state.robots[2] >= blueprint.robotCosts[3][2]) continue;

      const stateCopy: State = {
        robots: [...state.robots],
        materials: buyRobot(blueprint.robotCosts[r], newMaterials),
      };
      stateCopy.robots[r]++;
      moves.push((min: number) =>
        getMaxGeodes(blueprint, stateCopy, time - 1, cache, min)
      );
      if (r === 3) break;
    } else {
      canBuyAllRobots = false;
    }
  }

  // don't buy anything
  if (!canBuyAllRobots) {
    moves.push((min: number) =>
      getMaxGeodes(
        blueprint,
        { ...state, materials: newMaterials },
        time - 1,
        cache,
        min
      )
    );
  }

  let bestSoFar = -Infinity;
  for (const move of moves) {
    const res = move(bestSoFar);
    bestSoFar = Math.max(bestSoFar, res);
  }
  cache.set(h, bestSoFar);
  return bestSoFar;
};

const startingState: State = {
  materials: [0, 0, 0, 0],
  robots: [1, 0, 0, 0],
};

/**
 * Part I
 */

console.time("Total runtime");
let qualityLevels = [];
for (const blueprint of blueprints) {
  console.time("Runtime " + blueprint.id);
  const geos = getMaxGeodes(blueprint, copy(startingState), TIME, new Map(), -1);
  console.timeEnd("Runtime " + blueprint.id);
  qualityLevels.push(geos);
  console.log(`Blueprint #${blueprint.id}: ${geos} geodes.`);
}

console.timeEnd("Total runtime");
console.log(qualityLevels);
console.log(
  "Part 1:",
  qualityLevels.reduce((a, b, idx) => a + b * (idx + 1), 0)
);

/**
 * Part II
 */

// const cheat = [10, 54, 37];
// let longGeodes = [];
// for (const blueprint of blueprints.slice(0, 3)) {
//   console.time("Runtime " + blueprint.id);
//   const g = getMaxGeodes(
//     blueprint,
//     copy(startingState),
//     TIME,
//     new Map(),
//     cheat[longGeodes.length] - 1
//   );
//   console.log();
//   longGeodes.push(g);
//   console.timeEnd("Runtime " + blueprint.id);
//   console.log(blueprint.id + ": " + g);
// }

// console.log(
//   "\nPart 2:",
//   longGeodes.reduce((a, b) => a * b, 1)
// );

// 10 54 37 19980
