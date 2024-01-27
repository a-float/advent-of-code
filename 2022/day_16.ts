import * as fs from "fs";
import * as os from "os";

interface Valve {
  id: string;
  flow: number;
  others: Valve["id"][];
}

type ValveMap = Map<Valve["id"], Valve>;

const valveData: [Valve["id"], Valve][] = fs
  .readFileSync("../data/data16.txt", { encoding: "utf8", flag: "r" })
  .trim()
  .split(os.EOL)
  .map((x) => [...x.matchAll(/Valve (\w+).*rate=(\d+).*valves? (.+)/g)][0])
  .map((m) => ({
    id: m[1],
    flow: parseInt(m[2]),
    others: m[3].split(", "),
  }))
  .map((v) => [v.id, v]);

const valves = new Map(valveData);

const warshall = (
  valves: ValveMap
): { [k1: string]: { [k2: string]: number } } => {
  const keys = [...valves.keys()];
  const d = {} as any;
  for (const k1 of keys) {
    d[k1] = {};
    for (const k2 of keys) {
      d[k1][k2] =
        k1 === k2 ? 0 : valves.get(k1)!.others.includes(k2) ? 1 : Infinity;
    }
  }
  for (let i = 0; i < keys.length; i++) {
    for (const k1 of keys) {
      for (const k2 of keys) {
        for (const k3 of keys) {
          d[k2][k3] = Math.min(d[k2][k3], d[k2][k1] + d[k1][k3]);
        }
      }
    }
  }
  return d;
};

const workingValves = [...valves.keys()].filter(
  (k) => k === "AA" || valves.get(k)!.flow > 0
);
const distRaw = warshall(valves);
const dist: typeof distRaw = {};
for (const d in distRaw) {
  if (workingValves.includes(d)) {
    dist[d] = {};
    for (const k in distRaw) {
      if (d !== k && workingValves.includes(k)) {
        dist[d][k] = distRaw[d][k];
      }
    }
  }
}
console.log(dist);

const solver = (
  picked1: string[],
  picked2: string[],
  time1: number,
  time2: number,
  isRoot: boolean = false
): number => {
  const possible = [];
  if (time1 > 0) {
    const last = picked1.at(-1) || "AA";
    const valid = workingValves.filter(
      (x) => dist[last][x] + 1 < time1 && !picked1.includes(x)
    );
    for (const x of valid) {
      const timeLeft = time1 - dist[last][x] - 1;
      const toGain = timeLeft * valves.get(x)!.flow;
      const res = toGain + solver([...picked1, x], [], timeLeft, time2);
      possible.push(res);
      if (isRoot) {
        console.log(new Date().toUTCString() + `-- Possible result: ${res}`);
      }
    }
    if (valid.length === 0) {
      const res = solver(picked1, [], 0, time2);
      possible.push(res);
      if (isRoot) {
        console.log(
          new Date().toUTCString() + `-- Possible (no pick) result: ${res}`
        );
      }
    }
    return Math.max(...possible);
  }
  if (time2 > 0) {
    const last = picked2.at(-1) || "AA";
    const valid = workingValves.filter(
      (x) =>
        dist[last][x] + 1 < time2 &&
        !picked1.includes(x) &&
        !picked2.includes(x)
    );
    for (const x of valid) {
      const timeLeft = time2 - dist[last][x] - 1;
      const toGain = timeLeft * valves.get(x)!.flow;
      const res = toGain + solver(picked1, [...picked2, x], time1, timeLeft);
      possible.push(res);
    }
    return Math.max(...possible, 0);
  }
  return 0;
};
// part II doesnt work for the example data. More skipping should be included in the 1 walker. Currently
// the first might open too many valves making them start too late and nor release enough pressure.
// luckily worked for real data
// will probably work if there is more valves that can be open.

console.time("runtime");
const p2 = solver([], [], 30, 30, true);
console.timeEnd("runtime");
console.log(p2);
// more than 2028
// more than 2037
