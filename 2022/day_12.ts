import * as fs from "fs";
import * as os from "os";

type Pos = [number, number];

class Heightmap {
  map: string[];
  start: Pos;
  end: Pos;
  constructor(map: string[]) {
    this.map = map;
    this.start = this.findLetter("S");
    this.end = this.findLetter("E");
  }

  charAt = (pos: Pos) => this.map[pos[0]][pos[1]]
  getHeight = (pos: Pos) => this.charAt(pos) === "E" ? "z".charCodeAt(0) : this.charAt(pos).charCodeAt(0);
  distToEnd = (pos: Pos) => Math.abs(this.end[0] - pos[0]) + Math.abs(this.end[1] - pos[1])

  getNeighbours = (pos: Pos) =>
    (
      [
        [pos[0] + 1, pos[1]],
        [pos[0] - 1, pos[1]],
        [pos[0], pos[1] - 1],
        [pos[0], pos[1] + 1],
      ] as Pos[]
    ).filter(
      (p) =>
        p[0] >= 0 &&
        p[0] < this.map.length &&
        p[1] >= 0 &&
        p[1] < this.map[0].length
    );

  private findLetter = (letter: string): [number, number] =>
    this.map
      .map((x, i) => (x.indexOf(letter) >= 0 ? [i, x.indexOf(letter)] : null))
      .reduce((a, b) => a || b, null) as [number, number];
}

const map: string[] = fs
  .readFileSync("data12.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL);

const heightmap = new Heightmap(map);

const dijkstra = (queue: { pos: Pos, dist: number }[], map: Heightmap) => {
  const visited = new Set<string>()
  while (queue.length > 0) {
    const current = queue.shift()!;
    if (visited.has(JSON.stringify(current.pos))) continue

    if (map.charAt(current.pos) === "E") {
      console.log("Found: ", current);
      return current.dist;
    }
    const h = map.getHeight(current.pos);
    const neigh = map.getNeighbours(current.pos);
    neigh.sort((a, d) => map.distToEnd(a) - map.distToEnd(d))
    neigh
      .filter((n) => map.charAt(current.pos) === "S" || map.getHeight(n) <= h + 1)
      .forEach((n) => {
        visited.add(JSON.stringify(current.pos))
        queue.push({ pos: n, dist: current.dist + 1 })
      });
    queue.sort((a, d) => a.dist + map.distToEnd(a.pos) - d.dist - map.distToEnd(d.pos))
  }
  return Infinity;
};

// visited as set is 6.5x faster than list
const res: number[] = []
console.time("djkstraTime")
const p1 = dijkstra([{ pos: heightmap.start, dist: 0 }], heightmap)
console.log("Part I:  ", p1);
console.timeEnd("djkstraTime")


console.time("bruteForceSearch")
for (let y = 0; y < heightmap.map.length; y++) {
  for (let x = heightmap.map[0].length - 1; x >= 0; x--) {
    const pos = [y, x] as Pos;
    if (heightmap.charAt(pos) === "a") {
      console.log(res.length+1 + ". " + pos);
      const dist = dijkstra([{ pos, dist: 0 }], heightmap)
      res.push(dist)
    }
  }
}
console.timeEnd("bruteForceSearch")
console.log("Part II: ", Math.min(...res));
