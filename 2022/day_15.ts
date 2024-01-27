import * as fs from "fs";
import * as os from "os";

interface Sensor {
  x: number;
  y: number;
  range: number;
}

const beacons = new Set<string>();

const manhattan = (x1: number, y1: number, x2: number, y2: number): number =>
  Math.abs(x1 - x2) + Math.abs(y1 - y2);

const sensors: Sensor[] = fs
  .readFileSync("../data/data15.txt", { encoding: "utf8", flag: "r" })
  .trim()
  .split(os.EOL)
  .map((x) =>
    [...x.matchAll(/.*x=(-?\d+).*y=(-?\d+).*x=(-?\d+).*y=(-?\d+)/g)][0]
      .slice(1, 5)
      .map((x) => parseInt(x))
  )
  .map((m) => {
    beacons.add(m[2] + ":" + m[3]);
    return {
      x: m[0],
      y: m[1],
      range: manhattan(m[0], m[1], m[2], m[3]),
    };
  });

// const Y = 10;
const Y = 2e6;
let p1 = 0;
// for (let x = -1e8; x < 1e8; x++) {
//   for (const sensor of sensors) {
//     if (
//       manhattan(x, Y, sensor.x, sensor.y) <= sensor.range &&
//       !beacons.has(`${x}:${Y}`)
//     ) {
//       p1++;
//       break;
//     }
//   }
// }
// too slow
// const B = 20;
const B = 4000000;
let x = 0;
let y = B;
while (y >= 0) {
  while (x < B) {
    let canBe = true;
    for (const sensor of sensors) {
      const dist = manhattan(x, y, sensor.x, sensor.y);
      if (dist <= sensor.range) {
        canBe = false;
        const diff = sensor.range - Math.abs(sensor.y - y);
        // console.log("jumping ", sensor.x + diff - x, y, x);
        x = sensor.x + diff;
        break;
      }
    }
    if (canBe) {
      console.log("Solution: ", x, y, x * 4000000 + y); // 12567351400528 85.013225%
      process.exit();
    }
    x++;
  }
  console.log(y / B);
  y--;
  x = 0;
}
// console.log("Part 2:", p2);`
// console.log("Part 1:", p1);
