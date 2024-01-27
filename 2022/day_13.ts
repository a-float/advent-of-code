import * as fs from "fs";
import * as os from "os";
import * as _ from "lodash";

const packets: Packet[] = fs
  .readFileSync("data13.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL)
  .filter(x => x.length > 0)
  .map(x => JSON.parse(x))

type Packet = number | Packet[]

const comparePackets = (a: Packet, b: Packet): number => {
  // console.log("comparing", a, b);

  if (typeof a === "number" && typeof b === "number") {
    return Math.sign(b - a)
  }
  if (typeof a === "object" && typeof b === "object") {
    for (let i = 0; i < Math.min(a.length, b.length); i++) {
      const comp = comparePackets(a[i], b[i])
      if (comp !== 0) return comp
    }
    return Math.sign(b.length - a.length)
  }
  if (typeof a === "number") return comparePackets([a], b)
  if (typeof b === "number") return comparePackets(a, [b])
  return 0
}

const p1 = _.chunk(packets, 2).map(([a, b], idx) => comparePackets(a, b) === 1 ? idx + 1 : 0).reduce((a, b) => a + b, 0)
console.log("Part 1: ", p1);


const dividers = [[[2]], [[6]]]
const allPackets = [...packets, ...dividers]
allPackets.sort((a, d) => -comparePackets(a, d))

const p2 = dividers.map(d => 1 + allPackets.indexOf(d)).reduce(_.multiply, 1)
console.log("Part 2: ", p2);
