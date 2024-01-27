import * as fs from "fs";

const text = fs.readFileSync("../data/data3.txt", {
  encoding: "utf8",
  flag: "r",
});
const elves = text
  .split("\r\n")
  .map((x) => x.split(/[,-]/).map((e) => parseInt(e)));

const p1 = elves.filter(
  ([a, b, c, d]) => (a >= c && b <= d) || (c >= a && d <= b)
).length;

const p2 = elves.filter(
  ([a, b, c, d]) =>
    (a >= c && a <= d) ||
    (b >= c && b <= d) ||
    (c >= a && c <= b) ||
    (d >= a && d <= b)
).length;

console.log("Part 1:", p1);
console.log("Part 2:", p2);
