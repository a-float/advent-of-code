import * as fs from "fs";
import * as os from "os";

const lines = fs
  .readFileSync("../data/data25.txt", { encoding: "utf-8" })
  .split(os.EOL);

const vals: { [k: string | number]: number } = {
  "=": -2,
  "-": -1,
  0: 0,
  1: 1,
  2: 2,
};

const invVals: { [k: number]: string | number } = {
  "-2": "=",
  "-1": "-",
  0: 0,
  1: 1,
  2: 2,
};

const snafuToDec = (s: string): number =>
  s
    .split("")
    .reduce((acc, c, i) => acc + vals[c] * Math.pow(5, s.length - i - 1), 0);

// const decToSnafu = (x: number): string => {
//   let pow = 0;
//   do {
//     console.log(pow);
//     pow++;
//   } while (Math.pow(5, pow) < x);
//   let rest = x;
//   let res: ("=" | "-" | 0 | 1 | 2)[] = [];
//   while (rest > 0) {
//     const d = Math.floor(rest / Math.pow(5, pow));
//     if (d === 3 || d === 4) {
//       const last = res.pop()!;
//       res.push(invVals[vals[last] + d ==])
//     }
//   }
// };

// const addSnufs = (a: string, b: string): string => {
//   const res = [];
//   const [long, short] = [a, b].sort((a, d) => d.length - a.length);
//   for (let i = 0; i < short.length; i++) {
//     const sum = vals[short[i]] + vals[long[i]];
//     if (sum > 0 && sum < 3) {
//       res.push(sum + "");
//     }
//     if(sum > 3){}
//   }
// };

// console.log(decToSnafu(1747));
// console.log(lines);

const reqs = lines.map((x) => snafuToDec(x));
console.log(reqs);
console.log(reqs.reduce((a, b) => a + b, 0));

// completed in a spreadsheet