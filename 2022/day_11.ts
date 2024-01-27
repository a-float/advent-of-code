import * as fs from "fs";
import * as os from "os";

interface Monkey {
  items: number[];
  op: (a: number) => number;
  test: number;
  successMonkey: number;
  failureMonkey: number;
  inspects: number;
}

const parseMonkey = (inp: string): Monkey => {
  const lines = inp.split(os.EOL);
  const items = lines[1]
    .split(":")[1]
    .trim()
    .split(", ")
    .map((x) => parseInt(x));
  const ops = lines[2].split(" ").slice(-3);
  // console.log(ops);
  const op = (old: number) => eval(ops.join(" ")); // used eval lol
  return {
    items,
    op,
    test: parseInt(lines[3].split(" ").at(-1)!),
    successMonkey: parseInt(lines[4].split(" ").at(-1)!),
    failureMonkey: parseInt(lines[5].split(" ").at(-1)!),
    inspects: 0,
  };
};

const runMonkey = (monkey: Monkey, monkeys: Monkey[], worryMod: number) => {
  for (const item of monkey.items) {
    // const worry = Math.floor(monkey.op(item) / 3);
    const worry = monkey.op(item) % worryMod;
    monkeys[
      worry % monkey.test === 0 ? monkey.successMonkey : monkey.failureMonkey
    ].items.push(worry);
  }
  monkey.inspects += monkey.items.length;
  monkey.items.length = 0;
};

const monkeyStrings: string[] = fs
  .readFileSync("../data/data11.txt", { encoding: "utf8", flag: "r" })
  .trim()
  .split(os.EOL + os.EOL);

const monkeys = monkeyStrings.map((m) => parseMonkey(m));
const worryMod = monkeys.map((m) => m.test).reduce((a, b) => a * b, 1); // all are primes

console.time("executionTime");
for (let i = 0; i < 1e4; i++) {
  monkeys.forEach((m) => runMonkey(m, monkeys, worryMod));
}
monkeys.sort((a, d) => d.inspects - a.inspects);
console.timeEnd("executionTime");

console.log("Part 1: idk change runMonkey() to divide by 3");
console.log("Part 2:", monkeys[0].inspects * monkeys[1].inspects);
