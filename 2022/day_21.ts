import * as fs from "fs";
import * as os from "os";

type Monkey =
  | {
      name: string;
      value: number;
    }
  | {
      name: string;
      left: string;
      right: string;
      sign: "+" | "-" | "*" | "/" | "==";
      op: (a: number, b: number) => number;
    };

const getOp: (sign: string) => (a: number, b: number) => number = function (
  sign
) {
  switch (sign) {
    case "+":
      return (a: number, b: number) => a + b;
    case "-":
      return (a: number, b: number) => a - b;
    case "*":
      return (a: number, b: number) => a * b;
    case "/":
      return (a: number, b: number) => a / b;
  }
  throw new Error("Invalid sign: " + sign);
};

const monkeys: Map<string, Monkey> = new Map(
  fs
    .readFileSync("../data/data21.txt", { encoding: "utf-8" })
    .trim()
    .split(os.EOL)
    .map((x) => {
      const words = x.split(/\s+/);
      const name = words[0].slice(0, -1);
      if (words.length === 2) {
        return { name, value: parseInt(words[1]) };
      }
      return {
        name,
        left: words[1],
        right: words[3],
        sign: words[2] as any,
        op: getOp(words[2]),
      };
    })
    .map((x) => [x.name, x])
);

const evalMonkey = (name: string, monkeys: Map<string, Monkey>): number => {
  const m = monkeys.get(name)!;
  return "value" in m
    ? m.value
    : m.op(evalMonkey(m.left, monkeys), evalMonkey(m.right, monkeys));
};

console.time("Runtime 1");
console.log("Part 1: " + evalMonkey("root", monkeys));
console.timeEnd("Runtime 1");

// part 2

const dfs = (
  current: string,
  target: string,
  monkeys: Map<string, Monkey>,
  visited: string[] = []
): string[] => {
  const m = monkeys.get(current)!;
  if (m.name === target) return [...visited, current];
  if ("value" in m) return [];
  else {
    return [m.left, m.right].flatMap((x) =>
      dfs(x, target, monkeys, [...visited, current])
    );
  }
};

const forceValue = (
  name: string,
  child: string,
  value: number,
  monkeys: Map<string, Monkey>
): number => {
  const m = monkeys.get(name)!;
  if (!("op" in m)) return value;
  const independent = m.left === child ? m.right : m.left;
  const otherValue = evalMonkey(independent, monkeys);
  switch (m.sign) {
    case "+":
      return value - otherValue;
    case "*":
      return value / otherValue;
    case "-":
      return m.left === child ? value + otherValue : otherValue - value;
    case "/":
      return m.left === child ? value * otherValue : otherValue / value;
    case "==":
      return otherValue;
  }
};

console.time("Runtime 2")
const meRoute = dfs("root", "humn", monkeys);
const root = monkeys.get("root")!;
if (!("op" in root)) throw new Error("Root wtf");
root.sign = "==";
root.op = (a: number, b: number) => (a == b ? 1 : 0);

let forcedValue = 1;
for (let i = 0; i < meRoute.length - 1; i++) {
  forcedValue = forceValue(meRoute[i], meRoute[i + 1], forcedValue, monkeys);
}
console.log(forcedValue);
console.timeEnd("Runtime 2")

