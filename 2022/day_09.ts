import * as fs from "fs";
import * as os from "os";

type Vector2 = { x: number; y: number };

const commands: string[] = fs
  .readFileSync("../data/data9.txt", { encoding: "utf8", flag: "r" })
  .trim()
  .split(os.EOL);

const TAIL_LEN = 9;

const head = { x: 0, y: 0 };
let tail = [...Array.from({ length: TAIL_LEN })].map((x) => ({ ...head }));

const moveTail = (head: Vector2, tail: Vector2): Vector2 => {
  if (Math.abs(head.x - tail.x) < 2 && Math.abs(head.y - tail.y) < 2) {
    return { ...tail };
  }

  return {
    x: tail.x + Math.sign(head.x - tail.x),
    y: tail.y + Math.sign(head.y - tail.y),
  };
};

const set = new Set([JSON.stringify(tail.at(-1))]);
for (const comm of commands) {
  const [dir, len] = comm.split(/\s+/);
  for (let i = 0; i < parseInt(len); i++) {
    if (dir === "R") head.x += 1;
    if (dir === "L") head.x -= 1;
    if (dir === "U") head.y += 1;
    if (dir === "D") head.y -= 1;
    for (let j = 0; j < TAIL_LEN; j++) {
      tail[j] = moveTail(j === 0 ? head : tail[j - 1], tail[j]);
    }
    set.add(JSON.stringify(tail.at(-1)));
  }
}
console.log(set.size);

// console.log("Part 2:", maxViewScore);
