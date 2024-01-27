const fs = require("fs");
const os = require("os");

const lines = fs
  .readFileSync("data10.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL);

const states = [1];
for (const line of lines) {
  const regX = states.at(-1);
  const [comm, val] = line.split(" ");
  if (comm === "noop") states.push(regX);
  else if (comm === "addx") {
    states.push(regX);
    states.push(regX + parseInt(val));
  }
}

const keyCycles = [...Array.from({ length: 6 })].map((_, i) => 20 + i * 40);
const p1 = keyCycles.reduce((acc, cycle) => acc + states[cycle - 1] * cycle, 0);
console.log("Part 1:", p1);

const img = [];
for (let i = 0; i < 6 * 40; i++) {
  if(i > 0 && i % 40 === 0)img.push("\n")
  img.push(Math.abs(states[i] - i % 40) < 2 ? "#" : " ");
}
console.log("Part 2:");
console.log(img.join(""));
