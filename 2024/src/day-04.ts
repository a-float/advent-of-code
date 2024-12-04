function parseInput(input: string) {
  const core = input
    .replaceAll("\r", "")
    .split("\n")
    .map((line) => line.split(""));
  const padRow = Array(core[0].length + 2).fill(".");
  return [padRow, ...core.map((row) => [".", ...row, "."]), [...padRow]];
}

// prettier-ignore
const dirs = [[-1, 0], [1, 0], [0, -1], [0, 1], [1, 1], [-1, -1], [1, -1], [-1, 1]];

function checkSurrounding(data: string[][], xStart: number, yStart: number) {
  if (data[yStart][xStart] !== "X") return 0;
  const okDirs = dirs.filter((dir) => {
    let x = xStart;
    let y = yStart;
    for (let i = 0; i < 3; i++) {
      x += dir[0];
      y += dir[1];
      if (data[y][x] !== "MAS"[i]) return false;
    }
    return true;
  });
  return okDirs.length;
}

export function part1(input: string) {
  const data = parseInput(input);
  let total = 0;
  for (let y = 1; y < data.length - 1; y++) {
    for (let x = 1; x < data[y].length - 1; x++) {
      total += checkSurrounding(data, x, y);
    }
  }
  return total;
}

function checkCross(data: string[][], xStart: number, yStart: number) {
  if (data[yStart][xStart] !== "A") return false;
  const asc = [data[yStart + 1][xStart - 1], data[yStart - 1][xStart + 1]]
    .toSorted()
    .join("");

  const desc = [data[yStart - 1][xStart - 1], data[yStart + 1][xStart + 1]]
    .toSorted()
    .join("");

  return asc === "MS" && desc === "MS";
}

export async function part2(input: string) {
  const data = parseInput(input);
  let total = 0;
  for (let y = 1; y < data.length - 1; y++) {
    for (let x = 1; x < data[y].length - 1; x++) {
      if (checkCross(data, x, y)) total += 1;
    }
  }
  return total;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-04.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
