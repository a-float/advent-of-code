function parseInput(input: string) {
  return input.matchAll(/(?:don't)|(?:do)|(?:mul\((\d{1,3}),(\d{1,3})\))/g);
}

export function part1(input: string) {
  return Array.from(parseInput(input))
    .filter((op) => op[0].startsWith("mul"))
    .map((m) => Number(m[1]) * Number(m[2]))
    .reduce((a, b) => a + b, 0);
}

export async function part2(input: string) {
  let total = 0;
  let work = true;
  for (const op of parseInput(input)) {
    if (op[0].startsWith("mul") && work) total += Number(op[1]) * Number(op[2]);
    else if (op[0].startsWith("don't")) work = false;
    else if (op[0].startsWith("do")) work = true;
  }
  return total;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-03.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
