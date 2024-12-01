function parseInput(input: string) {
  const left: number[] = [];
  const right: number[] = [];
  input
    .trim()
    .split("\n")
    .map((line) => {
      const [a, b] = line.split(/\s+/).map(Number);
      left.push(a);
      right.push(b);
    });
  return [left, right];
}

export async function part1(input: string) {
  const [left, right] = parseInput(input);
  left.sort();
  right.sort();
  return left.map((a, i) => Math.abs(a - right[i])).reduce((a, d) => a + d, 0);
}

export async function part2(input: string) {
  const [left, right] = parseInput(input);
  const occurrences = right.reduce(
    (acc, curr) => (acc[curr] ? ++acc[curr] : (acc[curr] = 1), acc),
    {} as Record<number, number>
  );
  return left.reduce((a, l) => a + l * (occurrences[l] || 0), 0);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-01.txt").text();
  console.log(await part1(input));
  console.log(await part2(input));
}
