function parseInput(input: string) {
  return input.split("\n").map((line) => line.split(" ").map(Number));
}

function isSafe(nums: number[]) {
  const diffs = nums.flatMap((n, i, arr) => (i === 0 ? [] : [n - arr[i - 1]]));
  if (diffs.some((d) => Math.abs(d) > 3 || d === 0)) return false;
  if (diffs.some((d) => d < 0) && diffs.some((d) => d > 0)) return false;
  return true;
}

function isSafe2(nums: number[]) {
  if (isSafe(nums)) return true;
  for (let i = 0; i < nums.length; i++) {
    const cp = [...nums.slice(0, i), ...nums.slice(i + 1)];
    if (isSafe(cp)) return true;
  }
  return false;
}

export async function part1(input: string) {
  return parseInput(input).filter(isSafe).length;
}

export async function part2(input: string) {
  return parseInput(input).filter(isSafe2).length;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-02.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
