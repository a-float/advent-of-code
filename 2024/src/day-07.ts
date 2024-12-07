const parseInput = (input: string) =>
  input.split("\n").map((line) => {
    const split = line.split(": ");
    return {
      target: Number(split[0]),
      revNums: split[1].trim().split(" ").map(Number).toReversed(),
    };
  });

function canCombine(
  target: number,
  nums: number[],
  opts?: { withConcat: boolean }
): boolean {
  if (nums.length === 1) return target === nums[0];
  const [last, ...rest] = nums;
  if (target >= last && canCombine(target - last, rest, opts)) return true;
  if (target % last === 0 && canCombine(target / last, rest, opts)) return true;

  if (opts?.withConcat) {
    const tens = Math.pow(10, Math.floor(Math.log10(last) + 1));
    const diff = target - last;
    if (diff % tens === 0 && canCombine(diff / tens, rest, opts)) return true;
  }
  return false;
}

export function part1(input: string) {
  return parseInput(input)
    .filter(({ target, revNums }) => canCombine(target, revNums))
    .reduce((total, { target }) => total + target, 0);
}

export function part2(input: string) {
  return parseInput(input)
    .filter(({ target, revNums }) =>
      canCombine(target, revNums, { withConcat: true })
    )
    .reduce((total, { target }) => total + target, 0);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-07.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
