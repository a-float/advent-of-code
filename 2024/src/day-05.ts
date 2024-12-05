function parseInput(input: string) {
  const [top, down] = input.replaceAll("\r", "").split("\n\n");
  const rules = new Map();
  // key comes after all its values
  for (const line of top.split("\n")) {
    const [before, after] = line.split("|").map(Number);
    rules.set(after, [...(rules.get(after) ?? []), before]);
  }
  const updates = down.split("\n").map((line) => line.split(",").map(Number));
  return [rules, updates] as const;
}

const isValid = (rules: Map<number, number[]>, update: number[]) =>
  update.every((u, i) =>
    update.slice(0, i).every((p) => rules.get(u)?.includes(p))
  );

export function part1(input: string) {
  const [rules, updates] = parseInput(input);
  const correctUpdates = updates.filter((update) => isValid(rules, update));
  return correctUpdates
    .map((arr) => arr[arr.length >> 1])
    .reduce((a, b) => a + b, 0);
}

const fixUpdate = (rules: Map<number, number[]>, update: number[]) => {
  const fixed = [];
  let unused = [...update];
  for (let i = 0; i < update.length; i++) {
    const found = unused.find((u) =>
      unused.every((l) => !rules.get(u)?.includes(l))
    )!;
    fixed.push(found);
    unused = unused.filter((u) => u != found);
  }
  return fixed;
};

export async function part2(input: string) {
  const [rules, updates] = parseInput(input);
  const fixedUpdates = updates
    .filter((update) => !isValid(rules, update))
    .map((update) => fixUpdate(rules, update));

  return fixedUpdates
    .map((arr) => arr[arr.length >> 1])
    .reduce((a, b) => a + b, 0);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-05.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
