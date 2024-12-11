const transform = (value: number): number[] => {
  if (value === 0) return [1];
  if (value >= 10) {
    const log = Math.floor(Math.log10(value));
    const ten = Math.pow(10, (log + 1) / 2);
    if (log % 2 === 1) return [Math.floor(value / ten), value % ten];
  }
  return [value * 2024];
};

const fastBlink = (input: string, blinks: number): number => {
  let stoneMap = new Map<number, number>();
  input
    .split(" ")
    .map(Number)
    .forEach((num) => stoneMap.set(num, (stoneMap.get(num) ?? 0) + 1));

  for (let i = 0; i < blinks; i++) {
    const tmpMap = new Map<number, number>();
    [...stoneMap.entries()].forEach(([key, val]) =>
      transform(key).forEach((num) =>
        tmpMap.set(num, (tmpMap.get(num) ?? 0) + val)
      )
    );
    stoneMap = tmpMap;
  }

  return [...stoneMap.values()].reduce((acc, val) => acc + val, 0);
};

function part1(input: string) {
  return fastBlink(input, 25);
}

function part2(input: string) {
  return fastBlink(input, 75);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-11.txt").text();
  const time = async (fn: Function) => {
    const start = performance.now();
    const res = await fn();
    const end = performance.now();
    process.stdout.write(`(${(end - start).toFixed(3)}ms) `);
    return res;
  };
  console.log("Part 1:", await time(() => part1(input)));
  console.log("Part 2:", await time(() => part2(input)));
}
