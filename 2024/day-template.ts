export const part1 = (input: string) => 1;

export const part2 = (input: string) => 2;

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-dayNumber.txt").text();
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
