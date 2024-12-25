const parseBlock = (block: string) => {
  const lines = block.split("\n").map((line) => line.split(""));
  const cols = [...new Array(lines[0].length)].map(
    (_, c) =>
      [...new Array(lines.length)].filter((_, r) => lines[r][c] === "#").length
  );

  return lines[0].every((c) => c === "#")
    ? ({ type: "lock", cols } as const)
    : ({ type: "key", cols } as const);
};

export const part1 = (input: string) => {
  const blocks = input.split("\n\n");
  const parsed = blocks.map(parseBlock);
  const keys = parsed.filter((b) => b.type === "key");
  const locks = parsed.filter((b) => b.type === "lock");

  const matches = keys.reduce(
    (fits, key) =>
      fits +
      locks.filter((lock) => key.cols.every((c, i) => c + lock.cols[i] <= 7))
        .length,
    0
  );

  return matches;
};

export const part2 = (input: string) => "Merry Christmas!";

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-25.txt").text();
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
