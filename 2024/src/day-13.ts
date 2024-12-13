type Point = { x: bigint; y: bigint };

type Machine = {
  A: Point;
  B: Point;
  prize: Point;
};

const getMachines = (input: string) =>
  input
    .trim()
    .split("\n\n")
    .map((block) => {
      const makePoint = (re: RegExp) =>
        block
          .match(re)!
          .slice(1)
          .map(BigInt)
          .reduce(
            (acc, n, i) => ({ ...acc, [i === 0 ? "x" : "y"]: n }),
            {} as Point
          );

      return {
        A: makePoint(/A: X\+(\d+), Y\+(\d+)/),
        B: makePoint(/B: X\+(\d+), Y\+(\d+)/),
        prize: makePoint(/Prize: X=(\d+), Y=(\d+)/),
      };
    });

const getFewestTokens = (m: Machine) => {
  const { x: X, y: Y } = m.prize;
  const b = (m.A.x * Y - m.A.y * X) / (m.A.x * m.B.y - m.A.y * m.B.x);
  const a = (X - b * m.B.x) / m.A.x;
  const sumsUp = a * m.A.x + b * m.B.x === X && a * m.A.y + b * m.B.y === Y;
  return a >= 0 && b >= 0 && sumsUp ? a * BigInt(3) + b : BigInt(0);
};

export function part1(input: string) {
  return getMachines(input)
    .map(getFewestTokens)
    .reduce((a, b) => a + b, BigInt(0))
    .toString();
}

export function part2(input: string) {
  const MULT = BigInt(1e13);
  return getMachines(input)
    .map((m) => ({
      ...m,
      prize: { x: m.prize.x + MULT, y: m.prize.y + MULT },
    }))
    .map(getFewestTokens)
    .reduce((a, b) => a + b, BigInt(0))
    .toString();
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-13.txt").text();
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
