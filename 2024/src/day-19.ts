const parseInput = (input: string) => {
  const [top, bottom] = input.trim().split("\n\n");
  return { towels: top.split(", "), requests: bottom.split("\n") };
};

const cache = new Map<string, number>();

const countWaysToMake = (
  request: string,
  towels: string[],
  opts?: { justOne: boolean }
): number => {
  if (request.length === 0) return 1;
  if (cache.has(request)) return cache.get(request)!;
  let waysToMake = 0;
  for (const towel of towels) {
    if (!request.startsWith(towel)) continue;
    waysToMake += countWaysToMake(request.slice(towel.length), towels);
    if (waysToMake > 0 && opts?.justOne) break;
  }
  cache.set(request, waysToMake);
  return waysToMake;
};

export function part1(input: string) {
  cache.clear();
  const { towels, requests } = parseInput(input);
  return requests.filter((req) =>
    countWaysToMake(req, towels, { justOne: true })
  ).length;
}

export function part2(input: string) {
  cache.clear();
  const { towels, requests } = parseInput(input);
  return requests
    .map((way) => countWaysToMake(way, towels))
    .reduce((a, b) => a + b, 0);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-19.txt").text();
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
