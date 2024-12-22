const STEPS = 2000;
const MOD = BigInt((1 << 24) - 1);

const hash = (secret: bigint) => {
  secret = (secret ^ (secret << 6n)) & MOD;
  secret = (secret ^ (secret >> 5n)) & MOD;
  secret = (secret ^ (secret << 11n)) & MOD;
  return secret;
};

export function part1(input: string) {
  const seeds = input.split("\n").map(BigInt);
  let secretTotal = 0n;
  seeds.map((seed) => {
    let secret = seed;
    for (let i = 0; i < STEPS; i++) secret = hash(secret);
    secretTotal += secret;
  });
  return secretTotal.toString();
}

export function part2(input: string) {
  const seeds = input.split("\n").map(BigInt);
  const bananasPerKey = new Map<string, number>();
  seeds.forEach((seed) => {
    const lastDigits: number[] = [];
    const diffs: number[] = [];
    const buyerKeys = new Set<string>();
    let secret = seed;
    lastDigits.push(Number(secret % 10n));
    for (let i = 0; i < STEPS; i++) {
      secret = hash(secret);
      lastDigits.push(Number(secret % 10n));
      diffs.push(lastDigits.at(-1)! - lastDigits.at(-2)!);
      const val = lastDigits.at(-1)!;
      if (diffs.length >= 4 && val > 0) {
        const key = diffs.slice(-4).join(",");
        if (buyerKeys.has(key)) continue;
        buyerKeys.add(key);
        bananasPerKey.set(key, (bananasPerKey.get(key) ?? 0) + val);
      }
    }
  });

  return Math.max(...bananasPerKey.values());
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-22.txt").text();
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
