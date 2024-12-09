const inputToMemory = (input: string) => {
  const expanded: (string | number)[] = [];
  let free = false;
  let id = 0;
  for (const char of input) {
    const num = parseInt(char);
    if (free) expanded.push(...new Array(num).fill("."));
    else {
      expanded.push(...new Array(num).fill(id));
      id++;
    }
    free = !free;
  }
  return expanded;
};

const checksum = (memory: (string | number)[]) =>
  memory.reduce<number>(
    (acc, x, i) => (typeof x === "number" ? acc + x * i : acc),
    0
  );

export function part1(input: string) {
  const expanded = inputToMemory(input);
  let left = 0;
  let right = expanded.length - 1;
  while (left < right) {
    if (expanded[left] !== ".") left++;
    else if (expanded[right] === ".") right--;
    else {
      const tmp = expanded[left];
      expanded[left] = expanded[right];
      expanded[right] = tmp;
      left++;
      right--;
    }
  }
  return checksum(expanded);
}

export function part2(input: string) {
  const expanded = inputToMemory(input);
  let right = expanded.length - 1;
  const findFreeSpace = (space: number, limit: number) => {
    let combo = 0;
    for (let i = 0; i < limit; i++) {
      if (expanded[i] === ".") {
        combo++;
        if (combo == space) return i - combo + 1;
      } else {
        combo = 0;
      }
    }
    return -1;
  };

  while (right >= 0) {
    while (expanded[right] === ".") right--;
    const blockEnd = right;
    while (expanded[right] === expanded[blockEnd]) right--;
    const blockSize = blockEnd - right;
    right++;
    const dest = findFreeSpace(blockSize, right);
    if (dest >= 0) {
      for (let i = 0; i < blockSize; i++) {
        expanded[dest + i] = expanded[right + i];
        expanded[right + i] = ".";
      }
    }
    right--;
  }

  return checksum(expanded);
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-09.txt").text();
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
