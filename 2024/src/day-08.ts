type Point = [number, number]; // [x, y]

function parseInput(input: string) {
  const antenas = new Map<string, Point[]>();
  const lines = input.trim().split("\n");
  lines.forEach((line, y) =>
    [...line].forEach((char, x) => {
      if (char === "." || char === "#") return;
      antenas.set(char, [...(antenas.get(char) ?? []), [x, y]]);
    })
  );
  const width = lines[0].length;
  const height = lines.length;
  return {
    antenas,
    inBounds: (a: Point) =>
      a[0] >= 0 && a[0] < width && a[1] >= 0 && a[1] < height,
  };
}

export function part1(input: string) {
  const { antenas, inBounds } = parseInput(input);
  const unique = new Set<string>();
  for (const places of antenas.values()) {
    places.forEach((p, i) => {
      places.slice(i + 1).forEach((op) => {
        const [dx, dy] = [op[0] - p[0], op[1] - p[1]];
        const antinode1: Point = [op[0] + dx, op[1] + dy];
        const antinode2: Point = [p[0] - dx, p[1] - dy];
        [antinode1, antinode2]
          .filter(inBounds)
          .forEach((a) => unique.add(JSON.stringify(a)));
      });
    });
  }
  return unique.size;
}

export function part2(input: string) {
  const { antenas, inBounds } = parseInput(input);
  const unique = new Set<string>();
  for (const places of antenas.values()) {
    places.forEach((p, i) => {
      places.slice(i + 1).forEach((op) => {
        const [dx, dy] = [op[0] - p[0], op[1] - p[1]];
        let pos: Point = [...p];
        while (inBounds(pos)) {
          unique.add(JSON.stringify(pos));
          pos = [pos[0] + dx, pos[1] + dy];
        }
        pos = [...p];
        while (inBounds(pos)) {
          unique.add(JSON.stringify(pos));
          pos = [pos[0] - dx, pos[1] - dy];
        }
      });
    });
  }
  return unique.size;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-08.txt").text();
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
