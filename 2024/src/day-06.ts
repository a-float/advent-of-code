function parseInput(input: string) {
  const obstacles = new Set<string>();
  const lines = input.replaceAll("\r", "").split("\n");
  let start = [-1, -1];
  lines.forEach((line, y) => [...line].forEach((char, x) => {
    if (char === ".") return
    if (char === "#") obstacles.add(JSON.stringify([x, y]))
    if (char === "^") start = [x, y]
  }));
  return { obstacles, width: lines[0].length, height: lines.length, start };
}


const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];

const simulateGuard = (map: ReturnType<typeof parseInput>, opts?: { trackDir: boolean }) => {
  const { obstacles, width, height, start } = map;
  const visited = new Set<string>();

  let dir = 0;
  let pos = start;
  while (pos[0] >= 0 && pos[0] < width && pos[1] >= 0 && pos[1] < height) {
    const hash = JSON.stringify({ pos, ...(opts?.trackDir ? { dir } : {}) });
    if (opts?.trackDir && visited.has(hash)) {
      return { path: visited, loop: true };
    }
    visited.add(hash);
    let next = [pos[0] + dirs[dir][0], pos[1] + dirs[dir][1]];
    if (obstacles.has(JSON.stringify(next))) {
      dir = (dir + 1) % dirs.length
    } else {
      pos = next
    }
  }
  return { path: visited, loop: false }
}

export function part1(input: string) {
  return simulateGuard(parseInput(input)).path.size;
}

export function part2(input: string) {
  const map = parseInput(input);
  const guard = simulateGuard(map);
  let total = 0;
  for (const p of [...guard.path].slice(1)) {
    const pos = JSON.parse(p).pos;
    const hash = JSON.stringify(pos);

    map.obstacles.add(hash);
    const sim = simulateGuard(map, { trackDir: true });
    if (sim.loop) total += 1;
    map.obstacles.delete(hash);
  }
  return total;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-06.txt").text();
  console.log("Part 1:", await part1(input));
  console.log("Part 2:", await part2(input));
}
