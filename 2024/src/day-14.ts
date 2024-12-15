type Robot = {
  x: number;
  y: number;
  vx: number;
  vy: number;
};

const WIDTH = 101;
const HEIGHT = 103;

const loadRobots = (input: string): Robot[] =>
  input.split("\n").map((line) => {
    const [_, x, y, vx, vy] = line
      .match(/^p=(.+),(.+) v=(.+),(.+)$/)!
      .map(Number);
    return { x, y, vx, vy };
  });

const mod = (a: number, b: number) => ((a % b) + b) % b;

const simulateRobot = (robot: Robot, steps: number): Robot => {
  return {
    ...robot,
    x: mod(robot.x + robot.vx * steps, WIDTH),
    y: mod(robot.y + robot.vy * steps, HEIGHT),
  };
};

const getRobotMap = (robots: Robot[]): string => {
  const map = Array.from({ length: HEIGHT }, () =>
    Array.from({ length: WIDTH }, () => " ")
  );
  robots.forEach(({ x, y }) => {
    map[y][x] = "#";
  });
  return map.map((row) => row.join("")).join("\n");
};

const calculateSafetyFactor = (robots: Robot[]): number => {
  let [q1, q2, q3, q4] = [0, 0, 0, 0];
  const [edgeX, edgeY] = [Math.floor(WIDTH / 2), Math.floor(HEIGHT / 2)];
  for (const robot of robots) {
    if (robot.x < edgeX && robot.y < edgeY) q1++;
    if (robot.x > edgeX && robot.y < edgeY) q2++;
    if (robot.x < edgeX && robot.y > edgeY) q3++;
    if (robot.x > edgeX && robot.y > edgeY) q4++;
  }
  return q1 * q2 * q3 * q4;
};

export function part1(input: string) {
  const robots = loadRobots(input).map((robot) => simulateRobot(robot, 100));
  return calculateSafetyFactor(robots);
}

export async function part2(input: string) {
  let robots = loadRobots(input);
  const visited = new Set<string>();
  for (let i = 0; i <= 1e6; i++) {
    const robotString = JSON.stringify(robots);
    if (visited.has(robotString)) break;
    visited.add(robotString);
    const map = getRobotMap(robots);
    if (map.includes("#".repeat(20))) return i;
    robots = robots.map((robot) => simulateRobot(robot, 1));
  }
  return -1;
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-14.txt").text();
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
