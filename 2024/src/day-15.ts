type Point = [number, number];
type Move = "<" | ">" | "^" | "v";
const dirs: Record<Move, Point> = {
  "<": [-1, 0],
  ">": [1, 0],
  "^": [0, -1],
  v: [0, 1],
};

class Warehouse {
  protected grid: string[][];

  constructor(map: string) {
    this.grid = map.split("\n").map((row) => row.split(""));
  }

  has(x: number, y: number): boolean {
    return x >= 0 && y >= 0 && y < this.grid.length && x < this.grid[y].length;
  }

  get(x: number, y: number): string {
    if (!this.has(x, y) || this.grid[y][x] === "@") return "@";
    return this.grid[y][x];
  }

  canMove(x: number, y: number, [dx, dy]: Point): boolean {
    const [nx, ny] = [x + dx, y + dy];
    if (!this.has(nx, ny)) throw new Error(`Out of bounds x=${nx}, y=${ny}`);
    const next = this.get(nx, ny);
    if (next === ".") return true;
    if (next === "O") return this.canMove(nx, ny, [dx, dy]);
    return false;
  }

  move(x: number, y: number, [dx, dy]: Point): Point {
    const [nx, ny] = [x + dx, y + dy];
    if (this.get(nx, ny) === ".") {
      this.grid[ny][nx] = this.grid[y][x];
      this.grid[y][x] = ".";
      return [nx, ny];
    }
    if (this.canMove(x, y, [dx, dy])) {
      this.move(nx, ny, [dx, dy]);
      const tmp = this.grid[y][x];
      this.grid[y][x] = this.grid[ny][nx];
      this.grid[ny][nx] = tmp;
      return [nx, ny];
    }
    return [x, y];
  }

  getRobot(): Point {
    for (let y = 0; y < this.grid.length; y++) {
      for (let x = 0; x < this.grid[y].length; x++) {
        if (this.grid[y][x] === "@") return [x, y];
      }
    }
    throw new Error("There is no robot in the warehouse");
  }

  countBoxes(): number {
    let total = 0;
    for (let y = 0; y < this.grid.length; y++) {
      for (let x = 0; x < this.grid[y].length; x++) {
        if ("O[".includes(this.grid[y][x])) total += 100 * y + x;
      }
    }
    return total;
  }

  print() {
    console.log(this.grid.map((row) => row.join("")).join("\n"));
  }
}

class BigWarehouse extends Warehouse {
  constructor(map: string) {
    const wideMap = map
      .replaceAll("#", "##")
      .replaceAll("O", "[]")
      .replaceAll(".", "..")
      .replaceAll("@", "@.");
    super(wideMap);
  }

  canMove(x: number, y: number, [dx, dy]: Point): boolean {
    const [nx, ny] = [x + dx, y + dy];
    const curr = this.get(x, y);
    const next = this.get(nx, ny);
    if (curr === ".") return true;
    if (curr === "#") return false;
    if (curr === "@") return next === "." || this.canMove(nx, ny, [dx, dy]);
    if (curr === "[" && dx === 0) {
      const easy = next === "." && this.get(nx + 1, ny) === ".";
      return (
        easy ||
        (this.canMove(nx, ny, [dx, dy]) && this.canMove(nx + 1, ny, [dx, dy]))
      );
    }
    if (curr === "]" && dx === 0) {
      const easy = next === "." && this.get(nx - 1, ny) === ".";
      return (
        easy ||
        (this.canMove(nx, ny, [dx, dy]) && this.canMove(nx - 1, ny, [dx, dy]))
      );
    }
    if (curr === "]" && dx === 1) {
      return next === "." || this.canMove(nx, ny, [dx, dy]);
    }
    if (curr === "[" && dx === 1) {
      return this.canMove(nx, ny, [dx, dy]);
    }
    if (curr === "[" && dx === -1) {
      return next === "." || this.canMove(nx, ny, [dx, dy]);
    }
    if (curr === "]" && dx === -1) {
      return this.canMove(nx, ny, [dx, dy]);
    }
    return false;
  }

  move(x: number, y: number, [dx, dy]: Point): Point {
    const [nx, ny] = [x + dx, y + dy];
    const curr = this.get(x, y);
    if (curr === ".") return [x, y];
    if (this.canMove(x, y, [dx, dy])) {
      if (curr === "@") {
        this.move(nx, ny, [dx, dy]);
        this.grid[ny][nx] = "@";
        this.grid[y][x] = ".";
        return [nx, ny];
      }
      if (curr === "[" && dx === 0) {
        this.move(nx, ny, [dx, dy]);
        this.move(nx + 1, ny, [dx, dy]);
        this.grid[ny][nx] = "[";
        this.grid[ny][nx + 1] = "]";
        this.grid[y][x] = ".";
        this.grid[y][x + 1] = ".";
        return [nx, ny];
      }
      if (curr === "]" && dx === 0) {
        this.move(nx, ny, [dx, dy]);
        this.move(nx - 1, ny, [dx, dy]);
        this.grid[ny][nx] = "]";
        this.grid[ny][nx - 1] = "[";
        this.grid[y][x] = ".";
        this.grid[y][x - 1] = ".";
        return [nx, ny];
      }
      this.move(nx, ny, [dx, dy]);
      this.grid[ny][nx] = this.grid[y][x];
      this.grid[y][x] = ".";
      return [nx, ny];
    }
    return [x, y];
  }
}

export function part1(input: string) {
  const [map, rawMoves] = input.trim().split("\n\n");
  const moves = rawMoves.trim().replaceAll("\n", "").split("") as Move[];

  const warehouse = new Warehouse(map);
  moves.reduce(
    (robot, move) => warehouse.move(...robot, dirs[move]),
    warehouse.getRobot()
  );
  return warehouse.countBoxes();
}

export function part2(input: string) {
  const [map, rawMoves] = input.trim().split("\n\n");
  const moves = rawMoves.trim().replaceAll("\n", "").split("") as Move[];

  const warehouse = new BigWarehouse(map);
  moves.reduce(
    (robot, move) => warehouse.move(...robot, dirs[move]),
    warehouse.getRobot()
  );
  return warehouse.countBoxes();
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-15.txt").text();
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
