import * as fs from "fs";
import * as os from "os";


class Vector2 {
  cords: [number, number]
  constructor(vals: number[]) {
    if (vals.length < 2) {
      throw new Error("Vaector vals too short")
    }
    this.cords = [vals[0], vals[1]]
  }

  get x() {
    return this.cords[0]
  }

  get y() {
    return this.cords[1]
  }

  add(other: Vector2): Vector2 {
    if (this.cords.length != other.cords.length) {
      throw new Error("Adding vectors of different sizes")
    }
    return new Vector2([this.cords[0] + other.cords[0], this.cords[1] + other.cords[1]])
  }

  mult(scalar: number): Vector2 {
    return new Vector2(this.cords.map(x => x * scalar) as Vector2["cords"])
  }

  normalize(): Vector2 {
    const len = Math.sqrt(this.cords.reduce((acc, val) => acc + val * val, 0))
    return this.mult(1 / len)
  }

  subtract(other: Vector2) {
    return this.add(other.mult(-1))
  }

  equals(other: Vector2): boolean {
    return this.x === other.x && this.y === other.y
  }

  get hash() {
    return `${this.cords[0]}:${this.cords[1]}`
  }
}

const lines = fs
  .readFileSync("data14.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL)
  .map(l => [...l.matchAll(/(\d+),(\d+)/g)].map(x => [x[1], x[2]].map(y => parseInt(y))).map(v => new Vector2(v)))

enum Material {
  Sand,
  Rock,
}

class Cave {
  bounds: { left: number; right: number; top: number; bottom: number; };
  private rocks: Set<string>;
  private sand: Set<string>;
  source: Vector2;
  constructor(source: Vector2) {
    this.rocks = new Set()
    this.sand = new Set()
    this.source = source
    this.bounds = {
      left: Infinity,
      right: -Infinity,
      top: Infinity,
      bottom: -Infinity
    }
    this.updateBounds(source)
  }

  isEmpty(pos: Vector2): boolean {
    if (pos.y >= this.bounds.bottom + 2) return false // part 2 infinite floor
    return !this.rocks.has(pos.hash) && !this.sand.has(pos.hash)
  }

  clearSand() {
    this.sand.clear()
  }

  private updateBounds(pos: Vector2) {
    this.bounds = {
      left: Math.min(this.bounds.left, pos.x),
      right: Math.max(this.bounds.right, pos.x),
      top: Math.min(this.bounds.top, pos.y),
      bottom: Math.max(this.bounds.bottom, pos.y)
    }
  }

  addPos(pos: Vector2, what: Material) {
    if (what === Material.Rock) {
      this.rocks.add(pos.hash)
      this.updateBounds(pos)
    } else {
      this.sand.add(pos.hash)
    }
  }

  showCave() {
    for (let y = this.bounds.top; y <= this.bounds.bottom; y++) {
      for (let x = this.bounds.left; x <= this.bounds.right; x++) {
        const pos = new Vector2([x, y])
        if (pos.equals(this.source)) process.stdout.write("*")
        else if (this.rocks.has(pos.hash)) process.stdout.write("#")
        else if (this.sand.has(pos.hash)) process.stdout.write("o")
        else process.stdout.write(".")
      }
      process.stdout.write(os.EOL);
    }
  }
}

const cave = new Cave(new Vector2([500, 0])
)
for (const line of lines) {
  for (let i = 0; i < line.length - 1; i++) {
    const start = line[i]
    const end = line[i + 1]
    const diff = end.subtract(start).normalize()
    let mover = start;
    while (true) {
      cave.addPos(mover, Material.Rock)
      if (mover.equals(end)) break
      mover = mover.add(diff)
    }
  }
}

/**
 * Simulates sand fall. Modifies the provided cave
 * @param pos current sand position
 * @param cave cave it falls through
 * @returns false if sand came to rest, true if it will be falling forever
 */
const dropSand = (pos: Vector2, cave: Cave): Vector2 => {
  const below = pos.add(new Vector2([0, 1]))
  const bottomLeft = pos.add(new Vector2([-1, 1]))
  const bottomRight = pos.add(new Vector2([1, 1]))
  if (cave.isEmpty(below)) return dropSand(below, cave)
  if (cave.isEmpty(bottomLeft)) return dropSand(bottomLeft, cave)
  if (cave.isEmpty(bottomRight)) return dropSand(bottomRight, cave)
  cave.addPos(pos, Material.Sand)
  return pos
}

const sandCounter = (cave: Cave, condition: (pos: Vector2) => boolean): number => {
  let i = 0;
  while (true) {
    const pos = dropSand(cave.source, cave)
    if (condition(pos)) return i
    i++
  }
}

console.time("part1")
const p1 = sandCounter(cave, (pos) => pos.y >= cave.bounds.bottom)
console.timeEnd("part1")

console.time("part2")
const p2 = sandCounter(cave, (pos) => pos.equals(cave.source)) + p1 + 2
console.timeEnd("part2")

// cave.showCave()
console.log("Part 1: ", p1)
console.log("Part 2: ", p2)
