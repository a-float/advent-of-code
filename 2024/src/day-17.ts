// prettier-ignore
const OPCODES = {ADV: 0, BXL: 1, BST: 2, JNZ: 3, BXC: 4, OUT: 5, BDV: 6, CDV: 7} as const;

class Computer {
  program: bigint[];
  reg: { A: bigint; B: bigint; C: bigint };
  pc: number = 0;
  output: bigint[] = [];

  constructor(program: bigint[], reg: { A: bigint; B: bigint; C: bigint }) {
    this.program = [...program];
    this.reg = { ...reg };
  }

  combo(operand: bigint): bigint {
    switch (Number(operand)) {
      case 0:
      case 1:
      case 2:
      case 3:
        return operand;
      case 4:
        return this.reg.A;
      case 5:
        return this.reg.B;
      case 6:
        return this.reg.C;
    }
    throw new Error("Invalid combo operand");
  }

  execute(opcode: bigint, operand: bigint): void {
    switch (Number(opcode)) {
      case OPCODES.ADV:
        this.reg.A = this.reg.A >> this.combo(operand);
        this.pc += 2;
        break;
      case OPCODES.BXL:
        this.reg.B = this.reg.B ^ operand;
        this.pc += 2;
        break;
      case OPCODES.BST:
        this.reg.B = this.combo(operand) & 7n;
        this.pc += 2;
        break;
      case OPCODES.JNZ:
        if (this.reg.A === 0n) this.pc += 2;
        else {
          this.pc = Number(operand);
        }
        break;
      case OPCODES.BXC:
        this.reg.B = this.reg.B ^ this.reg.C;
        this.pc += 2;
        break;
      case OPCODES.OUT:
        this.output.push(this.combo(operand) & 7n);
        this.pc += 2;
        break;
      case OPCODES.BDV:
        this.reg.B = this.reg.A >> this.combo(operand);
        this.pc += 2;
        break;
      case OPCODES.CDV:
        this.reg.C = this.reg.A >> this.combo(operand);
        this.pc += 2;
        break;
      default:
        throw new Error(`Invalid opcode: ${opcode}`);
    }
  }

  run() {
    while (this.pc < this.program.length) {
      const [opcode, operand] = this.program.slice(this.pc, this.pc + 2);
      this.execute(opcode, operand);
    }
    return this.output;
  }
}

export function part1(input: string) {
  const [A, B, C, ...program] = input.match(/(\d+)/gm)!.map(BigInt);
  return new Computer(program, { A, B, C }).run().join(",");
}

export function part2(input: string) {
  const [_, B, C, ...program] = input.match(/(\d+)/gm)!.map(BigInt);
  const org = program;

  /**
   * We greedily pick 3 bits of the a registry at a time
   * as the input program trims 3 last bits of a for every output printed.
   * @param aEnd the largest bits of the a found so far
   * @param toMatch how many outputs are left to match
   * @returns the smallest a registry value or null if it doesn't exist
   */
  const greedyMatch = (aEnd: bigint, toMatch: number): bigint | null => {
    if (toMatch === 0) return aEnd;
    const need = org.slice(toMatch - 1).join(",");
    for (let i = 0; i <= 0b111; i++) {
      const a = (aEnd << 3n) + BigInt(i);
      const out = new Computer(program, { A: a, B, C }).run().join(",");
      if (out.endsWith(need)) {
        const tried = greedyMatch(a, toMatch - 1);
        if (tried) return tried;
      }
    }
    return null;
  };

  return greedyMatch(0n, program.length)?.toString();
}

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-17.txt").text();
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
