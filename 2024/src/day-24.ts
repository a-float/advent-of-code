type Gate = { op: string; a: string; b: string; out: string };

const getWires = (input: string) => {
  const [rawWires, rawGates] = input.split("\n\n").map((x) => x.split("\n"));
  const wires = new Map<string, number | Gate>();
  rawWires.forEach((x) => {
    const [name, state] = x.split(": ");
    wires.set(name, parseInt(state));
  });
  rawGates
    .map((x) => {
      const [a, op, b, _, out] = x.split(" ");
      return { op, a, b, out };
    })
    .forEach((x) => wires.set(x.out, x));
  return wires;
};

const resolveWire = (
  wires: Map<string, number | Gate>,
  name: string
): number => {
  const wire = wires.get(name)!;
  if (typeof wire === "number") return wire;
  const [a, b] = [resolveWire(wires, wire.a), resolveWire(wires, wire.b)];
  return wire.op === "AND" ? a & b : wire.op === "OR" ? a | b : a ^ b;
};

const resolveZ = (wires: Map<string, number | Gate>) => {
  const binStr = [...wires.keys()]
    .filter((k) => k.startsWith("z"))
    .toSorted()
    .toReversed()
    .map((k) => resolveWire(wires, k))
    .join("");

  return parseInt(binStr, 2);
};

export const part1 = (input: string) => resolveZ(getWires(input));

export const part2 = (input: string) => {
  const wires = getWires(input);

  const gates = [...wires.values()].filter(
    (x): x is Gate => typeof x !== "number"
  );

  const wrong: Gate[] = [];
  for (const gate of gates) {
    if (gate.out[0] === "z" && gate.op !== "XOR" && gate.out !== "z45") {
      wrong.push(gate);
    }

    if (
      gate.op === "XOR" &&
      !["z"].includes(gate.out[0]) &&
      !["x", "y"].includes(gate.a[0]) &&
      !["x", "y"].includes(gate.b[0])
    ) {
      wrong.push(gate);
    }

    if (gate.op === "AND" && ![gate.a, gate.b].includes("x00")) {
      for (const otherGate of gates) {
        if (
          [otherGate.a, otherGate.b].includes(gate.out) &&
          otherGate.op !== "OR"
        ) {
          wrong.push(gate);
        }
      }
    }

    if (gate.op === "XOR") {
      for (const otherGate of gates) {
        if (
          [otherGate.a, otherGate.b].includes(gate.out) &&
          otherGate.op === "OR"
        ) {
          wrong.push(gate);
        }
      }
    }
  }

  return [...new Set(wrong.map((g) => g.out))].toSorted().join(",");
};

if (typeof require !== "undefined" && require.main === module) {
  const input = await Bun.file("data/day-24.txt").text();
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
