import * as fs from 'fs';
import * as os from 'os';

const content: string = fs.readFileSync("../data/data1.txt", "utf-8");
const elfs = content.split(os.EOL + os.EOL);
const calories = elfs.map(elf => elf.split("\n").map(x => parseInt(x)).reduce((acc, val) => acc + val, 0));
calories.sort((a, b) => b - a);
console.log("Part 1:", calories[0]);
console.log("Part 2:", calories.slice(0, 3).reduce((acc, val) => acc + val, 0));

