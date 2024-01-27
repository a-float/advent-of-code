import * as readline from "node:readline";
import * as fs from 'fs'

// fancy line by line reading in case the file is too big (not likely)
const readInterface = readline.createInterface(fs.createReadStream('data.txt'));

const TARGET = 2020
const singles = new Set<number>();
// maps numbers sum to the numbers
const doubles = new Map<number, number[]>();

readInterface.on('line', (line) => {
    const num = parseInt(line); // trust the line is a number
    if (doubles.has(TARGET - num)) {
        const others = doubles.get(TARGET - num)!;
        console.log(`The numbers are ${others.join(', ')} and ${num}`);
        console.log([...others, num].reduce((a, b) => a * b, 1));
        process.exit(0);
    }
    for (const s of singles) {
        doubles.set(s + num, [s, num]);
    }
    singles.add(num);
})
