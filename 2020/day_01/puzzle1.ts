import * as readline from "node:readline";
import * as fs from 'fs'

// fancy line by line reading in case the file is too big (not likely)
const readInterface = readline.createInterface(fs.createReadStream('data.txt'));

const TARGET = 2020
const numbers = new Set<number>();

readInterface.on('line', (line) => {
    const num = parseInt(line) // trust the line is a number
    if(numbers.has(TARGET-num)){
        console.log(`The numbers are ${num} and ${TARGET-num}`)
        console.log(num*(TARGET-num));
        process.exit(0);
    }
    numbers.add(num);
})
