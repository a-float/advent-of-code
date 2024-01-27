import * as fs from 'fs';

const priorities: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

function* chunks<T>(list: T[], n: number): Iterable<T[]> {
    for (let i = 0; i < list.length; i += n) {
        yield list.slice(i, i + n);
    }
}

fs.readFile("../data/data2.txt", "utf8", (err, content) => {
    if (err) {
        console.error(err);
        return;
    }
    const lines: string[] = content.split("\n");
    let total: number = 0;
    for (const line of lines) {
        const h: number = Math.floor(line.length / 2);
        const a = line.slice(0, h);
        const b = new Set(line.slice(h));
        const match = Array.from(a).filter(x => b.has(x))[0];
        total += priorities.indexOf(match) + 1;
    }
    let total2: number = 0;
    for (const chunk of chunks(lines, 3)) {
        const a = chunk[0];
        const b = new Set(chunk[1]);
        const c = new Set(chunk[2]);
        const match = Array.from(a).filter(x => b.has(x) && c.has(x))[0];
        total2 += priorities.indexOf(match) + 1;
    }
    console.log("Part 1:", total);
    console.log("Part 2:", total2);
});

